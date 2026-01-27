# R/3.1_mod_livestock_area_stacked.R
# ---------------------------------------------------------------
# Surfaces d’élevage (Element == "Area") — empilé par scénario
# Items tracés : Dairy / Beef cattle / Meat sheep and goats
# Style harmonisé avec le module "Harvested" (R/12_stacked)
#
# SCENARIO RULES (project-wide):
# - Le module NE reconstruit aucune logique locale de scénarios (pas de fallback, pas de contrainte/extra calculée ici).
# - Il affiche exactement r_scenarios() (CODES fact$Scenario), intersecté avec les scénarios réellement présents dans fact
#   pour le pays/élément (via years_by_scenario()).
# - Ordre stable via SCENARIO_LEVELS_DEFAULT (codes) + append déterministe des codes inconnus.
# - Affichage: scenario_label(code) uniquement (axes, cartes KPI, hover).
# ---------------------------------------------------------------

suppressPackageStartupMessages({
  library(shiny)
  library(dplyr)
  library(stringr)
  library(plotly)
  library(scales)
})

mod_livestock_area_stacked_ui <- function(id, height = "520px", full_width = TRUE){
  ns <- NS(id)
  
  div(
    class = if (isTRUE(full_width)) "card full-bleed" else "card",
    div(
      class = "card-body",
      
      h2(class = "card-title", "Area allocated to livestock activity (in hectares)"),
      
      plotly::plotlyOutput(ns("stack_ls"), height = "420px", width = "100%"),
      
      h2("Total livestock area"),
      div(
        id    = ns("kpi_root"),
        class = "surface-cards",
        div(
          id    = ns("kpi_cards_root"),
          style = "--surf-kpi-h:120px;",
          uiOutput(ns("kpi_cards"))
        )
      ),
      tags$br(),
      
      div(
        class = "text-right",
        div(
          class = "u-actions",
          downloadLink(
            ns("dl_livestock_csv"),
            label = tagList(icon("download"), "CSV")
          )
        )
      ),
      tags$br(),
      
      uiOutput(ns("note"))
    )
  )
}

mod_livestock_area_stacked_server <- function(
    id,
    fact,
    r_country,
    r_scenarios,         # <--- REQUIRED: reactive/function returning scenario CODES (fact$Scenario)
    element_name     = "Area",
    items_keep       = c("Dairy","Beef cattle","Meat sheep and goats"),
    value_multiplier = NULL,
    baseline_label   = "Total use of land for pastures and meadows at base-year"
){
  moduleServer(id, function(input, output, session){
    
    `%||%` <- function(a, b) if (is.null(a) || length(a)==0 || (is.numeric(a) && !is.finite(a))) b else a
    
    # --- Dépendances "scénarios" (single source of truth) -------------------
    if (is.null(r_scenarios) || !is.function(r_scenarios)) {
      stop("mod_livestock_area_stacked_server(): 'r_scenarios' must be provided as a reactive/function returning scenario CODES.")
    }
    if (!exists("scenario_label", mode = "function", inherits = TRUE)) {
      stop("mod_livestock_area_stacked_server(): missing dependency 'scenario_label(code)'.")
    }
    if (!exists("SCENARIO_LEVELS_DEFAULT", inherits = TRUE)) {
      stop("mod_livestock_area_stacked_server(): missing dependency 'SCENARIO_LEVELS_DEFAULT'.")
    }
    
    scenario_label_vec <- function(x){
      x <- as.character(x)
      vapply(x, scenario_label, character(1))
    }
    
    # --- Palette ------------------------------------------------------------
    livestock_colors_for <- function(items){
      if (exists("pal_livestock", mode = "function", inherits = TRUE)) {
        unname(pal_livestock(items))
      } else if (exists("pal_crops", mode = "function", inherits = TRUE)) {
        unname(pal_crops(items))
      } else {
        scales::hue_pal()(length(items))
      }
    }
    
    # --- Scénarios: codes ordonnés via config ------------------------------
    scen_codes_ordered <- reactive({
      sc <- unique(as.character(r_scenarios()))
      sc <- sc[!is.na(sc) & nzchar(sc)]
      
      known   <- intersect(SCENARIO_LEVELS_DEFAULT, sc)
      unknown <- setdiff(sc, SCENARIO_LEVELS_DEFAULT)
      
      c(known, sort(unknown))
    })
    
    scen_levels_all <- reactive({
      sc <- scen_codes_ordered()
      c(SCENARIO_LEVELS_DEFAULT, setdiff(sc, SCENARIO_LEVELS_DEFAULT))
    })
    
    baseline_code <- reactive({
      b <- intersect(SCENARIO_LEVELS_DEFAULT, unique(as.character(r_scenarios())))
      b <- b[!is.na(b) & nzchar(b)]
      b[1] %||% NA_character_
    })
    
    # --- bindCache key (scalaire) ------------------------------------------
    cache_key_base <- reactive({
      req(r_country())
      paste0(
        "ls_area_stacked|",
        r_country(), "|el=", element_name,
        "|sc=", paste(scen_codes_ordered(), collapse = ",")
      )
    })
    
    # --- Années par scénario (intersection r_scenarios ∩ fact) -------------
    years_by_scenario <- reactive({
      req(r_country())
      sc_req <- scen_codes_ordered()
      validate(need(length(sc_req) > 0, "No scenario selected."))
      
      dat <- fact %>%
        filter(
          Region == r_country(),
          str_to_lower(str_trim(Element)) == str_to_lower(str_trim(element_name)),
          Scenario %in% sc_req
        )
      
      validate(need(nrow(dat) > 0, "No data available for this country/element."))
      
      # année max par scénario
      yrs <- dat %>%
        group_by(Scenario) %>%
        summarise(
          year_used = suppressWarnings(max(Year[!is.na(Value)], na.rm = TRUE)),
          .groups = "drop"
        ) %>%
        filter(is.finite(year_used))
      
      # Baseline: si 2018 est disponible et non-NA, on privilégie 2018
      b <- baseline_code()
      if (!is.na(b) && nzchar(b)) {
        has_2018 <- any(dat$Scenario == b & dat$Year == 2018 & !is.na(dat$Value))
        if (isTRUE(has_2018)) {
          yrs$year_used[as.character(yrs$Scenario) == b] <- 2018
        }
      }
      
      yrs %>%
        mutate(
          Scenario_code = as.character(Scenario),
          Scenario_f    = factor(Scenario_code, levels = scen_levels_all())
        ) %>%
        arrange(Scenario_f) %>%
        select(Scenario_code, year_used, Scenario_f)
    }) %>% bindCache(cache_key_base())
    
    # --- Données principales élevage ---------------------------------------
    data_ls <- reactive({
      yrs <- years_by_scenario()
      validate(need(nrow(yrs) > 0, "No scenario available for this country/element."))
      
      dat0 <- fact %>%
        inner_join(select(yrs, Scenario_code, year_used), by = c("Scenario" = "Scenario_code")) %>%
        filter(
          Region == r_country(),
          str_to_lower(str_trim(Element)) == str_to_lower(str_trim(element_name)),
          Year == year_used
        )
      
      # Multiplicateur automatique (si Unit = "1000 ha", etc.)
      unit_vals <- unique(na.omit(dat0$Unit))
      mult_auto <- if (length(unit_vals) && any(grepl("1000", unit_vals, fixed = TRUE))) 1000 else 1
      mult <- if (is.null(value_multiplier)) mult_auto else value_multiplier
      
      dat <- dat0 %>%
        filter(Item %in% items_keep) %>%
        group_by(Scenario, Item, year_used) %>%
        summarise(value = sum(Value, na.rm = TRUE), .groups = "drop") %>%
        mutate(
          value = value * mult,
          year  = year_used,
          Scenario_code = as.character(Scenario),
          Scenario_f    = factor(Scenario_code, levels = scen_levels_all())
        ) %>%
        droplevels()
      
      # Ordre des items = décroissant sur la baseline si elle existe, sinon sur total global
      b <- baseline_code()
      base_order <- dat %>%
        {
          if (!is.na(b) && any(.$Scenario_code == b)) {
            filter(., Scenario_code == b)
          } else {
            .
          }
        } %>%
        group_by(Item) %>%
        summarise(tot = sum(value, na.rm = TRUE), .groups = "drop") %>%
        arrange(desc(tot)) %>%
        pull(Item)
      
      dat %>%
        mutate(
          Item   = factor(Item, levels = base_order),
          scen_i = as.integer(Scenario_f),
          scen_t = scenario_label_vec(Scenario_code)  # label UI
        )
    }) %>% bindCache(cache_key_base())
    
    # --- KPI : totaux + deltas vs baseline ---------------------------------
    kpi_livestock <- reactive({
      da <- data_ls()
      validate(need(nrow(da) > 0, "No data available to compute KPIs."))
      
      agg <- da %>%
        group_by(Scenario_code, Scenario_f) %>%
        summarise(value_ha = sum(value, na.rm = TRUE), .groups = "drop") %>%
        arrange(Scenario_f)
      
      b <- baseline_code()
      base_total <- agg$value_ha[agg$Scenario_code == b][1] %||% NA_real_
      
      agg %>%
        mutate(
          diff_pct = if (is.finite(base_total) && base_total > 0) {
            100 * (value_ha - base_total) / base_total
          } else {
            NA_real_
          }
        )
    }) %>% bindCache(cache_key_base())
    
    # --- Graphique principal -----------------------------------------------
    output$stack_ls <- plotly::renderPlotly({
      da <- data_ls()
      req(nrow(da) > 0)
      
      # >>> THEME GLOBAL (R/99)
      th <- get_plotly_tokens()
      
      scen_codes_used <- levels(da$Scenario_f)
      tick_vals <- seq_along(scen_codes_used)
      tick_text <- scenario_label_vec(scen_codes_used)
      
      cols <- livestock_colors_for(levels(da$Item))
      
      p <- plotly::plot_ly() %>%
        plotly::layout(
          barmode = "stack",
          xaxis = list(
            title    = "",
            tickmode = "array",
            tickvals = tick_vals,
            ticktext = tick_text
          ),
          yaxis = list(
            title = "ha",
            separatethousands = TRUE
          ),
          legend = list(
            title       = list(text = ""),
            orientation = "h",
            x           = 0.5,
            xanchor     = "center",
            y           = 1.05,
            yanchor     = "bottom"
          ),
          margin = list(t = 20, r = 40)
        )
      
      items_vec <- levels(da$Item)
      if (is.null(items_vec)) items_vec <- unique(as.character(da$Item))
      
      for (it in items_vec) {
        sub <- da %>% filter(Item == it)
        if (nrow(sub) == 0) next
        
        sub <- sub %>%
          mutate(
            hover_value = if_else(
              is.finite(value),
              format(round(value), big.mark = " ", scientific = FALSE, trim = TRUE),
              "—"
            )
          )
        
        idx    <- match(it, items_vec)
        col_it <- if (!is.na(idx)) unname(cols[idx]) else NULL
        
        p <- plotly::add_bars(
          p,
          data  = sub,
          x     = ~scen_i,
          y     = ~value,
          name  = it,
          legendgroup = it,
          marker = list(color = col_it),
          text       = ~scen_t,
          textposition = "none",
          customdata = ~hover_value,
          hovertemplate = paste0(
            "%{text}<br>", it, " : %{customdata} ha<extra></extra>"
          )
        )
      }
      
      # Ligne baseline (si baseline présente dans les données utilisées)
      b <- baseline_code()
      base_total <- da %>%
        filter(Scenario_code == b) %>%
        summarise(tot = sum(value, na.rm = TRUE), .groups = "drop") %>%
        pull(tot) %||% NA_real_
      
      if (is.finite(base_total)) {
        x_min <- 0.5
        x_max <- length(scen_codes_used) + 0.5
        
        p <- p %>%
          plotly::add_trace(
            x = c(x_min, x_max),
            y = c(base_total, base_total),
            type = "scatter",
            mode = "lines",
            line = list(
              dash  = "dash",
              color = th$baseline_color,
              width = 1.5
            ),
            name        = baseline_label,
            hoverinfo   = "none",
            legendgroup = "baseline_line",
            showlegend  = TRUE
          )
      }
      
      p <- plotly_apply_global_theme(p, bg = "transparent", grid = "y")
      p
    }) %>% bindCache(cache_key_base())
    
    # --- Encadrés KPI -------------------------------------------------------
    output$kpi_cards <- renderUI({
      dat <- kpi_livestock()
      if (is.null(dat) || nrow(dat) == 0) return(NULL)
      
      fmt_num <- function(x){
        ifelse(
          is.finite(x),
          format(round(x), big.mark = " ", scientific = FALSE, trim = TRUE),
          "—"
        )
      }
      fmt_pct <- function(p){
        if (!is.finite(p)) return("—")
        paste0(ifelse(p >= 0, "+", ""), formatC(p, digits = 0, format = "f"), "%")
      }
      
      b <- baseline_code()
      
      cards <- lapply(seq_len(nrow(dat)), function(i){
        sc_code <- as.character(dat$Scenario_code[i])
        sc_lab  <- scenario_label(sc_code)
        val     <- dat$value_ha[i]
        dlt     <- dat$diff_pct[i]
        
        delta_tag <- if (!is.finite(dlt) || identical(sc_code, b)) {
          NULL
        } else if (dlt > 0) {
          span(class = "up", fmt_pct(dlt))
        } else if (dlt < 0) {
          span(class = "down", fmt_pct(dlt))
        } else {
          "0%"
        }
        
        subline_base <- if (identical(sc_code, b)) {
          # pas de texte pour la baseline
          p(class = "u-sub", htmltools::HTML("&nbsp;"))
        } else {
          p(class = "u-sub", "Vs base year: ", delta_tag)
        }
        
        div(
          class = "u-card u-card--flat u-card--hover",
          div(
            class = "u-box",
            p(class = "u-title", sc_lab),
            p(class = "u-value", fmt_num(val), span(class = "u-unit", "ha")),
            subline_base
          )
        )
      })
      
      div(class = "u-row", do.call(tagList, cards))
    }) %>% bindCache(cache_key_base())
    
    # --- Export CSV ---------------------------------------------------------
    output$dl_livestock_csv <- downloadHandler(
      filename = function(){
        paste0("Livestock_Area_", gsub(" ", "_", r_country()), ".csv")
      },
      content = function(file){
        da <- data_ls()
        req(nrow(da) > 0)
        
        out <- da %>%
          transmute(
            Country        = r_country(),
            Scenario_code  = Scenario_code,
            Scenario_label = scenario_label_vec(Scenario_code),
            Year           = year,
            Element        = element_name,
            Item           = as.character(Item),
            Value_ha       = value
          )
        
        readr::write_delim(out, file, delim = ";")
      }
    )
    
    # --- Note explicative ---------------------------------------------------
    output$note <- renderUI({
      da <- data_ls()
      if (nrow(da) == 0) return(NULL)
      
      htmltools::HTML(glue::glue(
        "<p>
        This chart shows, for the selected country, the <strong>agricultural area used for livestock</strong>
        in the base-year and under the selected scenarios.
        Each stacked bar represents the total area (in hectares) allocated to livestock farming (pastures + meadows),
        broken down into <strong>dairy</strong>, <strong>beef cattle</strong> and <strong>sheep and goats meat</strong>.
        </p>
        <p>
        The cards below the chart summarise, for each scenario, the total area allocated to livestock farming (in hectares)
        and its percentage change relative to the base-year.
        </p>"
      ))
    })
    
    # Ne renvoie plus r_scenarios : la sélection est désormais globale.
    invisible(NULL)
  })
}
