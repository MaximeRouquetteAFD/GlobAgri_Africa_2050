# R/12_mod_harvested_stacked.R
# ---------------------------------------------------------------
# Module : barres empilées "Area harvested" + cartes KPI
# - Utilise harvested_core() pour les calculs
# - Scénarios centralisés (config + helper global via r_scenarios)
# ---------------------------------------------------------------

mod_harvested_stacked_ui <- function(id, height = "480px", full_width = TRUE){
  ns <- NS(id)
  div(
    class = if (isTRUE(full_width)) "card full-bleed" else "card",
    div(
      class = "card-body",
      
      h2("Area harvested per crop according to the scenarios (in hectares)"),
      
      # Message explicatif si scénario extra redondant
      uiOutput(ns("limit_msg")),
      
      plotly::plotlyOutput(ns("stack_harv"), height = "490px", width = "100%"),
      
      h2("Total harvested area"),
      
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
            ns("dl_harvested_csv"),
            label = tagList(icon("download"), "CSV")
          )
        )
      ),
      tags$br(),
      
      uiOutput(ns("note"))
    )
  )
}


mod_harvested_stacked_server <- function(
    id,
    fact,
    r_country,
    r_scenarios = NULL, # <- NOUVEAU : scenarios effectifs depuis app.R (reactive)
    harvest_element = "Area harvested",
    exclude_items = c(
      "All products","All crops","Agricultural land occupation (Farm)",
      "Cropland","Forest land","Land under perm. meadows and pastures"
    ),
    value_multiplier = 1,
    baseline_label = "Total use of land for crops at base-year",
    group_var = NULL,
    plot_theme = c("dark", "light")
){
  plot_theme <- match.arg(plot_theme)
  
  moduleServer(id, function(input, output, session){
    
    # -------- Helpers config-first -----------------------------------------
    sc_norm <- if (exists("scenario_code", mode = "function", inherits = TRUE)) scenario_code else {
      function(x) stringr::str_squish(as.character(x))
    }
    sc_label <- if (exists("scenario_label", mode = "function", inherits = TRUE)) scenario_label else {
      function(x) sc_norm(x)
    }
    
    # Palette cultures (utilise pal_crops() si dispo)
    harvest_colors_for <- function(items){
      if (exists("pal_crops", mode = "function", inherits = TRUE)) {
        pal_crops(items)
      } else {
        scales::hue_pal()(length(items))
      }
    }
    
    # Renommage d'item (affichage + export)
    rename_harvest_items <- function(df){
      if (!"Item" %in% names(df)) return(df)
      
      old <- "Tea, cocoa, coffee, oilpalm, sugar cane"
      new <- "Perennial plants and stimulants"
      
      if (is.factor(df$Item)) {
        if (old %in% levels(df$Item)) {
          df$Item <- forcats::fct_recode(df$Item, !!new := old)
        }
      } else {
        df$Item <- ifelse(df$Item == old, new, df$Item)
      }
      df
    }
    
    # -----------------------------------------------------------
    # Noyau commun : scénarios centralisés + respect helper global
    # -----------------------------------------------------------
    core <- harvested_core(
      fact             = fact,
      r_country         = r_country,
      harvest_element   = harvest_element,
      exclude_items     = exclude_items,
      value_multiplier  = value_multiplier,
      group_var         = group_var,
      r_scenarios       = r_scenarios   # <- IMPORTANT : branchement helper global
    )
    
    scen_base            <- core$scen_base
    scen_ref             <- core$scen_ref
    scen_show             <- core$scen_show   # reactive (vecteur de codes)
    scen_extra_selected   <- core$scen_extra_selected     # reactive
    levels_all            <- core$levels_all              # reactive
    years_by_scenario     <- core$years_by_scenario
    data_harvested        <- core$data_harvested
    constraint_info       <- core$constraint_info
    scen_diets_effective  <- core$scen_diets_effective
    # data_harvested_groups <- core$data_harvested_groups  # pas utilisé ici
    
    scen_show_key <- reactive({
      paste(as.character(scen_show()), collapse = "|")
    })
    
    # -----------------------------------------------------------
    # KPI : total par scénario + % vs base + Δha vs trend (uniquement pour extra si affiché)
    # -----------------------------------------------------------
    kpi_harvested <- reactive({
      da <- rename_harvest_items(data_harvested())
      
      scen_show_codes <- scen_show()
      req(length(scen_show_codes) > 0)
      
      # Extra(s) depuis config (codes), optionnel
      extra_codes <- if (exists("SCENARIOS_EXTRA_CODES", inherits = TRUE)) {
        as.character(SCENARIOS_EXTRA_CODES)
      } else {
        character(0)
      }
      extra_codes <- intersect(as.character(scen_show_codes), extra_codes)
      
      # Agrégation sur ce qui existe
      agg_raw <- da %>%
        dplyr::group_by(Scenario) %>%
        dplyr::summarise(value_ha = sum(value, na.rm = TRUE), .groups = "drop") %>%
        dplyr::mutate(Scenario = as.character(Scenario))
      
      # Skeleton : garantit une ligne par scénario affiché
      skeleton <- tibble::tibble(
        Scenario = as.character(scen_show_codes)
      )
      
      agg <- skeleton %>%
        dplyr::left_join(agg_raw, by = "Scenario") %>%
        dplyr::mutate(
          Scenario = factor(Scenario, levels = as.character(scen_show_codes))
        ) %>%
        dplyr::arrange(Scenario)
      
      base_total <- agg %>%
        dplyr::filter(as.character(Scenario) == scen_base) %>%
        dplyr::pull(value_ha)
      base_total <- if (length(base_total)) base_total[1] else NA_real_
      
      ref_total <- agg %>%
        dplyr::filter(as.character(Scenario) == scen_ref) %>%
        dplyr::pull(value_ha)
      ref_total <- if (length(ref_total)) ref_total[1] else NA_real_
      
      agg %>%
        dplyr::mutate(
          diff_pct = dplyr::if_else(
            is.finite(base_total) & base_total > 0 & is.finite(value_ha),
            100 * (value_ha - base_total) / base_total,
            NA_real_
          ),
          diff_ha_ref = dplyr::if_else(
            is.finite(ref_total) & is.finite(value_ha),
            value_ha - ref_total,
            NA_real_
          ),
          Scenario_code  = as.character(Scenario),
          Scenario_label = sc_label(as.character(Scenario)),
          is_extra       = as.character(Scenario) %in% extra_codes
        )
    }) %>% bindCache(r_country(), harvest_element, scen_show_key())
    
    # -----------------------------------------------------------
    # Graphique empilé (Plotly) — thème global R/99 + labels UI
    # -----------------------------------------------------------
    output$stack_harv <- plotly::renderPlotly({
      da_full <- rename_harvest_items(data_harvested())
      req(nrow(da_full) > 0)
      
      th <- get_plotly_tokens()
      
      # Total baseline
      base_total <- da_full %>%
        dplyr::filter(as.character(Scenario) == scen_base) %>%
        dplyr::summarise(tot = sum(value, na.rm = TRUE), .groups = "drop") %>%
        dplyr::pull(tot)
      req(length(base_total) == 1, is.finite(base_total))
      
      # Scénarios réellement affichés (baseline + diets effectives)
      scen_used <- c(scen_base, scen_diets_effective())
      
      da <- da_full %>%
        dplyr::filter(as.character(Scenario) %in% as.character(scen_used)) %>%
        droplevels()
      req(nrow(da) > 0)
      
      # Couleurs
      cols <- harvest_colors_for(levels(da$Item))
      
      # Libellés d’axe : label UI (scenario_label) au lieu du code
      scen_lvls_chr   <- levels(da$Scenario)
      tick_vals       <- seq_along(scen_lvls_chr)
      tick_text_label <- vapply(scen_lvls_chr, sc_label, character(1))
      
      p <- plotly::plot_ly() %>%
        plotly::layout(
          barmode = "stack",
          xaxis = list(
            title    = "",
            type     = "linear",
            tickmode = "array",
            tickvals = tick_vals,
            ticktext = tick_text_label
          ),
          yaxis = list(title = "ha", separatethousands = TRUE),
          legend = list(
            title       = list(text = ""),
            orientation = "h",
            x           = 0.5,
            xanchor     = "center",
            y           = 1.05,
            yanchor     = "bottom"
          ),
          margin = list(t = 90, r = 40)
        )
      
      # Barres empilées par culture
      items_vec <- levels(da$Item)
      if (is.null(items_vec)) items_vec <- unique(as.character(da$Item))
      
      for (it in items_vec) {
        sub <- da %>% dplyr::filter(Item == it)
        if (nrow(sub) == 0) next
        
        sub <- sub %>%
          dplyr::mutate(
            scen_label = sc_label(as.character(Scenario)),
            hover_value = dplyr::if_else(
              is.finite(value),
              format(round(value), big.mark = " ", scientific = FALSE, trim = TRUE),
              "—"
            )
          )
        
        idx    <- match(it, items_vec)
        col_it <- if (!is.na(idx)) unname(cols[idx]) else NULL
        
        p <- plotly::add_bars(
          p, data = sub,
          x = ~scen_i, y = ~value,
          name = it, legendgroup = it,
          marker = list(color = col_it),
          text = ~scen_label, textposition = "none",
          customdata = ~hover_value,
          hovertemplate = paste0("%{text}<br>", it, " : %{customdata} ha<extra></extra>")
        )
      }
      
      # Ligne baseline
      x_min <- 0.5
      x_max <- length(scen_lvls_chr) + 0.5
      p <- p %>%
        plotly::add_trace(
          x = c(x_min, x_max),
          y = c(base_total, base_total),
          type  = "scatter",
          mode  = "lines",
          line  = list(dash = "dash", width = 1.5, color = th$baseline_color),
          name        = baseline_label,
          hoverinfo   = "none",
          legendgroup = "baseline_line",
          showlegend  = TRUE
        )
      
      plotly_apply_global_theme(p, bg = "transparent", grid = "y")
    })
    
    # -----------------------------------------------------------
    # Cartes KPI
    # -----------------------------------------------------------
    output$kpi_cards <- renderUI({
      dat <- kpi_harvested()
      if (is.null(dat) || nrow(dat) == 0) return(NULL)
      
      fmt_num <- function(x){
        ifelse(is.finite(x), format(round(x), big.mark=" ", scientific=FALSE, trim=TRUE), "—")
      }
      fmt_pct <- function(p){
        if (!is.finite(p)) return("—")
        paste0(ifelse(p >= 0, "+", ""), formatC(p, digits=0, format="f"), "%")
      }
      fmt_ha_signed <- function(x){
        if (!is.finite(x)) return("—")
        paste0(if (x > 0) "+" else "", format(round(x), big.mark=" ", scientific=FALSE, trim=TRUE))
      }
      
      cards <- lapply(seq_len(nrow(dat)), function(i){
        sc_code  <- dat$Scenario_code[i]
        sc_lbl   <- dat$Scenario_label[i]
        val      <- dat$value_ha[i]
        dlt      <- dat$diff_pct[i]
        dha_ref  <- dat$diff_ha_ref[i]
        is_extra <- isTRUE(dat$is_extra[i])
        
        delta_tag <- if (!is.finite(dlt) || sc_code == scen_base) {
          NULL
        } else if (dlt > 0) {
          span(class="up", fmt_pct(dlt))
        } else if (dlt < 0) {
          span(class="down", fmt_pct(dlt))
        } else {
          "0%"
        }
        
        subline_base <- if (sc_code == scen_base) {
          p(class="u-sub", htmltools::HTML("&nbsp;"))
        } else {
          p(class="u-sub", "Vs base year: ", delta_tag)
        }
        
        subline_ref <- if (is_extra && is.finite(dha_ref)) {
          cls <- if (dha_ref < 0) "down" else if (dha_ref > 0) "up" else ""
          p(
            class = "u-sub",
            "Vs trend diet: ",
            span(class = cls, paste0(fmt_ha_signed(dha_ref), " ha"))
          )
        } else {
          NULL
        }
        
        div(
          class="u-card u-card--flat u-card--hover",
          div(
            class="u-box",
            p(class="u-title", sc_lbl),
            p(class="u-value", fmt_num(val), span(class="u-unit","ha")),
            subline_base,
            subline_ref
          )
        )
      })
      
      div(class="u-row", do.call(tagList, cards))
    })
    
    
    # -----------------------------------------------------------
    # Export CSV
    # -----------------------------------------------------------
    output$dl_harvested_csv <- downloadHandler(
      filename = function(){
        paste0("AreaHarvested_", gsub(" ", "_", r_country()), ".csv")
      },
      content = function(file){
        da <- rename_harvest_items(data_harvested())
        req(nrow(da) > 0)
        
        out <- da %>%
          dplyr::transmute(
            Pays          = r_country(),
            Scenario_code = as.character(Scenario),
            Scenario      = sc_label(as.character(Scenario)),
            Annee         = year,
            Item          = as.character(Item),
            Valeur_ha     = value
          ) %>%
          dplyr::arrange(Scenario_code, Annee, Item)
        
        readr::write_delim(out, file, delim = ";")
      }
    )
    
    # -----------------------------------------------------------
    # Note
    # -----------------------------------------------------------
    output$note <- renderUI({
      da <- data_harvested()
      if (nrow(da) == 0) return(NULL)
      
      htmltools::HTML(glue::glue(
        "<p>
        This chart shows, for the selected country, the <strong>harvested area</strong> in the
        base-year (2018) and under the different scenarios.<br>
        Each stacked bar represents the total harvested area (in hectares), broken down
        by crop. The dashed line marks the harvested area in the base year.
        </p>
        <p>
        The cards below the chart summarise, for each scenario displayed, the total harvested area and its
        percentage change relative to the base year.
        </p>"
      ))
    })
    
  })
}
