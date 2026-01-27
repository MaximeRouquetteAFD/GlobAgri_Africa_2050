# R/4.1_mod_forest_stacked.R
# ---------------------------------------------------------------
# Forest land only — plotly bars + KPI cards
# Scenarios: STRICTLY driven by r_scenarios() (codes) + global order config
# No local redundancy/constraint logic
# ---------------------------------------------------------------

mod_forest_stacked_ui <- function(
    id,
    height = "360px",
    wrap = c("card", "none")
){
  ns <- NS(id)
  wrap <- match.arg(wrap)
  
  content <- tagList(
    h3("Forest land area by scenario"),
    
    plotly::plotlyOutput(ns("forest_plot"), height = height, width = "100%"),
    
    h3("Forest land area"),
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
          ns("dl_forest_csv"),
          label = tagList(icon("download"), "CSV")
        )
      )
    ),
    
    uiOutput(ns("note"))
  )
  
  if (wrap == "card") {
    div(class = "card", div(class = "card-body", content))
  } else {
    content
  }
}


mod_forest_stacked_server <- function(
    id,
    fact,
    r_country,
    unit = "mha",                 # "mha" (default) or "ha"
    r_scenarios,                  # reactive: vector of SCENARIO CODES to display
    ...
){
  moduleServer(id, function(input, output, session){
    
    # ------------------------------------------------------------------
    # Minimal dependency checks (no missing())
    # ------------------------------------------------------------------
    shiny::validate(
      shiny::need(exists("scenario_code", mode = "function"), "Missing scenario_code()"),
      shiny::need(exists("scenario_label", mode = "function"), "Missing scenario_label()"),
      shiny::need(exists("SCENARIO_LEVELS_DEFAULT", inherits = TRUE), "Missing SCENARIO_LEVELS_DEFAULT"),
      shiny::need(is.function(r_scenarios), "r_scenarios must be provided to this module.")
    )
    
    code_fun  <- scenario_code
    label_fun <- scenario_label
    
    # --- Guard robust on unit
    unit <- tolower(trimws(as.character(unit)[1]))
    if (!unit %in% c("mha", "ha")) unit <- "mha"
    
    AREA_ELT <- if (exists("AREA_ELEMENT", inherits = TRUE)) get("AREA_ELEMENT", inherits = TRUE) else "Area"
    
    scen_base <- if (exists("SCENARIO_BASE_YEAR_CODE", inherits = TRUE)) {
      code_fun(get("SCENARIO_BASE_YEAR_CODE", inherits = TRUE))
    } else {
      code_fun("Année de base")
    }
    
    # Robust recode if data contains extra UI label instead of code
    EXTRA_LABEL_TO_CODE <- if (exists("SCENARIOS_EXTRA_CHOICES", inherits = TRUE)) {
      setNames(
        unname(get("SCENARIOS_EXTRA_CHOICES", inherits = TRUE)),
        names(get("SCENARIOS_EXTRA_CHOICES", inherits = TRUE))
      )
    } else {
      NULL
    }
    
    levels_default <- code_fun(get("SCENARIO_LEVELS_DEFAULT", inherits = TRUE))
    
    # ------------------------------------------------------------------
    # Scenarios: wanted (from app) / present (in data) / show (intersection)
    # ------------------------------------------------------------------
    scen_wanted <- shiny::reactive({
      sc <- r_scenarios()
      shiny::validate(shiny::need(!is.null(sc) && length(sc) > 0, "No scenarios provided by r_scenarios()."))
      unique(code_fun(sc))
    })
    
    df_country_forest <- shiny::reactive({
      df <- fact %>%
        dplyr::filter(
          .data$Region == r_country(),
          stringr::str_to_lower(.data$Element) == stringr::str_to_lower(AREA_ELT),
          .data$Item == "Forest land"
        ) %>%
        dplyr::mutate(Scenario = code_fun(.data$Scenario))
      
      if (!is.null(EXTRA_LABEL_TO_CODE)) {
        df <- df %>% dplyr::mutate(
          Scenario = dplyr::recode(Scenario, !!!EXTRA_LABEL_TO_CODE, .default = Scenario)
        )
      }
      
      df
    }) %>% bindCache(r_country())
    
    scen_present <- shiny::reactive({
      unique(as.character(df_country_forest()$Scenario))
    }) %>% bindCache(r_country())
    
    scen_show <- shiny::reactive({
      wanted  <- scen_wanted()
      present <- scen_present()
      out <- wanted[wanted %in% present]
      shiny::validate(shiny::need(length(out) > 0, "No requested scenario is available for Forest land in this country."))
      out
    }) %>% bindCache(r_country())
    
    scen_levels_effective <- shiny::reactive({
      show <- scen_show()
      ordered <- levels_default[levels_default %in% show]
      missing <- setdiff(show, ordered) # robustness only
      c(ordered, missing)
    }) %>% bindCache(r_country())
    
    # Scalar cache key for bindCache (do NOT pass vectors)
    scen_key <- shiny::reactive({
      paste(scen_levels_effective(), collapse = "|")
    }) %>% bindCache(r_country())
    
    # ------------------------------------------------------------------
    # 1) Year used per scenario (same business logic; scenarios = scen_levels_effective())
    # ------------------------------------------------------------------
    years_by_scenario <- shiny::reactive({
      lvls <- scen_levels_effective()
      shiny::validate(shiny::need(length(lvls) > 0, "No forest scenarios available for this country."))
      
      dat <- df_country_forest() %>%
        dplyr::filter(.data$Scenario %in% lvls)
      
      base_year <- if (any(
        dat$Scenario == scen_base &
        dat$Year == 2018 &
        !is.na(dat$Value)
      )) {
        2018
      } else {
        suppressWarnings(max(dat$Year[dat$Scenario == scen_base & !is.na(dat$Value)], na.rm = TRUE))
      }
      
      others <- dat %>%
        dplyr::filter(.data$Scenario != scen_base) %>%
        dplyr::group_by(.data$Scenario) %>%
        dplyr::summarise(
          year_used = suppressWarnings(max(.data$Year[!is.na(.data$Value)], na.rm = TRUE)),
          .groups   = "drop"
        )
      
      dplyr::bind_rows(
        tibble::tibble(Scenario = scen_base, year_used = base_year),
        others
      ) %>%
        dplyr::filter(is.finite(.data$year_used)) %>%
        dplyr::mutate(Scenario = factor(as.character(.data$Scenario), levels = lvls)) %>%
        dplyr::arrange(.data$Scenario)
    }) %>% bindCache(r_country(), scen_key())
    
    # ------------------------------------------------------------------
    # 2) Forest data (ha) — unchanged business logic (Value * 1000)
    # ------------------------------------------------------------------
    data_forest_all <- shiny::reactive({
      yrs <- years_by_scenario()
      shiny::validate(shiny::need(nrow(yrs) > 0, "No year available for forest land area."))
      
      lvls <- levels(yrs$Scenario) %||% as.character(yrs$Scenario)
      
      out <- df_country_forest() %>%
        dplyr::inner_join(
          yrs %>% dplyr::mutate(Scenario = as.character(.data$Scenario)),
          by = "Scenario"
        ) %>%
        dplyr::filter(.data$Year == .data$year_used) %>%
        dplyr::mutate(
          Scenario = factor(as.character(.data$Scenario), levels = lvls),
          Value    = dplyr::if_else(!is.na(.data$Value) & .data$Value < 0, 0, .data$Value)
        ) %>%
        dplyr::group_by(.data$Scenario, .data$year_used) %>%
        dplyr::summarise(value_ha = sum(.data$Value, na.rm = TRUE) * 1000, .groups = "drop") %>%
        dplyr::arrange(.data$Scenario)
      
      shiny::validate(shiny::need(nrow(out) > 0, "No 'Forest land' data for this country."))
      out
    }) %>% bindCache(r_country(), scen_key())
    
    # ------------------------------------------------------------------
    # 3) Data for plot + KPI (unit conversion) — labels only for UI
    # ------------------------------------------------------------------
    data_forest_used <- shiny::reactive({
      df <- data_forest_all()
      req(nrow(df) > 0)
      
      lvls <- levels(df$Scenario)
      shiny::validate(shiny::need(length(lvls) > 0, "No scenario available for forest land."))
      
      scale <- if (identical(unit, "mha")) 1e-6 else 1
      
      df %>%
        dplyr::mutate(
          Scenario    = factor(as.character(.data$Scenario), levels = lvls),
          Scenario_ui = label_fun(as.character(.data$Scenario)),
          value_plot  = .data$value_ha * scale
        ) %>%
        droplevels() %>%
        dplyr::arrange(.data$Scenario)
    }) %>% bindCache(r_country(), scen_key(), unit)
    
    # ------------------------------------------------------------------
    # 4) Plotly bars — x tick labels via scenario_label()
    # ------------------------------------------------------------------
    output$forest_plot <- plotly::renderPlotly({
      df <- data_forest_used()
      req(nrow(df) > 0)
      
      scen_codes <- levels(df$Scenario)
      tick_vals  <- seq_along(scen_codes)
      tick_txt   <- vapply(scen_codes, label_fun, character(1))
      
      bar_w     <- 0.85
      col_forest <- unname(area_colors_for("Forest land")[1])
      y_title   <- if (unit == "mha") "million ha" else "ha"
      
      p <- plotly::plot_ly() %>%
        plotly::layout(
          barmode = "stack",
          xaxis = list(
            title    = "",
            type     = "linear",
            tickmode = "array",
            tickvals = tick_vals,
            ticktext = tick_txt
          ),
          yaxis = list(
            title = y_title,
            separatethousands = TRUE
          ),
          showlegend = TRUE,
          legend = list(title = list(text = ""))
        )
      
      p <- plotly::add_bars(
        p, data = df,
        x = ~match(Scenario, scen_codes),
        y = ~value_plot,
        width = bar_w,
        name = "Forest land",
        showlegend = TRUE,
        marker = list(color = col_forest),
        textposition = "none",
        customdata = ~Scenario_ui,
        hovertemplate = paste0(
          "%{customdata}<br>",
          "Forest land: %{y}<extra></extra>"
        )
      )
      
      if (exists("plotly_apply_global_theme", mode = "function")) {
        p <- plotly_apply_global_theme(p, bg = "transparent", grid = "y")
      }
      
      p
    })
    
    # ------------------------------------------------------------------
    # 5) KPI cards (value + deltas vs base year)
    #    Base year: remove "Vs base year" but keep same layout height (placeholder)
    # ------------------------------------------------------------------
    kpi_forest <- shiny::reactive({
      df_ha <- data_forest_all()
      req(nrow(df_ha) > 0)
      
      base_ha <- df_ha %>%
        dplyr::filter(as.character(.data$Scenario) == scen_base) %>%
        dplyr::pull(.data$value_ha)
      base_ha <- if (length(base_ha)) base_ha[1] else NA_real_
      
      df_ha %>%
        dplyr::mutate(
          diff_ha  = if (is.finite(base_ha)) .data$value_ha - base_ha else NA_real_,
          diff_pct = if (is.finite(base_ha) && base_ha > 0) 100 * (.data$value_ha - base_ha) / base_ha else NA_real_
        )
    }) %>% bindCache(r_country(), scen_key())
    
    output$kpi_cards <- renderUI({
      dat <- kpi_forest()
      if (is.null(dat) || nrow(dat) == 0) return(NULL)
      
      fmt_ha <- function(x){
        if (!is.finite(x)) return("—")
        format(round(x), big.mark = " ", scientific = FALSE, trim = TRUE)
      }
      fmt_ha_signed <- function(x){
        if (!is.finite(x)) return("—")
        paste0(if (x > 0) "+" else if (x < 0) "−" else "", fmt_ha(abs(x)))
      }
      fmt_pct <- function(p){
        if (!is.finite(p)) return("—")
        paste0(ifelse(p >= 0, "+", ""), formatC(p, digits = 0, format = "f"), "%")
      }
      
      base_lbl <- label_fun(scen_base)
      
      cards <- lapply(seq_len(nrow(dat)), function(i){
        sc_code <- as.character(dat$Scenario[i])
        sc_lbl  <- label_fun(sc_code)
        
        val <- dat$value_ha[i]
        dlt <- dat$diff_pct[i]
        dha <- dat$diff_ha[i]
        
        # Line 1 (ABS delta): keep placeholder for base
        subline_abs <- if (identical(sc_code, scen_base)) {
          p(class = "u-sub", htmltools::HTML("&nbsp;"))
        } else if (is.finite(dha)) {
          cls <- if (dha > 0) "up" else if (dha < 0) "down" else ""
          p(
            class = "u-sub",
            glue::glue("Δ vs {base_lbl}: "),
            span(class = cls, paste0(fmt_ha_signed(dha), " ha"))
          )
        } else {
          p(class = "u-sub", glue::glue("Δ vs {base_lbl}: —"))
        }
        
        # Line 2 (% delta): remove text for base, but keep placeholder to preserve card height
        subline_pct <- if (identical(sc_code, scen_base)) {
          p(class = "u-sub", htmltools::HTML("&nbsp;"))
        } else {
          delta_pct_tag <- if (!is.finite(dlt)) {
            "—"
          } else if (dlt > 0) {
            span(class = "up", fmt_pct(dlt))
          } else if (dlt < 0) {
            span(class = "down", fmt_pct(dlt))
          } else {
            "0%"
          }
          p(class = "u-sub", glue::glue("Vs {base_lbl}: "), delta_pct_tag)
        }
        
        div(
          class = "u-card u-card--flat u-card--hover",
          div(
            class = "u-box",
            p(class = "u-title", sc_lbl),
            p(
              class = "u-value",
              fmt_ha(val),
              span(class = "u-unit", "ha")
            ),
            subline_abs,
            subline_pct
          )
        )
      })
      
      div(class = "u-row", do.call(tagList, cards))
    })
    
    # ------------------------------------------------------------------
    # 6) Export CSV (ha): codes + labels
    # ------------------------------------------------------------------
    output$dl_forest_csv <- downloadHandler(
      filename = function(){
        paste0("Forest_land_", gsub(" ", "_", r_country()), ".csv")
      },
      content = function(file){
        df <- data_forest_all()
        req(nrow(df) > 0)
        
        out <- df %>%
          dplyr::transmute(
            Pays           = r_country(),
            Scenario_code  = as.character(.data$Scenario),
            Scenario_label = label_fun(as.character(.data$Scenario)),
            Annee          = .data$year_used,
            Item           = "Forest land",
            Valeur_ha      = .data$value_ha
          )
        
        readr::write_delim(out, file, delim = ";")
      }
    )
    
    # ------------------------------------------------------------------
    # 7) Note
    # ------------------------------------------------------------------
    output$note <- renderUI({
      df <- data_forest_used()
      if (nrow(df) == 0) return(NULL)
      
      htmltools::HTML(
        "<p>
          This chart shows, for the selected country, the forest land area by scenario.
          The cards below show the corresponding values and their percentage change compared with the base year.
        </p>"
      )
    })
    
    # If some downstream module still expects it
    return(list(
      scen_levels_effective = scen_levels_effective
    ))
  })
}
