# R/6.2_mod_emissions_stacked.R
# ----------------------------------------------------------
# Agricultural GHG emissions by scenario (without land use change)
#
# SCENARIOS (IMPORTANT):
# - No local scenario reconstruction.
# - Display exactly r_scenarios() (codes), optionally intersected with scenarios present in fact
#   for the country and the selected gas view.
# - Stable order via SCENARIO_LEVELS_DEFAULT.
# - Use scenario_label(code) for UI only (axis/hover/legend).
# - bindCache keys must be scalar -> scen_show_key() (preferred) or local scalar fallback.
# ----------------------------------------------------------

mod_emissions_stacked_ui <- function(id,
                                     plot_height = "420px",
                                     pie_height  = "500px"){
  ns <- NS(id)
  
  tagList(
    div(
      class = "card",
      div(
        class = "card-body",
        
        h3("Agricultural GHG emissions by scenario (without land use change)"),
        tags$div(style = "height:8px"),
        
        # ---- Controls (gas + stack dim when gas != co2e)
        div(
          class = "u-controls u-controls--inline",
          
          div(
            style = "display:flex; flex-direction:column; gap:4px;",
            tags$span(
              style = "font-size:12px; font-weight:600; color: var(--title-color);",
              "GHG shown"
            ),
            selectInput(
              inputId = ns("gas"),
              label   = NULL,
              choices = c(
                "Total in CO2 equivalent (CO2e)" = "co2e",
                "Methane (CH4)"                 = "ch4",
                "Carbon dioxide (CO2)"          = "co2",
                "Nitrous oxide (N2O)"           = "n2o"
              ),
              selected = "co2e",
              width = "320px"
            )
          ),
          
          conditionalPanel(
            condition = paste0("input['", ns("gas"), "'] != 'co2e'"),
            radioButtons(
              inputId  = ns("stack_dim"),
              label    = "",
              choices  = c("Stack by Item" = "Item",
                           "Stack by Animal" = "Animal"),
              selected = "Item",
              inline   = TRUE
            )
          )
        ),
        
        tags$div(style = "height:8px"),
        
        # ---- Top stacked bars
        plotly::plotlyOutput(ns("plot"), height = plot_height),
        
        tags$div(style = "height:24px"),
        tags$hr(),
        
        # ---- Evolution block
        h3("Change in agricultural GHG emissions between 2018 and 2050"),
        
        div(
          class = "u-controls u-controls--inline",
          radioButtons(
            inputId = ns("facet_mode"),
            label   = "",
            choices = c("By emission source" = "source",
                        "By scenario"        = "scenario"),
            selected = "source",
            inline   = TRUE
          ),
          checkboxInput(
            inputId = ns("show_pct_axis"),
            label   = "Show in percentages of total emissions",
            value   = FALSE
          )
        ),
        
        plotly::plotlyOutput(ns("emis_groups"), height = pie_height),
        
        tags$div(style = "height:24px"),
        
        div(
          class = "u-actions",
          downloadLink(
            ns("dl_csv"),
            label = tagList(icon("download"), "CSV")
          )
        ),
        
        tags$div(style = "height:8px"),
        uiOutput(ns("note"))
      )
    )
  )
}


mod_emissions_stacked_server <- function(
    id,
    fact,
    r_country,
    r_scenarios,
    scen_show_key = NULL
){
  moduleServer(id, function(input, output, session){
    
    `%||%` <- function(x, y) if (is.null(x)) y else x
    
    # ------------------------------------------------------------------ #
    # 0. Constantes + helpers
    # ------------------------------------------------------------------ #
    BASELINE_CODE <- scenario_code("Année de base")
    
    is_blank <- function(x) is.na(x) | trimws(x) == ""
    
    clean_emission_item <- function(x){
      dplyr::recode(
        x,
        "Fertilizer application and production and pesticides" =
          "Fertilizer/Production/Pesticides",
        .default = x
      )
    }
    
    gas_choice <- reactive({
      g <- input$gas
      if (is.null(g) || !g %in% c("co2e","ch4","co2","n2o")) "co2e" else g
    })
    
    stack_dim_choice <- reactive({
      if (gas_choice() == "co2e") return("Item")
      d <- input$stack_dim
      if (is.null(d) || !d %in% c("Item","Animal")) "Item" else d
    })
    
    gas_label <- reactive({
      switch(
        gas_choice(),
        "co2e" = "CO2e",
        "ch4"  = "CH4",
        "co2"  = "CO2",
        "n2o"  = "N2O"
      )
    })
    
    prefer_blank_level <- function(df, col){
      if (!col %in% names(df)) return(df)
      v <- as.character(df[[col]])
      blank <- is_blank(v)
      if (any(blank)) df[blank, , drop = FALSE] else df
    }
    
    # Keep rows where Element contains CH4/CO2/N2O as a "word"
    element_filter <- function(df, gas){
      if (!"Element" %in% names(df)) return(df[0, , drop = FALSE])
      
      if (gas == "co2e") {
        return(df[df$Element == "Emissions", , drop = FALSE])
      }
      
      token <- toupper(gas) # CH4 / CO2 / N2O
      el <- as.character(df$Element)
      keep <- grepl(paste0("\\b", token, "\\b"), el, ignore.case = TRUE)
      df[keep, , drop = FALSE]
    }
    
    # Collapse selected food items into "On-farm energy use" when Element indicates On-Farm energy use
    ITEMS_ONFARM <- unique(c(
      "Bovine Meat",
      "Milk including butter",
      "Mutton & Goat Meat",
      "Eggs",
      "Poultry Meat"
    ))
    
    collapse_onfarm_energy_use <- function(df){
      if (!all(c("Item","Element") %in% names(df))) return(df)
      df %>%
        dplyr::mutate(
          Item = dplyr::if_else(
            Item %in% ITEMS_ONFARM &
              grepl("On[- ]Farm energy use", Element, ignore.case = TRUE),
            "On-farm energy use",
            Item
          )
        )
    }
    
    # Robust subsetting to avoid losing gas rows
    subset_emis_raw <- function(df, gas, stack_dim){
      df <- element_filter(df, gas)
      if (nrow(df) == 0) return(df)
      
      # CO2e: keep historical behaviour (aggregated only)
      if (gas == "co2e") {
        if ("System" %in% names(df)) df <- df %>% dplyr::filter(is_blank(System))
        if ("Animal" %in% names(df)) df <- df %>% dplyr::filter(is_blank(Animal))
        return(df)
      }
      
      # Gas-specific: prefer aggregated System if present, else keep all
      df <- prefer_blank_level(df, "System")
      
      # Handle Animal level depending on stack choice (avoid double-counting)
      if ("Animal" %in% names(df)) {
        if (stack_dim == "Animal") {
          if (any(!is_blank(df$Animal))) df <- df %>% dplyr::filter(!is_blank(Animal))
        } else {
          if (any(is_blank(df$Animal))) df <- df %>% dplyr::filter(is_blank(Animal))
        }
      }
      
      df
    }
    
    ensure_palette_wrapper <- function(){
      exists("emissions_module_colors_for", mode = "function")
    }
    
    # ------------------------------------------------------------------ #
    # A. Scenario plumbing (single source of truth)
    # ------------------------------------------------------------------ #
    
    # Scalar key for bindCache (never cache on a vector)
    scen_set_key <- reactive({
      req(r_scenarios())
      if (!is.null(scen_show_key)) {
        k <- scen_show_key()
        req(length(k) == 1)
        return(as.character(k))
      }
      paste(r_scenarios(), collapse = "|")
    })
    
    # Scenarios actually used by THIS module for the current selection:
    # r_scenarios() (codes) ∩ SCENARIO_LEVELS_DEFAULT ∩ scenarios present in fact after gas filtering.
    scenarios_effective <- reactive({
      req(fact, r_country(), r_scenarios())
      
      shiny::validate(
        shiny::need(all(c("Region","Scenario","Element","Item","Value") %in% names(fact)),
                    "Emissions module: required columns are missing.")
      )
      
      scen_from_app <- intersect(SCENARIO_LEVELS_DEFAULT, r_scenarios())
      
      df0 <- fact %>%
        dplyr::filter(
          Region   == r_country(),
          Scenario %in% scen_from_app,
          Item     != "Land use change"
        )
      
      df0 <- subset_emis_raw(df0, gas_choice(), stack_dim_choice())
      scen_in_data <- unique(df0$Scenario)
      
      scen_keep <- scen_from_app[scen_from_app %in% scen_in_data]
      scen_keep
    }) %>%
      bindCache(r_country(), scen_set_key(), gas_choice(), stack_dim_choice())
    
    # Helper for UI labels
    scenario_ui_levels <- reactive({
      sc <- scenarios_effective()
      if (length(sc) == 0) character(0) else vapply(sc, scenario_label, FUN.VALUE = character(1))
    })
    
    # ------------------------------------------------------------------ #
    # 1. Données agrégées pour les barres empilées (TOP)
    # ------------------------------------------------------------------ #
    emissions_data <- reactive({
      req(r_country())
      scen_keep <- scenarios_effective()
      g         <- gas_choice()
      dimv      <- stack_dim_choice()
      
      shiny::validate(
        shiny::need(length(scen_keep) > 0, "No scenario available for this country / gas selection.")
      )
      
      df0 <- fact %>%
        dplyr::filter(
          Region   == r_country(),
          Scenario %in% scen_keep,
          Item     != "Land use change"
        )
      
      df0 <- subset_emis_raw(df0, g, dimv)
      
      shiny::validate(
        shiny::need(nrow(df0) > 0,
                    paste0("No data found for selected gas (", gas_label(), ")."))
      )
      
      group_var <- if (g == "co2e") "Item" else dimv
      
      if (g != "co2e" && group_var == "Item") {
        df0 <- collapse_onfarm_energy_use(df0)
      }
      
      df0 <- df0 %>% dplyr::mutate(Item = clean_emission_item(Item))
      
      # Aggregate for plot
      if (group_var == "Animal") {
        shiny::validate(shiny::need("Animal" %in% names(df0), "Column 'Animal' not available."))
        dfA <- df0 %>%
          dplyr::filter(!is_blank(Animal)) %>%
          dplyr::group_by(Scenario, Group = Animal) %>%
          dplyr::summarise(Value = sum(Value, na.rm = TRUE), .groups = "drop")
      } else {
        dfA <- df0 %>%
          dplyr::group_by(Scenario, Group = Item) %>%
          dplyr::summarise(Value = sum(Value, na.rm = TRUE), .groups = "drop")
      }
      
      # Stable scenario order: already ordered via SCENARIO_LEVELS_DEFAULT in scenarios_effective()
      ordre_scenarios <- scenarios_effective()
      
      group_levels_chr <- dfA %>%
        dplyr::distinct(Group) %>%
        dplyr::pull(Group) %>%
        as.character()
      
      # Ordering groups (unchanged logic)
      if (group_var == "Item" && g == "co2e" && exists("EMISSIONS_COLORS")) {
        known_first  <- intersect(names(EMISSIONS_COLORS), group_levels_chr)
        others       <- setdiff(group_levels_chr, known_first)
        group_levels <- c(known_first, others)
      } else {
        if (BASELINE_CODE %in% dfA$Scenario) {
          ord <- dfA %>%
            dplyr::filter(Scenario == BASELINE_CODE) %>%
            dplyr::arrange(dplyr::desc(Value)) %>%
            dplyr::pull(Group) %>%
            as.character()
          group_levels <- unique(c(ord, setdiff(group_levels_chr, ord)))
        } else {
          ord <- dfA %>%
            dplyr::group_by(Group) %>%
            dplyr::summarise(T = sum(Value, na.rm = TRUE), .groups = "drop") %>%
            dplyr::arrange(dplyr::desc(T)) %>%
            dplyr::pull(Group) %>%
            as.character()
          group_levels <- unique(ord)
        }
      }
      
      # ---- Display in kt
      unit_plot <- if (g == "co2e") "kt CO\u2082e" else paste0("kt ", gas_label())
      
      dfA %>%
        dplyr::mutate(
          Scenario = factor(Scenario, levels = ordre_scenarios),
          Scenario_ui = factor(vapply(as.character(Scenario), scenario_label, FUN.VALUE = character(1)),
                               levels = vapply(ordre_scenarios, scenario_label, FUN.VALUE = character(1))),
          Group    = factor(Group, levels = group_levels),
          Value_plot = Value / 1e3,
          UnitLabPlot = unit_plot,
          hover = paste0(
            "<b>Scenario:</b> ", vapply(as.character(Scenario), scenario_label, FUN.VALUE = character(1)), "<br>",
            "<b>", ifelse(group_var == "Animal","Animal","Item"), ":</b> ", as.character(Group), "<br>",
            "<b>Emissions (", gas_label(), "):</b> ",
            scales::comma(Value_plot, accuracy = 0.1, big.mark = " "),
            " ", unit_plot
          )
        ) %>%
        dplyr::arrange(Scenario, Group)
    }) %>%
      bindCache(r_country(), scen_set_key(), gas_choice(), stack_dim_choice())
    
    # ------------------------------------------------------------------ #
    # 2. Années utilisées par scénario pour le graph d’évolution
    # ------------------------------------------------------------------ #
    years_by_scenario_emis <- reactive({
      req(r_country())
      scen_keep <- scenarios_effective()
      g         <- gas_choice()
      dimv      <- stack_dim_choice()
      
      shiny::validate(
        shiny::need("Year" %in% names(fact),
                    "No 'Year' column available for emissions time series.")
      )
      
      dat <- fact %>%
        dplyr::filter(
          Region   == r_country(),
          Scenario %in% scen_keep
        )
      
      dat <- subset_emis_raw(dat, g, dimv)
      
      shiny::validate(
        shiny::need(nrow(dat) > 0,
                    paste0("No data available for emissions time series (", gas_label(), ")."))
      )
      
      base_year <- if (any(dat$Scenario == BASELINE_CODE & dat$Year == 2018 & !is.na(dat$Value))) {
        2018
      } else {
        suppressWarnings(max(dat$Year[dat$Scenario == BASELINE_CODE & !is.na(dat$Value)], na.rm = TRUE))
      }
      
      others <- dat %>%
        dplyr::filter(Scenario != BASELINE_CODE) %>%
        dplyr::group_by(Scenario) %>%
        dplyr::summarise(
          year_used = suppressWarnings(max(Year[!is.na(Value)], na.rm = TRUE)),
          .groups   = "drop"
        )
      
      dplyr::bind_rows(
        tibble::tibble(Scenario = BASELINE_CODE, year_used = base_year),
        others
      ) %>%
        dplyr::filter(is.finite(year_used)) %>%
        dplyr::mutate(
          Scenario = factor(Scenario, levels = scenarios_effective())
        ) %>%
        dplyr::arrange(Scenario)
    }) %>%
      bindCache(r_country(), scen_set_key(), gas_choice(), stack_dim_choice())
    
    # ------------------------------------------------------------------ #
    # 3. Données long pour le graph 2018 -> 2050
    # ------------------------------------------------------------------ #
    emissions_groups <- reactive({
      yrs <- years_by_scenario_emis()
      shiny::validate(shiny::need(nrow(yrs) > 0, "No year information available for emissions."))
      
      g    <- gas_choice()
      dimv <- stack_dim_choice()
      
      base_year <- yrs$year_used[yrs$Scenario == BASELINE_CODE]
      target_year <- suppressWarnings(max(yrs$year_used[yrs$Scenario != BASELINE_CODE], na.rm = TRUE))
      
      shiny::validate(
        shiny::need(length(base_year) == 1 && is.finite(base_year), "Base year for emissions not found."),
        shiny::need(is.finite(target_year), "Target year for emissions not found.")
      )
      
      scen_used <- unique(as.character(yrs$Scenario[yrs$Scenario != BASELINE_CODE]))
      shiny::validate(shiny::need(length(scen_used) > 0, "No non-baseline scenario available for evolution chart."))
      
      group_var <- if (g == "co2e") "Item" else dimv
      
      # ---- Base year data
      base_raw <- fact %>%
        dplyr::filter(
          Region   == r_country(),
          Scenario == BASELINE_CODE,
          Year     == base_year,
          Item     != "Land use change"
        )
      base_raw <- subset_emis_raw(base_raw, g, dimv)
      shiny::validate(shiny::need(nrow(base_raw) > 0, "No baseline emissions available for base year."))
      
      if (g != "co2e" && group_var == "Item") base_raw <- collapse_onfarm_energy_use(base_raw)
      base_raw <- base_raw %>% dplyr::mutate(Item = clean_emission_item(Item))
      
      if (group_var == "Item") {
        base_emis <- base_raw %>%
          dplyr::group_by(group = Item) %>%
          dplyr::summarise(emis_2018 = sum(Value, na.rm = TRUE), .groups = "drop")
      } else {
        shiny::validate(shiny::need("Animal" %in% names(base_raw), "Column 'Animal' not available."))
        base_emis <- base_raw %>%
          dplyr::filter(!is_blank(Animal)) %>%
          dplyr::group_by(group = Animal) %>%
          dplyr::summarise(emis_2018 = sum(Value, na.rm = TRUE), .groups = "drop")
      }
      shiny::validate(shiny::need(nrow(base_emis) > 0, "No baseline data after grouping."))
      
      # ---- Target year data
      targ_raw <- fact %>%
        dplyr::filter(
          Region   == r_country(),
          Scenario %in% scen_used,
          Year     == target_year,
          Item     != "Land use change"
        )
      targ_raw <- subset_emis_raw(targ_raw, g, dimv)
      if (g != "co2e" && group_var == "Item") targ_raw <- collapse_onfarm_energy_use(targ_raw)
      targ_raw <- targ_raw %>% dplyr::mutate(Item = clean_emission_item(Item))
      
      if (group_var == "Item") {
        emis_target <- targ_raw %>%
          dplyr::group_by(Scenario, group = Item) %>%
          dplyr::summarise(emis_2050 = sum(Value, na.rm = TRUE), .groups = "drop")
      } else {
        emis_target <- targ_raw %>%
          dplyr::filter(!is_blank(Animal)) %>%
          dplyr::group_by(Scenario, group = Animal) %>%
          dplyr::summarise(emis_2050 = sum(Value, na.rm = TRUE), .groups = "drop")
      }
      
      order_groups <- base_emis %>%
        dplyr::arrange(dplyr::desc(emis_2018)) %>%
        dplyr::pull(group)
      
      base_long <- base_emis %>%
        tidyr::crossing(Scenario = scen_used) %>%
        dplyr::transmute(group, Scenario, Year = base_year, emis = emis_2018)
      
      scen_long <- emis_target %>%
        dplyr::transmute(group, Scenario, Year = target_year, emis = emis_2050)
      
      df <- dplyr::bind_rows(base_long, scen_long) %>%
        dplyr::mutate(
          Scenario = factor(Scenario, levels = scen_used),
          Scenario_ui = factor(
            vapply(as.character(Scenario), scenario_label, FUN.VALUE = character(1)),
            levels = vapply(scen_used, scenario_label, FUN.VALUE = character(1))
          ),
          group    = factor(group, levels = order_groups)
        )
      
      base_vals <- df %>%
        dplyr::filter(Year == base_year) %>%
        dplyr::select(group, Scenario, emis_base = emis)
      
      df %>%
        dplyr::left_join(base_vals, by = c("group", "Scenario")) %>%
        dplyr::mutate(
          pct_change = dplyr::if_else(
            is.finite(emis_base) & emis_base != 0,
            100 * (emis - emis_base) / emis_base,
            NA_real_
          )
        )
    }) %>%
      bindCache(r_country(), scen_set_key(), gas_choice(), stack_dim_choice())
    
    # ------------------------------------------------------------------ #
    # 4. TOP stacked bars — colors via R/02 + display in kt
    # ------------------------------------------------------------------ #
    output$plot <- plotly::renderPlotly({
      df <- emissions_data()
      req(nrow(df) > 0)
      
      shiny::validate(
        shiny::need(
          ensure_palette_wrapper(),
          "Missing palette helper: emissions_module_colors_for() not found. Source R/02_utils_palette.R."
        )
      )
      
      df <- df %>% dplyr::filter(is.finite(Value_plot))
      req(nrow(df) > 0)
      
      g         <- gas_choice()
      dimv      <- stack_dim_choice()
      group_var <- if (g == "co2e") "Item" else dimv
      
      lv <- levels(df$Group)
      if (is.null(lv) || !length(lv)) lv <- unique(as.character(df$Group))
      
      pal <- emissions_module_colors_for(lv, breakdown = group_var)
      
      unit_plot <- unique(df$UnitLabPlot)
      if (length(unit_plot) != 1) unit_plot <- if (g == "co2e") "kt CO\u2082e" else paste0("kt ", gas_label())
      y_title <- paste0("Emissions (", unit_plot, ")")
      
      scen_codes <- levels(df$Scenario)
      scen_ticks <- vapply(scen_codes, scenario_label, FUN.VALUE = character(1))
      
      p <- plotly::plot_ly()
      
      for (grp in lv) {
        dfg <- df %>% dplyr::filter(as.character(Group) == grp)
        if (nrow(dfg) == 0) next
        
        p <- p %>%
          plotly::add_trace(
            data = dfg,
            x    = ~as.character(Scenario),
            y    = ~Value_plot,
            type = "bar",
            name = grp,
            marker = list(color = unname(pal[grp])),
            text   = ~hover,
            textposition = "none",
            hoverinfo = "text",
            showlegend = TRUE,
            inherit = FALSE
          )
      }
      
      p <- p %>%
        plotly::layout(
          barmode = "relative",
          showlegend = TRUE,
          legend = list(
            orientation = "h",
            x = 0.20,
            y = 1.12,
            title = list(text = "")
          ),
          yaxis = list(
            title = y_title,
            rangemode = "tozero",
            tickformat = ",.0f",
            separatethousands = TRUE,
            zeroline = TRUE
          ),
          xaxis = list(
            title = "",
            type = "category",
            categoryorder = "array",
            categoryarray = scen_codes,
            tickmode = "array",
            tickvals = scen_codes,
            ticktext = scen_ticks,
            tickfont = list(size = 13)
          )
        )
      
      # Totals per scenario (in kt)
      df_tot <- df %>%
        dplyr::group_by(Scenario) %>%
        dplyr::summarise(Total = sum(Value_plot, na.rm = TRUE), .groups = "drop")
      
      base_total <- df_tot %>%
        dplyr::filter(as.character(Scenario) == BASELINE_CODE) %>%
        dplyr::pull(Total)
      
      if (length(base_total) == 0 || is.na(base_total) || base_total == 0) {
        df_tot <- df_tot %>%
          dplyr::mutate(label = scales::comma(Total, accuracy = 0.1, big.mark = " "))
      } else {
        df_tot <- df_tot %>%
          dplyr::mutate(
            pct_change = dplyr::if_else(as.character(Scenario) == BASELINE_CODE, NA_real_, 100 * (Total / base_total - 1)),
            sign_symbol = dplyr::case_when(
              is.na(pct_change) ~ "",
              pct_change > 0    ~ "+",
              TRUE              ~ ""
            ),
            pct_str = dplyr::case_when(
              is.na(pct_change) ~ "",
              TRUE ~ paste0(" (", sign_symbol, sprintf("%.0f", pct_change), "%)")
            ),
            label = paste0(scales::comma(Total, accuracy = 0.1, big.mark = " "), pct_str)
          )
      }
      
      df_tot <- df_tot %>%
        dplyr::mutate(Scenario = as.character(Scenario))
      
      df_tot_pos <- df_tot %>% dplyr::filter(Total >= 0)
      df_tot_neg <- df_tot %>% dplyr::filter(Total < 0)
      
      if (nrow(df_tot_pos) > 0) {
        p <- p %>%
          plotly::add_trace(
            data = df_tot_pos,
            x    = ~Scenario,
            y    = ~Total,
            type = "scatter",
            mode = "text",
            text = ~label,
            textposition = "top center",
            textfont = list(size = 13),
            hoverinfo = "none",
            showlegend = FALSE,
            inherit = FALSE
          )
      }
      
      if (nrow(df_tot_neg) > 0) {
        p <- p %>%
          plotly::add_trace(
            data = df_tot_neg,
            x    = ~Scenario,
            y    = ~Total,
            type = "scatter",
            mode = "text",
            text = ~label,
            textposition = "bottom center",
            textfont = list(size = 13),
            hoverinfo = "none",
            showlegend = FALSE,
            inherit = FALSE
          )
      }
      
      if (exists("plotly_apply_global_theme", mode = "function")) {
        p <- plotly_apply_global_theme(p, bg = "transparent", grid = "y")
      } else if (exists("plotly_theme_transparent", mode = "function")) {
        p <- plotly_theme_transparent(p)
      }
      
      p
    }) %>%
      bindCache(r_country(), scen_set_key(), gas_choice(), stack_dim_choice())
    
    # ------------------------------------------------------------------ #
    # 5. Evolution 2018 -> 2050 (kt when absolute, % when checkbox)
    # ------------------------------------------------------------------ #
    output$emis_groups <- plotly::renderPlotly({
      df <- emissions_groups()
      req(nrow(df) > 0)
      
      mode <- input$facet_mode
      if (is.null(mode) || !mode %in% c("source", "scenario")) mode <- "source"
      show_pct <- isTRUE(input$show_pct_axis)
      
      base_year   <- min(df$Year, na.rm = TRUE)
      target_year <- max(df$Year, na.rm = TRUE)
      
      if (mode == "source" && show_pct) {
        shiny::validate(shiny::need(FALSE, "This graph is not relevant"))
      }
      
      g         <- gas_choice()
      dimv      <- stack_dim_choice()
      group_var <- if (g == "co2e") "Item" else dimv
      group_title <- if (group_var == "Animal") "Animal" else "Emission source"
      
      df <- df %>%
        dplyr::mutate(
          delta_label = dplyr::if_else(
            is.na(pct_change),
            "—",
            paste0(sprintf("%+.1f", pct_change), " %")
          )
        )
      
      df <- df %>%
        dplyr::group_by(Scenario_ui, Year) %>%
        dplyr::mutate(
          total_year_scen = sum(emis, na.rm = TRUE),
          share_pct = dplyr::if_else(total_year_scen > 0, 100 * emis / total_year_scen, NA_real_),
          share_label = dplyr::if_else(is.na(share_pct), "—", paste0(round(share_pct, 1), " %"))
        ) %>%
        dplyr::ungroup()
      
      if (show_pct) {
        y_lab <- "Share of total emissions (%)"
        df <- df %>%
          dplyr::mutate(
            y_val = share_pct,
            tooltip = paste0(
              group_title, ": ", group, "<br>",
              "Scenario: ", as.character(Scenario_ui), "<br>",
              "Year: ", Year, "<br>",
              "Share of total: ", share_label, "<br>",
              "Change vs base year (same group & scenario): ", delta_label
            )
          )
      } else {
        y_unit <- if (g == "co2e") "kt CO\u2082e" else paste0("kt ", gas_label())
        y_lab  <- paste0("Emissions (", y_unit, ")")
        
        df <- df %>%
          dplyr::mutate(
            emis_kt = emis / 1e3,
            y_val = emis_kt,
            value_label = paste0(scales::comma(round(emis_kt, 1), big.mark = " "), " ", y_unit),
            tooltip = paste0(
              group_title, ": ", group, "<br>",
              "Scenario: ", as.character(Scenario_ui), "<br>",
              "Year: ", Year, "<br>",
              "Emissions: ", value_label, "<br>",
              "Change vs base year (same group & scenario): ", delta_label
            )
          )
      }
      
      df <- df %>% dplyr::filter(is.finite(y_val))
      req(nrow(df) > 0)
      
      df_labels_target <- df %>% dplyr::filter(Year == target_year)
      df_labels_base   <- df %>% dplyr::filter(Year == base_year)
      
      if (mode == "source") {
        
        # Keep your label-offset logic, but apply it on UI scenario labels
        df_labels_plot <- df_labels_target %>%
          dplyr::group_by(group) %>%
          dplyr::mutate(
            yrange = max(y_val, na.rm = TRUE) - min(y_val, na.rm = TRUE),
            yrange = dplyr::if_else(yrange == 0 | is.na(yrange), 1, yrange),
            y_offset = dplyr::case_when(
              as.character(Scenario_ui) == "Même diète"     ~  0.05 * yrange,
              as.character(Scenario_ui) == "Diète probable" ~  0.00 * yrange,
              as.character(Scenario_ui) == "Diète saine"    ~ -0.05 * yrange,
              TRUE                                          ~  0.00 * yrange
            ),
            y_label = y_val + y_offset
          ) %>%
          dplyr::ungroup()
        
        gplt <- ggplot2::ggplot(
          df,
          ggplot2::aes(
            x      = Year,
            y      = y_val,
            colour = Scenario_ui,
            group  = interaction(Scenario_ui, group),
            text   = tooltip
          )
        ) +
          ggplot2::geom_vline(xintercept = base_year,   colour = "grey85", linewidth = 0.3, show.legend = FALSE) +
          ggplot2::geom_vline(xintercept = target_year, colour = "grey85", linewidth = 0.3, show.legend = FALSE) +
          ggplot2::geom_line(linetype = "dotted", linewidth = 0.5, alpha = 0.9) +
          ggplot2::geom_point(size = 2.2, alpha = 0.95) +
          ggplot2::geom_text(
            data = df_labels_plot,
            ggplot2::aes(x = Year, y = y_label, label = delta_label, colour = Scenario_ui,
                         group = interaction(Scenario_ui, group)),
            hjust = 0, nudge_x = 3.5, size = 3, show.legend = FALSE
          ) +
          ggplot2::scale_x_continuous(
            breaks = c(base_year, target_year),
            expand = ggplot2::expansion(mult = c(0.08, 0.08))
          ) +
          ggplot2::scale_y_continuous(
            expand = ggplot2::expansion(mult = c(0.05, 0.15))
          ) +
          ggplot2::labs(x = "Year", y = y_lab, colour = "Scenario") +
          ggplot2::facet_wrap(~ group, scales = "free_y") +
          ggplot2::theme_minimal(base_size = 11) +
          ggplot2::theme(
            strip.background = ggplot2::element_rect(fill = "grey90", colour = NA),
            strip.text       = ggplot2::element_text(face = "bold"),
            panel.grid.minor = ggplot2::element_blank(),
            legend.position  = "bottom",
            legend.title     = ggplot2::element_text(face = "bold")
          )
        
        # Scenario colors: accept either keys = codes or keys = UI labels
        if (exists("SCENARIO_COLORS")) {
          ui_lvls <- levels(df$Scenario_ui)
          code_lvls <- levels(df$Scenario)
          
          if (all(code_lvls %in% names(SCENARIO_COLORS))) {
            # Map codes -> UI labels
            map_ui <- setNames(vapply(code_lvls, scenario_label, FUN.VALUE = character(1)), code_lvls)
            cols <- SCENARIO_COLORS[code_lvls]
            names(cols) <- map_ui[code_lvls]
            gplt <- gplt + ggplot2::scale_colour_manual(values = cols, breaks = ui_lvls)
          } else if (all(ui_lvls %in% names(SCENARIO_COLORS))) {
            gplt <- gplt + ggplot2::scale_colour_manual(values = SCENARIO_COLORS[ui_lvls], breaks = ui_lvls)
          }
        }
        
      } else {
        
        gplt <- ggplot2::ggplot(
          df,
          ggplot2::aes(
            x      = Year,
            y      = y_val,
            colour = group,
            group  = group,
            text   = tooltip
          )
        ) +
          ggplot2::geom_vline(xintercept = base_year,   colour = "grey85", linewidth = 0.3, show.legend = FALSE) +
          ggplot2::geom_vline(xintercept = target_year, colour = "grey85", linewidth = 0.3, show.legend = FALSE) +
          ggplot2::geom_line(linetype = "dotted", linewidth = 0.5, alpha = 0.9) +
          ggplot2::geom_point(size = 2.0, alpha = 0.95)
        
        if (show_pct) {
          gplt <- gplt +
            ggplot2::geom_text(
              data = df_labels_base,
              ggplot2::aes(x = Year, y = y_val, label = share_label, colour = group, group = group),
              hjust = 1, nudge_x = -4, size = 3, show.legend = FALSE
            ) +
            ggplot2::geom_text(
              data = df_labels_target,
              ggplot2::aes(x = Year, y = y_val, label = share_label, colour = group, group = group),
              hjust = 0, nudge_x = 4, size = 3, show.legend = FALSE
            )
        } else {
          gplt <- gplt +
            ggplot2::geom_text(
              data = df_labels_target,
              ggplot2::aes(x = Year, y = y_val, label = delta_label, colour = group, group = group),
              hjust = 0, nudge_x = 4, size = 3, show.legend = FALSE
            )
        }
        
        gplt <- gplt +
          ggplot2::scale_x_continuous(
            breaks = c(base_year, target_year),
            expand = ggplot2::expansion(mult = c(0.08, 0.08))
          ) +
          ggplot2::scale_y_continuous(
            expand = ggplot2::expansion(mult = c(0.05, 0.15))
          ) +
          ggplot2::labs(x = "Year", y = y_lab, colour = group_title) +
          ggplot2::facet_wrap(~ Scenario_ui, scales = "free_y") +
          ggplot2::theme_minimal(base_size = 11) +
          ggplot2::theme(
            strip.background = ggplot2::element_rect(fill = "grey90", colour = NA),
            strip.text       = ggplot2::element_text(face = "bold"),
            panel.grid.minor = ggplot2::element_blank(),
            legend.position  = "bottom",
            legend.title     = ggplot2::element_text(face = "bold")
          )
        
        shiny::validate(
          shiny::need(ensure_palette_wrapper(),
                      "Missing palette helper: emissions_module_colors_for() not found. Source R/02_utils_palette.R.")
        )
        
        grp_lvls <- levels(df$group)
        if (is.null(grp_lvls) || !length(grp_lvls)) grp_lvls <- unique(as.character(df$group))
        cols_grp <- emissions_module_colors_for(grp_lvls, breakdown = group_var)
        gplt <- gplt + ggplot2::scale_colour_manual(values = cols_grp)
      }
      
      p <- plotly::ggplotly(gplt, tooltip = "text")
      
      if (exists("plotly_apply_global_theme", mode = "function")) {
        p <- plotly_apply_global_theme(p, bg = "transparent", grid = "y")
      } else if (exists("plotly_theme_transparent", mode = "function")) {
        p <- plotly_theme_transparent(p)
      }
      
      p
    }) %>%
      bindCache(r_country(), scen_set_key(), gas_choice(), stack_dim_choice(), input$facet_mode, input$show_pct_axis)
    
    # ------------------------------------------------------------------ #
    # 6. Download CSV (export in kt, consistent with display)
    # ------------------------------------------------------------------ #
    output$dl_csv <- downloadHandler(
      filename = function(){
        paste0("agricultural_emissions_", r_country(), "_", gas_label(), "_no_land_use_change_kt.csv")
      },
      content = function(file){
        df <- emissions_data()
        if (is.null(df) || nrow(df) == 0) {
          utils::write.csv(data.frame(), file, row.names = FALSE)
        } else {
          g         <- gas_choice()
          dimv      <- stack_dim_choice()
          group_var <- if (g == "co2e") "Item" else dimv
          
          df_export <- df %>%
            dplyr::mutate(
              Region    = r_country(),
              Gas       = gas_label(),
              Breakdown = ifelse(group_var == "Animal", "Animal", "Item"),
              Scenario_code  = as.character(Scenario),
              Scenario_label = vapply(as.character(Scenario), scenario_label, FUN.VALUE = character(1)),
              Group     = as.character(Group),
              Value_kt  = Value_plot,
              Unit      = as.character(UnitLabPlot)
            ) %>%
            dplyr::select(Region, Gas, Breakdown, Scenario_code, Scenario_label, Group, Value_kt, Unit)
          
          utils::write.csv(df_export, file, row.names = FALSE, fileEncoding = "UTF-8")
        }
      }
    )
    
    # ------------------------------------------------------------------ #
    # 7. Note explicative (mention kt)
    # ------------------------------------------------------------------ #
    output$note <- renderUI({
      req(r_country())
      df <- emissions_data()
      if (is.null(df) || nrow(df) == 0) return(NULL)
      
      g         <- gas_choice()
      dimv      <- stack_dim_choice()
      group_var <- if (g == "co2e") "Item" else dimv
      group_title <- if (group_var == "Animal") "Animal" else "Item"
      
      htmltools::HTML(glue::glue(
        "<p>
        This module displays two complementary views of
        <strong>agricultural greenhouse gas emissions</strong> (excluding land use change).
        The selected gas is <strong>{gas_label()}</strong>.
        <ul>
          <li><strong>Stacked bars (top)</strong>: totals are displayed in
              <strong>kilotonnes</strong> and broken down by <strong>{group_title}</strong>.</li>
          <li><strong>Evolution chart (bottom)</strong>: change between the baseline year and the target year.
              When the percentage checkbox is not selected, values are shown in <strong>kilotonnes</strong>.</li>
        </ul>
        </p>"
      ))
    })
  })
}
