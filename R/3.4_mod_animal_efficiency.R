# R/3.4_mod_animal_efficiency.R
# ---------------------------------------------------------------
# Animal feeding efficiencies
# - Mode "system" (default): pick one livestock system (incl. Global) -> bars by Scenario
# - Mode "scenario": pick one Scenario -> bars by System (incl. Global if present)
# - Scenario filter/order:
#     * if r_scenarios() provided: EXACT requested codes intersected with present
#     * order = SCENARIO_LEVELS_DEFAULT + deterministic append of unknown codes
#     * display labels via scenario_label(code) if available (axis/legend/export label)
# - System colors: COL_SYSTEMS (defined in R/02) if present; fallback hue
# - Missing bars mean: animal category not present in that system (not zero)
# - Bars are "stuck together" within each animal group in scenario mode
# ---------------------------------------------------------------

mod_animal_efficiency_ui <- function(id, height = "450px"){
  ns <- NS(id)
  
  tagList(
    div(
      class = "card",
      div(
        class = "card-body",
        h2("Animal feeding efficiencies"),
        
        div(
          class = "u-controls u-controls--inline",
          
          # --- Mode (exclusive toggle enforced in server)
          div(
            class = "form-group",
            checkboxGroupInput(
              inputId = ns("eff_mode"),
              label   = "",
              choices = c(
                "Display by livestock system" = "system",
                "Display by scenario"         = "scenario"
              ),
              selected = "system",
              inline   = TRUE
            )
          ),
          
          # --- Pick selector (system OR scenario) + Metric selector on the same line
          fluidRow(
            column(
              width = 6,
              uiOutput(ns("pick_ui"))
            ),
            column(
              width = 6,
              div(
                class = "form-group",
                style = "margin-bottom:10px;",
                selectInput(
                  inputId = ns("eff_type"),
                  label = tagList(
                    tags$div(
                      style = "font-weight:600;font-size:13px;margin:0 0 6px 0;",
                      "Efficiency metric"
                    )
                  ),
                  choices = c(
                    "Dry matter efficiency" = "dm",
                    "Protein efficiency"    = "prot",
                    "Energy efficiency"     = "en"
                  ),
                  selected = "dm"
                )
              )
            )
          )
        ),
        
        plotly::plotlyOutput(ns("eff_plot"), height = height),
        
        div(
          class = "u-actions",
          downloadLink(
            ns("dl_csv"),
            label = tagList(icon("download"), "CSV"),
            tags$br()
          )
        ),
        
        uiOutput(ns("note"))
      )
    )
  )
}


mod_animal_efficiency_server <- function(
    id,
    fact,
    r_country,          # reactive: Region
    r_scenarios = NULL  # OPTIONAL reactive/function returning scenario CODES (fact$Scenario)
){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    `%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x
    
    # -------------------------
    # Scenario helpers
    # -------------------------
    has_scen_label <- exists("scenario_label", mode = "function", inherits = TRUE)
    scen_label_vec <- function(x){
      x <- as.character(x)
      if (has_scen_label) vapply(x, scenario_label, character(1)) else x
    }
    
    # Requested scenarios (codes) with central order
    scen_codes_ordered <- reactive({
      # if r_scenarios provided, use it; otherwise leave empty here (we’ll fallback to present later)
      if (!is.null(r_scenarios) && is.function(r_scenarios)) {
        sc <- unique(as.character(r_scenarios()))
        sc <- sc[!is.na(sc) & nzchar(sc)]
      } else {
        sc <- character(0)
      }
      
      if (length(sc) == 0) return(character(0))
      
      if (exists("SCENARIO_LEVELS_DEFAULT", inherits = TRUE)) {
        base <- get("SCENARIO_LEVELS_DEFAULT", inherits = TRUE)
        known   <- intersect(base, sc)
        unknown <- setdiff(sc, base)
        return(c(known, sort(unknown)))
      }
      
      sort(sc)
    })
    
    # Effective scenarios = (requested if any) ∩ (present for this country / module)
    scen_levels_effective <- reactive({
      df0 <- eff_table_all()
      present <- unique(as.character(df0$Scenario))
      present <- present[!is.na(present) & nzchar(present)]
      
      req_sc <- scen_codes_ordered()
      
      if (length(req_sc) > 0) {
        keep <- intersect(req_sc, present)
        # preserve req_sc order
        keep <- req_sc[req_sc %in% keep]
        return(keep)
      }
      
      # fallback: present ordered by default levels if any
      if (exists("SCENARIO_LEVELS_DEFAULT", inherits = TRUE)) {
        base <- get("SCENARIO_LEVELS_DEFAULT", inherits = TRUE)
        known   <- intersect(base, present)
        unknown <- setdiff(present, base)
        return(c(known, sort(unknown)))
      }
      
      sort(present)
    })
    
    # -------------------------
    # Exclusive toggle (checkboxGroupInput behaves like radio)
    # -------------------------
    prev_mode <- shiny::reactiveVal("system")
    observeEvent(input$eff_mode, {
      sel <- input$eff_mode
      
      if (is.null(sel) || length(sel) == 0) {
        prev_mode("system")
        updateCheckboxGroupInput(session, "eff_mode", selected = "system")
        return()
      }
      
      if (length(sel) == 1) {
        prev_mode(sel)
        return()
      }
      
      old <- prev_mode() %||% character(0)
      newly <- setdiff(sel, old)
      keep <- if (length(newly) == 1) newly else tail(sel, 1)
      
      prev_mode(keep)
      updateCheckboxGroupInput(session, "eff_mode", selected = keep)
    }, ignoreInit = TRUE)
    
    eff_mode <- reactive({
      m <- input$eff_mode
      if (is.null(m) || length(m) == 0) return("system")
      m[1]
    })
    
    # -----------------------------------------------------------
    # 1) Raw data (keep system-level AND global; drop method)
    # -----------------------------------------------------------
    eff_raw_all <- reactive({
      req(r_country())
      
      has_method <- "method" %in% names(fact)
      
      df <- fact %>%
        dplyr::filter(
          .data$Region == r_country(),
          .data$Element %in% c(
            "Dry matter Requirement coefficient",
            "Protein Requirement coefficient",
            "Energy Requirement coefficient"
          ),
          if (has_method) (is.na(.data$method) | .data$method == "") else TRUE,
          !.data$Item %in% c(
            "Aquatic animal products No Fish Input",
            "Aquatic animal products"
          )
        ) %>%
        dplyr::filter(!is.na(.data$Value), is.finite(.data$Value)) %>%
        dplyr::mutate(
          System = dplyr::if_else(
            is.na(.data$System) | .data$System == "",
            "Global",
            as.character(.data$System)
          )
        )
      
      shiny::validate(
        shiny::need(nrow(df) > 0, "No animal efficiency coefficients available for this country.")
      )
      df
    }) %>% bindCache(r_country())
    
    # -----------------------------------------------------------
    # 2) Wide table + derived ratios (keep System)
    # -----------------------------------------------------------
    eff_table_all <- reactive({
      df <- eff_raw_all()
      
      wide <- df %>%
        dplyr::group_by(Region, Scenario, System, Item, Element) %>%
        dplyr::summarise(Value = mean(Value, na.rm = TRUE), .groups = "drop") %>%
        tidyr::pivot_wider(names_from = Element, values_from = Value) %>%
        dplyr::rename(
          dm_coeff   = `Dry matter Requirement coefficient`,
          prot_coeff = `Protein Requirement coefficient`,
          en_coeff   = `Energy Requirement coefficient`
        ) %>%
        dplyr::mutate(
          dm_eff   = dplyr::if_else(is.finite(dm_coeff)   & dm_coeff   > 0, 1000 * dm_coeff, NA_real_),
          prot_eff = dplyr::if_else(is.finite(prot_coeff) & prot_coeff > 0, prot_coeff,      NA_real_),
          en_eff   = dplyr::if_else(is.finite(en_coeff)   & en_coeff   > 0, en_coeff,        NA_real_)
        ) %>%
        dplyr::filter(
          dplyr::if_any(c(dm_eff, prot_eff, en_eff), ~ !is.na(.) & is.finite(.))
        )
      
      shiny::validate(
        shiny::need(nrow(wide) > 0, "No efficiency coefficients available for this country.")
      )
      
      wide
    }) %>% bindCache(r_country())
    
    # -----------------------------------------------------------
    # 3) Dynamic pick selector block (system OR scenario)
    # -----------------------------------------------------------
    output$pick_ui <- renderUI({
      df <- eff_table_all()
      req(nrow(df) > 0)
      mode <- eff_mode()
      
      title_style <- "font-weight:600;font-size:13px;margin:0 0 6px 0;"
      
      scen_lvls <- scen_levels_effective()
      shiny::validate(shiny::need(length(scen_lvls) > 0, "No scenario available for this selection."))
      
      if (identical(mode, "scenario")) {
        # value = code, label = scenario_label(code) if available
        scen_choices <- stats::setNames(scen_lvls, scen_label_vec(scen_lvls))
        
        cur <- isolate(input$pick_scenario)
        if (is.null(cur) || !(cur %in% scen_lvls)) {
          cur <- (scen_lvls[scen_lvls != "Année de base"][1] %||% scen_lvls[1])
        }
        
        selectInput(
          inputId  = ns("pick_scenario"),
          label    = tagList(tags$div(style = title_style, "Select a scenario")),
          choices  = scen_choices,
          selected = cur
        )
        
      } else {
        # system list (Global first)
        systems <- df$System |> as.character() |> unique()
        systems <- sort(unique(systems))
        systems <- c(intersect("Global", systems), setdiff(systems, "Global"))
        
        sys_choices <- c("Global" = "__GLOBAL__")
        sys_other <- setdiff(systems, "Global")
        if (length(sys_other) > 0) sys_choices <- c(sys_choices, stats::setNames(sys_other, sys_other))
        
        cur <- isolate(input$pick_system)
        if (is.null(cur) || !(cur %in% names(sys_choices))) cur <- "__GLOBAL__"
        
        selectInput(
          inputId  = ns("pick_system"),
          label    = tagList(tags$div(style = title_style, "Select a livestock system")),
          choices  = sys_choices,
          selected = cur
        )
      }
    })
    
    # -----------------------------------------------------------
    # 4) Plot data builder (mode-aware)
    # -----------------------------------------------------------
    plot_df <- reactive({
      df <- eff_table_all()
      req(nrow(df) > 0)
      
      eff_type <- input$eff_type %||% "dm"
      metric_col <- dplyr::case_when(
        eff_type == "dm"   ~ "dm_eff",
        eff_type == "prot" ~ "prot_eff",
        eff_type == "en"   ~ "en_eff",
        TRUE               ~ "dm_eff"
      )
      
      mode <- eff_mode()
      
      scen_lvls <- scen_levels_effective()
      df <- df %>%
        dplyr::filter(.data$Scenario %in% scen_lvls) %>%
        dplyr::mutate(
          Scenario = factor(as.character(.data$Scenario), levels = scen_lvls),
          metric   = .data[[metric_col]]
        ) %>%
        dplyr::filter(!is.na(.data$Item), !is.na(.data$metric), is.finite(.data$metric))
      
      shiny::validate(shiny::need(nrow(df) > 0, "No efficiency coefficients available for this selection."))
      
      if (identical(mode, "scenario")) {
        scen <- input$pick_scenario %||% ""
        shiny::validate(shiny::need(nzchar(scen), "Select a scenario."))
        
        df1 <- df %>% dplyr::filter(as.character(.data$Scenario) == scen)
        shiny::validate(shiny::need(nrow(df1) > 0, "No data for the selected scenario."))
        
        # Order items by Global (if present) else overall
        if (any(df1$System == "Global")) {
          ord <- df1 %>%
            dplyr::filter(.data$System == "Global") %>%
            dplyr::group_by(.data$Item) %>%
            dplyr::summarise(v = mean(metric, na.rm = TRUE), .groups = "drop") %>%
            dplyr::arrange(.data$v) %>%
            dplyr::pull(.data$Item)
        } else {
          ord <- df1 %>%
            dplyr::group_by(.data$Item) %>%
            dplyr::summarise(v = mean(metric, na.rm = TRUE), .groups = "drop") %>%
            dplyr::arrange(.data$v) %>%
            dplyr::pull(.data$Item)
        }
        df1 <- df1 %>% dplyr::mutate(Item = factor(.data$Item, levels = ord))
        
        # System ordering: Global first, then COL_SYSTEMS order, then others
        sys_present <- unique(as.character(df1$System))
        sys_levels <- c("Global")
        if (exists("COL_SYSTEMS", inherits = TRUE)) {
          sys_levels <- c(sys_levels, names(get("COL_SYSTEMS", inherits = TRUE)))
        }
        sys_levels <- c(intersect(sys_levels, sys_present), setdiff(sys_present, intersect(sys_levels, sys_present)))
        
        df1 <- df1 %>%
          dplyr::mutate(System = factor(as.character(.data$System), levels = sys_levels))
        
        list(mode = "scenario", df = df1, scen = scen)
      }
      
      # mode == system
      sys <- input$pick_system %||% "__GLOBAL__"
      sys_label <- if (identical(sys, "__GLOBAL__")) "Global" else sys
      
      df1 <- df %>% dplyr::filter(.data$System == sys_label)
      shiny::validate(shiny::need(nrow(df1) > 0, "No data for the selected system."))
      
      base_scen <- "Année de base"
      
      if (base_scen %in% levels(df1$Scenario)) {
        ord <- df1 %>%
          dplyr::filter(.data$Scenario == base_scen) %>%
          dplyr::arrange(.data$metric) %>%
          dplyr::pull(.data$Item)
        if (length(ord) > 0) df1 <- df1 %>% dplyr::mutate(Item = factor(.data$Item, levels = ord))
      } else {
        df1 <- df1 %>% dplyr::mutate(Item = factor(.data$Item))
      }
      
      df_base <- df1 %>%
        dplyr::filter(.data$Scenario == base_scen) %>%
        dplyr::select(Item, metric_base = metric)
      
      df1 <- df1 %>% dplyr::left_join(df_base, by = "Item")
      
      list(mode = "system", df = df1, sys_label = sys_label)
    })
    
    # -----------------------------------------------------------
    # 5) Plotly
    # -----------------------------------------------------------
    output$eff_plot <- plotly::renderPlotly({
      res <- plot_df()
      df <- res$df
      req(nrow(df) > 0)
      
      eff_type <- input$eff_type %||% "dm"
      
      axis_label <- dplyr::case_when(
        eff_type == "dm"   ~ "kg of dry matter ingested per kg of dry matter produced",
        eff_type == "prot" ~ "kg of protein ingested per kg of protein produced",
        eff_type == "en"   ~ "kcal ingested per kcal produced",
        TRUE               ~ "Units ingested per unit produced"
      )
      
      hover_suffix <- dplyr::case_when(
        eff_type == "dm"   ~ "kg DM in / kg DM out",
        eff_type == "prot" ~ "kg protein in / kg protein out",
        eff_type == "en"   ~ "kcal in / kcal out",
        TRUE               ~ "in / out"
      )
      
      BAR_WIDTH <- 0.18
      BAR_STEP  <- 0.18
      
      if (identical(res$mode, "scenario")) {
        # Scenario view: 1 scenario selected, bars by System
        sys_lvls <- levels(df$System)
        
        cols <- NULL
        if (exists("COL_SYSTEMS", inherits = TRUE)) {
          cs <- get("COL_SYSTEMS", inherits = TRUE)
          cols <- cs[sys_lvls]
        }
        if (is.null(cols)) cols <- rep(NA_character_, length(sys_lvls))
        
        missing_sys <- sys_lvls[is.na(cols)]
        if (length(missing_sys) > 0) {
          fill <- setNames(scales::hue_pal()(length(missing_sys)), missing_sys)
          cols[missing_sys] <- fill[missing_sys]
        }
        cols <- stats::setNames(unname(cols), sys_lvls)
        
        items <- levels(df$Item)
        item_idx <- setNames(seq_along(items), items)
        
        df_pos <- df %>%
          dplyr::mutate(item_i = item_idx[as.character(.data$Item)]) %>%
          dplyr::group_by(.data$Item) %>%
          dplyr::mutate(
            sys_in_item = as.character(.data$System),
            sys_rank = match(sys_in_item, sys_lvls),
            n_sys    = dplyr::n_distinct(sys_in_item),
            k        = dplyr::dense_rank(sys_rank),
            x_num    = item_i + (k - (n_sys + 1) / 2) * BAR_STEP
          ) %>%
          dplyr::ungroup() %>%
          dplyr::mutate(
            hover = paste0(
              "<b>Animal:</b> ", as.character(Item), "<br>",
              "<b>Scenario:</b> ", scen_label_vec(res$scen), "<br>",
              "<b>System:</b> ", as.character(System), "<br>",
              "<b>Value:</b> ", sprintf("%.2f", metric), " (", hover_suffix, ")",
              "<extra></extra>"
            )
          )
        
        p <- plotly::plot_ly()
        for (s in sys_lvls) {
          sub <- df_pos %>% dplyr::filter(.data$System == s)
          if (nrow(sub) == 0) next
          
          p <- p %>%
            plotly::add_bars(
              data   = sub,
              x      = ~x_num,
              y      = ~metric,
              name   = s,
              marker = list(color = unname(cols[s])),
              width  = BAR_WIDTH,
              hovertext = ~hover,
              hoverinfo = "text"
            )
        }
        
        p <- p %>%
          plotly::layout(
            barmode = "overlay",
            bargap  = 0,
            xaxis = list(
              title    = "",
              type     = "linear",
              tickmode = "array",
              tickvals = seq_along(items),
              ticktext = items,
              zeroline = FALSE
            ),
            yaxis = list(
              title    = axis_label,
              zeroline = FALSE,
              separatethousands = TRUE
            ),
            legend = list(
              orientation = "h",
              xanchor     = "center",
              x           = 0.5,
              y           = 1.08,
              title       = list(text = "")
            ),
            margin = list(l = 80, r = 20, t = 40, b = 70)
          )
        
        p <- plotly_apply_global_theme(p, bg = "transparent", grid = "y")
        return(p)
      }
      
      # System view: 1 system selected, bars by Scenario
      scen_lvls <- levels(df$Scenario)
      
      cols_scen <- if (exists("scenario_palette", mode = "function", inherits = TRUE)) {
        scenario_palette(scen_lvls)
      } else {
        stats::setNames(scales::hue_pal()(length(scen_lvls)), scen_lvls)
      }
      
      df2 <- df %>%
        dplyr::mutate(
          pct_change = dplyr::if_else(
            as.character(.data$Scenario) != "Année de base" &
              !is.na(.data$metric_base) & is.finite(.data$metric_base) & .data$metric_base > 0 &
              is.finite(.data$metric),
            100 * (.data$metric / .data$metric_base - 1),
            NA_real_
          ),
          pct_label = dplyr::if_else(
            is.na(.data$pct_change),
            NA_character_,
            paste0(dplyr::if_else(.data$pct_change >= 0, "+", ""), sprintf("%.0f", .data$pct_change), " %")
          ),
          hover = paste0(
            "<b>Animal:</b> ", as.character(Item), "<br>",
            "<b>System:</b> ", res$sys_label, "<br>",
            "<b>Scenario:</b> ", scen_label_vec(as.character(Scenario)), "<br>",
            "<b>Value:</b> ", sprintf("%.2f", metric), " (", hover_suffix, ")",
            "<extra></extra>"
          )
        )
      
      p <- plotly::plot_ly()
      
      for (s in scen_lvls) {
        sub <- df2 %>% dplyr::filter(.data$Scenario == s)
        if (nrow(sub) == 0) next
        
        p <- p %>%
          plotly::add_bars(
            data   = sub,
            x      = ~Item,
            y      = ~metric,
            name   = scen_label_vec(as.character(s)),
            marker = list(color = unname(cols_scen[as.character(s)])),
            offsetgroup = as.character(s),
            text   = ~pct_label,
            textposition = "outside",
            hovertext = ~hover,
            hoverinfo = "text"
          )
      }
      
      p <- p %>%
        plotly::layout(
          barmode = "group",
          bargap  = 0.15,  # compact groups
          xaxis = list(
            title     = "",
            type      = "category",
            tickangle = 0
          ),
          yaxis = list(
            title    = axis_label,
            zeroline = FALSE,
            separatethousands = TRUE
          ),
          legend = list(
            orientation = "h",
            xanchor     = "center",
            x           = 0.5,
            y           = 1.08,
            title       = list(text = "")
          ),
          margin = list(l = 80, r = 20, t = 40, b = 70)
        )
      
      p <- plotly_apply_global_theme(p, bg = "transparent", grid = "y")
      p
    })
    
    # -----------------------------------------------------------
    # 6) CSV export
    # -----------------------------------------------------------
    output$dl_csv <- downloadHandler(
      filename = function(){
        paste0("animal_efficiencies_", gsub("\\s+", "_", r_country()), ".csv")
      },
      content = function(file){
        df <- eff_table_all()
        req(nrow(df) > 0)
        
        # apply same effective scenario filter in export
        scen_keep <- scen_levels_effective()
        df <- df %>% dplyr::filter(.data$Scenario %in% scen_keep)
        
        out <- df %>%
          dplyr::transmute(
            Region,
            Scenario_code  = as.character(.data$Scenario),
            Scenario_label = scen_label_vec(as.character(.data$Scenario)),
            System         = as.character(.data$System),
            Item,
            Dry_matter_coeff = dm_coeff,
            Dry_matter_ratio = dm_eff,
            Protein_coeff    = prot_coeff,
            Protein_ratio    = prot_eff,
            Energy_coeff     = en_coeff,
            Energy_ratio     = en_eff
          ) %>%
          dplyr::arrange(Region, Scenario_code, System, Item)
        
        utils::write.csv(out, file, row.names = FALSE, fileEncoding = "UTF-8")
      }
    )
    
    # -----------------------------------------------------------
    # 7) Note (HTML clean)
    # -----------------------------------------------------------
    output$note <- renderUI({
      req(r_country())
      eff_type <- input$eff_type %||% "dm"
      
      metric_txt <- dplyr::case_when(
        eff_type == "dm" ~
          "Dry matter efficiency shows how many kilograms of dry matter must be ingested to produce one kilogram of dry matter in animal products (<em>kg DM in / kg DM out</em>).",
        eff_type == "prot" ~
          "Protein efficiency shows how many kilograms of protein must be ingested to produce one kilogram of protein in animal products (<em>kg protein in / kg protein out</em>).",
        eff_type == "en" ~
          "Energy efficiency shows how many kilocalories must be ingested to produce one kilocalorie of energy in animal products (<em>kcal in / kcal out</em>).",
        TRUE ~
          "Efficiency shows how many input units must be ingested to produce one unit of animal output."
      )
      
      missing_txt <- paste0(
        "If a bar is missing for a given animal category, it does <strong>not</strong> mean the efficiency is zero. ",
        "It means that the animal category is <strong>not present</strong> in that livestock system for this country, ",
        "so the model does not compute an efficiency coefficient for that system."
      )
      
      htmltools::HTML(paste0(
        "<p>",
        "This chart displays, for the selected country, <strong>feeding efficiency coefficients</strong> by animal category. ",
        "Lower values indicate a <strong>more efficient</strong> conversion of feed into animal output.",
        "<br>", metric_txt,
        "<br>", missing_txt,
        "</p>"
      ))
    })
    
  })
}
