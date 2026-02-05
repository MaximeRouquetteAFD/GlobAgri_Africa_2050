# R/3.5_mod_animal_efficiency.R
# ---------------------------------------------------------------

mod_animal_efficiency_ui <- function(id, height = "450px"){
  ns <- NS(id)
  
  tagList(
    div(
      class = "card",
      div(
        class = "card-body",
        h2("Animal feeding requirement"),
        tags$div(style="height:15px"),
        div(class = "u-controls u-controls--inline",
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
                      "Requirement metric"
                    )
                  ),
                  choices = c(
                    "Dry matter requirement" = "dm",
                    "Protein requirement"    = "prot",
                    "Energy requirement"     = "en"
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
        shiny::need(nrow(df) > 0, "No animal feeding requirement coefficients available for this country.")
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
        # ratios as displayed in the chart (same logic as before)
        dplyr::mutate(
          dm_req   = dplyr::if_else(is.finite(dm_coeff)   & dm_coeff   > 0, 1000 * dm_coeff, NA_real_),
          prot_req = dplyr::if_else(is.finite(prot_coeff) & prot_coeff > 0, prot_coeff,      NA_real_),
          en_req   = dplyr::if_else(is.finite(en_coeff)   & en_coeff   > 0, en_coeff,        NA_real_)
        ) %>%
        dplyr::filter(
          dplyr::if_any(c(dm_req, prot_req, en_req), ~ !is.na(.) & is.finite(.))
        )
      
      shiny::validate(
        shiny::need(nrow(wide) > 0, "No feeding requirement coefficients available for this country.")
      )
      
      wide
    }) %>% bindCache(r_country())
    
    # Effective scenarios = (requested if any) ∩ (present for this country / module)
    scen_levels_effective <- reactive({
      df0 <- eff_table_all()
      present <- unique(as.character(df0$Scenario))
      present <- present[!is.na(present) & nzchar(present)]
      
      req_sc <- scen_codes_ordered()
      
      if (length(req_sc) > 0) {
        keep <- intersect(req_sc, present)
        keep <- req_sc[req_sc %in% keep]
        return(keep)
      }
      
      if (exists("SCENARIO_LEVELS_DEFAULT", inherits = TRUE)) {
        base <- get("SCENARIO_LEVELS_DEFAULT", inherits = TRUE)
        known   <- intersect(base, present)
        unknown <- setdiff(present, base)
        return(c(known, sort(unknown)))
      }
      
      sort(present)
    })
    
    # baseline scenario code (CONFIG if available; fallback to "Année de base" if present; else first)
    base_scen_code <- reactive({
      sc <- scen_levels_effective()
      validate(need(length(sc) > 0, "No scenario available for this selection."))
      
      b <- if (exists("SCENARIO_BASE_YEAR_CODE", inherits = TRUE)) {
        as.character(get("SCENARIO_BASE_YEAR_CODE", inherits = TRUE))
      } else {
        ""
      }
      if (nzchar(b) && b %in% sc) return(b)
      
      if ("Année de base" %in% sc) return("Année de base")
      sc[1]
    })
    
    # -----------------------------------------------------------
    # 3) Pick selector block (SYSTEM ONLY)
    # -----------------------------------------------------------
    output$pick_ui <- renderUI({
      df <- eff_table_all()
      req(nrow(df) > 0)
      
      title_style <- "font-weight:600;font-size:13px;margin:0 0 6px 0;"
      
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
    })
    
    # -----------------------------------------------------------
    # 4) Plot data builder (SYSTEM ONLY)
    # -----------------------------------------------------------
    plot_df <- reactive({
      df <- eff_table_all()
      req(nrow(df) > 0)
      
      eff_type <- input$eff_type %||% "dm"
      metric_col <- dplyr::case_when(
        eff_type == "dm"   ~ "dm_req",
        eff_type == "prot" ~ "prot_req",
        eff_type == "en"   ~ "en_req",
        TRUE               ~ "dm_req"
      )
      
      scen_lvls <- scen_levels_effective()
      df <- df %>%
        dplyr::filter(.data$Scenario %in% scen_lvls) %>%
        dplyr::mutate(
          Scenario = factor(as.character(.data$Scenario), levels = scen_lvls),
          metric   = .data[[metric_col]]
        ) %>%
        dplyr::filter(!is.na(.data$Item), !is.na(.data$metric), is.finite(.data$metric))
      
      shiny::validate(shiny::need(nrow(df) > 0, "No feeding requirement coefficients available for this selection."))
      
      sys <- input$pick_system %||% "__GLOBAL__"
      sys_label <- if (identical(sys, "__GLOBAL__")) "Global" else sys
      
      df1 <- df %>% dplyr::filter(.data$System == sys_label)
      shiny::validate(shiny::need(nrow(df1) > 0, "No data for the selected system."))
      
      bsc <- base_scen_code()
      
      # item ordering by baseline scenario if present
      if (bsc %in% levels(df1$Scenario)) {
        ord <- df1 %>%
          dplyr::filter(as.character(.data$Scenario) == bsc) %>%
          dplyr::arrange(.data$metric) %>%
          dplyr::pull(.data$Item)
        if (length(ord) > 0) df1 <- df1 %>% dplyr::mutate(Item = factor(.data$Item, levels = ord))
      } else {
        df1 <- df1 %>% dplyr::mutate(Item = factor(.data$Item))
      }
      
      df_base <- df1 %>%
        dplyr::filter(as.character(.data$Scenario) == bsc) %>%
        dplyr::select(Item, metric_base = metric)
      
      df1 <- df1 %>% dplyr::left_join(df_base, by = "Item")
      
      list(df = df1, sys_label = sys_label, base_scen = bsc)
    })
    
    # -----------------------------------------------------------
    # 5) Plotly (SYSTEM ONLY)
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
      
      scen_lvls <- levels(df$Scenario)
      
      cols_scen <- if (exists("scenario_palette", mode = "function", inherits = TRUE)) {
        scenario_palette(scen_lvls)
      } else {
        stats::setNames(scales::hue_pal()(length(scen_lvls)), scen_lvls)
      }
      
      bsc <- res$base_scen
      
      df2 <- df %>%
        dplyr::mutate(
          pct_change = dplyr::if_else(
            as.character(.data$Scenario) != bsc &
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
            "<b>Requirement:</b> ", sprintf("%.2f", metric), " (", hover_suffix, ")",
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
          bargap  = 0.15,
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
        paste0("animal_requirements_", gsub("\\s+", "_", r_country()), ".csv")
      },
      content = function(file){
        df <- eff_table_all()
        req(nrow(df) > 0)
        
        scen_keep <- scen_levels_effective()
        df <- df %>% dplyr::filter(.data$Scenario %in% scen_keep)
        
        out <- df %>%
          dplyr::transmute(
            Region,
            Scenario_code  = as.character(.data$Scenario),
            Scenario_label = scen_label_vec(as.character(.data$Scenario)),
            System         = as.character(.data$System),
            Item,
            Dry_matter_coeff      = dm_coeff,
            Dry_matter_requirement = dm_req,
            Protein_coeff          = prot_coeff,
            Protein_requirement    = prot_req,
            Energy_coeff           = en_coeff,
            Energy_requirement     = en_req
          ) %>%
          dplyr::arrange(Region, Scenario_code, System, Item)
        
        utils::write.csv(out, file, row.names = FALSE, fileEncoding = "UTF-8")
      }
    )
    
    # -----------------------------------------------------------
    # 7) Note (Requirements wording)
    # -----------------------------------------------------------
    output$note <- renderUI({
      req(r_country())
      eff_type <- input$eff_type %||% "dm"
      
      metric_txt <- dplyr::case_when(
        eff_type == "dm" ~
          "Dry matter requirement indicates how many kilograms of dry matter must be ingested to produce one kilogram of dry matter in animal products (<em>kg DM in / kg DM out</em>).",
        eff_type == "prot" ~
          "Protein requirement indicates how many kilograms of protein must be ingested to produce one kilogram of protein in animal products (<em>kg protein in / kg protein out</em>).",
        eff_type == "en" ~
          "Energy requirement indicates how many kilocalories must be ingested to produce one kilocalorie of energy in animal products (<em>kcal in / kcal out</em>).",
        TRUE ~
          "Requirement indicates how many input units must be ingested to produce one unit of animal output."
      )
      
      missing_txt <- paste0(
        "If a bar is missing for a given animal category, it does <strong>not</strong> mean the requirement is zero. ",
        "It means that the animal category is <strong>not present</strong> in that livestock system for this country, ",
        "so the model does not compute a requirement coefficient for that system."
      )
      
      pct_txt <- paste0(
        "The percentages shown above the bars indicate the change in the requirement coefficient compared with the baseline scenario ",
        "(for the same animal category and the selected livestock system)."
      )
      
      htmltools::HTML(paste0(
        "<p>",
        "This chart displays, for the selected country, <strong>feeding requirement coefficients</strong> by animal category. ",
        "Lower values indicate a <strong>lower feed requirement</strong> per unit of animal output.",
        "<br>", pct_txt,
        "<br>", metric_txt,
        "<br>", missing_txt,
        "</p>"
      ))
    })
    
  })
}
