# R/3.2_mod_livestock_stocks.R
# ======================================================================
# Graphs "Stocks" (nombre de têtes) par espèce et par scénario (barres)
# - Filtre: Region ; Element == "Stocks" ; Unit ∈ {"Head","1000 Head"}
# - Filtre scénarios: EXACTEMENT r_scenarios() (codes fact$Scenario), intersecté avec ceux présents
# - Ordre: SCENARIO_LEVELS_DEFAULT (codes) + append déterministe des codes inconnus
# - Affichage: scenario_label(code) uniquement (axe x, cartes, export si souhaité)
# - Baseline year: BASE_YEAR si défini, sinon 2018 (technique, pas une règle métier de scénarios)
# ======================================================================

suppressPackageStartupMessages({
  library(shiny)
  library(dplyr)
  library(stringr)
  library(tidyr)
  library(ggplot2)
  library(plotly)
  library(scales)
})

mod_livestock_stocks_ui <- function(id, title = "Stock of animals") {
  ns <- NS(id)
  tagList(
    div(
      class = "card",
      div(
        class = "card-body",
        h2(class = "card-title", title),
        p(
          class = "text-muted",
          "Each chart presents the number of heads by species: comparison between the base year and diet scenarios."
        ),
        
        uiOutput(ns("plots_grid")),
        
        div(
          class = "u-actions",
          downloadLink(
            ns("dl_livestock_stocks_csv"),
            label = tagList(icon("download"), "CSV")
          )
        ),
        uiOutput(ns("note"))
      )
    )
  )
}

mod_livestock_stocks_server <- function(
    id,
    fact,
    r_country,
    r_scenarios               # <--- REQUIRED: reactive/function returning scenario CODES (fact$Scenario)
){
  # On n'exclut QUE les "equivalent" (pas "Beef cattle")
  exclude_items <- c("Beef cattle equivalent", "Dairy cattle equivalent")
  
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    `%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a
    
    # --- Dépendances scénarios (single source of truth) --------------------
    if (is.null(r_scenarios) || !is.function(r_scenarios)) {
      stop("mod_livestock_stocks_server(): 'r_scenarios' must be provided as a reactive/function returning scenario CODES.")
    }
    if (!exists("scenario_label", mode = "function", inherits = TRUE)) {
      stop("mod_livestock_stocks_server(): missing dependency 'scenario_label(code)'.")
    }
    if (!exists("SCENARIO_LEVELS_DEFAULT", inherits = TRUE)) {
      stop("mod_livestock_stocks_server(): missing dependency 'SCENARIO_LEVELS_DEFAULT'.")
    }
    if (!exists("scenario_palette", mode = "function", inherits = TRUE)) {
      stop("mod_livestock_stocks_server(): missing dependency 'scenario_palette(levels=...)'.")
    }
    if (!exists("SCENARIO_BASE_YEAR_CODE", inherits = TRUE)) {
      stop("mod_livestock_stocks_server(): missing dependency 'SCENARIO_BASE_YEAR_CODE'.")
    }
    if (!exists("SCENARIOS_BASE_CODES", inherits = TRUE)) {
      stop("mod_livestock_stocks_server(): missing dependency 'SCENARIOS_BASE_CODES'.")
    }
    
    EXTRA_CODES <- if (exists("SCENARIOS_EXTRA_CODES", inherits = TRUE)) {
      get("SCENARIOS_EXTRA_CODES", inherits = TRUE)
    } else {
      character(0)
    }
    SHOW_PCT_CODES <- unique(c(get("SCENARIOS_BASE_CODES", inherits = TRUE), EXTRA_CODES))
    
    
    scenario_label_vec <- function(x){
      x <- as.character(x)
      vapply(x, scenario_label, character(1))
    }
    
    base_year <- if (exists("BASE_YEAR", inherits = TRUE)) as.integer(BASE_YEAR) else 2018L
    
    # --- Scénarios demandés (CODES) + ordre central ------------------------
    scen_codes_ordered <- reactive({
      sc <- unique(as.character(r_scenarios()))
      sc <- sc[!is.na(sc) & nzchar(sc)]
      
      validate(need(length(sc) > 0, "No scenario provided to the module."))
      
      known   <- intersect(SCENARIO_LEVELS_DEFAULT, sc)
      unknown <- setdiff(sc, SCENARIO_LEVELS_DEFAULT)
      
      c(known, sort(unknown))
    })
    
    scen_levels_all <- reactive({
      sc <- scen_codes_ordered()
      c(SCENARIO_LEVELS_DEFAULT, setdiff(sc, SCENARIO_LEVELS_DEFAULT))
    })
    
    # Baseline code = premier code de SCENARIOS_BASE_CODES effectivement demandé / disponible
    scen_base <- reactive({
      sc_eff <- scen_codes_ordered()
      b <- as.character(get("SCENARIO_BASE_YEAR_CODE", inherits = TRUE))
      
      # baseline = base year si dispo, sinon fallback technique
      if (nzchar(b) && b %in% sc_eff) return(b)
      
      b2 <- intersect(get("SCENARIOS_BASE_CODES", inherits = TRUE), sc_eff)
      b2[1] %||% sc_eff[1]
    })
    
    
    # --- Clé scalaire pour bindCache ---------------------------------------
    cache_key <- reactive({
      req(r_country())
      paste0(
        "livestock_stocks|",
        r_country(), "|base_year=", base_year,
        "|sc=", paste(scen_codes_ordered(), collapse = ",")
      )
    })
    
    # Scénarios effectivement utilisables = demandés ∩ présents pour Stocks (ordre conservé)
    scen_levels_effective <- reactive({
      req(r_country())
      
      wanted <- scen_codes_ordered()
      
      present_country <- fact %>%
        filter(
          Region  == r_country(),
          Element == "Stocks",
          Unit    %in% c("Head","1000 Head"),
          Scenario %in% wanted
        ) %>%
        distinct(Scenario) %>%
        pull(Scenario) %>%
        as.character()
      
      keep <- intersect(wanted, present_country)
      
      # Ordre = SCENARIO_LEVELS_DEFAULT (codes), puis append des restants
      ordered <- scen_levels_all()[scen_levels_all() %in% keep]
      rest    <- setdiff(keep, ordered)
      
      unique(c(ordered, rest))
    }) %>% bindCache(cache_key())
    
    # --- 1) Préparation : filtre STOCKS ONLY + construction des 7 items -----
    prep_df <- reactive({
      req(fact, r_country())
      
      need_cols <- c("Region","Element","Unit","Item","Scenario","Year","Value","System","Animal")
      miss <- setdiff(need_cols, names(fact))
      validate(need(length(miss) == 0, paste("Colonnes manquantes :", paste(miss, collapse=", "))))
      
      scen_allowed <- scen_levels_effective()
      validate(need(length(scen_allowed) > 0, "No scenario available for Stocks in this country."))
      
      fact %>%
        filter(
          Region   == r_country(),
          Element  == "Stocks",
          Unit     %in% c("Head","1000 Head"),
          !(Item %in% exclude_items),
          Scenario %in% scen_allowed
        ) %>%
        mutate(
          item_trim   = str_trim(as.character(Item)),
          animal_trim = str_trim(as.character(Animal)),
          system_trim = str_trim(as.character(System)),
          
          system_empty = is.na(System) | system_trim == "",
          
          animal_is_dairy = !is.na(Animal) &
            str_detect(animal_trim, regex("Dairy", ignore_case = TRUE)),
          
          Item_display = case_when(
            animal_is_dairy &
              item_trim %in% c("Dairy cattle", "Beef cattle") ~ "Dairy cattle",
            
            animal_is_dairy &
              item_trim %in% c("Sheep and goats meat", "Sheep and goats milk") ~ "Dairy sheep and goats",
            
            system_empty &
              str_detect(item_trim, regex("Beef cattle", ignore_case = TRUE)) ~ "Beef cattle",
            
            system_empty &
              str_detect(item_trim, regex("Sheep and goats meat", ignore_case = TRUE)) ~ "Sheep and goats meat",
            
            system_empty &
              str_detect(item_trim, regex("Pigs", ignore_case = TRUE)) ~ "Pigs",
            
            system_empty &
              str_detect(item_trim, regex("Poultry eggs", ignore_case = TRUE)) ~ "Poultry eggs",
            
            system_empty &
              str_detect(item_trim, regex("Poultry meat", ignore_case = TRUE)) ~ "Poultry meat",
            
            TRUE ~ NA_character_
          ),
          
          value_head = if_else(Unit == "Head", Value, Value * 1000)
        ) %>%
        filter(!is.na(Item_display)) %>%
        group_by(Item_display, Scenario, Year) %>%
        summarise(value_head = sum(value_head, na.rm = TRUE), .groups = "drop")
    }) %>% bindCache(cache_key())
    
    # --- 2) Compact : baseline (base_year si dispo) + année max par scénario ---
    df_compact <- reactive({
      df <- prep_df()
      if (nrow(df) == 0) {
        return(tibble::tibble(Item_display=character(), serie_code=character(), value_mhead=double()))
      }
      
      b <- scen_base()
      
      # baseline: privilégie base_year, sinon fallback sur max Year pour le scénario baseline
      base_has_year <- df %>%
        filter(Scenario == b, Year == base_year) %>%
        nrow() > 0
      
      base <- if (isTRUE(base_has_year)) {
        df %>%
          filter(Scenario == b, Year == base_year) %>%
          transmute(Item_display, serie_code = as.character(Scenario), value_mhead = value_head / 1e6)
      } else {
        df %>%
          filter(Scenario == b) %>%
          group_by(Item_display) %>%
          filter(Year == max(Year, na.rm = TRUE)) %>%
          ungroup() %>%
          transmute(Item_display, serie_code = as.character(Scenario), value_mhead = value_head / 1e6)
      }
      
      proj <- df %>%
        filter(Scenario != b) %>%
        group_by(Item_display, Scenario) %>%
        filter(Year == max(Year, na.rm = TRUE)) %>%
        ungroup() %>%
        transmute(Item_display, serie_code = as.character(Scenario), value_mhead = value_head / 1e6)
      
      res <- bind_rows(base, proj)
      
      scen_allowed <- scen_levels_effective()
      
      res %>%
        filter(serie_code %in% scen_allowed) %>%
        mutate(
          serie_f = factor(serie_code, levels = scen_allowed)
        ) %>%
        arrange(Item_display, serie_f)
    }) %>% bindCache(cache_key())
    
    # --- 3) Liste des 7 espèces (ordre fixe) --------------------------------
    items <- reactive({
      d <- df_compact()
      if (nrow(d) == 0) return(character(0))
      
      wanted <- c(
        "Beef cattle",
        "Dairy cattle",
        "Sheep and goats meat",
        "Dairy sheep and goats",
        "Pigs",
        "Poultry eggs",
        "Poultry meat"
      )
      
      present <- d %>%
        distinct(Item_display) %>%
        pull(Item_display)
      
      wanted[wanted %in% present]
    }) %>% bindCache(cache_key())
    
    # --- 4) UI : cartes en grille, 3 par ligne ------------------------------
    output$plots_grid <- renderUI({
      it <- items()
      if (length(it) == 0) {
        d0 <- prep_df()
        sm <- function(v) paste(unique(utils::head(v, 6)), collapse = ", ")
        return(
          div(
            class = "alert alert-warning",
            strong("No 'Stocks' data for the selected country."),
            if (nrow(d0) > 0) p(tags$small(
              paste0(
                "Sample — Item_display: [", sm(d0$Item_display),
                "] ; Scenario: [", sm(d0$Scenario), "]"
              )
            ))
          )
        )
      }
      
      cards <- lapply(it, function(sp) {
        plotname <- paste0("plot_", gsub("[^a-zA-Z0-9]+", "_", tolower(sp)))
        div(
          class = "u-card u-card--flat u-card--hover ls-stock-card",
          div(
            class = "u-box",
            p(class = "u-title", sp),
            plotly::plotlyOutput(ns(plotname), height = 280)
          )
        )
      })
      
      rows <- list()
      i <- 1
      while (i <= length(cards)) {
        slice <- cards[i:min(i + 2L, length(cards))]
        cols  <- lapply(slice, function(card) column(width = 4, card))
        rows[[length(rows) + 1L]] <- fluidRow(cols)
        i <- i + 3L
      }
      
      tagList(rows)
    })
    
    # --- 5) Plots -----------------------------------------------------------
    observe({
      d_all <- df_compact()
      it    <- items()
      if (nrow(d_all) == 0 || length(it) == 0) return(invisible())
      
      scen_allowed <- scen_levels_effective()
      present      <- unique(as.character(d_all$serie_code))
      series_lvls  <- scen_allowed[scen_allowed %in% present]
      if (!length(series_lvls)) series_lvls <- present
      
      pal <- scenario_palette(levels = series_lvls)
      
      b <- scen_base()
      
      base_tbl <- d_all %>%
        filter(as.character(serie_code) == b) %>%
        group_by(Item_display) %>%
        summarise(base_val_m = sum(value_mhead, na.rm = TRUE), .groups = "drop")
      
      is_pig_item_raw <- function(x) grepl("(pig|porc|swine|hog|pork)", x, ignore.case = TRUE)
      
      for (sp in it) local({
        specie   <- sp
        plotname <- paste0("plot_", gsub("[^a-zA-Z0-9]+", "_", tolower(specie)))
        
        df_sp <- d_all %>%
          filter(Item_display == specie) %>%
          left_join(base_tbl, by = "Item_display") %>%
          mutate(
            serie_f = factor(as.character(serie_code), levels = series_lvls),
            is_pig  = is_pig_item_raw(Item_display),
            value_plot = if_else(is_pig, value_mhead * 1000, value_mhead),
            axis_lab   = if_else(is_pig, "thousand heads", "million heads"),
            base_plot  = if_else(is_pig, base_val_m * 1000, base_val_m),
            delta_pct  = if_else(is.finite(base_plot) & base_plot > 0, 100 * (value_plot / base_plot - 1), NA_real_),
            show_pct = as.character(serie_code) %in% SHOW_PCT_CODES,
            lbl = if_else(
              as.character(serie_code) == b | !show_pct,
              "",
              if_else(
                is.finite(delta_pct),
                paste0(scales::number(delta_pct, accuracy = 1, signed = TRUE), "%"),
                ""
              )
            )
            
          )
        
        unit_label <- unique(df_sp$axis_lab)
        if (length(unit_label) != 1L) unit_label <- unit_label[1L]
        
        ymax <- max(df_sp$value_plot, na.rm = TRUE)
        gap  <- ymax * 0.03
        ypad <- ymax * 0.16 + gap
        
        output[[plotname]] <- plotly::renderPlotly({
          validate(need(nrow(df_sp) > 0, "No data for this species."))
          
          p <- ggplot(
            df_sp,
            aes(x = serie_f, y = value_plot, fill = serie_f)
          ) +
            geom_col(width = 0.7, alpha = 0.95) +
            geom_text(
              aes(label = lbl),
              position = position_nudge(y = gap),
              vjust = 0, size = 3, lineheight = 0.95
            ) +
            scale_x_discrete(labels = function(x) scenario_label_vec(x)) +
            scale_y_continuous(
              limits = c(0, ymax + ypad),
              labels = if (any(df_sp$is_pig)) {
                scales::label_number(accuracy = 1, suffix = " k")
              } else {
                scales::label_number(accuracy = 1, suffix = " M")
              }
            ) +
            labs(x = NULL, y = unit_label) +
            theme_minimal(base_size = 8) +
            theme(
              legend.position = "none",
              axis.text.x = element_text(hjust = 0.5),
              axis.title.y = element_text(margin = margin(r = 10)),
              plot.margin = grid::unit(c(4, 24, 12, 40), "pt"),
              panel.background = element_rect(fill = NA, colour = NA),
              plot.background  = element_rect(fill = NA, colour = NA)
            ) +
            scale_fill_manual(values = pal[levels(df_sp$serie_f)])
          
          pl <- ggplotly(p, tooltip = c("x", "y"))
          
          pl <- layout(
            pl,
            paper_bgcolor = "rgba(0,0,0,0)",
            plot_bgcolor  = "rgba(0,0,0,0)",
            margin = list(l = 40, r = 24, b = 36, t = 8, pad = 0)
          )
          
          pl <- config(
            pl,
            displayModeBar = TRUE,
            modeBarButtonsToRemove = c(
              "lasso2d", "select2d", "zoom2d", "zoomIn2d",
              "zoomOut2d", "autoScale2d", "pan2d", "resetScale2d"
            ),
            modeBarButtonsToAdd = list("toImage"),
            toImageButtonOptions = list(
              format = "png",
              filename = paste0("livestock_", gsub("[^a-zA-Z0-9]+","_", specie)),
              width = 1600,
              height = 900,
              scale = 2
            )
          )
          
          pl
        })
      })
    })
    
    # --- Download CSV : STOCKS ONLY (7 items) -------------------------------
    output$dl_livestock_stocks_csv <- downloadHandler(
      filename = function() {
        paste0("Livestock_stocks_", gsub(" ", "_", r_country()), ".csv")
      },
      content = function(file) {
        d <- df_compact()
        req(nrow(d) > 0)
        
        out <- d %>%
          mutate(
            is_pig = grepl("(pig|porc|swine|hog|pork)", Item_display, ignore.case = TRUE),
            Unit = if_else(is_pig, "Thousands of heads (k)", "Millions of heads (M)")
          ) %>%
          arrange(Item_display, serie_f) %>%
          transmute(
            Country        = r_country(),
            Species        = Item_display,
            Scenario_code  = as.character(serie_code),
            Scenario_label = scenario_label_vec(serie_code),
            Value_million_heads = value_mhead,
            Unit           = Unit
          )
        
        readr::write_delim(out, file, delim = ";")
      }
    )
    
    # --- Note explicative ---------------------------------------------------
    output$note <- renderUI({
      d <- df_compact()
      if (nrow(d) == 0) return(NULL)
      
      txt <- glue::glue(
        "<p>
        This set of charts shows, for the selected country, the <strong>stock of animals</strong> (number of heads)
        in the <strong>baseline</strong> and under the selected scenarios.<br>
        Each card corresponds to one aggregated species group and displays the total stock for each scenario.
        Values are expressed in <strong>millions of heads</strong> (and in <strong>thousands of heads</strong> for pigs when needed for readability).<br>
        The percentage labels above the bars indicate the change in total stock compared with the baseline for each scenario.
        </p>"
      )
      htmltools::HTML(txt)
    })
    
    invisible(NULL)
  })
}
