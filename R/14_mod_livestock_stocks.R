# R/14_mod_livestock_stocks.R
# ======================================================================
# Graphs "Stocks" (nombre de têtes) par espèce et par scénario (barres)
# Données requises: Region, Element, Unit, Item, Scenario, Year, Value
# - Filtre: Region ; Element == "Stocks" ; Unit ∈ {"Head","1000 Head"}
# - Conversion en têtes, affichage en millions
# - 1 espèce = 1 graphique, 2 graphiques par ligne
# - Couleurs centralisées via scenario_palette(levels) (R/02)
# - Baseline = "Année de base" en 2018 (fixe)
# ======================================================================

mod_livestock_stocks_ui <- function(id,
                                    title = "Stocks d’animaux (têtes)") {
  ns <- NS(id)
  tagList(
    div(class = "card",
        div(class = "card-body",
            h3(class = "card-title", title),
            p(class = "text-muted",
              "Chaque carte présente le nombre de têtes par espèce : comparaison entre ",
              "l’Année de base et les scénarios. Les unités 'Head' et '1000 Head' sont harmonisées."
            ),
            uiOutput(ns("plots_grid"))
        )
    )
  )
}

mod_livestock_stocks_server <- function(
    id,
    fact,        # objet tibble/data.frame
    r_country    # reactive(country)
){
  exclude_items <- c("Beef cattle equivalent", "Dairy cattle equivalent","Beef cattle")
  moduleServer(id, function(input, output, session){
    
    # --- 1) Préparation : filtre + harmonisation en têtes ---------------------
    prep_df <- reactive({
      req(fact, r_country())
      df <- fact
      
      need_cols <- c("Region","Element","Unit","Item","Scenario","Year","Value")
      miss <- setdiff(need_cols, names(df))
      validate(need(length(miss) == 0, paste("Colonnes manquantes :", paste(miss, collapse=", "))))
      
      df |>
        dplyr::filter(
          .data$Region  == r_country(),
          .data$Element == "Stocks",
          .data$Unit    %in% c("Head","1000 Head"),
          !(.data$Item %in% exclude_items) 
        ) |>
        dplyr::mutate(
          value_head = dplyr::if_else(.data$Unit == "Head", .data$Value, .data$Value * 1000L)
        ) |>
        dplyr::group_by(.data$Item, .data$Scenario, .data$Year) |>
        dplyr::summarise(value_head = sum(.data$value_head, na.rm = TRUE), .groups = "drop")
    })
    
    # --- 2) Compact : baseline 2018 + année max par scénario ------------------
    df_compact <- reactive({
      df <- prep_df()
      if (nrow(df) == 0) {
        return(tibble::tibble(Item=character(), serie=character(), value_mhead=double()))
      }
      
      base <- df |>
        dplyr::filter(.data$Scenario == "Année de base", .data$Year == 2018L) |>
        dplyr::transmute(Item, serie = .data$Scenario, value_mhead = .data$value_head / 1e6)
      
      proj <- df |>
        dplyr::filter(.data$Scenario != "Année de base") |>
        dplyr::group_by(.data$Item, .data$Scenario) |>
        dplyr::filter(.data$Year == max(.data$Year, na.rm = TRUE)) |>
        dplyr::ungroup() |>
        dplyr::transmute(Item, serie = .data$Scenario, value_mhead = .data$value_head / 1e6)
      
      dplyr::bind_rows(base, proj)
    })
    
    # --- 2b) Préparation LSU (TLU) pour Dairy & Beef cattle -----------------------
    prep_df_lsu <- reactive({
      req(fact, r_country())
      df <- fact
      
      need_cols <- c("Region","Element","Unit","Item","Scenario","Year","Value")
      miss <- setdiff(need_cols, names(df))
      validate(need(length(miss) == 0, paste("Colonnes manquantes :", paste(miss, collapse=", "))))
      
      df |>
        dplyr::filter(
          .data$Region  == r_country(),
          .data$Element == "LSU",
          .data$Item    %in% c("Dairy", "Beef cattle")
        ) |>
        dplyr::mutate(
          # Si l'unité contient "1000", on remet à l'unité (x1000). Sinon, on garde tel quel.
          mult = dplyr::if_else(grepl("1000", .data$Unit, ignore.case = TRUE), 1000, 1L),
          value_lsu = .data$Value * .data$mult
        ) |>
        dplyr::group_by(.data$Item, .data$Scenario, .data$Year) |>
        dplyr::summarise(value_lsu = sum(.data$value_lsu, na.rm = TRUE), .groups = "drop")
    })
    
    # --- 2c) Compact LSU : baseline 2018 + année max par scénario -----------------
    df_compact_lsu <- reactive({
      d <- prep_df_lsu()
      if (nrow(d) == 0) {
        return(tibble::tibble(Item=character(), serie=character(), value_klsu=double()))
      }
      
      base <- d |>
        dplyr::filter(.data$Scenario == "Année de base", .data$Year == 2018L) |>
        dplyr::transmute(Item, serie = .data$Scenario, value_klsu = .data$value_lsu / 1e3)  # milliers
      
      proj <- d |>
        dplyr::filter(.data$Scenario != "Année de base") |>
        dplyr::group_by(.data$Item, .data$Scenario) |>
        dplyr::filter(.data$Year == max(.data$Year, na.rm = TRUE)) |>
        dplyr::ungroup() |>
        dplyr::transmute(Item, serie = .data$Scenario, value_klsu = .data$value_lsu / 1e3)  # milliers
      
      dplyr::bind_rows(base, proj)
    })
    
    # --- 2d) Fusion STOCKS + LSU pour l’affichage --------------------------------
    df_compact_all <- reactive({
      d_stk <- df_compact()      # a déjà value_mhead (Millions de têtes)
      d_lsu <- df_compact_lsu()  # a déjà value_klsu (Milliers de TLU)
      
      # STOCKS -> structure commune
      stocks_std <- d_stk |>
        dplyr::transmute(
          Item_display = .data$Item,
          Item, serie,
          type = "stocks",
          value_plot_base = .data$value_mhead,     # par défaut millions de têtes
          unit_axis = "Millions de têtes"
        )
      
      # LSU -> deux nouvelles cartes "Dairy (TLU)" et "Beef cattle (TLU)"
      lsu_std <- d_lsu |>
        dplyr::mutate(Item_display = paste0(.data$Item, " (TLU)")) |>
        dplyr::transmute(
          Item_display,
          Item, serie,
          type = "lsu",
          value_plot_base = .data$value_klsu,      # milliers de TLU
          unit_axis = "Milliers de TLU"
        )
      
      dplyr::bind_rows(stocks_std, lsu_std)
    })
    
    
    # --- 3) Liste des espèces -------------------------------------------------
    items <- reactive({
      d <- df_compact_all()
      if (nrow(d) == 0) return(character(0))
      d |>
        dplyr::distinct(.data$Item_display) |>
        dplyr::arrange(.data$Item_display) |>
        dplyr::pull()
    })
  
    
    # --- 4) UI : grille 2 par ligne ------------------------------------------
    output$plots_grid <- renderUI({
      it <- items()
      if (length(it) == 0) {
        d0 <- prep_df()
        sm <- function(v) paste(unique(utils::head(v, 6)), collapse = ", ")
        return(div(class="alert alert-warning",
                   strong("Aucune donnée 'Stocks' pour la région sélectionnée."),
                   if (nrow(d0) > 0) p(tags$small(
                     paste0("Échantillon — Element: [", sm(d0$Element),
                            "] ; Unit: [", sm(d0$Unit),
                            "] ; Scenario: [", sm(d0$Scenario), "]")
                   ))
        ))
      }
      
      rows <- list(); i <- 1
      while (i <= length(it)) {
        pair <- it[i:min(i+1, length(it))]
        cols <- lapply(pair, function(sp) {
          plotname <- paste0("plot_", gsub("[^a-zA-Z0-9]+", "_", tolower(sp)))
          column(
            width = 6,
            div(class = "card mb-4",
                div(class = "card-body",
                    h4(class = "card-title card-title--sm", sp),      # <— sous-titre dans le body
                    plotly::plotlyOutput(NS(id, plotname), height = 200)
                )
            )
          )
        })
        
        rows[[length(rows) + 1]] <- fluidRow(cols)
        i <- i + 2
      }
      tagList(rows)
    })
    
    # --- 5) Plots (delta% sans baseline, labels surélevés, cochons en milliers) ---
    # --- 5) Plots (delta% sans baseline, labels surélevés, cochons en milliers,
    #               intégration TLU pour Dairy & Beef) -----------------------------
    observe({
      d_all <- df_compact_all(); it <- items()
      if (nrow(d_all) == 0 || length(it) == 0) return(invisible())
      
      present     <- unique(d_all$serie)
      series_lvls <- SCENARIO_LEVELS_DEFAULT[SCENARIO_LEVELS_DEFAULT %in% present]
      pal         <- scenario_palette(levels = series_lvls)
      
      # Baseline par "Item_display" (pour calcul % vs 2018 par carte)
      base_tbl <- d_all |>
        dplyr::filter(.data$serie == "Année de base") |>
        dplyr::group_by(.data$Item_display) |>
        dplyr::summarise(base_val = sum(.data$value_plot_base, na.rm = TRUE), .groups = "drop")
      
      # helper cochons sur les noms bruts (Item) stocks
      is_pig_item_raw <- function(x) grepl("(pig|porc|swine|hog|pork)", x, ignore.case = TRUE)
      
      for (sp in it) local({
        specie   <- sp
        plotname <- paste0("plot_", gsub("[^a-zA-Z0-9]+", "_", tolower(specie)))
        
        df_sp <- d_all |>
          dplyr::filter(.data$Item_display == specie) |>
          dplyr::left_join(base_tbl, by = "Item_display") |>
          dplyr::mutate(
            serie = factor(.data$serie, levels = series_lvls),
            
            # Détection type
            is_lsu = .data$type == "lsu",
            
            # Pour STOCKS : par défaut "Millions de têtes".
            # Si l'espèce est cochon -> on veut "Milliers de têtes": on *multiplie* par 1000
            # pour passer de M têtes à k têtes, et on ajuste l'unité d'axe.
            is_pig = is_pig_item_raw(.data$Item) & !.data$is_lsu,
            
            value_plot = dplyr::if_else(.data$is_lsu, .data$value_plot_base,
                                        dplyr::if_else(.data$is_pig, .data$value_plot_base * 1000, .data$value_plot_base)
            ),
            axis_lab = dplyr::if_else(.data$is_lsu, "Milliers de TLU",
                                      dplyr::if_else(.data$is_pig, "Milliers de têtes", "Millions de têtes")
            ),
            
            # baseline pour le % (cohérente avec la même unité que value_plot)
            base_plot = dplyr::if_else(.data$is_lsu, .data$base_val,
                                       dplyr::if_else(.data$is_pig, .data$base_val * 1000, .data$base_val)
            ),
            
            delta_pct = 100 * (.data$value_plot / .data$base_plot - 1),
            lbl       = dplyr::if_else(
              .data$serie == "Année de base", "",
              paste0(scales::number(.data$delta_pct, accuracy = 1, signed = TRUE), "%")
            )
          )
        
        # Un label d'axe unique (identique pour toutes les lignes de cette carte)
        unit_label <- unique(df_sp$axis_lab)
        if (length(unit_label) != 1L) unit_label <- unit_label[1L]
        
        ymax <- max(df_sp$value_plot, na.rm = TRUE)
        gap  <- ymax * 0.03
        ypad <- ymax * 0.16 + gap
        
        output[[plotname]] <- plotly::renderPlotly({
          validate(need(nrow(df_sp) > 0, "Pas de données pour cette espèce."))
          
          p <- ggplot2::ggplot(
            df_sp,
            ggplot2::aes(x = .data$serie, y = .data$value_plot, fill = .data$serie)
          ) +
            ggplot2::geom_col(width = 0.7, alpha = 0.95) +
            ggplot2::geom_text(
              ggplot2::aes(label = .data$lbl),
              position = ggplot2::position_nudge(y = gap),
              vjust = 0, size = 3, lineheight = 0.95
            ) +
            ggplot2::scale_y_continuous(
              limits = c(0, ymax + ypad),
              labels = if (unique(df_sp$is_lsu)) {
                # Milliers de TLU
                scales::label_number(accuracy = 1, suffix = " k")
              } else if (any(df_sp$is_pig)) {
                # Milliers de têtes
                scales::label_number(accuracy = 1, suffix = " k")
              } else {
                # Millions de têtes
                scales::label_number(accuracy = 0.1, suffix = " M")
              }
            ) +
            ggplot2::labs(x = NULL, y = unit_label) +
            ggplot2::theme_minimal(base_size = 8) +
            ggplot2::theme(
              legend.position = "none",
              axis.text.x = ggplot2::element_text(hjust = 0.5),
              plot.margin = grid::unit(c(4, 24, 12, 40), "pt")
            ) +
            ggplot2::scale_fill_manual(values = pal[levels(df_sp$serie)])
          
          pl <- plotly::ggplotly(p, tooltip = c("x", "y"))
          pl <- plotly::layout(
            pl,
            margin = list(l = 40, r = 24, b = 36, t = 8, pad = 0),
            xaxis  = list(automargin = FALSE, fixedrange = TRUE),
            yaxis  = list(automargin = FALSE, fixedrange = TRUE)
          )
          plotly::config(pl, displayModeBar = FALSE)
        })
      })
    })
  })
}

