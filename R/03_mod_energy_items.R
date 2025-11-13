# R/03_mod_energy_items.R
# ------------------------------------------------------------------
# Graphique "FoodEnergy per capita — détail par Item" (stacked bar)
# + Option "Total uniquement"
# + Triptyque de camemberts sous le graphique (part des items)
# ------------------------------------------------------------------

mod_energy_items_ui <- function(id){
  ns <- NS(id)
  tagList(
    uiOutput(ns("block"))
  )
}

mod_energy_items_server <- function(
    id,
    fact,
    r_country,
    r_items,
    ENERGY_ELEMENT = "FoodEnergy per capita"
){
  moduleServer(id, function(input, output, session){
    
    SCEN_SHOW <- c("Même diète", "Diète probable", "Diète saine")
    
    # === block UI =============================================================
    output$block <- renderUI({
      sel <- r_items()
      if (is.null(sel) || length(sel) == 0) return(NULL)
      ns <- session$ns
      
      div(
        class = "card",
        div(
          class = "card-body",
          h3("FoodEnergy per capita (kcal/j/hab) — détail par Item"),
          
          # contrôle
          div(
            class = "u-controls",
            checkboxInput(
              inputId = ns("only_total"),
              label   = "Afficher uniquement le total",
              value   = FALSE
            )
          ),
          
          # plot barres + camemberts
          plotly::plotlyOutput(ns("bar_energy_items"), height = "460px"),
          # camemberts seulement si on n'est PAS en "total uniquement"
          conditionalPanel(
            condition = sprintf("!input['%s']", ns("only_total")),
            plotly::plotlyOutput(ns("pie_energy_items"), height = "390px")
          ),
          br(),
          
          # note
          p(tags$em(
            "Note : les barres représentent la somme des items sélectionnés pour chaque scénario. ",
            "Trois scénarios sont affichés : « Même diète », « Diète probable » et « Diète saine »."
          )),
          
          # actions (CSV)
          div(
            class = "u-actions",
            downloadLink(ns("dl_plot"), label = tagList(icon("download"), "CSV"))
          )
        )
      )
    })
    # ==========================================================================
    
    # années par scénario ------------------------------------------------------
    years_by_scenario <- reactive({
      fact %>%
        dplyr::filter(
          Region == r_country(),
          stringr::str_trim(Element) == ENERGY_ELEMENT,
          Scenario %in% SCEN_SHOW
        ) %>%
        dplyr::group_by(Scenario) %>%
        dplyr::summarise(
          year_used = suppressWarnings(max(Year[!is.na(Value)], na.rm = TRUE)),
          .groups = "drop"
        ) %>%
        dplyr::filter(is.finite(year_used)) %>%
        dplyr::mutate(Scenario = forcats::fct_relevel(Scenario, SCEN_SHOW)) %>%
        dplyr::arrange(Scenario)
    }) %>% bindCache(r_country(), ENERGY_ELEMENT)
    
    # données items ------------------------------------------------------------
    data_items <- reactive({
      req(!is.null(r_items()), length(r_items()) > 0)
      yrs <- years_by_scenario()
      validate(need(nrow(yrs) > 0, paste0("Aucune donnée pour « ", ENERGY_ELEMENT, " » dans ce pays.")))
      
      df <- fact %>%
        dplyr::inner_join(yrs, by = "Scenario") %>%
        dplyr::filter(
          Region == r_country(),
          stringr::str_trim(Element) == ENERGY_ELEMENT,
          Year == year_used,
          Item %in% r_items()
        ) %>%
        dplyr::group_by(Scenario, Item) %>%
        dplyr::summarise(
          value = sum(Value, na.rm = TRUE),
          unit  = dplyr::first(stats::na.omit(Unit)),
          year  = dplyr::first(Year),
          .groups = "drop"
        )
      
      ylab <- if (nrow(df) == 0 || all(is.na(df$unit))) "Valeur" else as.character(df$unit[which(!is.na(df$unit))[1]])
      
      tidyr::expand_grid(
        Scenario = factor(SCEN_SHOW, levels = SCEN_SHOW),
        Item     = r_items()
      ) %>%
        dplyr::left_join(dplyr::select(yrs, Scenario, year_used), by = "Scenario") %>%
        dplyr::left_join(df, by = c("Scenario","Item")) %>%
        dplyr::mutate(
          value    = dplyr::coalesce(value, 0),
          unit     = dplyr::coalesce(unit, ylab),
          year     = dplyr::coalesce(year, year_used),
          Scenario = forcats::fct_relevel(Scenario, SCEN_SHOW),
          .ylab    = ylab
        ) %>%
        dplyr::arrange(Scenario, Item)
    }) %>% bindCache(r_country(), r_items(), ENERGY_ELEMENT)
    
    # --- barres empilées / total ---------------------------------------------
    output$bar_energy_items <- plotly::renderPlotly({
      pd <- data_items(); req(nrow(pd) > 0)
      validate(need(sum(pd$value, na.rm = TRUE) > 0, "Aucune valeur non nulle à afficher."))
      
      ylab <- unique(pd$.ylab)[1]
      pd   <- pd %>% dplyr::mutate(Item = forcats::fct_relevel(Item, sort(unique(Item))))
      show_total_only <- isTRUE(input$only_total)
      
      # palette commune barres + camemberts
      cols <- if (exists("item_colors_for"))
        item_colors_for(levels(pd$Item))
      else
        scales::hue_pal()(length(levels(pd$Item)))
      names(cols) <- levels(pd$Item)
      
      if (show_total_only) {
        pd_tot <- pd %>% dplyr::group_by(Scenario) %>%
          dplyr::summarise(total = sum(value, na.rm = TRUE), .groups = "drop") %>%
          dplyr::mutate(lbl = scales::number(total, big.mark = " "))
        
        gg <- ggplot2::ggplot(pd_tot, ggplot2::aes(Scenario, total)) +
          ggplot2::geom_col(width = 0.8, fill = "#CCCCCC") +
          ggplot2::geom_text(ggplot2::aes(y = total/2, label = lbl),
                             fontface = "bold", size = 4, color = "black") +
          ggplot2::labs(x = NULL, y = ylab) +
          ggplot2::scale_y_continuous(labels = scales::label_number(big.mark = " ")) +
          ggplot2::theme_minimal(base_size = 13) +
          ggplot2::theme(panel.grid.minor = ggplot2::element_blank(),
                         legend.position  = "none",
                         panel.background = ggplot2::element_rect(fill = "transparent", colour = NA),
                         plot.background  = ggplot2::element_rect(fill = "transparent", colour = NA))
        
        plotly::ggplotly(gg, tooltip = c("x","y")) %>%
          plotly::layout(paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)") %>%
          plotly::config(displaylogo = FALSE)
        
      } else {
        gg <- ggplot2::ggplot(pd, ggplot2::aes(Scenario, value, fill = Item)) +
          ggplot2::geom_col(width = 0.8, position = "stack") +
          ggplot2::scale_fill_manual(
            values = cols,
            drop   = FALSE
          ) +
          ggplot2::labs(x = NULL, y = ylab, fill = "Item") +
          ggplot2::scale_y_continuous(labels = scales::label_number(big.mark = " ")) +
          ggplot2::theme_minimal(base_size = 13) +
          ggplot2::theme(panel.grid.minor = ggplot2::element_blank(),
                         legend.position  = "right",
                         panel.background = ggplot2::element_rect(fill = "transparent", colour = NA),
                         plot.background  = ggplot2::element_rect(fill = "transparent", colour = NA))
        
        plotly::ggplotly(gg, tooltip = c("x","fill","y")) %>%
          plotly::layout(paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)") %>%
          plotly::config(displaylogo = FALSE)
      }
    })
    
    # --- camemberts par scénario ---------------------------------------------
    output$pie_energy_items <- plotly::renderPlotly({
      # si "total uniquement" coché, on ne fait rien (UI masqué de toute façon)
      if (isTRUE(input$only_total)) return(NULL)
      
      pd <- data_items(); req(nrow(pd) > 0)
      validate(need(sum(pd$value, na.rm = TRUE) > 0, "Aucune valeur non nulle à afficher."))
      
      ylab <- unique(pd$.ylab)[1]
      
      # même ordre d'items que pour les barres
      pd <- pd %>% dplyr::mutate(Item = forcats::fct_relevel(Item, sort(unique(Item))))
      
      # mêmes couleurs que les barres
      cols <- if (exists("item_colors_for"))
        item_colors_for(levels(pd$Item))
      else
        scales::hue_pal()(length(levels(pd$Item)))
      names(cols) <- levels(pd$Item)
      
      # parts par scénario + texte pourcentages + position texte
      df_share <- pd %>%
        dplyr::group_by(Scenario) %>%
        dplyr::mutate(
          total = sum(value, na.rm = TRUE),
          share = dplyr::if_else(total > 0, value / total, NA_real_),
          label_pct = dplyr::if_else(
            !is.na(share) & total > 0,
            scales::percent(share, accuracy = 1),   # "15%"
            NA_character_
          ),
          # en dehors du camembert si part < 5 %
          text_pos = dplyr::if_else(share < 0.05, "outside", "inside")
        ) %>%
        dplyr::ungroup()
      
      # domaines horizontaux : exactement par tiers de la largeur
      x_domains <- list(
        c(0.00, 1/3),   # Même diète
        c(1/3, 2/3),    # Diète probable
        c(2/3, 1.00)    # Diète saine
      )
      
      p <- plotly::plot_ly()
      
      for(i in seq_along(SCEN_SHOW)){
        sc <- SCEN_SHOW[i]
        d  <- df_share %>%
          dplyr::filter(Scenario == sc, total > 0, value > 0)
        if (nrow(d) == 0) next
        
        p <- p %>%
          plotly::add_pie(
            data   = d,
            labels = ~Item,
            values = ~share,
            text   = ~label_pct,       # pourcentages arrondis
            textinfo = "text",
            name   = sc,
            domain = list(x = x_domains[[i]], y = c(0, 1)),
            sort   = FALSE,
            textposition = ~text_pos,  # inside / outside selon la part
            textfont     = list(color = "black", size = 12),  # même taille, noir
            insidetextorientation = "horizontal",
            marker       = list(colors = cols[d$Item]),
            customdata   = ~value,
            hovertemplate = paste0(
              "<b>", sc, "</b><br>",
              "%{label}<br>",
              "Part : %{percent}<br>",
              "Valeur : %{customdata} ", ylab,
              "<extra></extra>"
            ),
            showlegend = (i == 1)      # une seule légende
          )
      }
      
      p %>%
        plotly::layout(
          margin        = list(t = 45, b = 60, l = 70, r = 200),
          paper_bgcolor = "rgba(0,0,0,0)",
          plot_bgcolor  = "rgba(0,0,0,0)",
          legend        = list(
            orientation = "h",
            x = 0.5,
            xanchor = "center",
            y = 1.15   # légende au-dessus de la rangée de camemberts
          )
        ) %>%
        plotly::config(displaylogo = FALSE)
    })
    
    
    
    # --- export CSV -----------------------------------------------------------
    output$dl_plot <- downloadHandler(
      filename = function(){ paste0("FoodEnergy_", gsub(" ", "_", r_country()), ".csv") },
      content  = function(file){
        pd <- data_items(); req(nrow(pd) > 0)
        ylab <- unique(pd$.ylab)[1]
        readr::write_delim(
          pd %>% dplyr::transmute(
            Pays = r_country(),
            Scenario = as.character(Scenario),
            Annee = year,
            Item, Valeur = value, Unite = ylab
          ),
          file, delim = ";"
        )
      }
    )
  })
}

