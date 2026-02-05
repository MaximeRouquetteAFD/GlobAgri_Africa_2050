# R/0.1_mod_energy_items.R
# ------------------------------------------------------------------
# Graphique "FoodEnergy per capita — détail par Item" (stacked bar)
# + Vue détaillée sous forme :
#    - soit camemberts des parts d'énergie par produit
#    - soit camemberts des parts d'apport protéique (module R/16)
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
    
    # ------------------------------------------------------------------
    # Scenarios: BASE ONLY, but ordered by the new config
    # ------------------------------------------------------------------
    shiny::validate(
      shiny::need(exists("SCENARIOS_BASE_CODES", inherits = TRUE),
                  "Missing config: SCENARIOS_BASE_CODES"),
      shiny::need(exists("SCENARIO_LEVELS_DEFAULT", inherits = TRUE),
                  "Missing config: SCENARIO_LEVELS_DEFAULT")
    )
    
    label_scen <- function(x){
      if (exists("scenario_label", mode = "function", inherits = TRUE)) {
        vapply(x, scenario_label, FUN.VALUE = character(1))
      } else {
        as.character(x)
      }
    }
    
    # Base scenarios (codes) in global config order
    SCEN_SHOW <- intersect(SCENARIO_LEVELS_DEFAULT, SCENARIOS_BASE_CODES)
    if (length(SCEN_SHOW) == 0) SCEN_SHOW <- SCENARIOS_BASE_CODES
    
    SCEN_SHOW_KEY <- paste(SCEN_SHOW, collapse = "|")
    SCEN_SHOW_UI_LEVELS <- label_scen(SCEN_SHOW)
    
    # Scalar cache key for items
    items_key <- reactive({
      sel <- r_items()
      if (is.null(sel) || length(sel) == 0) "" else paste(sort(sel), collapse = "|")
    })
    
    # === block UI ===========================================================
    output$block <- renderUI({
      sel <- r_items()
      if (is.null(sel) || length(sel) == 0) return(NULL)
      ns <- session$ns
      
      div(
        class = "card",
        div(
          class = "card-body",
          div(
            class = "u-row diet-cards",
            
            div(
              class = "u-card u-card--flat diet-card",
              div(class = "diet-card-title", "Same diet"),
              div(
                class = "diet-card-text",
                "This diet projects base year diets (2018) to 2050. It mainly isolates the demographic effect on agricultural land demand in 2050."
              )
            ),
            
            div(
              class = "u-card u-card--flat diet-card",
              div(class = "diet-card-title", "Healthy diet"),
              div(
                class = "diet-card-text",
                "This normative diet represents a desirable pathway (nutrition, diversification) promoted by international agencies for population health and sustainable food systems. It is inspired by FAO/WHO recommendations and the Agrimonde/Lancet work."
              )
                ),
            
            div(
              class = "u-card u-card--flat diet-card",
              div(class = "diet-card-title", "Likely diet"),
              div(
                class = "diet-card-text",
                "This diet is based on projections of Alexandratos & Bruinsma (2012), which rely on economic growth, inequalities and undernourishment. It describes a median trajectory grounded in historical trends, without assuming a voluntary change in dietary habits."
              )
            )
          ),
          
          tags$br(),
          
          plotly::plotlyOutput(ns("bar_energy_items"), height = "430px"),
          
          div(
            class = "u-controls",
            selectInput(
              inputId = ns("view_mode"),
              label   = "As a share",
              choices = c(
                "Share of energy intake by product (%)"   = "energy",
                "Share of protein intake by product (%)" = "protein"
              ),
              selected = "energy"
            )
          ),
          
          conditionalPanel(
            condition = sprintf("input['%s'] == 'energy'", ns("view_mode")),
            plotly::plotlyOutput(ns("pie_energy_items"), height = "370px")
          ),
          conditionalPanel(
            condition = sprintf("input['%s'] == 'protein'", ns("view_mode")),
            uiOutput(ns("protein_plot"))
          ),
          div(
            class = "u-actions",
            downloadLink(ns("dl_plot"), label = tagList(icon("download"), "CSV"))
          ),
          tags$br(),
          uiOutput(ns("note"))
        )
      )
    })
    # =======================================================================
    
    # années par scénario ----------------------------------------------------
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
          .groups   = "drop"
        ) %>%
        dplyr::filter(is.finite(year_used)) %>%
        dplyr::mutate(Scenario = forcats::fct_relevel(Scenario, SCEN_SHOW)) %>%
        dplyr::arrange(Scenario)
    }) %>% bindCache(r_country(), ENERGY_ELEMENT, SCEN_SHOW_KEY)
    
    # données items ----------------------------------------------------------
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
          
          # UI scenario (labels) while keeping Scenario as code for joins
          Scenario_ui = factor(
            label_scen(as.character(Scenario)),
            levels = SCEN_SHOW_UI_LEVELS
          ),
          
          .ylab    = ylab
        ) %>%
        dplyr::arrange(Scenario, Item)
    }) %>% bindCache(r_country(), items_key(), ENERGY_ELEMENT, SCEN_SHOW_KEY)
    
    # --- barres empilées (toujours) ----------------------------------------
    output$bar_energy_items <- plotly::renderPlotly({
      pd <- data_items(); req(nrow(pd) > 0)
      validate(need(sum(pd$value, na.rm = TRUE) > 0, "Aucune valeur non nulle à afficher."))
      
      th <- get_plotly_tokens()
      
      ylab <- unique(pd$.ylab)[1]
      pd   <- pd %>% dplyr::mutate(Item = forcats::fct_relevel(Item, sort(unique(Item))))
      
      totals <- pd %>%
        dplyr::group_by(Scenario_ui) %>%
        dplyr::summarise(total = sum(value, na.rm = TRUE), .groups = "drop")
      
      ymax  <- max(totals$total, na.rm = TRUE)
      y_pad <- ymax * 0.08
      
      cols <- if (exists("item_colors_for"))
        item_colors_for(levels(pd$Item))
      else
        scales::hue_pal()(length(levels(pd$Item)))
      names(cols) <- levels(pd$Item)
      
      gg <- ggplot2::ggplot(pd, ggplot2::aes(Scenario_ui, value, fill = Item)) +
        ggplot2::geom_col(width = 0.8, position = "stack") +
        ggplot2::geom_text(
          data = totals,
          ggplot2::aes(
            x = Scenario_ui,
            y = total * 1.04,
            label = scales::number(total, big.mark = " ", accuracy = 1)
          ),
          inherit.aes = FALSE,
          vjust = 0,
          size  = 4.5,
          color = th$font_color
        ) +
        ggplot2::scale_fill_manual(values = cols, drop = FALSE) +
        ggplot2::labs(x = NULL, y = ylab, fill = "Item") +
        ggplot2::scale_y_continuous(
          labels = scales::label_number(big.mark = " "),
          limits = c(0, ymax + y_pad),
          expand = ggplot2::expansion(mult = c(0, 0))
        ) +
        ggplot2::theme_minimal(base_size = 13) +
        ggplot2::theme(
          panel.grid.minor = ggplot2::element_blank(),
          legend.position  = "right",
          panel.background = ggplot2::element_rect(fill = "transparent", colour = NA),
          plot.background  = ggplot2::element_rect(fill = "transparent", colour = NA),
          axis.text  = ggplot2::element_text(color = th$muted_color),
          axis.title = ggplot2::element_text(color = th$muted_color),
          legend.text  = ggplot2::element_text(color = th$muted_color),
          legend.title = ggplot2::element_text(color = th$muted_color)
        )
      
      p <- plotly::ggplotly(gg, tooltip = c("x","fill","y")) %>%
        plotly::layout(
          paper_bgcolor = APP_TRANSPARENT,
          plot_bgcolor  = APP_TRANSPARENT
        ) %>%
        plotly::config(displaylogo = FALSE)
      
      p <- plotly_apply_global_theme(p, bg = "transparent", grid = "y")
      p
    })
    
    # --- camemberts par scénario (vue énergie) ------------------------------
    output$pie_energy_items <- plotly::renderPlotly({
      req(input$view_mode == "energy")
      validate(need(length(SCEN_SHOW) == 3, "SCENARIOS_BASE_CODES doit contenir exactement 3 scénarios pour ce module."))
      
      pd <- data_items(); req(nrow(pd) > 0)
      validate(need(sum(pd$value, na.rm = TRUE) > 0, "Aucune valeur non nulle à afficher."))
      ylab <- unique(pd$.ylab)[1]
      
      th <- get_plotly_tokens()
      
      pd <- pd %>% dplyr::mutate(Item = forcats::fct_relevel(Item, sort(unique(Item))))
      
      cols <- if (exists("item_colors_for"))
        item_colors_for(levels(pd$Item))
      else
        scales::hue_pal()(length(levels(pd$Item)))
      names(cols) <- levels(pd$Item)
      
      df_share <- pd %>%
        dplyr::group_by(Scenario) %>%
        dplyr::mutate(
          total = sum(value, na.rm = TRUE),
          share = dplyr::if_else(total > 0, value / total, NA_real_),
          label_pct = dplyr::if_else(
            !is.na(share) & total > 0,
            scales::percent(share, accuracy = 1),
            NA_character_
          ),
          text_pos = dplyr::if_else(share < 0.05, "outside", "inside")
        ) %>%
        dplyr::ungroup()
      
      x_domains <- list(
        c(0.00, 1/3),
        c(1/3, 2/3),
        c(2/3, 1.00)
      )
      
      p <- plotly::plot_ly()
      
      for(i in seq_along(SCEN_SHOW)){
        sc_code <- SCEN_SHOW[i]
        sc_lab  <- label_scen(sc_code)
        
        d  <- df_share %>% dplyr::filter(Scenario == sc_code, total > 0, value > 0)
        if (nrow(d) == 0) next
        
        p <- p %>%
          plotly::add_pie(
            data   = d,
            labels = ~Item,
            values = ~share,
            text   = ~label_pct,
            textinfo = "text",
            name   = sc_lab,
            domain = list(x = x_domains[[i]], y = c(0, 1)),
            sort   = FALSE,
            textposition = ~text_pos,
            textfont     = list(color = th$font_color, size = 12),
            insidetextorientation = "horizontal",
            marker       = list(colors = cols[d$Item]),
            customdata   = ~value,
            hovertemplate = paste0(
              "<b>", sc_lab, "</b><br>",
              "%{label}<br>",
              "Part : %{percent}<br>",
              "Valeur : %{customdata} ", ylab,
              "<extra></extra>"
            ),
            showlegend = (i == 1)
          )
      }
      
      p %>%
        plotly::layout(
          margin        = list(t = 45, b = 60, l = 70, r = 200),
          paper_bgcolor = APP_TRANSPARENT,
          plot_bgcolor  = APP_TRANSPARENT,
          font = list(color = th$font_color),
          hoverlabel = list(
            bgcolor = th$hover_bg,
            font    = list(color = th$hover_font)
          ),
          legend = list(
            orientation = "h",
            x = 0.5,
            xanchor = "center",
            y = 1.15,
            font = list(color = th$muted_color)
          )
        ) %>%
        plotly::config(displaylogo = FALSE)
    })
    
    # --- Graphique protéines (module R/16) ----------------------------------
    output$protein_plot <- renderUI({
      ns <- session$ns
      if (input$view_mode == "protein") {
        mod_protein_treemap_ui(ns("protein_treemap"))
      }
    })
    
    mod_protein_treemap_server(
      id        = "protein_treemap",
      fact      = fact,
      r_country = r_country
    )
    
    # --- export CSV commun (énergie + protéines) ---------------------------
    output$dl_plot <- downloadHandler(
      filename = function(){
        paste0("Nutrition_", gsub(" ", "_", r_country()), "_data.csv")
      },
      content = function(file){
        pd_energy <- data_items()
        ylab <- unique(pd_energy$.ylab)[1]
        energy_df <- pd_energy %>%
          dplyr::transmute(
            Pays = r_country(),
            Scenario = as.character(Scenario),         # codes
            Scenario_label = as.character(Scenario_ui),# labels (new config)
            Annee = year,
            Item, Valeur = value, Unite = ylab
          )
        
        prot_data <- try(mod_prot$protein_data(), silent = TRUE)
        if (inherits(prot_data, "try-error") || is.null(prot_data)) {
          df_final <- energy_df
        } else {
          df_prot <- prot_data() %>%
            dplyr::transmute(
              Pays = r_country(),
              Scenario = as.character(scenario),
              Scenario_label = if (exists("scenario_label", mode = "function", inherits = TRUE))
                vapply(as.character(scenario), scenario_label, FUN.VALUE = character(1)) else as.character(scenario),
              Annee = NA,
              Item, Valeur = share, Unite = "%"
            )
          df_final <- dplyr::bind_rows(energy_df, df_prot)
        }
        
        readr::write_delim(df_final, file, delim = ";")
      }
    )
    
    # --- Note de base ------------------------------------------------------
    output$note <- renderUI({
      htmltools::HTML("
  The stacked bars above represent the <strong>total daily energy supply per capita</strong> (kcal/hab/day)
  under each dietary scenario.<br>
  The charts below show either the energy shares of each product
  or their contribution to total protein or energy intake.
  </p>")
    })
  })
}
