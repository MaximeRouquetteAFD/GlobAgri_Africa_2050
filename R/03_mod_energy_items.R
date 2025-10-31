# R/03_mod_energy_items.R
# ------------------------------------------------------------------
# Graphique "FoodEnergy per capita — détail par Item" (stacked bar)
# ------------------------------------------------------------------

mod_energy_items_ui <- function(id){
  ns <- NS(id)
  uiOutput(ns("block"))
}

mod_energy_items_server <- function(
    id,
    fact,
    r_country,
    r_items,
    ENERGY_ELEMENT = "FoodEnergy per capita"
){
  moduleServer(id, function(input, output, session){
    
    output$block <- renderUI({
      sel <- r_items()
      if (is.null(sel) || length(sel) == 0) return(NULL)
      
      ns <- session$ns
      
      # ICI on wrap DIRECTEMENT le contenu dans .u-card
      div(
        class = "u-card u-card--flat u-card--hover",
        
        h3("FoodEnergy per capita (kcal/j/hab) — détail par Item"),
        
        plotly::plotlyOutput(ns("bar_energy_items"), height = "520px"),
        br(),
        
        p(tags$em(
          "Note : les barres représentent la somme des items sélectionnés pour chaque scénario. ",
          "Trois scénarios sont affichés : « Même diète », « Diète probable » et « Diète saine »."
        )),
        br(),
        
        div(
          class = "u-actions",
          shiny::downloadLink(
            ns("dl_plot"),
            label = tagList(shiny::icon("download"), "CSV")
          )
        )
      )
    })
    
    
    # -- Années par scénario
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
    }) %>%
      bindCache(r_country(), ENERGY_ELEMENT)
    
    # -- Données
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
      
      ylab <- if (nrow(df) == 0 || all(is.na(df$unit))) {
        "Valeur"
      } else {
        as.character(df$unit[which(!is.na(df$unit))[1]])
      }
      
      all_grid <- tidyr::expand_grid(
        Scenario = factor(levels(yrs$Scenario), levels = SCEN_SHOW),
        Item     = r_items()
      ) %>%
        dplyr::left_join(dplyr::select(yrs, Scenario, year_used), by = "Scenario")
      
      all_grid %>%
        dplyr::left_join(df, by = c("Scenario","Item")) %>%
        dplyr::mutate(
          value    = dplyr::coalesce(value, 0),
          unit     = dplyr::coalesce(unit, ylab),
          year     = dplyr::coalesce(year, year_used),
          Scenario = forcats::fct_relevel(Scenario, SCEN_SHOW),
          .ylab    = ylab
        ) %>%
        dplyr::arrange(Scenario, Item)
    }) %>%
      bindCache(r_country(), r_items(), ENERGY_ELEMENT)
    
    # -- Graphe
    output$bar_energy_items <- plotly::renderPlotly({
      pd <- data_items(); req(nrow(pd) > 0)
      validate(need(sum(pd$value, na.rm = TRUE) > 0, "Aucune valeur non nulle à afficher."))
      
      ylab <- unique(pd$.ylab)[1]
      
      pd <- pd %>%
        dplyr::mutate(Item = forcats::fct_relevel(Item, sort(unique(Item))))
      
      gg <- ggplot2::ggplot(pd, ggplot2::aes(Scenario, value, fill = Item)) +
        ggplot2::geom_col(width = 0.8, position = "stack") +
        ggplot2::scale_fill_manual(values = item_colors_for(levels(pd$Item)), drop = FALSE) +
        ggplot2::labs(x = NULL, y = ylab, fill = "Item") +
        ggplot2::scale_y_continuous(labels = scales::label_number(big.mark = " ")) +
        ggplot2::theme_minimal(base_size = 13) +
        ggplot2::theme(
          panel.grid.minor = ggplot2::element_blank(),
          legend.position  = "right"
        )
      
      plotly::ggplotly(gg, tooltip = c("x","fill","y"))
    })
    
    # -- Export CSV
    output$dl_plot <- downloadHandler(
      filename = function(){
        paste0("FoodEnergy_", gsub(" ", "_", r_country()), ".csv")
      },
      content = function(file){
        pd <- data_items(); req(nrow(pd) > 0)
        ylab <- unique(pd$.ylab)[1]
        readr::write_delim(
          pd %>%
            dplyr::transmute(
              Pays     = r_country(),
              Scenario = as.character(Scenario),
              Annee    = year,
              Item,
              Valeur   = value,
              Unite    = ylab
            ),
          file,
          delim = ";"
        )
      }
    )
  })
}
