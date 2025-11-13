# R/16_mod_protein_treemap.R
# -------------------------------------------------------------
# Treemap double : part de l'apport protéique par produit (≥ 1%)
# - Pays = r_country() (colonne 'Region' dans fact)
# - Élément = "Protein consumption per capita"
# - Scénarios : "Même diète", "Diète probable"
# - Couleurs : item_colors_for() / ITEM_COLORS (via utils_palette)
# -------------------------------------------------------------

# R/16_mod_protein_treemap.R — UI alignée sur R/14_mod_livestock_stocks.R
mod_protein_treemap_ui <- function(id, height = "380px"){
  ns <- NS(id)
  tagList(
    div(class = "card",
        div(class = "card-body",
            h3(class = "card-title", "Part de l’apport protéique journalier par produit"),
            p(class = "text-muted",
              "Répartition en % par produit pour « Même diète » et « Diète probable ». ",
              "Seules les parts ≥ 1 % sont affichées."
            ),
            
            # légende au-dessus
            plotly::plotlyOutput(ns("legend_plot"), height = "48px"),
            
            # deux treemaps côte à côte + titres en dessous
            fluidRow(
              column(
                6,
                plotly::plotlyOutput(ns("plot_same"), height = height),
                div(class = "text-center u-subtitle mt-2", "Même diète")
              ),
              column(
                6,
                plotly::plotlyOutput(ns("plot_prob"), height = height),
                div(class = "text-center u-subtitle mt-2", "Diète probable")
              )
            ),
            
            # actions
            div(class = "u-actions",
                downloadLink(ns("dl_csv"),
                             label = tagList(shiny::icon("download"), "CSV")))
        )
    )
  )
}


mod_protein_treemap_server <- function(
    id,
    fact,
    r_country,
    PROT_ELEMENT = "Protein consumption per capita"
){
  moduleServer(id, function(input, output, session){
    
    SCEN <- c("Même diète", "Diète probable")
    
    # Année utilisée par scénario (comme tes autres modules)
    years_by_scenario <- reactive({
      fact %>%
        dplyr::filter(
          Region == r_country(),
          stringr::str_trim(Element) == PROT_ELEMENT,
          Scenario %in% SCEN
        ) %>%
        dplyr::group_by(Scenario) %>%
        dplyr::summarise(
          year_used = suppressWarnings(max(Year[!is.na(Value)], na.rm = TRUE)),
          .groups = "drop"
        )
    }) %>% bindCache(r_country(), PROT_ELEMENT)
    
    # Couleurs à partir de utils_palette (item_colors_for / ITEM_COLORS)
    get_item_colors <- function(items){
      items <- as.character(items)
      if (exists("item_colors_for", mode = "function", inherits = TRUE)) {
        cols <- item_colors_for(items)
      } else if (exists("ITEM_COLORS", inherits = TRUE)) {
        base <- get("ITEM_COLORS", inherits = TRUE)
        miss <- setdiff(items, names(base))
        if (length(miss)) base <- c(base, setNames(scales::hue_pal()(length(miss)), miss))
        cols <- base[items]
      } else {
        cols <- setNames(scales::hue_pal()(length(unique(items))), unique(items))
      }
      setNames(as.character(unname(cols)), items)
    }
    
    # Préparation d'un scénario
    prep <- function(scen, yrs){
      yr <- yrs$year_used[match(scen, yrs$Scenario)]
      validate(need(length(yr) == 1 && is.finite(yr), paste("Année indisponible pour", scen)))
      fact %>%
        dplyr::filter(
          Region == r_country(),
          stringr::str_trim(Element) == PROT_ELEMENT,
          Scenario == scen,
          Year == yr
        ) %>%
        dplyr::group_by(Item) %>%
        dplyr::summarise(value = sum(Value, na.rm = TRUE), .groups = "drop") %>%
        dplyr::filter(value > 0) %>%
        dplyr::mutate(total = sum(value),
                      share = 100 * value / total) %>%
        dplyr::filter(share >= 1) %>%                         # règle des 1 %
        dplyr::arrange(dplyr::desc(share)) %>%
        dplyr::mutate(scenario = scen)
    }
    
    # Données combinées (2 scénarios)
    data_both <- reactive({
      yrs <- years_by_scenario()
      validate(need(nrow(yrs) > 0, paste0("Aucune donnée pour « ", PROT_ELEMENT, " » dans ce pays.")))
      dplyr::bind_rows(
        prep("Même diète", yrs),
        prep("Diète probable", yrs)
      )
    }) %>% bindCache(r_country(), PROT_ELEMENT)
    
    # Palette nommée pour tous les items présents
    palette_map <- reactive({
      items <- sort(unique(data_both()$Item))
      get_item_colors(items)   # named vector: names = items
    })
    
    # Fabrique un treemap pour un sous-ensemble
    make_treemap <- function(df, cols_map){
      if (nrow(df) == 0) {
        return(
          plotly::plot_ly() %>%
            plotly::layout(
              annotations = list(text = "Aucune part ≥ 1 %", x = 0.5, y = 0.5, showarrow = FALSE),
              xaxis = list(visible = FALSE), yaxis = list(visible = FALSE),
              margin = list(l=0,r=0,t=0,b=0)
            ) %>%
            plotly_theme(bg = "transparent", grid = "none")
        )
      }
      plotly::plot_ly(
        type    = "treemap",
        labels  = df$Item,
        parents = rep("", nrow(df)),
        values  = df$share,
        text    = sprintf("%.2f%%", df$share),
        textinfo= "text",
        hovertemplate = "<b>%{label}</b><br>Part: %{value:.2f} %<extra></extra>",
        marker  = list(colors = unname(cols_map[df$Item]))
      ) %>%
        plotly::layout(margin = list(l=0,r=0,t=0,b=0)) %>%
        plotly_theme(bg = "transparent", grid = "none")
    }
    
    output$plot_same <- plotly::renderPlotly({
      df <- data_both() %>% dplyr::filter(scenario == "Même diète")
      make_treemap(df, palette_map()) %>%
        plotly_theme(bg = "transparent", grid = "none") %>%
        plotly::config(displayModeBar = FALSE)
    })
    
    output$plot_prob <- plotly::renderPlotly({
      df <- data_both() %>% dplyr::filter(scenario == "Diète probable")
      make_treemap(df, palette_map()) %>%
        plotly_theme(bg = "transparent", grid = "none") %>%
        plotly::config(displayModeBar = FALSE)
    })
    
    output$legend_plot <- plotly::renderPlotly({
      cols <- palette_map(); if (length(cols) == 0) return(NULL)
      items <- names(cols)
      
      p <- plotly::plot_ly()
      for (it in items){
        p <- plotly::add_trace(
          p, x = 2, y = 2, type = "scatter", mode = "markers",
          marker = list(size = 10, color = unname(cols[it]), symbol = "square"),
          name = it, legendgroup = it, showlegend = TRUE, hoverinfo = "skip",
          inherit = FALSE
        )
      }
      
      p %>%
        plotly::layout(
          showlegend = TRUE,
          legend = list(
            orientation = "h",
            x = 0.5, y = 1, xanchor = "center", yanchor = "top",
            itemclick = FALSE, itemdoubleclick = FALSE
          ),
          xaxis = list(visible = FALSE, range = c(0, 1), fixedrange = TRUE),
          yaxis = list(visible = FALSE, range = c(0, 1), fixedrange = TRUE),
          margin = list(l = 0, r = 0, t = 0, b = 0)
        ) %>%
        plotly_theme(bg = "transparent", grid = "none") %>%
        plotly::config(displayModeBar = FALSE)
    })
    

    # Export CSV combiné (2 scénarios)
    output$dl_csv <- shiny::downloadHandler(
      filename = function(){
        paste0("protein_shares_", gsub(" ", "_", r_country()), "_2scenarios.csv")
      },
      content = function(file){
        readr::write_csv(
          data_both() %>%
            dplyr::select(scenario, Item, share),
          file
        )
      }
    )
  })
}
