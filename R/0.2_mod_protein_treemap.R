# R/16_mod_protein_treemap.R
# -------------------------------------------------------------
# Share of protein intake by product (≥ 1 %)
# - Deux camemberts (Même diète / Diète probable)
# - Couleurs : item_colors_for() / ITEM_COLORS
# - Pas de bouton CSV (géré dans R/03)
# -------------------------------------------------------------

mod_protein_treemap_ui <- function(id, height = "370px"){
  ns <- NS(id)
  tagList(
    plotly::plotlyOutput(ns("pie_plot"), height = height)
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
    
    # ---- Année par scénario ------------------------------------------------
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
    
    # ---- Palette couleurs --------------------------------------------------
    get_item_colors <- function(items){
      items <- as.character(items)
      if (exists("item_colors_for", mode = "function", inherits = TRUE)) {
        cols <- item_colors_for(items)
      } else if (exists("ITEM_COLORS", inherits = TRUE)) {
        base <- get("ITEM_COLORS", inherits = TRUE)
        miss <- setdiff(items, names(base))
        if (length(miss)) {
          base <- c(base, stats::setNames(scales::hue_pal()(length(miss)), miss))
        }
        cols <- base[items]
      } else {
        cols <- stats::setNames(
          scales::hue_pal()(length(items)),
          items
        )
      }
      cols
    }
    
    # ---- Données par scénario ----------------------------------------------
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
        dplyr::mutate(
          total = sum(value),
          share = 100 * value / total
        ) %>%
        dplyr::filter(share >= 1) %>%
        dplyr::arrange(dplyr::desc(share)) %>%
        dplyr::mutate(scenario = scen)
    }
    
    # ---- Données combinées -------------------------------------------------
    data_both <- reactive({
      yrs <- years_by_scenario()
      validate(need(nrow(yrs) > 0, paste0("Aucune donnée pour « ", PROT_ELEMENT, " » dans ce pays.")))
      dplyr::bind_rows(
        prep("Même diète", yrs),
        prep("Diète probable", yrs)
      )
    }) %>% bindCache(r_country(), PROT_ELEMENT)
    
    # ---- Graphique en camemberts -------------------------------------------
    output$pie_plot <- plotly::renderPlotly({
      df <- data_both(); req(nrow(df) > 0)
      
      # Palette
      items_order <- sort(unique(df$Item))
      cols <- get_item_colors(items_order)
      names(cols) <- items_order
      
      # Prepa labels / position (comme les camemberts énergie de R/03)
      df <- df %>%
        dplyr::mutate(
          Scenario   = factor(scenario, levels = SCEN),
          share_prop = share / 100,
          label_pct  = scales::percent(share_prop, accuracy = 1),
          text_pos   = dplyr::if_else(share_prop < 0.05, "outside", "inside")
        )
      
      # Domaines (positions horizontales)
      x_domains <- list(c(0.00, 0.48), c(0.52, 1.00))
      titles    <- SCEN
      
      p <- plotly::plot_ly()
      
      for (i in seq_along(SCEN)) {
        scen <- SCEN[i]
        dat  <- df[df$Scenario == scen, ]
        if (nrow(dat) == 0) next
        
        p <- p %>%
          plotly::add_pie(
            data   = dat,
            labels = ~Item,
            values = ~share,
            text   = ~label_pct,          # "15%" etc.
            textinfo = "text",
            textposition = ~text_pos,     # inside / outside selon la part
            textfont     = list(color = "black", size = 12),
            insidetextorientation = "horizontal",
            marker = list(colors = cols[dat$Item]),
            name   = scen,
            domain = list(x = x_domains[[i]], y = c(0, 1)),
            sort   = FALSE,
            hovertemplate = paste0(
              "<b>", scen, "</b><br>",
              "%{label}<br>",
              "Part : %{percent}<extra></extra>"
            ),
            showlegend = FALSE            
            )
      }
      
      p %>%
        plotly::layout(
          annotations = list(
            list(
              x = 0.24,              # centre du 1er camembert
              y = -0.23,             # plus bas sous le graphique
              text = titles[1],
              showarrow = FALSE,
              font = list(size = 14)
            ),
            list(
              x = 0.76,              # centre du 2e camembert
              y = -0.23,             # même hauteur
              text = titles[2],
              showarrow = FALSE,
              font = list(size = 14)
            )
          ),
          showlegend    = FALSE,
          margin        = list(t = 40, b = 100, l = 30, r = 30),  # + de place en bas
          paper_bgcolor = "rgba(0,0,0,0)",
          plot_bgcolor  = "rgba(0,0,0,0)"
        ) %>%
        plotly::config(displaylogo = FALSE)
      
    })
    
    # Si tu veux récupérer ces données dans R/03 pour le CSV :
    return(list(
      protein_data = reactive(data_both())
    ))
  })
}

