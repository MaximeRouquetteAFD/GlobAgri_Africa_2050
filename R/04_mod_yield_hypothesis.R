# R/04_mod_yield_hypothesis.R
# -------------------------------------------------
# Rendements (Element == "Yield") : comparaison 2018 vs 2050

# R/04_mod_yield_hypothesis.R
# -------------------------------------------------
# Rendements (Element == "Yield") : comparaison 2018 vs 2050

mod_yield_ui <- function(id, wrap_in_card = TRUE){
  ns <- NS(id)
  
  content <- tagList(
    h3("Hypothèses de rendements (2018 vs 2050)"),
    p(class = "text-muted",
      "Rendement moyen par produit agricole : année de base (2018) vs ",
      "niveaux projetés en 2050 (valeurs indépendantes du scénario)."),
    plotly::plotlyOutput(ns("plot"), height = "auto"),
    div(class = "u-actions",
        shiny::downloadLink(ns("dl_csv"),
                            label = tagList(shiny::icon("download"), "CSV"))),
    uiOutput(ns("note"))
  )
  
  if (isTRUE(wrap_in_card)) {
    # ⬇️ ici : on utilise .card > .card-body au lieu de .u-card
    div(class = "card",
        div(class = "card-body", content))
  } else {
    content
  }
}


mod_yield_server <- function(id,
                             fact_reactive,  # reactive({ fact })
                             country_sel     # reactive du pays (Region)
) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    dat_plot <- reactive({
      req(fact_reactive(), country_sel())
      fact <- fact_reactive()
      
      # (Optionnel) si tu veux t'assurer qu'on reste Afrique :
      if ("iso3" %in% names(fact) && exists("ref") && !is.null(ref$dim_country)) {
        fact <- dplyr::semi_join(fact, ref$dim_country %>% dplyr::select(iso3), by = "iso3")
      }
      
      # 1) Filtrer Yield pour le pays et pour les 2 années
      df <- fact %>%
        dplyr::filter(.data$Element == "Yield",
                      .data$Region  == country_sel(),
                      .data$Year %in% c(2018, 2050)) %>%
        dplyr::group_by(Item, Year) %>%
        dplyr::summarise(Value = mean(Value, na.rm = TRUE), .groups = "drop") %>%
        # ---- CONVERSION ICI : de "1000 t/ha" vers "t/ha"
        dplyr::mutate(Value = Value * 1000) %>%
        dplyr::group_by(Item) %>%
        dplyr::filter(dplyr::n() == 2) %>%
        dplyr::ungroup()
      
      # Ordre des items = valeur 2050 décroissante (lecture plus claire)
      ord <- df %>% dplyr::filter(Year == 2050) %>%
        dplyr::arrange(dplyr::desc(Value)) %>% dplyr::pull(Item)
      df$Item <- factor(df$Item, levels = ord)
      
      df %>% dplyr::mutate(Year = as.character(Year))
    })
    
    # --- Téléchargement CSV (format large : Item, 2018, 2050, Region, Unite)
    output$dl_csv <- downloadHandler(
      filename = function() {
        ctry <- country_sel()
        paste0("yield_", gsub("\\s+", "_", ctry), "_2018_2050.csv")
      },
      content = function(file) {
        df_long <- dat_plot()  # colonnes: Item, Year, Value
        # On passe en format large pour plus de lisibilité
        df_wide <- tidyr::pivot_wider(
          df_long,
          names_from = Year, values_from = Value
        ) %>%
          dplyr::mutate(Region = country_sel(), Unite = "t/ha") %>%
          dplyr::select(Region, Item, `2018`, `2050`, Unite)
        
        utils::write.csv(df_wide, file, row.names = FALSE, fileEncoding = "UTF-8")
      },
      contentType = "text/csv"
    )
    
    output$plot <- plotly::renderPlotly({
      df <- dat_plot()
      validate(need(nrow(df) > 0, "Aucune donnée disponible (Yield 2018 et 2050)."))
      
      # ordre fixe + comptage
      df <- df %>% dplyr::mutate(Year = factor(as.character(Year), levels = c("2018","2050")))
      n_items <- dplyr::n_distinct(df$Item)
      h <- max(560, 38 * n_items + 220)
      
      # palette OBLIGATOIRE
      validate(need(exists("PALETTE_YEARS", inherits = TRUE), "Palette 'PALETTE_YEARS' non chargée."))
      pal  <- get("PALETTE_YEARS", inherits = TRUE)
      cols <- pal[c("2018","2050")]
      validate(need(all(!is.na(cols)), "La palette doit définir '2018' et '2050'."))
      
      d50 <- df %>% dplyr::filter(Year == "2050")
      d18 <- df %>% dplyr::filter(Year == "2018")
      
      # IMPORTANT: largeur réduite + offsetgroup distincts -> côte à côte sans chevauchement
      p <- plotly::plot_ly(height = h)
      
      p <- p %>% plotly::add_bars(
        data = d50, x = ~Value, y = ~Item, orientation = "h",
        name = "2050",
        marker = list(color = unname(cols["2050"])),
        offsetgroup = "2050", alignmentgroup = "year",
        width = 0.36,
        hovertemplate = "<b>%{y}</b><br>Année: 2050<br>Rendement: %{x:.4f} t/ha<extra></extra>"
      )
      
      # tracer 2018 après 2050 => 2018 apparaît AU-DESSUS
      p <- p %>% plotly::add_bars(
        data = d18, x = ~Value, y = ~Item, orientation = "h",
        name = "2018",
        marker = list(color = unname(cols["2018"])),
        offsetgroup = "2018", alignmentgroup = "year",
        width = 0.36,
        hovertemplate = "<b>%{y}</b><br>Année: 2018<br>Rendement: %{x:.4f} t/ha<extra></extra>"
      )
      
      p <- p %>% plotly::layout(
        barmode = "group",
        bargap  = 0.35,
        legend  = list(orientation = "v", traceorder = "reversed"),
        xaxis   = list(
          title    = "t/ha",
          zeroline = TRUE,
          showline = FALSE
        ),
        yaxis   = list(
          title      = "",
          automargin = TRUE,
          ticks      = "outside",
          ticklen    = 6,
          tickcolor  = "rgba(0,0,0,0)",
          tickfont   = list(size = 12) #taille de police des produits agricoles
        ),
        margin  = list(l = 200, r = 40, t = 40, b = 48)
      )
      p <- plotly_theme(p, bg = "transparent", grid = "none")
      p   
      
    })
  })
}

