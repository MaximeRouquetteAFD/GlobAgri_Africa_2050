# R/12_mod_energy_kpi_cards.R
# -------------------------------------------------
# 3 cartes KPI énergie (Kcal/j/hab) en 2050 par scénario, % vs 2018
# Cartes cliquables (toggle) + hook clavier; style piloté par app.css

mod_energy_kpis_ui <- function(id) {
  ns <- NS(id)
  tagList(
    # conteneur racine (sert au JS délégué)
    div(id = ns("root"),
        # rangée de 3 cartes (utilise u-row / u-card)
        uiOutput(ns("cards_row"))
    ),
    
    # JS délégué : clic + clavier → input
    tags$script(HTML(sprintf("
      // Clic souris
      $(document).on('click', '#%s .energy-kpi', function(){
        var scen = $(this).data('scenario') || null;
        Shiny.setInputValue('%s', scen, {priority:'event'});
      });
      // Espace/Entrée
      $(document).on('keydown', '#%s .energy-kpi', function(e){
        if (e.key === 'Enter' || e.key === ' ') { e.preventDefault(); $(this).click(); }
      });
    ", ns("root"), ns("energy_kpi_clicked"), ns("root"))))
  )
}

mod_energy_kpis_server <- function(id,
                                   fact,          # data.frame/tibble 'fact'
                                   r_country,     # reactive Region
                                   ENERGY_ELEMENT # ex: "Food energy supply (kcal/cap/day)"
){
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Helpers formatage (fallback si non définis globalement)
    fmt0 <- if (exists("fmt0", inherits = TRUE)) get("fmt0", inherits = TRUE) else
      function(x) if (is.na(x)) "—" else format(round(x), big.mark = " ", scientific = FALSE, trim = TRUE)
    fmt1 <- if (exists("fmt1", inherits = TRUE)) get("fmt1", inherits = TRUE) else
      function(x) if (is.na(x)) "—" else formatC(x, format = "f", digits = 1, big.mark = " ")
    
    wanted <- c("Même diète", "Diète probable", "Diète saine")
    
    # ---- Données agrégées
    totals <- reactive({
      req(r_country())
      df <- fact %>%
        dplyr::filter(.data$Region == r_country(),
                      .data$Element == ENERGY_ELEMENT)
      
      base2018 <- df %>%
        dplyr::filter(.data$Year == 2018) %>%
        dplyr::summarise(val = sum(.data$Value, na.rm = TRUE)) %>%
        dplyr::pull(val)
      if (length(base2018) == 0) base2018 <- NA_real_
      
      by_scen_2050 <- df %>%
        dplyr::filter(.data$Year == 2050, .data$Scenario %in% wanted) %>%
        dplyr::group_by(.data$Scenario) %>%
        dplyr::summarise(val = sum(.data$Value, na.rm = TRUE), .groups = "drop")
      
      tibble::tibble(Scenario = factor(wanted, levels = wanted)) %>%
        dplyr::left_join(by_scen_2050, by = "Scenario") %>%
        dplyr::mutate(
          base2018  = base2018,
          delta_pct = dplyr::if_else(is.finite(base2018) & base2018 > 0,
                                     100 * (val - base2018) / base2018,
                                     as.numeric(NA))
        )
    }) %>% bindCache(r_country())
    
    # ---- Sélection (toggle) exposée aux autres modules
    selected <- reactiveVal(NULL)
    observeEvent(input$energy_kpi_clicked, {
      scen <- input$energy_kpi_clicked
      cur  <- selected()
      if (!is.null(cur) && identical(cur, scen)) selected(NULL) else selected(scen)
    })
    
    # ---- Générateur de carte (u-card + hooks)
    make_card <- function(df_row, scen_label, scen_key, is_active){
      # Pas de lignes dispo
      if (nrow(df_row) == 0) {
        return(
          div(
            class = "u-card u-card--flat u-card--hover energy-kpi",
            tabindex = "0", role = "button", `aria-pressed` = "false",
            `data-scenario` = scen_key,
            div(class = "u-box",
                p(class = "u-title", scen_label),
                p(class = "u-value", "—", span(class = "u-unit", "Kcal")),
                p(class = "u-sub", "Données indisponibles")
            )
          )
        )
      }
      
      val2050 <- suppressWarnings(as.numeric(df_row$val[1]))
      base    <- suppressWarnings(as.numeric(df_row$base2018[1]))
      dp      <- suppressWarnings(as.numeric(df_row$delta_pct[1]))
      
      # Cas spécial "Même diète" sans 2050 → afficher 2018
      if (identical(scen_key, "Même diète") && (!is.finite(val2050) || is.na(val2050))) {
        return(
          div(
            class = paste("u-card u-card--flat u-card--hover energy-kpi", if (is_active) "is-active"),
            tabindex = "0", role = "button",
            `aria-pressed` = if (is_active) "true" else "false",
            `data-scenario` = scen_key,
            div(class = "u-box",
                p(class = "u-title", scen_label),
                p(class = "u-value", fmt0(base), span(class = "u-unit", "Kcal")),
                p(class = "u-sub", HTML("Année de base : <b>2018</b>"))
            )
          )
        )
      }
      
      delta_tag <- if (!is.finite(dp) || is.na(dp)) "Vs Année de base : —" else
        tagList("Vs Année de base : ",
                if (dp > 0) span(class="up",  paste0("+", fmt1(dp), "%"))
                else if (dp < 0) span(class="down", paste0(fmt1(dp), "%"))
                else "0%")
      
      div(
        class = paste(
          "u-card u-card--flat u-card--hover u-card--clickable u-card--focus energy-kpi",
          if (is_active) "is-active"
        ),
        tabindex = "0", role = "button",
        `aria-pressed` = if (is_active) "true" else "false",
        `data-scenario` = scen_key,
        div(class = "u-box",
            p(class = "u-title", scen_label),
            p(class = "u-value", fmt0(val2050), span(class = "u-unit", "Kcal")),
            p(class = "u-sub", delta_tag)
        )
      )
    }
    
    # ---- Rendu des 3 cartes
    output$cards_row <- renderUI({
      t <- totals()
      if (!nrow(t)) return(NULL)
      sel <- selected()
      
      # ordre voulu
      t$Scenario <- factor(t$Scenario, levels = wanted)
      t <- dplyr::arrange(t, .data$Scenario)
      
      cards <- list(
        make_card(t[t$Scenario == "Même diète",    , drop = FALSE], "Même diète",    "Même diète",    isTRUE(!is.null(sel) && sel == "Même diète")),
        make_card(t[t$Scenario == "Diète probable", , drop = FALSE], "Diète probable","Diète probable",isTRUE(!is.null(sel) && sel == "Diète probable")),
        make_card(t[t$Scenario == "Diète saine",    , drop = FALSE], "Diète saine",   "Diète saine",   isTRUE(!is.null(sel) && sel == "Diète saine"))
      )
      
      # rangée fluide (même logique que tes autres modules)
      div(class = "u-row", !!!cards)
    })
    
    # ---- Expose le scénario sélectionné
    return(list(
      selected_scenario = reactive(selected())
    ))
  })
}

