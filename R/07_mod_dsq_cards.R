# R/07_mod_dsq_cards.R — version alignée sur app.css sans nouveau CSS

mod_dsq_cards_ui <- function(id){
  ns <- NS(id)
  tagList(
    h3(class = "section-title", "Offre intérieure totale (Gcal)"),
    # conteneur racine (sert au JS)
    div(id = ns("root"), uiOutput(ns("cards_row"))),
    
    # Clic + clavier -> input
    tags$script(HTML(sprintf("
      $(document).on('click', '#%s .dsq-card', function(){
        var scen = $(this).data('scenario') || null;
        Shiny.setInputValue('%s', scen, {priority:'event'});
      });
      $(document).on('keydown', '#%s .dsq-card', function(e){
        if (e.key === 'Enter' || e.key === ' ') { e.preventDefault(); $(this).click(); }
      });
    ", ns("root"), ns("dsq_card_clicked"), ns("root"))))
  )
}

mod_dsq_cards_server <- function(
    id, fact, r_country, unit_label = "Gcal"
){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    fmt_int <- function(x){
      ifelse(is.finite(x),
             format(round(x), big.mark = " ", scientific = FALSE, trim = TRUE),
             "—")
    }
    fmt_pct_pm <- function(x){
      ifelse(!is.finite(x), "—",
             ifelse(x >= 0, paste0("+", scales::percent(x, 0.1)),
                    scales::percent(x, 0.1)))
    }
    fmt_vs_base <- function(x) paste("Vs Année de base :", fmt_pct_pm(x))
    
    scen_order <- if (exists("SCEN_SHOW", inherits = TRUE)) SCEN_SHOW
    else c("Même diète", "Diète probable", "Diète saine")
    
    cards_df <- reactive({
      reg <- r_country(); req(nzchar(reg))
      base18 <- fact %>%
        dplyr::filter(Region==reg, Scenario=="Année de base",
                      Element=="Energy Domestic supply quantity", Year==2018) %>%
        dplyr::summarise(v=sum(Value,na.rm=TRUE)) %>% dplyr::pull(v)
      base18 <- if (length(base18)==0) NA_real_ else as.numeric(base18)
      
      last_year <- fact %>%
        dplyr::filter(Region==reg, Element=="Energy Domestic supply quantity",
                      Scenario %in% scen_order) %>%
        dplyr::group_by(Scenario) %>%
        dplyr::summarise(Year=suppressWarnings(max(Year[!is.na(Value)],na.rm=TRUE)),
                         .groups="drop")
      
      vals <- fact %>%
        dplyr::filter(Region==reg, Element=="Energy Domestic supply quantity",
                      Scenario %in% scen_order) %>%
        dplyr::inner_join(last_year, by=c("Scenario","Year")) %>%
        dplyr::group_by(Scenario) %>%
        dplyr::summarise(value=sum(Value,na.rm=TRUE), .groups="drop") %>%
        dplyr::mutate(delta = if (is.finite(base18) && base18>0) (value-base18)/base18 else NA_real_)
      
      vals$Scenario <- factor(vals$Scenario, levels=scen_order)
      dplyr::arrange(vals, Scenario)
    }) %>% bindCache(r_country())
    
    selected <- reactiveVal(NULL)
    observeEvent(input$dsq_card_clicked, {
      scen <- input$dsq_card_clicked
      cur  <- selected()
      if (!is.null(cur) && identical(cur, scen)) selected(NULL) else selected(scen)
    })
    
    # Pré-sélection au chargement : "Même diète" si dispo, sinon 1er scénario dispo
    observe({
      df <- cards_df()
      if (is.null(selected()) && nrow(df)) {
        # on compare sur la version caractère (évite les soucis de facteurs)
        scen_char <- as.character(df$Scenario)
        pref <- "Même diète"
        if (!(pref %in% scen_char)) pref <- scen_char[1]
        selected(pref)
      }
    })
    
    output$cards_row <- renderUI({
      df <- cards_df()
      if (!nrow(df)) return(div(class="text-muted", "Aucune donnée disponible."))
      
      sel <- selected()
      
      # NOTE: on utilise 'u-row' et les classes KPI existantes.
      cards <- lapply(seq_len(nrow(df)), function(i){
        scen <- as.character(df$Scenario[i])
        val  <- df$value[i]
        dlt  <- df$delta[i]
        is_active <- !is.null(sel) && identical(sel, scen)
        
        delta_node <- if (!is.finite(dlt)) {
          "Vs Année de base : —"
        } else {
          tagList(
            "Vs Année de base : ",
            span(
              class = if (dlt > 0) "up" else if (dlt < 0) "down" else NULL,
              strong(paste0(ifelse(dlt >= 0, "+", ""), scales::percent(dlt, accuracy = 0.1)))
            )
          )
        }
        p(class = "u-sub", delta_node)
        
        div(
          class = paste(
            "u-card u-card--flat u-card--hover u-card--clickable u-card--focus",
            "dsq-card energy-kpi", if (is_active) "is-active"
          ),
          tabindex = "0", role = "button",
          `aria-pressed` = if (is_active) "true" else "false",
          `data-scenario` = scen,
          div(class = "u-box",
              p(class = "u-title", scen),
              p(class = "u-value", fmt_int(val), span(class="u-unit", unit_label)),
              p(class = "u-sub", delta_node)
          )
        )
      })
      
      
      div(class = "u-row", !!!cards)
    })
    
    return(list(selected_scenario = reactive(selected())))
  })
}


