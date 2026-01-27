# R/10_mod_surface_cards.R
# ---------------------------------------------------------------
# Encadrés KPI par scénario (simple)
# - Affiche 3 cartes : Même diète / Diète probable / Diète saine
# - Valeurs = somme des AREA_CHILDREN (hors "Forest land"), en ha
# - Delta = % vs "Année de base"
# Dépendances : AREA_ELEMENT, AREA_CHILDREN
# ---------------------------------------------------------------

mod_surface_cards_ui <- function(id, height = "120px", show_title = TRUE){
  ns <- NS(id)
  tagList(
    div(id = ns("root"), class = "surface-cards",
        if (isTRUE(show_title)) h3("Surfaces agricoles (hors forêt) — KPI par scénario"),
        # Cartes KPI
        div(id = ns("cards_root"),
            style = sprintf("--surf-kpi-h:%s;", height),
            uiOutput(ns("cards"))
            
        )
    )
  )
}

mod_surface_cards_server <- function(
    id,
    fact,
    r_country,
    r_scenarios = shiny::reactive(NULL),   # rétro-compatibilité
    r_area_item = shiny::reactive(NULL),   # rétro-compatibilité
    ...
){
  moduleServer(id, function(input, output, session){
    
    allowed_show <- c("Même diète","Diète probable","Diète saine")
    SCEN_LEVELS  <- c("Année de base", allowed_show)
    
    # ---- Année retenue par scénario
    years_by_scenario <- reactive({
      dat <- fact %>%
        dplyr::filter(
          Region == r_country(),
          stringr::str_to_lower(Element) == stringr::str_to_lower(AREA_ELEMENT),
          Scenario %in% SCEN_LEVELS
        )
      
      base_year <- if (any(dat$Scenario == "Année de base" & dat$Year == 2018 & !is.na(dat$Value))) 2018
      else suppressWarnings(max(dat$Year[dat$Scenario == "Année de base" & !is.na(dat$Value)], na.rm = TRUE))
      
      others <- dat %>%
        dplyr::filter(Scenario != "Année de base") %>%
        dplyr::group_by(Scenario) %>%
        dplyr::summarise(year_used = suppressWarnings(max(Year[!is.na(Value)], na.rm = TRUE)), .groups = "drop")
      
      dplyr::bind_rows(tibble::tibble(Scenario = "Année de base", year_used = base_year), others) %>%
        dplyr::filter(is.finite(year_used))
    }) %>% bindCache(r_country())
    
    # ---- KPI (ha) + delta vs base
    kpi_surface <- reactive({
      yrs <- years_by_scenario(); req(nrow(yrs) > 0)
      
      base <- fact %>%
        dplyr::inner_join(yrs, by = "Scenario") %>%
        dplyr::filter(
          Region == r_country(),
          stringr::str_to_lower(Element) == stringr::str_to_lower(AREA_ELEMENT),
          Year == year_used,
          Item %in% AREA_CHILDREN | Item == "Forest land"
        ) %>%
        dplyr::mutate(Value = dplyr::if_else(Item == "Forest land" & !is.na(Value) & Value < 0, 0, Value))
      
      agg <- base %>%
        dplyr::filter(Item %in% AREA_CHILDREN) %>%      # **hors forêt**
        dplyr::group_by(Scenario) %>%
        dplyr::summarise(value_ha = sum(Value, na.rm = TRUE) * 1000, .groups = "drop") %>%
        dplyr::mutate(value_ha = pmax(value_ha, 0))
      
      b <- agg %>% dplyr::filter(Scenario == "Année de base") %>% dplyr::pull(value_ha)
      b <- if (length(b)) b[1] else NA_real_
      
      agg %>%
        dplyr::filter(Scenario %in% c("Même diète","Diète probable","Diète saine")) %>%
        dplyr::mutate(
          diff_pct = if (is.finite(b) && b > 0) 100 * (value_ha - b) / b else NA_real_,
          Scenario = factor(Scenario, levels = c("Même diète","Diète probable","Diète saine"))
        ) %>%
        dplyr::arrange(Scenario)
    }) %>% bindCache(r_country())
    
    
    # ---- Rendu cartes
    output$cards <- renderUI({
      dat <- kpi_surface()
      if (is.null(dat) || nrow(dat) == 0) return(NULL)
      
      fmt_num <- function(x){
        ifelse(is.finite(x), format(round(x), big.mark = " ", scientific = FALSE, trim = TRUE), "—")
      }
      fmt_pct <- function(p){
        if (!is.finite(p)) return("—")
        paste0(ifelse(p >= 0, "+", ""), formatC(p, digits = 1, format = "f"), "%")
      }
      
      cards <- lapply(seq_len(nrow(dat)), function(i){
        sc  <- as.character(dat$Scenario[i])
        val <- dat$value_ha[i]
        dlt <- dat$diff_pct[i]
        
        delta_tag <- if (!is.finite(dlt)) "—" else
          if (dlt > 0) span(class = "up",   fmt_pct(dlt)) else
            if (dlt < 0) span(class = "down", fmt_pct(dlt)) else "0%"
        
        # >>> utilise les classes du design system (pas de style inline)
        div(class = "u-card u-card--flat u-card--hover",
            div(class = "u-box",
                p(class = "u-title", sc),
                p(class = "u-value", fmt_num(val), span(class = "u-unit", "ha")),
                p(class = "u-sub", "Vs Année de base : ", delta_tag)
            )
        )
      })
      
      # conteneur flex standard de tes cartes
      div(class = "u-row", do.call(tagList, cards))
    })
    
  })
}
