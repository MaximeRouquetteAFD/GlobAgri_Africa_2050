# R/05_mod_area_stacked.R — version simple, fond transparent harmonisé
# ---------------------------------------------------------------

# R/05_mod_area_stacked.R

mod_area_stacked_ui <- function(
    id,
    height = "590px",
    wrap = c("card", "none")  # <- NOUVEAU : "card" (par défaut) ou "none"
){
  ns <- NS(id)
  wrap <- match.arg(wrap)
  
  content <- tagList(
    h3("Surfaces (Area) — décomposition empilée par scénario"),
    plotly::plotlyOutput(ns("stack_area"), height = height, width = "100%"),
    div(class = "text-right",
        div(class = "u-actions",
            downloadLink(ns("dl_area_csv"),
                         label = tagList(icon("download"), "CSV"))
        )
    )
  )
  
  if (wrap == "card") {
    div(class = "u-card u-card--flat u-card--hover", content)
  } else {
    content  # pas de carte -> plus de double encadré si parent en a déjà une
  }
}



mod_area_stacked_server <- function(
    id, fact, r_country,
    r_scenarios = shiny::reactive(NULL),
    r_area_item = shiny::reactive(NULL),
    ...
){
  moduleServer(id, function(input, output, session){
    
    SCEN_LEVELS  <- c("Année de base","Même diète","Diète probable","Diète saine")
    ITEMS_ORDER  <- c("Cropland",
                      "Land under perm. meadows and pastures",
                      "Forest land",
                      "Agricultural land occupation (Farm)")
    
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
        dplyr::summarise(year_used = suppressWarnings(max(Year[!is.na(Value)], na.rm = TRUE)),
                         .groups = "drop")
      
      dplyr::bind_rows(tibble::tibble(Scenario = "Année de base", year_used = base_year), others) %>%
        dplyr::filter(is.finite(year_used)) %>%
        dplyr::mutate(Scenario = factor(Scenario, levels = SCEN_LEVELS)) %>%
        dplyr::arrange(Scenario)
    }) %>% bindCache(r_country())
    
    data_area <- reactive({
      yrs <- years_by_scenario()
      shiny::validate(shiny::need(nrow(yrs) > 0, "Pas d'année disponible pour 'Area'."))
      
      fact %>%
        dplyr::inner_join(yrs, by = "Scenario") %>%
        dplyr::filter(
          Region == r_country(),
          stringr::str_to_lower(Element) == stringr::str_to_lower(AREA_ELEMENT),
          Year == year_used,
          Item %in% c("Forest land", AREA_CHILDREN)
        ) %>%
        dplyr::mutate(
          Scenario = factor(Scenario, levels = SCEN_LEVELS),
          Value    = dplyr::if_else(Item == "Forest land" & !is.na(Value) & Value < 0, 0, Value)
        ) %>%
        dplyr::group_by(Scenario, Item, year_used) %>%
        dplyr::summarise(value = sum(Value, na.rm = TRUE), .groups = "drop") %>%
        dplyr::mutate(
          value  = value * 1000,  # ha (si milliers d'ha)
          Item   = factor(as.character(Item), levels = ITEMS_ORDER),
          year   = year_used,
          scen_i = as.integer(Scenario),
          scen_t = as.character(Scenario)
        ) %>%
        dplyr::arrange(Scenario, Item)
    }) %>% bindCache(r_country())
    
    output$stack_area <- plotly::renderPlotly({
      da <- data_area(); req(nrow(da) > 0)
      
      base_total <- da %>%
        dplyr::filter(Scenario == "Année de base") %>%
        dplyr::summarise(tot = sum(value, na.rm = TRUE), .groups = "drop") %>%
        dplyr::pull(tot)
      req(length(base_total) == 1, is.finite(base_total))
      
      cols <- area_colors_for(levels(da$Item))
      
      da <- da %>%
        dplyr::group_by(Scenario) %>%
        dplyr::mutate(cum_prev = dplyr::lag(cumsum(value), default = 0)) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(
          covered = pmin(value, pmax(base_total - cum_prev, 0)),
          excess  = pmax(value - covered, 0)
        )
      
      scen_lvls_chr <- levels(da$Scenario)
      p <- plotly::plot_ly() %>%
        plotly::layout(
          barmode = "stack",
          xaxis = list(title = "", type = "linear",
                       tickmode = "array",
                       tickvals = seq_along(scen_lvls_chr),
                       ticktext = scen_lvls_chr),
          yaxis = list(title = "ha", separatethousands = TRUE),
          legend = list(title = list(text = ""))
        )
      
      for (it in levels(da$Item)) {
        sub <- da %>% dplyr::filter(Item == it)
        p <- plotly::add_bars(
          p, data = sub,
          x = ~scen_i, y = ~covered, name = it, legendgroup = it,
          marker = list(color = cols[match(it, levels(da$Item))]),
          text = ~scen_t, textposition = "none",
          hovertemplate = paste0("%{text}<br>", it, " : %{y:.0f} ha<extra></extra>")
        )
      }
      
      first_excess <- TRUE
      for (it in levels(da$Item)) {
        sub <- da %>% dplyr::filter(Item == it, excess > 0)
        if (nrow(sub) == 0) next
        p <- plotly::add_bars(
          p, data = sub,
          x = ~scen_i, y = ~excess,
          name = if (first_excess) "Besoin non-couvert" else "Besoin non-couvert",
          showlegend = first_excess, legendgroup = "excess",
          marker = list(
            color = cols[match(it, levels(da$Item))],
            line  = list(color = I(cols[match(it, levels(da$Item))]), width = 0.5),
            pattern = list(shape = "/", bgcolor = cols[match(it, levels(da$Item))],
                           fgcolor = "rgba(40,40,40,0.7)", solidity = 0.5)
          ),
          text = ~scen_t, textposition = "none",
          hovertemplate = paste0("%{text}<br>", it, " — besoin non-couvert : %{y:.0f} ha<extra></extra>")
        )
        first_excess <- FALSE
      }
      
      p <- plotly::add_trace(
        p, type = "scatter", mode = "lines",
        x = c(0.5, length(scen_lvls_chr) + 0.5),
        y = c(base_total, base_total),
        name = "surface exploitable maximum",
        legendgroup = "baseline",
        hoverinfo = "skip",
        line = list(dash = "dash", color = "gray", width = 1.2)
      )
      
      # fond transparent harmonisé (helper)
      plotly_theme_transparent(p)
    })
    
    # CSV
    output$dl_area_csv <- downloadHandler(
      filename = function(){ paste0("Area_", gsub(" ", "_", r_country()), ".csv") },
      content = function(file){
        da <- data_area(); req(nrow(da) > 0)
        out <- da %>%
          dplyr::transmute(
            Pays      = r_country(),
            Scenario  = as.character(Scenario),
            Annee     = year,
            Item      = as.character(Item),
            Valeur_ha = value
          ) %>%
          dplyr::arrange(Scenario, Annee, Item)
        readr::write_delim(out, file, delim = ";")
      }
    )
  })
}
