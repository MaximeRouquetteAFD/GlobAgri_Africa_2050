# R/12_mod_harvested_stacked.R
# Empilé "Area harvested" par culture

# R/12_mod_harvested_stacked.R
mod_harvested_stacked_ui <- function(id, height = "520px", full_width = TRUE){
  ns <- NS(id)
  div(
    class = if (isTRUE(full_width)) "card full-bleed" else "card",
    div(
      class = "card-body",
      h3("Surfaces récoltées (Area harvested) — décomposition empilée par scénario"),
      plotly::plotlyOutput(ns("stack_harv"), height = height, width = "100%"),
      # --- barre d'actions (stylée par ton CSS .u-actions)
      div(class = "text-right",
          div(class = "u-actions",
              downloadLink(ns("dl_harvested_csv"),
                           label = tagList(icon("download"), "CSV"))
          )
      )
    )
  )
}


mod_harvested_stacked_server <- function(
    id, fact, r_country,
    harvest_element = "Area harvested",
    exclude_items = c("All products","All crops","Agricultural land occupation (Farm)",
                      "Cropland","Forest land","Land under perm. meadows and pastures"),
    value_multiplier = 1,
    baseline_label = "Cropland of the base year"   # <— nouveau
){
  
  moduleServer(id, function(input, output, session){
    
    # mêmes libellés FR que ton module d’exemple + support optionnel Prob-S-limitée
    SCEN_LEVELS_ALL <- c("Année de base", "Même diète", "Diète probable", "Diète saine")
    
    # petite palette cultures (utilise pal_crops() si dispo)
    harvest_colors_for <- function(items){
      if (exists("pal_crops", mode = "function")) {
        cols <- pal_crops(items); cols[match(items, names(cols), nomatch = 0)] # aligne
        unname(cols)
      } else {
        scales::hue_pal()(length(items))
      }
    }
    
    years_by_scenario <- reactive({
      dat <- fact %>%
        dplyr::filter(
          Region == r_country(),
          stringr::str_to_lower(Element) == stringr::str_to_lower(harvest_element),
          Scenario %in% SCEN_LEVELS_ALL
        )
      
      # Base = 2018 si dispo, sinon dernier millésime non-NA
      base_year <- if (any(dat$Scenario == "Année de base" & dat$Year == 2018 & !is.na(dat$Value))) 2018
      else suppressWarnings(max(dat$Year[dat$Scenario == "Année de base" & !is.na(dat$Value)], na.rm = TRUE))
      
      others <- dat %>%
        dplyr::filter(Scenario != "Année de base") %>%
        dplyr::group_by(Scenario) %>%
        dplyr::summarise(year_used = suppressWarnings(max(Year[!is.na(Value)], na.rm = TRUE)),
                         .groups = "drop")
      
      dplyr::bind_rows(tibble::tibble(Scenario = "Année de base", year_used = base_year), others) %>%
        dplyr::filter(is.finite(year_used)) %>%
        dplyr::mutate(Scenario = factor(Scenario, levels = SCEN_LEVELS_ALL)) %>%
        dplyr::arrange(Scenario)
    }) %>% bindCache(r_country())
    
    data_harvested <- reactive({
      yrs <- years_by_scenario()
      shiny::validate(shiny::need(nrow(yrs) > 0, "Pas d'année disponible pour 'Area harvested'."))
      
      # on prend TOUTES les cultures de l'élément 'Area harvested' (moins exclusions)
      dat <- fact %>%
        dplyr::inner_join(yrs, by = "Scenario") %>%
        dplyr::filter(
          Region == r_country(),
          stringr::str_to_lower(Element) == stringr::str_to_lower(harvest_element),
          Year == year_used,
          !Item %in% exclude_items
        ) %>%
        dplyr::mutate(
          Scenario = factor(Scenario, levels = SCEN_LEVELS_ALL)
        ) %>%
        dplyr::group_by(Scenario, Item, year_used) %>%
        dplyr::summarise(value = sum(Value, na.rm = TRUE), .groups = "drop") %>%
        dplyr::mutate(
          value  = value * value_multiplier,  # ha (si Value = milliers d'ha)
          year   = year_used,
          scen_i = as.integer(Scenario),
          scen_t = as.character(Scenario)
        )
      
      # Ordre stable des items : décroissant sur la base (ça copie ton style “ITEMS_ORDER”)
      base_order <- dat %>%
        dplyr::filter(Scenario == "Année de base") %>%
        dplyr::group_by(Item) %>%
        dplyr::summarise(tot = sum(value, na.rm = TRUE), .groups = "drop") %>%
        dplyr::arrange(dplyr::desc(tot)) %>%
        dplyr::pull(Item)
      
      items_levels <- unique(c(base_order, setdiff(unique(dat$Item), base_order)))
      dat %>% dplyr::mutate(Item = factor(as.character(Item), levels = items_levels)) %>%
        dplyr::arrange(Scenario, Item)
    }) %>% bindCache(r_country())
    
    output$stack_harv <- plotly::renderPlotly({
      da <- data_harvested(); req(nrow(da) > 0)
      
      # total de référence (2018) pour tracer la ligne pointillée
      base_total <- da %>%
        dplyr::filter(Scenario == "Année de base") %>%
        dplyr::summarise(tot = sum(value, na.rm = TRUE), .groups = "drop") %>%
        dplyr::pull(tot)
      req(length(base_total) == 1, is.finite(base_total))
      
      # couleurs (non nommées pour éviter le warning jsonlite)
      cols <- harvest_colors_for(levels(da$Item))
      
      scen_lvls_chr <- levels(da$Scenario)
      p <- plotly::plot_ly() %>%
        plotly::layout(
          barmode = "stack",
          xaxis = list(
            title   = "",
            type    = "linear",
            tickmode = "array",
            tickvals = seq_along(scen_lvls_chr),
            ticktext = scen_lvls_chr
          ),
          yaxis = list(title = "ha", separatethousands = TRUE),
          legend = list(title = list(text = ""))
        )
      
      # --- barres empilées "classiques" : on utilise directement 'value'
      for (it in levels(da$Item)) {
        sub <- da %>% dplyr::filter(Item == it)
        p <- plotly::add_bars(
          p, data = sub,
          x = ~scen_i, y = ~value, name = it, legendgroup = it,
          marker = list(color = unname(cols[match(it, levels(da$Item))])),
          text = ~scen_t, textposition = "none",
          hovertemplate = paste0("%{text}<br>", it, " : %{y:.0f} ha<extra></extra>")
        )
      }
      
      # --- ligne de référence (affichée dans la légende)
      p <- plotly::add_trace(
        p, type = "scatter", mode = "lines",
        x = c(0.5, length(scen_lvls_chr) + 0.5),
        y = c(base_total, base_total),
        name = baseline_label,
        legendgroup = "baseline",
        showlegend = TRUE,
        hoverinfo = "skip",
        line = list(dash = "dash", color = "gray", width = 1.2)
      )
      
      # 🎯 Thème harmonisé : fond transparent + grille Y (fine)
      p <- plotly_theme(p, bg = "transparent", grid = "y")
      p
    })
    
    
    # export CSV (même esprit)
    output$dl_harvested_csv <- downloadHandler(
      filename = function(){ paste0("AreaHarvested_", gsub(" ", "_", r_country()), ".csv") },
      content = function(file){
        da <- data_harvested(); req(nrow(da) > 0)
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


