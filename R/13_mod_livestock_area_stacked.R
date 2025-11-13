# R/13_mod_livestock_area_stacked.R
# ---------------------------------------------------------------
# Surfaces d’élevage (Element == "Area") — empilé par scénario
# Items tracés : Dairy / Beef cattle / Meat sheep and goats
# Style identique aux graphes "Crops" (barres empilées + ligne pointillée en légende)

mod_livestock_area_stacked_ui <- function(id, height = "420px", full_width = TRUE){
  ns <- NS(id)
  
  div(
    class = if (isTRUE(full_width)) "card full-bleed" else "card",
    div(class = "card-body",
        h3(class = "card-title", "Surfaces d’élevage (Area) — décomposition empilée par scénario"),
        plotly::plotlyOutput(ns("stack_ls"), height = height, width = "100%"),
        div(class = "text-right",
            div(class = "u-actions",
                downloadLink(ns("dl_livestock_csv"),
                             label = tagList(icon("download"), "CSV"))
            )
        )
    )
  )
}



mod_livestock_area_stacked_server <- function(
    id, fact, r_country,
    element_name     = "Area",
    items_keep       = c("Dairy","Beef cattle","Meat sheep and goats"),
    value_multiplier = NULL,                       # NULL => auto-détection via Unit ("1000 ha" -> *1000)
    baseline_label   = "Livestock area of the base year"
){
  moduleServer(id, function(input, output, session){
    
    # ---- Niveaux de scénario (FR) + helpers robustes ----
    SCEN_LEVELS_ALL <- c("Année de base", "Même diète", "Diète probable", "Diète saine")
    
    # Nettoyage fin des libellés (NBSP, unicodes, espaces multiples)
    clean_scen <- function(x){
      x <- as.character(x)
      x <- stringi::stri_trans_general(x, "NFKC")          # normalise Unicode
      x <- gsub("\u00A0|\u202F", " ", x, perl = TRUE)      # NBSP / fine NBSP -> espace
      stringr::str_squish(x)                               # trim + compresse
    }
    
    # Palette : pal_livestock() si dispo, sinon pal_crops(), sinon hue
    livestock_colors_for <- function(items){
      if (exists("pal_livestock", mode = "function")) {
        unname(pal_livestock(items))
      } else if (exists("pal_crops", mode = "function")) {
        unname(pal_crops(items))
      } else {
        scales::hue_pal()(length(items))
      }
    }
    
    # ---- Année retenue par scénario (comme tes autres modules) ----
    years_by_scenario <- reactive({
      req(r_country(), nzchar(r_country()[1]))
      
      dat <- fact %>%
        dplyr::filter(
          .data$Region %in% r_country(),
          stringr::str_to_lower(.data$Element) == stringr::str_to_lower(element_name)
        ) %>%
        dplyr::mutate(Scenario = clean_scen(.data$Scenario)) %>%
        dplyr::filter(.data$Scenario %in% SCEN_LEVELS_ALL)
      
      # Base = 2018 si dispo, sinon dernier millésime non-NA
      base_year <- if (any(dat$Scenario == "Année de base" & dat$Year == 2018 & !is.na(dat$Value))) 2018
      else suppressWarnings(max(dat$Year[dat$Scenario == "Année de base" & !is.na(dat$Value)], na.rm = TRUE))
      
      others <- dat %>%
        dplyr::filter(.data$Scenario != "Année de base") %>%
        dplyr::group_by(Scenario) %>%
        dplyr::summarise(year_used = suppressWarnings(max(Year[!is.na(Value)], na.rm = TRUE)), .groups = "drop")
      
      dplyr::bind_rows(tibble::tibble(Scenario = "Année de base", year_used = base_year), others) %>%
        dplyr::filter(is.finite(.data$year_used)) %>%
        dplyr::mutate(Scenario = factor(.data$Scenario, levels = SCEN_LEVELS_ALL)) %>%
        dplyr::arrange(.data$Scenario)
    }) %>% bindCache(r_country())
    
    # ---- Données agrégées Dairy/Beef/Sheep&Goats ----
    data_ls <- reactive({
      yrs <- years_by_scenario()
      shiny::validate(shiny::need(nrow(yrs) > 0, "Pas d'année disponible pour 'Area' (élevage)."))
      req(r_country())
      
      dat0 <- fact %>%
        dplyr::inner_join(yrs, by = "Scenario") %>%
        dplyr::filter(
          .data$Region %in% r_country(),
          stringr::str_to_lower(.data$Element) == stringr::str_to_lower(element_name),
          .data$Year == .data$year_used
        ) %>%
        dplyr::mutate(
          Item     = stringr::str_trim(.data$Item),
          Scenario = clean_scen(.data$Scenario)
        )
      
      # Détection du multiplicateur selon Unit (priorité au paramètre si fourni)
      unit_vals <- unique(na.omit(dat0$Unit))
      mult_auto <- if (length(unit_vals) && any(grepl("1000", unit_vals, fixed = TRUE))) 1000 else 1
      mult <- if (is.null(value_multiplier)) mult_auto else value_multiplier
      
      dat <- dat0 %>%
        dplyr::filter(.data$Item %in% items_keep) %>%
        dplyr::group_by(Scenario, Item, year_used) %>%
        dplyr::summarise(value = sum(.data$Value, na.rm = TRUE), .groups = "drop") %>%
        dplyr::mutate(
          value  = value * mult,         # ha
          year   = .data$year_used
        )
      
      shiny::validate(shiny::need(nrow(dat) > 0,
                                  paste0("Aucune donnée trouvée pour Items: ", paste(items_keep, collapse = ", "))
      ))
      
      # Ordre des items = décroissant sur la base
      base_order <- dat %>%
        dplyr::filter(.data$Scenario == "Année de base") %>%
        dplyr::group_by(Item) %>%
        dplyr::summarise(tot = sum(.data$value, na.rm = TRUE), .groups = "drop") %>%
        dplyr::arrange(dplyr::desc(.data$tot)) %>% dplyr::pull(Item)
      
      items_levels <- unique(c(base_order, setdiff(unique(dat$Item), base_order)))
      
      dat %>%
        dplyr::mutate(
          Item     = factor(as.character(.data$Item), levels = items_levels),
          Scenario = factor(.data$Scenario, levels = SCEN_LEVELS_ALL),
          scen_i   = as.integer(.data$Scenario),   # codes 1..n pour l’axe
          scen_t   = as.character(.data$Scenario)
        ) %>%
        dplyr::filter(!is.na(.data$scen_i)) %>%      # on écarte d’éventuels scénarios exotiques
        dplyr::arrange(.data$Scenario, .data$Item)
    }) %>% bindCache(r_country())
    
    # ---- Plotly ----
    output$stack_ls <- plotly::renderPlotly({
      da <- data_ls(); req(nrow(da) > 0)
      
      base_total <- da %>%
        dplyr::filter(.data$Scenario == "Année de base") %>%
        dplyr::summarise(tot = sum(.data$value, na.rm = TRUE), .groups = "drop") %>%
        dplyr::pull(tot)
      req(length(base_total) == 1, is.finite(base_total))
      
      cols <- livestock_colors_for(levels(da$Item))
      scen_lvls_chr <- levels(da$Scenario)
      tick_vals     <- seq_along(scen_lvls_chr)
      
      p <- plotly::plot_ly() %>%
        plotly::layout(
          barmode = "stack",
          xaxis = list(title = "", type = "linear",
                       tickmode = "array", tickvals = tick_vals, ticktext = scen_lvls_chr),
          yaxis = list(title = "ha", separatethousands = TRUE),
          legend = list(title = list(text = ""))
        )
      
      items_vec <- levels(da$Item); if (is.null(items_vec)) items_vec <- unique(as.character(da$Item))
      for (it in items_vec) {
        sub <- da %>% dplyr::filter(.data$Item == it)
        if (nrow(sub) == 0) next
        p <- plotly::add_bars(
          p, data = sub,
          x = ~scen_i, y = ~value, name = it, legendgroup = it,
          marker = list(color = unname(cols[match(it, items_vec)])),
          text = ~scen_t, textposition = "none",
          hovertemplate = paste0("%{text}<br>", it, " : %{y:.0f} ha<extra></extra>")
        )
      }
      
      # Ligne de référence (base) — visible dans la légende
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
    
    # ---- Export CSV (en dehors du renderPlotly + bon id) ----
    output$dl_livestock_csv <- downloadHandler(
      filename = function(){
        paste0("Livestock_Area_", gsub(" ", "_", r_country()), ".csv")
      },
      content = function(file){
        da <- data_ls(); req(nrow(da) > 0)
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
