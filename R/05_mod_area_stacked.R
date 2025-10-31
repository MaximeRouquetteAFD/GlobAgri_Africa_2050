# R/05_mod_area_stacked.R
# ---------------------------------------------------------------
# Surfaces (Area) — décomposition empilée par scénario
# - Scénarios affichés : "Année de base" + ("Même diète","Diète probable","Diète saine")
# - Filtre par item via selectInput (r_area_item) ; "__ALL__" => pas de filtre
# - Baseline calculée sur le TOTAL (tous items)
# - Axe X catégoriel (supprime le warning "discrete & non-discrete")
# Dépendances attendues (global/utils) :
#   AREA_ELEMENT, AREA_CHILDREN, area_colors_for()
#   (facultatif) COL_NON_ARID, COL_ARID
# ---------------------------------------------------------------

mod_area_stacked_ui <- function(id){
  ns <- NS(id)
  tagList(
    h3("Surfaces (Area) — décomposition empilée par scénario"),
    plotly::plotlyOutput(ns("stack_area"), height = "520px"),
    p(tags$em(
      "Note : les barres représentent la somme des items sélectionnés pour chaque scénario. ",
      "Trois scénarios sont affichés : « Même diète », « Diète probable » et « Diète saine »."
    )),
    br(),
    div(class = "u-actions",
        shiny::downloadLink(ns("dl_area_csv"), label = tagList(shiny::icon("download"), "CSV"))),
  )
}

# r_area_item : reactive string (peut être NULL) — item unique ("__ALL__" = tout)
mod_area_stacked_server <- function(id, fact, r_country,
                                    r_scenarios = reactive(NULL),
                                    r_area_item = reactive(NULL)){
  moduleServer(id, function(input, output, session){
    
    # Helper %||%
    if (!exists("%||%", mode = "function")) `%||%` <- function(a, b) if (is.null(a)) b else a
    
    # Constantes locales
    allowed_ui_scen <- c("Même diète", "Diète probable", "Diète saine")
    SCEN_LEVELS     <- c("Année de base", allowed_ui_scen)
    PASTURE_ITEM    <- "Land under perm. meadows and pastures"
    
    # -----------------------
    # 1) Année retenue par scénario
    # -----------------------
    years_by_scenario_area2 <- reactive({
      dat <- fact %>%
        dplyr::filter(
          Region == r_country(),
          stringr::str_to_lower(Element) == stringr::str_to_lower(AREA_ELEMENT),
          Scenario %in% SCEN_LEVELS
        )
      
      # Baseline toujours incluse ; on filtre sur les scénarios cochés si fournis
      scs <- r_scenarios()
      if (!is.null(scs) && length(scs)) dat <- dat %>% dplyr::filter(Scenario %in% c("Année de base", scs))
      
      base_year <- if (any(dat$Scenario == "Année de base" & dat$Year == 2018 & !is.na(dat$Value))) 2018
      else suppressWarnings(max(dat$Year[dat$Scenario == "Année de base" & !is.na(dat$Value)], na.rm = TRUE))
      
      others <- dat %>%
        dplyr::filter(Scenario != "Année de base") %>%
        dplyr::group_by(Scenario) %>%
        dplyr::summarise(year_used = suppressWarnings(max(Year[!is.na(Value)], na.rm = TRUE)), .groups = "drop")
      
      dplyr::bind_rows(tibble::tibble(Scenario = "Année de base", year_used = base_year), others) %>%
        dplyr::filter(is.finite(year_used)) %>%
        dplyr::mutate(Scenario = factor(Scenario, levels = SCEN_LEVELS)) %>%
        dplyr::arrange(Scenario)
    }) %>% bindCache(
      r_country(),
      paste(sort(r_scenarios() %||% character()), collapse = "|")
    )
    
    # -----------------------
    # 2) Données "tous items" (pour baseline totale & split)
    # -----------------------
    data_area_all <- reactive({
      yrs <- years_by_scenario_area2()
      validate(need(nrow(yrs) > 0, "Pas d'année disponible pour le graphique 'Area'."))
      
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
          value = value * 1000,  # ha (×1000)
          Item  = forcats::fct_relevel(
            Item,
            # ordre de pile souhaité (bas -> haut)
            "Cropland",
            "Land under perm. meadows and pastures",
            "Forest land",
            "Agricultural land occupation (Farm)"
          )
        ) %>%
        dplyr::rename(year = year_used) %>%
        dplyr::mutate(Scenario_cat = as.character(Scenario))
    })
    
    # -----------------------
    # 3) Données filtrées par item (sélecteur)
    # -----------------------
    data_area_filtered <- reactive({
      df <- data_area_all()
      it <- as.character(r_area_item() %||% "__ALL__")
      if (nzchar(it) && !(it %in% c("__ALL__", "Tout")) && it %in% levels(df$Item)) {
        df <- df %>% dplyr::filter(Item == it)
      }
      df
    }) %>% bindCache(
      r_country(),
      paste(sort(r_scenarios() %||% character()), collapse = "|"),
      { it <- as.character(r_area_item() %||% ""); if (it %in% c("__ALL__","Tout")) "" else it }
    )
    
    # -----------------------
    # 3-bis) Données split pour Pâturages : Aride / Non aride (détection robuste)
    # -----------------------
    data_pasture_arid_split <- reactive({
      yrs <- years_by_scenario_area2(); req(nrow(yrs) > 0)
      
      # 0) Total pasture (référence) par scénario/année
      pasture_tot <- data_area_all() %>%
        dplyr::filter(Item == PASTURE_ITEM) %>%
        dplyr::transmute(Scenario, year, value_tot = value,
                         scen_idx = as.integer(Scenario),
                         scen_label = as.character(Scenario))
      
      # 1) Sous-ensemble "area"
      dat <- fact %>%
        dplyr::inner_join(yrs, by = "Scenario") %>%
        dplyr::filter(
          Region == r_country(),
          stringr::str_to_lower(Element) == stringr::str_to_lower(AREA_ELEMENT),
          Year == year_used
        )
      
      # 2) Détection FR/EN, Item ou Element
      norm_part <- function(x){
        x <- tolower(x)
        dplyr::case_when(
          grepl("non[- ]?arid|non[- ]?aride", x) ~ "Non aride",
          grepl("\\barid\\b|aride", x)           ~ "Aride",
          TRUE                                   ~ NA_character_
        )
      }
      
      # A) depuis Item
      split_A <- dat %>%
        dplyr::filter(
          grepl("arid|aride", Item, ignore.case = TRUE) |
            (grepl("pasture|meadow|pâtur", Item, ignore.case = TRUE) & grepl("arid|aride", Item, ignore.case = TRUE))
        ) %>%
        dplyr::mutate(Part = norm_part(Item))
      
      # B) depuis Element (si Item = pasture agrégé)
      split_B <- dat %>%
        dplyr::filter(Item == PASTURE_ITEM, grepl("arid|aride", Element, ignore.case = TRUE)) %>%
        dplyr::mutate(Part = norm_part(Element))
      
      split_raw <- dplyr::bind_rows(split_A, split_B) %>%
        dplyr::filter(!is.na(Part)) %>%
        dplyr::group_by(Scenario, Part, year_used) %>%
        dplyr::summarise(value = sum(Value, na.rm = TRUE), .groups = "drop") %>%
        dplyr::mutate(value = value * 1000) %>%
        dplyr::rename(year = year_used)
      
      if (nrow(split_raw) == 0) return(NULL)
      
      # 3) Re-scale pour sommer au total pasture
      out <- split_raw %>%
        dplyr::left_join(pasture_tot, by = c("Scenario","year")) %>%
        dplyr::group_by(Scenario, year) %>%
        dplyr::mutate(sum_parts = sum(value, na.rm = TRUE)) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(value = dplyr::if_else(
          is.finite(sum_parts) & sum_parts > 0 & is.finite(value_tot),
          value * (value_tot / sum_parts),
          value
        )) %>%
        dplyr::mutate(
          Item       = factor(paste0(PASTURE_ITEM, " — ", Part),
                              levels = c(paste0(PASTURE_ITEM, " — Non aride"),
                                         paste0(PASTURE_ITEM, " — Aride"))),
          Scenario   = factor(Scenario, levels = c("Année de base","Même diète","Diète probable","Diète saine")),
          scen_idx   = as.integer(Scenario),
          scen_label = as.character(Scenario)
        ) %>%
        dplyr::select(Scenario, year, scen_idx, scen_label, Item, Part, value)
      
      out
    }) %>% bindCache(
      r_country(),
      paste(sort(r_scenarios() %||% character()), collapse = "|")
    )
    
    # -----------------------
    # 4) Plot
    # -----------------------
    output$stack_area <- plotly::renderPlotly({
      da_all <- data_area_all();   req(nrow(da_all) > 0)
      da     <- data_area_filtered(); req(nrow(da) > 0)
      
      # Scénarios (ordre & étiquettes)
      scen_lvls_chr <- as.character(levels(da$Scenario))
      x_ticks       <- seq_along(scen_lvls_chr)
      
      # Baseline totale (sur TOUTES les composantes)
      base_total <- da_all %>%
        dplyr::filter(Scenario == "Année de base") %>%
        dplyr::summarise(total = sum(value, na.rm = TRUE), .groups = "drop") %>%
        dplyr::pull(total)
      
      # Visible par défaut seulement si item = "Tout"
      sel_it <- as.character(r_area_item() %||% "__ALL__")
      base_visible <- if (is.finite(base_total) && sel_it %in% c("__ALL__", "Tout", "")) TRUE else "legendonly"
      
      # Ajout scen_idx/scen_label
      da <- da %>%
        dplyr::mutate(
          scen_idx   = as.integer(Scenario),
          scen_label = as.character(Scenario)
        )
      
      # Injection split Aride/Non aride
      if (identical(sel_it, PASTURE_ITEM)) {
        split_df <- data_pasture_arid_split()
        if (!is.null(split_df) && nrow(split_df) > 0) {
          da <- da %>% dplyr::filter(Item != PASTURE_ITEM)
          da <- dplyr::bind_rows(
            da,
            split_df %>% dplyr::transmute(Scenario, year, Item, value, scen_idx, scen_label)
          )
        }
      }
      
      # ----- Ordre définitif des items pour l'empilement (bas -> haut)
      nm_non_arid <- paste0(PASTURE_ITEM, " — Non aride")
      nm_arid     <- paste0(PASTURE_ITEM, " — Aride")
      
      if (any(da$Item %in% c(nm_non_arid, nm_arid))) {
        # split actif
        item_lvls_final <- c(
          "Cropland",
          nm_non_arid,
          nm_arid,
          "Forest land",
          "Agricultural land occupation (Farm)"
        )
      } else {
        # pas de split
        item_lvls_final <- c(
          "Cropland",
          PASTURE_ITEM,
          "Forest land",
          "Agricultural land occupation (Farm)"
        )
      }
      
      # Recatégorise selon l'ordre voulu (clé pour l’empilement)
      da <- da %>% dplyr::mutate(Item = factor(as.character(Item), levels = item_lvls_final))
      
      # On bouclera ensuite sur cet ordre :
      item_lvls <- item_lvls_final
      
      # 1) On essaie de récupérer la palette une seule fois.
      pal <- tryCatch(area_colors_for(unique(c(item_lvls, PASTURE_ITEM))),
                      error = function(e) NULL)
      
      # 2) On construit un "sac de couleurs" vide et on ne copie que ce qui existe.
      cols_items <- setNames(vector("list", length(item_lvls)), item_lvls)
      if (!is.null(pal)) {
        have <- intersect(names(pal), item_lvls)
        for (nm in have) cols_items[[nm]] <- pal[[nm]]
      }
      
      # 3) Couleurs forcées pour le split pâturage
      nm_non_arid <- paste0(PASTURE_ITEM, " — Non aride")
      nm_arid     <- paste0(PASTURE_ITEM, " — Aride")
      col_non_arid <- get0("COL_NON_ARID", ifnotfound = NULL); if (is.null(col_non_arid)) col_non_arid <- "#81C784"
      col_arid     <- get0("COL_ARID",     ifnotfound = NULL); if (is.null(col_arid))     col_arid     <- "#2E7D32"
      if (nm_non_arid %in% names(cols_items)) {
        if (is.null(cols_items[[nm_non_arid]]) || is.na(cols_items[[nm_non_arid]])) cols_items[[nm_non_arid]] <- col_non_arid
      } else {
        cols_items[[nm_non_arid]] <- col_non_arid
      }
      if (nm_arid %in% names(cols_items)) {
        if (is.null(cols_items[[nm_arid]]) || is.na(cols_items[[nm_arid]])) cols_items[[nm_arid]] <- col_arid
      } else {
        cols_items[[nm_arid]] <- col_arid
      }
      
      # 4) Couleur de repli (si un item n’a toujours pas de couleur)
      base_pasture_col <- "#4CAF50"
      if (!is.null(pal) && PASTURE_ITEM %in% names(pal) && !is.null(pal[[PASTURE_ITEM]])) {
        base_pasture_col <- pal[[PASTURE_ITEM]]
      }
      for (nm in names(cols_items)) {
        if (is.null(cols_items[[nm]]) || is.na(cols_items[[nm]])) cols_items[[nm]] <- base_pasture_col
      }
      
      # Prépare cumul pour split covered/excess
      da <- da %>%
        dplyr::arrange(scen_idx, Item) %>%
        dplyr::group_by(scen_idx) %>%
        dplyr::mutate(cum_prev = dplyr::lag(cumsum(value), default = 0)) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(
          covered = dplyr::case_when(
            identical(base_visible, TRUE) & cum_prev < base_total ~ pmin(value, pmax(base_total - cum_prev, 0)),
            identical(base_visible, TRUE) & cum_prev >= base_total ~ 0,
            TRUE ~ value
          ),
          excess = if (identical(base_visible, TRUE)) value - covered else 0
        )
      
      # --- sommes par scénario pour % (uniquement si split actif)
      split_mode <- any(grepl(paste0("^", PASTURE_ITEM, " — "), da$Item))
      totals_pasture <- if (split_mode) {
        da %>% dplyr::filter(Item %in% c(nm_non_arid, nm_arid)) %>%
          dplyr::group_by(scen_idx) %>%
          dplyr::summarise(tot_pasture = sum(value, na.rm = TRUE), .groups = "drop")
      } else NULL
      
      # Totaux par scénario (pour hover des autres items)
      scen_totals <- da_all %>%
        dplyr::group_by(Scenario) %>%
        dplyr::summarise(tot = sum(value, na.rm = TRUE), .groups = "drop") %>%
        dplyr::mutate(scen_idx = as.integer(Scenario))
      
      p <- plotly::plot_ly() %>%
        plotly::layout(
          barmode = "stack",
          xaxis = list(title = "", type = "linear", tickmode = "array",
                       tickvals = seq_along(scen_lvls_chr), ticktext = scen_lvls_chr),
          yaxis = list(title = "ha", separatethousands = TRUE),
          legend = list(title = list(text = ""))
        )
      
      # Barres "covered" (+ % inside si split actif)
      for (it in item_lvls) {
        sub <- da %>% dplyr::filter(Item == it) %>%
          dplyr::left_join(scen_totals, by = "scen_idx")
        
        if (split_mode && grepl(paste0("^", PASTURE_ITEM, " — "), it)) {
          sub <- sub %>%
            dplyr::left_join(totals_pasture, by = "scen_idx") %>%
            dplyr::mutate(
              pct = ifelse(is.finite(tot_pasture) & tot_pasture > 0, 100 * value / tot_pasture, NA_real_),
              label_pct = ifelse(is.finite(pct) & covered > 0, paste0(round(pct, 1), "%"), "")
            )
          
          p <- plotly::add_bars(
            p, data = sub,
            x = ~scen_idx, y = ~covered,
            customdata = ~tot_pasture,         # total pâturage correct pour le hover
            text = ~label_pct,
            textposition = "inside",
            insidetextanchor = "middle",
            textfont = list(color = "white"),
            name = it, legendgroup = it,
            marker = list(color = cols_items[[it]]),
            hovertemplate = paste0(
              "%{text}<br>", it, " (couvert) : %{y:.0f} ha",
              "<br>Total pâturage : %{customdata:.0f} ha<extra></extra>"
            )
          )
        } else {
          p <- plotly::add_bars(
            p, data = sub,
            x = ~scen_idx, y = ~covered, customdata = ~tot, text = ~scen_label,
            textposition = "none",
            name = it, legendgroup = it,
            marker = list(color = cols_items[[it]]),
            hovertemplate = paste0(
              "%{text}<br>", it, " (couvert) : %{y:.0f} ha",
              "<br><b>Total scénario</b> : %{customdata:.0f} ha<extra></extra>"
            )
          )
        }
      }
      
      # Barres "excess" (hachurées) uniquement si baseline visible
      if (identical(base_visible, TRUE)) {
        first_excess <- TRUE
        for (it in item_lvls) {
          sub <- da %>% dplyr::filter(Item == it, excess > 0) %>%
            dplyr::left_join(scen_totals, by = "scen_idx")
          if (nrow(sub) == 0) next
          p <- plotly::add_bars(
            p, data = sub,
            x = ~scen_idx, y = ~excess, customdata = ~tot, text = ~scen_label,
            textposition = "none",
            name = if (first_excess) "Besoin non-couvert" else "Besoin non-couvert",
            showlegend = first_excess, legendgroup = "excess",
            marker = list(
              color = cols_items[[it]],
              line = list(color = I(cols_items[[it]]), width = 0.5),
              pattern = list(shape = "/", bgcolor = cols_items[[it]], fgcolor = "rgba(40,40,40,0.7)", solidity = 0.5)
            ),
            hovertemplate = paste0(
              "%{text}<br>", it, " — besoin non-couvert : %{y:.0f} ha",
              "<br><b>Total scénario</b> : %{customdata:.0f} ha<extra></extra>"
            )
          )
          first_excess <- FALSE
        }
      }
      
      # Ligne baseline
      if (is.finite(base_total)) {
        p <- plotly::add_trace(
          p, type = "scatter", mode = "lines",
          x = c(0.5, length(x_ticks) + 0.5),
          y = c(base_total, base_total),
          name = "surface exploitable maximum",
          legendgroup = "baseline",
          hoverinfo = "skip",
          line = list(dash = "dash", color = "gray", width = 1.2),
          visible = base_visible
        )
      }
      
      p
    })
    
    # -----------------------
    # 5) Export CSV — inclure TOUT (items agrégés + split aride/non-aride)
    # -----------------------
    output$dl_area_csv <- downloadHandler(
      filename = function(){ paste0("Area_", gsub(" ", "_", r_country()), ".csv") },
      content = function(file){
        da_all <- data_area_all(); req(nrow(da_all) > 0)
        split_df <- data_pasture_arid_split()
        
        export_df <- da_all %>%
          dplyr::mutate(
            Pays     = r_country(),
            Scenario = as.character(Scenario),
            Annee    = year,
            Item     = as.character(Item),
            Valeur_ha= value
          ) %>%
          dplyr::select(Pays, Scenario, Annee, Item, Valeur_ha)
        
        if (!is.null(split_df) && nrow(split_df) > 0) {
          export_df <- dplyr::bind_rows(
            export_df,
            split_df %>%
              dplyr::transmute(
                Pays      = r_country(),
                Scenario  = as.character(Scenario),
                Annee     = year,
                Item      = as.character(Item),   # "Pasture — Non aride" / "Pasture — Aride"
                Valeur_ha = value
              )
          )
        }
        
        export_df <- export_df %>%
          dplyr::arrange(Scenario, Annee, Item)
        
        readr::write_delim(export_df, file, delim = ";")
      }
    )
  })
}


