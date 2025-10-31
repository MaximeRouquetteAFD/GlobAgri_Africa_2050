# R/04_mod_harvest_prod.R
# ------------------------------------------------------------------
# Tableau : Surface cultivée/pâturée (ha) — par item (groupé) et scénario
# + Ajout : "Part des surfaces dans le scénario (%)"
# + Affichage : surfaces (ha) sans décimales
# ------------------------------------------------------------------

mod_harvest_prod_ui <- function(id){
  ns <- NS(id)
  uiOutput(ns("block"))
}

mod_harvest_prod_server <- function(
    id,
    fact,
    r_country,
    r_energy_items     = reactive(NULL),   # ignoré ici (compat signature)
    r_scenarios        = reactive(NULL),
    r_area_item_select = reactive(NULL)    # sélecteur mono-choix (Surface)
){
  moduleServer(id, function(input, output, session){

    # ---- Helper %||%
    if (!exists("%||%", mode = "function")) {
      `%||%` <- function(a, b) if (is.null(a)) b else a
    }

    # ---- Canonisation des libellés (items)
    SHEEP_RAW <- c(
      "Sheep and goats","Sheep & goats","Sheep and goat",
      "Sheep","Goats","Goat",
      "Mutton and goat","Sheep meat and goat meat",
      "Meat sheep and goats","Meat sheep and goat"
    )

    canonize_item <- function(x){
      x <- sub("^Other$", "Other crops", x)
      x <- ifelse(x == "Beef cattle", "Bovine meat", x)
      x_norm <- tolower(trimws(x))
      x[ x_norm %in% tolower(SHEEP_RAW) ] <- "Meat sheep and goats"
      x
    }
    canonize_df <- function(df){
      if (!"Item" %in% names(df)) return(df)
      df$Item <- canonize_item(df$Item)
      df
    }

    # ---- UI : bloc replié + bouton esthétique (Bootstrap collapse)
    output$block <- renderUI({
      ns <- session$ns
      panel_id <- ns("tbl_panel")
      btn_id   <- ns("toggle_btn")

      tagList(
        tags$style(HTML(
          ".tbl-toggle{font-weight:600;border-radius:8px;padding:8px 12px;}
           .tbl-toggle i{margin-right:6px;}
           .tbl-panel{margin-top:10px;}"
        )),
        tags$a(
          id = btn_id, class = "btn btn-outline-primary tbl-toggle",
          `data-toggle` = "collapse", href = paste0("#", panel_id),
          role = "button", `aria-expanded` = "false",
          `aria-controls` = panel_id,
          HTML("<i class='fa fa-table'></i> Voir les surfaces par item")
        ),
        div(id = panel_id, class = "collapse tbl-panel",
            br(),
            h4("Surface cultivée/pâturée (ha) — par item (groupé) et scénario"),
            DT::DTOutput(ns("harvest_prod_table")),
            br(),
            downloadButton(ns("dl_harvest_prod"), "csv"),
            tags$hr()
        ),
        tags$script(HTML(sprintf("
          (function($){
            var btn = $('#%s'), pnl = $('#%s');
            pnl.on('shown.bs.collapse', function(){
              btn.removeClass('btn-outline-primary').addClass('btn-primary')
                 .html(\"<i class='fa fa-eye-slash'></i> Masquer les surfaces par item\");
            });
            pnl.on('hidden.bs.collapse', function(){
              btn.removeClass('btn-primary').addClass('btn-outline-primary')
                 .html(\"<i class='fa fa-table'></i> Voir les surfaces par item\");
            });
          })(jQuery);
        ", btn_id, panel_id)))
      )
    })

    # ---- Années par scénario (baseline incluse), filtrées par cases scénarios
    years_by_scenario_table <- reactive({
      dat <- fact %>%
        dplyr::filter(
          Region == r_country(),
          tolower(Element) %in% c("area harvested","production","area"),
          Scenario %in% intersect(SCEN_TABLE, unique(Scenario))
        )

      scs <- r_scenarios()
      if (!is.null(scs) && length(scs)) {
        dat <- dat %>% dplyr::filter(Scenario %in% c("Année de base", scs))
      }

      dat %>%
        dplyr::group_by(Scenario) %>%
        dplyr::summarise(
          year_used = suppressWarnings(max(Year[!is.na(Value)], na.rm = TRUE)),
          .groups = "drop"
        ) %>%
        dplyr::filter(is.finite(year_used)) %>%
        dplyr::mutate(Scenario = factor(Scenario, levels = SCEN_TABLE)) %>%
        dplyr::arrange(Scenario)
    }) %>% bindCache(r_country(), paste(sort(r_scenarios() %||% character()), collapse = "|"))

    # ---- Données principales
    data_harvest_prod <- reactive({
      yrs <- years_by_scenario_table()
      validate(need(nrow(yrs) > 0, "Pas d'année disponible pour le tableau."))

      animals_canon     <- c("Dairy","Bovine meat","Meat sheep and goats")
      avail_table_items <- sort(unique(canonize_item(fact$Item)))

      # Items autorisés selon le sélecteur d’item de surface
      selected_area <- as.character(r_area_item_select() %||% "__ALL__")
      if (!nzchar(selected_area) || selected_area %in% c("__ALL__", "Tout")) {
        allowed_items <- avail_table_items
      } else if (selected_area == "Cropland") {
        allowed_items <- setdiff(avail_table_items, animals_canon)
      } else if (selected_area == "Land under perm. meadows and pastures") {
        allowed_items <- intersect(avail_table_items, animals_canon)
      } else {
        allowed_items <- avail_table_items
      }
      validate(need(length(allowed_items) > 0, "Aucun item disponible pour ce filtre."))

      # Noms "bruts" pour Element == "Area" (animaux)
      allowed_animals_raw <- character(0)
      if ("Dairy" %in% allowed_items)                allowed_animals_raw <- c(allowed_animals_raw, "Dairy")
      if ("Bovine meat" %in% allowed_items)          allowed_animals_raw <- c(allowed_animals_raw, "Beef cattle")
      if ("Meat sheep and goats" %in% allowed_items) allowed_animals_raw <- c(allowed_animals_raw, SHEEP_RAW)

      # A) Surfaces cultures
      harv_crops <- fact %>%
        dplyr::inner_join(yrs, by = "Scenario") %>%
        dplyr::filter(
          Region == r_country(),
          tolower(Element) == "area harvested",
          Year == year_used
        ) %>%
        canonize_df() %>%
        dplyr::filter(Item %in% setdiff(allowed_items, animals_canon)) %>%
        dplyr::group_by(Item, Scenario) %>%
        dplyr::summarise(`Surface cultivée/pâturée (ha)` = sum(Value, na.rm = TRUE), .groups = "drop")

      # B) Surfaces animales (×1000)
      harv_animals <- fact %>%
        dplyr::inner_join(yrs, by = "Scenario") %>%
        dplyr::filter(
          Region == r_country(),
          tolower(Element) == "area",
          Year == year_used,
          tolower(Item) %in% tolower(allowed_animals_raw)
        ) %>%
        dplyr::mutate(
          Item = dplyr::case_when(
            Item == "Beef cattle"                 ~ "Bovine meat",
            tolower(Item) %in% tolower(SHEEP_RAW) ~ "Meat sheep and goats",
            TRUE                                  ~ Item
          )
        ) %>%
        dplyr::group_by(Item, Scenario) %>%
        dplyr::summarise(`Surface cultivée/pâturée (ha)` = sum(Value, na.rm = TRUE) * 1000, .groups = "drop")

      # Fusion cultures + animaux
      harv <- dplyr::bind_rows(harv_crops, harv_animals)
      validate(need(nrow(harv) > 0, "Aucune donnée disponible pour ce filtre."))

      # C) Baseline (pour % d'évolution) — année 2018 prioritaire sinon max
      base_year_tab <- {
        datb <- fact %>%
          dplyr::filter(
            Region == r_country(), Scenario == "Année de base",
            tolower(Element) %in% c("area harvested","area"),
            !is.na(Value)
          )
        if (nrow(datb) == 0) NA_integer_
        else if (any(datb$Year == 2018, na.rm = TRUE)) 2018
        else suppressWarnings(max(datb$Year, na.rm = TRUE))
      }

      harv_base_crops <- fact %>%
        dplyr::filter(
          Region == r_country(), Scenario == "Année de base",
          tolower(Element) == "area harvested", Year == base_year_tab
        ) %>%
        canonize_df() %>%
        dplyr::filter(Item %in% setdiff(allowed_items, animals_canon)) %>%
        dplyr::group_by(Item) %>%
        dplyr::summarise(base_surface = sum(Value, na.rm = TRUE), .groups = "drop")

      harv_base_animals <- fact %>%
        dplyr::filter(
          Region == r_country(), Scenario == "Année de base",
          tolower(Element) == "area", Year == base_year_tab,
          tolower(Item) %in% tolower(allowed_animals_raw)
        ) %>%
        dplyr::mutate(
          Item = dplyr::case_when(
            Item == "Beef cattle"                 ~ "Bovine meat",
            tolower(Item) %in% tolower(SHEEP_RAW) ~ "Meat sheep and goats",
            TRUE                                  ~ Item
          )
        ) %>%
        dplyr::group_by(Item) %>%
        dplyr::summarise(base_surface = sum(Value, na.rm = TRUE) * 1000, .groups = "drop")

      harv_base <- dplyr::bind_rows(harv_base_crops, harv_base_animals)

      # D) Totaux par scénario (sur les lignes affichées) pour la "Part des surfaces"
      totals_by_scen <- harv %>%
        dplyr::group_by(Scenario) %>%
        dplyr::summarise(total_surface_scen = sum(`Surface cultivée/pâturée (ha)`, na.rm = TRUE), .groups = "drop")

      sc_levels <- intersect(SCEN_TABLE, unique(harv$Scenario))

      out <- harv %>%
        dplyr::left_join(totals_by_scen, by = "Scenario") %>%
        dplyr::left_join(harv_base, by = "Item") %>%
        dplyr::mutate(
          `Evolution des surfaces / Année de base (%)` = dplyr::if_else(
            !is.na(base_surface) & base_surface > 0,
            (`Surface cultivée/pâturée (ha)` - base_surface) / base_surface,
            NA_real_
          ),
          `Part des surfaces dans le scénario (%)` = dplyr::if_else(
            !is.na(total_surface_scen) & total_surface_scen > 0,
            `Surface cultivée/pâturée (ha)` / total_surface_scen,
            NA_real_
          ),
          Item     = factor(Item, levels = sort(unique(Item))),
          Scenario = factor(Scenario, levels = sc_levels)
        ) %>%
        dplyr::select(
          Item, Scenario,
          `Surface cultivée/pâturée (ha)`,
          `Evolution des surfaces / Année de base (%)`,
          `Part des surfaces dans le scénario (%)`
        ) %>%
        dplyr::arrange(Item, Scenario)

      out
    }) %>% bindCache(
      r_country(),
      paste(sort(r_scenarios() %||% character()), collapse = "|"),
      { it <- as.character(r_area_item_select() %||% ""); if (it %in% c("__ALL__","Tout")) "" else it }
    )

    # ---- Rendu DataTable (sans pagination/infos)
    output$harvest_prod_table <- DT::renderDT({
      df <- data_harvest_prod(); req(nrow(df) > 0)
      
      df <- df %>%
        dplyr::mutate(
          `Surface cultivée/pâturée (ha)`              = as.numeric(tidyr::replace_na(`Surface cultivée/pâturée (ha)`, 0)),
          `Evolution des surfaces / Année de base (%)` = as.numeric(`Evolution des surfaces / Année de base (%)`),
          `Part des surfaces dans le scénario (%)`     = as.numeric(`Part des surfaces dans le scénario (%)`)
        )
      
      DT::datatable(
        df,
        colnames = c(
          "", "Scénario",
          "Surface cultivée/pâturée (ha)",
          "Evolution des surfaces / Année de base (%)",
          "Part des surfaces dans le scénario (%)"
        ),
        rownames = FALSE,
        options = list(
          paging   = FALSE,
          info     = FALSE,
          searching= FALSE,
          ordering = FALSE,
          dom      = "t",
          rowGroup = list(
            dataSrc = 0,
            startRender = DT::JS("function(rows, group){ return '<span>' + group + '</span>'; }")
          ),
          columnDefs = list(
            list(targets = 0, visible = FALSE),
            list(targets = 1, className = "col-scenario", width = "40%"),
            list(targets = 2, className = "col-surface",  width = "30%"),
            list(targets = 3, className = "col-aug",      width = "15%"),
            list(targets = 4, className = "col-share",    width = "15%")
          )
        ),
        extensions = "RowGroup",
        escape = FALSE
      ) %>%
        # --- D'abord les formats (valeurs)
        DT::formatCurrency(
          "Surface cultivée/pâturée (ha)",
          currency = "", interval = 3, mark = " ", digits = 0
        ) %>%
        DT::formatPercentage(
          c("Evolution des surfaces / Année de base (%)",
            "Part des surfaces dans le scénario (%)"), 1
        ) %>%
        # --- Puis l'alignement
        DT::formatStyle(
          c("Surface cultivée/pâturée (ha)",
            "Evolution des surfaces / Année de base (%)",
            "Part des surfaces dans le scénario (%)"),
          `text-align` = "center"
        ) %>%
        # --- Et enfin les couleurs (appliquées en dernier)
        DT::formatStyle(
          "Evolution des surfaces / Année de base (%)",
          backgroundColor = DT::styleInterval(HEAT_BREAKS, HEAT_COLORS)
        ) %>%
        DT::formatStyle(
          "Part des surfaces dans le scénario (%)",
          backgroundColor = DT::styleInterval(SHARE_BREAKS, SHARE_COLORS)
        )
    })
    
    # ---- Export CSV (inclut la colonne de part)
    output$dl_harvest_prod <- downloadHandler(
      filename = function(){
        paste0("harvest_surfaces_", gsub(" ", "_", r_country()), ".csv")
      },
      content = function(file){
        readr::write_delim(
          data_harvest_prod() %>% dplyr::mutate(Scenario = as.character(Scenario)),
          file, delim = ";"
        )
      }
    )
  })
}
