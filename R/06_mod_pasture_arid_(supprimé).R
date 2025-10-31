# R/06_mod_pasture_arid.R
# ----------------------------------------------------------------------
# Barres horizontales 100 % : part "aride" vs "non-aride" dans les pâturages
# + Export CSV des parts par scénario
# ----------------------------------------------------------------------

mod_pasture_arid_ui <- function(id){
  ns <- NS(id)
  uiOutput(ns("block"))
}

mod_pasture_arid_server <- function(
    id, fact, r_country, r_scenarios = reactive(NULL), r_area_item_select = reactive(NULL)
){
  moduleServer(id, function(input, output, session){
    
    if (!exists("%||%", mode = "function")) {
      `%||%` <- function(a, b) if (is.null(a)) b else a
    }
    
    PASTURE_ITEM   <- "Land under perm. meadows and pastures"
    ITEM_ARID      <- "Arid Land under perm. meadows and pastures"
    ITEM_NON_ARID  <- "Non arid Land under perm. meadows and pastures"
    ALLOWED_SCENS  <- c("Même diète", "Diète probable", "Diète saine")
    
    show_block <- reactive({
      it <- as.character(r_area_item_select() %||% "__ALL__")
      it %in% c("__ALL__", "Tout", PASTURE_ITEM)
    })
    
    years_by_scenario <- reactive({
      dat <- fact %>%
        dplyr::filter(
          Region == r_country(),
          tolower(Element) == "area",
          Item %in% c(ITEM_ARID, ITEM_NON_ARID),
          Scenario %in% intersect(ALLOWED_SCENS, unique(Scenario))
        )
      scs <- r_scenarios()
      if (!is.null(scs) && length(scs)) dat <- dat %>% dplyr::filter(Scenario %in% scs)
      
      dat %>%
        dplyr::group_by(Scenario) %>%
        dplyr::summarise(
          year_used = suppressWarnings(max(Year[!is.na(Value)], na.rm = TRUE)),
          .groups = "drop"
        ) %>%
        dplyr::filter(is.finite(year_used)) %>%
        dplyr::mutate(Scenario = factor(Scenario, levels = ALLOWED_SCENS)) %>%
        dplyr::arrange(Scenario)
    }) %>% bindCache(r_country(), paste(sort(r_scenarios() %||% character()), collapse = "|"))
    
    data_base <- reactive({
      yrs <- years_by_scenario()
      validate(need(nrow(yrs) > 0, "Pas d'année disponible pour ce graphique."))
      
      fact %>%
        dplyr::inner_join(yrs, by = "Scenario") %>%
        dplyr::filter(
          Region == r_country(),
          tolower(Element) == "area",
          Year == year_used,
          Item %in% c(ITEM_ARID, ITEM_NON_ARID)
        ) %>%
        dplyr::mutate(
          Scenario = factor(Scenario, levels = ALLOWED_SCENS),
          Aridity  = dplyr::case_when(
            Item == ITEM_ARID     ~ "Aride",
            Item == ITEM_NON_ARID ~ "Non aride",
            TRUE                  ~ NA_character_
          )
        ) %>%
        dplyr::group_by(Scenario, Aridity, year_used) %>%
        dplyr::summarise(value = sum(Value, na.rm = TRUE) * 1000, .groups = "drop") %>%
        dplyr::rename(year = year_used) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(Aridity = factor(Aridity, levels = PASTURE_ARID_LEVELS)) %>%
        dplyr::arrange(Scenario, Aridity)
    }) %>% bindCache(r_country(), paste(sort(r_scenarios() %||% character()), collapse = "|"))
    
    data_bars <- reactive({
      db <- data_base(); req(nrow(db) > 0)
      
      tot <- db %>%
        dplyr::group_by(Scenario, year) %>%
        dplyr::summarise(total = sum(value, na.rm = TRUE), .groups = "drop")
      
      db %>%
        dplyr::left_join(tot, by = c("Scenario", "year")) %>%
        dplyr::mutate(
          part     = dplyr::if_else(total > 0, value / total, NA_real_),
          part_lab = dplyr::if_else(is.finite(part) & part >= 0.05,
                                    scales::percent(part, accuracy = 0.1), "")
        ) %>%
        dplyr::arrange(Scenario, Aridity)
    })
    
    output$block <- renderUI({
      if (!isTRUE(show_block())) return(NULL)
      ns <- session$ns
      tagList(
        h4("Part aride / non-aride — Pâturages (barres 100 % par scénario)"),
        tags$p(tags$em(
          "Calcul sur l’élément « Area » (×1000) pour Arid/Non arid Land under perm. meadows and pastures. ",
          "Chaque barre = 100 % de la surface de pâturage du scénario."
        )),
        plotOutput(ns("arid_bars"), height = "340px"),
        br(),
        downloadButton(ns("dl_pies_csv"), "csv"),
        tags$hr()
      )
    })
    
    # --- Plot (sans argument 'dev' pour compatibilité Shiny) ---
    output$arid_bars <- renderPlot({
      req(isTRUE(show_block()))
      db <- data_bars(); req(nrow(db) > 0)
      
      cols <- pasture_arid_colors_for(c("Aride", "Non aride"))
      cols <- setNames(cols, c("Aride", "Non aride"))
      
      ggplot2::ggplot(db, ggplot2::aes(x = Scenario, y = value, fill = Aridity)) +
        ggplot2::geom_col(position = "fill", width = 0.62) +
        ggplot2::geom_text(
          ggplot2::aes(label = part_lab),
          position = ggplot2::position_fill(vjust = 0.5),
          size = 4, color = "black"
        ) +
        ggplot2::scale_y_continuous(labels = scales::percent) +
        ggplot2::scale_fill_manual(values = cols, drop = FALSE) +
        ggplot2::coord_flip() +
        ggplot2::labs(x = NULL, y = "Part de la surface de pâturage (%)", fill = NULL) +
        ggplot2::theme_minimal(base_size = 13) +
        ggplot2::theme(
          legend.position = "top",
          legend.direction = "horizontal",
          legend.key.width  = grid::unit(18, "pt"),
          legend.key.height = grid::unit(12, "pt"),
          legend.text = ggplot2::element_text(size = 12),
          axis.text  = ggplot2::element_text(size = 11),
          axis.title = ggplot2::element_text(size = 12),
          panel.grid.major.y = ggplot2::element_blank()
        )
    }, res = 90, height = 400)  # ↑ garder un 'res' élevé pour la netteté
    
    # ---- Export CSV
    data_pies_export <- reactive({
      db <- data_bars(); req(nrow(db) > 0)
      db %>%
        dplyr::transmute(
          Pays        = r_country(),
          Scenario    = as.character(Scenario),
          Annee       = year,
          Zone        = as.character(Aridity),
          Surface_ha  = value,
          Part        = part,
          `Part (%)`  = 100 * part
        )
    })
    
    output$dl_pies_csv <- downloadHandler(
      filename = function(){
        paste0("pasture_arid_shares_", gsub(" ", "_", r_country()), ".csv")
      },
      content = function(file){
        readr::write_delim(data_pies_export(), file, delim = ";")
      }
    )
  })
}

