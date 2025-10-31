# R/10_mod_surface_cards.R
# ---------------------------------------------------------------
# Encadrés KPI par scénario + drilldown (branchés sur app.css)
# ---------------------------------------------------------------

mod_surface_cards_ui <- function(id, height = "120px"){
  ns <- NS(id)
  tagList(
    # Racine du module
    div(id = ns("root"), class = "surface-cards",
        # Titre dynamique (affiché seulement si “Tout”)
        uiOutput(ns("cards_title")),
        
        # Cartes KPI
        div(
          id = ns("cards_root"),
          # On propage la hauteur via une variable CSS minimale (optionnelle)
          style = sprintf("--surf-kpi-h:%s;", height),
          uiOutput(ns("cards"))
        ),
        
        # Drilldown
        uiOutput(ns("drill_header")),
        plotly::plotlyOutput(ns("drill_plot"), height = "420px")
    ),
    
    # JS délégué : clic + clavier (sur les hooks .surf-kpi)
    tags$script(HTML(paste0(
      "$(document).on('click', '#", ns("root"), " .surf-kpi', function(){",
      "  var scen = $(this).data('scen') || null;",
      "  Shiny.setInputValue('", ns("card_clicked"), "', scen, {priority:'event'});",
      "});",
      "$(document).on('keydown', '#", ns("root"), " .surf-kpi', function(e){",
      "  if (e.key === 'Enter' || e.key === ' ') { e.preventDefault(); $(this).click(); }",
      "});"
    )))
  )
}

mod_surface_cards_server <- function(
    id,
    fact,
    r_country,
    r_scenarios = reactive(NULL),
    r_area_item = reactive("__ALL__")
){
  moduleServer(id, function(input, output, session){
    
    # --------- Constantes
    allowed_show <- c("Même diète","Diète probable","Diète saine")
    if (!exists("%||%", mode = "function")) `%||%` <- function(a, b) if (is.null(a)) b else a
    if (!exists("PASTURE_ITEM", inherits = TRUE))
      PASTURE_ITEM <- "Land under perm. meadows and pastures"
    
    # --------- Année retenue par scénario
    years_by_scenario <- reactive({
      allowed <- c("Année de base", allowed_show)
      dat <- fact %>%
        dplyr::filter(
          Region == r_country(),
          stringr::str_to_lower(Element) == stringr::str_to_lower(AREA_ELEMENT),
          Scenario %in% allowed
        )
      base_year <- if (any(dat$Scenario == "Année de base" & dat$Year == 2018 & !is.na(dat$Value))) 2018
      else suppressWarnings(max(dat$Year[dat$Scenario == "Année de base" & !is.na(dat$Value)], na.rm = TRUE))
      others <- dat %>%
        dplyr::filter(Scenario != "Année de base") %>%
        dplyr::group_by(Scenario) %>%
        dplyr::summarise(year_used = suppressWarnings(max(Year[!is.na(Value)], na.rm = TRUE)), .groups = "drop")
      dplyr::bind_rows(tibble::tibble(Scenario = "Année de base", year_used = base_year), others) %>%
        dplyr::filter(is.finite(year_used))
    })
    
    # --------- Valeurs KPI (ha) + delta vs base
    vals_surface <- reactive({
      yrs <- years_by_scenario(); req(nrow(yrs) > 0)
      it  <- r_area_item() %||% "__ALL__"
      
      base <- fact %>%
        dplyr::inner_join(yrs, by = "Scenario") %>%
        dplyr::filter(
          Region == r_country(),
          stringr::str_to_lower(Element) == stringr::str_to_lower(AREA_ELEMENT),
          Year == year_used
        ) %>%
        dplyr::mutate(Value = dplyr::if_else(Item == "Forest land" & !is.na(Value) & Value < 0, 0, Value))
      
      sel <- if (it %in% c("__ALL__","Tout","")) {
        base %>% dplyr::filter(Item %in% AREA_CHILDREN)   # somme hors forêt
      } else {
        base %>% dplyr::filter(Item == it)
      }
      
      agg <- sel %>%
        dplyr::group_by(Scenario) %>%
        dplyr::summarise(value_ha = sum(Value, na.rm = TRUE) * 1000, .groups = "drop") %>%
        dplyr::mutate(value_ha = pmax(value_ha, 0))
      
      base_ha <- agg %>% dplyr::filter(Scenario == "Année de base") %>% dplyr::pull(value_ha)
      base_ha <- if (length(base_ha)) base_ha[1] else NA_real_
      
      agg %>%
        dplyr::filter(Scenario %in% allowed_show) %>%
        dplyr::mutate(
          pct_vs_base = if (is.finite(base_ha) && base_ha > 0) 100 * (value_ha - base_ha) / base_ha else NA_real_,
          Scenario = factor(Scenario, levels = allowed_show)
        ) %>%
        dplyr::arrange(Scenario)
    }) %>% bindCache(r_country(), r_area_item())
    
    # --------- Sélection (toggle)
    r_drill_scen <- reactiveVal(NULL)
    observeEvent(input$card_clicked, {
      scen <- input$card_clicked
      cur  <- r_drill_scen()
      if (!is.null(cur) && identical(cur, scen)) r_drill_scen(NULL) else r_drill_scen(scen)
    }, ignoreInit = TRUE)
    observeEvent(r_country(),   { r_drill_scen(NULL) }, ignoreInit = TRUE)
    observeEvent(r_area_item(), { r_drill_scen(NULL) }, ignoreInit = TRUE)
    
    # --------- Titre au-dessus des cartes (uniquement si "Tout")
    output$cards_title <- renderUI({
      if (r_area_item() %in% c("__ALL__", "Tout")) {
        h3("Somme des surfaces agricoles (hors forêt)")
      } else NULL
    })
    
    # --------- Rendu des cartes (utilise u-card / u-row)
    output$cards <- renderUI({
      dat <- vals_surface()
      if (is.null(dat) || nrow(dat) == 0) return(NULL)
      
      fmt0    <- function(x){ if (is.na(x)) "—" else format(round(x), big.mark = " ", scientific = FALSE, trim = TRUE) }
      round1  <- function(x) ifelse(is.finite(x), round(x, 1), NA_real_)
      fmtSign <- function(p){
        if (!is.finite(p)) return("—")
        paste0(ifelse(p >= 0, "+", ""), formatC(p, digits = 1, format = "f"), "%")
      }
      
      sel <- r_drill_scen()
      
      cards <- lapply(seq_len(nrow(dat)), function(i){
        sc  <- as.character(dat$Scenario[i])
        val <- dat$value_ha[i]
        dlt <- round1(dat$pct_vs_base[i])
        delta_tag <- if (!is.finite(dlt)) "—" else
          if (dlt > 0) span(class="up",  fmtSign(dlt)) else
            if (dlt < 0) span(class="down",fmtSign(dlt)) else "0%"
        
        div(
          class = paste(
            "u-card u-card--flat u-card--hover u-card--clickable u-card--focus surf-kpi",
            if (!is.null(sel) && identical(sel, sc)) "is-active" else ""
          ),
          tabindex = "0",
          `data-scen` = sc,
          # contenu
          div(class = "u-box",
              p(class = "u-title", sc),
              p(class = "u-value", fmt0(val), span(class = "u-unit", "ha")),
              p(class = "u-sub", "Vs Année de base : ", delta_tag)
          )
        )
      })
      
      div(class = "u-row", !!!cards)
    })
    
    # --------- Données "pâturages -> troupeaux" (Area)
    pasture_livestock_split_for <- function(scen){
      yrs <- years_by_scenario(); req(nrow(yrs) > 0)
      base_year <- yrs %>% dplyr::filter(Scenario == "Année de base") %>% dplyr::pull(year_used) %>% { if (length(.)>0) .[1] else NA_integer_ }
      scen_year <- yrs %>% dplyr::filter(Scenario == scen) %>% dplyr::pull(year_used) %>% { if (length(.)>0) .[1] else NA_integer_ }
      
      wanted <- LIVESTOCK_LABELS
      
      base_df <- fact %>%
        dplyr::filter(Region==r_country(), Element=="Area",
                      Scenario=="Année de base", Year==base_year, Item %in% wanted) %>%
        dplyr::group_by(Item) %>% dplyr::summarise(value=sum(Value,na.rm=TRUE)*1000, .groups="drop") %>%
        dplyr::mutate(Phase="Année de base")
      
      scen_df <- fact %>%
        dplyr::filter(Region==r_country(), Element=="Area",
                      Scenario==scen, Year==scen_year, Item %in% wanted) %>%
        dplyr::group_by(Item) %>% dplyr::summarise(value=sum(Value,na.rm=TRUE)*1000, .groups="drop") %>%
        dplyr::mutate(Phase=scen)
      
      dplyr::bind_rows(base_df, scen_df) %>%
        dplyr::mutate(Item = factor(Item, levels = wanted),
                      Lab  = as.character(Item))
    }
    
    # --------- Données Emissions (Base vs Scénario) pour item "Tout"
    emissions_for <- function(scen){
      yrs <- years_by_scenario(); req(nrow(yrs) > 0)
      base_year <- yrs %>% dplyr::filter(Scenario=="Année de base") %>% dplyr::pull(year_used) %>% { if (length(.)>0) .[1] else NA_integer_ }
      scen_year <- yrs %>% dplyr::filter(Scenario==scen) %>% dplyr::pull(year_used) %>% { if (length(.)>0) .[1] else NA_integer_ }
      
      base_df <- fact %>%
        dplyr::filter(Region==r_country(), Element=="Emissions",
                      Scenario=="Année de base", Year==base_year) %>%
        dplyr::group_by(Item) %>% dplyr::summarise(value=sum(Value,na.rm=TRUE), .groups="drop") %>%
        dplyr::mutate(Phase="Année de base")
      
      scen_df <- fact %>%
        dplyr::filter(Region==r_country(), Element=="Emissions",
                      Scenario==scen, Year==scen_year) %>%
        dplyr::group_by(Item) %>% dplyr::summarise(value=sum(Value,na.rm=TRUE), .groups="drop") %>%
        dplyr::mutate(Phase=scen)
      
      dplyr::bind_rows(base_df, scen_df) %>%
        dplyr::mutate(Item = as.character(Item))
    }
    
    # --------- Drill header + contrôles
    output$drill_header <- renderUI({
      sc <- r_drill_scen(); it <- r_area_item()
      if (is.null(sc)) return(NULL)
      
      div(
        # pas d’encadré : header “plain”
        h3(
          if (it %in% c("__ALL__","Tout"))
            sprintf("Émissions de CO₂ associées (tCO₂) — %s", sc)
          else
            sprintf("Détail — %s", sc)
        ),
        if (it %in% c("__ALL__","Tout")) {
          checkboxInput(session$ns("include_luc"),
                        "Inclure le changement d'allocation des terres", FALSE)
        }
      )
    })
    
    # --------- Plot : selon l’item sélectionné
    output$drill_plot <- plotly::renderPlotly({
      sc <- r_drill_scen(); req(sc)
      area_item <- r_area_item()
      
      if (identical(area_item, PASTURE_ITEM)) {
        df <- pasture_livestock_split_for(sc); req(nrow(df) > 0)
        col_vec <- livestock_colors_for(LIVESTOCK_LABELS)
        
        base_df <- df[df$Phase == "Année de base", ]
        scen_df <- df[df$Phase == sc, ]
        
        p <- plotly::plot_ly() %>%
          plotly::add_pie(
            data = base_df, labels = ~Lab, values = ~value, sort = FALSE,
            textinfo = "percent",
            hovertemplate = "%{label} : %{value:.0f} ha<extra></extra>",
            name = "Année de base", legendgroup = "base", showlegend = TRUE,
            marker = list(colors = col_vec), domain = list(x = c(0.00, 0.48), y = c(0, 1))
          ) %>%
          plotly::add_pie(
            data = scen_df, labels = ~Lab, values = ~value, sort = FALSE,
            textinfo = "percent",
            hovertemplate = "%{label} : %{value:.0f} ha<extra></extra>",
            name = sc, legendgroup = "base", showlegend = TRUE,
            marker = list(colors = col_vec), domain = list(x = c(0.52, 1.00), y = c(0, 1))
          ) %>%
          plotly::layout(
            annotations = list(
              list(text = "Année de base", x = 0.24, y = 1.08, xref = "paper", yref = "paper",
                   showarrow = FALSE, font = list(size = 14)),
              list(text = sc, x = 0.76, y = 1.08, xref = "paper", yref = "paper",
                   showarrow = FALSE, font = list(size = 14))
            ),
            legend = list(orientation = "h", y = -0.1)
          )
        return(p)
      }
      
      if (area_item %in% c("__ALL__","Tout")) {
        df <- emissions_for(sc); req(nrow(df) > 0)
        if (!isTRUE(input$include_luc)) {
          df <- df %>% dplyr::filter(tolower(Item) != "land use change")
        }
        df$Phase <- factor(df$Phase, levels = c("Année de base", sc))
        
        pal <- emissions_colors_for(sort(unique(df$Item)))
        p <- plotly::plot_ly()
        items_ord <- sort(unique(df$Item))
        
        for (itm in items_ord) {
          sub <- df[df$Item == itm, , drop = FALSE]
          col_it <- if (!is.null(pal[[itm]]) && !is.na(pal[[itm]])) pal[[itm]] else "#999999"
          
          pos <- sub; pos$value <- pmax(pos$value, 0)
          p <- plotly::add_bars(
            p, data = pos, x = ~Phase, y = ~value,
            name = itm, legendgroup = itm, showlegend = TRUE,
            marker = list(color = col_it),
            customdata = sub$value,
            hovertemplate = paste0("<b>", itm, "</b><br>%{x} : %{customdata:.0f}<extra></extra>")
          )
          
          neg <- sub; neg$value <- pmin(neg$value, 0)
          p <- plotly::add_bars(
            p, data = neg, x = ~Phase, y = ~value,
            name = itm, legendgroup = itm, showlegend = FALSE,
            marker = list(color = col_it),
            customdata = sub$value,
            hovertemplate = paste0("<b>", itm, "</b><br>%{x} : %{customdata:.0f}<extra></extra>")
          )
        }
        
        return(plotly::layout(
          p,
          font    = list(family = "Inter, Segoe UI, Roboto, Helvetica, Arial, sans-serif"),
          barmode = "relative",
          xaxis   = list(title = ""),
          yaxis   = list(title = "Émissions"),
          legend  = list(orientation = "h", y = -0.2),
          bargap  = 0.25
        ))
      }
      
      plotly::plot_ly() %>%
        plotly::layout(
          font = list(family = "Inter, Segoe UI, Roboto, Helvetica, Arial, sans-serif"),
          annotations = list(list(
            text = "Pas de détail pour cet item.",
            showarrow = FALSE, x = 0.5, y = 0.5, xref = "paper", yref = "paper"
          )),
          xaxis = list(visible = FALSE), yaxis = list(visible = FALSE)
        )
    })
  })
}

