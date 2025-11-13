# R/15_mod_ls_sankey_tonnes.R
# -------------------------------------------------------------------
# KPI tiles + Sankey for bovine products in TONNES
#   - items: Dairy, Bovine meat
#   - elements: Production / Import Quantity / Export Quantity /
#               Domestic supply quantity / Food / Feed / Losses / Seed /
#               Other uses (non-food)
#   - 'fact' stores values in 1000 tonnes -> converted to tonnes (× 1000)
# -------------------------------------------------------------------

mod_ls_sankey_tonnes_ui <- function(id, plot_height = "440px"){
  ns <- NS(id)
  tagList(
    div(class = "card",
        div(class = "card-body",
            # --- Filtres -------------------------------------------------------
            div(
              tags$label("Product", `for` = ns("prod_sel"), class = "form-label mb-1"),
              selectInput(
                ns("prod_sel"), NULL,
                choices  = c("All bovine products", "Bovine meat", "Dairy"),
                selected = "All bovine products",
                width    = "220px"
              )
            ),
            # --- Titre + tuiles KPI -------------------------------------------
            h3(textOutput(ns("title_domestic"))),
            tags$div(style="height:24px"),
            uiOutput(ns("tiles")),
            tags$div(style="height:28px"),
            
            # --- Titre Sankey + toggle % --------------------------------------
            h3(textOutput(ns("title_flow"))),
            div(class = "d-flex gap-3 flex-wrap align-items-center",
                div(class = "ms-auto",
                    checkboxInput(ns("as_pct"), "Afficher en pourcentage (%)",
                                  value = FALSE, width = "auto"))
            ),
            
            # --- Plot + export -------------------------------------------------
            plotly::plotlyOutput(ns("sankey"), height = plot_height),
            div(class = "text-right",
                div(class = "u-actions",
                    downloadLink(ns("dl_csv"), label = tagList(icon("download"), "CSV"))
                )
            )
        )
    )
  )
}

mod_ls_sankey_tonnes_server <- function(id, fact, r_country){
  moduleServer(id, function(input, output, session){
    
    # -----------------------------------------------------------------------
    # Helpers
    # -----------------------------------------------------------------------
    `%||%` <- function(a, b) if (is.null(a) || length(a)==0 || (is.numeric(a) && !is.finite(a))) b else a
    
    if (!exists("plotly_theme_transparent", mode = "function")) {
      plotly_theme_transparent <- function(p = NULL){
        if (is.null(p)) return(NULL)
        plotly::layout(p, paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)")
      }
    }
    
    SCEN_TILES <- c("Même diète","Diète probable","Diète saine")
    ELEMENTS   <- c("Production","Import Quantity","Export Quantity",
                    "Domestic supply quantity","Feed","Food","Losses","Seed",
                    "Other uses (non-food)")
    
    r_selected <- reactiveVal(SCEN_TILES[1])
    
    # -----------------------------------------------------------------------
    # Sélection produits + titres
    # -----------------------------------------------------------------------
    items_selected <- reactive({
      switch(input$prod_sel,
             "Bovine meat" = "Bovine meat",
             "Dairy"       = "Dairy",
             c("Dairy","Bovine meat"))
    })
    
    label_selected <- reactive({
      switch(input$prod_sel,
             "Bovine meat" = "bovine meat",
             "Dairy"       = "dairy products",
             "bovine products")
    })
    
    output$title_domestic <- renderText({
      paste0("Domestic supply of ", label_selected(), " (tonnes)")
    })
    output$title_flow <- renderText({
      paste0("Flow of ", label_selected())
    })
    
    # -----------------------------------------------------------------------
    # KPI (tonnes + delta vs Année de base)
    # -----------------------------------------------------------------------
    sum_by_scen <- function(sc, country){
      fact %>%
        dplyr::filter(Region == !!country,
                      Scenario == !!sc,
                      Item %in% items_selected(),
                      Element == "Domestic supply quantity") %>%
        dplyr::summarise(val = sum(Value, na.rm = TRUE), .groups = "drop") %>%
        dplyr::pull(val) %||% 0
    }
    
    baseline_value <- reactive({
      (sum_by_scen("Année de base", r_country()) * 1000) %||% 0
    })
    
    tiles_values <- reactive({
      rg <- r_country()
      setNames(lapply(SCEN_TILES, function(sc) sum_by_scen(sc, rg) * 1000), SCEN_TILES)
    })
    
    tiles_deltas <- reactive({
      base <- baseline_value()
      vals <- tiles_values()
      setNames(lapply(vals, function(v) if (is.finite(base) && base>0) 100*(v-base)/base else NA_real_), names(vals))
    })
    
    output$tiles <- renderUI({
      ns <- session$ns
      vals <- tiles_values(); dlt <- tiles_deltas(); sel <- r_selected()
      
      make_tile <- function(sc_label){
        id <- paste0("tile_", gsub("\\s+","_", sc_label))
        is_active <- identical(sel, sc_label)
        tags$div(class = "u-box",
                 tags$button(
                   id = ns(id), type = "button",
                   class = paste("u-card u-card--clickable u-card--focus", if (is_active) "is-active"),
                   onclick = sprintf("Shiny.setInputValue('%s', Date.now());", ns(id)),
                   div(class = "u-title", sc_label),
                   div(class = "u-value",
                       format(round(vals[[sc_label]] %||% 0), big.mark = " ", scientific = FALSE),
                       tags$span(class = "u-unit", "tonnes")),
                   {
                     dv <- dlt[[sc_label]]; cls <- if (is.finite(dv) && dv >= 0) "up" else "down"
                     div(class = "u-sub",
                         HTML(sprintf("Vs Année de base : <span class='%s'>%s</span>",
                                      cls,
                                      if (is.finite(dv)) scales::percent(dv/100, accuracy = 0.1) else "—")))
                   }
                 )
        )
      }
      div(class = "u-row", lapply(SCEN_TILES, make_tile))
    })
    
    observeEvent(input$tile_Même_diète,     ignoreInit = TRUE, { r_selected("Même diète") })
    observeEvent(input$tile_Diète_probable, ignoreInit = TRUE, { r_selected("Diète probable") })
    observeEvent(input$tile_Diète_saine,    ignoreInit = TRUE, { r_selected("Diète saine") })
    
    # -----------------------------------------------------------------------
    # Données Sankey (toujours en TONNES pour le tracé)
    # -----------------------------------------------------------------------
    make_sankey_data <- reactive({
      sc  <- r_selected();  req(nzchar(sc))
      reg <- r_country();   req(nzchar(reg))
      as_pct <- isTRUE(input$as_pct)
      
      dat0 <- fact %>%
        dplyr::filter(Region  == reg,
                      Scenario == sc,
                      Item %in% items_selected(),
                      Element %in% ELEMENTS)
      
      year_used <- dat0 %>%
        dplyr::filter(Element == "Domestic supply quantity", is.finite(Value)) %>%
        dplyr::summarise(year_used = suppressWarnings(max(Year, na.rm = TRUE))) %>%
        dplyr::pull(year_used)
      validate(need(is.finite(year_used), "Aucune année disponible pour ce scénario."))
      
      tot <- dat0 %>%
        dplyr::filter(Year == year_used) %>%
        dplyr::group_by(Element) %>%
        dplyr::summarise(val = sum(Value, na.rm = TRUE), .groups = "drop") %>%
        tidyr::pivot_wider(names_from = Element, values_from = val, values_fill = 0)
      
      g  <- function(nm) as.numeric(tot[[nm]] %||% 0)
      k  <- 1000
      P     <- k * g("Production")
      M     <- k * g("Import Quantity")
      E     <- k * g("Export Quantity")
      DS    <- k * g("Domestic supply quantity")
      feed  <- k * g("Feed")
      food  <- k * g("Food")
      losses<- k * g("Losses")
      seed  <- k * g("Seed")
      other <- k * g("Other uses (non-food)")
      
      # Décomposition des flux
      exp_from_prod    <- max(0, min(P, E))
      exp_from_imp     <- max(0, E - exp_from_prod)
      to_dom_from_prod <- max(0, P - exp_from_prod)
      to_dom_from_imp  <- max(0, M - exp_from_imp)
      
      DS_calc <- to_dom_from_prod + to_dom_from_imp
      if (is.finite(DS) && abs(DS_calc - DS) > 1e-6) DS <- DS_calc
      
      uses_sum <- feed + food + losses + seed + other
      residual <- DS - uses_sum
      
      # Tolérance pour les micro-écarts (1 tonne ici)
      tol <- 1  # 1 t
      
      if (is.finite(residual) && residual > tol) {
        # vrai écart significatif -> on l’ajoute aux pertes
        losses <- losses + residual
      } else {
        # sinon on ignore le résidu (considéré comme 0)
        residual <- 0
      }
      
      # CAS 1 / CAS 2 : avec ou sans exportations
      has_exports <- is.finite(E) && E > 0
      
      if (has_exports) {
        # ----- Structure avec EXPORTS -----
        nds <- c("Production","Imports","Exports","Domestic supply",
                 "Feed","Food","Losses","Seed","Other uses (non-food)")
        id <- stats::setNames(seq_along(nds) - 1L, nds)
        
        src <- c(id["Production"],   id["Imports"],
                 id["Production"],   id["Imports"],
                 id["Domestic supply"], id["Domestic supply"],
                 id["Domestic supply"], id["Domestic supply"],
                 id["Domestic supply"])
        
        trg <- c(id["Exports"], id["Exports"],
                 id["Domestic supply"], id["Domestic supply"],
                 id["Feed"], id["Food"], id["Losses"], id["Seed"],
                 id["Other uses (non-food)"])
        
        val_tonnes <- c(exp_from_prod, exp_from_imp,
                        to_dom_from_prod, to_dom_from_imp,
                        feed, food, losses, seed, other)
        
      } else {
        # ----- Structure SANS EXPORTS -----
        # On ne crée pas de nœud "Exports" -> évite les nœuds isolés
        nds <- c("Production","Imports","Domestic supply",
                 "Feed","Food","Losses","Seed","Other uses (non-food)")
        id <- stats::setNames(seq_along(nds) - 1L, nds)
        
        src <- c(id["Production"],   id["Imports"],
                 id["Domestic supply"], id["Domestic supply"],
                 id["Domestic supply"], id["Domestic supply"],
                 id["Domestic supply"])
        
        trg <- c(id["Domestic supply"], id["Domestic supply"],
                 id["Feed"], id["Food"], id["Losses"], id["Seed"],
                 id["Other uses (non-food)"])
        
        # ici to_dom_from_prod = P, to_dom_from_imp = M
        val_tonnes <- c(to_dom_from_prod, to_dom_from_imp,
                        feed, food, losses, seed, other)
      }
      
      # Supprime les liens nuls (avant d'ajouter l'ancre)
      keep <- val_tonnes > 0
      src <- src[keep]; trg <- trg[keep]; val_tonnes <- val_tonnes[keep]
      
      # ---- ANCRE pour forcer FOOD à droite ---------------------------------
      eps <- 1e-6
      nds  <- c(nds, "Food__anchor")
      id   <- stats::setNames(seq_along(nds) - 1L, nds)
      src  <- c(src, id["Food"])
      trg  <- c(trg, id["Food__anchor"])
      val_tonnes <- c(val_tonnes, eps)
      
      # Positionnement fixe des nœuds
      x_map <- c("Production"=0.05, "Imports"=0.05,
                 "Exports"=0.50, "Domestic supply"=0.50,
                 "Feed"=0.93, "Food"=0.93, "Losses"=0.93,
                 "Seed"=0.93, "Other uses (non-food)"=0.93,
                 "Food__anchor"=0.98)
      
      y_map <- c("Production"=0.20, "Imports"=0.70,
                 "Exports"=0.88, "Domestic supply"=0.40,
                 "Feed"=0.15, "Food"=0.55, "Losses"=0.70,
                 "Seed"=0.82, "Other uses (non-food)"=0.92,
                 "Food__anchor"=0.55)
      
      # % des liens : exports rapportés à E, le reste à DS
      if (has_exports) {
        trg_is_exports <- (trg == id["Exports"])
      } else {
        trg_is_exports <- rep(FALSE, length(trg))
      }
      denom_link <- ifelse(trg_is_exports, E, DS)
      pct_link   <- ifelse(denom_link > 0, val_tonnes / denom_link, NA_real_)
      
      list(
        nodes    = nds,
        src      = src,
        trg      = trg,
        val_t    = as.numeric(val_tonnes),
        pct_link = pct_link,
        node_x   = unname(x_map[nds]),
        node_y   = unname(y_map[nds]),
        totals   = list(
          P=P, M=M, E=E, DS=DS,
          feed=feed, food=food, losses=losses, seed=seed, other=other,
          in_DS_prod = to_dom_from_prod, in_DS_imp = to_dom_from_imp
        )
      )
    })
    
    
    # -----------------------------------------------------------------------
    # Plotly Sankey
    # -----------------------------------------------------------------------
    output$sankey <- plotly::renderPlotly({
      sd  <- make_sankey_data()
      nds <- sd$nodes
      src <- sd$src
      trg <- sd$trg
      val_t <- sd$val_t
      as_pct <- isTRUE(input$as_pct)
      
      stopifnot(is.numeric(val_t), length(val_t) == length(src))
      
      # Couleurs
      node_cols <- if (exists("sankey_node_colors_for", inherits = TRUE)) {
        cols <- try(sankey_node_colors_for(nds), silent = TRUE)
        if (inherits(cols, "try-error") || length(cols) != length(nds) ||
            any(!nzchar(cols) | is.na(cols))) rep("#CCCCCC", length(nds)) else cols
      } else rep("#CCCCCC", length(nds))
      
      # Rendre le noeud Food__anchor invisible (remplit transparent)
      anchor_node_idx <- which(nds == "Food__anchor")
      if (length(anchor_node_idx) == 1L) {
        node_cols[anchor_node_idx] <- "rgba(0,0,0,0)"
      }
      
      link_cols <- try({
        if (exists("sankey_link_colors_from_src", inherits = TRUE)) {
          sankey_link_colors_from_src(nds[src + 1], alpha = 0.35)
        } else {
          base <- node_cols[src + 1]
          to_rgba <- function(hex, a = 0.35){
            if (is.na(hex) || !nzchar(hex)) return(sprintf("rgba(204,204,204,%.2f)", a))
            r <- strtoi(substr(hex, 2, 3), 16)
            g <- strtoi(substr(hex, 4, 5), 16)
            b <- strtoi(substr(hex, 6, 7), 16)
            sprintf("rgba(%d,%d,%d,%.2f)", r, g, b, a)
          }
          vapply(base, to_rgba, character(1))
        }
      }, silent = TRUE)
      if (inherits(link_cols, "try-error") || length(link_cols) != length(src))
        link_cols <- rep("rgba(204,204,204,0.5)", length(src))
      
      # Labels nœuds
      fmt_t <- function(x) format(round(x), big.mark = " ", scientific = FALSE)
      P  <- sd$totals$P; M <- sd$totals$M; E <- sd$totals$E; DS <- sd$totals$DS
      uses <- sd$totals[c("feed","food","losses","seed","other")]
      in_DS_prod <- sd$totals$in_DS_prod
      in_DS_imp  <- sd$totals$in_DS_imp
      
      # % pour nœuds DS / E : base DS+E (somme = 100%)
      sum_DS_E <- DS + E
      share_DS <- if (sum_DS_E > 0) DS/sum_DS_E else NA_real_
      share_E  <- if (sum_DS_E > 0) E/sum_DS_E  else NA_real_
      
      pct_or_dash <- function(x, denom){
        if (is.finite(denom) && denom > 0) scales::percent(x/denom, accuracy = 0.1) else "—"
      }
      
      # Labels de nœuds dynamiques, alignés sur sd$nodes
      # Labels de nœuds dynamiques, alignés sur sd$nodes
      mk_node_label <- function(name){
        # On ne montre jamais Food__anchor
        if (name == "Food__anchor") return("")
        
        if (as_pct) {
          val_pct <- switch(
            name,
            "Production"        = pct_or_dash(in_DS_prod, DS),
            "Imports"           = pct_or_dash(in_DS_imp,  DS),
            "Exports"           = if (is.finite(share_E))  scales::percent(share_E,  accuracy = 0.1) else "—",
            "Domestic supply"   = if (is.finite(share_DS)) scales::percent(share_DS, accuracy = 0.1) else "—",
            "Feed"              = pct_or_dash(uses$feed,   DS),
            "Food"              = pct_or_dash(uses$food,   DS),
            "Losses"            = pct_or_dash(uses$losses, DS),
            "Seed"              = pct_or_dash(uses$seed,   DS),
            "Other uses (non-food)" = pct_or_dash(uses$other, DS),
            "—"
          )
          paste0(name, "<br><span>", val_pct, "</span>")
        } else {
          val_num <- switch(
            name,
            "Production"        = P,
            "Imports"           = M,
            "Exports"           = E,
            "Domestic supply"   = DS,
            "Feed"              = uses$feed,
            "Food"              = uses$food,
            "Losses"            = uses$losses,
            "Seed"              = uses$seed,
            "Other uses (non-food)" = uses$other,
            NA_real_
          )
          if (!is.finite(val_num)) return("")
          paste0(name, "<br><span>", fmt_t(val_num), " t</span>")
        }
      }
      
      
      # On applique la fonction à *chaque* nœud renvoyé par make_sankey_data()
      node_label <- vapply(nds, mk_node_label, character(1))
      
      
      # Hover des liens (en % du bon dénominateur + tonnes)
      link_label <- if (as_pct) {
        paste0(
          nds[src + 1], " → ", nds[trg + 1],
          "<br>", scales::percent(sd$pct_link, accuracy = 0.1),
          " (", fmt_t(val_t), " t)",
          "<extra></extra>"
        )
      } else {
        paste0(
          nds[src + 1], " → ", nds[trg + 1],
          "<br>", fmt_t(val_t), " t",
          ifelse(is.finite(sd$pct_link),
                 paste0(" (", scales::percent(sd$pct_link, accuracy = 0.1), ")"), ""),
          "<extra></extra>"
        )
      }
      
      # Rendre le *dernier* lien (ancre) invisible
      anchor_idx <- length(val_t)
      link_cols[anchor_idx]  <- "rgba(0,0,0,0)"
      link_label[anchor_idx] <- "<extra></extra>"
      
      
      # Tracé (toujours en tonnes)
      p <- plotly::plot_ly(
        type = "sankey",
        arrangement = "snap",
        node = list(
          label = node_label,
          color = node_cols,
          x = sd$node_x,
          y = sd$node_y,
          pad = 24, thickness = 32,
          line = list(color = "rgba(0,0,0,0.25)", width = 0.5)
        ),
        link = list(
          source = src,
          target = trg,
          value  = val_t,
          color  = link_cols,
          hovertemplate = link_label
        )
      ) %>%
        plotly::layout(
          margin = list(l=10, r=10, t=10, b=45),  # Vhanger b=** pour rajouter de la longueur au schéma 
          font   = list(size = 12)
        ) %>%
        plotly::config(displaylogo = FALSE)
      
      
      plotly_theme_transparent(p)
    })
    
    # -----------------------------------------------------------------------
    # Export CSV
    # -----------------------------------------------------------------------
    output$dl_csv <- downloadHandler(
      filename = function(){
        sprintf("sankey_%s_%s_%s.csv",
                gsub("\\s+","_", r_country() %||% "country"),
                gsub("\\s+","_", r_selected() %||% "scenario"),
                gsub("\\s+","_", input$prod_sel %||% "All"))
      },
      content = function(file){
        sd  <- make_sankey_data()
        nds <- sd$nodes; src <- sd$src; trg <- sd$trg
        out <- tibble::tibble(
          Country      = r_country(),
          Scenario     = r_selected(),
          Product      = input$prod_sel %||% "All bovine products",
          Source       = nds[src + 1],
          Target       = nds[trg + 1],
          Value_tonnes = sd$val_t,
          Share_ref    = sd$pct_link        # part du bon dénominateur (E pour exports, DS sinon)
        )
        readr::write_delim(out, file, delim = ";")
      }
    )
    
    return(list(selected_scenario = reactive(r_selected())))
  })
}
