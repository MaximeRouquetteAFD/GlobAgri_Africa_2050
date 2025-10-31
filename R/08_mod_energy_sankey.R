# R/08_mod_energy_sankey.R
# ------------------------------------------------------------------------------
# Sankey des flux énergétiques — affichage conditionnel :
# - Tant qu'aucune carte n'est sélectionnée -> rien n'est rendu (0px de hauteur)
# - Dès qu'un scénario est choisi -> titre + plotly + lien CSV
# ------------------------------------------------------------------------------

mod_energy_sankey_ui <- function(id){
  ns <- NS(id)
  # Conteneur dynamique : si NULL côté server, il n'occupe AUCUNE place
  uiOutput(ns("block"))
}

mod_energy_sankey_server <- function(
    id,
    fact,
    r_country,
    r_selected_scenario,             # scénario cliqué (module 07)
    unit_label = "Gcal",
    plot_height = "540px"            # hauteur quand visible
){
  moduleServer(id, function(input, output, session){
    
    # opérateur %||%
    if (!exists("%||%", mode = "function")) {
      `%||%` <- function(a, b) {
        if (is.null(a) || length(a) == 0 || (is.numeric(a) && !is.finite(a))) b else a
      }
    }
    
    # Éléments requis (même set que précédemment)
    needed_elements <- c(
      "Energy Production",
      "Energy Import Quantity",
      "Energy Export Quantity",
      "Energy Domestic supply quantity",
      "Energy Feed","Energy Food","Energy Losses","Energy Seed",
      "Energy Other uses (non-food)"
    )
    
    # === Bloc UI conditionnel ==================================================
    output$block <- renderUI({
      sc <- r_selected_scenario()
      if (is.null(sc) || !nzchar(sc)) return(NULL)
      
      ns <- session$ns
      tagList(
        br(),  # l'espace n'apparaît que si visible
        h3("Flux énergétiques"),
        plotly::plotlyOutput(ns("sankey"), height = plot_height),
        
        # Barre d'actions (alignée à droite) : lien CSV
        div(class = "text-right",
            div(class = "u-actions",
                downloadLink(ns("dl_csv"), label = tagList(icon("download"), "CSV"))
            )
        )
      )
    })
    
    # --- Fonction interne : assemble les données (nodes/links) ----------------
    make_sankey_data <- reactive({
      sc  <- r_selected_scenario(); req(nzchar(sc))
      reg <- r_country();           req(nzchar(reg))
      
      dat <- fact %>%
        dplyr::filter(Region == reg, Scenario == sc, Element %in% needed_elements)
      
      year_used <- dat %>%
        dplyr::filter(Element == "Energy Domestic supply quantity", !is.na(Value)) %>%
        dplyr::summarise(year_used = suppressWarnings(max(Year, na.rm = TRUE))) %>%
        dplyr::pull(year_used)
      validate(need(is.finite(year_used), "Aucune année disponible pour ce scénario."))
      
      tot <- dat %>%
        dplyr::filter(Year == year_used) %>%
        dplyr::group_by(Element) %>%
        dplyr::summarise(val = sum(Value, na.rm = TRUE), .groups = "drop") %>%
        tidyr::pivot_wider(names_from = Element, values_from = val, values_fill = 0)
      
      g  <- function(nm) as.numeric(tot[[nm]] %||% 0)
      P  <- g("Energy Production")
      M  <- g("Energy Import Quantity")
      E  <- g("Energy Export Quantity")
      DS <- g("Energy Domestic supply quantity")
      feed <- g("Energy Feed"); food <- g("Energy Food")
      losses <- g("Energy Losses"); seed <- g("Energy Seed")
      other  <- g("Energy Other uses (non-food)")
      
      # Décomposition simple des flux
      exp_from_prod     <- pmin(P, E)
      exp_from_imp      <- pmax(0, E - exp_from_prod)
      to_dom_from_prod  <- pmax(0, P - exp_from_prod)
      to_dom_from_imp   <- pmax(0, M - exp_from_imp)
      
      DS_calc <- to_dom_from_prod + to_dom_from_imp
      if (is.finite(DS) && abs(DS_calc - DS) > 1e-6) DS <- DS_calc
      
      uses_sum <- feed + food + losses + seed + other
      residual <- DS - uses_sum
      if (is.finite(residual) && residual > 0) losses <- losses + residual
      
      # Libellés de nœuds (doivent matcher la palette de R/02)
      nodes <- c("Production","Importations","Exportations","Offre intérieure",
                 "Feed","Food","Losses","Seed","Other uses (non-food)")
      id <- stats::setNames(seq_along(nodes) - 1L, nodes)
      
      # Liens (source/target/valeur)
      src <- c(id["Production"], id["Importations"],
               id["Production"], id["Importations"],
               id["Offre intérieure"], id["Offre intérieure"], id["Offre intérieure"],
               id["Offre intérieure"], id["Offre intérieure"])
      trg <- c(id["Exportations"], id["Exportations"],
               id["Offre intérieure"], id["Offre intérieure"],
               id["Feed"], id["Food"], id["Losses"], id["Seed"], id["Other uses (non-food)"])
      val <- c(exp_from_prod, exp_from_imp,
               to_dom_from_prod, to_dom_from_imp,
               feed, food, losses, seed, other)
      
      keep <- val > 0
      src  <- src[keep]; trg <- trg[keep]; val <- val[keep]
      
      # Position X fixe de quelques nœuds (facultatif)
      x_map <- c("Production"=0.05, "Importations"=0.05,
                 "Exportations"=0.50, "Offre intérieure"=0.50,
                 "Feed"=0.93, "Food"=0.93, "Losses"=0.93, "Seed"=0.93,
                 "Other uses (non-food)"=0.93)
      node_x <- unname(x_map[nodes])
      
      list(
        year_used = year_used,
        nodes     = nodes,
        node_x    = node_x,
        src       = src,
        trg       = trg,
        val       = val,
        totals    = list(E = E, DS = DS),
        unit      = unit_label
      )
    })
    
    # === Plotly Sankey ========================================================
    output$sankey <- plotly::renderPlotly({
      sd  <- make_sankey_data()
      nds <- sd$nodes
      src <- sd$src; trg <- sd$trg; val <- sd$val
      
      # Couleurs de nœuds centralisées + garde-fous
      node_cols <- if (exists("sankey_node_colors_for", inherits = TRUE)) {
        tryCatch({
          cols <- sankey_node_colors_for(nds)
          if (length(cols) != length(nds) || any(!nzchar(cols) | is.na(cols))) {
            rep("#CCCCCC", length(nds))
          } else cols
        }, error = function(e) rep("#CCCCCC", length(nds)))
      } else rep("#CCCCCC", length(nds))
      
      # Couleurs de liens : helper si dispo, sinon dérivé de la couleur source
      link_cols <- tryCatch({
        if (exists("sankey_link_colors_from_src", inherits = TRUE)) {
          sankey_link_colors_from_src(nds[src + 1], alpha = 0.35)
        } else {
          base <- node_cols[src + 1]
          # passe en rgba(…,0.35)
          to_rgba <- function(hex, a = 0.35){
            if (is.na(hex) || !nzchar(hex)) return(sprintf("rgba(204,204,204,%.2f)", a))
            r <- strtoi(substr(hex, 2, 3), 16)
            g <- strtoi(substr(hex, 4, 5), 16)
            b <- strtoi(substr(hex, 6, 7), 16)
            sprintf("rgba(%d,%d,%d,%.2f)", r, g, b, a)
          }
          vapply(base, to_rgba, character(1))
        }
      }, error = function(e) rep("rgba(204,204,204,0.5)", length(src)))
      if (length(link_cols) != length(src)) link_cols <- rep("rgba(204,204,204,0.5)", length(src))
      
      # Labels de hover (avec %)
      E  <- sd$totals$E
      DS <- sd$totals$DS
      denom <- ifelse(trg == match("Exportations", nds) - 1L, E, DS)
      pct   <- ifelse(denom > 0, val / denom, NA_real_)
      link_label <- paste0(
        nds[src + 1], " → ", nds[trg + 1],
        "<br>", format(round(val), big.mark=" ", scientific=FALSE), " ", sd$unit,
        ifelse(is.finite(pct), paste0(" (", scales::percent(pct, accuracy=0.1), ")"), "")
      )
      
      plotly::plot_ly(
        type = "sankey",
        arrangement = "snap",
        node = list(
          label = nds,
          color = node_cols,
          x = sd$node_x,
          pad = 24, thickness = 32,
          line = list(color = "rgba(0,0,0,0.25)", width = 0.5)
        ),
        link = list(
          source = src, target = trg, value = val,
          color = link_cols, hovertemplate = link_label
        )
      ) %>%
        plotly::layout(margin = list(l=10,r=10,t=10,b=10), font = list(size=12)) %>%
        plotly::config(displaylogo = FALSE)
    })
    
    # === Export CSV : table des liens (source, target, value, % du total) =====
    output$dl_csv <- downloadHandler(
      filename = function(){
        sc <- r_selected_scenario() %||% "scenario"
        rg <- r_country()           %||% "pays"
        sprintf("sankey_%s_%s.csv", gsub("\\s+","_", rg), gsub("\\s+","_", sc))
      },
      content = function(file){
        sd  <- make_sankey_data()
        nds <- sd$nodes
        src <- sd$src; trg <- sd$trg; val <- sd$val
        E   <- sd$totals$E
        DS  <- sd$totals$DS
        
        denom <- ifelse(trg == match("Exportations", nds) - 1L, E, DS)
        pct   <- ifelse(denom > 0, val / denom, NA_real_)
        
        out <- tibble::tibble(
          Pays     = r_country(),
          Scenario = r_selected_scenario(),
          Annee    = sd$year_used,
          Source   = nds[src + 1],
          Cible    = nds[trg + 1],
          Valeur   = val,
          Unite    = sd$unit,
          Part     = pct
        )
        readr::write_delim(out, file, delim = ";")
      }
    )
  })
}
