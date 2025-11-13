# R/08_mod_energy_sankey.R
# ------------------------------------------------------------------------------
# Global energy-flow Sankey (crops + livestock) in Gcal
#   - fact contains energy elements:
#       "Energy Production", "Energy Import Quantity", "Energy Export Quantity",
#       "Energy Domestic supply quantity", "Energy Food", "Energy Feed",
#       "Energy Losses", "Energy Seed", "Energy Other uses (non-food)"
#   - Here we:
#       * keep only Item == "All" and Unit == "Gcal"
#       * drop the "Energy " prefix and work with base names
# ------------------------------------------------------------------------------

mod_energy_sankey_ui <- function(id){
  ns <- NS(id)
  uiOutput(ns("block"))
}

mod_energy_sankey_server <- function(
    id,
    fact,
    r_country,
    r_selected_scenario,
    unit_label = "Gcal",
    plot_height = "540px"
){
  moduleServer(id, function(input, output, session){
    
    # -------------------------------------------------------------------
    # Helpers
    # -------------------------------------------------------------------
    if (!exists("%||%", mode = "function")) {
      `%||%` <- function(a, b){
        if (is.null(a) || length(a) == 0 || (is.numeric(a) && !is.finite(a))) b else a
      }
    }
    if (!exists("plotly_theme_transparent", mode = "function")) {
      plotly_theme_transparent <- function(p = NULL){
        if (is.null(p)) return(NULL)
        plotly::layout(
          p,
          paper_bgcolor = "rgba(0,0,0,0)",
          plot_bgcolor  = "rgba(0,0,0,0)"
        )
      }
    }
    
    # Base elements (without the "Energy " prefix)
    base_elements <- c(
      "Production",
      "Import Quantity",
      "Export Quantity",
      "Domestic supply quantity",
      "Feed","Food","Losses","Seed",
      "Other uses (non-food)"
    )
    # Corresponding energy elements in fact$Element
    energy_elements <- paste("Energy", base_elements)
    
    # -------------------------------------------------------------------
    # Conditional UI (only when a scenario is selected)
    # -------------------------------------------------------------------
    output$block <- renderUI({
      sc <- r_selected_scenario()
      if (is.null(sc) || !nzchar(sc)) return(NULL)
      
      ns <- session$ns
      tagList(
        br(),
        div(
          class = "d-flex gap-3 flex-wrap align-items-center",
          h3("Total energy flows (crops and livestock)"),
          div(
            class = "ms-auto",
            checkboxInput(
              ns("as_pct"), "Show as percentage (%)",
              value = FALSE, width = "auto"
            )
          )
        ),
        plotly::plotlyOutput(ns("sankey"), height = plot_height),
        div(
          class = "text-right",
          div(
            class = "u-actions",
            downloadLink(ns("dl_csv"), label = tagList(icon("download"), "CSV"))
          )
        )
      )
    })
    
    # -------------------------------------------------------------------
    # Prepare Sankey data (always in Gcal for the trace)
    # -------------------------------------------------------------------
    make_sankey_data <- reactive({
      sc  <- r_selected_scenario(); req(nzchar(sc))
      reg <- r_country();           req(nzchar(reg))
      
      # Keep only energy lines for Item == "All" and unit_label (Gcal)
      dat <- fact %>%
        dplyr::filter(
          Region  == reg,
          Scenario == sc,
          Item     == "All",
          Unit     == unit_label,
          Element  %in% energy_elements
        ) %>%
        dplyr::mutate(
          Element_base = sub("^Energy\\s+", "", Element)  # drop "Energy "
        )
      
      year_used <- dat %>%
        dplyr::filter(Element_base == "Domestic supply quantity", is.finite(Value)) %>%
        dplyr::summarise(year_used = suppressWarnings(max(Year, na.rm = TRUE))) %>%
        dplyr::pull(year_used)
      validate(need(is.finite(year_used), "No year available for this scenario."))
      
      # Aggregate by base element (Production, Food, etc.)
      tot <- dat %>%
        dplyr::filter(Year == year_used) %>%
        dplyr::group_by(Element_base) %>%
        dplyr::summarise(val = sum(Value, na.rm = TRUE), .groups = "drop") %>%
        tidyr::pivot_wider(
          names_from  = Element_base,
          values_from = val,
          values_fill = 0
        )
      
      g  <- function(nm) as.numeric(tot[[nm]] %||% 0)
      P     <- g("Production")
      M     <- g("Import Quantity")
      E     <- g("Export Quantity")
      DS    <- g("Domestic supply quantity")
      feed  <- g("Feed")
      food  <- g("Food")
      losses<- g("Losses")
      seed  <- g("Seed")
      other <- g("Other uses (non-food)")
      
      # Flow decomposition
      exp_from_prod    <- max(0, min(P, E))
      exp_from_imp     <- max(0, E - exp_from_prod)
      to_dom_from_prod <- max(0, P - exp_from_prod)
      to_dom_from_imp  <- max(0, M - exp_from_imp)
      
      DS_calc <- to_dom_from_prod + to_dom_from_imp
      if (is.finite(DS) && abs(DS_calc - DS) > 1e-6) DS <- DS_calc
      
      uses_sum <- feed + food + losses + seed + other
      residual <- DS - uses_sum
      
      # Only keep "real" residuals, ignore numerical noise
      tol_resid <- 1   # 1 Gcal
      if (is.finite(residual) && residual > tol_resid) {
        losses <- losses + residual
      } else {
        residual <- 0
      }
      
      has_exports <- is.finite(E) && E > 0
      
      if (has_exports) {
        # ---- With Exports node ----------------------------------------
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
        
        val <- c(exp_from_prod, exp_from_imp,
                 to_dom_from_prod, to_dom_from_imp,
                 feed, food, losses, seed, other)
        
      } else {
        # ---- Without Exports node -------------------------------------
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
        
        val <- c(to_dom_from_prod, to_dom_from_imp,
                 feed, food, losses, seed, other)
      }
      
      # Remove tiny flows (< 1 Gcal) before adding the anchor
      tol_flow <- 1
      val[abs(val) < tol_flow] <- 0
      keep <- val > 0
      src  <- src[keep]; trg <- trg[keep]; val <- val[keep]
      
      # ---- Anchor to keep Food on the far right -----------------------
      eps <- 1e-6
      nds  <- c(nds, "Food__anchor")
      id   <- stats::setNames(seq_along(nds) - 1L, nds)
      src  <- c(src, id["Food"])
      trg  <- c(trg, id["Food__anchor"])
      val  <- c(val, eps)
      
      # Fixed node positions
      x_map <- c(
        "Production"       = 0.05,
        "Imports"          = 0.05,
        "Exports"          = 0.50,
        "Domestic supply"  = 0.50,
        "Feed"             = 0.93,
        "Food"             = 0.93,
        "Losses"           = 0.93,
        "Seed"             = 0.93,
        "Other uses (non-food)" = 0.93,
        "Food__anchor"     = 0.98
      )
      
      y_map <- c(
        "Production"       = 0.20,
        "Imports"          = 0.70,
        "Exports"          = 0.88,
        "Domestic supply"  = 0.40,
        "Feed"             = 0.15,
        "Food"             = 0.55,
        "Losses"           = 0.70,
        "Seed"             = 0.82,
        "Other uses (non-food)" = 0.92,
        "Food__anchor"     = 0.55
      )
      
      # Link shares: Exports flows / E, others / DS
      if (has_exports) {
        trg_is_exports <- (trg == id["Exports"])
      } else {
        trg_is_exports <- rep(FALSE, length(trg))
      }
      denom_link <- ifelse(trg_is_exports, E, DS)
      pct_link   <- ifelse(denom_link > 0, val / denom_link, NA_real_)
      
      list(
        year_used = year_used,
        nodes     = nds,
        src       = src,
        trg       = trg,
        val       = as.numeric(val),
        pct_link  = pct_link,
        node_x    = unname(x_map[nds]),
        node_y    = unname(y_map[nds]),
        totals    = list(
          P=P, M=M, E=E, DS=DS,
          feed=feed, food=food, losses=losses, seed=seed, other=other,
          in_DS_prod = to_dom_from_prod, in_DS_imp = to_dom_from_imp
        ),
        unit      = unit_label
      )
    })
    
    # -------------------------------------------------------------------
    # Plotly Sankey
    # -------------------------------------------------------------------
    output$sankey <- plotly::renderPlotly({
      sd  <- make_sankey_data()
      nds <- sd$nodes
      src <- sd$src
      trg <- sd$trg
      val <- sd$val
      as_pct <- isTRUE(input$as_pct)
      
      stopifnot(is.numeric(val), length(val) == length(src))
      
      # Node colours
      node_cols <- if (exists("sankey_node_colors_for", inherits = TRUE)) {
        cols <- try(sankey_node_colors_for(nds), silent = TRUE)
        if (inherits(cols, "try-error") || length(cols) != length(nds) ||
            any(!nzchar(cols) | is.na(cols))) rep("#CCCCCC", length(nds)) else cols
      } else rep("#CCCCCC", length(nds))
      
      # Make the anchor node fully invisible
      anchor_node_idx <- which(nds == "Food__anchor")
      if (length(anchor_node_idx) == 1L) {
        node_cols[anchor_node_idx] <- "rgba(0,0,0,0)"
      }
      
      # Link colours
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
      
      # Totals and helpers for labels
      fmt_v <- function(x) format(round(x), big.mark = " ", scientific = FALSE)
      P  <- sd$totals$P; M <- sd$totals$M; E <- sd$totals$E; DS <- sd$totals$DS
      uses <- sd$totals[c("feed","food","losses","seed","other")]
      in_DS_prod <- sd$totals$in_DS_prod
      in_DS_imp  <- sd$totals$in_DS_imp
      
      sum_DS_E <- DS + E
      share_DS <- if (sum_DS_E > 0) DS/sum_DS_E else NA_real_
      share_E  <- if (sum_DS_E > 0) E/sum_DS_E  else NA_real_
      
      pct_or_dash <- function(x, denom){
        if (is.finite(denom) && denom > 0) scales::percent(x/denom, accuracy = 0.1) else "—"
      }
      
      # --------- Labels displayed under the nodes ----------------------
      mk_node_display_label <- function(name){
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
          # Gcal mode: only show the node name
          name
        }
      }
      node_label <- vapply(nds, mk_node_display_label, character(1))
      
      # --------- Hover text for nodes (always shows Gcal) --------------
      mk_node_hover_text <- function(name){
        if (name == "Food__anchor") return("")
        
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
        
        if (as_pct) {
          paste0(
            "<b>", name, "</b><br>",
            "Share: ", val_pct, "<br>",
            fmt_v(val_num), " ", sd$unit
          )
        } else {
          paste0(
            "<b>", name, "</b><br>",
            fmt_v(val_num), " ", sd$unit,
            " (", val_pct, ")"
          )
        }
      }
      # Texte montré AU SURVOL des nœuds (Gcal + %)
      node_hover_text <- vapply(nds, mk_node_hover_text, character(1))
      
      # On met le texte directement dans customdata
      # et on laisse un texte vide pour le nœud d'ancrage
      node_customdata   <- ifelse(nds == "Food__anchor", "", node_hover_text)
      node_hovertemplate <- "%{customdata}<extra></extra>"
      
      
      
      # --------- Hover for links (share + Gcal) ------------------------
      link_label <- if (as_pct) {
        paste0(
          nds[src + 1], " → ", nds[trg + 1],
          "<br>", scales::percent(sd$pct_link, accuracy = 0.1),
          " (", fmt_v(val), " ", sd$unit, ")",
          "<extra></extra>"
        )
      } else {
        paste0(
          nds[src + 1], " → ", nds[trg + 1],
          "<br>", fmt_v(val), " ", sd$unit,
          ifelse(is.finite(sd$pct_link),
                 paste0(" (", scales::percent(sd$pct_link, accuracy = 0.1), ")"),
                 ""),
          "<extra></extra>"
        )
      }
      
      # Hide the last link (anchor)
      anchor_idx <- length(val)
      link_cols[anchor_idx]  <- "rgba(0,0,0,0)"
      link_label[anchor_idx] <- "<extra></extra>"
      
      # --------- Plot --------------------------------------------------
      p <- plotly::plot_ly(
        type = "sankey",
        arrangement = "snap",
        node = list(
          label         = node_label,
          color         = node_cols,
          x             = sd$node_x,
          y             = sd$node_y,
          pad           = 24,
          thickness     = 32,
          line          = list(color = "rgba(0,0,0,0.25)", width = 0.5),
          customdata    = node_customdata,
          hovertemplate = node_hovertemplate
        ),
        link = list(
          source        = src,
          target        = trg,
          value         = val,
          color         = link_cols,
          hovertemplate = link_label
        )
      ) %>%
        plotly::layout(
          margin = list(l = 0, r = 0, t = 0, b = 100),
          font   = list(size = 12)
        ) %>%
        plotly::config(displaylogo = FALSE)
      
      plotly_theme_transparent(p)
    })
    
    # -------------------------------------------------------------------
    # Export CSV — node data (not link flows)
    # -------------------------------------------------------------------
    output$dl_csv <- downloadHandler(
      filename = function(){
        sc <- r_selected_scenario() %||% "scenario"
        rg <- r_country()           %||% "country"
        sprintf("energy_nodes_%s_%s.csv",
                gsub("\\s+","_", rg), gsub("\\s+","_", sc))
      },
      content = function(file){
        sd  <- make_sankey_data()
        nds <- sd$nodes
        
        # Remove the technical anchor node
        node_names <- nds[nds != "Food__anchor"]
        
        P  <- sd$totals$P; M <- sd$totals$M; E <- sd$totals$E; DS <- sd$totals$DS
        uses <- sd$totals[c("feed","food","losses","seed","other")]
        in_DS_prod <- sd$totals$in_DS_prod
        in_DS_imp  <- sd$totals$in_DS_imp
        
        sum_DS_E <- DS + E
        share_DS <- if (sum_DS_E > 0) DS/sum_DS_E else NA_real_
        share_E  <- if (sum_DS_E > 0) E/sum_DS_E  else NA_real_
        
        pct_or_num <- function(x, denom){
          if (is.finite(denom) && denom > 0) x/denom else NA_real_
        }
        
        # Values per node (same as in node hover)
        node_values <- vapply(node_names, function(name){
          switch(
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
        }, numeric(1))
        
        # Shares used for the node labels:
        # - Production / Imports / Feed / Food / Losses / Seed / Other vs DS
        # - Domestic supply / Exports vs DS + E
        node_shares <- vapply(node_names, function(name){
          switch(
            name,
            "Production"        = pct_or_num(in_DS_prod, DS),
            "Imports"           = pct_or_num(in_DS_imp,  DS),
            "Exports"           = if (is.finite(share_E))  share_E  else NA_real_,
            "Domestic supply"   = if (is.finite(share_DS)) share_DS else NA_real_,
            "Feed"              = pct_or_num(uses$feed,    DS),
            "Food"              = pct_or_num(uses$food,    DS),
            "Losses"            = pct_or_num(uses$losses,  DS),
            "Seed"              = pct_or_num(uses$seed,    DS),
            "Other uses (non-food)" = pct_or_num(uses$other, DS),
            NA_real_
          )
        }, numeric(1))
        
        out <- tibble::tibble(
          Country   = r_country(),
          Scenario  = r_selected_scenario(),
          Year      = sd$year_used,
          Node      = node_names,
          Value     = node_values,
          Unit      = sd$unit,
          Share_ref = node_shares   # share vs DS or DS+E depending on the node
        )
        
        readr::write_delim(out, file, delim = ";")
      }
    )
  })
}


