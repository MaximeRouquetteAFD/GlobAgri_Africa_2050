# R/3.6_mod_ls_sankey_tonnes.R
# -------------------------------------------------------------------
# KPI tiles + Sankey for LIVESTOCK products in TONNES
#   - Product groups: meat (ruminants / monogastrics), dairy, eggs,
#     aquatic animal products...
#   - Elements: Production / Import Quantity / Export Quantity /
#               Domestic supply quantity / Food / Feed / Losses / Seed /
#               Other uses (non-food)
#   - 'fact' stores values in 1000 tonnes -> converted to tonnes (× 1000)
#   - Scenarios MUST come from r_scenarios() (codes, single source of truth)
# -------------------------------------------------------------------

# --------- Groupes de produits animaux -------------------------------------
LIVESTOCK_GROUPS <- list(
  "All animal products" = c(
    "Bovine meat",
    "Pork meat",
    "Poultry meat",
    "Small ruminant meat",
    "Dairy",
    "Eggs",
    "Aquatic animal products"
  ),
  "Meat (all species)" = c(
    "Bovine meat",
    "Pork meat",
    "Poultry meat",
    "Small ruminant meat",
    "Aquatic animal products"
  ),
  "Ruminant meat & dairy" = c(
    "Bovine meat",
    "Small ruminant meat",
    "Dairy"
  ),
  "Monogastric meat" = c(
    "Pork meat",
    "Poultry meat"
  ),
  "Bovine meat"             = "Bovine meat",
  "Pork meat"               = "Pork meat",
  "Poultry meat"            = "Poultry meat",
  "Small ruminant meat"     = "Small ruminant meat",
  "Dairy"                   = "Dairy",
  "Eggs"                    = "Eggs",
  "Aquatic animal products" = "Aquatic animal products"
)

LIVESTOCK_LABELS <- c(
  "All animal products"      = "animal products",
  "Meat (all species)"       = "animal meat",
  "Ruminant meat & dairy"    = "ruminant meat and dairy products",
  "Monogastric meat"         = "monogastric meat",
  "Bovine meat"              = "bovine meat",
  "Pork meat"                = "pork meat",
  "Poultry meat"             = "poultry meat",
  "Small ruminant meat"      = "small ruminant meat",
  "Dairy"                    = "dairy products",
  "Eggs"                     = "eggs",
  "Aquatic animal products"  = "aquatic animal products"
)

# ---------------------------------------------------------------------------
# UI (inchangée)
# ---------------------------------------------------------------------------
mod_ls_sankey_tonnes_ui <- function(id, plot_height = "500px"){
  ns <- NS(id)
  tagList(
    div(class = "card",
        div(class = "card-body",
            
            # --- Titre + tuiles KPI -----------------------------------------
            h2(textOutput(ns("title_domestic"))),
            tags$div(style="height:24px"),
            uiOutput(ns("tiles")),
            tags$div(style="height:28px"),
            
            # --- Filtres -----------------------------------------------------
            div(
              tags$label("Product group", `for` = ns("prod_sel"), class = "form-label mb-1"),
              selectInput(
                ns("prod_sel"), NULL,
                choices  = names(LIVESTOCK_GROUPS),
                selected = "All animal products",
                width    = "260px"
              )
            ),
            
            # --- Titre Sankey + toggle % ------------------------------------
            h2(textOutput(ns("title_flow"))),
            div(class = "d-flex gap-3 flex-wrap align-items-center",
                div(class = "ms-auto",
                    checkboxInput(ns("as_pct"), "Show as percentage (%)",
                                  value = FALSE, width = "auto"))
            ),
            
            # --- Plot + export ----------------------------------------------
            plotly::plotlyOutput(ns("sankey"), height = plot_height),
            div(class = "text-right",
                div(class = "u-actions",
                    downloadLink(ns("dl_csv"), label = tagList(icon("download"), "CSV"))
                )
            ),
            uiOutput(ns("note"))
        )
    )
  )
}

# ---------------------------------------------------------------------------
# Server
# ---------------------------------------------------------------------------
mod_ls_sankey_tonnes_server <- function(id, fact, r_country, r_scenarios){
  moduleServer(id, function(input, output, session){
    
    # -----------------------------------------------------------------------
    # Helpers
    # -----------------------------------------------------------------------
    `%||%` <- function(a, b) if (is.null(a) || length(a)==0 || (is.numeric(a) && !is.finite(a))) b else a
    
    # dépendances attendues (config scénarios)
    sc_code <- if (exists("scenario_code", mode = "function")) scenario_code else function(x) stringr::str_squish(as.character(x))
    sc_label <- if (exists("scenario_label", mode = "function")) scenario_label else function(x) sc_code(x)
    
    scen_base_year <- if (exists("SCENARIO_BASE_YEAR_CODE", inherits = TRUE)) get("SCENARIO_BASE_YEAR_CODE", inherits = TRUE) else sc_code("Année de base")
    scen_levels_default <- if (exists("SCENARIO_LEVELS_DEFAULT", inherits = TRUE)) get("SCENARIO_LEVELS_DEFAULT", inherits = TRUE) else NULL
    scen_ref <- if (exists("SCENARIO_REF_CODE", inherits = TRUE)) get("SCENARIO_REF_CODE", inherits = TRUE) else NULL
    
    # Elements (métier) inchangés
    ELEMENTS <- c("Production","Import Quantity","Export Quantity",
                  "Domestic supply quantity","Feed","Food","Losses","Seed",
                  "Other uses (non-food)")
    
    # -----------------------------------------------------------------------
    # Sélection produits + titres (inchangé)
    # -----------------------------------------------------------------------
    items_selected <- reactive({
      grp <- input$prod_sel %||% "All animal products"
      LIVESTOCK_GROUPS[[grp]] %||% LIVESTOCK_GROUPS[["All animal products"]]
    })
    
    label_selected <- reactive({
      grp <- input$prod_sel %||% "All animal products"
      LIVESTOCK_LABELS[[grp]] %||% "animal products"
    })
    
    output$title_domestic <- renderText({
      paste0("Domestic supply of ", label_selected(), " (tons)")
    })
    output$title_flow <- renderText({
      paste0("Flow of ", label_selected())
    })
    
    # -----------------------------------------------------------------------
    # NOUVEAU CALAGE SCENARIOS (contract)
    # - le module prend r_scenarios() (CODES) comme unique entrée
    # - ordre stable via SCENARIO_LEVELS_DEFAULT
    # - pas de fallback local / pas de logique extra ici
    # -----------------------------------------------------------------------
    r_scen_codes <- reactive({
      sc <- r_scenarios()
      sc <- sc_code(sc)
      sc[is.finite(match(sc, sc))] # no-op safe; keep vector
    })
    
    r_scen_levels <- reactive({
      sc <- r_scen_codes()
      req(length(sc) > 0)
      
      if (!is.null(scen_levels_default) && length(scen_levels_default) > 0) {
        # ordre stable centralisé + append si un code inattendu arrive
        ord <- c(intersect(scen_levels_default, sc), setdiff(sc, scen_levels_default))
      } else {
        ord <- sc
      }
      unique(ord)
    })
    
    # sélection courante (code)
    r_selected <- reactiveVal(NULL)
    
    observeEvent(r_scen_levels(), {
      avail <- r_scen_levels()
      if (length(avail) == 0) return()
      
      cur <- r_selected()
      if (!is.null(cur) && cur %in% avail) return()
      
      base <- sc_code(scen_base_year)
      ref  <- if (!is.null(scen_ref)) sc_code(scen_ref) else NA_character_
      
      pick <- if (base %in% avail) {
        base
      } else if (is.character(ref) && nzchar(ref) && ref %in% avail) {
        ref
      } else {
        avail[1]
      }
      
      r_selected(pick)
    }, ignoreInit = FALSE)
    
    
    observeEvent(input$tile_clicked, {
      code <- input$tile_clicked$code %||% ""
      code <- scenario_code(code)
      if (nzchar(code)) r_selected(code)
    }, ignoreInit = TRUE)
    
    
    # -----------------------------------------------------------------------
    # KPI (tonnes + delta vs baseline year) — calculs inchangés
    # -----------------------------------------------------------------------
    sum_by_scen <- function(sc, country){
      fact %>%
        dplyr::filter(
          Region == !!country,
          Scenario == !!sc,
          Item %in% items_selected(),
          Element == "Domestic supply quantity"
        ) %>%
        dplyr::summarise(val = sum(Value, na.rm = TRUE), .groups = "drop") %>%
        dplyr::pull(val) %||% 0
    }
    
    # clé scalaire pour bindCache (country + product group + scenarios list)
    key_tiles <- reactive({
      paste0(
        sc_code(r_country() %||% ""),
        "||", (input$prod_sel %||% ""),
        "||", paste(r_scen_levels(), collapse = "|")
      )
    })
    
    baseline_value <- reactive({
      req(r_country())
      (sum_by_scen(scen_base_year, r_country()) * 1000) %||% 0
    }) %>% bindCache(sc_code(r_country() %||% ""))
    
    tiles_values <- reactive({
      rg <- r_country(); req(rg)
      scs <- r_scen_levels()
      setNames(lapply(scs, function(sc) sum_by_scen(sc, rg) * 1000), scs)
    }) %>% bindCache(key_tiles())
    
    tiles_deltas <- reactive({
      base <- baseline_value()
      vals <- tiles_values()
      setNames(
        lapply(names(vals), function(sc){
          v <- vals[[sc]] %||% NA_real_
          if (identical(sc, scen_base_year)) return(NA_real_)
          if (is.finite(base) && base > 0 && is.finite(v)) 100 * (v - base) / base else NA_real_
        }),
        names(vals)
      )
    }) %>% bindCache(key_tiles())
    
    # -----------------------------------------------------------------------
    # Tuiles KPI — dynamiques, basées sur r_scenarios() (CODES)
    # Affichage via scenario_label(code) uniquement
    # -----------------------------------------------------------------------
    output$tiles <- renderUI({
      ns <- session$ns
      vals <- tiles_values()
      dlt  <- tiles_deltas()
      sel  <- r_selected()
      
      scen_list <- names(vals)
      if (length(scen_list) == 0) return(NULL)
      
      js_escape <- function(x){
        x <- gsub("\\\\", "\\\\\\\\", x)
        x <- gsub("'", "\\\\'", x)
        x
      }
      make_tile <- function(sc_code_i){
        sc_code_i <- scenario_code(sc_code_i)  # sécurité
        is_active <- identical(sel, sc_code_i)
        
        sc_lbl   <- scenario_label(sc_code_i)
        base_lbl <- scenario_label(scen_base_year)
        
        # JS payload robuste (déclenche même si on reclique la même tuile)
        payload <- jsonlite::toJSON(
          list(code = sc_code_i, nonce = as.numeric(Sys.time()) ),
          auto_unbox = TRUE
        )
        
        tags$div(
          class = "u-box",
          tags$button(
            type  = "button",
            class = paste(
              "u-card u-card--clickable u-card--focus",
              if (is_active) "is-active"
            ),
            onclick = sprintf(
              "Shiny.setInputValue('%s', %s, {priority:'event'});",
              ns("tile_clicked"),
              payload
            ),
            
            div(class = "u-title", sc_lbl),
            
            div(
              class = "u-value",
              format(round(vals[[sc_code_i]] %||% 0),
                     big.mark = " ", scientific = FALSE),
              tags$span(class = "u-unit", "tonnes")
            ),
            
            # Sous-ligne : toujours présente (même hauteur), mais invisible pour la baseline
            {
              if (identical(sc_code_i, scen_base_year)) {
                div(
                  class = "u-sub",
                  style = "visibility:hidden;",
                  htmltools::HTML("&nbsp;")
                )
              } else {
                dv  <- dlt[[sc_code_i]]
                cls <- if (is.finite(dv) && dv >= 0) "up" else "down"
                div(
                  class = "u-sub",
                  htmltools::HTML(sprintf(
                    "Vs %s : <span class='%s'>%s</span>",
                    base_lbl,
                    cls,
                    if (is.finite(dv)) scales::percent(dv/100, accuracy = 0.1) else "—"
                  ))
                )
              }
            }
          )
        )
      }
      
      div(class = "u-row", lapply(scen_list, make_tile))
    })
    
    # -----------------------------------------------------------------------
    # Données Sankey (TONNES) — INCHANGÉ, sauf scénario sélectionné = code
    # -----------------------------------------------------------------------
    key_sankey <- reactive({
      paste0(
        sc_code(r_country() %||% ""),
        "||", (r_selected() %||% ""),
        "||", (input$prod_sel %||% "")
      )
    })
    
    make_sankey_data <- reactive({
      sc  <- r_selected();  req(nzchar(sc))
      reg <- r_country();   req(nzchar(reg))
      
      dat0 <- fact %>%
        dplyr::filter(
          Region  == reg,
          Scenario == sc,
          Item %in% items_selected(),
          Element %in% ELEMENTS,
          # garder uniquement les lignes AGRÉGÉES (pas de System / Animal)
          (is.na(System) | System == "" | System == "NA" | trimws(System) == ""),
          (is.na(Animal) | Animal == "" | Animal == "NA" | trimws(Animal) == "")
        )
      
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
      
      # Tolérance pour les micro-écarts (1 tonne)
      tol <- 1
      if (is.finite(residual) && residual > tol) {
        losses <- losses + residual
      } else {
        residual <- 0
      }
      
      has_exports <- is.finite(E) && E > 0
      
      # ---------- Construction des liens (edges) ---------------------------
      edges <- tibble::tibble(
        from  = c("Production","Imports","Production","Imports",
                  rep("Domestic supply", 5)),
        to    = c("Exports","Exports","Domestic supply","Domestic supply",
                  "Feed","Food","Losses","Seed","Other uses (non-food)"),
        value = c(exp_from_prod, exp_from_imp,
                  to_dom_from_prod, to_dom_from_imp,
                  feed, food, losses, seed, other)
      )
      
      if (!has_exports) {
        edges <- edges %>% dplyr::filter(to != "Exports")
      }
      
      edges <- edges %>% dplyr::filter(value > 0)
      validate(need(nrow(edges) > 0, "No flow available for this product group and scenario."))
      
      nodes_present <- unique(c(edges$from, edges$to))
      if (is.finite(DS) && DS > 0 && !("Domestic supply" %in% nodes_present)) {
        nodes_present <- c(nodes_present, "Domestic supply")
      }
      if (!("Food" %in% nodes_present)) {
        nodes_present <- c(nodes_present, "Food")
      }
      
      node_order <- c("Production","Imports","Exports","Domestic supply",
                      "Feed","Food","Losses","Seed","Other uses (non-food)")
      nodes_core <- node_order[node_order %in% nodes_present]
      
      nds <- c(nodes_core, "Food__anchor")
      id  <- stats::setNames(seq_along(nds) - 1L, nds)
      
      src <- unname(id[edges$from])
      trg <- unname(id[edges$to])
      val_tonnes <- edges$value
      
      if (has_exports) {
        trg_is_exports <- edges$to == "Exports"
      } else {
        trg_is_exports <- rep(FALSE, nrow(edges))
      }
      denom_link_real <- ifelse(trg_is_exports, E, DS)
      pct_link <- ifelse(denom_link_real > 0, val_tonnes / denom_link_real, NA_real_)
      
      eps <- 1e-6
      src        <- c(src, id["Food"])
      trg        <- c(trg, id["Food__anchor"])
      val_tonnes <- c(val_tonnes, eps)
      pct_link   <- c(pct_link, NA_real_)
      
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
    }) %>% bindCache(key_sankey())
    
    # -----------------------------------------------------------------------
    # Plotly Sankey — STRICTEMENT INCHANGÉ
    # -----------------------------------------------------------------------
    output$sankey <- plotly::renderPlotly({
      sd  <- make_sankey_data()
      nds <- sd$nodes
      src <- sd$src
      trg <- sd$trg
      val_t <- sd$val_t
      as_pct <- isTRUE(input$as_pct)
      
      stopifnot(is.numeric(val_t), length(val_t) == length(src))
      
      th <- if (exists("get_plotly_tokens", mode = "function")) get_plotly_tokens() else list(
        font_color     = "#111827",
        muted_color    = "#6B7280",
        axis_linecolor = "rgba(0,0,0,.18)"
      )
      
      node_cols <- if (exists("sankey_node_colors_for", inherits = TRUE)) {
        cols <- try(sankey_node_colors_for(nds), silent = TRUE)
        if (inherits(cols, "try-error") || length(cols) != length(nds) ||
            any(!nzchar(cols) | is.na(cols))) rep("#CCCCCC", length(nds)) else cols
      } else rep("#CCCCCC", length(nds))
      
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
      
      fmt_t <- function(x) format(round(x), big.mark = " ", scientific = FALSE)
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
      
      mk_node_label <- function(name){
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
      
      node_label <- vapply(nds, mk_node_label, character(1))
      
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
      
      anchor_idx <- length(val_t)
      link_cols[anchor_idx]  <- "rgba(0,0,0,0)"
      link_label[anchor_idx] <- "<extra></extra>"
      
      node_border_col <- th$axis_linecolor %||% "rgba(0,0,0,0.25)"
      
      p <- plotly::plot_ly(
        type = "sankey",
        arrangement = "snap",
        domain = list(x = c(0, 1), y = c(0.20, 0.98)),
        node = list(
          label = node_label,
          color = node_cols,
          x = sd$node_x,
          y = sd$node_y,
          pad = 22,
          thickness = 32,
          line = list(color = node_border_col, width = 0.5)
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
          margin = list(l = 10, r = 30, t = 10, b = 45),
          font = list(size = 12, color = th$font_color %||% "#111827")
        ) %>%
        plotly::config(displaylogo = FALSE)
      
      if (exists("plotly_apply_global_theme", mode = "function")) {
        p <- plotly_apply_global_theme(p, bg = "transparent", grid = "none")
      }
      
      p
    })
    
    # -----------------------------------------------------------------------
    # Export CSV (scénarios = codes ; inchangé)
    # -----------------------------------------------------------------------
    output$dl_csv <- downloadHandler(
      filename = function(){
        sprintf("sankey_livestock_%s_%s_%s.csv",
                gsub("\\s+","_", r_country() %||% "country"),
                gsub("\\s+","_", r_selected() %||% "scenario"),
                gsub("\\s+","_", input$prod_sel %||% "All_animal_products"))
      },
      content = function(file){
        sd  <- make_sankey_data()
        nds <- sd$nodes; src <- sd$src; trg <- sd$trg
        out <- tibble::tibble(
          Country      = r_country(),
          Scenario     = r_selected(),
          Product      = input$prod_sel %||% "All animal products",
          Source       = nds[src + 1],
          Target       = nds[trg + 1],
          Value_tonnes = sd$val_t,
          Share_ref    = sd$pct_link
        )
        readr::write_delim(out, file, delim = ";")
      }
    )
    
    # -----------------------------------------------------------------------
    # Note explicative (inchangée)
    # -----------------------------------------------------------------------
    output$note <- renderUI({
      sd <- try(make_sankey_data(), silent = TRUE)
      if (inherits(sd, "try-error") || is.null(sd$nodes) || length(sd$nodes) == 0) {
        return(NULL)
      }
      
      txt <- glue::glue(
        "<p>
        This figure shows, for the selected country and product group,
        how <strong>livestock products</strong> (meat, dairy, eggs, aquatic animal products)
        flow through the agri-food system.<br>
        The tiles above the diagram indicate, for each diet scenario, the total
        <strong>domestic supply</strong> of the selected products in <strong>tonnes</strong>,
        together with the percentage change compared with the 2018 baseline.
        </p>
        <p>
        The Sankey diagram below details the same scenario: quantities (in tonnes)
        flowing from <em>Production</em> and <em>Imports</em> to <em>Domestic supply</em> and
        <em>Exports</em>, and then to the different final uses
        (<em>Food</em>, <em>Feed</em>, <em>Losses</em>, <em>Seed</em>,
        <em>Other uses (non-food)</em>).
        </p>
        <p>
        When the option <strong>\"Show as percentage (%)\"</strong> is ticked, node and link labels
        are expressed as shares of three reference poles:
        </p>
        <ul>
          <li><strong>Sources</strong> (left side): the percentages shown at
              <em>Production</em> and <em>Imports</em> indicate the share of
              domestic supply that comes from each source.</li>
          <li><strong>Market balance</strong> (middle): the percentage of quantities
              intended for the domestic supply or for export describes how total
              marketable quantities are split between internal and external use.</li>
          <li><strong>Uses</strong> (right side): the percentages at the nodes
              <em>Food</em>, <em>Feed</em>, <em>Losses</em>, <em>Seed</em> and
              <em>Other uses (non-food)</em> represent each use as a share of
              domestic supply.</li>
        </ul>
        <p>
        In all cases, the tooltip still displays the underlying volumes in
        <strong>tonnes</strong>, even when percentages are shown in the diagram.
        </p>"
      )
      
      htmltools::HTML(txt)
    })
    
    return(list(selected_scenario = reactive(r_selected())))
  })
}
