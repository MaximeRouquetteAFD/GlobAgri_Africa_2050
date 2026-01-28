# R/2.5_mod_crop_sankey_tonnes.R
# -------------------------------------------------------------------
# KPI tiles + Sankey for CROP products in TONNES *or* ENERGY (Gcal)
#   - Toggle unit: Energy (Gcal) / Mass (tonnes)  (default = Energy)
#   - Mass elements (fact): Production / Import Quantity / Export Quantity /
#                           Domestic supply quantity / Food / Feed / Losses /
#                           Seed / Other uses (non-food)
#   - Energy elements (fact):
#       Energy Domestic supply quantity
#       Energy Export Quantity
#       Energy Food
#       Energy Import Quantity
#       Energy Losses
#       Energy Other uses (non-food)
#       Energy Processing
#       Energy Production
#       Energy Unused
#
# SCENARIO RULES (project-wide):
# - Le module NE reconstruit aucune logique historique de scénarios (pas de harvested_core, pas de fallback).
# - Il utilise r_scenarios() (codes) comme source unique, et intersecte uniquement avec les scénarios réellement présents
#   dans fact pour le pays + produit + élément nécessaire (selon unité).
# - En interne: codes (fact$Scenario). En UI: scenario_label(code) uniquement.
# - Ordre: SCENARIO_LEVELS_DEFAULT pour factor(levels=...), puis filtrage.
# -------------------------------------------------------------------

suppressPackageStartupMessages({
  library(shiny)
  library(dplyr)
  library(tidyr)
  library(plotly)
  library(stringr)
  library(scales)
})

# --- Groupes de produits végétaux -------------------------------------------
CROP_GROUPS <- list(
  "All crop products" = c(
    "Cake Other Oilcrops","Fibers etc.","Fruits and vegetables","Grass",
    "Maize","Millet and Sorghum","Oil Other Oilcrops","Oilpalm fruit",
    "Olive Oil","Olives","Other Oilcrops","Other cereals",
    "Other plant products","Other products","Palm Products Oil",
    "Palmkernel Cake","Pulses","Rape and Mustard Cake","Rape and Mustard Oil",
    "Rape and Mustardseed","Rice","Roots and Tuber","Soyabean Cake",
    "Soyabean Oil","Soyabeans","Sugar plants and products",
    "Sunflowerseed","Sunflowerseed Cake","Sunflowerseed Oil","Wheat"
  ),
  "Cereals" = c("Maize", "Millet and Sorghum", "Other cereals", "Rice", "Wheat"),
  "Roots and tubers" = c("Roots and Tuber"),
  "Pulses" = c("Pulses"),
  "Oilcrops (incl. cakes & oils)" = c(
    "Cake Other Oilcrops","Oil Other Oilcrops","Oilpalm fruit","Olive Oil",
    "Olives","Other Oilcrops","Palm Products Oil","Palmkernel Cake",
    "Rape and Mustard Cake","Rape and Mustard Oil","Rape and Mustardseed",
    "Soyabean Cake","Soyabean Oil","Soyabeans","Sunflowerseed",
    "Sunflowerseed Cake","Sunflowerseed Oil"
  ),
  "Fruits & vegetables" = c("Fruits and vegetables"),
  "Sugar crops"         = c("Sugar plants and products"),
  "Grass & fodder"      = c("Grass"),
  "Fibres & other products" = c("Fibers etc.", "Other plant products", "Other products")
)

CROP_LABELS <- c(
  "All crop products"             = "crop products",
  "Cereals"                       = "cereals",
  "Roots and tubers"              = "roots and tubers",
  "Pulses"                        = "pulses",
  "Oilcrops (incl. cakes & oils)" = "oilcrops",
  "Fruits & vegetables"           = "fruits and vegetables",
  "Sugar crops"                   = "sugar crops",
  "Grass & fodder"                = "grass and fodder crops",
  "Fibres & other products"       = "fibre and other plant products"
)

order_scen_by_config <- function(x){
  x <- unique(as.character(x))
  x <- x[!is.na(x) & nzchar(x)]
  known   <- SCENARIO_LEVELS_DEFAULT[SCENARIO_LEVELS_DEFAULT %in% x]
  unknown <- setdiff(x, SCENARIO_LEVELS_DEFAULT)
  c(known, sort(unknown))
}

# ---------------------------------------------------------------------------
# UI
# ---------------------------------------------------------------------------

mod_crop_sankey_tonnes_ui <- function(id, plot_height = "500px"){
  ns <- NS(id)
  tagList(
    div(
      class = "card",
      div(
        class = "card-body",
      
        # --- Titre + tuiles KPI -----------------------------------------
        h2(textOutput(ns("title_domestic"))),
        tags$div(style="height:24px"),
        uiOutput(ns("tiles")),
        tags$div(style="height:28px"),
        
        # --- Filtres -----------------------------------------------------
        div(
          h2(textOutput(ns("title_flow"))),
          # Unit toggle (default Energy)
          radioButtons(
            ns("unit"),
            label    = NULL,
            choices  = c("Energy (Gcal)" = "energy", "Mass (tonnes)" = "mass"),
            selected = "energy",
            inline   = TRUE),
          tags$label("Product group", `for` = ns("prod_sel"), class = "form-label mb-1"),
          selectInput(
            ns("prod_sel"), NULL,
            choices  = names(CROP_GROUPS),
            selected = "All crop products",
            width    = "220px")
        ),
        
        # --- Titre Sankey + toggles -------------------------------------
        div(
          class = "d-flex gap-3 flex-wrap align-items-center",
          div(
            class = "ms-auto",
            checkboxInput(
              ns("as_pct"),
              "Show as percentage (%)",
              value = FALSE,
              width = "auto"
            )
          )
        ),
        
        # --- Plot + export ----------------------------------------------
        plotly::plotlyOutput(ns("sankey"), height = plot_height),
        div(
          class = "text-right",
          div(
            class = "u-actions",
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

mod_crop_sankey_tonnes_server <- function(
    id,
    fact,
    r_country,
    r_scenarios,         # reactive/function returning scenario CODES (fact$Scenario)
    harvest_element = "Area harvested",
    exclude_items = c(
      "All products","All crops","Agricultural land occupation (Farm)",
      "Cropland","Forest land","Land under perm. meadows and pastures"
    ),
    value_multiplier = 1,            # applied to BOTH unit systems
    value_multiplier_energy = 1,     # extra multiplier for energy if needed
    group_var = NULL
){
  moduleServer(id, function(input, output, session){
    
    # -----------------------------------------------------------------------
    # Helpers
    # -----------------------------------------------------------------------
    `%||%` <- function(a, b) if (is.null(a) || length(a)==0 || (is.numeric(a) && !is.finite(a))) b else a
    
    if (is.null(r_scenarios) || !is.function(r_scenarios)) {
      stop("mod_crop_sankey_tonnes_server(): 'r_scenarios' must be provided as a reactive/function returning scenario CODES.")
    }
    if (!exists("scenario_label", mode = "function", inherits = TRUE)) {
      stop("mod_crop_sankey_tonnes_server(): missing dependency 'scenario_label(code)'.")
    }
    if (!exists("SCENARIO_LEVELS_DEFAULT", inherits = TRUE)) {
      stop("mod_crop_sankey_tonnes_server(): missing dependency 'SCENARIO_LEVELS_DEFAULT'.")
    }
    
    scenario_label_vec <- function(x){
      x <- as.character(x)
      vapply(x, scenario_label, character(1))
    }
    
    safe_id <- function(x){
      x <- as.character(x)
      x <- gsub("[^A-Za-z0-9_]+", "_", x)
      x <- gsub("_+", "_", x)
      x
    }
    
    if (!exists("plotly_theme_transparent", mode = "function")) {
      plotly_theme_transparent <- function(p = NULL){
        if (is.null(p)) return(NULL)
        plotly::layout(p, paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)")
      }
    }
    
    # --- Elements by unit -----------------------------------------------
    ELEMENTS_BY_UNIT <- list(
      mass = c(
        "Production","Import Quantity","Export Quantity",
        "Domestic supply quantity","Feed","Food","Processing","Losses","Seed",
        "Other uses (non-food)",
        "Unused"                         # <-- AJOUT (tonnes)
      ),
      energy = c(
        "Energy Domestic supply quantity",
        "Energy Export Quantity",
        "Energy Food",
        "Energy Feed",
        "Energy Import Quantity",
        "Energy Losses",
        "Energy Other uses (non-food)",
        "Energy Processing",
        "Energy Production",
        "Energy Unused"
      )
    )
    
    FACTMAP <- list(
      mass = list(
        P  = "Production",
        M  = "Import Quantity",
        E  = "Export Quantity",
        DS = "Domestic supply quantity",
        uses = c("Feed","Food","Processing","Losses","Seed","Other uses (non-food)","Unused") # <-- AJOUT Unused
      ),
      energy = list(
        P  = "Energy Production",
        M  = "Energy Import Quantity",
        E  = "Energy Export Quantity",
        DS = "Energy Domestic supply quantity",
        uses = c("Energy Food","Energy Feed","Energy Losses","Energy Processing",
                 "Energy Other uses (non-food)","Energy Unused")
      )
    )
    
    NODEMAP <- list(
      mass = list(
        uses_nodes = c("Feed","Food","Processing","Losses","Seed","Other uses (non-food)","Unused"), # <-- AJOUT Unused
        uses_fact  = c("Feed","Food","Processing","Losses","Seed","Other uses (non-food)","Unused")
      ),
      energy = list(
        uses_nodes = c("Food","Feed","Losses","Processing","Other uses (non-food)","Unused"),
        uses_fact  = c("Energy Food","Energy Feed","Energy Losses","Energy Processing",
                       "Energy Other uses (non-food)","Energy Unused")
      )
    )
    
    
    unit_mode <- reactive({
      u <- input$unit %||% "energy"
      if (!u %in% c("energy","mass")) u <- "energy"
      u
    })
    
    unit_label <- reactive({
      if (identical(unit_mode(), "energy")) "Gcal" else "tonnes"
    })
    
    unit_symbol <- reactive({
      if (identical(unit_mode(), "energy")) "Gcal" else "t"
    })
    
    unit_multiplier <- reactive({
      if (identical(unit_mode(), "energy")) {
        as.numeric(value_multiplier %||% 1) * as.numeric(value_multiplier_energy %||% 1)
      } else {
        # mass stored in 1000 tonnes in fact
        as.numeric(value_multiplier %||% 1) * 1000
      }
    })
    
    # --- Scénarios (codes) : ordre central puis filtrage --------------------
    scen_codes_ordered <- reactive({
      sc <- unique(as.character(r_scenarios()))
      sc <- sc[!is.na(sc) & nzchar(sc)]
      
      known   <- intersect(SCENARIO_LEVELS_DEFAULT, sc)
      unknown <- setdiff(sc, SCENARIO_LEVELS_DEFAULT)
      
      c(known, sort(unknown))
    })
    
    scen_levels_all <- reactive({
      sc <- scen_codes_ordered()
      c(SCENARIO_LEVELS_DEFAULT, setdiff(sc, SCENARIO_LEVELS_DEFAULT))
    })
    
    baseline_code <- reactive({
      b <- intersect(SCENARIO_LEVELS_DEFAULT, unique(as.character(r_scenarios())))
      b <- b[!is.na(b) & nzchar(b)]
      b[1] %||% NA_character_
    })
    
    # -----------------------------------------------------------------------
    # Sélection produits + titres
    # -----------------------------------------------------------------------
    items_selected <- reactive({
      grp <- input$prod_sel %||% "All crop products"
      CROP_GROUPS[[grp]] %||% CROP_GROUPS[["All crop products"]]
    })
    
    label_selected <- reactive({
      grp <- input$prod_sel %||% "All crop products"
      CROP_LABELS[[grp]] %||% "crop products"
    })
    
    output$title_domestic <- renderText({
      paste0("Domestic supply of ", label_selected(), " (", unit_label(), ")")
    })
    output$title_flow <- renderText({
      paste0("Flow of ", label_selected(), " (", unit_label(), ")")
    })
    
    # -----------------------------------------------------------------------
    # Scénario sélectionné (code)
    # -----------------------------------------------------------------------
    r_selected <- reactiveVal(NULL)
    
    # -----------------------------------------------------------------------
    # bindCache keys (scalaires)
    # -----------------------------------------------------------------------
    cache_key_years <- reactive({
      req(r_country(), input$prod_sel, unit_mode())
      paste0(
        "crop_sankey|years|",
        r_country(), "|prod=", input$prod_sel,
        "|unit=", unit_mode(),
        "|sc=", paste(scen_codes_ordered(), collapse = ",")
      )
    })
    
    cache_key_tiles <- reactive({
      req(r_country(), input$prod_sel, unit_mode())
      paste0(
        "crop_sankey|tiles|",
        r_country(), "|prod=", input$prod_sel,
        "|unit=", unit_mode(),
        "|sc=", paste(scen_codes_ordered(), collapse = ",")
      )
    })
    
    cache_key_sankey_data <- reactive({
      req(r_country(), input$prod_sel, unit_mode())
      paste0(
        "crop_sankey|sankeydata|",
        r_country(), "|prod=", input$prod_sel,
        "|unit=", unit_mode(),
        "|sel=", (r_selected() %||% ""),
        "|sc=", paste(scen_codes_ordered(), collapse = ",")
      )
    })
    
    cache_key_plot <- reactive({
      req(r_country(), input$prod_sel, unit_mode())
      paste0(
        "crop_sankey|plot|",
        r_country(), "|prod=", input$prod_sel,
        "|unit=", unit_mode(),
        "|sel=", (r_selected() %||% ""),
        "|pct=", isTRUE(input$as_pct)
      )
    })
    
    # -----------------------------------------------------------------------
    # Scénarios réellement disponibles (selon unité)
    # -----------------------------------------------------------------------
    years_by_scenario <- reactive({
      sc_req <- scen_codes_ordered()
      req(length(sc_req) > 0, r_country(), items_selected(), unit_mode())
      
      ds_el <- FACTMAP[[unit_mode()]]$DS
      
      fact %>%
        filter(
          Region   == r_country(),
          Scenario %in% sc_req,
          Item     %in% items_selected(),
          stringr::str_trim(Element) == ds_el
        ) %>%
        group_by(Scenario) %>%
        summarise(
          year_used = suppressWarnings(max(Year[is.finite(Value)], na.rm = TRUE)),
          .groups   = "drop"
        ) %>%
        filter(is.finite(year_used)) %>%
        mutate(
          Scenario_code = as.character(Scenario),
          Scenario      = factor(Scenario_code, levels = scen_levels_all())
        ) %>%
        arrange(Scenario) %>%
        select(Scenario_code, Scenario, year_used)
    }) %>% bindCache(cache_key_years())
    
    scen_available <- reactive({
      yrs <- years_by_scenario()
      yrs$Scenario_code %||% character(0)
    }) %>% bindCache(cache_key_years())
    
    observeEvent(list(scen_available(), unit_mode()), {
      avail <- order_scen_by_config(scen_available())
      if (length(avail) == 0) return()
      
      cur <- r_selected()
      if (is.null(cur) || !cur %in% avail) {
        r_selected(avail[1])
      }
    }, ignoreInit = FALSE)
    
    # Click unique pour les tuiles
    observeEvent(input$tile_click, {
      req(!is.null(input$tile_click$code))
      code <- as.character(input$tile_click$code)
      if (nzchar(code) && code %in% scen_available()) {
        r_selected(code)
      }
    }, ignoreInit = TRUE)
    
    # -----------------------------------------------------------------------
    # KPI (domestic supply + delta vs baseline) selon unité
    # -----------------------------------------------------------------------
    tiles_values <- reactive({
      rg   <- r_country()
      scs  <- order_scen_by_config(scen_available())
      req(nzchar(rg), length(scs) > 0, unit_mode())
      
      ds_el <- FACTMAP[[unit_mode()]]$DS
      mult  <- unit_multiplier()
      
      df <- fact %>%
        filter(
          Region   == rg,
          Scenario %in% scs,
          Item     %in% items_selected(),
          stringr::str_trim(Element) == ds_el
        ) %>%
        group_by(Scenario) %>%
        summarise(val_raw = sum(Value, na.rm = TRUE), .groups = "drop") %>%
        mutate(Scenario = as.character(Scenario)) %>%
        right_join(tibble::tibble(Scenario = scs), by = "Scenario") %>%
        mutate(val_raw = coalesce(val_raw, 0))
      
      vals <- setNames(as.list(df$val_raw * mult), df$Scenario) # tonnes or Gcal
      vals
    }) %>% bindCache(cache_key_tiles())
    
    baseline_value <- reactive({
      b <- baseline_code()
      vals <- tiles_values()
      
      if (is.na(b) || !nzchar(b)) return(NA_real_)
      v <- as.numeric(vals[[b]] %||% NA_real_)
      if (!is.finite(v)) return(NA_real_)
      v
    }) %>% bindCache(cache_key_tiles())
    
    tiles_deltas <- reactive({
      base <- baseline_value()
      vals <- tiles_values()
      
      setNames(
        lapply(vals, function(v){
          v <- as.numeric(v %||% NA_real_)
          if (is.finite(base) && base > 0 && is.finite(v)) 100 * (v - base) / base else NA_real_
        }),
        names(vals)
      )
    }) %>% bindCache(cache_key_tiles())
    
    # -----------------------------------------------------------------------
    # Tuiles KPI (affichent exactement r_scenarios() intersect données dispo)
    # -----------------------------------------------------------------------
    output$tiles <- renderUI({
      ns <- session$ns
      vals <- tiles_values()
      dlt  <- tiles_deltas()
      sel  <- r_selected()
      u_lab <- unit_label()
      
      scen_list <- order_scen_by_config(names(vals))
      if (length(scen_list) == 0) return(NULL)
      
      make_tile <- function(sc_code){
        sc_lab <- scenario_label(sc_code)
        is_active <- identical(sel, sc_code)
        base_code <- baseline_code()
        
        tags$div(
          class = "u-box",
          tags$button(
            type  = "button",
            class = paste(
              "u-card u-card--clickable u-card--focus",
              if (is_active) "is-active"
            ),
            onclick = sprintf(
              "Shiny.setInputValue('%s', {code:'%s', nonce:Date.now()}, {priority:'event'});",
              ns("tile_click"), sc_code
            ),
            div(class = "u-title", sc_lab),
            div(
              class = "u-value",
              format(round(as.numeric(vals[[sc_code]] %||% 0)),
                     big.mark = " ", scientific = FALSE),
              tags$span(class = "u-unit", u_lab)
            ),
            {
              if (identical(sc_code, base_code)) {
                div(class = "u-sub", HTML("&nbsp;"))
              } else {
                dv  <- as.numeric(dlt[[sc_code]] %||% NA_real_)
                cls <- if (is.finite(dv) && dv >= 0) "up" else "down"
                div(
                  class = "u-sub",
                  HTML(sprintf(
                    "Vs baseline : <span class='%s'>%s</span>",
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
    # Données Sankey (selon unité)
    # -----------------------------------------------------------------------
    make_sankey_data <- reactive({
      sc  <- r_selected(); req(nzchar(sc))
      reg <- r_country();  req(nzchar(reg))
      um  <- unit_mode();  req(um %in% c("mass","energy"))
      
      yrs <- years_by_scenario()
      year_used <- yrs$year_used[yrs$Scenario_code == sc][1] %||% NA
      validate(need(is.finite(year_used), "No elements configured for this unit."))
      
      mult <- unit_multiplier()
      
      elements_needed <- ELEMENTS_BY_UNIT[[um]] %||% character(0)
      validate(need(length(elements_needed) > 0, "No elements configured for this unit."))
      
      dat0 <- fact %>%
        filter(
          Region   == reg,
          Scenario == sc,
          Item     %in% items_selected(),
          stringr::str_trim(Element) %in% elements_needed,
          Year     == year_used,
          (is.na(System) | System == "" | System == "NA" | trimws(System) == ""),
          (is.na(Animal) | Animal == "" | Animal == "NA" | trimws(Animal) == "")
        )
      
      tot <- dat0 %>%
        mutate(Element = stringr::str_trim(Element)) %>%
        group_by(Element) %>%
        summarise(val = sum(Value, na.rm = TRUE), .groups = "drop") %>%
        tidyr::pivot_wider(names_from = Element, values_from = val, values_fill = 0)
      
      g  <- function(nm) as.numeric(tot[[nm]] %||% 0)
      
      P_raw  <- g(FACTMAP[[um]]$P)
      M_raw  <- g(FACTMAP[[um]]$M)
      E_raw  <- g(FACTMAP[[um]]$E)
      DS_raw <- g(FACTMAP[[um]]$DS)
      
      P  <- mult * P_raw
      M  <- mult * M_raw
      E  <- mult * E_raw
      DS <- mult * DS_raw
      
      # Uses
      uses_fact  <- NODEMAP[[um]]$uses_fact
      uses_nodes <- NODEMAP[[um]]$uses_nodes
      
      use_vals <- stats::setNames(rep(0, length(uses_nodes)), uses_nodes)
      for (i in seq_along(uses_nodes)) {
        use_vals[[uses_nodes[i]]] <- mult * g(uses_fact[i])
      }
      
      exp_from_prod    <- max(0, min(P, E))
      exp_from_imp     <- max(0, E - exp_from_prod)
      to_dom_from_prod <- max(0, P - exp_from_prod)
      to_dom_from_imp  <- max(0, M - exp_from_imp)
      
      DS_calc <- to_dom_from_prod + to_dom_from_imp
      if (is.finite(DS_calc) && is.finite(DS) && abs(DS_calc - DS) > 1e-6) DS <- DS_calc
      if (identical(um, "mass")) {
        
        uses_no_unused <- sum(use_vals[c("Feed","Food","Processing","Losses","Seed","Other uses (non-food)")], na.rm = TRUE)
        residual <- DS - uses_no_unused
        
        # Tolérance micro-écarts (1 tonne)
        tol <- 1
        if (is.finite(residual) && residual > tol) {
          use_vals[["Unused"]] <- residual
        } else {
          # si l'écart est très faible ou négatif, on ne force pas (évite flux négatifs)
          use_vals[["Unused"]] <- 0
        }
        
      } else {
        
        uses_no_unused <- sum(use_vals[c("Food","Feed","Losses","Processing","Other uses (non-food)")], na.rm = TRUE)
        residual <- DS - uses_no_unused
        use_vals[["Unused"]] <- max(0, residual)
      }
      
      has_exports <- is.finite(E) && E > 0
      
      edges <- tibble::tibble(
        from  = c("Production","Imports","Production","Imports", rep("Domestic supply", length(uses_nodes))),
        to    = c("Exports","Exports","Domestic supply","Domestic supply", uses_nodes),
        value = c(exp_from_prod, exp_from_imp, to_dom_from_prod, to_dom_from_imp, unname(use_vals[uses_nodes]))
      )
      
      if (!has_exports) {
        edges <- edges %>% filter(to != "Exports")
      }
      
      edges <- edges %>% filter(is.finite(value), value > 0)
      validate(need(nrow(edges) > 0, "No flow available for this product group and scenario."))
      
      nodes_present <- unique(c(edges$from, edges$to))
      if (is.finite(DS) && DS > 0 && !("Domestic supply" %in% nodes_present)) {
        nodes_present <- c(nodes_present, "Domestic supply")
      }
      if (!("Food" %in% nodes_present)) {
        nodes_present <- c(nodes_present, "Food")
      }
      
      node_order <- if (identical(um, "energy")) {
        c("Production","Imports","Exports","Domestic supply",
          "Food","Feed","Losses","Processing","Other uses (non-food)","Unused")
      } else {
        c("Production","Imports","Exports","Domestic supply",
          "Feed","Food","Processing","Losses","Seed","Other uses (non-food)","Unused")  # <-- AJOUT Unused
      }
      
      nodes_core <- node_order[node_order %in% nodes_present]
      
      # Ancre invisible pour Food à droite
      nds <- c(nodes_core, "Food__anchor")
      id  <- stats::setNames(seq_along(nds) - 1L, nds)
      
      src <- unname(id[edges$from])
      trg <- unname(id[edges$to])
      val_u <- edges$value
      
      # % links: exports relative to total exports; other links relative to domestic supply
      if (has_exports) {
        trg_is_exports <- edges$to == "Exports"
      } else {
        trg_is_exports <- rep(FALSE, nrow(edges))
      }
      denom_link_real <- ifelse(trg_is_exports, E, DS)
      pct_link <- ifelse(denom_link_real > 0, val_u / denom_link_real, NA_real_)
      
      # Lien d’ancre (Food -> Food__anchor), invisible
      eps <- 1e-6
      src   <- c(src, id["Food"])
      trg   <- c(trg, id["Food__anchor"])
      val_u <- c(val_u, eps)
      pct_link <- c(pct_link, NA_real_)
      
      # Positions fixes (mêmes repères, ajustés si energy)
      x_map <- c("Production"=0.05, "Imports"=0.05,
                 "Exports"=0.50, "Domestic supply"=0.50,
                 "Feed"=0.93, "Food"=0.93, "Losses"=0.93,
                 "Seed"=0.93, "Other uses (non-food)"=0.93,
                 "Processing"=0.93, "Unused"=0.93,
                 "Food__anchor"=0.98)
      
      y_map <- if (identical(um, "energy")) {
        c("Production"=0.20, "Imports"=0.70,
          "Exports"=0.88, "Domestic supply"=0.40,
          "Food"=0.22, "Feed"=0.10, "Losses"=0.45, "Processing"=0.62,
          "Other uses (non-food)"=0.78, "Unused"=0.92,
          "Food__anchor"=0.22)
      } else {
        c("Production"=0.20, "Imports"=0.70,
          "Exports"=0.88, "Domestic supply"=0.40,
          "Feed"=0.12, "Food"=0.48, "Processing"=0.62, "Losses"=0.72,
          "Seed"=0.82, "Other uses (non-food)"=0.90, "Unused"=0.96,  # <-- AJOUT Unused
          "Food__anchor"=0.48)
      }
      
      list(
        unit_mode      = um,
        unit_label     = unit_label(),
        unit_symbol    = unit_symbol(),
        scenario_code  = sc,
        scenario_label = scenario_label(sc),
        year_used      = year_used,
        nodes          = nds,
        src            = src,
        trg            = trg,
        val_u          = as.numeric(val_u),
        pct_link       = pct_link,
        node_x         = unname(x_map[nds]),
        node_y         = unname(y_map[nds]),
        totals         = list(
          P=P, M=M, E=E, DS=DS,
          uses = use_vals,
          in_DS_prod = to_dom_from_prod, in_DS_imp = to_dom_from_imp
        )
      )
    }) %>% bindCache(cache_key_sankey_data())
    
    # -----------------------------------------------------------------------
    # Plotly Sankey
    # -----------------------------------------------------------------------
    output$sankey <- plotly::renderPlotly({
      sd    <- make_sankey_data()
      nds   <- sd$nodes
      src   <- sd$src
      trg   <- sd$trg
      val_u <- sd$val_u
      as_pct <- isTRUE(input$as_pct)
      
      stopifnot(is.numeric(val_u), length(val_u) == length(src))
      
      th <- get_plotly_tokens()
      
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
      
      fmt_u <- function(x) format(round(x), big.mark = " ", scientific = FALSE)
      
      P  <- sd$totals$P; M <- sd$totals$M; E <- sd$totals$E; DS <- sd$totals$DS
      uses <- sd$totals$uses
      in_DS_prod <- sd$totals$in_DS_prod
      in_DS_imp  <- sd$totals$in_DS_imp
      u_sym <- sd$unit_symbol
      
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
            "Feed"              = pct_or_dash(uses[["Feed"]],   DS),
            "Food"              = pct_or_dash(uses[["Food"]],   DS),
            "Losses"            = pct_or_dash(uses[["Losses"]], DS),
            "Seed"              = pct_or_dash(uses[["Seed"]],   DS),
            "Other uses (non-food)" = pct_or_dash(uses[["Other uses (non-food)"]], DS),
            "Processing"        = pct_or_dash(uses[["Processing"]], DS),
            "Unused"            = pct_or_dash(uses[["Unused"]], DS),
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
            "Feed"              = uses[["Feed"]],
            "Food"              = uses[["Food"]],
            "Losses"            = uses[["Losses"]],
            "Seed"              = uses[["Seed"]],
            "Other uses (non-food)" = uses[["Other uses (non-food)"]],
            "Processing"        = uses[["Processing"]],
            "Unused"            = uses[["Unused"]],
            NA_real_
          )
          if (!is.finite(val_num)) return("")
          paste0(name, "<br><span>", fmt_u(val_num), " ", u_sym, "</span>")
        }
      }
      
      node_label <- vapply(nds, mk_node_label, character(1))
      
      link_label <- if (as_pct) {
        paste0(
          nds[src + 1], " → ", nds[trg + 1],
          "<br>", scales::percent(sd$pct_link, accuracy = 0.1),
          " (", fmt_u(val_u), " ", u_sym, ")",
          "<extra></extra>"
        )
      } else {
        paste0(
          nds[src + 1], " → ", nds[trg + 1],
          "<br>", fmt_u(val_u), " ", u_sym,
          ifelse(is.finite(sd$pct_link),
                 paste0(" (", scales::percent(sd$pct_link, accuracy = 0.1), ")"), ""),
          "<extra></extra>"
        )
      }
      
      # lien d'ancre invisible
      anchor_idx <- length(val_u)
      link_cols[anchor_idx]  <- "rgba(0,0,0,0)"
      link_label[anchor_idx] <- "<extra></extra>"
      
      p <- plotly::plot_ly(
        type = "sankey",
        arrangement = "snap",
        domain = list(x = c(0, 1), y = c(0.20, 0.98)),
        node = list(
          label = node_label,
          color = node_cols,
          x = sd$node_x,
          y = sd$node_y,
          pad = 25,
          thickness = 32,
          line = list(color = th$node_border, width = 0.5)
        ),
        link = list(
          source = src,
          target = trg,
          value  = val_u,
          color  = link_cols,
          hovertemplate = link_label
        )
      ) %>%
        plotly::layout(
          margin = list(l = 10, r = 30, t = 10, b = 45),
          font   = list(size = 12, color = th$font_color),
          paper_bgcolor = APP_TRANSPARENT,
          plot_bgcolor  = APP_TRANSPARENT,
          hoverlabel = list(
            bgcolor = th$hover_bg,
            font    = list(color = th$hover_font)
          )
        ) %>%
        plotly::config(displaylogo = FALSE)
      
      p <- plotly_apply_global_theme(p, bg = "transparent", grid = "none")
      p
    }) %>% bindCache(cache_key_plot())
    
    # -----------------------------------------------------------------------
    # Export CSV
    # -----------------------------------------------------------------------
    output$dl_csv <- downloadHandler(
      filename = function(){
        sprintf("sankey_%s_%s_%s_%s.csv",
                gsub("\\s+","_", r_country() %||% "country"),
                gsub("\\s+","_", scenario_label(r_selected() %||% "scenario")),
                gsub("\\s+","_", input$prod_sel %||% "All_crop_products"),
                unit_mode() %||% "unit")
      },
      content = function(file){
        sd  <- make_sankey_data()
        nds <- sd$nodes; src <- sd$src; trg <- sd$trg
        
        # Keep backward-compat columns for mass; add generic Value + Unit
        out <- tibble::tibble(
          Country        = r_country(),
          Scenario_code  = sd$scenario_code,
          Scenario_label = sd$scenario_label,
          Year           = sd$year_used,
          Product        = input$prod_sel %||% "All crop products",
          Unit           = sd$unit_label,
          Source         = nds[src + 1],
          Target         = nds[trg + 1],
          Value          = sd$val_u,
          Value_tonnes   = if (identical(sd$unit_mode, "mass")) sd$val_u else NA_real_,
          Value_Gcal     = if (identical(sd$unit_mode, "energy")) sd$val_u else NA_real_,
          Share_ref      = sd$pct_link
        )
        readr::write_delim(out, file, delim = ";")
      }
    )
    
    # -----------------------------------------------------------------------
    # Note explicative
    # -----------------------------------------------------------------------
    output$note <- renderUI({
      sd <- try(make_sankey_data(), silent = TRUE)
      if (inherits(sd, "try-error") || is.null(sd$nodes) || length(sd$nodes) == 0) {
        return(NULL)
      }
      
      unit_txt <- if (identical(sd$unit_mode, "energy")) "energy (Gcal)" else "mass (tonnes)"
      
      txt <- glue::glue(
        "<p>
    This figure shows, for the selected country and product group, how <strong>crop products</strong>
    flow through the agri-food system in <strong>{unit_txt}</strong>.<br>
    The tiles above the diagram indicate, for each scenario, the total <strong>domestic supply</strong>
    of the selected products in <strong>{sd$unit_label}</strong>, together with the percentage change compared with the baseline.
    </p>
    <p>
    The Sankey diagram below details the selected scenario ({sd$scenario_label}, {sd$year_used}):
    flows from <em>Production</em> and <em>Imports</em> to <em>Domestic supply</em> and <em>Exports</em>, and then to final uses
    (e.g. <em>Food</em>, <em>Feed</em>, <em>Processing</em>, <em>Losses</em>, <em>Seed</em>, <em>Other uses (non-food)</em>, <em>Unused</em>).
    </p>
    <p>
    When <strong>\"Show as percentage (%)\"</strong> is ticked, node and link labels are expressed as shares of reference poles
    (exports relative to total exports; other links relative to domestic supply). Tooltips always include underlying volumes in <strong>{sd$unit_label}</strong>.
    </p>
    <p>
    <em>Unused</em> is displayed as the balancing item when needed: it captures the residual between <em>Domestic supply</em> and the sum of other internal uses.
    </p>"
      )
      
      htmltools::HTML(txt)
    })
    
  })
}
