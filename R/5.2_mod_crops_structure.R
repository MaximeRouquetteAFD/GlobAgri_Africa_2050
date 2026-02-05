# R/5.2_mod_crop_structure.R
# -------------------------------------------------------------------
# Shares of crop groups in Production / Import Quantity
# (one pie chart per scenario, incl. base year if present)
# + Pie size proportional to total (to show shares + levels)
# + Scenario totals displayed under pies, adapted to the selected unit:
#     - Mass (tons): Total shown in Mt (million tons)
#     - Energy (Gcal): Total shown in Gcal
#
# NEW:
# - Unit toggle: Energy (Gcal) / Mass (tons)  (default = Energy)
# - Uses Energy elements when "Energy (Gcal)" is selected:
#     * Energy Production
#     * Energy Import Quantity
# - Uses Mass elements when "Mass (tons)" is selected:
#     * Production
#     * Import Quantity
#
# SCENARIO RULES (project-wide):
# - The module NEVER rebuilds scenario logic locally.
# - It displays exactly r_scenarios() (codes), intersected with scenarios actually present in fact for the country/element.
# - Internally: Scenario = codes (fact$Scenario). UI: scenario_label(code).
# - Ordering: centralized with SCENARIO_LEVELS_DEFAULT (plus deterministic append for unknown codes, if any).
# -------------------------------------------------------------------

suppressPackageStartupMessages({
  library(shiny)
  library(dplyr)
  library(tidyr)
  library(forcats)
  library(ggplot2)
  library(plotly)
  library(stringr)
  library(scales)
})

# --- Crop groups -----------------------------------------------------------
# 👉 Grass & fodder is EXCLUDED from groups and from the total
CROP_GROUPS <- list(
  "All crop products" = c(
    "Cake Other Oilcrops","Fibers etc.","Fruits and vegetables",
    # "Grass",  # excluded from total
    "Maize","Millet and Sorghum","Oil Other Oilcrops","Oilpalm fruit",
    "Olive Oil","Olives","Other Oilcrops","Other cereals",
    "Other plant products","Other products","Palm Products Oil",
    "Palmkernel Cake","Pulses","Rape and Mustard Cake","Rape and Mustard Oil",
    "Rape and Mustardseed","Rice","Roots and Tuber","Soyabean Cake",
    "Soyabean Oil","Soyabeans","Sugar plants and products",
    "Sunflowerseed","Sunflowerseed Cake","Sunflowerseed Oil","Wheat"
  ),
  "Cereals" = c("Maize","Millet and Sorghum","Other cereals","Rice","Wheat"),
  "Roots and tubers" = c("Roots and Tuber"),
  "Pulses"           = c("Pulses"),
  "Oilcrops (incl. cakes & oils)" = c(
    "Cake Other Oilcrops","Oil Other Oilcrops","Oilpalm fruit","Olive Oil",
    "Olives","Other Oilcrops","Palm Products Oil","Palmkernel Cake",
    "Rape and Mustard Cake","Rape and Mustard Oil","Rape and Mustardseed",
    "Soyabean Cake","Soyabean Oil","Soyabeans","Sunflowerseed",
    "Sunflowerseed Cake","Sunflowerseed Oil"
  ),
  "Fruits & vegetables"     = c("Fruits and vegetables"),
  "Sugar crops"             = c("Sugar plants and products"),
  # "Grass & fodder"        = c("Grass"),  # removed
  "Fibres & other products" = c("Fibers etc.","Other plant products","Other products")
)

CROP_LABELS <- c(
  "All crop products"             = "crop products",
  "Cereals"                       = "cereals",
  "Roots and tubers"              = "roots and tubers",
  "Pulses"                        = "pulses",
  "Oilcrops (incl. cakes & oils)" = "oilcrops",
  "Fruits & vegetables"           = "fruits and vegetables",
  "Sugar crops"                   = "sugar crops",
  # "Grass & fodder"              = "grass and fodder crops",
  "Fibres & other products"       = "fibre and other plant products"
)

# Item -> crop_group table
CROP_ITEM_GROUP <- tibble::tibble(
  Item       = unlist(CROP_GROUPS[names(CROP_GROUPS) != "All crop products"]),
  crop_group = rep(
    names(CROP_GROUPS)[names(CROP_GROUPS) != "All crop products"],
    lengths(CROP_GROUPS[names(CROP_GROUPS) != "All crop products"])
  )
) |>
  distinct(Item, .keep_all = TRUE)

# Map crop group labels -> ITEM_COLORS families (defined in R/02)
CROP_TO_ITEM_FAMILY <- c(
  "cereals"                        = "Cereals",
  "roots and tubers"               = "Roots and tubers",
  "pulses"                         = "Pulses",
  "oilcrops"                       = "Oil",
  "fruits and vegetables"          = "Vegetables and fruits",
  "sugar crops"                    = "Sugar",
  "fibre and other plant products" = "Other"
)

# -------------------------------------------------------------------
# UI
# -------------------------------------------------------------------
mod_crop_structure_ui <- function(id){
  ns <- NS(id)
  tagList(uiOutput(ns("block")))
}

# -------------------------------------------------------------------
# Server
# -------------------------------------------------------------------
mod_crop_structure_server <- function(
    id,
    fact,
    r_country,
    r_scenarios,         # REQUIRED: reactive/function returning scenario CODES to display
    group_var = NULL,
    harvest_element = "Area harvested",
    exclude_items = c(
      "All products","All crops","Agricultural land occupation (Farm)",
      "Cropland","Forest land","Land under perm. meadows and pastures"
    ),
    value_multiplier = 1,
    value_multiplier_energy = 1  # optional extra scaling for energy if needed
){
  moduleServer(id, function(input, output, session){
    
    `%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a
    
    # --- Hard dependencies (scenario single source of truth) ----------------
    if (is.null(r_scenarios) || !is.function(r_scenarios)) {
      stop("mod_crop_structure_server(): 'r_scenarios' must be provided as a reactive/function returning scenario CODES.")
    }
    if (!exists("scenario_label", mode = "function", inherits = TRUE)) {
      stop("mod_crop_structure_server(): missing dependency 'scenario_label(code)'.")
    }
    if (!exists("SCENARIO_LEVELS_DEFAULT", inherits = TRUE)) {
      stop("mod_crop_structure_server(): missing dependency 'SCENARIO_LEVELS_DEFAULT'.")
    }
    
    scenario_label_vec <- function(x){
      x <- as.character(x)
      vapply(x, scenario_label, character(1))
    }
    
    ELEMENT_CHOICES <- c("Production", "Import Quantity")
    
    # --- Unit toggle --------------------------------------------------------
    unit_mode <- reactive({
      u <- input$unit %||% "energy"
      if (!u %in% c("energy","mass")) u <- "energy"
      u
    })
    
    unit_lbl <- reactive(if (identical(unit_mode(), "energy")) "Gcal" else "t")
    
    # UI -> fact$Element mapping (explicit, avoids string mismatch)
    element_fact <- reactive({
      req(input$element)
      if (identical(unit_mode(), "energy")) {
        if (identical(input$element, "Production")) return("Energy Production")
        if (identical(input$element, "Import Quantity")) return("Energy Import Quantity")
        return(paste0("Energy ", input$element))
      } else {
        input$element
      }
    })
    
    # Display multiplier:
    # - MASS: fact values are in 1000 tonnes -> show in tonnes (× 1000)
    # - ENERGY: assume already in Gcal
    value_mult <- reactive({
      if (identical(unit_mode(), "mass")) {
        as.numeric(value_multiplier %||% 1) * 1000
      } else {
        as.numeric(value_multiplier %||% 1) * as.numeric(value_multiplier_energy %||% 1)
      }
    })
    
    # --- Scenario codes to display (ordered centrally) ----------------------
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
    
    # --- bindCache keys (scalar) -------------------------------------------
    cache_key_years <- reactive({
      req(element_fact(), r_country(), unit_mode())
      paste0(
        "crop_structure|years|",
        r_country(), "|",
        element_fact(), "|unit=", unit_mode(), "|sc=",
        paste(scen_codes_ordered(), collapse = ",")
      )
    })
    
    cache_key_data <- reactive({
      req(element_fact(), r_country(), unit_mode())
      paste0(
        "crop_structure|data|",
        r_country(), "|",
        element_fact(), "|unit=", unit_mode(), "|sc=",
        paste(scen_codes_ordered(), collapse = ",")
      )
    })
    
    cache_key_plot <- reactive({
      req(element_fact(), r_country(), unit_mode())
      paste0(
        "crop_structure|plot|",
        r_country(), "|",
        element_fact(), "|unit=", unit_mode(), "|sc=",
        paste(scen_codes_ordered(), collapse = ",")
      )
    })
    
    # --- Years by scenario for the selected element ------------------------
    years_by_scenario <- reactive({
      req(element_fact())
      sc_req <- scen_codes_ordered()
      validate(need(length(sc_req) > 0, "No scenario selected."))
      
      fact %>%
        filter(
          Region == r_country(),
          stringr::str_trim(Element) == element_fact(),
          Scenario %in% sc_req
        ) %>%
        group_by(Scenario) %>%
        summarise(
          year_used = suppressWarnings(max(Year[!is.na(Value)], na.rm = TRUE)),
          .groups   = "drop"
        ) %>%
        filter(is.finite(year_used)) %>%
        mutate(
          Scenario_code  = as.character(Scenario),
          Scenario       = factor(Scenario_code, levels = scen_levels_all())
        ) %>%
        arrange(Scenario) %>%
        select(Scenario_code, Scenario, year_used)
    }) %>% bindCache(cache_key_years())
    
    # Palette based on ITEM_COLORS (R/02)
    if (!exists("crop_group_colors_for", mode = "function")) {
      crop_group_colors_for <- function(groups){
        groups <- as.character(groups)
        if (exists("ITEM_COLORS", inherits = TRUE)) {
          cols_item <- ITEM_COLORS
          fams <- CROP_TO_ITEM_FAMILY[groups]
          vapply(seq_along(groups), function(i){
            fam <- fams[i]
            if (!is.na(fam) && fam %in% names(cols_item)) {
              cols_item[[fam]]
            } else {
              "#CCCCCC"
            }
          }, character(1))
        } else {
          scales::hue_pal()(length(groups))
        }
      }
    }
    
    # --- Aggregated data by crop group -------------------------------------
    data_groups <- reactive({
      req(element_fact(), unit_mode())
      yrs <- years_by_scenario()
      
      validate(need(nrow(yrs) > 0,
                    sprintf("No data for %s in this country.", element_fact())))
      
      scen_present <- yrs$Scenario_code
      mult <- value_mult()
      uout <- unit_lbl()
      
      df_raw <- fact %>%
        filter(
          Region == r_country(),
          stringr::str_trim(Element) == element_fact(),
          Scenario %in% scen_present,
          Item %in% CROP_GROUPS[["All crop products"]]
        ) %>%
        inner_join(select(yrs, Scenario_code, year_used), by = c("Scenario" = "Scenario_code")) %>%
        filter(Year == year_used) %>%
        left_join(CROP_ITEM_GROUP, by = "Item") %>%
        group_by(Scenario, crop_group) %>%
        summarise(
          value   = sum(Value, na.rm = TRUE) * mult,
          unit    = uout,
          year    = dplyr::first(Year),
          .groups = "drop"
        ) %>%
        mutate(
          Scenario = factor(as.character(Scenario), levels = scen_levels_all())
        )
      
      groups_all <- names(CROP_LABELS)[names(CROP_LABELS) != "All crop products"]
      
      grid <- tidyr::expand_grid(
        Scenario   = factor(scen_present, levels = scen_levels_all()),
        crop_group = groups_all
      )
      
      df <- grid %>%
        left_join(df_raw, by = c("Scenario","crop_group")) %>%
        left_join(select(yrs, Scenario, year_used), by = "Scenario") %>%
        mutate(
          value = coalesce(value, 0),
          unit  = coalesce(unit, uout),
          year  = coalesce(year, year_used),
          crop_group_label = CROP_LABELS[crop_group]
        ) %>%
        group_by(Scenario) %>%
        mutate(
          total = sum(value, na.rm = TRUE),
          share = if_else(total > 0, value / total, NA_real_)
        ) %>%
        ungroup()
      
      df
    }) %>% bindCache(cache_key_data())
    
    # --- Palette ------------------------------------------------------------
    palette_groups <- reactive({
      pd <- data_groups()
      
      group_levels <- CROP_LABELS[names(CROP_LABELS) != "All crop products"]
      levs <- group_levels[group_levels %in% unique(pd$crop_group_label)]
      if (length(levs) == 0) levs <- unique(pd$crop_group_label)
      
      cols <- crop_group_colors_for(levs)
      names(cols) <- levs
      cols
    }) %>% bindCache(cache_key_data())
    
    # === BLOCK UI ===========================================================
    output$block <- renderUI({
      ns <- session$ns
      
      div(
        class = "card",
        div(
          class = "card-body",
          
          h2(textOutput(ns("title"))),
          
          div(
            class = "u-controls",
            radioButtons(
              inputId  = ns("unit"),
              label    = NULL,
              choices  = c("Energy (Gcal)" = "energy", "Mass (tons)" = "mass"),
              selected = "energy",
              inline   = TRUE
            ),
            selectInput(
              inputId  = ns("element"),
              label    = "Flow considered",
              choices  = ELEMENT_CHOICES,
              selected = "Production",
              width    = "220px"
            )
          ),
          
          
          plotly::plotlyOutput(ns("pie_crops"), height = "460px", width = "100%"),
          
          div(
            class = "u-actions",
            downloadLink(
              ns("dl_csv"),
              label = tagList(icon("download"), "CSV")
            )
          ),
          
          uiOutput(ns("note"))
        )
      )
    })
    
    output$title <- renderText({
      u <- if (identical(unit_mode(), "energy")) "Energy (Gcal)" else "Mass (tons)"
      paste0("Structure of crop production or imports - ", u)
    })
    
    # --- Pies ---------------------------------------------------------------
    output$pie_crops <- renderPlotly({
      pd <- data_groups()
      req(nrow(pd) > 0)
      
      validate(need(sum(pd$value, na.rm = TRUE) > 0,
                    "No non-zero value to display."))
      
      # >>> THEME GLOBAL (R/99)
      th <- get_plotly_tokens()
      
      group_levels <- CROP_LABELS[names(CROP_LABELS) != "All crop products"]
      
      pd <- pd %>%
        mutate(
          crop_group_label = factor(crop_group_label, levels = group_levels)
        )
      
      cols <- palette_groups()
      if (any(!pd$crop_group_label %in% names(cols))) {
        extra <- setdiff(as.character(pd$crop_group_label), names(cols))
        add_cols <- crop_group_colors_for(extra)
        names(add_cols) <- extra
        cols <- c(cols, add_cols)
      }
      
      df_share <- pd %>%
        filter(total > 0) %>%
        mutate(
          label_pct = if_else(
            !is.na(share),
            scales::percent(share, accuracy = 1),
            NA_character_
          ),
          text_pos = if_else(share < 0.05, "outside", "inside")
        )
      
      u_lbl <- unit_lbl()
      flow_lbl <- input$element %||% "Flow"
      
      totals <- df_share %>%
        distinct(Scenario, total, year) %>%
        arrange(Scenario) %>%
        mutate(
          Scenario_code  = as.character(Scenario),
          Scenario_label = scenario_label_vec(Scenario_code)
        )
      
      max_total <- max(totals$total, na.rm = TRUE)
      if (!is.finite(max_total) || max_total <= 0) max_total <- 1
      
      scen_facets <- totals$Scenario_code
      n_pies <- length(scen_facets)
      validate(need(n_pies > 0, "No scenario available."))
      
      x_slots <- lapply(seq_len(n_pies), function(i){
        c((i - 1) / n_pies, i / n_pies)
      })
      
      dom_y_fixed <- c(0.22, 0.90)
      padding <- 0.92
      
      fmt_total <- function(x){
        if (identical(unit_mode(), "mass")) {
          # x in tonnes -> show in Mt (million tonnes)
          mt <- x / 1e6
          list(
            text   = scales::number(mt, accuracy = 0.001, big.mark = ",", decimal.mark = "."),
            suffix = "Mt"
          )
        } else {
          # x in Gcal -> show in Gcal
          list(
            text   = scales::number(x, accuracy = 1, big.mark = ",", decimal.mark = "."),
            suffix = "Gcal"
          )
        }
      }
      
      p <- plotly::plot_ly()
      annotations <- vector("list", n_pies)
      
      for (i in seq_len(n_pies)) {
        sc_code <- scen_facets[i]
        sc_lab  <- totals$Scenario_label[totals$Scenario_code == sc_code][1] %||% sc_code
        
        slot <- x_slots[[i]]
        slot_center <- mean(slot)
        slot_half   <- diff(slot) / 2
        
        sc_total <- totals$total[totals$Scenario_code == sc_code][1] %||% 0
        sc_year  <- totals$year[totals$Scenario_code == sc_code][1] %||% NA
        
        r <- sqrt(max(sc_total, 0) / max_total)
        r <- min(max(r, 0.20), 1)
        
        dom_x <- c(
          slot_center - slot_half * r * padding,
          slot_center + slot_half * r * padding
        )
        
        d <- df_share %>%
          filter(as.character(Scenario) == sc_code, value > 0)
        
        if (nrow(d) == 0) next
        
        show_leg <- (i == 1)
        
        p <- p %>%
          plotly::add_pie(
            data   = d,
            labels = ~crop_group_label,
            values = ~share,
            text   = ~label_pct,
            textinfo = "text",
            name   = sc_lab,
            domain = list(x = dom_x, y = dom_y_fixed),
            sort   = FALSE,
            textposition = ~text_pos,
            textfont     = list(color = th$font_color, size = 12),
            insidetextorientation = "horizontal",
            marker       = list(colors = cols[as.character(d$crop_group_label)]),
            customdata   = ~value,
            hovertemplate = paste0(
              "<b>", sc_lab, "</b><br>",
              "%{label}<br>",
              "Share: %{percent}<br>",
              "Value: %{customdata:,} ", u_lbl,
              "<extra></extra>"
            ),
            showlegend  = show_leg
          )
        
        tot_disp <- fmt_total(sc_total)
        
        annotations[[i]] <- list(
          x = slot_center,
          y = 0.12,
          xanchor = "center",
          yanchor = "top",
          showarrow = FALSE,
          align = "center",
          text = paste0(
            "<span style='color:", th$font_color, ";'>",
            sc_lab,
            if (!is.na(sc_year)) paste0(" (", sc_year, ")") else "",
            "</span>",
            "<br><span style='font-size:11px;color:", th$muted_color, ";'>",
            "Total ", flow_lbl, ": ", tot_disp$text, " ", tot_disp$suffix,
            "</span>"
          ),
          font = list(size = 12, color = th$font_color)
        )
      }
      
      p <- plotly_apply_global_theme(p, bg = "transparent", grid = "none")
      
      p %>%
        plotly::layout(
          title         = NULL,
          annotations   = annotations,
          margin        = list(t = 80, b = 20, l = 40, r = 40),
          showlegend    = TRUE,
          legend        = list(
            orientation = "h",
            x = 0.5, xanchor = "center",
            y = 1.02, yanchor = "bottom",
            font = list(color = th$font_color, size = 12),
            traceorder = "normal"
          ),
          paper_bgcolor = APP_TRANSPARENT,
          plot_bgcolor  = APP_TRANSPARENT,
          hoverlabel    = list(
            bgcolor = th$hover_bg,
            font    = list(color = th$hover_font)
          ),
          font          = list(color = th$font_color)
        ) %>%
        plotly::config(displaylogo = FALSE)
    }) %>% bindCache(cache_key_plot())
    
    # --- CSV export ---------------------------------------------------------
    output$dl_csv <- downloadHandler(
      filename = function(){
        paste0(
          "Crop_structure_",
          gsub(" ", "_", r_country()),
          "_",
          gsub(" ", "_", element_fact()),
          "_",
          unit_mode(),
          ".csv"
        )
      },
      content = function(file){
        pd <- data_groups()
        out <- pd %>%
          transmute(
            Country        = r_country(),
            Scenario_code  = as.character(Scenario),
            Scenario_label = scenario_label_vec(as.character(Scenario)),
            Year           = year,
            Element_UI     = input$element,
            Element_fact   = element_fact(),
            Unit_mode      = unit_mode(),
            Unit           = unit_lbl(),
            Crop_group     = crop_group_label,
            Value          = value,
            Total          = total,
            Share          = share
          )
        readr::write_delim(out, file, delim = ";")
      }
    )
    
    # --- Note ---------------------------------------------------------------
    output$note <- renderUI({
      u_txt <- if (identical(unit_mode(), "energy")) "energy (Gcal)" else "mass (tons)"
      e_txt <- input$element %||% "Production"
      
      htmltools::HTML(paste0(
        "<p>
        Each pie chart shows, for the selected country, the structure of crop <strong>", e_txt, "</strong> by broad crop groups
        (cereals, roots and tubers, pulses, oilcrops, fruits and vegetables, sugar crops, fibre and other plant products),
        excluding forage and grass crops.
        </p>
        <p>
        Use the toggle to display results in <strong>", u_txt, "</strong>. Slice sizes represent shares within each scenario.
        The overall pie area is proportional to the scenario total (to convey both composition and level).
        </p>
        <p>
        Totals shown under each pie are expressed in <strong>Mt</strong> when mass is selected (million tons),
        and in <strong>Gcal</strong> when energy is selected.
        </p>"
      ))
    })
    
  })
}
