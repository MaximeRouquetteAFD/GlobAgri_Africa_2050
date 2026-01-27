# R/4.2_mod_forest_share100.R
# ---------------------------------------------------------------
# 100% stacked bars: share of Forest vs Agricultural (Cropland + Pastures)
# Scenarios: come ONLY from r_scenarios() (codes), ordered by SCENARIO_LEVELS_DEFAULT.
# Display labels: scenario_label(code) used for axis / hover.
# No local scenario logic, no redundancy/constraint logic here.
# ---------------------------------------------------------------

mod_forest_share100_ui <- function(
    id,
    height = "380px",
    wrap = c("card", "none")
){
  ns <- NS(id)
  wrap <- match.arg(wrap)
  
  content <- tagList(
    h3("Share of forest in total area (forest land + agricultural land)"),
    plotly::plotlyOutput(ns("share_plot"), height = height, width = "100%"),
    tags$br(),
    
    div(
      class = "text-right",
      div(
        class = "u-actions",
        downloadLink(
          ns("dl_share_csv"),
          label = tagList(icon("download"), "CSV")
        )
      )
    ),
    
    uiOutput(ns("note"))
  )
  
  if (wrap == "card") {
    div(class = "card", div(class = "card-body", content))
  } else {
    content
  }
}


mod_forest_share100_server <- function(
    id, fact, r_country,
    r_scenarios,                 # <- reactive : vecteur de CODES (baseline + diets + éventuel extra)
    ...
){
  moduleServer(id, function(input, output, session){
    
    # -----------------------------------------------------------------------
    # Dependencies (assumed to exist globally)
    # - scenario_code()
    # - scenario_label()
    # - SCENARIO_LEVELS_DEFAULT
    # - (optional) SCENARIOS_EXTRA_CHOICES for robust recode label->code in data
    # -----------------------------------------------------------------------
    
    AREA_ELT <- if (exists("AREA_ELEMENT", inherits = TRUE)) get("AREA_ELEMENT", inherits = TRUE) else "Area"
    
    # Items sources (agri = cropland + pastures)
    IT_FOREST  <- "Forest land"
    IT_CROP    <- "Cropland"
    IT_PASTURE <- "Land under perm. meadows and pastures"
    
    # Couleurs (palette R/02)
    col_forest <- unname(area_colors_for(IT_FOREST)[1])
    col_agri   <- unname(area_colors_for("Agricultural land occupation (Farm)")[1])
    
    # -----------------------------------------------------------------------
    # Scenarios (CODES only) — no fallback business logic
    # -----------------------------------------------------------------------
    
    code_fun  <- if (exists("scenario_code",  mode = "function")) scenario_code  else function(x) x
    label_fun <- if (exists("scenario_label", mode = "function")) scenario_label else function(x) x
    
    levels_default <- if (exists("SCENARIO_LEVELS_DEFAULT", inherits = TRUE)) {
      get("SCENARIO_LEVELS_DEFAULT", inherits = TRUE)
    } else {
      # fallback technique (non-métier) si jamais le config n’est pas chargé
      unique(code_fun(c("Année de base","Même diète","Diète probable","Diète saine")))
    }
    
    # Robust recode in case data contains UI label instead of code for extra
    EXTRA_LABEL_TO_CODE <- if (exists("SCENARIOS_EXTRA_CHOICES", inherits = TRUE)) {
      setNames(unname(get("SCENARIOS_EXTRA_CHOICES", inherits = TRUE)),
               names(get("SCENARIOS_EXTRA_CHOICES", inherits = TRUE)))
    } else {
      NULL
    }
    
    # Scenarios requested by app (codes)
    scen_wanted <- reactive({
      sc <- r_scenarios()
      validate(shiny::need(!is.null(sc) && length(sc) > 0, "No scenarios provided to the module."))
      unique(code_fun(sc))
    })
    
    # Country + element slice (normalized Scenario codes) used to detect "present"
    df_country_area <- reactive({
      df <- fact %>%
        dplyr::filter(
          Region == r_country(),
          stringr::str_to_lower(Element) == stringr::str_to_lower(AREA_ELT)
        ) %>%
        dplyr::mutate(Scenario = code_fun(Scenario))
      
      if (!is.null(EXTRA_LABEL_TO_CODE)) {
        df <- df %>% dplyr::mutate(
          Scenario = dplyr::recode(Scenario, !!!EXTRA_LABEL_TO_CODE, .default = Scenario)
        )
      }
      
      df
    }) %>% bindCache(r_country())
    
    scen_present <- reactive({
      unique(as.character(df_country_area()$Scenario))
    }) %>% bindCache(r_country())
    
    # Effective scenarios for THIS module = wanted ∩ present (no new rules)
    scen_show <- reactive({
      wanted  <- scen_wanted()
      present <- scen_present()
      out <- wanted[wanted %in% present]
      validate(shiny::need(length(out) > 0, "No requested scenario is available in the data for this country/element."))
      out
    }) %>% bindCache(r_country())
    
    # Stable order from config levels_default, restricted to scen_show()
    scen_levels_show <- reactive({
      show <- scen_show()
      ordered <- levels_default[levels_default %in% show]
      # Robustness: if some codes are not in levels_default, append them (should not happen in normal operation)
      missing <- setdiff(show, ordered)
      c(ordered, missing)
    }) %>% bindCache(r_country())
    
    # Scalar cache key (do NOT pass vectors to bindCache)
    scen_key <- reactive({
      # If a global helper exists, you can swap to it; otherwise keep local scalar key.
      paste(scen_levels_show(), collapse = "|")
    }) %>% bindCache(r_country())
    
    # -----------------------------------------------------------------------
    # 1) Année retenue par scénario (même logique, mais scénarios = scen_levels_show())
    # -----------------------------------------------------------------------
    years_by_scenario <- reactive({
      scen_used <- scen_levels_show()
      validate(shiny::need(length(scen_used) > 0, "No scenario available."))
      
      dat <- df_country_area() %>%
        dplyr::filter(Scenario %in% scen_used)
      
      scen_base <- if (exists("SCENARIO_BASE_YEAR_CODE", inherits = TRUE)) {
        code_fun(get("SCENARIO_BASE_YEAR_CODE", inherits = TRUE))
      } else {
        code_fun("Année de base")
      }
      
      base_year <- if (any(
        dat$Scenario == scen_base &
        dat$Year == 2018 &
        !is.na(dat$Value)
      )) {
        2018
      } else {
        suppressWarnings(
          max(dat$Year[dat$Scenario == scen_base & !is.na(dat$Value)], na.rm = TRUE)
        )
      }
      
      others <- dat %>%
        dplyr::filter(Scenario != scen_base) %>%
        dplyr::group_by(Scenario) %>%
        dplyr::summarise(
          year_used = suppressWarnings(max(Year[!is.na(Value)], na.rm = TRUE)),
          .groups   = "drop"
        )
      
      dplyr::bind_rows(
        tibble::tibble(Scenario = scen_base, year_used = base_year),
        others
      ) %>%
        dplyr::filter(is.finite(year_used)) %>%
        dplyr::mutate(Scenario = factor(as.character(Scenario), levels = scen_used)) %>%
        dplyr::arrange(Scenario)
    }) %>% bindCache(r_country(), scen_key())
    
    # -----------------------------------------------------------------------
    # 2) Données components (ha) : forest + (cropland + pasture)
    # -----------------------------------------------------------------------
    data_components <- reactive({
      scen_used <- scen_levels_show()
      yrs <- years_by_scenario()
      validate(shiny::need(nrow(yrs) > 0, "Pas d'année disponible pour 'Area'."))
      
      df <- df_country_area() %>%
        dplyr::inner_join(yrs, by = "Scenario") %>%
        dplyr::filter(
          Scenario %in% scen_used,
          Year == year_used,
          Item %in% c(IT_FOREST, IT_CROP, IT_PASTURE)
        ) %>%
        dplyr::mutate(
          Scenario = factor(as.character(Scenario), levels = scen_used),
          Value = dplyr::if_else(!is.na(Value) & Value < 0, 0, Value)
        )
      
      out <- df %>%
        dplyr::group_by(Scenario, year_used) %>%
        dplyr::summarise(
          forest_ha  = sum(Value[Item == IT_FOREST], na.rm = TRUE) * 1000,
          crop_ha    = sum(Value[Item == IT_CROP], na.rm = TRUE) * 1000,
          pasture_ha = sum(Value[Item == IT_PASTURE], na.rm = TRUE) * 1000,
          .groups    = "drop"
        ) %>%
        dplyr::mutate(
          agri_ha  = crop_ha + pasture_ha,
          total_ha = forest_ha + agri_ha
        ) %>%
        dplyr::arrange(Scenario)
      
      validate(shiny::need(any(is.finite(out$total_ha) & out$total_ha > 0), "Total area is missing or zero for this country."))
      out
    }) %>% bindCache(r_country(), scen_key())
    
    # -----------------------------------------------------------------------
    # 3) Données long format en %
    # -----------------------------------------------------------------------
    data_share_long <- reactive({
      dat <- data_components()
      req(nrow(dat) > 0)
      
      dat <- dat %>%
        dplyr::mutate(
          forest_pct = dplyr::if_else(total_ha > 0, 100 * forest_ha / total_ha, NA_real_),
          agri_pct   = dplyr::if_else(total_ha > 0, 100 * agri_ha   / total_ha, NA_real_)
        )
      
      out <- dat %>%
        tidyr::pivot_longer(
          cols = c(agri_pct, forest_pct),
          names_to  = "Component",
          values_to = "pct"
        ) %>%
        dplyr::mutate(
          Component = dplyr::recode(
            Component,
            agri_pct   = "Agricultural land",
            forest_pct = "Forest land"
          ),
          Component = factor(Component, levels = c("Forest land", "Agricultural land")),
          value_ha  = dplyr::if_else(Component == "Forest land", forest_ha, agri_ha),
          label_pct = dplyr::if_else(is.finite(pct), paste0("<b>", sprintf("%.2f%%", pct), "</b>"), "")
        )
      
      validate(shiny::need(any(is.finite(out$pct)), "No share can be computed for this country."))
      out
    }) %>% bindCache(r_country(), scen_key())
    
    # -----------------------------------------------------------------------
    # 4) Plotly 100% stacked (display uses scenario_label)
    # -----------------------------------------------------------------------
    output$share_plot <- plotly::renderPlotly({
      df <- data_share_long()
      req(nrow(df) > 0)
      
      scen_codes <- levels(df$Scenario)
      scen_ticks <- vapply(scen_codes, label_fun, character(1))
      bar_w <- 0.85
      cols <- c("Agricultural land" = col_agri, "Forest land" = col_forest)
      
      p <- plotly::plot_ly() %>%
        plotly::layout(
          barmode = "stack",
          xaxis = list(
            title    = "",
            type     = "linear",
            tickmode = "array",
            tickvals = seq_along(scen_codes),
            ticktext = scen_ticks
          ),
          yaxis = list(
            title = "Share of total (%)",
            range = c(0, 100)
          ),
          legend = list(title = list(text = ""))
        )
      
      for (comp in levels(df$Component)) {
        sub <- df %>% dplyr::filter(Component == comp)
        if (nrow(sub) == 0) next
        
        sub <- sub %>%
          dplyr::mutate(
            cd_scen = label_fun(as.character(Scenario)),
            cd_ha   = format(round(value_ha), big.mark = " ", scientific = FALSE, trim = TRUE)
          )
        
        p <- plotly::add_bars(
          p, data = sub,
          x = ~match(Scenario, scen_codes),
          y = ~pct,
          width = bar_w,
          name = comp,
          marker = list(color = unname(cols[[comp]])),
          text = ~label_pct,
          textposition = "inside",
          insidetextanchor = "middle",
          customdata = ~cbind(cd_scen, cd_ha),
          hovertemplate = paste0(
            "%{customdata[0]}<br>",
            comp, ": %{y:.2f}%<br>",
            "Value: %{customdata[1]} ha<extra></extra>"
          )
        )
      }
      
      if (exists("plotly_apply_global_theme", mode = "function")) {
        p <- plotly_apply_global_theme(p, bg = "transparent", grid = "y")
      }
      
      p
    })
    
    # -----------------------------------------------------------------------
    # 5) Export CSV (Scenario kept as CODE + add label column for convenience)
    # -----------------------------------------------------------------------
    output$dl_share_csv <- downloadHandler(
      filename = function(){
        paste0("Forest_Agri_shares_", gsub(" ", "_", r_country()), ".csv")
      },
      content = function(file){
        df <- data_share_long()
        req(nrow(df) > 0)
        
        out <- df %>%
          dplyr::transmute(
            Pays          = r_country(),
            Scenario      = as.character(Scenario),                 # CODE
            Scenario_UI   = label_fun(as.character(Scenario)),      # LABEL
            Annee         = year_used,
            Composante    = as.character(Component),
            Part_pct      = pct,
            Valeur_ha     = value_ha
          ) %>%
          dplyr::arrange(Scenario, Composante)
        
        readr::write_delim(out, file, delim = ";")
      }
    )
    
    # -----------------------------------------------------------------------
    # 6) Note
    # -----------------------------------------------------------------------
    output$note <- renderUI({
      HTML(
        "<p>
          This 100% stacked chart shows, for each scenario provided by the application,
          the share of <strong>Forest land</strong> and <strong>Agricultural land</strong>
          in the total (Forest + Agricultural).
        </p>
        <p>
          Agricultural land is computed as <em>Cropland + Permanent meadows and pastures</em>.
        </p>"
      )
    })
    
    invisible(NULL)
  })
}
