# R/6.1_mod_land_use_change.R
# ----------------------------------------------------------
# Land use change (surfaces) et émissions associées
#  - Pour les scénarios affichés par l'app (r_scenarios, codes) :
#      * Δ Cropland (ha) vs baseline (Année de base, BASE_YEAR)
#      * Δ Pastures and meadows (ha) vs baseline
#      * Emissions "Land use change" (tCO2e) en TARGET_YEAR
#
# SCENARIOS (IMPORTANT):
# - No local scenario reconstruction, no masking, no "Avec contrainte" logic.
# - The module uses r_scenarios() (codes) as single source of truth,
#   optionally intersected with scenarios present in fact for the country/elements.
# - Scenario order is centralized via SCENARIO_LEVELS_DEFAULT.
# - UI display uses scenario_label(code) only.
# - bindCache keys must be scalar -> scen_show_key() (preferred) or local scalar fallback.
# ----------------------------------------------------------

# Palette spécifique au module Land use change
LAND_USE_CHANGE_COLORS <- c(
  "Cropland"             = "#CCCCCC",
  "Pastures and meadows" = "#999999",
  "LUC_pos"              = "#EF4444",  # emissions positives
  "LUC_neg"              = "#59A14F"   # emissions négatives
)

land_use_colors_for <- function(types){
  types <- as.character(types)
  cols  <- LAND_USE_CHANGE_COLORS
  miss  <- setdiff(types, names(cols))
  if (length(miss)) {
    cols <- c(cols, setNames(scales::hue_pal()(length(miss)), miss))
  }
  cols[types]
}


mod_land_use_change_ui <- function(id, height = "600px"){
  ns <- NS(id)
  tagList(
    div(
      class = "card",
      div(
        class = "card-body",
        h2("Land use change and associated emissions"),
        plotly::plotlyOutput(ns("plot"), height = height),
        
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
  )
}


mod_land_use_change_server <- function(
    id,
    fact,
    r_country,
    r_scenarios,
    scen_show_key = NULL,
    BASE_YEAR   = 2018,
    TARGET_YEAR = 2050,
    AREA_ELEMENT = "Area",
    CROPLAND_ITEM = "Cropland",
    PASTURE_ITEM  = "Land under perm. meadows and pastures",
    LUC_ELEMENT   = "Emissions",
    LUC_ITEM      = "Land use change"
){
  moduleServer(id, function(input, output, session){
    
    `%||%` <- function(x, y) if (is.null(x)) y else x
    
    BASELINE_CODE <- scenario_code("Année de base")
    
    # --------- Formatage labels -----------------------------------------
    fmt_label <- function(value, unit){
      if (!is.finite(value) || value == 0) return("")
      sign <- ifelse(value > 0, "+", ifelse(value < 0, "−", ""))
      num  <- scales::comma(
        abs(value),
        accuracy     = 0.1,
        big.mark     = " ",
        decimal.mark = ","
      )
      paste0(sign, " ", num, unit)
    }
    
    # ---- Scalar key for bindCache (scenario set is a vector) ----
    scen_set_key <- reactive({
      req(r_scenarios())
      if (!is.null(scen_show_key)) {
        k <- scen_show_key()
        req(length(k) == 1)
        return(as.character(k))
      }
      paste(r_scenarios(), collapse = "|")
    })
    
    # ---- Scenarios effective for THIS module: r_scenarios ∩ fact (country/elements) ----
    scenarios_effective <- reactive({
      req(fact, r_country(), r_scenarios())
      
      df0 <- fact %>%
        dplyr::filter(
          Region == r_country(),
          (Element == AREA_ELEMENT & Item %in% c(CROPLAND_ITEM, PASTURE_ITEM)) |
            (Element == LUC_ELEMENT  & Item == LUC_ITEM)
        )
      
      scen_in_data <- unique(df0$Scenario)
      
      scen_from_app <- intersect(SCENARIO_LEVELS_DEFAULT, r_scenarios())
      scen_keep     <- scen_from_app[scen_from_app %in% scen_in_data]
      
      scen_keep
    }) %>%
      bindCache(r_country(), scen_set_key())
    
    # ======================= Données brutes ==============================
    data_luc_raw <- reactive({
      req(fact, r_country(), scenarios_effective())
      
      scen_eff <- scenarios_effective()
      # On a besoin du baseline pour calculer des deltas
      if (!(BASELINE_CODE %in% scen_eff)) {
        return(tibble::tibble(
          Scenario = character(),
          type     = character(),
          unit     = character(),
          value    = numeric()
        ))
      }
      
      fact_cty <- fact %>%
        dplyr::filter(Region == r_country())
      
      # 1) Surface de référence : baseline, BASE_YEAR
      area_base <- fact_cty %>%
        dplyr::filter(
          Element  == AREA_ELEMENT,
          Item %in% c(CROPLAND_ITEM, PASTURE_ITEM),
          Scenario == BASELINE_CODE,
          Year     == BASE_YEAR
        ) %>%
        dplyr::group_by(Item) %>%
        dplyr::summarise(
          area_base = sum(Value, na.rm = TRUE),
          .groups = "drop"
        ) %>%
        dplyr::filter(is.finite(area_base))
      
      # 2) Surfaces TARGET_YEAR pour les scénarios affichés (hors baseline)
      scen_future <- setdiff(scen_eff, BASELINE_CODE)
      
      area_2050 <- fact_cty %>%
        dplyr::filter(
          Element  == AREA_ELEMENT,
          Item %in% c(CROPLAND_ITEM, PASTURE_ITEM),
          Year     == TARGET_YEAR,
          Scenario %in% scen_future
        ) %>%
        dplyr::group_by(Scenario, Item) %>%
        dplyr::summarise(
          area_2050 = sum(Value, na.rm = TRUE),
          .groups = "drop"
        ) %>%
        dplyr::filter(is.finite(area_2050))
      
      if (nrow(area_base) == 0L || nrow(area_2050) == 0L) {
        return(tibble::tibble(
          Scenario = character(),
          type     = character(),
          unit     = character(),
          value    = numeric()
        ))
      }
      
      # 3) Delta = 2050 - base
      #    Area en milliers d'ha -> on convertit en ha via * 1000
      area_change <- area_2050 %>%
        dplyr::left_join(area_base, by = "Item") %>%
        dplyr::mutate(
          delta_thousand = area_2050 - area_base,   # en 1 000 ha
          value          = delta_thousand * 1000,   # en ha
          type  = dplyr::case_when(
            Item == CROPLAND_ITEM ~ "Cropland",
            Item == PASTURE_ITEM  ~ "Pastures and meadows",
            TRUE ~ as.character(Item)
          ),
          unit  = " ha"
        ) %>%
        dplyr::select(Scenario, type, unit, value)
      
      # 4) Emissions LUC TARGET_YEAR (mêmes scénarios affichés hors baseline)
      luc_emis <- fact_cty %>%
        dplyr::filter(
          Element  == LUC_ELEMENT,
          Item     == LUC_ITEM,
          Year     == TARGET_YEAR,
          Scenario %in% scen_future
        ) %>%
        dplyr::group_by(Scenario) %>%
        dplyr::summarise(
          value = sum(Value, na.rm = TRUE),
          .groups = "drop"
        ) %>%
        dplyr::filter(is.finite(value)) %>%
        dplyr::mutate(
          type = "Land use change emission",
          unit = " tCO\u2082e"
        )
      
      dplyr::bind_rows(area_change, luc_emis)
    }) %>%
      bindCache(r_country(), scen_set_key())
    
    # ======================= Données prêtes pour affichage =======================
    data_luc <- reactive({
      df <- data_luc_raw()
      if (nrow(df) == 0L) return(df)
      
      scen_levels <- scenarios_effective()
      # sécurité : levels ordonnés par SCENARIO_LEVELS_DEFAULT déjà appliqué dans scenarios_effective()
      
      df %>%
        dplyr::mutate(
          Scenario = factor(Scenario, levels = scen_levels),
          type = factor(type, levels = c("Cropland", "Pastures and meadows", "Land use change emission"))
        ) %>%
        dplyr::filter(!is.na(Scenario), !is.na(type), is.finite(value)) %>%
        dplyr::arrange(Scenario, type)
    }) %>%
      bindCache(r_country(), scen_set_key())
    
    # ======================= Graphique principal ==========================
    output$plot <- plotly::renderPlotly({
      df <- data_luc()
      req(nrow(df) > 0)
      
      # --- tokens R/99 (dark/light) ---
      th <- if (exists("get_plotly_tokens", mode = "function")) {
        get_plotly_tokens()
      } else {
        list(
          font_color     = "#111827",
          muted_color    = "#6B7280",
          axis_linecolor = "rgba(0,0,0,.18)"
        )
      }
      
      # ---- Valeurs pour l'affichage : k ha et Mt CO2e ---------------------
      df <- df %>%
        dplyr::mutate(
          value_plot = dplyr::case_when(
            type %in% c("Cropland", "Pastures and meadows") ~ value / 1000,  # -> milliers ha
            type == "Land use change emission"             ~ value / 1e6,   # -> Mt CO2e
            TRUE ~ value
          ),
          unit_plot = dplyr::case_when(
            type %in% c("Cropland", "Pastures and meadows") ~ " k ha",
            type == "Land use change emission"              ~ " Mt CO\u2082e",
            TRUE ~ unit
          )
        ) %>%
        dplyr::filter(is.finite(value_plot), !is.na(Scenario))
      
      req(nrow(df) > 0)
      
      # Couleurs
      col_cropland <- LAND_USE_CHANGE_COLORS[["Cropland"]]
      col_pasture  <- LAND_USE_CHANGE_COLORS[["Pastures and meadows"]]
      col_luc_pos  <- LAND_USE_CHANGE_COLORS[["LUC_pos"]]
      col_luc_neg  <- LAND_USE_CHANGE_COLORS[["LUC_neg"]]
      
      # Labels + hover (label scénario via scenario_label)
      df <- df %>%
        dplyr::mutate(
          scen_code = as.character(Scenario),
          scen_lbl  = vapply(scen_code, scenario_label, FUN.VALUE = character(1)),
          label     = purrr::map2_chr(value_plot, unit_plot, fmt_label)
        )
      
      df_ha <- df %>%
        dplyr::filter(type %in% c("Cropland", "Pastures and meadows")) %>%
        dplyr::mutate(
          hover = paste0(
            "<b>Scenario:</b> ", scen_lbl, "<br>",
            "<b>Type:</b> ", as.character(type), "<br>",
            "<b>Δ area:</b> ",
            scales::comma(value_plot, big.mark = " ", accuracy = 0.1), " k ha"
          )
        )
      
      df_luc <- df %>%
        dplyr::filter(type == "Land use change emission") %>%
        dplyr::mutate(
          hover = paste0(
            "<b>Scenario:</b> ", scen_lbl, "<br>",
            "<b>LUC emissions:</b> ",
            scales::comma(value_plot, big.mark = " ", accuracy = 0.01), " Mt CO\u2082e"
          )
        )
      
      # --- Ranges dynamiques (inchangé) -----------------------------------
      vals_ha  <- df_ha$value_plot[is.finite(df_ha$value_plot)]
      vals_luc <- df_luc$value_plot[is.finite(df_luc$value_plot)]
      
      if (length(vals_ha) == 0 || length(vals_luc) == 0) {
        y_range  <- NULL
        y2_range <- NULL
      } else {
        ha_min  <- min(vals_ha,  na.rm = TRUE); ha_max  <- max(vals_ha,  na.rm = TRUE)
        luc_min <- min(vals_luc, na.rm = TRUE); luc_max <- max(vals_luc, na.rm = TRUE)
        
        marge_ha  <- 0.35 * (ha_max  - ha_min)
        marge_luc <- 0.35 * (luc_max - luc_min)
        
        # si amplitude nulle, on force une marge minimale
        if (!is.finite(marge_ha)  || marge_ha  == 0) marge_ha  <- max(1, 0.05 * abs(ha_max))
        if (!is.finite(marge_luc) || marge_luc == 0) marge_luc <- max(0.1, 0.05 * abs(luc_max))
        
        ha_min  <- ha_min  - marge_ha;  ha_max  <- ha_max  + marge_ha
        luc_min <- luc_min - marge_luc; luc_max <- luc_max + marge_luc
        
        ratio_ha  <- abs(ha_min)  / (ha_max  + abs(ha_min))
        ratio_luc <- abs(luc_min) / (luc_max + abs(luc_min))
        
        if (is.finite(ratio_ha) && is.finite(ratio_luc) && ratio_ha != ratio_luc) {
          total_luc <- luc_max + abs(luc_min)
          luc_min <- - total_luc * ratio_ha
          luc_max <-   total_luc * (1 - ratio_ha)
        }
        
        y_range  <- c(ha_min,  ha_max)
        y2_range <- c(luc_min, luc_max)
      }
      
      # ---- Axis tick labels : show scenario labels, keep codes internally
      scen_levels <- levels(df$Scenario)
      ticktext <- vapply(scen_levels, scenario_label, FUN.VALUE = character(1))
      
      df_crop <- df_ha %>% dplyr::filter(type == "Cropland")
      df_past <- df_ha %>% dplyr::filter(type == "Pastures and meadows")
      
      p <- plotly::plot_ly()
      
      if (nrow(df_crop) > 0) {
        p <- p %>% plotly::add_bars(
          data         = df_crop,
          x            = ~scen_code,
          y            = ~value_plot,
          name         = "Cropland",
          marker       = list(color = col_cropland),
          text         = ~label,
          textposition = "outside",
          textfont     = list(size = 12),
          hovertext    = ~hover,
          hoverinfo    = "text",
          yaxis        = "y",
          offsetgroup  = "cropland",
          cliponaxis   = FALSE
        )
      }
      
      if (nrow(df_past) > 0) {
        p <- p %>% plotly::add_bars(
          data         = df_past,
          x            = ~scen_code,
          y            = ~value_plot,
          name         = "Pastures and meadows",
          marker       = list(color = col_pasture),
          text         = ~label,
          textposition = "outside",
          textfont     = list(size = 12),
          hovertext    = ~hover,
          hoverinfo    = "text",
          yaxis        = "y",
          offsetgroup  = "pasture",
          cliponaxis   = FALSE
        )
      }
      
      if (nrow(df_luc) > 0) {
        luc_colors <- ifelse(df_luc$value >= 0, col_luc_pos, col_luc_neg)
        p <- p %>% plotly::add_bars(
          data         = df_luc,
          x            = ~scen_code,
          y            = ~value_plot,
          name         = "Land use change emission",
          marker       = list(color = luc_colors),
          text         = ~label,
          textposition = "outside",
          textfont     = list(size = 12),
          hovertext    = ~hover,
          hoverinfo    = "text",
          yaxis        = "y2",
          offsetgroup  = "luc",
          cliponaxis   = FALSE
        )
      }
      
      p <- p %>% plotly::layout(
        barmode = "group",
        xaxis = list(
          title         = "",
          zeroline      = FALSE,
          type          = "category",
          categoryorder = "array",
          categoryarray = scen_levels,
          tickmode      = "array",
          tickvals      = scen_levels,
          ticktext      = ticktext,
          tickfont      = list(size = 13)
        ),
        yaxis = list(
          title         = "Land use change (k ha)",
          range         = y_range,
          zeroline      = TRUE,
          zerolinecolor = th$axis_linecolor %||% "rgba(0,0,0,.18)",
          showgrid      = TRUE
        ),
        yaxis2 = list(
          title      = "Land use change emissions (Mt CO\u2082e)",
          overlaying = "y",
          side       = "right",
          range      = y2_range,
          zeroline   = FALSE,
          showgrid   = FALSE
        ),
        legend = list(
          orientation = "h",
          x = 0.3, xanchor = "left",
          y = 1.12
        ),
        margin = list(l = 60, r = 60, t = 10, b = 110)
      )
      
      if (exists("plotly_apply_global_theme", mode = "function")) {
        p <- plotly_apply_global_theme(p, bg = "transparent", grid = "y")
      }
      
      p
    }) %>%
      bindCache(r_country(), scen_set_key())
    
    # ======================= Download CSV =================================
    output$dl_csv <- downloadHandler(
      filename = function(){
        paste0("land_use_change_", r_country(), ".csv")
      },
      content = function(file){
        df <- data_luc()
        if (is.null(df) || nrow(df) == 0) {
          utils::write.csv(data.frame(), file, row.names = FALSE)
        } else {
          df_export <- df %>%
            dplyr::mutate(
              Scenario = as.character(Scenario),
              Scenario_label = vapply(Scenario, scenario_label, FUN.VALUE = character(1)),
              type     = as.character(type)
            )
          utils::write.csv(df_export, file, row.names = FALSE, fileEncoding = "UTF-8")
        }
      }
    )
    
    # ============================ Note ====================================
    output$note <- renderUI({
      req(r_country())
      df <- data_luc()
      if (is.null(df) || nrow(df) == 0) return(NULL)
      
      txt <- glue::glue(
        "<p>
        This chart shows how land use changes between
        <strong>{BASE_YEAR}</strong> (baseline) and <strong>{TARGET_YEAR}</strong> under the scenarios displayed in the application,
        together with the <strong>net CO\u2082 emissions</strong> associated with land use change.
        </p>
        <p>
        For each scenario, the two grey bars (left axis, in <strong>thousand hectares (k ha)</strong>)
        show the change in <strong>cropland</strong> (light grey) and
        <strong>permanent meadows and pastures</strong> (dark grey)
        compared with the baseline. Positive values indicate an expansion of the corresponding land use,
        while negative values indicate a reduction in area.
        </p>
        <p>
        The coloured bar represents <strong>net land-use-change emissions</strong> in
        <strong>million tonnes of CO\u2082-equivalent (Mt CO\u2082e)</strong> (right axis).
        When the bar is <span style='color:#59A14F;font-weight:bold;'>green</span>, land use change leads
        to a net removal of CO\u2082.
        When the bar is <span style='color:#EF4444;font-weight:bold;'>red</span>, land use change generates
        net CO\u2082 emissions.
        </p>"
      )
      
      htmltools::HTML(txt)
    }) %>%
      bindCache(r_country(), scen_set_key())
  })
}
