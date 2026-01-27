# R/5.2_mod_energy_balance.R
# -------------------------------------------------
# Energy balance by scenario
#  - 2 bars per scenario:
#      * "Sources"  = Production + Imports (+ possible balancing item)
#      * "Uses"     = Exports + all internal uses
#        (Food, Feed, Losses, Seed, Other uses, Unallocated/Residual)
#  - Bars are shown as PERCENTAGES within each bar (Sources and Uses separately)
#  - Checkboxes let you hide/show individual flows; percentages are always computed
#    on the full bar (before filtering visible flows)
#  - KPI cards show Sources/Uses (or Uses/Sources) computed on currently visible flows
# -------------------------------------------------

mod_energy_balance_ui <- function(id, wrap_in_card = TRUE){
  ns <- NS(id)
  
  content <- tagList(
    h3("Energy balance by scenario (in % of Gcal)"),
    
    uiOutput(ns("elements_selector")),
    plotly::plotlyOutput(ns("plot"), height = "390px"),
    uiOutput(ns("ratio_cards")),
    
    div(
      class = "u-actions",
      downloadLink(
        ns("dl_csv"),
        label = tagList(icon("download"), "CSV")
      )
    ),
    
    uiOutput(ns("note"))
  )
  
  if (isTRUE(wrap_in_card)) {
    div(class = "card", div(class = "card-body", content))
  } else {
    content
  }
}

mod_energy_balance_server <- function(
    id,
    fact,
    r_country,
    r_scenarios,   # reactive(): vector of SCENARIO CODES to display
    ...
){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    shiny::validate(
      shiny::need(exists("scenario_code", mode = "function"), "Missing scenario_code()."),
      shiny::need(exists("scenario_label", mode = "function"), "Missing scenario_label()."),
      shiny::need(exists("SCENARIO_LEVELS_DEFAULT", inherits = TRUE), "Missing SCENARIO_LEVELS_DEFAULT."),
      shiny::need(is.function(r_scenarios), "r_scenarios must be provided to this module.")
    )
    
    # toggle interne pour le bouton Select / Deselect all
    all_selected <- shiny::reactiveVal(TRUE)
    
    # Map label -> code (au cas où fact contient un label UI pour l'extra)
    EXTRA_LABEL_TO_CODE <- if (exists("SCENARIOS_EXTRA_CHOICES", inherits = TRUE)) {
      x <- get("SCENARIOS_EXTRA_CHOICES", inherits = TRUE)
      setNames(unname(x), names(x))
    } else {
      NULL
    }
    
    # ------------------------------------------------------------------
    # Helper: map Element -> Flow (centralisé pour éviter incohérences)
    # ------------------------------------------------------------------
    map_energy_flow <- function(element_chr){
      el <- stringr::str_to_lower(element_chr)
      
      dplyr::case_when(
        stringr::str_detect(el, "production")      ~ "Production",
        stringr::str_detect(el, "import")          ~ "Imports",
        stringr::str_detect(el, "export")          ~ "Exports",
        stringr::str_detect(el, "domestic supply") ~ "Domestic supply",
        stringr::str_detect(el, "other uses")      ~ "Other uses (non-food)",
        stringr::str_detect(el, "unallocated")     ~ "Unallocated",
        stringr::str_detect(el, "loss")            ~ "Losses",
        stringr::str_detect(el, "seed")            ~ "Seed",
        stringr::str_detect(el, "feed")            ~ "Feed",
        stringr::str_detect(el, "food")            ~ "Food",
        TRUE                                       ~ element_chr
      )
    }
    
    # ------------------------------------------------------------------
    # Scalar key for bindCache (NEVER pass the vector directly)
    # ------------------------------------------------------------------
    scen_show_key <- shiny::reactive({
      sc <- scenario_code(r_scenarios())
      paste(sc, collapse = "|")
    })
    
    # ------------------------------------------------------------------
    # Stable scenario levels for THIS module:
    # - global order: SCENARIO_LEVELS_DEFAULT
    # - restricted to wanted (r_scenarios)
    # - restricted to scenarios present in fact for this country & scope
    # ------------------------------------------------------------------
    scen_levels_effective <- shiny::reactive({
      req(r_country())
      wanted <- r_scenarios()
      shiny::validate(shiny::need(!is.null(wanted) && length(wanted) > 0, "No scenarios provided by r_scenarios()."))
      
      wanted <- scenario_code(wanted)
      lvls_default <- scenario_code(get("SCENARIO_LEVELS_DEFAULT", inherits = TRUE))
      
      present <- fact %>%
        dplyr::filter(
          Region == r_country(),
          stringr::str_starts(Element, "Energy "),
          Item == "All",
          Year %in% c(2018, 2050)
        ) %>%
        dplyr::mutate(
          Scenario = scenario_code(Scenario),
          Scenario = if (!is.null(EXTRA_LABEL_TO_CODE)) dplyr::recode(Scenario, !!!EXTRA_LABEL_TO_CODE, .default = Scenario) else Scenario
        ) %>%
        dplyr::pull(Scenario) %>%
        unique()
      
      lvls_default[lvls_default %in% wanted & lvls_default %in% present]
    }) %>% bindCache(r_country(), scen_show_key())
    
    # -------------------------------------------------
    # 1. Données brutes énergie (pays + années) + scénarios effectifs
    # -------------------------------------------------
    data_raw <- shiny::reactive({
      req(fact, r_country())
      lvls <- scen_levels_effective()
      shiny::validate(shiny::need(length(lvls) > 0, "No energy scenarios available for this country (after r_scenarios ∩ present)."))
      
      fact %>%
        dplyr::filter(
          Region == r_country(),
          stringr::str_starts(Element, "Energy "),
          Item == "All",
          Year %in% c(2018, 2050)
        ) %>%
        dplyr::mutate(
          Scenario = scenario_code(Scenario),
          Scenario = if (!is.null(EXTRA_LABEL_TO_CODE)) dplyr::recode(Scenario, !!!EXTRA_LABEL_TO_CODE, .default = Scenario) else Scenario,
          Scenario = factor(Scenario, levels = lvls),
          Flow     = map_energy_flow(Element)
        ) %>%
        dplyr::filter(!is.na(Scenario)) %>%
        dplyr::mutate(Scenario = forcats::fct_drop(Scenario))
    }) %>% bindCache(r_country(), scen_show_key())
    
    # -------------------------------------------------
    # 2. Données préparées :
    #    - agrégation par (Scenario, Group, Flow, Unit)
    #    - ajout automatique d'un item d'équilibrage si Sources != Uses
    #    - calcul des parts % par barre (avant filtrage UI)
    # -------------------------------------------------
    data_prepared <- shiny::reactive({
      df <- data_raw()
      req(nrow(df) > 0)
      
      source_flows <- c("Production", "Imports")
      
      # Retire "Domestic supply" (informative, pas une composante du bilan)
      df <- df %>%
        dplyr::filter(Flow != "Domestic supply") %>%
        dplyr::mutate(
          Group = dplyr::case_when(
            Flow %in% source_flows ~ "Sources",
            TRUE                   ~ "Uses"
          ),
          Group = factor(Group, levels = c("Sources", "Uses"))
        )
      
      df_sum <- df %>%
        dplyr::group_by(Scenario, Group, Flow, Unit) %>%
        dplyr::summarise(
          Value = sum(Value, na.rm = TRUE),
          .groups = "drop"
        )
      
      # --- Ajout "Unallocated" (si absent) ou "Residual (balancing)" (si Unallocated existe déjà)
      # Tolérance relative (évite de créer un résidu sur du bruit d'arrondi)
      tot <- df_sum %>%
        dplyr::group_by(Scenario, Unit) %>%
        dplyr::summarise(
          total_sources = sum(Value[Group == "Sources"], na.rm = TRUE),
          total_uses    = sum(Value[Group == "Uses"],    na.rm = TRUE),
          has_unalloc   = any(Flow == "Unallocated"),
          .groups = "drop"
        ) %>%
        dplyr::mutate(
          gap = total_sources - total_uses,
          tol = 1e-6 * pmax(abs(total_sources), abs(total_uses), 1),
          need_balance = abs(gap) > tol
        )
      
      balancing_rows <- tot %>%
        dplyr::filter(need_balance) %>%
        dplyr::mutate(
          Flow  = dplyr::if_else(!has_unalloc, "Unallocated", "Residual (balancing)"),
          Group = dplyr::if_else(gap >= 0, "Uses", "Sources"),
          Value = abs(gap)
        ) %>%
        dplyr::select(Scenario, Group, Flow, Unit, Value) %>%
        dplyr::mutate(Group = factor(Group, levels = c("Sources", "Uses")))
      
      if (nrow(balancing_rows) > 0) {
        df_sum <- dplyr::bind_rows(df_sum, balancing_rows)
      }
      
      # Ordre de présentation : on garde l'ordre “connu” puis on ajoute les flux “extra”
      flow_order_known <- c(
        "Production", "Imports",
        "Exports",
        "Food", "Feed", "Losses", "Seed",
        "Other uses (non-food)",
        "Unallocated",
        "Residual (balancing)"
      )
      flow_extra <- sort(setdiff(unique(df_sum$Flow), flow_order_known))
      flow_levels <- c(flow_order_known[flow_order_known %in% unique(df_sum$Flow)], flow_extra)
      
      df_sum <- df_sum %>%
        dplyr::mutate(Flow = factor(Flow, levels = flow_levels))
      
      df_sum %>%
        dplyr::group_by(Scenario, Group) %>%
        dplyr::mutate(
          group_total = sum(Value, na.rm = TRUE),
          Share       = dplyr::if_else(group_total > 0, 100 * Value / group_total, NA_real_),
          Value_m     = Value / 1e6
        ) %>%
        dplyr::ungroup()
    }) %>% bindCache(r_country(), scen_show_key())
    
    # -------------------------------------------------
    # 3. Liste des flux par groupe (dérivée des données préparées)
    #    -> garantit que "Other uses (non-food)" et l'item d'équilibrage apparaissent
    # -------------------------------------------------
    flow_lists <- shiny::reactive({
      df <- data_prepared()
      req(nrow(df) > 0)
      
      list(
        sources = df %>% dplyr::filter(Group == "Sources") %>% dplyr::pull(Flow) %>% as.character() %>% unique(),
        uses    = df %>% dplyr::filter(Group == "Uses")    %>% dplyr::pull(Flow) %>% as.character() %>% unique()
      )
    })
    
    # -------------------------------------------------
    # 4. Cases à cocher : flux Sources vs Uses + bouton toggle
    # -------------------------------------------------
    output$elements_selector <- shiny::renderUI({
      fl <- flow_lists()
      src <- fl$sources
      use <- fl$uses
      
      tagList(
        div(
          style = "margin-bottom:8px;",
          actionButton(
            inputId = ns("toggle_flows"),
            label   = if (isTRUE(all_selected())) "Deselect all flows" else "Select all flows",
            class   = "btn btn-default btn-sm"
          )
        ),
        div(
          style = "display:flex; gap:24px; align-items:flex-start; flex-wrap:wrap;",
          div(
            style = "min-width:160px;",
            tags$strong("Sources"),
            checkboxGroupInput(
              inputId  = ns("elements_sources"),
              label    = NULL,
              choices  = src,
              selected = src
            )
          ),
          div(
            style = "min-width:220px;",
            tags$strong("Uses"),
            checkboxGroupInput(
              inputId  = ns("elements_uses"),
              label    = NULL,
              choices  = use,
              selected = use
            )
          )
        )
      )
    })
    
    observeEvent(input$toggle_flows, {
      fl <- flow_lists()
      src <- fl$sources
      use <- fl$uses
      
      if (isTRUE(all_selected())) {
        updateCheckboxGroupInput(session, "elements_sources", selected = character(0))
        updateCheckboxGroupInput(session, "elements_uses",    selected = character(0))
        all_selected(FALSE)
      } else {
        updateCheckboxGroupInput(session, "elements_sources", selected = src)
        updateCheckboxGroupInput(session, "elements_uses",    selected = use)
        all_selected(TRUE)
      }
      
      new_label <- if (isTRUE(all_selected())) "Deselect all flows" else "Select all flows"
      updateActionButton(session, "toggle_flows", label = new_label)
    }, ignoreNULL = TRUE)
    
    r_flows_selected <- shiny::reactive({
      c(input$elements_sources %||% character(0),
        input$elements_uses    %||% character(0))
    })
    
    # -------------------------------------------------
    # 5. Résumé : Sources / Uses par scénario (flux visibles)
    # -------------------------------------------------
    ratio_data <- shiny::reactive({
      df_all <- data_prepared()
      if (nrow(df_all) == 0) return(df_all[0, ])
      
      flows_selected <- r_flows_selected()
      if (length(flows_selected) == 0) return(df_all[0, ])
      
      df_vis <- df_all %>% dplyr::filter(as.character(Flow) %in% flows_selected)
      if (nrow(df_vis) == 0) return(df_vis[0, ])
      
      df_vis %>%
        dplyr::group_by(Scenario, Group) %>%
        dplyr::summarise(total = sum(Value, na.rm = TRUE), .groups = "drop") %>%
        tidyr::pivot_wider(names_from = Group, values_from = total) %>%
        dplyr::mutate(
          ratio_SU = dplyr::if_else(!is.na(Sources) & !is.na(Uses) & Uses > 0,    100 * Sources / Uses, NA_real_),
          ratio_US = dplyr::if_else(!is.na(Sources) & !is.na(Uses) & Sources > 0, 100 * Uses / Sources, NA_real_)
        )
    })
    
    output$ratio_cards <- shiny::renderUI({
      df <- ratio_data()
      if (is.null(df) || nrow(df) == 0) return(NULL)
      
      mode <- input$ratio_mode
      if (is.null(mode)) mode <- "SU"
      
      ratio_label <- if (mode == "US") "Uses / Sources" else "Sources / Uses"
      
      tagList(
        selectInput(
          inputId = ns("ratio_mode"),
          label   = "Ratio displayed:",
          choices = c("Sources / Uses" = "SU", "Uses / Sources" = "US"),
          selected = mode
        ),
        div(
          id    = ns("ratio_cards_root"),
          class = "energy-kpi",
          div(
            class = "u-row",
            lapply(seq_len(nrow(df)), function(i){
              scen_code <- as.character(df$Scenario[i])
              scen_lbl  <- scenario_label(scen_code)
              
              val <- if (mode == "US") df$ratio_US[i] else df$ratio_SU[i]
              label_val <- if (is.na(val)) "—" else paste0(round(val), "%")
              
              div(
                class = "u-card u-card--flat u-card--hover",
                div(
                  class = "u-box",
                  p(class = "u-title", scen_lbl),
                  p(class = "u-value", label_val, tags$span(class = "u-unit", ratio_label)),
                  p(class = "u-sub", "Based on currently visible flows")
                )
              )
            }) |> do.call(what = tagList)
          )
        )
      )
    })
    
    # -------------------------------------------------
    # 6. Graphique (2 barres en % par scénario)
    # -------------------------------------------------
    output$plot <- plotly::renderPlotly({
      df_all <- data_prepared()
      if (nrow(df_all) == 0) return(NULL)
      
      flows_selected <- r_flows_selected()
      if (length(flows_selected) == 0) return(NULL)
      
      th <- if (exists("get_plotly_tokens", mode = "function")) get_plotly_tokens() else list(
        font_color       = "#111827",
        muted_color      = "#6B7280",
        gridcolor        = "rgba(0,0,0,.15)",
        axis_linecolor   = "rgba(0,0,0,.18)"
      )
      gg_txt   <- th$font_color %||% "#111827"
      gg_grid  <- th$gridcolor %||% "rgba(0,0,0,.15)"
      
      offset    <- 0.15
      bar_width <- 0.28
      
      df_all <- df_all %>%
        dplyr::mutate(
          scen_idx = as.numeric(Scenario),
          x_pos = dplyr::case_when(
            Group == "Sources" ~ scen_idx - offset,
            Group == "Uses"    ~ scen_idx + offset,
            TRUE               ~ scen_idx
          )
        )
      
      df <- df_all %>% dplyr::filter(as.character(Flow) %in% flows_selected)
      if (nrow(df) == 0) return(NULL)
      
      flow_levels <- levels(df_all$Flow)
      
      base_cols <- if (exists("sankey_node_palette", mode = "function")) sankey_node_palette() else NULL
      if (!is.null(base_cols)) {
        flow_colors <- base_cols[flow_levels]
        miss <- is.na(flow_colors)
        if (any(miss)) flow_colors[miss] <- scales::hue_pal()(sum(miss))
      } else {
        flow_colors <- setNames(scales::hue_pal()(length(flow_levels)), flow_levels)
      }
      
      unit_label <- paste(unique(df_all$Unit), collapse = ", ")
      
      scen_axis <- df_all %>%
        dplyr::distinct(Scenario, scen_idx) %>%
        dplyr::arrange(scen_idx)
      
      labels_df <- df_all %>%
        dplyr::distinct(Scenario, Group, scen_idx, x_pos) %>%
        dplyr::mutate(label_y = 103)
      
      gg <- ggplot2::ggplot(
        df,
        ggplot2::aes(
          x = x_pos,
          y = Share,
          fill = Flow,
          text = paste0(
            "Scenario: ", scenario_label(as.character(Scenario)), "<br>",
            "Bar: ", as.character(Group), "<br>",
            "Flow: ", as.character(Flow), "<br>",
            "Share within bar: ", sprintf("%.0f%%", Share), "<br>",
            "Approx. volume: ", scales::comma(Value_m, accuracy = 0.1), " million ", unit_label
          )
        )
      ) +
        ggplot2::geom_col(width = bar_width) +
        ggplot2::geom_text(
          data = labels_df,
          ggplot2::aes(x = x_pos, y = label_y, label = Group),
          inherit.aes = FALSE,
          size  = 3.5,
          vjust = 0,
          colour = gg_txt
        ) +
        ggplot2::scale_fill_manual(values = flow_colors) +
        ggplot2::scale_x_continuous(
          breaks = scen_axis$scen_idx,
          labels = scenario_label(as.character(scen_axis$Scenario)),
          expand = ggplot2::expansion(mult = c(0.02, 0.02))
        ) +
        ggplot2::scale_y_continuous(
          labels = scales::label_percent(accuracy = 1, scale = 1),
          limits = c(0, 110),
          expand = ggplot2::expansion(mult = c(0, 0.05))
        ) +
        ggplot2::labs(x = NULL, y = "Share within each bar (%)") +
        ggplot2::theme_minimal(base_size = 13) +
        ggplot2::theme(
          axis.text.x      = ggplot2::element_text(angle = 0, hjust = 0.5, vjust = 1, colour = gg_txt),
          axis.text.y      = ggplot2::element_text(colour = gg_txt),
          axis.title.y     = ggplot2::element_text(colour = gg_txt),
          panel.grid.minor = ggplot2::element_blank(),
          panel.grid.major = ggplot2::element_line(colour = gg_grid),
          plot.background  = ggplot2::element_rect(fill = "transparent", colour = NA),
          panel.background = ggplot2::element_rect(fill = "transparent", colour = NA)
        )
      
      p <- plotly::ggplotly(gg, tooltip = "text")
      p <- plotly::layout(p, margin = list(l = 40, r = 20, t = 20, b = 60))
      
      if (exists("plotly_apply_global_theme", mode = "function")) {
        p <- plotly_apply_global_theme(p, bg = "transparent", grid = "y")
      } else if (exists("plotly_theme_transparent", mode = "function")) {
        p <- plotly_theme_transparent(p)
      }
      
      p
    })
    
    # -------------------------------------------------
    # 7. Download CSV
    # -------------------------------------------------
    output$dl_csv <- downloadHandler(
      filename = function(){
        paste0("energy_balance_", r_country(), ".csv")
      },
      content = function(file){
        df <- data_prepared()
        readr::write_csv(df, file)
      }
    )
    
    # -------------------------------------------------
    # 8. Note (inclut l’explication du balancing item)
    # -------------------------------------------------
    output$note <- renderUI({
      df <- data_prepared()
      if (nrow(df) == 0) return(NULL)
      
      unit_label <- paste(unique(df$Unit), collapse = ", ")
      
      has_balance <- any(as.character(df$Flow) %in% c("Unallocated", "Residual (balancing)"))
      
      balance_txt <- if (isTRUE(has_balance)) {
        "<br><em>Note:</em> When Sources and Uses do not match in the raw outputs, the module adds a small balancing item:
        <strong>Unallocated</strong> (if absent in the model outputs) or <strong>Residual (balancing)</strong> (if an Unallocated flow already exists),
        so that the full selection closes the balance (Sources ≈ Uses)."
      } else ""
      
      htmltools::HTML(glue::glue(
        "<p>
        This chart shows, for each scenario, two stacked bars that describe the
        <strong>energy balance</strong> of the agri-food system:<br>
        <ul>
          <li><strong>Sources</strong> (left): the share of <em>production</em> and <em>imports</em> in the total energy entering the system;</li>
          <li><strong>Uses</strong> (right): the share of <em>exports</em> and internal uses of energy within the agri-food system
              (food, feed, losses, seed, other non-food uses, etc.).</li>
        </ul>
        Heights are expressed in <strong>percentages</strong> within each bar.
        When you uncheck a flow in the list, its segment is hidden but the percentages still
        refer to the full bar.<br>
        The small cards below the chart indicate, for each scenario, a ratio between
        <strong>Sources</strong> and <strong>Uses</strong>, based on the flows currently visible
        in the chart. Volumes in the tooltips are given in <strong>million {unit_label}</strong>.
        {balance_txt}
        </p>"
      ))
    })
    
    invisible(NULL)
  })
}
