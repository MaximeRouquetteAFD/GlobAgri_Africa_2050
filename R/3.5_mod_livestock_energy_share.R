# R/3.5_mod_livestock_energy_share.R
# -------------------------------------------------
# Share of resources allocated to livestock
# (Energy Feed vs Energy Domestic supply quantity)
#
# Scenario logic (aligned):
# - Filter scenarios: EXACT r_scenarios() (codes) ∩ present
# - Order: SCENARIO_LEVELS_DEFAULT + deterministic append of unknown codes
# - Display: scenario_label(code) if available; otherwise code
# -------------------------------------------------

suppressPackageStartupMessages({
  library(shiny)
  library(dplyr)
  library(tidyr)
  library(plotly)
  library(scales)
  library(readr)
  library(glue)
  library(stringr)
})

`%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x

mod_livestock_energy_share_ui <- function(id, wrap_in_card = TRUE) {
  ns <- NS(id)
  
  content <- tagList(
    h2("Share of feed in total available domestic agricultural products (Gcal)"),
    plotly::plotlyOutput(ns("plot"), height = "450px"),
    
    div(
      class = "text-right",
      div(
        class = "u-actions",
        downloadLink(
          ns("dl_energy_share_csv"),
          label = tagList(icon("download"), "CSV")
        )
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


mod_livestock_energy_share_server <- function(
    id,
    fact,
    r_country,
    r_scenarios = shiny::reactive(NULL)  # reactive/function returning scenario CODES (or NULL)
) {
  moduleServer(id, function(input, output, session) {
    
    # --- Scenario label helper (display only) ------------------------------
    has_scen_label <- exists("scenario_label", mode = "function", inherits = TRUE)
    scenario_label_vec <- function(x){
      x <- as.character(x)
      if (has_scen_label) vapply(x, scenario_label, character(1)) else x
    }
    
    # --- Ordered requested scenario codes ---------------------------------
    scen_codes_ordered <- reactive({
      if (is.null(r_scenarios) || !is.function(r_scenarios)) return(character(0))
      sc <- unique(as.character(r_scenarios()))
      sc <- sc[!is.na(sc) & nzchar(sc)]
      if (!length(sc)) return(character(0))
      
      if (exists("SCENARIO_LEVELS_DEFAULT", inherits = TRUE)) {
        base <- get("SCENARIO_LEVELS_DEFAULT", inherits = TRUE)
        known   <- intersect(base, sc)
        unknown <- setdiff(sc, base)
        return(c(known, sort(unknown)))
      }
      sort(sc)
    })
    
    # --- Effective scenarios: wanted ∩ present (ordered) -------------------
    scen_levels_effective <- reactive({
      req(r_country())
      
      df0 <- fact %>%
        dplyr::filter(
          .data$Region == r_country(),
          .data$Element %in% c("Energy Domestic supply quantity", "Energy Feed"),
          .data$Item == "All",
          .data$Unit == "Gcal"
        ) %>%
        dplyr::distinct(.data$Scenario) %>%
        dplyr::pull(.data$Scenario) %>%
        as.character()
      
      df0 <- df0[!is.na(df0) & nzchar(df0)]
      if (!length(df0)) return(character(0))
      
      wanted <- scen_codes_ordered()
      
      if (length(wanted) > 0) {
        keep <- intersect(wanted, df0)
        return(wanted[wanted %in% keep])  # preserve wanted order
      }
      
      # fallback: present ordered by SCENARIO_LEVELS_DEFAULT if available
      if (exists("SCENARIO_LEVELS_DEFAULT", inherits = TRUE)) {
        base <- get("SCENARIO_LEVELS_DEFAULT", inherits = TRUE)
        known   <- intersect(base, df0)
        unknown <- setdiff(df0, base)
        return(c(known, sort(unknown)))
      }
      
      sort(df0)
    }) %>% bindCache(r_country(), paste0(scen_codes_ordered(), collapse = ","))
    
    # ----------------------------------------------------------------------
    # Data builder
    # ----------------------------------------------------------------------
    data_share <- reactive({
      req(r_country())
      
      scen_allowed <- scen_levels_effective()
      if (!length(scen_allowed)) {
        return(list(long = tibble(), wide = tibble()))
      }
      
      df_raw <- fact %>%
        dplyr::filter(
          .data$Region == r_country(),
          .data$Element %in% c("Energy Domestic supply quantity", "Energy Feed"),
          .data$Item == "All",
          .data$Unit == "Gcal",
          .data$Scenario %in% scen_allowed
        )
      
      if (!nrow(df_raw)) {
        return(list(long = tibble(), wide = tibble()))
      }
      
      # numeric safety for Value
      v_raw <- df_raw$Value
      v_num <- suppressWarnings(as.numeric(v_raw))
      if (anyNA(v_num) && !is.numeric(v_raw)) {
        v_num <- readr::parse_number(
          as.character(v_raw),
          locale = readr::locale(decimal_mark = ",")
        )
      }
      
      df <- df_raw %>%
        dplyr::mutate(Value_num = v_num) %>%
        dplyr::group_by(.data$Scenario, .data$Element) %>%
        dplyr::summarise(Value = sum(.data$Value_num, na.rm = TRUE), .groups = "drop")
      
      df_wide <- df %>%
        tidyr::pivot_wider(names_from = .data$Element, values_from = .data$Value)
      
      # Ensure both columns exist even if missing in some countries
      if (!("Energy Domestic supply quantity" %in% names(df_wide))) df_wide[["Energy Domestic supply quantity"]] <- NA_real_
      if (!("Energy Feed" %in% names(df_wide))) df_wide[["Energy Feed"]] <- NA_real_
      
      df_wide <- df_wide %>%
        dplyr::mutate(
          share_feed     = dplyr::if_else(
            is.finite(`Energy Domestic supply quantity`) & `Energy Domestic supply quantity` > 0 &
              is.finite(`Energy Feed`),
            `Energy Feed` / `Energy Domestic supply quantity`,
            NA_real_
          ),
          share_feed_pct = 100 * .data$share_feed,
          total_million  = `Energy Domestic supply quantity` / 1e6,
          feed_million   = `Energy Feed` / 1e6
        ) %>%
        dplyr::mutate(
          Scenario = factor(as.character(.data$Scenario), levels = scen_allowed),
          idx      = as.integer(.data$Scenario)
        ) %>%
        dplyr::arrange(.data$Scenario)
      
      df_long <- df_wide %>%
        tidyr::pivot_longer(
          cols = c("Energy Domestic supply quantity", "Energy Feed"),
          names_to  = "Element",
          values_to = "Value"
        ) %>%
        dplyr::mutate(
          Value_million = .data$Value / 1e6,
          Scenario_code = as.character(.data$Scenario),
          Scenario_lab  = scenario_label_vec(.data$Scenario_code),
          hover = paste0(
            "<b>", Scenario_lab, "</b><br>",
            Element, "<br>",
            "Value: ", format(round(Value, 0), big.mark = " ", decimal.mark = ","), " Gcal<br>",
            "Share feed / total: ",
            ifelse(is.finite(share_feed_pct), paste0(round(share_feed_pct, 1), " %"), "NA"),
            "<extra></extra>"
          )
        )
      
      list(long = df_long, wide = df_wide)
    }) %>% bindCache(r_country(), paste0(scen_levels_effective(), collapse = ","))
    
    # ----------------------------------------------------------------------
    # Plot
    # ----------------------------------------------------------------------
    output$plot <- plotly::renderPlotly({
      dat <- data_share()
      df_long <- dat$long
      df_wide <- dat$wide
      
      validate(need(nrow(df_long) > 0, "No data available for this country / scenario selection."))
      
      # R/99 tokens (if available)
      th <- if (exists("get_plotly_tokens", mode = "function", inherits = TRUE)) {
        get_plotly_tokens()
      } else {
        list(
          font_color     = "#111827",
          muted_color    = "#6B7280",
          axis_linecolor = "rgba(0,0,0,.18)"
        )
      }
      
      # Colors (prefer sankey palette if available, else defaults)
      col_total <- "#F28E2B"
      col_feed  <- "#59A14F"
      if (exists("sankey_node_palette", mode = "function", inherits = TRUE)) {
        pal_sankey <- sankey_node_palette()
        col_total  <- pal_sankey[["Domestic supply"]] %||% col_total
        col_feed   <- pal_sankey[["Feed"]]            %||% col_feed
      }
      
      df_total <- df_long %>% dplyr::filter(.data$Element == "Energy Domestic supply quantity")
      df_feed  <- df_long %>% dplyr::filter(.data$Element == "Energy Feed")
      
      tick_vals  <- df_wide$idx
      tick_texts <- scenario_label_vec(as.character(df_wide$Scenario))
      
      p <- plotly::plot_ly()
      
      if (nrow(df_total) > 0) {
        p <- p %>%
          plotly::add_bars(
            data = df_total,
            x    = ~idx,
            y    = ~Value_million,
            name = "Domestic supply",
            marker = list(color = col_total),
            hovertext = ~hover,
            hoverinfo = "text",
            textposition = "none",
            offsetgroup = "total"
          )
      }
      
      if (nrow(df_feed) > 0) {
        p <- p %>%
          plotly::add_bars(
            data = df_feed,
            x    = ~idx,
            y    = ~Value_million,
            name = "Feed",
            marker = list(color = col_feed),
            
            # ✅ hover only (no text inside bars)
            hovertext = ~hover,
            hoverinfo = "text",
            textposition = "none",
            
            offsetgroup = "feed"
          )
      }
      
      p <- p %>%
        plotly::layout(
          barmode = "group",
          xaxis = list(
            title    = "",
            tickmode = "array",
            tickvals = tick_vals,
            ticktext = tick_texts,
            zeroline = FALSE
          ),
          yaxis = list(
            title      = "Millions of Gcal",
            tickformat = ",.1f",
            zeroline   = FALSE
          ),
          legend = list(
            orientation = "h",
            x = 0.5, xanchor = "center",
            y = 1.20,
            title = list(text = "")
          ),
          margin = list(l = 70, r = 20, t = 80, b = 70)
        )
      
      # Horizontal dotted lines + % labels (at feed level)
      df_labels <- df_wide %>%
        dplyr::filter(
          is.finite(.data$idx),
          is.finite(.data$feed_million),
          is.finite(.data$share_feed_pct)
        )
      
      if (nrow(df_labels) > 0) {
        dotted_col <- th$axis_linecolor %||% "rgba(0,0,0,0.70)"
        
        shapes_list <- lapply(seq_len(nrow(df_labels)), function(i) {
          xi <- df_labels$idx[i]
          yi <- df_labels$feed_million[i]
          list(
            type = "line",
            xref = "x", yref = "y",
            x0 = xi - 0.25,
            x1 = xi + 0.25,
            y0 = yi,
            y1 = yi,
            line = list(dash = "dot", width = 1, color = dotted_col)
          )
        })
        
        p <- p %>% plotly::layout(shapes = shapes_list)
        
        p <- p %>%
          plotly::add_trace(
            data = df_labels,
            x    = ~idx,
            y    = ~(feed_million * 1.02),
            type = "scatter",
            mode = "text",
            text = ~paste0(round(share_feed_pct, 1), " %"),
            textposition = "top center",
            textfont = list(size = 13, color = th$font_color %||% "#111827"),
            showlegend = FALSE,
            hoverinfo  = "none"
          )
      }
      
      # Apply R/99 global theme if available
      if (exists("plotly_apply_global_theme", mode = "function", inherits = TRUE)) {
        p <- plotly_apply_global_theme(p, bg = "transparent", grid = "y")
      }
      
      p
    })
    
    # ----------------------------------------------------------------------
    # Note
    # ----------------------------------------------------------------------
    output$note <- renderUI({
      dat <- data_share()
      if (nrow(dat$long) == 0) return(NULL)
      
      txt <- glue::glue(
        "<p>
        This chart shows the <strong>share of energy allocated to livestock feed</strong> within the country’s
        total <em>domestic supply</em> of agricultural products (Gcal), by scenario.<br>
        The orange bar is the <em>total domestic supply</em>, and the green bar is the <em>energy used as feed</em>.
        Values are shown in <strong>millions of Gcal</strong>.<br>
        The horizontal dotted line and the percentage label indicate the <strong>feed share</strong>
        (Energy Feed / Energy Domestic supply quantity).
        </p>"
      )
      htmltools::HTML(txt)
    })
    
    # ----------------------------------------------------------------------
    # CSV export
    # ----------------------------------------------------------------------
    output$dl_energy_share_csv <- downloadHandler(
      filename = function() {
        paste0("Energy_share_", gsub("\\s+", "_", r_country()), ".csv")
      },
      content = function(file) {
        dat <- data_share()
        df_wide <- dat$wide
        req(nrow(df_wide) > 0)
        
        out <- df_wide %>%
          dplyr::transmute(
            Country             = r_country(),
            Scenario_code       = as.character(.data$Scenario),
            Scenario_label      = scenario_label_vec(as.character(.data$Scenario)),
            Domestic_supply_Gcal = `Energy Domestic supply quantity`,
            Feed_Gcal           = `Energy Feed`,
            Share_feed_pct      = .data$share_feed_pct
          )
        
        readr::write_delim(out, file, delim = ";", na = "")
      }
    )
    
  })
}
