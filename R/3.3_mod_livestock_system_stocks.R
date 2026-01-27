# R/3.3_mod_livestock_stocks.R
# ======================================================================
# DROP-IN REPLACEMENT — same calls from app.R
#   mod_livestock_stocks_ui("livestock_stocks")
#   mod_livestock_stocks_server(id="livestock_stocks", fact=..., r_country=..., r_scenarios=...)
#
# Compare baseline 2018 ("Année de base") vs one selected scenario
#   - year auto: 2050 if available else max year
#
# System-level composition (System filled) — Ruminants only:
#   - Beef cattle
#   - Dairy cattle
#   - Dairy sheep and goats
#   - Meat sheep and goats
#
# Included fixes:
#   1) Dairy disaggregation based on Item, BUT:
#        if Animal == "Dairy" AND Item == "Beef cattle" -> keep as "Beef cattle"
#   2) Local colors for stacked bars + pies (no external palette dependency).
#   3) Pies react to checkbox selection AND Plotly legend hide/show on bars
#      (robust JS capture; no plotly::event_data warnings).
#   4) Y axis headroom: ymax = 1.10 * max(total stacked per bar).
#   5) Enforce Plotly stacking to prevent occasional disappearance of traces.
#   6) Axis spacing tuning (requested):
#        - reduce left blank space (smaller margins + small title standoff)
#        - increase gap between y tick labels and first stacked bar
#          via x discrete expansion (padding before first category)
#        - visible vertical separator line between BASE and SCEN
#
# Requested change:
#   - Remove "VS Base year" from Base-year box/caption. (Keep only "Base-year")
# ======================================================================

suppressPackageStartupMessages({
  library(shiny)
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(forcats)
  library(ggplot2)
  library(plotly)
  library(scales)
  library(readr)
  library(htmlwidgets)
})

# ---- Local helpers -------------------------------------------------------
`%||%` <- function(a, b) if (is.null(a)) b else a
.get_fact <- function(x) if (is.function(x)) x() else x

LS_SYSTEM_LEVELS <- c("MixedArid","MixedNonArid","PastoralArid","PastoralNonArid","Specific")
BASE_SCENARIO    <- "Année de base"
BASE_YEAR        <- 2018L

RUMINANT_GROUPS <- c(
  "Beef cattle",
  "Dairy cattle",
  "Dairy sheep and goats",
  "Meat sheep and goats"
)

# ---- Colors (EDIT HERE) --------------------------------------------------
COL_RUMINANTS <- c(
  "Beef cattle"           = "#E73121",
  "Dairy cattle"          = "#4E79A7",
  "Dairy sheep and goats" = "#76B7B2",
  "Meat sheep and goats"  = "#FF9793"
)

COL_SYSTEMS <- c(
  "MixedArid"       = "#E8C547",
  "MixedNonArid"    = "#59A14F",
  "PastoralArid"    = "#F28E2B",
  "PastoralNonArid" = "#8CD17D",
  "Specific"        = "#AF7AA1"
)

.empty_plot <- function(msg){
  plotly::plot_ly(
    type = "scatter", mode = "markers",
    x = NA, y = NA, hoverinfo = "none", showlegend = FALSE
  ) %>%
    plotly::layout(
      xaxis = list(visible = FALSE),
      yaxis = list(visible = FALSE),
      annotations = list(list(
        text = msg, xref = "paper", yref = "paper",
        x = 0.5, y = 0.5, showarrow = FALSE,
        font = list(size = 14)
      )),
      paper_bgcolor = "rgba(0,0,0,0)",
      plot_bgcolor  = "rgba(0,0,0,0)",
      margin = list(l = 10, r = 10, t = 10, b = 10)
    )
}

# ---- UI ------------------------------------------------------------------
# KEEP THE SAME EXPORTED NAMES as in app.R:
#   mod_livestock_stocks_ui / mod_livestock_stocks_server
mod_livestock_system_stocks_ui <- function(
    id,
    title = "Ruminant herd composition by livestock system"
){
  ns <- NS(id)
  
  caption_style <- paste(
    "margin-top:6px;",
    "text-align:center;",
    "font-weight:600;",
    "font-size:13px;"
  )
  
  tagList(
    div(class = "card",
        div(class = "card-body",
            h2(class = "card-title", title),
            tags$br(),
            fluidRow(
              column(
                width = 5,
                selectInput(
                  ns("scen_cmp"),
                  "Choose a scenario to compare with base-year",
                  choices = character(0)
                )
              ),
              column(
                width = 7,
                checkboxGroupInput(
                  ns("rum_groups"),
                  "Ruminant groups (show/hide)",
                  choices  = RUMINANT_GROUPS,
                  selected = RUMINANT_GROUPS,
                  inline   = TRUE
                )
              )
            ),
            
            plotlyOutput(ns("p_bars"), height = "520px"),
            
            h3(style = "margin-top: 6px; font-weight:700; font-size:14px;",
               "Share of systems in total selected ruminant stocks (%)"),
            tags$br(),
            fluidRow(
              column(
                width = 6,
                plotlyOutput(ns("p_pie_base"), height = "320px"),
                tags$br(),
                # >>> Requested: no "VS Base year" anywhere, keep only this label
                div(style = caption_style, "Base-year")
              ),
              column(
                width = 6,
                plotlyOutput(ns("p_pie_scen"), height = "320px"),
                tags$br(),
                div(style = caption_style, uiOutput(ns("caption_scen_short")))
              )
            ),
            
            div(
              class = "u-actions",
              downloadLink(
                ns("dl_livestock_system_csv"),
                label = tagList(icon("download"), "CSV")
              )
            ),
            uiOutput(ns("note"))
        )
    )
  )
}

# ---- SERVER ---------------------------------------------------------------
mod_livestock_system_stocks_server <- function(
    id,
    fact,
    r_country,
    r_scenarios = shiny::reactive(NULL)
){
  moduleServer(id, function(input, output, session){
    
    # ---- Scenario set (effective list) -----------------------------------
    scenario_levels_all <- if (exists("SCENARIO_LEVELS_DEFAULT", inherits = TRUE)) {
      get("SCENARIO_LEVELS_DEFAULT", inherits = TRUE)
    } else {
      sort(unique(.get_fact(fact)$Scenario))
    }
    
    scen_levels_effective <- reactive({
      scen_all <- scenario_levels_all
      if (!is.null(r_scenarios) && !is.null(r_scenarios())) {
        scen_all <- intersect(scen_all, r_scenarios())
      }
      scen_all
    })
    
    # ---- Palettes ---------------------------------------------------------
    group_palette_for <- function(groups_vec){
      groups_vec <- intersect(RUMINANT_GROUPS, groups_vec)
      if (!length(groups_vec)) groups_vec <- RUMINANT_GROUPS
      cols <- COL_RUMINANTS[groups_vec]
      if (any(is.na(cols))) cols[is.na(cols)] <- scales::hue_pal()(sum(is.na(cols)))
      setNames(cols, groups_vec)
    }
    
    pal_sys <- local({
      out <- COL_SYSTEMS
      miss <- LS_SYSTEM_LEVELS[is.na(out[LS_SYSTEM_LEVELS])]
      if (length(miss)) out[miss] <- scales::hue_pal()(length(miss))
      out[LS_SYSTEM_LEVELS]
    })
    
    # ---- Legend-driven filtering state (no plotly::event_data) ------------
    rv_hidden_groups <- reactiveVal(character(0))
    
    observeEvent(input$rum_groups, {
      rv_hidden_groups(character(0))
    }, ignoreInit = TRUE)
    
    observeEvent(list(r_country(), input$scen_cmp), {
      rv_hidden_groups(character(0))
    }, ignoreInit = TRUE)
    
    observeEvent(input$bars_legend_evt, {
      ed <- input$bars_legend_evt
      req(is.list(ed), ed$type, ed$name)
      
      nm  <- as.character(ed$name)
      sel <- intersect(RUMINANT_GROUPS, input$rum_groups %||% RUMINANT_GROUPS)
      if (!(nm %in% sel)) return()
      
      if (identical(ed$type, "click")) {
        hid <- rv_hidden_groups()
        hid <- if (nm %in% hid) setdiff(hid, nm) else union(hid, nm)
        rv_hidden_groups(intersect(hid, sel))
      }
      
      if (identical(ed$type, "doubleclick")) {
        target_hid <- setdiff(sel, nm)
        if (setequal(rv_hidden_groups(), target_hid)) {
          rv_hidden_groups(character(0))
        } else {
          rv_hidden_groups(target_hid)
        }
      }
    }, ignoreInit = TRUE)
    
    groups_for_pies <- reactive({
      sel <- intersect(RUMINANT_GROUPS, input$rum_groups %||% RUMINANT_GROUPS)
      if (!length(sel)) sel <- RUMINANT_GROUPS
      setdiff(sel, rv_hidden_groups())
    })
    
    # ---- Build clean df (heads) ------------------------------------------
    prep_df <- reactive({
      req(r_country())
      df0 <- .get_fact(fact)
      req(df0)
      
      need_cols <- c("Region","Scenario","Element","Item","Year","System","Animal","Unit","Value")
      miss <- setdiff(need_cols, names(df0))
      validate(need(length(miss) == 0, paste("Missing columns:", paste(miss, collapse = ", "))))
      
      scen_allowed <- scen_levels_effective()
      
      df <- df0 %>%
        mutate(
          Element = str_trim(as.character(.data$Element)),
          Item    = str_trim(as.character(.data$Item)),
          System  = na_if(str_trim(as.character(.data$System)), ""),
          Animal  = na_if(str_trim(as.character(.data$Animal)), "")
        ) %>%
        filter(
          .data$Region  == r_country(),
          .data$Element == "Stocks",
          .data$Scenario %in% scen_allowed,
          .data$Unit %in% c("Head","1000 Head"),
          !is.na(.data$System)
        ) %>%
        mutate(Year = suppressWarnings(as.integer(.data$Year)))
      
      v_raw <- df$Value
      v_num <- suppressWarnings(as.numeric(v_raw))
      if (anyNA(v_num) && !is.numeric(v_raw)) {
        v_num <- readr::parse_number(
          as.character(v_raw),
          locale = readr::locale(decimal_mark = ",")
        )
      }
      
      df2 <- df %>%
        mutate(
          Value_num = v_num,
          mult      = if_else(.data$Unit == "1000 Head", 1000, 1),
          head      = .data$Value_num * .data$mult,
          Unit_out  = "Head",
          
          group_raw = coalesce(.data$Animal, .data$Item),
          animal_lc = str_to_lower(coalesce(.data$Animal, "")),
          item_lc   = str_to_lower(coalesce(.data$Item, "")),
          
          is_dairy  = (animal_lc == "dairy") | (str_to_lower(coalesce(.data$group_raw, "")) == "dairy"),
          
          group = case_when(
            # FIX 1: Dairy + Beef cattle stays Beef cattle
            is_dairy & str_detect(item_lc, "\\bbeef\\s+cattle\\b") ~ "Beef cattle",
            is_dairy & str_detect(item_lc, "\\bcattle\\b")         ~ "Dairy cattle",
            is_dairy & str_detect(item_lc, "\\bgoats\\b|\\bgoat\\b")~ "Dairy sheep and goats",
            is_dairy                                               ~ NA_character_,
            TRUE                                                   ~ .data$group_raw
          )
        )
      
      dairy_unclassified <- df2 %>%
        filter(.data$is_dairy, is.na(.data$group)) %>%
        distinct(.data$Item) %>%
        pull(.data$Item)
      
      validate(need(
        length(dairy_unclassified) == 0,
        paste0(
          "Some dairy rows could not be classified (Animal='dairy' but Item does not contain 'beef cattle', 'cattle' nor 'goat(s)'). ",
          "Examples of Item: ",
          paste(utils::head(dairy_unclassified, 6), collapse = " | "),
          if (length(dairy_unclassified) > 6) " | ..." else ""
        )
      ))
      
      df2 %>%
        filter(.data$group %in% RUMINANT_GROUPS) %>%
        mutate(
          System = factor(as.character(.data$System), levels = LS_SYSTEM_LEVELS),
          group  = factor(as.character(.data$group),  levels = RUMINANT_GROUPS)
        ) %>%
        filter(!is.na(.data$System)) %>%
        select(Region, Scenario, Year, System, group, Unit_out, head)
    })
    
    # ---- Populate scenario dropdown --------------------------------------
    observeEvent(list(r_country(), scen_levels_effective()), {
      scen_eff <- scen_levels_effective()
      choices  <- setdiff(scen_eff, BASE_SCENARIO)
      if (!length(choices)) choices <- scen_eff
      
      updateSelectInput(
        session, "scen_cmp",
        choices  = choices,
        selected = if (length(choices)) choices[1] else character(0)
      )
    }, ignoreInit = FALSE)
    
    # scenario year: 2050 if available else max
    year_cmp <- reactive({
      req(input$scen_cmp)
      d <- prep_df()
      
      y <- d %>%
        filter(.data$Scenario == input$scen_cmp) %>%
        distinct(.data$Year) %>%
        pull(.data$Year)
      
      if (!length(y)) return(NA_integer_)
      y <- y[is.finite(y)]
      if (!length(y)) return(NA_integer_)
      
      if (2050L %in% y) return(2050L)
      suppressWarnings(as.integer(max(y, na.rm = TRUE)))
    })
    
    output$caption_scen_short <- renderUI({
      yc <- year_cmp()
      if (is.na(yc)) return(HTML(input$scen_cmp %||% ""))
      HTML(paste0(input$scen_cmp))
    })
    
    d_period <- reactive({
      req(input$scen_cmp)
      req(input$rum_groups)
      
      sel_groups <- intersect(RUMINANT_GROUPS, input$rum_groups)
      validate(need(length(sel_groups) > 0, "Select at least one ruminant group."))
      
      d <- prep_df()
      
      df_base <- d %>%
        filter(.data$Scenario == BASE_SCENARIO, .data$Year == BASE_YEAR, .data$group %in% sel_groups) %>%
        mutate(period = "BASE")
      
      yc <- year_cmp()
      validate(need(!is.na(yc), "No year available for selected scenario."))
      
      df_scen <- d %>%
        filter(.data$Scenario == input$scen_cmp, .data$Year == yc, .data$group %in% sel_groups) %>%
        mutate(period = "SCEN")
      
      out <- bind_rows(df_base, df_scen) %>%
        group_by(.data$period, .data$System, .data$group) %>%
        summarise(head = sum(.data$head, na.rm = TRUE), .groups = "drop") %>%
        mutate(
          period = factor(.data$period, levels = c("BASE","SCEN")),
          System = factor(.data$System, levels = LS_SYSTEM_LEVELS),
          group  = factor(as.character(.data$group), levels = sel_groups)
        )
      
      out %>%
        tidyr::complete(
          period = c("BASE","SCEN"),
          System = factor(LS_SYSTEM_LEVELS, levels = LS_SYSTEM_LEVELS),
          group  = factor(sel_groups, levels = sel_groups),
          fill   = list(head = 0)
        )
    })
    
    # ---- Bars -------------------------------------------------------------
    output$p_bars <- renderPlotly({
      req(input$scen_cmp)
      
      sel_groups <- intersect(RUMINANT_GROUPS, input$rum_groups %||% character(0))
      if (!length(sel_groups)) return(.empty_plot("Select at least one ruminant group."))
      
      yc <- year_cmp()
      if (is.na(yc)) return(.empty_plot("No year available for selected scenario."))
      
      d <- d_period()
      if (!nrow(d)) return(.empty_plot("No system-level data to display."))
      
      # THEME GLOBAL R/99
      th <- get_plotly_tokens()
      
      max_head <- max(d$head, na.rm = TRUE)
      if (!is.finite(max_head) || max_head <= 0) max_head <- 1
      
      if (max_head >= 1e6) {
        div <- 1e6; ylab <- "Million heads"
      } else if (max_head >= 1e3) {
        div <- 1e3; ylab <- "Thousand heads"
      } else {
        div <- 1; ylab <- "Heads"
      }
      
      d2 <- d %>% mutate(y = .data$head / div)
      
      ymax_stack <- d2 %>%
        group_by(.data$period, .data$System) %>%
        summarise(y_tot = sum(.data$y, na.rm = TRUE), .groups = "drop") %>%
        summarise(mx = max(.data$y_tot, na.rm = TRUE), .groups = "drop") %>%
        pull(.data$mx)
      
      if (!is.finite(ymax_stack) || ymax_stack <= 0) ymax_stack <- 1
      ymax <- 1.10 * ymax_stack
      
      pal <- group_palette_for(sel_groups)
      
      mk <- function(dd, show_legend, y_title){
        
        gg <- ggplot(dd, aes(
          x = System, y = y, fill = group,
          text = paste0(
            "System: ", System, "<br>",
            "Group: ", group, "<br>",
            "Stock: ", scales::comma(head), " heads"
          )
        )) +
          geom_col(width = 0.72) +
          scale_x_discrete(drop = FALSE, expand = expansion(add = c(0.55, 1.10))) +
          scale_fill_manual(values = pal, drop = FALSE) +
          scale_y_continuous(labels = scales::label_number(accuracy = 0.1)) +
          coord_cartesian(ylim = c(0, ymax), expand = FALSE) +
          labs(x = NULL, y = y_title, fill = NULL) +
          theme_minimal(base_size = 11) +
          theme(
            legend.position   = if (show_legend) "right" else "none",
            panel.background  = element_rect(fill = "transparent", colour = NA),
            plot.background   = element_rect(fill = "transparent", colour = NA),
            panel.border      = element_rect(colour = th$axis_linecolor, fill = NA, linewidth = 0.4),
            legend.background = element_rect(fill = "transparent", colour = NA),
            legend.key        = element_rect(fill = "transparent", colour = NA),
            axis.text.x       = element_text(hjust = 0.5, color = th$muted_color),
            axis.text.y       = element_text(color = th$muted_color),
            axis.title.y      = element_text(margin = margin(r = 6), color = th$muted_color)
          )
        
        pl <- plotly::ggplotly(gg, tooltip = "text") %>%
          layout(
            barmode = "stack",
            xaxis = list(
              ticklabelposition = "outside",
              tickpadding = 6,
              ticklen = 6,
              automargin = TRUE,
              title = list(standoff = 2)
            ),
            yaxis = list(
              ticklabelposition = "outside",
              tickpadding = 6,
              ticklen = 6,
              automargin = TRUE,
              title = list(standoff = 2)
            ),
            margin = list(l = 40, r = 24, b = 36, t = 8, pad = 0)
          )
        
        pl <- plotly_apply_global_theme(pl, bg = "transparent", grid = "y")
        pl
      }
      
      p_left  <- mk(d2 %>% filter(period == "BASE"), show_legend = FALSE, y_title = ylab)
      p_right <- mk(d2 %>% filter(period == "SCEN"), show_legend = TRUE,  y_title = "") %>%
        layout(yaxis = list(title = list(text = "")))
      
      p <- plotly::subplot(p_left, p_right, nrows = 1, shareY = TRUE, titleX = TRUE, titleY = TRUE) %>%
        layout(
          barmode = "stack",
          margin = list(l = 65, r = 20, t = 20, b = 90),
          
          xaxis  = list(ticklabelposition="outside", tickpadding=6, ticklen=6, automargin=TRUE, title=list(standoff=2)),
          xaxis2 = list(ticklabelposition="outside", tickpadding=6, ticklen=6, automargin=TRUE, title=list(standoff=2)),
          yaxis  = list(ticklabelposition="outside", tickpadding=6, ticklen=6, automargin=TRUE, title=list(standoff=2)),
          
          shapes = list(
            list(
              type  = "line",
              layer = "above",
              xref  = "paper", yref = "paper",
              x0 = 0.5, x1 = 0.5,
              y0 = 0.04, y1 = 0.96,
              line = list(color = th$baseline_color, width = 2)
            )
          ),
          
          annotations = list(
            list(
              text = "Base-year",
              x = 0.18, y = 1, xref = "paper", yref = "paper",
              showarrow = FALSE, font = list(size = 14)
            ),
            list(
              text = paste0(input$scen_cmp),
              x = 0.78, y = 1, xref = "paper", yref = "paper",
              showarrow = FALSE, font = list(size = 14)
            ),
            list(
              x = 0.513, y = 0.50, xref = "paper", yref = "paper",
              ax = -30, ay = 0,
              showarrow  = TRUE,
              arrowhead  = 3,
              arrowsize  = 1.3,
              arrowwidth = 2.5,
              arrowcolor = th$baseline_color,
              text = ""
            )
          )
        ) %>%
        config(
          displayModeBar = TRUE,
          modeBarButtonsToRemove = c(
            "lasso2d","select2d","zoom2d","zoomIn2d","zoomOut2d",
            "autoScale2d","pan2d","resetScale2d"
          )
        )
      
      p <- plotly_apply_global_theme(p, bg = "transparent", grid = "y")
      
      # Fix legend duplication
      seen <- character(0)
      p$x$data <- lapply(p$x$data, function(tr){
        xa <- tr$xaxis %||% "x"
        nm <- tr$name  %||% ""
        if (xa == "x") {
          tr$showlegend <- FALSE
          return(tr)
        }
        if (nzchar(nm) && nm %in% seen) {
          tr$showlegend <- FALSE
        } else if (nzchar(nm)) {
          tr$showlegend <- TRUE
          seen <<- c(seen, nm)
        } else {
          tr$showlegend <- FALSE
        }
        tr
      })
      
      # Capture legend interactions via JS
      inp_id <- session$ns("bars_legend_evt")
      p <- htmlwidgets::onRender(
        p,
        sprintf(
          "function(el, x){
var gd = document.getElementById(el.id);
if(!gd) return;

if(gd.__legendHandlersAttached) return;
gd.__legendHandlersAttached = true;

function traceNameFromEvent(e){
  if(!gd.data || typeof e.curveNumber === 'undefined') return null;
  var tr = gd.data[e.curveNumber];
  if(!tr) return null;
  return tr.name || null;
}

gd.on('plotly_legendclick', function(e){
  var nm = traceNameFromEvent(e);
  if(window.Shiny && nm){
    Shiny.setInputValue('%s', {type:'click', name:nm, ts:Date.now()}, {priority:'event'});
  }
});

gd.on('plotly_legenddoubleclick', function(e){
  var nm = traceNameFromEvent(e);
  if(window.Shiny && nm){
    Shiny.setInputValue('%s', {type:'doubleclick', name:nm, ts:Date.now()}, {priority:'event'});
  }
});
}", inp_id, inp_id)
      )
      
      p
    })
    
    # ---- Pies -------------------------------------------------------------
    pie_data <- reactive({
      req(input$scen_cmp, input$rum_groups)
      yc <- year_cmp()
      req(!is.na(yc))
      
      g_pie <- groups_for_pies()
      validate(need(length(g_pie) > 0, "No ruminant group visible for pies (all hidden)."))
      
      d_period() %>%
        filter(.data$group %in% g_pie) %>%
        group_by(.data$period, .data$System) %>%
        summarise(head = sum(.data$head, na.rm = TRUE), .groups = "drop") %>%
        mutate(System = factor(.data$System, levels = LS_SYSTEM_LEVELS)) %>%
        tidyr::complete(
          period = c("BASE","SCEN"),
          System = factor(LS_SYSTEM_LEVELS, levels = LS_SYSTEM_LEVELS),
          fill   = list(head = 0)
        )
    })
    
    make_pie <- function(dd, show_legend = FALSE){
      th <- get_plotly_tokens()
      
      total <- sum(dd$head, na.rm = TRUE)
      dd2 <- dd %>%
        mutate(
          pct100  = if (is.finite(total) && total > 0) 100 * head / total else 0,
          pct_txt = ifelse(pct100 < 0.05, "", sprintf("%.1f%%", pct100))
        )
      
      p <- plotly::plot_ly(
        data   = dd2,
        type   = "pie",
        labels = ~System,
        values = ~head,
        text   = ~pct_txt,
        textinfo = "text",
        textfont = list(color = th$font_color, size = 12),
        marker = list(colors = unname(pal_sys[as.character(dd2$System)])),
        hovertemplate = paste0(
          "%{label}<br>",
          "%{customdata:.1f}%<br>",
          "%{value:,.0f} heads",
          "<extra></extra>"
        ),
        customdata = ~pct100,
        showlegend = show_legend
      ) %>%
        plotly::layout(
          margin = list(l = 10, r = 10, t = 10, b = 10),
          legend = list(
            orientation = "v",
            x = 1.02, y = 1,
            font = list(color = th$muted_color)
          )
        ) %>%
        plotly::config(displayModeBar = FALSE)
      
      p <- plotly_apply_global_theme(p, bg = "transparent", grid = "none")
      p
    }
    
    output$p_pie_base <- renderPlotly({
      req(input$scen_cmp, input$rum_groups)
      if (!length(groups_for_pies())) return(.empty_plot("No ruminant group visible for pies (all hidden)."))
      d <- pie_data() %>% filter(.data$period == "BASE")
      if (!nrow(d) || sum(d$head, na.rm = TRUE) <= 0) return(.empty_plot("No data for current selection."))
      make_pie(d, show_legend = FALSE)
    })
    
    output$p_pie_scen <- renderPlotly({
      req(input$scen_cmp, input$rum_groups)
      if (!length(groups_for_pies())) return(.empty_plot("No ruminant group visible for pies (all hidden)."))
      d <- pie_data() %>% filter(.data$period == "SCEN")
      if (!nrow(d) || sum(d$head, na.rm = TRUE) <= 0) return(.empty_plot("No data for current selection."))
      make_pie(d, show_legend = TRUE)
    })
    
    # ---- CSV export -------------------------------------------------------
    output$dl_livestock_system_csv <- downloadHandler(
      filename = function() {
        paste0("Livestock_ruminants_by_system_", gsub("[^A-Za-z0-9]+","_", r_country()), ".csv")
      },
      content = function(file) {
        req(input$scen_cmp, input$rum_groups)
        yc <- year_cmp()
        req(!is.na(yc))
        
        sel_groups <- intersect(RUMINANT_GROUPS, input$rum_groups)
        req(length(sel_groups) > 0)
        
        df <- prep_df() %>%
          filter(.data$group %in% sel_groups) %>%
          filter(
            (.data$Scenario == BASE_SCENARIO & .data$Year == BASE_YEAR) |
              (.data$Scenario == input$scen_cmp & .data$Year == yc)
          ) %>%
          mutate(
            System = as.character(.data$System),
            group  = as.character(.data$group),
            Unit   = "Head"
          ) %>%
          select(Region, Scenario, Year, System, group, Unit, head)
        
        df_sys_tot <- df %>%
          group_by(Region, Scenario, Year, System) %>%
          summarise(head = sum(head, na.rm = TRUE), .groups = "drop") %>%
          mutate(group = "ALL_SELECTED_RUMINANTS", Unit = "Head") %>%
          select(Region, Scenario, Year, System, group, Unit, head)
        
        final <- bind_rows(
          df %>% mutate(dataset = "system_x_group"),
          df_sys_tot %>% mutate(dataset = "system_total")
        ) %>%
          select(dataset, Region, Scenario, Year, System, group, Unit, head)
        
        readr::write_delim(final, file, delim = ";", na = "")
      }
    )
    
    # ---- Note -------------------------------------------------------------
    output$note <- renderUI({
      txt <- paste0(
        "<p>",
        "<strong>How to read these charts:</strong> This module compares the <strong>2018 baseline</strong> (left) with the selected scenario (right).<br>",
        "The stacked bars show <strong>ruminant stocks (heads)</strong> by livestock system, with colours representing ruminant groups ",
        "(beef cattle, dairy cattle, dairy sheep &amp; goats, meat sheep &amp; goats).<br>",
        "The pie charts below show, for each period, the <strong>share of livestock systems</strong> in the <strong>total stocks</strong> ",
        "of the ruminant groups currently selected (in %). Checkbox selections apply to both bars and pies; if a group is hidden from the bar-chart legend, ",
        "the pies are recalculated using the remaining visible groups.",
        "</p>"
      )
      htmltools::HTML(txt)
    })
    
    invisible(list(
      scen_levels_effective = scen_levels_effective
    ))
  })
}
