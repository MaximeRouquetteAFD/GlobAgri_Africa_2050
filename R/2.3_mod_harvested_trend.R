# R/2.3_mod_harvested_trend.R
# ---------------------------------------------------------------
# Module : évolution 2018 → 2050 des surfaces récoltées
# - Vue "By product group" / "By scenario"
# - Utilise harvested_core() pour les calculs
# - Scénarios centralisés (config + helper global via r_scenarios)
# ---------------------------------------------------------------

mod_harvested_trend_ui <- function(id, height = "800px", full_width = TRUE){
  ns <- NS(id)
  div(
    class = if (isTRUE(full_width)) "card full-bleed" else "card",
    div(
      class = "card-body",
      
      h2("Focus on changes in harvested areas between 2018 and 2050"),
      
      div(
        class = "u-controls u-controls--inline",
        radioButtons(
          inputId = ns("facet_mode"),
          label   = "",
          choices = c("By product group" = "product",
                      "By scenario"      = "scenario"),
          selected = "product",
          inline   = TRUE
        )
      ),
      
      uiOutput(ns("small_crops_msg")),
      
      plotly::plotlyOutput(ns("harv_groups"), height = height, width = "100%"),
      
      div(
        class = "text-right",
        div(
          class = "u-actions",
          downloadLink(
            ns("dl_harvested_csv"),
            label = tagList(icon("download"), "CSV")
          )
        )
      ),
      uiOutput(ns("note"))
    )
  )
}


mod_harvested_trend_server <- function(
    id,
    fact,
    r_country,
    r_scenarios = NULL,   # <- NOUVEAU : scénarios effectifs (reactive) depuis app.R
    harvest_element = "Area harvested",
    exclude_items = c(
      "All products","All crops","Agricultural land occupation (Farm)",
      "Cropland","Forest land","Land under perm. meadows and pastures"
    ),
    value_multiplier = 1,
    group_var = NULL
){
  moduleServer(id, function(input, output, session){
    
    # -------- Helpers config-first -----------------------------------------
    sc_norm <- if (exists("scenario_code", mode = "function", inherits = TRUE)) scenario_code else {
      function(x) stringr::str_squish(as.character(x))
    }
    sc_label <- if (exists("scenario_label", mode = "function", inherits = TRUE)) scenario_label else {
      function(x) sc_norm(x)
    }
    
    # -----------------------------------------------------------
    # Noyau commun
    # -----------------------------------------------------------
    core <- harvested_core(
      fact             = fact,
      r_country         = r_country,
      harvest_element   = harvest_element,
      exclude_items     = exclude_items,
      value_multiplier  = value_multiplier,
      group_var         = group_var,
      r_scenarios       = r_scenarios     # <- IMPORTANT : branchement helper global
    )
    
    scen_base            <- core$scen_base
    scen_diets_effective <- core$scen_diets_effective
    data_harvested_groups <- core$data_harvested_groups
    
    # -----------------------------------------------------------
    # Graphique "2018 → 2050"
    # -----------------------------------------------------------
    output$harv_groups <- plotly::renderPlotly({
      df <- data_harvested_groups()
      req(nrow(df) > 0)
      
      th <- get_plotly_tokens()
      
      mode <- input$facet_mode
      if (is.null(mode) || !mode %in% c("product", "scenario")) mode <- "product"
      
      base_year   <- suppressWarnings(min(df$Year, na.rm = TRUE))
      target_year <- suppressWarnings(max(df$Year, na.rm = TRUE))
      
      # Sécurise les niveaux de scénarios (codes) depuis le facteur renvoyé par le core
      scen_levels <- if (is.factor(df$Scenario)) levels(df$Scenario) else sort(unique(as.character(df$Scenario)))
      scen_levels <- scen_levels[!is.na(scen_levels) & nzchar(scen_levels)]
      
      df <- df %>%
        dplyr::mutate(
          Scenario_code  = as.character(Scenario),
          Scenario_label = sc_label(Scenario_code),
          # on garde Scenario factor pour l’ordre + couleurs, mais on a label à côté
          Scenario = factor(Scenario_code, levels = scen_levels),
          Scenario_label = factor(Scenario_label, levels = sc_label(scen_levels)),
          area_1000 = area / 1000,
          pct_label = dplyr::if_else(
            is.na(pct_change),
            "—",
            paste0(sprintf("%+.0f", pct_change), " %")
          ),
          tooltip = paste0(
            "Group: ", as.character(group), "<br>",
            "Scenario: ", as.character(Scenario_label), "<br>",
            "Year: ", Year, "<br>",
            "Area: ", scales::comma(round(area_1000, 1)), " 1000 ha<br>",
            "Change vs base year: ", pct_label
          )
        )
      
      df_labels <- df %>% dplyr::filter(Year == target_year)
      
      if (mode == "product") {
        # -------------------------
        # MODE "BY PRODUCT GROUP"
        # -------------------------
        
        # Offsets génériques des labels (%), dépendants du rang du scénario
        df_labels_plot <- df_labels %>%
          dplyr::group_by(group) %>%
          dplyr::mutate(
            yrange = suppressWarnings(max(area_1000, na.rm = TRUE) - min(area_1000, na.rm = TRUE)),
            yrange = dplyr::if_else(!is.finite(yrange) | yrange == 0, 1, yrange),
            n_sc   = dplyr::n_distinct(Scenario),
            scen_rank = as.integer(Scenario),                 # basé sur l’ordre factor (core)
            offset_idx = scen_rank - (n_sc + 1) / 2,          # centré
            y_offset = offset_idx * 0.06 * yrange,
            area_label = area_1000 + y_offset
          ) %>%
          dplyr::ungroup()
        
        g <- ggplot2::ggplot(
          df,
          ggplot2::aes(
            x      = Year,
            y      = area_1000,
            colour = Scenario,   # <- codes (couleurs stables)
            group  = interaction(Scenario, group),
            text   = tooltip
          )
        ) +
          ggplot2::geom_vline(xintercept = base_year,
                              colour = "grey85", linewidth = 0.3, show.legend = FALSE) +
          ggplot2::geom_vline(xintercept = target_year,
                              colour = "grey85", linewidth = 0.3, show.legend = FALSE) +
          ggplot2::geom_line(linetype = "dotted", linewidth = 0.5, alpha = 0.9) +
          ggplot2::geom_point(size = 2.2, alpha = 0.95) +
          ggplot2::geom_text(
            data = df_labels_plot,
            ggplot2::aes(
              x      = Year,
              y      = area_label,
              label  = pct_label,
              colour = Scenario,
              group  = interaction(Scenario, group)
            ),
            hjust   = 0,
            nudge_x = 3.5,
            size    = 3,
            show.legend = FALSE
          ) +
          ggplot2::scale_x_continuous(
            breaks = c(base_year, target_year),
            expand = ggplot2::expansion(mult = c(0.08, 0.08))
          ) +
          ggplot2::scale_y_continuous(
            expand = ggplot2::expansion(mult = c(0.05, 0.15))
          ) +
          ggplot2::labs(
            x = "Year",
            y = "Area harvested (1000 ha)",
            colour = "Scenario"
          ) +
          ggplot2::facet_wrap(~ group, scales = "free_y") +
          ggplot2::theme_minimal(base_size = 11) +
          ggplot2::theme(
            strip.background = ggplot2::element_rect(fill = "grey90", colour = NA),
            strip.text = ggplot2::element_text(face = "bold", color = "black"),
            panel.grid.minor = ggplot2::element_blank(),
            legend.position  = "bottom",
            legend.title     = ggplot2::element_text(face = "bold", color = th$muted_color),
            legend.text      = ggplot2::element_text(color = th$muted_color),
            axis.text        = ggplot2::element_text(color = th$muted_color),
            axis.title       = ggplot2::element_text(color = th$muted_color),
            panel.background = ggplot2::element_rect(fill = "transparent", colour = NA),
            plot.background  = ggplot2::element_rect(fill = "transparent", colour = NA)
          )
        
        # Couleurs scénarios : valeurs sur codes, labels UI via scenario_label()
        if (exists("SCENARIO_COLORS", inherits = TRUE)) {
          scen_use <- as.character(scen_diets_effective())
          scen_use <- intersect(scen_use, names(SCENARIO_COLORS))
          if (length(scen_use) > 0) {
            g <- g + ggplot2::scale_colour_manual(
              values = SCENARIO_COLORS,
              breaks = scen_use,
              labels = sc_label(scen_use)
            )
          }
        } else {
          # Au minimum, renommer les breaks (si ggplot choisit un ordre)
          g <- g + ggplot2::scale_colour_discrete(labels = sc_label)
        }
        
      } else {
        # -------------------------
        # MODE "BY SCENARIO"
        # -------------------------
        
        df_scen <- df %>%
          dplyr::group_by(Scenario, group) %>%
          dplyr::mutate(area_2050 = area_1000[Year == target_year]) %>%
          dplyr::ungroup() %>%
          dplyr::group_by(Scenario) %>%
          dplyr::mutate(
            max_2050 = suppressWarnings(max(area_2050, na.rm = TRUE)),
            keep_group = dplyr::if_else(
              !is.finite(max_2050) | max_2050 < 50,
              TRUE,
              is.na(area_2050) | area_2050 >= 50
            )
          ) %>%
          dplyr::ungroup() %>%
          dplyr::filter(keep_group) %>%
          dplyr::select(-max_2050, -keep_group)
        
        req(nrow(df_scen) > 0)
        
        df_labels_scen <- df_scen %>% dplyr::filter(Year == target_year)
        
        g <- ggplot2::ggplot(
          df_scen,
          ggplot2::aes(
            x      = Year,
            y      = area_1000,
            colour = group,
            group  = group,
            text   = tooltip
          )
        ) +
          ggplot2::geom_vline(xintercept = base_year,
                              colour = "grey85", linewidth = 0.3, show.legend = FALSE) +
          ggplot2::geom_vline(xintercept = target_year,
                              colour = "grey85", linewidth = 0.3, show.legend = FALSE) +
          ggplot2::geom_line(linetype = "dotted", linewidth = 0.5, alpha = 0.9) +
          ggplot2::geom_point(size = 2.0, alpha = 0.95) +
          ggplot2::geom_text(
            data = df_labels_scen,
            ggplot2::aes(
              x      = Year,
              y      = area_1000,
              label  = pct_label,
              colour = group,
              group  = group
            ),
            hjust   = 0,
            nudge_x = 4,
            size    = 3,
            show.legend = FALSE
          ) +
          ggplot2::scale_x_continuous(
            breaks = c(base_year, target_year),
            expand = ggplot2::expansion(mult = c(0.08, 0.08))
          ) +
          ggplot2::scale_y_continuous(
            expand = ggplot2::expansion(mult = c(0.05, 0.15))
          ) +
          ggplot2::labs(
            x = "Year",
            y = "Area harvested (1000 ha)",
            colour = "Product group"
          ) +
          # Facettes sur LABELS UI (pas sur codes)
          ggplot2::facet_wrap(~ Scenario_label, nrow = 1) +
          ggplot2::theme_minimal(base_size = 11) +
          ggplot2::theme(
            strip.background = ggplot2::element_rect(fill = "grey90", colour = NA),
            strip.text = ggplot2::element_text(face = "bold", color = "black"),
            panel.grid.minor = ggplot2::element_blank(),
            legend.position  = "bottom",
            legend.title     = ggplot2::element_text(face = "bold", color = th$muted_color),
            legend.text      = ggplot2::element_text(color = th$muted_color),
            axis.text        = ggplot2::element_text(color = th$muted_color),
            axis.title       = ggplot2::element_text(color = th$muted_color),
            panel.background = ggplot2::element_rect(fill = "transparent", colour = NA),
            plot.background  = ggplot2::element_rect(fill = "transparent", colour = NA)
          )
        
        if (exists("pal_crops", mode = "function", inherits = TRUE)) {
          grp_lvls <- levels(df_scen$group)
          cols_grp <- pal_crops(grp_lvls)
          names(cols_grp) <- grp_lvls
          g <- g + ggplot2::scale_colour_manual(values = cols_grp)
        }
      }
      
      p <- plotly::ggplotly(g, tooltip = "text")
      
      p <- plotly_apply_global_theme(p, bg = "transparent", grid = "y") %>%
        plotly::layout(
          legend = list(
            orientation = "h",
            x           = 0.5,
            xanchor     = "center",
            y           = 1.05,
            yanchor     = "bottom",
            title       = list(text = ""),
            font        = list(color = th$muted_color)
          ),
          margin = list(t = 20)
        )
      
      p
    })
    
    
    output$small_crops_msg <- renderUI({
      if (is.null(input$facet_mode) || input$facet_mode != "scenario") return(NULL)
      
      df <- data_harvested_groups()
      if (nrow(df) == 0) return(NULL)
      
      target_year <- suppressWarnings(max(df$Year, na.rm = TRUE))
      
      df <- df %>%
        dplyr::mutate(area_1000 = area / 1000) %>%
        dplyr::group_by(Scenario, group) %>%
        dplyr::mutate(area_2050 = area_1000[Year == target_year]) %>%
        dplyr::ungroup() %>%
        dplyr::group_by(Scenario) %>%
        dplyr::mutate(
          max_2050 = suppressWarnings(max(area_2050, na.rm = TRUE)),
          keep_group = dplyr::if_else(
            !is.finite(max_2050) | max_2050 < 50,
            TRUE,
            is.na(area_2050) | area_2050 >= 50
          )
        ) %>%
        dplyr::ungroup()
      
      has_filtered <- any(df$keep_group == FALSE, na.rm = TRUE)
      if (!has_filtered) return(NULL)
      
      htmltools::tags$p(
        class = "text-muted small",
        htmltools::tags$em(
          "In this view, crop groups with less than 50,000 ha of harvested area in 2050 ",
          "are not displayed for readability."
        )
      )
    })
    
    # -----------------------------------------------------------
    # Export CSV : données du graphique d'évolution
    # -----------------------------------------------------------
    output$dl_harvested_csv <- downloadHandler(
      filename = function(){
        paste0("AreaHarvested_trend_", gsub(" ", "_", r_country()), ".csv")
      },
      content = function(file){
        df <- data_harvested_groups()
        req(nrow(df) > 0)
        
        mode <- input$facet_mode
        if (is.null(mode) || !mode %in% c("product", "scenario")) mode <- "product"
        
        base_year   <- suppressWarnings(min(df$Year, na.rm = TRUE))
        target_year <- suppressWarnings(max(df$Year, na.rm = TRUE))
        
        scen_levels <- if (is.factor(df$Scenario)) levels(df$Scenario) else sort(unique(as.character(df$Scenario)))
        scen_levels <- scen_levels[!is.na(scen_levels) & nzchar(scen_levels)]
        
        df <- df %>%
          dplyr::mutate(
            Scenario_code  = as.character(Scenario),
            Scenario_label = sc_label(Scenario_code),
            Scenario       = factor(Scenario_code, levels = scen_levels),
            area_1000      = area / 1000
          )
        
        if (mode == "scenario") {
          df <- df %>%
            dplyr::group_by(Scenario, group) %>%
            dplyr::mutate(area_2050 = area_1000[Year == target_year]) %>%
            dplyr::ungroup() %>%
            dplyr::group_by(Scenario) %>%
            dplyr::mutate(
              max_2050 = suppressWarnings(max(area_2050, na.rm = TRUE)),
              keep_group = dplyr::if_else(
                !is.finite(max_2050) | max_2050 < 50,
                TRUE,
                is.na(area_2050) | area_2050 >= 50
              )
            ) %>%
            dplyr::ungroup() %>%
            dplyr::filter(keep_group) %>%
            dplyr::select(-area_2050, -max_2050, -keep_group)
        }
        
        out <- df %>%
          dplyr::arrange(Scenario, group, Year) %>%
          dplyr::transmute(
            Pays                    = r_country(),
            Scenario_code           = Scenario_code,
            Scenario                = Scenario_label,
            Groupe_produit          = as.character(group),
            Annee                   = Year,
            Surface_ha              = area,
            Surface_1000ha          = area_1000,
            Variation_pct_vs_base   = pct_change
          )
        
        readr::write_delim(out, file, delim = ";")
      }
    )
    
    # -----------------------------------------------------------
    # Note explicative
    # -----------------------------------------------------------
    output$note <- renderUI({
      df <- data_harvested_groups()
      if (nrow(df) == 0) return(NULL)
      
      base_year   <- suppressWarnings(min(df$Year, na.rm = TRUE))
      target_year <- suppressWarnings(max(df$Year, na.rm = TRUE))
      
      htmltools::HTML(glue::glue(
        "<p>
        This chart shows, for the selected country, the <strong>change in harvested area</strong>
        between <strong>{base_year}</strong> and <strong>{target_year}</strong>, under the different diet scenarios.<br>
        Two complementary views are available:
        <ul>
          <li><strong>By product group</strong>: each panel corresponds to a crop or product group,
              and the coloured lines represent the different diet scenarios over time.</li>
          <li><strong>By scenario</strong>: each panel corresponds to one diet scenario, and the coloured lines
              represent the different crops or product groups.</li>
        </ul>
        Harvested areas on the y-axis are expressed in <strong>thousand hectares (1000 ha)</strong>.
        Labels near the <strong>{target_year}</strong> points indicate the percentage change
        in harvested area compared with the base year for each group–scenario combination.<br>
        In the “By scenario” view, crop groups with less than <strong>50,000 ha</strong> of harvested area in
        {target_year} may be hidden to improve readability.
        </p>"
      ))
    })
    
  })
}
