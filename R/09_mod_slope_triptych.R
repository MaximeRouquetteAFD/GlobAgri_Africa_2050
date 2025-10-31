# R/09_mod_slope_triptych.R
# -------------------------------------------------------------------
# Triptyque slopegraphs (Imports/(Imp+Prod), Energy Food, Energy Feed)
# -------------------------------------------------------------------

mod_slope_triptych_ui <- function(
    id,
    height = "360px",
    title  = "Évolution des principales parts et usages énergétiques",
    show_title   = TRUE,
    wrap_in_cards = TRUE    # <- TRUE : chaque bloc dans une u-card
){
  ns <- NS(id)
  
  # Fabrique 1 bloc (titre + plot + note), éventuellement dans une u-card
  make_block <- function(h4_title, plot_id, note_id){
    inner <- tagList(
      h4(h4_title),
      plotOutput(ns(plot_id), height = height),
      p(class = "text-muted", uiOutput(ns(note_id)))
    )
    if (isTRUE(wrap_in_cards)) {
      div(class = "u-card u-card--flat u-card--hover", inner)
    } else {
      inner
    }
  }
  
  tagList(
    if (isTRUE(show_title)) h3(title),
    # rangée des 3 panneaux (utilitaire u-row)
    div(
      class = "slope-triptych u-row",
      div(class = "u-box",
          make_block(
            "Importations / (Importations + Production)",
            "plot_imp", "note_imp"
          )
      ),
      div(class = "u-box",
          make_block(
            "Part de l’usage « Energy Food »",
            "plot_food", "note_food"
          )
      ),
      div(class = "u-box",
          make_block(
            "Part de l’usage « Energy Feed »",
            "plot_feed", "note_feed"
          )
      )
    )
  )
}

mod_slope_triptych_server <- function(id, fact, r_country){
  moduleServer(id, function(input, output, session){
    
    ALL_USES <- c("Energy Food","Energy Feed","Energy Losses","Energy Seed","Energy Other uses (non-food)")
    EL_IMPORT <- "Energy Import Quantity"
    EL_PROD   <- "Energy Production"
    
    scen_levels <- if (exists("SCEN_SHOW", inherits = TRUE))
      SCEN_SHOW else c("Même diète","Diète probable","Diète saine")
    
    scen_cols <- if (exists("SCEN_COLORS", inherits = TRUE)) {
      unname(SCEN_COLORS[scen_levels])
    } else {
      setNames(c("#4E79A7","#59A14F","#E15759")[seq_along(scen_levels)], scen_levels)
    }
    
    # ---- année DSQ horizon (≈2050) par scénario
    end_year_by_scenario <- reactive({
      reg <- r_country(); req(nzchar(reg))
      fact %>%
        dplyr::filter(Region==reg,
                      Element=="Energy Domestic supply quantity",
                      Scenario %in% scen_levels, !is.na(Value)) %>%
        dplyr::group_by(Scenario) %>%
        dplyr::summarise(Year = suppressWarnings(max(Year, na.rm=TRUE)), .groups="drop")
    })
    
    # ---- année de base robuste (et libellé à afficher)
    base_year_for <- function(type){
      reg <- r_country()
      pool_elements <- switch(type,
                              "imports" = c(EL_IMPORT, EL_PROD),
                              "food"    = ALL_USES,
                              "feed"    = ALL_USES
      )
      db <- fact %>% dplyr::filter(Region==reg, Scenario=="Année de base",
                                   Element %in% pool_elements, !is.na(Value))
      if (!nrow(db)) return(NA_integer_)
      if (any(db$Year==2018)) 2018 else suppressWarnings(max(db$Year, na.rm=TRUE))
    }
    base_label_for <- function(type){
      by <- base_year_for(type)
      if (is.finite(by)) as.character(by) else "2018"
    }
    
    # ---- calcule le df (x,y,Scenario) pour un type
    make_slope_df <- function(type){
      reg <- r_country(); req(nzchar(reg))
      by <- base_year_for(type); validate(need(is.finite(by), "Aucune année de base disponible."))
      end_tab <- end_year_by_scenario(); validate(need(nrow(end_tab)>0, "Pas d'année d'horizon disponible."))
      
      base_lab <- base_label_for(type)   # "2018" ou année réelle
      lev_x    <- c(base_lab, "2050")
      
      # Base
      share18 <- switch(
        type,
        "imports" = {
          im18 <- fact %>% dplyr::filter(Region==reg, Scenario=="Année de base", Year==by, Element==EL_IMPORT) %>%
            dplyr::summarise(v=sum(Value,na.rm=TRUE)) %>% dplyr::pull(v)
          pr18 <- fact %>% dplyr::filter(Region==reg, Scenario=="Année de base", Year==by, Element==EL_PROD) %>%
            dplyr::summarise(v=sum(Value,na.rm=TRUE)) %>% dplyr::pull(v)
          den <- im18 + pr18
          ifelse(is.finite(den)&&den>0, im18/den, NA_real_)
        },
        "food" = {
          num <- fact %>% dplyr::filter(Region==reg, Scenario=="Année de base", Year==by, Element=="Energy Food") %>%
            dplyr::summarise(v=sum(Value,na.rm=TRUE)) %>% dplyr::pull(v)
          den <- fact %>% dplyr::filter(Region==reg, Scenario=="Année de base", Year==by, Element %in% ALL_USES) %>%
            dplyr::summarise(v=sum(Value,na.rm=TRUE)) %>% dplyr::pull(v)
          ifelse(is.finite(den)&&den>0, num/den, NA_real_)
        },
        "feed" = {
          num <- fact %>% dplyr::filter(Region==reg, Scenario=="Année de base", Year==by, Element=="Energy Feed") %>%
            dplyr::summarise(v=sum(Value,na.rm=TRUE)) %>% dplyr::pull(v)
          den <- fact %>% dplyr::filter(Region==reg, Scenario=="Année de base", Year==by, Element %in% ALL_USES) %>%
            dplyr::summarise(v=sum(Value,na.rm=TRUE)) %>% dplyr::pull(v)
          ifelse(is.finite(den)&&den>0, num/den, NA_real_)
        }
      )
      
      d2018 <- data.frame(
        Scenario = factor(scen_levels, levels=scen_levels),
        x = factor(base_lab, levels = lev_x),
        y = share18
      )
      
      # Horizon
      pool <- switch(
        type,
        "imports" = fact %>%
          dplyr::filter(Region==reg, Scenario %in% scen_levels,
                        Element %in% c(EL_IMPORT, EL_PROD)) %>%
          dplyr::inner_join(end_tab, by=c("Scenario","Year")),
        "food" = fact %>%
          dplyr::filter(Region==reg, Scenario %in% scen_levels,
                        Element %in% c("Energy Food", ALL_USES)) %>%
          dplyr::inner_join(end_tab, by=c("Scenario","Year")),
        "feed" = fact %>%
          dplyr::filter(Region==reg, Scenario %in% scen_levels,
                        Element %in% c("Energy Feed", ALL_USES)) %>%
          dplyr::inner_join(end_tab, by=c("Scenario","Year"))
      )
      
      end <- switch(
        type,
        "imports" = {
          im <- pool %>% dplyr::filter(Element==EL_IMPORT) %>%
            dplyr::group_by(Scenario) %>% dplyr::summarise(im=sum(Value,na.rm=TRUE), .groups="drop")
          pr <- pool %>% dplyr::filter(Element==EL_PROD) %>%
            dplyr::group_by(Scenario) %>% dplyr::summarise(pr=sum(Value,na.rm=TRUE), .groups="drop")
          im %>% dplyr::left_join(pr, by="Scenario") %>%
            dplyr::mutate(den = im + pr, share = ifelse(den>0, im/den, NA_real_))
        },
        "food" = {
          num <- pool %>% dplyr::filter(Element=="Energy Food") %>%
            dplyr::group_by(Scenario) %>% dplyr::summarise(num=sum(Value,na.rm=TRUE), .groups="drop")
          den <- pool %>% dplyr::filter(Element %in% ALL_USES) %>%
            dplyr::group_by(Scenario) %>% dplyr::summarise(den=sum(Value,na.rm=TRUE), .groups="drop")
          num %>% dplyr::left_join(den, by="Scenario") %>%
            dplyr::mutate(share = ifelse(den>0, num/den, NA_real_))
        },
        "feed" = {
          num <- pool %>% dplyr::filter(Element=="Energy Feed") %>%
            dplyr::group_by(Scenario) %>% dplyr::summarise(num=sum(Value,na.rm=TRUE), .groups="drop")
          den <- pool %>% dplyr::filter(Element %in% ALL_USES) %>%
            dplyr::group_by(Scenario) %>% dplyr::summarise(den=sum(Value,na.rm=TRUE), .groups="drop")
          num %>% dplyr::left_join(den, by="Scenario") %>%
            dplyr::mutate(share = ifelse(den>0, num/den, NA_real_))
        }
      ) %>%
        dplyr::filter(is.finite(share)) %>%
        dplyr::mutate(Scenario = factor(Scenario, levels=scen_levels))
      
      d2050 <- data.frame(
        Scenario = end$Scenario,
        x = factor("2050", levels = lev_x), 
        y = end$share
      )
      
      dplyr::bind_rows(d2018, d2050)
    }
    
    compute_ylim <- function(y){
      y <- y[is.finite(y)]
      if (!length(y)) return(c(0,1))
      ymin <- min(y); ymax <- max(y); rng <- ymax - ymin
      window <- max(0.10, min(0.50, rng * 1.25 + 0.04))
      center <- (ymin + ymax)/2
      lo <- max(0, center - window/2)
      hi <- min(1, center + window/2)
      c(lo, hi)
    }
    
    draw_slope <- function(df, ylab){
      library(ggplot2)
      base_lab <- levels(df$x)[1]
      p2018 <- unique(df[df$x == base_lab, c("x","y")])
      yl <- compute_ylim(df$y)
      
      ggplot(df, aes(x=x, y=y, group=Scenario, color=Scenario)) +
        geom_line(linewidth=1.6, lineend="round") +
        geom_point(size=3) +
        geom_text(data=subset(df, x=="2050"),
                  aes(label=scales::percent(y, accuracy=0.1)),
                  hjust=-0.25, size=4, show.legend=FALSE) +
        geom_point(data=p2018, inherit.aes=FALSE,
                   aes(x=x, y=y), color="grey30", size=3) +
        geom_text(data=p2018, inherit.aes=FALSE,
                  aes(x=x, y=y, label=scales::percent(y, accuracy=0.1)),
                  hjust=1.2, size=4, color="grey30") +
        scale_color_manual(values=scen_cols, drop=FALSE) +
        scale_x_discrete(expand=expansion(mult=c(0.10,0.30))) +
        scale_y_continuous(labels=scales::percent_format(accuracy=1),
                           limits = yl, expand = expansion(mult=c(0,0))) +
        labs(x=NULL, y=ylab, color=NULL) +
        theme_minimal(base_size=12) +
        theme(panel.grid.major.x=element_blank(),
              panel.grid.minor=element_blank(),
              legend.position="top",
              plot.margin=margin(10,40,10,10),
              axis.text.x=element_text(face="bold")) +
        coord_cartesian(clip="off")
    }
    
    output$plot_imp  <- renderPlot({ draw_slope(make_slope_df("imports"),
                                                "Part des importations / (Importations + Production) (%)") })
    output$plot_food <- renderPlot({ draw_slope(make_slope_df("food"),
                                                "Part de « Energy Food » (%)") })
    output$plot_feed <- renderPlot({ draw_slope(make_slope_df("feed"),
                                                "Part de « Energy Feed » (%)") })
    
    note_for <- function(type){
      by <- base_year_for(type)
      if (is.finite(by) && by != 2018)
        HTML(paste0("Note : l'année de base utilisée est <b>", by, "</b> (2018 non disponible)."))
      else NULL
    }
    output$note_imp  <- renderUI(note_for("imports"))
    output$note_food <- renderUI(note_for("food"))
    output$note_feed <- renderUI(note_for("feed"))
  })
}
