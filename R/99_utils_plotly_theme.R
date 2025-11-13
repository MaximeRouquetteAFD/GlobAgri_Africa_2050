# R/99_utils_plotly_theme.R
APP_CARD_BG     <- "#f8f9fa"          # même teinte que --card-bg
APP_TRANSPARENT <- "rgba(0,0,0,0)"

plotly_theme <- function(p,
                         bg   = c("transparent", "card"),
                         grid = c("none", "x", "y", "both"),
                         gridcolor = "rgba(0,0,0,.15)",
                         minor_gridcolor = "rgba(0,0,0,.06)"){
  bg   <- match.arg(bg)
  grid <- match.arg(grid)
  
  bgcol <- if (bg == "card") APP_CARD_BG else APP_TRANSPARENT
  
  # flags grille
  show_x <- grid %in% c("x","both")
  show_y <- grid %in% c("y","both")
  
  plotly::layout(
    p,
    paper_bgcolor = bgcol,
    plot_bgcolor  = bgcol,
    xaxis = list(
      showgrid = show_x,
      gridcolor = if (show_x) gridcolor else "rgba(0,0,0,0)",
      gridwidth = if (show_x) 1 else 0
    ),
    yaxis = list(
      showgrid  = show_y,
      gridcolor = if (show_y) gridcolor else "rgba(0,0,0,0)",
      gridwidth = if (show_y) 1 else 0,
      # grille mineure (si votre version de plotly la supporte)
      minor = list(
        showgrid  = show_y,
        gridcolor = if (show_y) minor_gridcolor else "rgba(0,0,0,0)",
        gridwidth = if (show_y) 0.5 else 0
      )
    )
  )
}

# Thème 100 % transparent pour Plotly (papier + plot)
plotly_theme_transparent <- function(p, ...) {
  if (missing(p) || is.null(p)) return(NULL)
  plotly::layout(
    p,
    paper_bgcolor = "rgba(0,0,0,0)",
    plot_bgcolor  = "rgba(0,0,0,0)",
    ...
  )
}
