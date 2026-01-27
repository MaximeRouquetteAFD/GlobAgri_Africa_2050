# R/99_utils_plotly_theme.R
# ---------------------------------------------------------------
# Changer le style de l'intérieur des graphiques selon deux styles déjà prédéfinis :"dark" et "light"
# ---------------------------------------------------------------

APP_CARD_BG     <- "#f8f9fa"          # same as your --card-bg
APP_TRANSPARENT <- "rgba(0,0,0,0)"
APP_PLOTLY_FONT_FAMILY <- "'Segoe UI', Roboto, Helvetica, Arial, sans-serif"

# ----------------------------------------------------------------
# 1) GLOBAL SWITCH (single source of truth)
# ----------------------------------------------------------------
# Choose ONE: "dark" or "light"
APP_PLOTLY_MODE <- "dark"

`%||%` <- function(x, y) if (is.null(x)) y else x

# ----------------------------------------------------------------
# 2) TOKENS (presets)
# ----------------------------------------------------------------
PLOTLY_THEME_LIGHT <- list(
  font_color       = "#111827",          # main text
  muted_color      = "#6B7280",          # ticks / legend / axis titles
  gridcolor        = "rgba(0,0,0,.15)",
  minor_gridcolor  = "rgba(0,0,0,.06)",
  axis_linecolor   = "rgba(0,0,0,.18)",
  hover_bg         = "#FFFFFF",
  hover_font       = "#111827",
  baseline_color   = "rgba(0,0,0,.65)"   # to use for your baseline trace
)

PLOTLY_THEME_DARK <- list(
  font_color       = "#FFFFFF",
  muted_color      = "#FFFFFF",
  gridcolor        = "rgba(255,255,255,.18)",
  minor_gridcolor  = "rgba(255,255,255,.10)",
  axis_linecolor   = "rgba(255,255,255,.25)",
  hover_bg         = "#FFFFFF",
  hover_font       = "#111827",
  baseline_color   = "rgba(255,255,255,.65)"
)

get_plotly_tokens <- function(mode = APP_PLOTLY_MODE){
  mode <- tolower(as.character(mode %||% "dark"))
  if (identical(mode, "light")) PLOTLY_THEME_LIGHT else PLOTLY_THEME_DARK
}

# Optional convenience setter (still "global", not reactive runtime)
set_plotly_mode <- function(mode = c("dark", "light")){
  mode <- match.arg(mode)
  APP_PLOTLY_MODE <<- mode
  invisible(APP_PLOTLY_MODE)
}

# ----------------------------------------------------------------
# 3) THEME APPLIER (layout style only)
#    Notes:
#    - This function is meant to be called AFTER your main layout()
#      so it enriches axes/legend fonts and grid styling.
#    - Baseline color is NOT applied here (baseline is a trace).
#      Use get_plotly_tokens()$baseline_color in add_trace(line=...).
# ----------------------------------------------------------------
plotly_theme <- function(
    p,
    bg   = c("transparent", "card"),
    grid = c("none", "x", "y", "both"),
    
    # grid
    gridcolor       = "rgba(0,0,0,.15)",
    minor_gridcolor = "rgba(0,0,0,.06)",
    
    # text/axes/hover
    font_color      = NULL,
    muted_color     = NULL,
    axis_linecolor  = NULL,
    hover_bg        = NULL,
    hover_font      = NULL,
    
    # NEW: font family
    font_family     = NULL
){
  bg   <- match.arg(bg)
  grid <- match.arg(grid)
  
  bgcol <- if (bg == "card") APP_CARD_BG else APP_TRANSPARENT
  
  show_x <- grid %in% c("x","both")
  show_y <- grid %in% c("y","both")
  
  fg    <- font_color     %||% "#111827"
  muted <- muted_color    %||% "#6B7280"
  axln  <- axis_linecolor %||% "rgba(0,0,0,.18)"
  hbg   <- hover_bg       %||% "#FFFFFF"
  hfg   <- hover_font     %||% "#111827"
  fam   <- font_family    %||% APP_PLOTLY_FONT_FAMILY
  
  plotly::layout(
    p,
    paper_bgcolor = bgcol,
    plot_bgcolor  = bgcol,
    
    # global text
    font = list(color = fg, family = fam),
    
    # legend text
    legend = list(
      font = list(color = muted, family = fam)
    ),
    
    # hover bubble
    hoverlabel = list(
      bgcolor = hbg,
      font    = list(color = hfg, family = fam)
    ),
    
    # axes styling
    xaxis = list(
      showgrid  = show_x,
      gridcolor = if (show_x) gridcolor else "rgba(0,0,0,0)",
      gridwidth = if (show_x) 1 else 0,
      
      tickfont  = list(color = muted, family = fam),
      titlefont = list(color = muted, family = fam),
      
      linecolor     = axln,
      zerolinecolor = axln
    ),
    
    yaxis = list(
      showgrid  = show_y,
      gridcolor = if (show_y) gridcolor else "rgba(0,0,0,0)",
      gridwidth = if (show_y) 1 else 0,
      
      tickfont  = list(color = muted, family = fam),
      titlefont = list(color = muted, family = fam),
      
      linecolor     = axln,
      zerolinecolor = axln,
      
      minor = list(
        showgrid  = show_y,
        gridcolor = if (show_y) minor_gridcolor else "rgba(0,0,0,0)",
        gridwidth = if (show_y) 0.5 else 0
      )
    )
  )
}

# One-liner to apply the globally selected preset
plotly_apply_global_theme <- function(p, bg = "transparent", grid = "y"){
  th <- get_plotly_tokens()
  plotly_theme(
    p,
    bg = bg,
    grid = grid,
    gridcolor       = th$gridcolor,
    minor_gridcolor = th$minor_gridcolor,
    font_color      = th$font_color,
    muted_color     = th$muted_color,
    axis_linecolor  = th$axis_linecolor,
    hover_bg        = th$hover_bg,
    hover_font      = th$hover_font
  )
}

# ----------------------------------------------------------------
# 4) Fully transparent helper (kept from your original)
# ----------------------------------------------------------------
plotly_theme_transparent <- function(p, ...) {
  if (missing(p) || is.null(p)) return(NULL)
  plotly::layout(
    p,
    paper_bgcolor = "rgba(0,0,0,0)",
    plot_bgcolor  = "rgba(0,0,0,0)",
    ...
  )
}
