# ============================================================
# Redundancy test — extra scenario vs reference scenario
# Returns TRUE if extra is "redundant" vs ref (within tol_rel)
# tol_rel is LOCKED to config if config is available.
# ============================================================
is_scenario_redundant <- function(
    df_country,
    scenario_ref,
    scenario_extra,
    tol_rel = 0.05,
    element = NULL,
    items = NULL
){
  # --- Guard minimal: columns required
  req_cols <- c("Scenario", "Element", "Item", "Year", "Value")
  if (!all(req_cols %in% names(df_country))) {
    warning("is_scenario_redundant(): df_country must contain Scenario/Element/Item/Year/Value.")
    return(FALSE)
  }
  
  # --- tol_rel: LOCK to config (single source of truth) if available
  # Priority: SC$tol_rel_default -> SCENARIO_REDUNDANCE_TOL_REL -> fallback (current default)
  tol_cfg <- NULL
  if (exists("SC", inherits = TRUE)) {
    sc_obj <- get("SC", inherits = TRUE)
    if (is.list(sc_obj) && !is.null(sc_obj$tol_rel_default)) tol_cfg <- sc_obj$tol_rel_default
  }
  if (is.null(tol_cfg) && exists("SCENARIO_REDUNDANCE_TOL_REL", inherits = TRUE)) {
    tol_cfg <- get("SCENARIO_REDUNDANCE_TOL_REL", inherits = TRUE)
  }
  
  tol_cfg_ok <- is.numeric(tol_cfg) && length(tol_cfg) == 1 && is.finite(tol_cfg) && tol_cfg > 0
  if (tol_cfg_ok) {
    tol_in_ok <- is.numeric(tol_rel) && length(tol_rel) == 1 && is.finite(tol_rel) && tol_rel > 0
    
    # If user passed a different tol_rel, force it to config (no divergence possible)
    if (!missing(tol_rel) && tol_in_ok && !isTRUE(all.equal(tol_rel, tol_cfg, tolerance = 0))) {
      warning(sprintf(
        "is_scenario_redundant(): tol_rel=%s ignored; locked to config value tol_rel=%s.",
        format(tol_rel), format(tol_cfg)
      ))
    }
    tol_rel <- tol_cfg
  } else {
    # No config found -> keep tol_rel argument (fallback behaviour)
    if (!is.numeric(tol_rel) || length(tol_rel) != 1 || !is.finite(tol_rel) || tol_rel <= 0) {
      tol_rel <- 0.01
    }
  }
  
  # --- Normalisation scenarios (uses config if present)
  code <- if (exists("scenario_code", mode = "function")) scenario_code else function(x) x
  scen_ref  <- code(scenario_ref)
  scen_extr <- code(scenario_extra)
  
  # --- Element (Area) — configurable
  if (is.null(element) || !nzchar(element)) {
    element <- if (exists("AREA_ELEMENT", inherits = TRUE)) get("AREA_ELEMENT", inherits = TRUE) else "Area"
  }
  
  # --- Items list — configurable
  if (is.null(items) || length(items) == 0) {
    if (exists("AREA_CHILDREN", inherits = TRUE)) {
      items <- c("Forest land", get("AREA_CHILDREN", inherits = TRUE))
    } else {
      items <- c("Cropland", "Land under perm. meadows and pastures", "Forest land")
    }
  }
  
  # --- Filter to the 2 scenarios + Area element + items of interest
  d0 <- df_country %>%
    dplyr::mutate(
      Scenario = code(Scenario)
    ) %>%
    dplyr::filter(
      Scenario %in% c(scen_ref, scen_extr),
      stringr::str_to_lower(Element) == stringr::str_to_lower(element),
      Item %in% items
    )
  
  if (nrow(d0) == 0) return(FALSE)
  
  # --- Pick last available year per scenario (max year with non-NA value)
  yrs <- d0 %>%
    dplyr::filter(is.finite(Value)) %>%
    dplyr::group_by(Scenario) %>%
    dplyr::summarise(
      year_used = suppressWarnings(max(Year, na.rm = TRUE)),
      .groups = "drop"
    )
  
  # If either scenario has no valid year => no redundancy test (prudence)
  if (!all(c(scen_ref, scen_extr) %in% yrs$Scenario) || any(!is.finite(yrs$year_used))) {
    return(FALSE)
  }
  
  d1 <- d0 %>%
    dplyr::inner_join(yrs, by = "Scenario") %>%
    dplyr::filter(Year == year_used)
  
  if (nrow(d1) == 0) return(FALSE)
  
  # --- Aggregate per scenario x item
  agg <- d1 %>%
    dplyr::group_by(Scenario, Item) %>%
    dplyr::summarise(val = sum(Value, na.rm = TRUE), .groups = "drop")
  
  # --- Wide: force missing items to 0 for each scenario
  wide <- agg %>%
    tidyr::complete(
      Scenario = c(scen_ref, scen_extr),
      Item     = items,
      fill     = list(val = 0)
    ) %>%
    tidyr::pivot_wider(
      names_from  = Scenario,
      values_from = val,
      values_fill = 0
    )
  
  # Columns exist?
  if (!all(c(scen_ref, scen_extr) %in% names(wide))) return(FALSE)
  
  v_ref  <- as.numeric(wide[[scen_ref]])
  v_extr <- as.numeric(wide[[scen_extr]])
  
  total_ref <- sum(v_ref, na.rm = TRUE)
  if (!is.finite(total_ref) || total_ref <= 0) return(FALSE)
  
  rel_l1 <- sum(abs(v_extr - v_ref), na.rm = TRUE) / total_ref
  
  isTRUE(is.finite(rel_l1) && rel_l1 <= tol_rel)
}
