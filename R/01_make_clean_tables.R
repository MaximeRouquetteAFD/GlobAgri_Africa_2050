# R/01_make_clean_tables.R
suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(stringr)
  library(stringi)   # normalisation accents (NFC)
  library(tidyr)
  library(forcats)
  library(fs)
})

# Charger la lecture centralisée
source("R/00_load_data.R", local = TRUE)

# --- Helpers légers : on normalise sans recoder vers 5 familles
.norm_scenario_keep <- function(x) {
  x |>
    stringi::stri_trans_nfc() |>
    stringr::str_replace_all("_", " ") |>
    stringr::str_squish()
}

# Méthode technique éventuellement encodée dans le libellé de scénario
.extract_method <- function(x) {
  stringr::str_extract(x, "(?i)(50perGapMuellerAgMIPCC|50perGap\\w*|Mueller|AgMIP|IPCC)")
}

# Détection soft de la baseline
.is_baseline_like <- function(scn) {
  s <- stringr::str_to_lower(scn)
  stringr::str_detect(s, "ann(é|e)e\\s*de\\s*base|baseline|\\b2018\\b")
}

# -------------------------------------------------------------------
# Fonction principale
# -------------------------------------------------------------------
#' Construire les tables propres (version ouverte)
#' @param data_dir Dossier contenant BDD_CLEAN.csv
#' @param overwrite Réécriture des fichiers de sortie
#' @return (invisible) liste avec fact et petites dimensions
make_clean_tables <- function(data_dir = "data", overwrite = TRUE) {
  stopifnot(fs::dir_exists(data_dir))
  
  # 1) Charger la BDD source
  ld <- load_data(data_dir = data_dir, assign = FALSE)
  raw_bdd <- ld$BDD_Clean
  
  # 2) Garde-fous schéma minimal
  required_cols <- c("Region","Scenario","Element","Item","Year","Unit","Value")
  missing <- setdiff(required_cols, names(raw_bdd))
  if (length(missing)) {
    stop("Colonnes manquantes dans BDD_CLEAN.csv : ", paste(missing, collapse = ", "))
  }
  
  # 3) Types au plus tôt
  raw_bdd <- raw_bdd %>%
    mutate(
      Region   = as.character(Region),
      Scenario = as.character(Scenario),
      Element  = as.character(Element),
      Item     = as.character(Item),
      Year     = suppressWarnings(as.integer(Year)),
      Unit     = as.character(Unit),
      Value    = suppressWarnings(as.numeric(Value))
    )
  
  # --- garder TOUS les scénarios (nettoyés) + recoder "Avec contrainte"
  scenario_raw   <- as.character(raw_bdd$Scenario)
  scenario_norm  <- .norm_scenario_keep(scenario_raw)  # "Prob S limitee" -> "Prob S limitee"
  
  # source 'constraint' éventuelle (colonne existante)
  has_constraint_col <- "constraint" %in% names(raw_bdd)
  constraint_src_txt <- if (has_constraint_col) {
    tolower(trimws(as.character(raw_bdd$constraint)))
  } else {
    rep("", nrow(raw_bdd))
  }
  
  # détection des cas 'Prob_S_limitee' & co (insensible à la casse, espaces/_,-)
  is_constrained <- grepl("(?i)^\\s*prob[\\s_\\-]*s[\\s_\\-]*lim", scenario_norm) |
    grepl("(?i)land[\\s_\\-]*limit", scenario_norm) |
    grepl("(?i)limitee|limitée", scenario_norm) |
    grepl("(?i)land[\\s_\\-]*limited|limitee|limitée", constraint_src_txt)
  
  # libellé final du scénario
  scenario_final <- ifelse(is_constrained, "Avec contrainte", scenario_norm)
  
  tidy <- raw_bdd %>%
    mutate(
      Scenario_raw = scenario_raw,
      Scenario     = scenario_final,
      method       = coalesce(.extract_method(Scenario_raw), ""),
      constraint   = as.logical(is_constrained),
      is_baseline  = .is_baseline_like(Scenario) & Year == 2018
    ) %>%
    transmute(
      Region,
      Scenario,
      method,
      constraint,          # <-- logique (TRUE/FALSE)
      Element,
      Item,
      Year  = as.integer(Year),
      Unit  = as.character(Unit),
      Value = as.numeric(Value),
      is_baseline = as.logical(is_baseline)
    )
  
  # 5) Petites dimensions (facultatives)
  dim_scenario <- tidy %>%
    distinct(Scenario) %>% arrange(Scenario)
  
  dim_method <- tidy %>%
    distinct(method) %>% filter(nzchar(method)) %>% arrange(method)
  
  dim_constraint <- tidy %>%
    distinct(constraint) %>%
    arrange(constraint) %>%
    rename(label = constraint)
  
  # 6) Warnings non bloquants utiles
  if (anyNA(tidy$Value)) {
    warning("Certaines 'Value' sont NA après coercition numérique. Vérifie les données sources.")
  }
  if (!all(tidy$Year >= 1900 | is.na(tidy$Year))) {
    warning("Des 'Year' semblent hors plage (avant 1900).")
  }
  
  # 7) Sauvegardes
  out_csv <- fs::path(data_dir, "fact_results_tidy.csv")
  out_rds <- fs::path(data_dir, "fact_results_tidy.rds")
  if (isTRUE(overwrite) || !fs::file_exists(out_rds)) {
    write_delim(tidy, out_csv, delim = ";")
    saveRDS(tidy, out_rds)
    saveRDS(dim_scenario,   fs::path(data_dir, "dim_scenario.rds"))
    saveRDS(dim_method,     fs::path(data_dir, "dim_method.rds"))
    saveRDS(dim_constraint, fs::path(data_dir, "dim_constraint.rds"))
  }
  
  invisible(list(
    fact           = tidy,
    dim_scenario   = dim_scenario,
    dim_method     = dim_method,
    dim_constraint = dim_constraint
  ))
}

