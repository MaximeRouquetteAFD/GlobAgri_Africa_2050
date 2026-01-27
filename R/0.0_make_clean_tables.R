# R/0.0_make_clean_tables.R
suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(stringr)
  library(stringi)   # NFC normalization
  library(fs)
})

# IMPORTANT:
# - on doit avoir scenario_code(), SCENARIOS_EXTRA_CHOICES, SCENARIO_BASE_YEAR_CODE, etc.
# - donc on charge la config en amont
source("R/0.0_config_scenarios.R")

# Charger la lecture centralisée (adapte le nom si besoin : 00_load_data.R vs 00_A.load_data.R)
source("R/0.0_load_data.R", local = TRUE)

# Détection soft baseline (optionnel : utile seulement si tes sources mettent "baseline"/"2018" au lieu du code)
.is_baseline_like <- function(scn) {
  s <- stringr::str_to_lower(scn)
  stringr::str_detect(s, "ann(é|e)e\\s*de\\s*base|baseline|\\b2018\\b")
}

# Normalisation canonique : UN SEUL CHEMIN
# -> NFC + scenario_code() (squish + "_" -> "-" + autres règles centralisées)
norm_scen <- function(x) scenario_code(stringi::stri_trans_nfc(as.character(x)))

make_clean_tables <- function(data_dir = "data", overwrite = TRUE) {
  stopifnot(fs::dir_exists(data_dir))
  
  # 1) Charger la BDD source
  ld <- load_data(data_dir = data_dir, assign = FALSE)
  raw_bdd <- ld$BDD_Clean
  
  # 2) Colonnes optionnelles
  if (!"System" %in% names(raw_bdd)) raw_bdd$System <- NA_character_
  if (!"Animal" %in% names(raw_bdd)) raw_bdd$Animal <- NA_character_
  
  # 3) Garde-fous schéma minimal
  required_cols <- c("Region","Scenario","Element","Item","Year","Unit","Value")
  missing <- setdiff(required_cols, names(raw_bdd))
  if (length(missing)) {
    stop("Colonnes manquantes dans BDD_CLEAN.csv : ", paste(missing, collapse = ", "))
  }
  
  # 4) Types
  raw_bdd <- raw_bdd %>%
    mutate(
      Region   = as.character(Region),
      Scenario = as.character(Scenario),
      Element  = as.character(Element),
      Item     = as.character(Item),
      System   = as.character(System),
      Animal   = as.character(Animal),
      Year     = suppressWarnings(as.integer(Year)),
      Unit     = as.character(Unit),
      Value    = suppressWarnings(as.numeric(Value))
    )
  
  # ------------------------------------------------------------
  # 5) Scénarios : produire UNIQUEMENT des CODES dans fact$Scenario
  # ------------------------------------------------------------
  scenario_final <- norm_scen(raw_bdd$Scenario)
  
  # 5.a) Recoder tout label UI d’extra -> CODE data (si jamais la source contient "Avec contrainte")
  # SCENARIOS_EXTRA_CHOICES : names = UI label ; values = CODE data
  extra_choices <- if (exists("SCENARIOS_EXTRA_CHOICES", inherits = TRUE)) {
    get("SCENARIOS_EXTRA_CHOICES", inherits = TRUE)
  } else NULL
  
  if (!is.null(extra_choices) && length(extra_choices) > 0) {
    # map: normalized(UI label) -> normalized(code)
    extra_map <- setNames(norm_scen(unname(extra_choices)), norm_scen(names(extra_choices)))
    scenario_final <- dplyr::recode(scenario_final, !!!extra_map, .default = scenario_final)
  }
  
  # 5.b) Recoder les variantes techniques "Prob_S_limitee" / etc -> CODE canonique
  # (si tu veux éviter des divergences underscore/espaces)
  if (!is.null(extra_choices) && "Avec contrainte" %in% names(extra_choices)) {
    code_contrainte <- norm_scen(unname(extra_choices[["Avec contrainte"]]))  # ex: "Prob-S-limitee"
    # couvre des variantes fréquentes
    is_constrained_variant <- stringr::str_detect(scenario_final, "(?i)^prob[- ]*s[- ]*lim") |
      stringr::str_detect(scenario_final, "(?i)limitee|limitée") |
      stringr::str_detect(scenario_final, "(?i)land[- ]*limit")
    scenario_final[is_constrained_variant] <- code_contrainte
  }
  
  # 5.c) Baseline : si la source contient "baseline"/"2018", forcer le code baseline (optionnel)
  if (exists("SCENARIO_BASE_YEAR_CODE", inherits = TRUE)) {
    base_code <- norm_scen(get("SCENARIO_BASE_YEAR_CODE", inherits = TRUE))
    scenario_final[.is_baseline_like(scenario_final)] <- base_code
  }
  
  # 5.d) GARDE-FOU : plus jamais de label UI dans Scenario final
  if (!is.null(extra_choices) && length(extra_choices) > 0) {
    ui_labels_norm <- norm_scen(names(extra_choices)) # ex: "Avec contrainte" (normalisé)
    leaked <- intersect(unique(scenario_final), ui_labels_norm)
    if (length(leaked) > 0) {
      stop(
        "Un label UI d’extra a fuité dans fact$Scenario (attendu: CODES uniquement). Valeurs en cause : ",
        paste(leaked, collapse = " | ")
      )
    }
  }
  
  # 6) Construire fact tidy (MINIMAL : uniquement les colonnes utiles)
  tidy <- raw_bdd %>%
    mutate(Scenario = scenario_final) %>%
    select(
      Region,
      Scenario,
      Element,
      Item,
      System,
      Animal,
      Year,
      Unit,
      Value
    ) %>%
    mutate(
      System = tidyr::replace_na(System, ""),
      Animal = tidyr::replace_na(Animal, "")
    )
  
  # 7) Dimension scénario (souvent utile pour debug / QA)
  dim_scenario <- tidy %>%
    distinct(Scenario) %>%
    arrange(Scenario)
  
  # 8) Warnings utiles
  if (anyNA(tidy$Value)) {
    warning("Certaines 'Value' sont NA après coercition numérique. Vérifie les données sources.")
  }
  
  # 9) Sauvegardes (IMPORTANT: fact_rds est dans data/)
  out_rds <- fs::path(data_dir, "fact_results_tidy.rds")
  out_csv <- fs::path("data_raw", "fact_results_tidy.csv")
  
  if (isTRUE(overwrite) || !fs::file_exists(out_rds)) {
    readr::write_delim(tidy, out_csv, delim = ";")
    saveRDS(tidy, out_rds)
    saveRDS(dim_scenario, fs::path(data_dir, "dim_scenario.rds"))
  }
  
  invisible(list(fact = tidy, dim_scenario = dim_scenario))
}
