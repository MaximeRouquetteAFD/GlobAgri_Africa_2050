# R/00_load_data.R
# ---------------------------------------------
# Chargement des fichiers sources du projet :
# - BDD_CLEAN.csv (obligatoire)
# - dim_country.csv (optionnel)
# Auto-détection du séparateur ; nettoyage basique des chaînes.
# ---------------------------------------------

# helper : deviner le séparateur à partir de la 1re ligne
.guess_delim <- function(path) {
  l1 <- readLines(path, n = 1, warn = FALSE)
  if (is.na(l1) || !nzchar(l1)) return(",")
  if (stringr::str_count(l1, ";") >= stringr::str_count(l1, ",")) ";" else ","
}

#' Charger les données brutes du projet
#' @param data_dir Dossier contenant les CSV (par défaut "data")
#' @param assign   Si TRUE, assigne BDD_Clean et dim_country (si présent) en global
#' @param encoding Encodage utilisé à la lecture (UTF-8 par défaut)
#' @return Une liste contenant BDD_Clean, dim_country (ou NULL), paths et delims
load_data <- function(
    data_dir = "data",
    assign   = TRUE,
    encoding = "UTF-8"
){
  stopifnot(fs::dir_exists(data_dir))
  
  # chemins
  bdd_path  <- fs::path("data_raw", "BDD_CLEAN.csv")
  dimc_path <- fs::path("data_raw", "dim_country.csv")
  
  if (!fs::file_exists(bdd_path)) {
    stop("Fichier introuvable : ", bdd_path)
  }
  
  # --- BDD_CLEAN : auto-détection du séparateur
  delim_bdd <- .guess_delim(bdd_path)
  BDD_Clean <- readr::read_delim(
    file   = bdd_path,
    delim  = delim_bdd,
    locale = readr::locale(encoding = encoding, decimal_mark = ".", grouping_mark = ","),
    show_col_types = FALSE,
    progress = FALSE
  )
  
  # nettoyage chaînes
  BDD_Clean <- dplyr::mutate(BDD_Clean,
                             dplyr::across(dplyr::where(is.character), ~ trimws(.x))
  )
  
  # --- dim_country : optionnel
  if (fs::file_exists(dimc_path)) {
    delim_dim   <- .guess_delim(dimc_path)
    dim_country <- readr::read_delim(
      file   = dimc_path,
      delim  = delim_dim,
      locale = readr::locale(encoding = encoding),
      show_col_types = FALSE,
      progress = FALSE
    )
    dim_country <- dplyr::mutate(dim_country,
                                 dplyr::across(dplyr::where(is.character), ~ trimws(.x))
    )
  } else {
    delim_dim   <- NA_character_
    dim_country <- NULL
  }
  
  out <- list(
    BDD_Clean   = BDD_Clean,
    dim_country = dim_country,        # peut être NULL
    paths       = list(bdd = bdd_path, dimc = if (is.null(dim_country)) NA_character_ else dimc_path),
    delims      = list(bdd = delim_bdd, dimc = delim_dim)
  )
  
  if (assign) {
    list2env(list(BDD_Clean = BDD_Clean), envir = .GlobalEnv)
    if (!is.null(dim_country)) assign("dim_country", dim_country, envir = .GlobalEnv)
  }
  
  return(out)
}

