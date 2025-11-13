## ==========================
## global.R — AFD Valorisation
## Rebuild intelligent de `fact` depuis data/BDD_CLEAN.csv
## ==========================

source("R/99_utils_plotly_theme.R")

suppressPackageStartupMessages({
  library(fs)
  library(dplyr)
  library(stringr)
  library(digest)   
})

## --- Paramètres projet
data_dir  <- "data"
stopifnot(dir_exists(data_dir))

fact_rds  <- fs::path(data_dir, "fact_results_tidy.rds")
meta_rds  <- fs::path(data_dir, "fact_build_meta.rds")   
src_csv   <- c(fs::path(data_dir, "BDD_CLEAN.csv"))   

## --- Aide: détection changement
needs_rebuild_time <- function(src_paths, rds_path){
  if (!file_exists(rds_path)) return(TRUE)
  if (!all(file_exists(src_paths))) {
    stop("Source(s) manquante(s) : ", paste(src_paths[!file_exists(src_paths)], collapse=", "))
  }
  rds_time <- file_info(rds_path)$modification_time
  any(file_info(src_paths)$modification_time > rds_time)
}
hash_file <- function(p) digest(file = p, algo = "xxhash64")
hashes_equal <- function(src_paths, meta_path){
  if (!file_exists(meta_path)) return(FALSE)
  meta <- readRDS(meta_path)
  all(vapply(src_paths, function(p) identical(meta[[basename(p)]], hash_file(p)), logical(1)))
}

## --- Forcer manuellement (option R ou variable d'environnement)
force_rebuild <- isTRUE(getOption("AFD_FORCE_REBUILD")) ||
  identical(Sys.getenv("AFD_FORCE_REBUILD"), "1")

## --- Décider si rebuild
do_rebuild <- force_rebuild || needs_rebuild_time(src_csv, fact_rds) || !hashes_equal(src_csv, meta_rds)

if (do_rebuild) {
  message("► Build ‘fact’ (motif : ",
          if (force_rebuild) "FORCE"
          else if (!file_exists(fact_rds)) "RDS absent"
          else if (!hashes_equal(src_csv, meta_rds)) "hash différent"
          else "sources plus récentes",
          ") …")
  
  source("R/01_make_clean_tables.R", local = TRUE)
  make_clean_tables(data_dir = data_dir, overwrite = TRUE)
  
  # met à jour les hash pour traçabilité
  hashes <- setNames(lapply(src_csv, hash_file), basename(src_csv))
  saveRDS(hashes, meta_rds)
} else {
  message("► ‘fact’ à jour — pas de rebuild (", fact_rds, ")")
}

## --- Chargement de la table propre
fact <- readRDS(fact_rds)

## --- Constantes utilisées par l’app
ENERGY_ELEMENT <- "FoodEnergy per capita"

default_country <- {
  u <- sort(unique(fact$Region))
  if ("Algeria" %in% u) "Algeria" else u[[1]]
}

if (requireNamespace("ragg", quietly = TRUE)) {
  options(shiny.graphics.dev = "ragg")   # plots Shiny via ragg::agg_png
}

## --- Petit log utile (optionnel)
message("► fact: ", nrow(fact), " lignes ; pays: ", length(unique(fact$Region)),
        " ; scénarios: ", length(unique(fact$Scenario)))

