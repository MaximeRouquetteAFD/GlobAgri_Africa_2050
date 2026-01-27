## ==========================
## global.R — AFD Valorisation
## Rebuild intelligent de `fact` depuis data_raw/BDD_CLEAN.csv
## Rebuild ONLY if build inputs changed (hash), not based on mtime
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

src_csv <- fs::path("data_raw", "BDD_CLEAN.csv")
src_available <- all(file_exists(src_csv))

## --- Scripts qui impactent la construction des RDS
## --- Inputs qui DOIVENT déclencher un rebuild s'ils changent
build_inputs <- c(
  src_csv,
  "R/00_make_clean_tables.R",
  "R/00_config_scenarios.R"
)

## --- Hashing robuste (clés = chemins absolus canonisés)
canon_path <- function(p) normalizePath(p, winslash = "/", mustWork = FALSE)
hash_file  <- function(p) digest(file = p, algo = "xxhash64")

hashes_current <- function(paths){
  # paths supposés existants
  p_abs <- vapply(paths, canon_path, character(1))
  ord   <- order(p_abs)
  p_abs <- p_abs[ord]
  setNames(vapply(p_abs, hash_file, character(1)), p_abs)
}

hashes_equal <- function(paths, meta_path){
  if (!all(fs::file_exists(paths))) return(TRUE)      # pas de rebuild si on ne peut pas vérifier
  if (!fs::file_exists(meta_path)) return(FALSE)
  
  meta <- readRDS(meta_path)
  cur  <- hashes_current(paths)
  
  identical(meta, cur)
}

hash_diff_report <- function(paths, meta_path){
  if (!all(fs::file_exists(paths))) {
    message("► HASH REPORT: some build inputs missing; skipping report.")
    return(invisible(NULL))
  }
  cur <- hashes_current(paths)
  
  if (!fs::file_exists(meta_path)) {
    message("► HASH REPORT: meta_rds missing -> rebuild expected.")
    print(data.frame(file = names(cur), current = unname(cur), stringsAsFactors = FALSE))
    return(invisible(NULL))
  }
  
  meta <- readRDS(meta_path)
  
  all_files <- sort(unique(c(names(meta), names(cur))))
  df <- data.frame(
    file    = all_files,
    meta    = unname(meta[all_files]),
    current = unname(cur[all_files]),
    same    = unname(meta[all_files] == cur[all_files]),
    exists  = fs::file_exists(all_files),
    stringsAsFactors = FALSE
  )
  
  # NA (fichiers absents d’un côté) => same = FALSE
  df$same[is.na(df$same)] <- FALSE
  
  message("► HASH REPORT (mismatches only):")
  print(df[df$same == FALSE, ], row.names = FALSE)
  
  invisible(df)
}

## --- Forcer manuellement (option R ou variable d'environnement)
force_rebuild <- isTRUE(getOption("AFD_FORCE_REBUILD")) ||
  identical(Sys.getenv("AFD_FORCE_REBUILD"), "1")

message("► AFD_FORCE_REBUILD option = ", getOption("AFD_FORCE_REBUILD"),
        " | env = '", Sys.getenv("AFD_FORCE_REBUILD"), "'",
        " | force_rebuild = ", force_rebuild)

## --- Cas bloquant
if (!fs::file_exists(fact_rds) && !src_available) {
  stop("RDS absent (", fact_rds, ") et source CSV absente (", src_csv,
       "). Impossible de (re)construire.")
}

## --- Décision rebuild (HASH ONLY)
hash_ok <- hashes_equal(build_inputs, meta_rds)

message("► CHECK: src_available=", src_available,
        " | fact_rds_exists=", fs::file_exists(fact_rds),
        " | hash_ok=", hash_ok)

do_rebuild <- if (isTRUE(force_rebuild)) {
  src_available
} else {
  src_available && (!fs::file_exists(fact_rds) || !hash_ok)
}

if (do_rebuild) {
  # Diagnostic précis: QUEL fichier bouge ?
  hash_diff_report(build_inputs, meta_rds)
  
  message("► BUILD START: ", Sys.time())
  message("  global.R path = ", normalizePath("global.R", mustWork = FALSE))
  
  source("R/00_make_clean_tables.R", local = TRUE)
  make_clean_tables(data_dir = data_dir, overwrite = TRUE)
  
  # Sauvegarde meta stable (chemins canonisés)
  meta <- hashes_current(build_inputs)
  saveRDS(meta, meta_rds)
  
  message("► BUILD END: ", Sys.time())
} else {
  message("► SKIP BUILD (do_rebuild=FALSE)")
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
message(
  "► fact: ", nrow(fact), " lignes ; pays: ", length(unique(fact$Region)),
  " ; scénarios: ", length(unique(fact$Scenario))
)
