# deploy.R — déploiement shinyapps.io en un clic -----------------------------

message("▶️  Déploiement shinyapps.io — démarrage")

# ----------------------- 0) Pré-check projet --------------------------------
stopifnot(file.exists("app.R"))
if (!dir.exists("www")) message("ℹ️  Dossier www/ absent (ok si inutile).")
if (!dir.exists("data")) message("ℹ️  Dossier data/ absent (ok si inutile).")

# ----------------------- 1) Tokens & compte ---------------------------------
acc   <- Sys.getenv("RSCONNECT_ACCOUNT", unset = NA)
token <- Sys.getenv("RSCONNECT_TOKEN",   unset = NA)
secret<- Sys.getenv("RSCONNECT_SECRET",  unset = NA)

if (any(is.na(c(acc, token, secret)))) {
  stop("❌ Tokens manquants. Renseigne RSCONNECT_ACCOUNT / RSCONNECT_TOKEN / RSCONNECT_SECRET dans ~/.Renviron.")
}

if (!requireNamespace("rsconnect", quietly = TRUE)) install.packages("rsconnect")
rsconnect::setAccountInfo(name = acc, token = token, secret = secret)

# ----------------------- 2) Dépendances R -----------------------------------
# Si renv est présent, on s'assure qu'il est propre (mais on ne force pas restore ici)
if (file.exists("renv.lock")) {
  if (!requireNamespace("renv", quietly = TRUE)) install.packages("renv")
  try({
    source("renv/activate.R")  # active renv si dispo
    s <- renv::status()
    message("ℹ️  renv : ", if (grepl("No issues found", capture.output(print(s))[1])) "OK (consistent)" else "⚠️  vérifier renv::status()")
  }, silent = TRUE)
} else {
  message("ℹ️  Pas de renv.lock : déploiement avec versions CRAN actuelles.")
}

# ----------------------- 3) Manifeste & diagnostic ---------------------------
deps <- try(rsconnect::appDependencies(appDir = "."), silent = TRUE)
if (!inherits(deps, "try-error")) {
  message("ℹ️  Packages détectés (top 10) : ", paste(head(deps$Package, 10), collapse = ", "), " …")
}

# ----------------------- 4) Paramètres appli --------------------------------
APP_NAME  <- Sys.getenv("APP_NAME",  unset = "GlobAfrique")                     # change si besoin
APP_TITLE <- Sys.getenv("APP_TITLE", unset = "AFD – Prospective agricole")      # change si besoin

# ----------------------- 5) Déploiement -------------------------------------
message("🚀 Déploiement de l’app ‘", APP_NAME, "’…")
res <- rsconnect::deployApp(
  appDir   = ".",
  appName  = APP_NAME,
  appTitle = APP_TITLE,
  forceUpdate = TRUE
)

# ----------------------- 6) URL & logs --------------------------------------
url <- res$url
message("✅  Déployé : ", url)

# Affiche les logs en temps réel (Ctrl+C pour quitter)
message("📜 Ouverture des logs (streaming). Ctrl+C pour stopper.")
rsconnect::showLogs(appName = APP_NAME, streaming = TRUE)
