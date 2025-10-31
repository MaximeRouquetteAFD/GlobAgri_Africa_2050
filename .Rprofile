## 1) Filet de sécurité : force le cache persistant si non défini
if (!nzchar(Sys.getenv("RENV_PATHS_CACHE"))) {
  Sys.setenv(RENV_PATHS_CACHE = "~/work/.renvcache")
}

## 2) Active renv automatiquement si présent dans le projet
if (file.exists("renv/activate.R")) {
  source("renv/activate.R")
}

