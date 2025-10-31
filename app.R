# app.R — AFD Valorisation (Aperçu + Surface + KPI pays)
# ======================================================

library(shiny)
library(dplyr); library(forcats); library(tidyr)
library(ggplot2); library(plotly); library(scales)
library(stringr); library(shinyWidgets); library(DT)
library(readr); library(tibble)

# ---- Sécurise le help_type
ht <- getOption("help_type")
if (length(ht) != 1L || !(ht %in% c("html","text"))) options(help_type = "html")

# ---- Chargements projet (doit fournir 'fact' + constantes)
source("global.R", local = FALSE, chdir = TRUE)
source("R/02_utils_palette.R")
source("R/03_mod_energy_items.R")
source("R/05_mod_area_stacked.R")
source("R/07_mod_dsq_cards.R")
source("R/08_mod_energy_sankey.R")
source("R/09_mod_slope_triptych.R")
source("R/10_mod_surface_cards.R")
source("R/11_mod_yield_hypothesis.R")
source("R/12_mod_energy_kpi_cards.R")


# ---- Helpers
if (!exists("%||%", mode = "function")) `%||%` <- function(a, b) if (is.null(a)) b else a
fmt0 <- function(x) if (is.na(x)) "—" else format(round(x), big.mark = " ", scientific = FALSE, trim = TRUE)
fmt1 <- function(x) if (is.na(x)) "—" else formatC(x, format = "f", digits = 1, big.mark = " ")

# ===================================================================
# Données manuelles pour le cadre KPI
# ===================================================================
pop2050_dict <- c(
  "Senegal"=30364954,"Ivory Coast"=55746985,"Ethiopia"=225021874.5,"Morocco"=43440480.5,"South Africa"=79177328,
  "Tanzania"=129621102.5,"Nigeria"=359185955.5,"Tunisia"=13145141,"Algeria"=59565554.5,"Angola"=74295394.5,
  "Benin"=24433809.5,"Botswana"=3437430,"Burkina Faso"=37304383,"Cameroon"=51096317,"Chad"=38857686,"Egypt"=161630192,
  "Gambia"=4301895.5,"Ghana"=50553047,"Guinea"=23404583.5,"Kenya"=83593239,"Mali"=46154079,"Mozambique"=63530951.5,
  "Namibia"=4512300.5,"Niger"=52513876,"North and South Sudan"=103548356,"Rwanda"=22707910,"Zambia"=38083385.5,"Zimbabwe"=25866384.5
)
pop2018_dict <- c(
  "Senegal"=15914498.5,"Ivory Coast"=27464172.5,"Ethiopia"=112664152.5,"Morocco"=35839760,"South Africa"=58613000.5,
  "Tanzania"=57437144.5,"Nigeria"=204938754.5,"Tunisia"=11765514.5,"Algeria"=42505035,"Angola"=31297155,"Benin"=12383347.5,
  "Botswana"=2299141,"Burkina Faso"=20438288,"Cameroon"=24806383,"Chad"=16156530.5,"Egypt"=105682093.5,"Gambia"=2399632,
  "Ghana"=30637584.5,"Guinea"=12704774,"Kenya"=50207107.5,"Mali"=20442029.5,"Mozambique"=29018644.5,"Namibia"=2577326.5,
  "Niger"=22188069,"North and South Sudan"=54353573,"Rwanda"=12487996,"Zambia"=17973569,"Zimbabwe"=15034452
)
area_km2_dict <- c(
  "Senegal"=196710,"Ivory Coast"=322460,"Ethiopia"=1136240,"Morocco"=446550,"South Africa"=1219090,"Tanzania"=947300,
  "Nigeria"=923770,"Tunisia"=163610,"Algeria"=2381741,"Angola"=1246700,"Benin"=114760,"Botswana"=581730,"Burkina Faso"=274200,
  "Cameroon"=475440,"Chad"=1284000,"Egypt"=1001450,"Gambia"=11300,"Ghana"=238533,"Guinea"=245860,"Kenya"=580370,"Mali"=1240190,
  "Mozambique"=799380,"Namibia"=824290,"Niger"=1267000,"North and South Sudan"=2524883,"Rwanda"=26340,"Zambia"=752610,"Zimbabwe"=390760
)
flag_iso2_dict <- c(
  "Senegal"="sn","Ivory Coast"="ci","Ethiopia"="et","Morocco"="ma","South Africa"="za","Tanzania"="tz","Nigeria"="ng","Tunisia"="tn",
  "Algeria"="dz","Angola"="ao","Benin"="bj","Botswana"="bw","Burkina Faso"="bf","Cameroon"="cm","Chad"="td","Egypt"="eg","Gambia"="gm",
  "Ghana"="gh","Guinea"="gn","Kenya"="ke","Mali"="ml","Mozambique"="mz","Namibia"="na","Niger"="ne","North and South Sudan"=NA_character_,
  "Rwanda"="rw","Zambia"="zm","Zimbabwe"="zw"
)

# Contrôles compacts
for (chk in list(
  list(nm="pop2018_dict", x=pop2018_dict),
  list(nm="pop2050_dict", x=pop2050_dict),
  list(nm="area_km2_dict", x=area_km2_dict),
  list(nm="flag_iso2_dict", x=flag_iso2_dict)
)) {
  miss <- setdiff(unique(fact$Region), names(chk$x))
  if (length(miss)) message("À ajouter dans ", chk$nm, " : ", paste(miss, collapse = ", "))
}

# ===================================================================
# UI avec footer global (non fixe) et wrapper .app-page
# ===================================================================

ui <- tagList(
  
  # ----------- CONTENU PRINCIPAL DE L'APP -----------
  div(
    class = "app-page",
    
    navbarPage(
      title = "AFD x GlobAgri",
      id    = "main_tabs",
      
      # ---------- HEADER GLOBAL (bandeau du haut) ----------
      header = tagList(
        # Charge le CSS global
        tags$head(includeCSS("www/app.css")),
        
        # Ajoute la classe .app sur le <body> pour cibler les styles globaux
        tags$script(HTML("document.body.classList.add('app');")),
        
        # Affiche le bandeau SEULEMENT si on n'est PAS dans l'onglet 'about'
        conditionalPanel(
          condition = "input.main_tabs !== 'about'",
          div(
            class = "app-header",
            div(
              class = "container-fluid",
              div(
                class = "row app-header-row",
                
                # Colonne gauche : sélecteur de pays
                div(
                  class = "col-xs-12 col-sm-4",
                  selectInput(
                    "country_global", "Pays",
                    choices  = sort(unique(fact$Region)),
                    selected = default_country
                  )
                ),
                
                # Colonne droite : KPI population / info pays
                div(
                  class = "col-xs-12 col-sm-4 text-right",
                  uiOutput("pop2050_card"),
                )
              )
            )
          )
        )
      ),
      
      # --------- ONGLET 1 : HYPOTHÈSES ----------
      tabPanel(
        title = "Hypothèses",
        div(
          class = "container-fluid",
          mod_energy_items_ui("energy_hypo"),
          tags$br(),
          mod_energy_kpis_ui("energy_kpis"),
          tags$hr(style = "margin:10px 0; border-color:#e9ecef;"),
          mod_yield_ui("yield_hypo")
        )
      ),
      
      # --------- ONGLET 2 : SURFACES ----------
      tabPanel(
        title = "Surfaces et émissions de CO₂",
        div(
          class = "container-fluid",   # <-- remet des marges et une largeur cohérente
          
          sidebarLayout(
            sidebarPanel(
              width = 2,
              selectInput(
                "surface_area_item", "Item de surface :",
                choices = c(
                  "Tout" = "__ALL__",
                  "Cropland" = "Cropland",
                  "Land under perm. meadows and pastures" = "Land under perm. meadows and pastures",
                  "Agricultural land occupation (Farm)"   = "Agricultural land occupation (Farm)",
                  "Forest land" = "Forest land"
                ),
                selected = "__ALL__",
                multiple = FALSE
              )
            ),
            mainPanel(
              width = 10,
              div(
                class = "u-card u-card--flat u-card--hover",
                mod_area_stacked_ui("area")
              ),
              
              # Les cartes KPI surfaces en dessous
              mod_surface_cards_ui("surface_kpis")
            )
          )
        )
      ),
      
      # --------- ONGLET 3 : FLUX ÉNERGÉTIQUES ----------
      tabPanel(
        title = "Flux énergétiques et production",
        div(
          class = "app container-fluid",
          mod_dsq_cards_ui("dsq_cards"),
          tags$br(),
          mod_energy_sankey_ui("sankey"),
          tags$hr(style = "margin:10px 0; border-color:#e9ecef;"),
          fluidRow(
            column(
              width = 12,
              mod_slope_triptych_ui("triptych", height = "360px")
            )
          )
        )
      ),
      
      # ---------- ONGLET 4 : COMMERCE EXTÉRIEUR ----------
      tabPanel(
        title = "Commerce extérieur",
        value = "commerce",
        div(
          class = "container-fluid",
          h3(class = "app-title app-title--lg", "Commerce extérieur et dépendance"),
          p(class = "app-subtitle", "Contenu à venir.")
        )
      ),
      
      # ---------- ONGLET 5 : SYNTHÈSE ----------
      tabPanel(
        title = "Synthèse",
        value = "synthese",
        div(
          class = "container-fluid",
          h3(class = "app-title app-title--lg", "Synthèse"),
          p(class = "app-subtitle", "Contenu à venir.")
        )
      ),
      
      # ---------- ONGLET 6 : À PROPOS ----------
      tabPanel(
        title = "À propos",
        value = "about",
        div(
          class = "container-fluid",
          
          div(
            class = "about-doc",
            includeMarkdown("www/about.md")
          ),
          
          br(),
          
          div(
            class = "about-doc",
            DT::dataTableOutput("tbl_assumptions")
          )
        )
      )
    )
 ),
  
  # ----------- FOOTER GLOBAL (apparaît sous tout le contenu) -----------
  tags$footer(
    class = "app-footer",
    div(class = "container-fluid",
      div(class = "app-footer-row",
        
        # Bloc marque (logo)
        div(class = "brand",
          tags$img(
            src   = "logo_footer.png",   # le fichier doit être dans www/
            class = "footer-logo",
            alt   = "AFD × GlobAgri"
          )
        ),
        
        # Texte mention légale / scénarios
        div(
          HTML("&copy; 2025 AFD × GlobAgri — Scénarios : même diète, probable, saine, Prob-S-limitée")
        )
      )
    )
  )
)


# ===================================================================
# SERVER
# ===================================================================
server <- function(input, output, session){
  
  # ===== Pays GLOBAL
  r_country <- reactive(input$country_global)
  
  # ===== KPI (population 2050, delta vs 2018, densité, drapeau)
  r_pop2050 <- reactive({ val <- unname(pop2050_dict[[ r_country() ]]); if (is.null(val)) NA_real_ else as.numeric(val) })
  r_pop2018 <- reactive({ val <- unname(pop2018_dict[[ r_country() ]]); if (is.null(val)) NA_real_ else as.numeric(val) })
  r_area_km2 <- reactive({ val <- unname(area_km2_dict[[ r_country() ]]); if (is.null(val)) NA_real_ else as.numeric(val) })
  r_flag_src <- reactive({
    nm <- r_country()
    if (nm == "North and South Sudan") return(c(file.path("flags","sd.png"), file.path("flags","ss.png")))
    iso2 <- flag_iso2_dict[[ nm ]]
    if (is.null(iso2) || is.na(iso2)) return(NA_character_)
    rel <- file.path("flags", paste0(tolower(iso2), ".png"))
    if (file.exists(file.path("www", rel))) rel else NA_character_
  })
  
  output$pop2050_card <- renderUI({
    v50 <- r_pop2050(); v18 <- r_pop2018(); a52 <- r_area_km2()
    delta_abs <- if (is.finite(v50) && is.finite(v18)) v50 - v18 else NA_real_
    delta_pct <- if (is.finite(v50) && is.finite(v18) && v18 > 0) 100 * delta_abs / v18 else NA_real_
    dens50    <- if (is.finite(v50) && is.finite(a52) && a52 > 0) v50 / a52 else NA_real_
    
    # prépare le(s) drapeau(x)
    flag_src <- r_flag_src()
    flag_img <- NULL
    if (is.character(flag_src)) {
      valid <- flag_src[!is.na(flag_src)]
      if (length(valid) == 1) {
        flag_img <- tags$img(src = valid, class = "kpi-flag", alt = paste0("Drapeau ", r_country()))
      } else if (length(valid) > 1) {
        flag_img <- tagList(lapply(valid, function(p)
          tags$img(src = p, class = "kpi-flag", alt = paste0("Drapeau ", r_country()))
        ))
      }
    }
    
    div(class = "kpi-card",
        # --- drapeau PUIS titre, dans le même header
        div(class = "kpi-head",
            flag_img,
            p(class = "kpi-title", "Population projetée en 2050 : ")
        ),
        p(class = "kpi-value", fmt0(v50), tags$span(class = "kpi-unit", "habitants")),
        p(class = "kpi-sub",
          if (is.na(delta_abs)) "Augmentation vs 2018 : —"
          else HTML(paste0("Augmentation vs 2018 : <b>", fmt0(delta_abs),
                           " hab.</b> (", fmt1(delta_pct), " %)"))
        ),
        p(class = "kpi-sub",
          if (is.na(dens50)) "Densité projetée en 2050 : —"
          else HTML(paste0("Densité projetée en 2050 : <b>", fmt1(dens50), " hab./km²</b>"))
        )
    )
  })
  
  # =======================
  # ONGLET 1 — HYPOTHÈSES
  # =======================
  
  # Liste complète des items énergie dispo pour le pays (définie AVANT usage)
  r_items_all <- reactive({
    fact %>%
      filter(Region == r_country(), str_trim(Element) == ENERGY_ELEMENT) %>%
      distinct(Item) %>% arrange(Item) %>% pull(Item)
  })
  
  # Graphique n°1 réutilisé
  mod_energy_items_server("energy_hypo", fact = fact, r_country = r_country,
                          r_items = r_items_all, ENERGY_ELEMENT = ENERGY_ELEMENT)
  
  mod_energy_kpis_server(
    id = "energy_kpis",
    fact = fact,
    r_country = r_country,
    ENERGY_ELEMENT = ENERGY_ELEMENT   # tu l’as déjà dans ton app
  )
  
  mod_yield_server(
    id = "yield_hypo",
    fact_reactive = reactive(fact),
    country_sel   = r_country   # <— ton reactive Region déjà existant
  )
  
  # =======================
  # ONGLET 3 — SURFACES
  # =======================
  # scénarios disponibles dans les données (ordre fixé)
  scenarios_avail_surface <- reactive({
    allowed <- c("Même diète", "Diète probable", "Diète saine")
    present <- fact %>%
      dplyr::filter(Region == r_country(), Scenario %in% allowed) %>%
      dplyr::distinct(Scenario) %>% dplyr::pull(Scenario)
    intersect(allowed, present)
  })
  
  # on passe TOUJOURS les scénarios dispo (plus d'UI de filtre)
  r_scenarios_surface <- reactive(scenarios_avail_surface())
  r_area_selected     <- reactive(input$surface_area_item)
  
  mod_area_stacked_server("area", fact = fact, r_country = r_country,
                          r_scenarios = r_scenarios_surface, r_area_item = r_area_selected)
  mod_surface_cards_server(
    "surface_kpis",
    fact        = fact,
    r_country   = r_country,
    r_scenarios = reactive(NULL),                  
    r_area_item = reactive(input$surface_area_item)
  )
  
  # =======================
  # ONGLET 3 — FLUX ALIMENTAIRES ET PRODUCTION 
  # =======================
  dsq <- mod_dsq_cards_server("dsq_cards", fact = fact, r_country = r_country)
  mod_energy_sankey_server("sankey", fact = fact, r_country = r_country,
                           r_selected_scenario = dsq$selected_scenario, unit_label = "Gcal")
  mod_slope_triptych_server("triptych", fact = fact, r_country = r_country)
  
  # =======================
  # ONGLET — A PROPOS
  # =======================
  
  mod_about_server("about")
  
}

shinyApp(ui, server)

