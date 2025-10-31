# R/utils_palette.R
library(scales)

`%||%` <- function(a, b) { if (is.null(a) || length(a)==0 || (is.numeric(a) && !is.finite(a))) b else a }

# ========= Couleurs =============

ITEM_COLORS <- c(
  "Cereals"               = "#E15759",
  "Dairy"                 = "#4E79A7",
  "Meat, eggs and fish"   = "#59A14F",
  "Oil"                   = "#F28E2B",
  "Other"                 = "#AF7AA1",
  "Pulses"                = "#76B7B2",
  "Roots and tubers"      = "#EDC948",
  "Sugar"                 = "#FF9DA7",
  "Vegetables and fruits" = "#9C755F"
)
item_colors_for <- function(items){
  items <- as.character(items)
  cols <- ITEM_COLORS
  miss <- setdiff(items, names(cols))
  if (length(miss)) cols <- c(cols, setNames(scales::hue_pal()(length(miss)), miss))
  cols[items]
}

AREA_COLORS <- c(
  "Cropland"                              = "#F28E2B",
  "Land under perm. meadows and pastures" = "#59A14F",
  "Agricultural land occupation (Farm)"   = "#76B7B2",
  "Forest land"                           = "#8CD17D"
)

area_colors_for <- function(items){
  items <- as.character(items)
  cols <- AREA_COLORS
  miss <- setdiff(items, names(cols))
  if (length(miss)) cols <- c(cols, setNames(scales::hue_pal()(length(miss)), miss))
  cols[items]
}

# Palette de heatmap (symétrique) pour la table
HEAT_BREAKS <- c(-1.50, -1.00, -0.50, -0.15, 0.15, 0.50, 1.00, 1.50)
HEAT_COLORS <- c(
  "#D07A0B", "#E89A1C", "#F5C269", "#FFFAF0",
  "#FFFFFF",
  "#FFFAF0", "#F5C269", "#E89A1C", "#D07A0B"
) # length = length(breaks)+1

# ---- Part des surfaces (0–1) : seuils et couleurs
# Catégories : <5% ; 5–10% ; 10–15% ; 15–20% ; >20%
SHARE_BREAKS <- c(0.05, 0.10, 0.15, 0.20)

# Même camaïeu que la colonne "Evolution …" (partie positive, du plus clair au plus foncé)
# Avec la palette actuelle (9 couleurs), 5:9 = centre clair -> oranges plus foncés.
SHARE_COLORS <- (function(){
  if (exists("HEAT_COLORS", inherits = TRUE) && length(HEAT_COLORS) >= 9) {
    HEAT_COLORS[5:9]
  } else {
    # Repli robuste si la palette thermique n'est pas chargée
    c("#FFFFFF", "#FFFAF0", "#F5C269", "#E89A1C", "#D07A0B")
  }
})()


# Palette (adaptée daltonisme, orange/bleu)
pasture_arid_palette <- function(){
  c(
    "Aride"     = "#F28E2B",  # orange
    "Non aride" = "#4E79A7"   # bleu
  )
}

# ========= R/11 - hypothèses de rendement =============

# 02_utils_palette.R
PALETTE_YEARS <- c("2018" = "#D43F3A", "2050" = "#F0AD4E")  # ou tes couleurs projet


# ========= ORDRE DES ITEMS =============

ITEM_ORDER_TBL <- c(
  "Maize","Wheat","Rice","Other Cereals","Oilcrops",
  "Pulses and Soyabeans","Millet and Sorghum","Roots and Tuber",
  "Tea, cocoa, coffee, oilpalm, sugar cane","Other crops"
)

NO_IMPUTE_ITEMS <- c("Tea, cocoa, coffee, oilpalm, sugar cane", "Other crops")
ANIMAL_ITEMS <- c("Dairy", "Bovine meat")

YIELD_ITEM_REGEX <- c(
  "Maize"                                   = "\\bMaize\\b",
  "Wheat"                                   = "\\bWheat\\b",
  "Rice"                                    = "\\bRice\\b",
  "Other Cereals"                           = "\\bother\\s*cereals\\b",
  "Oilcrops"                                = "Other\\s*Oilcrops|Sunflowerseed|Olives",
  "Pulses and Soyabeans"                    = "Pulses|Soyabeans",
  "Millet and Sorghum"                      = "\\bMillet\\s*and\\s*Sorghum\\b",
  "Roots and Tuber"                         = "Roots\\s*and\\s*Tuber",
  "Tea, cocoa, coffee, oilpalm, sugar cane" = "Oilpalm\\s*fruit|oil\\s*palm",
  "Other"                                   = "Fruits\\s*and\\s*vegetables|Other\\s*plant\\s*products|Fibers\\s*etc\\."
)


SCEN_SHOW  <- c("Même diète","Diète probable","Diète saine")
SCEN_TABLE <- c("Même diète","Diète probable","Diète saine")
SCEN_AREA  <- c("Année de base","Même diète","Diète probable","Diète saine")
AREA_ELEMENT  <- "Area"
AREA_CHILDREN <- c("Cropland","Land under perm. meadows and pastures","Agricultural land occupation (Farm)")
PASTURE_ARID_LEVELS <- c("Aride", "Non aride")

# Palette de heatmap (symétrique) pour la table
HEAT_BREAKS <- c(-1.50, -1.00, -0.50, -0.15, 0.15, 0.50, 1.00, 1.50)
HEAT_COLORS <- c(
  "#D07A0B", "#E89A1C", "#F5C269", "#FFFAF0",
  "#FFFFFF",
  "#FFFAF0", "#F5C269", "#E89A1C", "#D07A0B"
) # length = length(breaks)+1

# ---- Part des surfaces (0–1) : seuils et couleurs
# Catégories : <5% ; 5–10% ; 10–15% ; 15–20% ; >20%
SHARE_BREAKS <- c(0.05, 0.10, 0.15, 0.20)

# Même camaïeu que la colonne "Evolution …" (partie positive, du plus clair au plus foncé)
# Avec la palette actuelle (9 couleurs), 5:9 = centre clair -> oranges plus foncés.
SHARE_COLORS <- (function(){
  if (exists("HEAT_COLORS", inherits = TRUE) && length(HEAT_COLORS) >= 9) {
    HEAT_COLORS[5:9]
  } else {
    # Repli robuste si la palette thermique n'est pas chargée
    c("#FFFFFF", "#FFFAF0", "#F5C269", "#E89A1C", "#D07A0B")
  }
})()


# Palette (adaptée daltonisme, orange/bleu)
pasture_arid_palette <- function(){
  c(
    "Aride"     = "#F28E2B",  # orange
    "Non aride" = "#4E79A7"   # bleu
  )
}

# Helper : renvoie un vector de couleurs pour un vecteur de libellés
pasture_arid_colors_for <- function(x){
  pal <- pasture_arid_palette()
  out <- pal[as.character(x)]
  out[is.na(out)] <- "#999999"
  unname(out)
}

# ---- Mapping "items énergie" -> "items tableau"
ENERGY_TO_TABLE_ITEMS <- list(
  "Cereals"               = c("Maize","Wheat","Rice","Other Cereals","Millet and Sorghum"),
  "Dairy"                 = c("Dairy"),
  "Meat, eggs and fish"   = c("Bovine meat"),
  "Oil"                   = c("Oilcrops"),
  "Other"                 = c("Other crops"),
  "Pulses"                = c("Pulses and Soyabeans"),
  "Roots and tubers"      = c("Roots and Tuber"),
  "Sugar"                 = c("Tea, cocoa, coffee, oilpalm, sugar cane"),
  "Vegetables and fruits" = c("Other crops")  
)
  
map_energy_to_table_items <- function(selected_energy, available_table_items){
  if (is.null(selected_energy) || !length(selected_energy)) return(available_table_items)
  out <- unlist(ENERGY_TO_TABLE_ITEMS[names(ENERGY_TO_TABLE_ITEMS) %in% selected_energy], use.names = FALSE)
  if (!length(out)) return(available_table_items)
  intersect(out, available_table_items)
}

# ==== Sankey : palette centralisée ============================================

# Couleurs des NOEUDS du Sankey (on réutilise tes palettes existantes)
sankey_node_palette <- function(){
  c(
    # Sources / réservoirs
    "Production"        = ITEM_COLORS[["Dairy"]]                 %||% "#4E79A7",  # bleu
    "Importations"      = ITEM_COLORS[["Pulses"]]               %||% "#76B7B2",  # teal
    "Exportations"      = HEAT_COLORS[length(HEAT_COLORS)-1L]   %||% "#D07A0B",  # orange foncé
    "Offre intérieure"  = AREA_COLORS[["Cropland"]]             %||% "#F28E2B",  # orange
    
    # Usages (mapping vers ITEM_COLORS pour cohérence visuelle)
    "Food"                  = ITEM_COLORS[["Cereals"]]              %||% "#E15759", # rouge
    "Feed"                  = ITEM_COLORS[["Meat, eggs and fish"]]  %||% "#59A14F", # vert
    "Losses"                = ITEM_COLORS[["Roots and tubers"]]     %||% "#EDC948", # jaune
    "Seed"                  = ITEM_COLORS[["Pulses"]]               %||% "#76B7B2", # teal
    "Other uses (non-food)" = ITEM_COLORS[["Other"]]                %||% "#AF7AA1", # violet
    "Non-attribué"          = ITEM_COLORS[["Vegetables and fruits"]]%||% "#9C755F"  # brun
  )
}

sankey_node_colors_for <- function(labels){
  pal <- sankey_node_palette()
  labs <- as.character(labels)
  out <- pal[labs]
  out[is.na(out)] <- "#999999"
  unname(out)
}

# Convertit un hex en "rgba(r,g,b,a)" (utile pour colorer les liens Plotly)
hex_to_rgba <- function(hex, alpha = 0.35){
  if (is.null(hex) || is.na(hex) || !nzchar(hex)) return(sprintf("rgba(153,153,153,%.2f)", alpha))
  rgb <- grDevices::col2rgb(hex)
  sprintf("rgba(%d,%d,%d,%.2f)", rgb[1], rgb[2], rgb[3], alpha)
}

# Couleur des LIENS = couleur du nœud source avec transparence
sankey_link_colors_from_src <- function(src_labels, alpha = 0.35){
  hex <- sankey_node_colors_for(src_labels)
  vapply(hex, function(h) hex_to_rgba(h, alpha), character(1))
}

# ==== Couleurs des ÉMISSIONS ================================================

EMISSIONS_COLORS <- c(
  "Enteric fermentation"   = "#59A14F",  # vert
  "Manure management"      = "#4E79A7",  # bleu
  "Manure on soils"        = "#76B7B2",  # teal (fourre-tout 'manure applied/left')
  "Synthetic fertilizer"   = "#F28E2B",  # orange
  "Organic fertilizer"     = "#AF7AA1",  # violet
  "Rice cultivation"       = "#EDC948",  # jaune
  "Crop residues"          = "#E15759",  # rouge
  "Energy use"             = "#9C755F",  # brun
  "Other"                  = "#BAB0AC",  # gris
  "Land use change"        = "#8C564B"   # brun foncé (distinctif)
)

emissions_colors_for <- function(items){
  items <- as.character(items)
  cols  <- EMISSIONS_COLORS
  miss  <- setdiff(items, names(cols))
  # Couleur de repli pour tout item non défini explicitement
  if (length(miss)) cols <- c(cols, setNames(scales::hue_pal()(length(miss)), miss))
  cols[items]
}

# ---- Pâturages -> répartition par troupeaux (Area)
LIVESTOCK_LABELS <- c("Beef cattle", "Dairy", "Meat sheep and goats")

LIVESTOCK_COLORS <- c(
  "Beef cattle"          = "#59A14F",  # vert
  "Dairy"                = "#4E79A7",  # bleu
  "Meat sheep and goats" = "#AF7AA1"   # violet
)

livestock_colors_for <- function(x){
  x <- as.character(x)
  pal <- LIVESTOCK_COLORS
  miss <- setdiff(x, names(pal))
  if (length(miss)) pal <- c(pal, setNames(scales::hue_pal()(length(miss)), miss))
  unname(pal[x])
}

