# R/02_utils_palette.R
# =============================================================================
# Palette & helpers centralisés (propres, sans doublons)
# =============================================================================

library(scales)

`%||%` <- function(a, b) {
  if (is.null(a) || length(a) == 0 || (is.numeric(a) && !is.finite(a))) b else a
}

# =============================================================================
# A) Scénarios (ordre + couleurs)
# =============================================================================

# Ordre global recommandé (utilise exactement ces libellés dans les modules)
SCENARIO_LEVELS_DEFAULT <- c("Année de base", "Même diète", "Diète probable", "Diète saine", "Prob-S-limitée")

# Couleurs officielles des scénarios (modifiable en un point unique)
SCENARIO_COLORS <- c(
  "Année de base"  = "#6B7280", # gris (baseline)
  "Même diète"     = "#0EA5E9", # bleu
  "Diète probable" = "#22C55E", # vert
  "Diète saine"    = "#F59E0B", # ambre
  "Prob-S-limitée" = "#EF4444"  # rouge
)

# Renvoie les couleurs dans l'ordre demandé + complète les manquants
scenario_palette <- function(levels = NULL){
  lv <- levels %||% names(SCENARIO_COLORS)
  base <- SCENARIO_COLORS
  miss <- setdiff(lv, names(base))
  if (length(miss)) base <- c(base, setNames(scales::hue_pal()(length(miss)), miss))
  base[lv]
}

# =============================================================================
# B) Familles d’items (énergie/tableau) + couleurs
# =============================================================================

ITEM_COLORS <- c(
  "Cereals"               = "#E15759",
  "Dairy"                 = "#4E79A7",
  "Meat, eggs and fish"   = "#59A14F",
  "Oil"                   = "#F28E2B",
  "Other"                 = "#AF7AA1",
  "Pulses"                = "#76B7B2",
  "Roots and tubers"      = "#EDC948",
  "Sugar"                 = "#FF9DA7",
  "Vegetables and fruits" = "#9C755F",
  "total"                 = "#EDC948"
)
item_colors_for <- function(items){
  items <- as.character(items)
  cols <- ITEM_COLORS
  miss <- setdiff(items, names(cols))
  if (length(miss)) cols <- c(cols, setNames(scales::hue_pal()(length(miss)), miss))
  cols[items]
}

# =============================================================================
# C) Surfaces / occupations
# =============================================================================

AREA_COLORS <- c(
  "Cropland"                              = "#F28E2B",
  "Land under perm. meadows and pastures" = "#59A14F",
  "Agricultural land occupation (Farm)"   = "#76B7B2",
  "Forest land"                           = "#8CD17D"
)
area_colors_for <- function(items){
  items <- as.character(items)
  cols  <- AREA_COLORS
  miss  <- setdiff(items, names(cols))
  if (length(miss)) cols <- c(cols, setNames(scales::hue_pal()(length(miss)), miss))
  cols[items]
}

# =============================================================================
# D) Palettes par produit (crops) et élevage (livestock)
# =============================================================================

pal_crops <- function(items){
  base <- c(
    "Wheat"                          = "#F47560",
    "Other Cereals"                  = "#E8C547",
    "Other"                          = "#A1BF65",
    "Tea, cocoa, coffee, oilpalm, sugar cane" = "#2CA25F",
    "Roots and Tuber"                = "#2C7FB8",
    "Pulses and Soyabeans"           = "#4C78A8",
    "Oilcrops"                       = "#8E6C8A",
    "Millet and Sorghum"             = "#E45756"
  )
  out <- base[items]
  if (any(is.na(out))) out[is.na(out)] <- scales::hue_pal()(sum(is.na(out)))
  unname(out)
}

pal_livestock <- function(items){
  base <- c(
    "Dairy"                = ITEM_COLORS[["Dairy"]]                 %||% "#4E79A7",
    "Beef cattle"          = ITEM_COLORS[["Meat, eggs and fish"]]  %||% "#59A14F",
    "Meat sheep and goats" = "#E8C547"
  )
  out <- base[items]
  if (any(is.na(out))) out[is.na(out)] <- scales::hue_pal()(sum(is.na(out)))
  unname(out)
}

# =============================================================================
# E) Sankey — palette nœuds + helpers
# =============================================================================

sankey_node_palette <- function(){
  c(
    # Sources / reservoirs
    "Production"        = ITEM_COLORS[["Dairy"]]                 %||% "#4E79A7",
    "Imports"           = ITEM_COLORS[["Pulses"]]                %||% "#76B7B2",
    "Exports"           = "#D07A0B",
    "Domestic supply"   = AREA_COLORS[["Cropland"]]              %||% "#F28E2B",
    
    # Uses
    "Food"              = ITEM_COLORS[["Cereals"]]               %||% "#E15759",
    "Feed"              = ITEM_COLORS[["Meat, eggs and fish"]]   %||% "#59A14F",
    "Losses"            = ITEM_COLORS[["Roots and tubers"]]      %||% "#EDC948",
    "Seed"              = ITEM_COLORS[["Pulses"]]                %||% "#76B7B2",
    "Other uses (non-food)" = ITEM_COLORS[["Other"]]             %||% "#AF7AA1",
    
    # Technique / fallback (si jamais utilisé)
    "Unallocated"       = ITEM_COLORS[["Vegetables and fruits"]] %||% "#9C755F"
  )
}

sankey_node_colors_for <- function(labels){
  pal  <- sankey_node_palette()
  labs <- as.character(labels)
  out  <- pal[labs]
  out[is.na(out) | !nzchar(out)] <- "#CCCCCC"
  unname(out)
}

hex_to_rgba <- function(hex, alpha = 0.35){
  if (is.null(hex) || is.na(hex) || !nzchar(hex)) {
    return(sprintf("rgba(153,153,153,%.2f)", alpha))
  }
  rgb <- grDevices::col2rgb(hex)
  sprintf("rgba(%d,%d,%d,%.2f)", rgb[1], rgb[2], rgb[3], alpha)
}

sankey_link_colors_from_src <- function(src_labels, alpha = 0.35){
  hex <- sankey_node_colors_for(src_labels)
  vapply(hex, function(h) hex_to_rgba(h, alpha), character(1))
}


# =============================================================================
# F) Émissions (inventaire GHG)
# =============================================================================

EMISSIONS_COLORS <- c(
  "Enteric fermentation"   = "#59A14F",
  "Manure management"      = "#4E79A7",
  "Manure on soils"        = "#76B7B2",
  "Synthetic fertilizer"   = "#F28E2B",
  "Organic fertilizer"     = "#AF7AA1",
  "Rice cultivation"       = "#EDC948",
  "Crop residues"          = "#E15759",
  "Energy use"             = "#9C755F",
  "Other"                  = "#BAB0AC",
  "Land use change"        = "#8C564B"
)
emissions_colors_for <- function(items){
  items <- as.character(items)
  cols  <- EMISSIONS_COLORS
  miss  <- setdiff(items, names(cols))
  if (length(miss)) cols <- c(cols, setNames(scales::hue_pal()(length(miss)), miss))
  cols[items]
}

# =============================================================================
# G) Pâturages (aride / non-aride)
# =============================================================================

pasture_arid_palette <- function(){
  c("Aride" = "#F28E2B", "Non aride" = "#4E79A7")
}
pasture_arid_colors_for <- function(x){
  pal <- pasture_arid_palette()
  out <- pal[as.character(x)]
  out[is.na(out)] <- "#999999"
  unname(out)
}

# =============================================================================
# H) Années (ex. comparaisons 2018 vs 2050)
# =============================================================================

# Définition UNIQUE. Si tu en as besoin ailleurs, utilise celle-ci.
PALETTE_YEARS <- c("2018" = "#EDC948", "2050" = "#E15759")

# =============================================================================
# I) Mappings & constantes utiles à l’app
# =============================================================================

ITEM_ORDER_TBL <- c(
  "Maize","Wheat","Rice","Other Cereals","Oilcrops",
  "Pulses and Soyabeans","Millet and Sorghum","Roots and Tuber",
  "Tea, cocoa, coffee, oilpalm, sugar cane","Other crops"
)

NO_IMPUTE_ITEMS <- c("Tea, cocoa, coffee, oilpalm, sugar cane", "Other crops")
ANIMAL_ITEMS    <- c("Dairy", "Bovine meat")

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





