# funciones con los índices de vegetación con base RGB
# G. De la Cruz A. @ 20240213
# 
# Jiang, j. et al. 2019. Using Digital Cameras on an Unmanned Aerial Vehicle to
# Derive Optimum Color Vegetation Indices for Leaf Nitrogen Concentration
# Monitoring in Winter Wheat. Remote Sensing, 11, 2667; doi:10.3390/rs11222667
# seis índices ya implementados: EGVI, KI, NGRDI, RGRI, TCVI, VARI
# 
# otros índice en:
# Michez, A. et al. 2016. Classification of riparian forest species and health 
# condition using multi-temporal and hyperspatial imagery from unmanned aerial
# system. Environment Monitoring and Assessment, 188, 146; 
# doi:10.1007/s10661-015-4996-2
# incluye NGRDI, KI, más BRIGHT, NGBI, GR, GB, RB, NORR, NORG, NORB 
# 
# ajustar todas las funciones para responder al tipo de datos de entrada:
# class()[1] = array o (SpatRaster, RasterStack) para tomar las tres bandas del caso
# 
# Poner todo en una misma función...IndVeg(img = "imagen", indice = c(NGRDI, KI, RGRI, 
# EGVI, TCVI), r = 1, g = 2, b = 3, ajuste = 0.4)
# 
# Normalized green red difference index
NGRDI <- function(img, r = 1, g = 2, b = 3) {
  rband <- img[[r]]
  gband <- img[[g]]
  bband <- img[[b]]
  indice <- (gband-rband) / (gband + rband)
  return(indice)
}

# Kawashima index
KI <- function(img, r = 1, g = 2, b = 3) {
  rband <- img[[r]]
  gband <- img[[g]]
  bband <- img[[b]]
  indice <- (rband-bband) / (rband + bband)
  return(indice)
}

# Red green ratio index
RGRI <- function(img, r = 1, g = 2, b = 3) {
  rband <- img[[r]]
  gband <- img[[g]]
  bband <- img[[b]]
  indice <- rband / gband
  return(indice)
}

# Visible atmospherically resistance index
VARI <- function(img, r = 1, g = 2, b = 3) {
  rband <- img[[r]]
  gband <- img[[g]]
  bband <- img[[b]]
  indice <- (gband - rband) / (gband + rband - bband)
  return(indice)
}

# Excess green vegetation index
EGVI <- function(img, r = 1, g = 2, b = 3) {
  rband <- img[[r]]
  gband <- img[[g]]
  bband <- img[[b]]
  indice <- (2 * gband - rband - bband) / (gband + rband + bband)
  return(indice)
}

# True Color Vegetation Index
TCVI <- function(img, r = 1, g = 2, b = 3, ajuste = 0.4) {
  rband <- img[[r]]
  gband <- img[[g]]
  bband <- img[[b]]
  indice <- 1.4 * (2 * rband - 2 * bband) / (2 * rband - gband - 2 * bband + 255 * ajuste)
  return(indice)
}

# Brightness
BRIGHT <- function(img, r = 1, g = 2, b = 3) {
  rband <- img[[r]]
  gband <- img[[g]]
  bband <- img[[b]]
  indice <- (rband + gband + bband) / 3
  return(indice)
}

# Normalized Green Blue Index
NGBI <- function(img, r = 1, g = 2, b = 3) {
  rband <- img[[r]]
  gband <- img[[g]]
  bband <- img[[b]]
  indice <- (gband - bband) / (gband + bband)
  return(indice)
}

# G/R Band Ratio
GRBR <- function(img, r = 1, g = 2, b = 3) {
  rband <- img[[r]]
  gband <- img[[g]]
  bband <- img[[b]]
  indice <- gband / rband
  return(indice)
}

# G/B Band Ratio
GBBR <- function(img, r = 1, g = 2, b = 3) {
  rband <- img[[r]]
  gband <- img[[g]]
  bband <- img[[b]]
  indice <- gband / bband
  return(indice)
}

# R/B Band Ratio
RBBR <- function(img, r = 1, g = 2, b = 3) {
  rband <- img[[r]]
  gband <- img[[g]]
  bband <- img[[b]]
  indice <- rband / bband
  return(indice)
}

# Normalized Red
NORR <- function(img, r = 1, g = 2, b = 3) {
  rband <- img[[r]]
  gband <- img[[g]]
  bband <- img[[b]]
  indice <- rband / (rband + gband + bband)
  return(indice)
}

# Normalized Green
NORG <- function(img, r = 1, g = 2, b = 3) {
  rband <- img[[r]]
  gband <- img[[g]]
  bband <- img[[b]]
  indice <- gband / (rband + gband + bband)
  return(indice)
}

# Normalized Blue
NORB <- function(img, r = 1, g = 2, b = 3) {
  rband <- img[[r]]
  gband <- img[[g]]
  bband <- img[[b]]
  indice <- bband / (rband + gband + bband)
  return(indice)
}

# normaliza cada una de las bandas RGB de una imagen
NormalizaColor <- function(img, r = 1, g = 2, b = 3){
  rband <- img[[r]]
  gband <- img[[g]]
  bband <- img[[b]]
  img[[r]] <- rband/(rband + gband + bband)
  img[[g]] <- gband/(rband + gband + bband)
  img[[b]] <- bband/(rband + gband + bband)
  return(img)
}

