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
# otros en:
# Tran, T et al. 2022. A review of spectral indices for mangrove remote sensing.
# Remote Sensing, 14: 4868, doi:10.3390/rs14194868
# incluye: NGRDI2, GLI, ExG, ExR, NegExR, CIVE, VEGI, ExGlessExR, TGI, CI, VDVI
# 
# ajustar todas las funciones para responder al tipo de datos de entrada:
# class()[1] = array o (SpatRaster, RasterStack) para tomar las tres bandas del caso
# 
# Poner todo en una misma función...IndVeg(img = "imagen", indice = c(NGRDI, KI, RGRI, 
# EGVI, TCVI), r = 1, g = 2, b = 3, ajuste = 0.4)
# 
# 1 Normalized green red difference index
NGRDI <- function(img, r = 1, g = 2, b = 3) {
  rband <- img[[r]]
  gband <- img[[g]]
  bband <- img[[b]]
  indice <- (gband - rband) / (gband + rband)
  return(indice)
}

# 2 Normalized green red difference index 2
NGRDI2 <- function(img, r = 1, g = 2, b = 3) {
  rband <- img[[r]]
  gband <- img[[g]]
  bband <- img[[b]]
  indice <- ((gband - rband) / (gband + rband)) + 0.08
  return(indice)
}

# 3 Kawashima index o NRBI
KI <- function(img, r = 1, g = 2, b = 3) {
  rband <- img[[r]]
  gband <- img[[g]]
  bband <- img[[b]]
  indice <- (rband - bband) / (rband + bband)
  return(indice)
}

# 4 Red green ratio index
RGRI <- function(img, r = 1, g = 2, b = 3) {
  rband <- img[[r]]
  gband <- img[[g]]
  bband <- img[[b]]
  indice <- rband / gband
  return(indice)
}

# 5 Visible atmospherically resistance index
VARI <- function(img, r = 1, g = 2, b = 3) {
  rband <- img[[r]]
  gband <- img[[g]]
  bband <- img[[b]]
  indice <- (gband - rband) / (gband + rband - bband)
  return(indice)
}

# 6 Excess green vegetation index
EGVI <- function(img, r = 1, g = 2, b = 3) {
  rband <- img[[r]]
  gband <- img[[g]]
  bband <- img[[b]]
  indice <- (2 * gband - rband - bband) / (gband + rband + bband)
  return(indice)
}

# 7 True Color Vegetation Index
TCVI <- function(img, r = 1, g = 2, b = 3, ajuste = 0.4) {
  rband <- img[[r]]
  gband <- img[[g]]
  bband <- img[[b]]
  indice <- 1.4 * (2 * rband - 2 * bband) / (2 * rband - gband - 2 * bband + 255 * ajuste)
  return(indice)
}

# 8 Brightness
BRIGHT <- function(img, r = 1, g = 2, b = 3) {
  rband <- img[[r]]
  gband <- img[[g]]
  bband <- img[[b]]
  indice <- (rband + gband + bband) / 3
  return(indice)
}

# 9 Normalized Green Blue Index
NGBI <- function(img, r = 1, g = 2, b = 3) {
  rband <- img[[r]]
  gband <- img[[g]]
  bband <- img[[b]]
  indice <- (gband - bband) / (gband + bband)
  return(indice)
}

# 10 G/R Band Ratio
GRBR <- function(img, r = 1, g = 2, b = 3) {
  rband <- img[[r]]
  gband <- img[[g]]
  bband <- img[[b]]
  indice <- gband / rband
  return(indice)
}

# 11 G/B Band Ratio
GBBR <- function(img, r = 1, g = 2, b = 3) {
  rband <- img[[r]]
  gband <- img[[g]]
  bband <- img[[b]]
  indice <- gband / bband
  return(indice)
}

# 12 R/B Band Ratio
RBBR <- function(img, r = 1, g = 2, b = 3) {
  rband <- img[[r]]
  gband <- img[[g]]
  bband <- img[[b]]
  indice <- rband / bband
  return(indice)
}

# 13 Normalized Red
NORR <- function(img, r = 1, g = 2, b = 3) {
  rband <- img[[r]]
  gband <- img[[g]]
  bband <- img[[b]]
  indice <- rband / (rband + gband + bband)
  return(indice)
}

# 14 Normalized Green
NORG <- function(img, r = 1, g = 2, b = 3) {
  rband <- img[[r]]
  gband <- img[[g]]
  bband <- img[[b]]
  indice <- gband / (rband + gband + bband)
  return(indice)
}

# 15 Normalized Blue
NORB <- function(img, r = 1, g = 2, b = 3) {
  rband <- img[[r]]
  gband <- img[[g]]
  bband <- img[[b]]
  indice <- bband / (rband + gband + bband)
  return(indice)
}

# 16 normaliza cada una de las bandas RGB de una imagen
NormalizaColor <- function(img, r = 1, g = 2, b = 3){
  rband <- img[[r]]
  gband <- img[[g]]
  bband <- img[[b]]
  img[[r]] <- rband/(rband + gband + bband)
  img[[g]] <- gband/(rband + gband + bband)
  img[[b]] <- bband/(rband + gband + bband)
  return(img)
}

# 17 Green leaf index
GLI <- function(img, r = 1, g = 2, b = 3){
  rband <- img[[r]]
  gband <- img[[g]]
  bband <- img[[b]]
  indice <- (((gband - rband) + (gband - bband)) / ((gband + rband) + (gband + bband))) + 0.07
  return(indice)
}

# 18 Excess green vegetation index
ExG <- function(img, r = 1, g = 2, b = 3){
  rband <- img[[r]]
  gband <- img[[g]]
  bband <- img[[b]]
  indice <- 2 * gband - rband - bband + 50
  return(indice)
}

# 19 Excess red vegetation index
ExR <- function(img, r = 1, g = 2, b = 3){
  rband <- img[[r]]
  gband <- img[[g]]
  bband <- img[[b]]
  indice <- 2 * rband - gband - bband + 50
  return(indice)
}

# 20 Negative excess red vegetation index
NegExR <- function(img, r = 1, g = 2, b = 3){
  rband <- img[[r]]
  gband <- img[[g]]
  bband <- img[[b]]
  indice <- gband - 1.4 * rband
  return(indice)
}

# 21 Colour index of vegetation extraction
CIVE <- function(img, r = 1, g = 2, b = 3){
  rband <- img[[r]]
  gband <- img[[g]]
  bband <- img[[b]]
  indice <- 0.441 * rband - 0.881 * gband + 0.385 * bband + 18.78745
  return(indice)
}

# 22 Vegetative index
VEGI <- function(img, r = 1, g = 2, b = 3){
  rband <- img[[r]]
  gband <- img[[g]]
  bband <- img[[b]]
  indice <- (gband / rband ^ 0.667) * bband ^ 0.333
  return(indice)
}

# 23 Excess green minus excess red index
ExGlessExR <- function(img, r = 1, g = 2, b = 3){
  indice <- ExG(img) - ExR(img)
  return(indice)
}

# 24 Triangular greenness index
TGI <- function(img, r = 1, g = 2, b = 3){
  rband <- img[[r]]
  gband <- img[[g]]
  bband <- img[[b]]
  indice <- gband - 0.39 * rband - 0.61 * bband
  return(indice)
}

# 25 combined index
CI <- function(img, r = 1, g = 2, b = 3){
  indice <- 0.25 * ExG(img) + 0.3 * ExR(img) + 0.33 * CIVE(img) + 0.12 * VEGI(img)
  return(indice)
}

# 26 Visible-band difference vegetation index
VDVI <- function(img, r = 1, g = 2, b = 3){
  rband <- img[[r]]
  gband <- img[[g]]
  bband <- img[[b]]
  indice <- (2 * gband - rband - bband) / (2 * gband + rband + bband)
  return(indice)
}
