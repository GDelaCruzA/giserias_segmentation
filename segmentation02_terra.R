#prueba de segmentación paquetes actualizados
#
library(terra) |> suppressPackageStartupMessages()
library(OpenImageR) |> suppressPackageStartupMessages()

source(file = "indices_vegetacion_RGB.R")

# Thanks to Matthias Forkel we can use a nice palette for vegetation indices:
rampa <- function(n) {
  .fun <- colorRampPalette(c("chocolate4", "orange", "yellow", "grey", "green", "green3", "darkgreen"))
  col <- .fun(n)
  return(col)
}

# se lee la imagen
# r = rast("~/giserias/conchalito-vuelo2/RPubs_crops.jpg")
r = rast("~/giserias/conchalito-vuelo2/conchal_10.tif")

# Se definen las bandas que se interpretarían como RGB y se visualiza
RGB(r) <- 1:3
# la función plot de r sin la función RGB() despliega todas las bandas en una sola imagen
plot(r) 

# se calculan los índices para la imagen
r_egvi <- EGVI(r)
plot(r_egvi, main = "ExG Vegetation Index sin rampa")
# se guarda la imagen del índice como tif
writeRaster(r_egvi, filename = "conchal_egvi.tif")

# se definen los intervalos y colores para la imagen
cortes <- seq(-0.35, 1, by=0.1)
cols <- rampa(length(cortes)-1)

plot(r_egvi, col = cols, breaks=cortes, main = "ExG Vegetation Index con rampa")

r_tcvi <- TCVI(r)
plot(r_tcvi, col = cols, breaks=cortes, main = "ExG Vegetation Index con rampa")

# probar con los otros índices y rampas de color

## Segmentación de imágenes

r_egvi_mat <- 