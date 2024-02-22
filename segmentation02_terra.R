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
rast_conchal = rast("~/giserias/conchalito-vuelo2/conchal_10.tif")

# Se definen las bandas que se interpretarían como RGB y se visualiza
RGB(rast_conchal) <- 1:3
# la función plot de rast_conchal sin la función RGB() despliega todas las bandas en una sola imagen
plot(rast_conchal) 

# se calculan los índices para la imagen
rast_conchal_egvi <- EGVI(rast_conchal)
plot(rast_conchal_egvi, main = "ExG Vegetation Index sin rampa")
# se guarda la imagen del índice como tif
writeRaster(rast_conchal_egvi, filename = "conchal_egvi.tif")

# se definen los intervalos y colores para la imagen
cortes <- seq(-0.35, 1, by=0.1)
cols <- rampa(length(cortes)-1)

plot(rast_conchal_egvi, col = cols, breaks=cortes, main = "ExG Vegetation Index con rampa")

rast_conchal_tcvi <- TCVI(rast_conchal)
plot(rast_conchal_tcvi, col = cols, breaks=cortes, main = "ExG Vegetation Index con rampa")

# probar con los otros índices y rampas de color
# ver si esta transformación a grises se puede usar
library(imager)
z = load.image("Slide3.JPG")
z = grayscale(z)
y = resize(z, size_x = 600, size_y = 400)
save.image(im = y, "img3.JPG")
# revisar también {imager}::grayscale y/o {SpatialPack}::RGB2gray

## Segmentación de imágenes

