#prueba de segmentación paquetes actualizados
#
library(terra) |> suppressPackageStartupMessages()
library(OpenImageR) |> suppressPackageStartupMessages()
library(raster) |> suppressPackageStartupMessages()
library (RImagePalette) |> suppressPackageStartupMessages()
library (rasterVis) |> suppressPackageStartupMessages()


source(file = "indices_vegetacion_RGB.R")

# Thanks to Matthias Forkel we can use a nice palette for vegetation indices:
rampa <- function(n) {
  .fun <- colorRampPalette(c("chocolate4", "orange", "yellow", "grey", "green", "green3", "darkgreen"))
  col <- .fun(n)
  return(col)
}

# Identify a measure of central tendency of each superpixel escrita por Richard Plan
make.segments <- function(x, ftn){
  # The argument ftn is any functional measure of central tendency
  z <- x
  # For each identified superpixel, compute measure of central tendency
  for (k in unique(as.vector(x$labels))){
    # Identify members of the superpixel having the given label
    in.super <- matrix(0, nrow(x$label), ncol(x$label))
    for (i in 1:nrow(x$label))
      for (j in 1:ncol(x$label))
        if (x$label[i,j] == k)
          in.super[i,j] <- 1
    #Identify the boundary cells as having all values 0
    on.bound <- matrix(0, nrow(x$label), ncol(x$label))
    for (i in 1:nrow(x$label))
      for (j in 1:ncol(x$label))
        if (in.super[i,j] == 1){
          if (x$slic_data[i,j,1] == 0 & x$slic_data[i,j,2] == 0 
              & x$slic_data[i,j,3] == 0)
            on.bound[i,j] <- 1
        }
    #Identify the superpixel cells not on the boundary
    sup.data <- matrix(0, nrow(x$label), ncol(x$label))
    for (i in 1:nrow(x$label))
      for (j in 1:ncol(x$label))
        if (in.super[i,j] == 1 & on.bound[i,j] == 0)
          sup.data[i,j] <- 1
    # Compute the measure of central tendency of the cells in R, G, B
    for (n in 1:3){
      # Create a matrix M of the same size as the matrix of superpixel values
      M <- matrix(0, dim(x$slic_data)[1], dim(x$slic_data)[2]) 
      for (i in 1:nrow(x$label))
        for (j in 1:ncol(x$label))
          # Assign to M the values in the superpixel
          if (sup.data[i,j] == 1) M[i,j] <- x$slic_data[i,j,n]
      if (length(M[which(M > 0 & M < 255)]) > 0)
        # Compute the measure of central tendency
        ftn.n <- round(ftn(M[which(M > 0 & M < 255)]), 0)
      else
        ftn.n <- 0
      for (i in 1:nrow(x$label))
        for (j in 1:ncol(x$label))
          if (in.super[i,j] == 1) z$slic_data[i,j,n] <- ftn.n
    }
  }
  return(z)
}

# definir la imagen a trabajar
imagen <- "~/giserias/conchalito-vuelo2/conchal_10Q.tif"
# imagen <- "~/giserias/conchalito-vuelo2/RPubs_crops.jpg"

# se lee la imagen
# r = rast("~/giserias/conchalito-vuelo2/RPubs_crops.jpg")
# warning = FALSE
spatrast_img <- rast(imagen) # la carga como SpatRaster
array_img <- readImage(imagen) # la carga como array 

# Se definen las bandas que se interpretarían como RGB y se visualiza
RGB(spatrast_img) <- 1:3
# la función plot de spatrast_img sin la función RGB() despliega todas las bandas en una sola imagen
# a partir del tif original (conchal_10.tif)
plot(spatrast_img)
imageShow(array_img)

# se calculan los índices para la imagen, se obtienen los valores crudos
spatrast_img_egvi <- EGVI(spatrast_img)
plot(spatrast_img_egvi, main = "EG Vegetation Index sin rampa") # grafica cruda

# se guarda la imagen del índice como tif
# writeRaster(rast_img_egvi, filename = "conchal_egvi.tif")

# se definen los intervalos y colores para la imagen
cortes <- seq(-0.35, 1, by=0.1)
cols <- rampa(length(cortes)-1)

plot(spatrast_img_egvi, col = cols, breaks=cortes, main = "EG Vegetation Index con rampa")

# otro índice
spatrast_img_tcvi <- TCVI(spatrast_img)
plot(spatrast_img_tcvi, col = cols, breaks=cortes, main = "TC Vegetation Index con rampa")

# probar con los otros índices y rampas de color
# ver si esta transformación a grises se puede usar
# library(imager)
# z = load.image("Slide3.JPG")
# z = grayscale(z)
# y = resize(z, size_x = 600, size_y = 400)
# save.image(im = y, "img3.JPG")
# revisar también {imager}::grayscale y/o {SpatialPack}::RGB2gray

## Segmentación de imágenes

# se tiene que convertir de SpatRaster (los VI) a RasterLayer
rasterlayer_egvi <- raster(spatrast_img_egvi)
plot(rasterlayer_egvi, col = cols, breaks=cortes, main = "del SpatRaster Vegetation Index con rampa")

egvi.mat <- matrix(rasterlayer_egvi@data@values,
                   nrow = rasterlayer_egvi@nrows,
                   ncol = rasterlayer_egvi@ncols, byrow = TRUE)

min(egvi.mat, na.rm = TRUE)
max(egvi.mat, na.rm = TRUE)

# como en los índices tenemos valores negativos, tenemos que normalizar valores a 0 - 1
egvi.mat.norm <- NormalizeObject(egvi.mat)
min(egvi.mat.norm, na.rm = TRUE)
max(egvi.mat.norm, na.rm = TRUE)
# como la matriz tiene NaN, los sustituimos por 1 (blanco)
egvi.mat.norm[is.na(egvi.mat.norm)] = 1
imageShow(egvi.mat.norm) # representación en tonos de grises del índice, una banda

# regresamos los valores normalizados al array para hacer la segmentación; las tres bandas
# tendrán el mismo valor del índice de vegetación
egvi.data <- array_img # la imagen RGB original o... la capa del índice repetida 
egvi.data[,,1] <- egvi.mat.norm # para obtener la clasificación con base en el VI
egvi.data[,,2] <- egvi.mat.norm
egvi.data[,,3] <- egvi.mat.norm

# segmentamos la imagen original o la del índice de vegetación para iniciar 
# la clasificación; la imagen en formato de array con tres bandas
egvi.superpx = superpixels(input_image = egvi.data,
                      method = "slic", 
                      superpixel = 200,
                      compactness = 20, 
                      return_slic_data = TRUE,
                      return_labels = TRUE, 
                      write_slic = "",
                      verbose = TRUE)

# vemos como queda
imageShow(egvi.superpx$slic_data)

# vemos las clases definidas por la segmentación; los parámteros son el número de 
# superpixels y lo compacto de los mismos
#
sort(unique(as.vector(egvi.superpx$labels))) # por sus etiquetas

# los homogeneizamos por la media de los pixels en cada segmento; 
# esta es la función más larga y tardada
egvi.means <- make.segments(egvi.superpx, mean)
imageShow(egvi.means$slic_data)

# para agrupar los segmentos más afines, los acomodamos en una matriz
mat_km <- cbind(as.vector(egvi.means$slic_data[,, 1]), 
                as.vector(egvi.means$slic_data[,, 2]),
                as.vector(egvi.means$slic_data[,, 3]))

# Luego los segmentos se aglomeran por su semejanza en la media para 
# formar un número menor de clases que las definidas para los superpixels
set.seed(123)
# OJO: para la función kmeans se establece el número de clases subsiguientes
nclass <- 15
# kmeans se calcula sobre la matriz de los valores de las tres bandas en 
# lugar de solo una como en el original, de tal manera que podemos procesar
# las tres bandas de otros resultados; i.e. superpixels
# egvi.clus <-  kmeans(as.vector(egvi.means$slic_data[,,1]), nclass)

egvi.clus <-  kmeans(mat_km, nclass)
# el resultado de k-means (los grupos y que superpixels pertenecen a cada uno) lo
# acomodamos como una matriz (de nuevo)
vege.class <- matrix(egvi.clus$cluster,
                     nrow = rasterlayer_egvi@nrows, 
                     ncol = rasterlayer_egvi@ncols, byrow = FALSE)
# con las clases se construye la imagen raster resultante
class.ras <- raster(vege.class, 
                    xmn = 0,
                    xmx = ncol(array_img),
                    ymn = 0,
                    ymx = nrow(array_img),
                    crs = "")

# cada valor en la imagen se codifica como factores
class.ras <- ratify(class.ras)
rat.class <- levels(class.ras)[[1]] # se asignan sus niveles

# eventualmente, se asignan etiquetas para cada una de las clases del resultado;
rat.class$landcover <- paste("Clase", nclass:1)

# extraemos la paleta de colores de la imagen, en el original, pero no es práctico
# img.pal <- image_palette(array_img, n=nclass, volume = TRUE)
# se usa la rampa de color con el número de clases
img.pal <- rampa(nclass)
# la desplegamos
levels(class.ras) <- rat.class
levelplot(class.ras, margin=FALSE,
          col.regions= img.pal[1:nclass],
          # se pueden reasignar los colores así como agrupar asignando el mismo a diferentes clases
          # col.regions= crops.pal[c(5,7,9,4,8,10,2,4,1,6)],
          main = "Clases de cobertura en la imagen")

# luego las regiones hay que convertirlas a polígonos...
# ver: {raster}::rasterToPolygons y {terra}::as.polygons

# vía {raster}; se tiene que definir la acción para cada clase kmeans
poly.raster <- rasterToPolygons(class.ras, fun = function(x){x == 5})
plot(poly.raster, col = "red")

# en {terra} la función rast() convierte un RasterLayer a (class.ras) a SpatRaster
# que se requiere para la función as.polygons()
# 
spatraster_class.ras <- rast(class.ras)
poly.spatraster <- as.polygons(spatraster_class.ras)
plot(poly.spatraster)

# revisar como guardar los rasters como tif y los vectoriales como shp
# 