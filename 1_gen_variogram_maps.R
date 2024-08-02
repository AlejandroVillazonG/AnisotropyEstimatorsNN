library(reticulate)
library(gstat)
library(sp)

np <- import("numpy")
ruta_al_archivo_npy <- ".../X_train.npy" # X_test.npy # fields_sst.npy
datos_python <- np$load(ruta_al_archivo_npy)

N <- dim(datos_python)[1] # cantidad de simulaciones
size_field <- dim(datos_python)[2] # largo/ancho campo

cutoff <- size_field * sqrt(2) / 3 # valor por defecto en variogram()

maps_vario <- array(NA, dim = c(N, 13, 13))
field_xyz <- expand.grid(x = 1:16, y = 1:16)

time <- system.time(
for (i in 1:N){
  Z <- datos_python[i,,]
  field_xyz$Z <- rev(c(apply(Z, 1, rev))) # Vectorizamos en orden correcto
  field <- SpatialPointsDataFrame(coords = field_xyz[, c("x", "y")],
                                  data = field_xyz)
  variograma <- variogram(Z~1, data=field, cutoff=cutoff, width=cutoff*2/12.8, map=TRUE)
  maps_vario[i,,] <- matrix(variograma$map$var1, nrow=13, byrow=TRUE) # Guardamos el mapa como matriz
}
)
print(time) 
# TRAIN SET
# user  system elapsed 
# 3546.28    3.00 3605.16 

# TEST SET
# user  system elapsed 
# 920.10    0.61  923.08

# REAL DATA
# user  system elapsed 
# 157.66    0.25  159.84 

maps_vario <- np$array(maps_vario)
np$save('.../X_train_vario', maps_vario) # X_test_vario # fields_sst_vario