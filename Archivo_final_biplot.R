# -------------------------------------------------------
# Cargar librerías
# -------------------------------------------------------
library(Rtsne)
library(ggplot2)
library(dplyr)
library(readxl)
library(compositions)
library(jsonlite)
library(alphahull)  # Se añade alphahull para Convex Hull
library(cluster)

# -------------------------------------------------------
# Importación y preparación de datos
# -------------------------------------------------------
Anios = read_excel("C:/Users/DELL/OneDrive - Escuela Superior Politécnica del Litoral/Escritorio/Ayudantias/Estadística Multivariante/matrices_biplot.xlsx", sheet = "Year")
Revistas = read_excel("C:/Users/DELL/OneDrive - Escuela Superior Politécnica del Litoral/Escritorio/Ayudantias/Estadística Multivariante/matrices_biplot.xlsx", sheet = "Source")
Paises = read_excel("C:/Users/DELL/OneDrive - Escuela Superior Politécnica del Litoral/Escritorio/Ayudantias/Estadística Multivariante/matrices_biplot.xlsx", sheet = "Countries")

colnames(Anios)[1] = "Año"

Anios_subset = Anios[, c("Año", paste0("t_", 1:14))]
Revistas_subset = Revistas[, c("Source", paste0("t_", 1:14))]
Paises_subset = Paises[, c("Countries", paste0("t_", 1:14))]

Revistas_subset = head(Revistas_subset, n = 29)
Paises_subset = head(Paises_subset, n = 29)

colnames(Anios_subset)[-1] = paste0("Anios_", colnames(Anios_subset)[-1])
colnames(Revistas_subset)[-1] = paste0("Revistas_", colnames(Revistas_subset)[-1])
colnames(Paises_subset)[-1] = paste0("Paises_", colnames(Paises_subset)[-1])

# -------------------------------------------------------
# Transformación CLR y escalado
# -------------------------------------------------------
Anios_clr = scale(clr(Anios_subset[, -1]))
Paises_clr = scale(clr(Paises_subset[, -1]))
Revistas_clr = scale(clr(Revistas_subset[, -1]))

Anios_clr = cbind(Anios_subset[, 1], Anios_clr) %>% rename(Año = 1)
Paises_clr = cbind(Paises_subset[, 1], Paises_clr) %>% rename(Pais = 1)
Revistas_clr = cbind(Revistas_subset[, 1], Revistas_clr) %>% rename(Revista = 1)

# -------------------------------------------------------
# Unificación de datos
# -------------------------------------------------------
unified_data = data.frame(matrix(nrow = 29, ncol = 14))
colnames(unified_data) = paste0("t_", 1:14)

for (i in 1:14) {
  unified_data[, i] = rowMeans(cbind(
    Anios_clr[, i+1],
    Paises_clr[, i+1],
    Revistas_clr[, i+1]
  ))
}

# -------------------------------------------------------
# Análisis t-SNE y clustering
# -------------------------------------------------------
set.seed(123)
tsne_result = Rtsne(unified_data, theta = 0, pca = FALSE, perplexity = 8)
coords_tsne = as.data.frame(tsne_result$Y) %>% rename(Dim1 = V1, Dim2 = V2)

k_optimo = 3
set.seed(123)
kmeans_tsne = kmeans(coords_tsne, centers = k_optimo, nstart = 25)
coords_tsne$Cluster = as.factor(kmeans_tsne$cluster)

tsne_with_info = coords_tsne %>% 
  cbind(
    Anios_clr[, "Año", drop = FALSE],
    Revistas_clr[, "Revista", drop = FALSE],
    Paises_clr[, "Pais", drop = FALSE]
  )

# -------------------------------------------------------
# t-SNE para variables/tópicos
# -------------------------------------------------------
tsne_vars_result = Rtsne(t(unified_data), perplexity = 4, theta = 0)
coords_vars_tsne = as.data.frame(tsne_vars_result$Y) %>% 
  rename(Dim1 = V1, Dim2 = V2) %>% 
  mutate(Variable = colnames(unified_data))

# -------------------------------------------------------
# Cálculo del Convex Hull y generación de JSON
# -------------------------------------------------------
convex_hulls = list()

for (cluster in unique(coords_tsne$Cluster)) {
  puntos_cluster = coords_tsne %>% filter(Cluster == cluster)
  
  if (nrow(puntos_cluster) >= 3) {  
    hull_indices = chull(puntos_cluster$Dim1, puntos_cluster$Dim2)
    convex_hulls[[cluster]] = puntos_cluster[hull_indices, ] 
  }
}

# Convex Hulls a JSON con la estructura correcta
convex_hulls_json = lapply(names(convex_hulls), function(cluster) {
  # Combine Dim1 y Dim2 en un solo array de objetos
  puntos = mapply(function(d1, d2) list(Dim1 = d1, Dim2 = d2),
                  convex_hulls[[cluster]]$Dim1, convex_hulls[[cluster]]$Dim2, SIMPLIFY = FALSE)
  
  list(
    cluster = cluster,
    puntos = puntos  # Ahora es un array de objetos
  )
})

# Guardar Convex Hulls en JSON
write_json(convex_hulls_json, "C:/xampp/htdocs/practicas/convex_hulls.json", pretty = TRUE, auto_unbox = TRUE)

# Guardar visualización principal en JSON
json_data = list(
  observaciones = tsne_with_info %>% 
    mutate(etiqueta = paste(Año, Revista, Pais, sep = ", ")) %>%
    select(Dim1, Dim2, Cluster, etiqueta),
  
  topicos = coords_vars_tsne %>% 
    select(Dim1, Dim2, Variable)
)
write_json(json_data, "C:/xampp/htdocs/practicas/visualizacion.json", pretty = TRUE, auto_unbox = TRUE)

# -------------------------------------------------------
# Método del Codo con coords_tsne
# -------------------------------------------------------
k_values <- 2:10  #  Se probarán entre 2 y 10 clusters
wss <- numeric(length(k_values))  # Vector para almacenar la inercia total (WSS)

for (i in seq_along(k_values)) {
  k <- k_values[i]
  set.seed(123)
  kmeans_model <- kmeans(coords_tsne, centers = k, nstart = 25)
  wss[i] <- kmeans_model$tot.withinss  #  Suma de los errores cuadrados dentro de los clusters
}

# Convertimos los resultados a JSON
elbow_json = lapply(seq_along(k_values), function(i) {
  list(k = k_values[i], wss = wss[i])
})

write_json(elbow_json, "C:/xampp/htdocs/practicas/elbow.json", pretty = TRUE, auto_unbox = TRUE)


