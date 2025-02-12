# Cargar librerías necesarias
library(Rtsne)
library(ggplot2)
library(dplyr)
library(readxl)
library(jsonlite)

# Lectura de las bases de datos
Anios = read_excel("C:/Users/DELL/OneDrive - Escuela Superior Politécnica del Litoral/Escritorio/Ayudantias/Estadística Multivariante/matrices_biplot.xlsx", sheet = "Year")
Revistas = read_excel("C:/Users/DELL/OneDrive - Escuela Superior Politécnica del Litoral/Escritorio/Ayudantias/Estadística Multivariante/matrices_biplot.xlsx", sheet = "Source")
Paises = read_excel("C:/Users/DELL/OneDrive - Escuela Superior Politécnica del Litoral/Escritorio/Ayudantias/Estadística Multivariante/matrices_biplot.xlsx", sheet = "Countries")

# Renombrar la primera columna de la base de datos 'Anios'
colnames(Anios)[1] = "Año"

# Crear subconjuntos de datos con solo las variables numéricas
Anios_subset = Anios[, c("Año", paste0("t_", 1:14))]
Revistas_subset = Revistas[, c("Source", paste0("t_", 1:14))]
Paises_subset = Paises[, c("Countries", paste0("t_", 1:14))]

# Eliminar la última observación de Revistas y Paises para que coincidan con el número de observaciones de Anios
Revistas_subset = head(Revistas_subset, n = 29)
Paises_subset = head(Paises_subset, n = 29)

# Renombrar las columnas para evitar duplicados
colnames(Anios_subset)[-1] = paste0("Anios_", colnames(Anios_subset)[-1])
colnames(Revistas_subset)[-1] = paste0("Revistas_", colnames(Revistas_subset)[-1])
colnames(Paises_subset)[-1] = paste0("Paises_", colnames(Paises_subset)[-1])

# Combinar las tres bases de datos en una sola, excluyendo las columnas no numéricas
combined_data = cbind(Anios_subset[, -1], Revistas_subset[, -1], Paises_subset[, -1])

# Aplicar la transformación CLR a los datos composicionales
compositional_combined_data = clr(combined_data)

# Aplicación de t-SNE
set.seed(123)
tsne_result = Rtsne(compositional_combined_data, theta = 0, perplexity = 8, pca = FALSE, check_duplicates = FALSE)
coords_tsne = as.data.frame(tsne_result$Y)
colnames(coords_tsne) = c("Dim1", "Dim2")

# Elegir un número óptimo de clusters
k_optimo = 4

# Aplicación de k-means con el número óptimo de clusters
set.seed(123)
kmeans_tsne = kmeans(coords_tsne, centers = k_optimo, nstart = 25)
coords_tsne$Cluster = as.factor(kmeans_tsne$cluster)

# Crear etiquetas para las observaciones
source_labels = c(rep("Año", nrow(Anios_subset)), rep("Revista", nrow(Revistas_subset)), rep("País", nrow(Paises_subset)))
source_labels = source_labels[1:nrow(coords_tsne)]
coords_tsne$Source = factor(source_labels)

# Combinar información adicional
tsne_with_info = cbind(coords_tsne, 
                        Año = Anios_subset[1:29, 1], 
                        Revista = Revistas_subset[1:29, 1], 
                        Pais = Paises_subset[1:29, 1])

# Generar JSON con las coordenadas y etiquetas
output_json_path = "C:/xampp/htdocs/practicas/tsne_data.json"
write_json(tsne_with_info, output_json_path, pretty = TRUE, auto_unbox = TRUE)

# Confirmar generación del JSON
cat("Archivo JSON generado en:", output_json_path, "\n")
