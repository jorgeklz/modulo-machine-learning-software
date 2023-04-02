#Librerias
library(tidyverse)
library(tidyr)
library(dplyr)
library(fastDummies)
library(ggplot2)
library(haven)
library(tidyr)
library(GGally)
library(modeest)
library(sjmisc)
library(caret)
library(scales)
library(dplyr) 

library(openai)
#Sys.setenv(JAVA_HOME='C:\\Program Files\\Java\\jre1.8.0_361') # for 64-bit version
#library(rJava) 
#library(entropy)

options(scipen=999)
set.seed(1987)





###########################
# Etapa 2. Adquisición ---------------------------------------------------------------------
###########################

# Leer data ---------------------------------------------------------------

#datos  <-  readxl::read_excel("C:/Users/UTM/Dropbox/Modulo ML/Practicas/tienda.xlsx")

datos_originales  <-  readr::read_delim("Datos/tienda-online.csv", delim = ",")

datos_originales %>% glimpse()



###########################
# Etapa 3. Preprocesamiento ---------------------------------------------------------------------
###########################




###########################
# Eliminar valores ausentes 
########################### 
# Se utiliza la función na.omit para eliminar las filas que contienen valores ausentes en cualquier columna.

datos_limpios <- na.omit(datos_originales)

###########################
# Confirmar tipos de datos
###########################
datos_limpios %>% glimpse

#datos_limpios <- datos_limpios %>% mutate(Edad=as.numeric(Edad))
#datos_limpios <- datos_limpios %>% filter(Edad>17)

# cuantitivas estan ok



# cualitativas categoricas deben convertirse a factor.

datos_limpios <- datos_limpios %>%
  mutate_if(is.character, as.factor)



###########################
# Confirmar rango de datos correctas
###########################

# cualitativas




# Diagrama de columna

# Seleccionar solo las columnas que contienen variables categoricas

df_chr <- datos_limpios  %>% mutate_if(is.character,as.factor) %>%
  dplyr::select(where(is.factor))

datos_largo <- pivot_longer(df_chr, 
                            cols = c(Genero, Nivel.educativo, Producto, Influencer, Pago, Compra), 
                            names_to = "variable", 
                            values_to = "valor")
conteo <- datos_largo %>% count(variable, valor)

conteo %>% ggplot(aes(x = variable, y = n, fill = valor)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Variable", y = "Frecuencia", fill = "Valor") +
  facet_grid(. ~ variable, scales = "free_x")  +
  theme_bw()








# Contar las categorías que existen para todas las variables categóricas

datos_limpios %>%
  select_if(is.factor) %>% count(Genero)

datos_limpios %>%
  select_if(is.factor) %>% count(Nivel.educativo)

# Presenta inconsistencia
datos_limpios %>%
  select_if(is.factor) %>% count(Producto) 
# Corrige inconsistencia 
datos_limpios <- datos_limpios %>% filter(Producto!="-24"| Producto!=-24)
levels(datos_limpios$Producto)[1] <- NA


# verifica solucion
datos_limpios %>%
  select_if(is.factor) %>% count(Producto) 

datos_limpios %>%
  select_if(is.factor) %>% count(Influencer) 

datos_limpios %>%
  select_if(is.factor) %>% count(Pago) 

datos_limpios %>%
  select_if(is.factor) %>% count(Compra) 





# Cuantitativas

# configuramos datos para poder generar histogramas  (ver inconsistencias)
datos_long <- datos_limpios %>% pivot_longer(where(is.numeric), names_to = "variable", values_to = "valor")

# generamos histogramas de todas las variables cuantitativas
ggplot(datos_long, aes(x = valor)) +
  geom_histogram() + 
  labs(title = "Histogramas de variables numéricas", 
       x = "", y = "Frecuencia") +
  facet_wrap(~ variable, scales = "free")



# Presentan valores fuera de rangos: 
# Frecuencia.compra
# Descuento
# Edad
# Rating
# Total.pagar

# Corrige Descuento
datos_limpios <- datos_limpios %>% filter(Frecuencia.compra>0)

# Corrige Descuento
datos_limpios <- datos_limpios %>% filter(Descuento>=0)

# Corrige Edad
datos_limpios <- datos_limpios %>% filter(Edad>0)

# Corrige Rating

datos_limpios <- datos_limpios %>% filter(between(Rating, 0, 5))

# Corrige Total.pagar

datos_limpios <- datos_limpios %>% mutate(Total.pagar=round(((Precio.unidad*Cantidad) * ((100-Descuento)/100)), 2))





# generamos histogramas de todas las variables cuantitativas (verificar correcciones)

datos_long <- datos_limpios %>% pivot_longer(where(is.numeric), names_to = "variable", values_to = "valor")

ggplot(datos_long, aes(x = valor)) +
  geom_histogram() + 
  labs(title = "Histogramas de variables numéricas", 
       x = "", y = "Frecuencia") +
  facet_wrap(~ variable, scales = "free")







###########################
# EDA
###########################


source("Scripts/funciones.R")

# Medidas de tendencia central y dispersión (cuantitativa)

estadisticas <- datos_limpios %>% select_if(is.numeric) %>% 
  summarize_all(list(media = mean, mediana = median, moda=moda, rango=rango, varianza = var, p.desviacion = sd )) %>% 
  pivot_longer(cols = everything(),
               names_to = c("variable", "medida"),
               names_sep = "_",
               values_to = "valor")

medida.central.disper <- spread(estadisticas, key="variable", value="valor" )






# Medidas de posición (cuantitativa)




# Cuartiles 

cuartiles <- datos_limpios %>% 
  summarise(across(where(is.numeric), 
                   ~ quibble(.x, c(0.25, 0.50, 0.75, 1), dropNA = TRUE)), .groups = 'drop') %>% 
  tidyr::unnest(names_repair = 'unique', names_sep = "_") %>% 
  dplyr::select(ends_with("_x")) %>% rotate_df() %>% rename(Q1=V1, Q2=V2, Q3=V3, Q4=V4)





# Diagrama de caja

# Se considera outliers a todo valor que está más allá de los bigotes. 
# Los bigotes son las líneas que se determinan como el tercer cuartil + 1.5 veces 
# el rango intercuartílico (Tercer cuartil menos el primer cuartil) y el primer cuartil -1.5 veces el rango intercuartílico.

#Bigote superior=3Q+1.5*RIC

#Bigote inferior=1Q-1.5*RIC 



# Seleccionar solo las columnas que contienen variables num?ricas
df_num <- datos_limpios %>%
  dplyr::select(where(is.numeric))

# Convertir los datos a formato largo para usar ggplot
df_num <- reshape2::melt(df_num)

# Diagrama de caja

df_num %>% ggplot(aes(x = variable, y = value)) +
  geom_boxplot(fill="dodgerblue", color = "black", outlier.colour="red") +
  labs(x = "Variable", y = "Valor") +
  facet_grid(. ~ variable, scales = "free_x")




# Hay algunos outliers. ¿Cuáles son?

# Total.pagar
# Gasto.promedio
# Frecuencia.compra


# Resolver eliminando outliers usando el operador pertenece %in% que funciona igual que el símbolo matemático ∈


# Elimina outliers Total.pagar 
datos_limpios <- datos_limpios %>%
  mutate(ruido = es_outlier(Total.pagar)) %>% 
  filter(ruido == FALSE) %>% dplyr::select(-ruido)

# Elimina outliers Gasto.promedio  

datos_limpios <- datos_limpios %>%
  mutate(ruido = es_outlier(Gasto.promedio)) %>% 
  filter(ruido == FALSE) %>% dplyr::select(-ruido)


# Elimina outliers Frecuencia.compra  
datos_limpios <- datos_limpios %>%
  mutate(ruido = es_outlier(Frecuencia.compra)) %>% 
  filter(ruido == FALSE) %>% dplyr::select(-ruido)

 
 
 
# Escalado y variables dummy  (para las variables categoricas)

# Escalado de las variables continuas

datos_final <- datos_limpios %>%
  dplyr::select(where(is.numeric), Compra)  

 
 

 
 
#######################
# Etapa 4. Modelado -------------------------------------------------------
#####################



# Cargar las bibliotecas necesarias
library(tidyverse)
library(stats)
library(factoextra)
library(cluster)

datos_agrupar <- datos_final %>% select(-Compra)

# Realizar el análisis de clustering con k-means
set.seed(123) # Fijar una semilla para reproducibilidad
kmeans_resultados <- kmeans(datos_agrupar, centers = 5, nstart = 20)

# Ver los resultados del clustering
kmeans_resultados
table(kmeans_resultados$cluster)

# Calcular la silueta para evaluar el número óptimo de grupos
silueta <- silhouette(kmeans_resultados$cluster, dist(datos_agrupar, method = "euclidean"))
fviz_nbclust(datos_agrupar, kmeans, method = "silhouette")

# Visualizar los grupos en un gráfico de dispersión
 




datos_caracteristicas <- datos_agrupar %>%  
  mutate(grupo = kmeans_resultados$cluster) %>% 
  group_by(grupo) %>% 
  summarize(Rating = mean(Rating),
            Tiempo.web = mean(Tiempo.web),
            Gasto.promedio = mean(Gasto.promedio),
            Total.pagar = sum(Total.pagar))
datos_caracteristicas
 