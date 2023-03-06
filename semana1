#Librerias

library(dplyr)
library(fastDummies)
library(ggplot2)
library(haven)
library(tidyr)
library(GGally)


 
set.seed(1987)
n <- 1150
 

# Leer data ---------------------------------------------------------------

#leer archivos de datos
datos  <-  readxl::read_excel("/home/jorgeklz/Dropbox/Modulo ML/tienda.xlsx")

datos <- datos %>% mutate(Edad=as.numeric(Edad))


# Eliminar valores ausentes
#En el primer paso, se utiliza la función na.omit para eliminar las filas que contienen valores ausentes en cualquier columna.

df_clean <- na.omit(datos)





# EDA ---------------------------------------------------------------------

# Calcular media
mean(df_clean$Precio)

# Calcular mediana
median(df_clean$Precio)

# Calcular moda
library(modeest)
mlv(df_clean$Precio)

# Calcular percentiles
quantile(df_clean$Precio, c(0.25, 0.5, 0.75))

# Calcular rango
range(df_clean$Precio)

# Calcular desviación estándar
sd(df_clean$Edad)

 
# Calcular desviación estándar poblacional
sqrt(var(df_clean$Precio) * ((n-1)/n))





# Graficos ----------------------------------------------------------------


# Variables numericas

# Seleccionar solo las columnas que contienen variables numéricas
df_num <- df_clean %>%
  select(where(is.numeric))

# Convertir los datos a formato largo para usar ggplot
df_num <- reshape2::melt(df_num)

# Histograma
df_num %>% ggplot( aes(x = value)) + 
  geom_histogram(binwidth = 1, fill="dodgerblue", color = "black") +
  facet_wrap(~ variable, scales = "free_x") +
  labs(title = "Histogramas de variables numericas", 
       x = "", y = "Frecuencia") +
  theme_bw()

 

# Diagrama de caja

df_num %>% ggplot(aes(x = variable, y = value)) +
  geom_boxplot(fill="dodgerblue", color = "black") +
  labs(x = "Variable", y = "Valor") +
  facet_grid(. ~ variable, scales = "free_x")







# Variables categoricas

# Diagrama de columna

# Seleccionar solo las columnas que contienen variables texto (categoria)
df_chr <- df_clean  %>% mutate_if(is.character,as.factor) %>%
  select(where(is.factor))

datos_largo <- pivot_longer(df_chr, 
                            cols = c(Genero, Nivel_educativo, Producto, Envio, Devolucion, Compra), 
                            names_to = "variable", 
                            values_to = "valor")
conteo <- datos_largo %>% count(variable, valor)

conteo %>% ggplot(aes(x = variable, y = n, fill = valor)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Variable", y = "Número de valores", fill = "Valor") +
  facet_grid(. ~ variable, scales = "free_x")  +
  theme_bw()




# Diagrama de dispersión
ggplot(df_clean, aes(x=Precio, y=Popularidad)) + 
  geom_point(color = "dodgerblue") + 
  ggtitle("Diagrama de Dispersion de Precios vs Popularidad") + 
  xlab("Precio") + 
  ylab("Cantidad")




# correlación de variables

df_clean %>%   select(where(is.numeric), Compra) %>% 
  ggpairs(columns = 1:6, mapping = aes(color = Compra))








# Eliminar outliers


# Se utiliza la librería dplyr para seleccionar los datos por cada una de las variables numéricas 
# y, posteriormente, escala los datos de cada grupo para luego usar apply y una función anónima para verificar si cada 
# fila contiene valores dentro del rango aceptable de -3 a 3 (observaciones que se encuentran a más de 3 desviaciones estándar de la media ),
# La función log() normaliza estas variables, restándoles su media y dividiéndolas por su desviación estándar.
# A continuación, usamos filter para seleccionar solo las filas que pasan el criterio y almacenamos el conjunto de datos limpio en data_clean.
 
# Esto se hace con el objetivo de eliminar outliers que puedan afectar el modelo de clasificación.
#Después de aplicar estos pasos, se obtiene un nuevo conjunto de datos df_clean sin valores ausentes ni outliers en las variables numéricas especificadas en var_num.

 
  df_clean <- df_clean %>%
    select(where(is.numeric)) %>% 
    mutate(across(1:5, scale)) %>%
    apply(1, function(x) all(between(x, -3, 3))) %>%
    unlist() %>%
    as.logical() %>%
    filter(df_clean, .)
  
  
 
  glimpse(df_clean)

  
  

  # Escalado y variables dummy  (para las variables categoricas)
 
  # Escalado de las variables continuas
  
  df_scaled <- df_clean %>%
    #mutate_at(vars(Precio, Descuento, Calidad, Popularidad), scale)
    mutate((across(c(1, 5:8), log, .names = 'new_{col}')))
  
  # Dummy de las variables discretas (categoricas)
  # Creación de variables dummy
  df_dummy <- df_scaled %>%
    dummy_cols(select_columns = c("Nivel_educativo", "Producto"), remove_first_dummy = FALSE)
  
    

# Numerizar -------------------------------------------------------------

  df_final <- df_dummy %>% mutate(Envio=ifelse(Envio == "No", 1, 2), 
                                  Devolucion=ifelse(Devolucion == "No", 1, 2))
   
  

# Final -------------------------------------------------------------------

  
  df_final <- df_final %>% select(new_Edad, new_Precio, new_Descuento, new_Calidad, new_Popularidad,
                                  Nivel_educativo_Primaria, Nivel_educativo_Secundaria, Nivel_educativo_Universitaria,
                                  Producto_Electronica, Producto_Hogar, Producto_Juguetes, Producto_Ropa,
                                  Envio, Devolucion, Compra)  
  
  df_final <- df_final %>% rename(Edad = new_Edad, Precio = new_Precio, Descuento = new_Descuento, Calidad = new_Calidad, Popularidad = new_Popularidad)
    
    
    
  
  
  df_final %>%   select(where(is.numeric), Compra) %>% 
    ggpairs(columns = c(1:5,15), mapping = aes(color = Compra))
  
  
  
  
# 
