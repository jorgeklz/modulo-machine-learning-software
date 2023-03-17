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



options(scipen=999)

 

###########################
# Etapa 1. Dominio ---------------------------------------------------------------------
###########################

















###########################
# Etapa 2. Adquisición ---------------------------------------------------------------------
###########################

# Leer data ---------------------------------------------------------------
 
#datos  <-  readxl::read_excel("C:/Users/UTM/Dropbox/Modulo ML/Practicas/tienda.xlsx")
 
datos_originales  <-  readr::read_delim("tienda-online.csv", delim = ",")

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
  select(where(is.factor))

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


source("funciones.R")

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
  select(ends_with("_x")) %>% rotate_df() %>% rename(Q1=V1, Q2=V2, Q3=V3, Q4=V4)
  
 
 


# Diagrama de caja

# Se considera outliers a todo valor que está más allá de los bigotes. 
# Los bigotes son las líneas que se determinan como el tercer cuartil + 1.5 veces 
# el rango intercuartílico (Tercer cuartil menos el primer cuartil) y el primer cuartil -1.5 veces el rango intercuartílico.

#Bigote superior=3Q+1.5*RIC

#Bigote inferior=1Q-1.5*RIC 

 

# Seleccionar solo las columnas que contienen variables num?ricas
df_num <- datos_limpios %>%
  select(where(is.numeric))

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
  filter(ruido == FALSE) %>% select(-ruido)

# Elimina outliers Gasto.promedio  

datos_limpios <- datos_limpios %>%
  mutate(ruido = es_outlier(Gasto.promedio)) %>% 
  filter(ruido == FALSE) %>% select(-ruido)


# Elimina outliers Frecuencia.compra  
datos_limpios <- datos_limpios %>%
  mutate(ruido = es_outlier(Frecuencia.compra)) %>% 
  filter(ruido == FALSE) %>% select(-ruido)



# Verificar corrección de outlier 

datos_limpios %>% 
  select(Total.pagar, Compra) %>% 
  group_by(Compra) %>% 
  mutate(outlier = es_outlier(Total.pagar)) %>% 
  filter(outlier == FALSE) %>%
  ggplot(aes(Compra, Total.pagar, fill = Compra)) +
  geom_boxplot(outlier.colour="red")


datos_limpios %>% 
  select(Gasto.promedio, Compra) %>% 
  group_by(Compra) %>% 
  mutate(outlier = es_outlier(Gasto.promedio)) %>% 
  filter(outlier == FALSE) %>%
  ggplot(aes(Compra, Gasto.promedio, fill = Compra)) +
  geom_boxplot(outlier.colour="red")


datos_limpios %>% 
  select(Frecuencia.compra, Compra) %>% 
  group_by(Compra) %>% 
  mutate(outlier = es_outlier(Frecuencia.compra)) %>% 
  filter(outlier == FALSE) %>%
  ggplot(aes(Compra, Frecuencia.compra, fill = Compra)) +
  geom_boxplot(outlier.colour="red")



























# Diagrama de dispersión (correlación)

# Sólo columnas numéricas
#ggscatmat(datos_limpios %>% select_if(is.numeric) )

 

# Ver correlacion entre variables independientes

datos_limpios %>% 
  select(where(is.numeric), Compra) %>% 
  ggpairs(columns = 1:9, mapping = aes(color = Compra))


# Ver correlacion entre variables independientes y la dependiente

datos_limpios %>% select( c(1, 5:12, 15)) %>%
  ggpairs(., 
          legend = 1, 
          mapping = ggplot2::aes(colour=Compra), 
          lower = list(continuous = wrap("smooth", alpha = 0.3, size=0.1))) +
  theme(legend.position = "bottom")  


 


# Escalado y variables dummy  (para las variables categoricas)
 
# Escalado de las variables continuas
  
datos_escalados <- datos_limpios %>%
  select(where(is.numeric)) %>%
    #mutate_at(vars(Precio, Descuento, Calidad, Popularidad), scale)
    mutate((across(c(1:9), log, .names = 'nueva_{col}')))
  


# Dummy de las variables discretas (categoricas)

# Creación de variables dummy (a partir de 3 niveles)
datos_dummies <- datos_limpios %>%
    dummy_cols(select_columns = c("Nivel.educativo", "Producto", "Pago"), remove_first_dummy = FALSE)
  
    

# Numerizar -------------------------------------------------------------

datos_numerizados <- datos_dummies %>% mutate(nueva_Genero=ifelse(Genero == "Femenino", 1, 2), 
                                  nueva_Influencer=ifelse(Influencer == "No", 1, 2),
                                  nueva_Compra =ifelse(Compra == "No", 1, 2))



# Unir escalados+ dummies  + numerizados


ds_escalados <- datos_escalados %>% select(starts_with("nueva", ignore.case = TRUE, vars = NULL))

ds_dummies <- datos_dummies %>% select(starts_with(c("Nivel_", "Producto_","Pago_"), ignore.case = TRUE, vars = NULL))

ds_numerizados <- datos_numerizados %>% select(nueva_Genero, nueva_Influencer, nueva_Compra)






# Final -------------------------------------------------------------------

datos_final <- bind_cols(list(ds_escalados, ds_dummies, ds_numerizados))
 

datos_final <- datos_final %>% rename(Edad = nueva_Edad, 
                                   Genero = nueva_Genero ,                                   
                                   Precio.unidad = nueva_Precio.unidad,
                                   Cantidad = nueva_Cantidad,
                                   Descuento = nueva_Descuento, 
                                   Total.pagar = nueva_Total.pagar, 
                                   Gasto.promedio = nueva_Gasto.promedio,
                                   Frecuencia.compra = nueva_Frecuencia.compra, 
                                   Rating = nueva_Rating, 
                                   Tiempo.web =nueva_Tiempo.web  , 
                                   Influencer = nueva_Influencer  , 
                                   Compra = nueva_Compra 
                                   )
    
    
    
  
  

  
  
  
  
  
 
