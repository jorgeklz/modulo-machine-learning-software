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
library(Boruta)
#library(mlr)
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



# Verificar corrección de outlier 

datos_limpios %>% 
  dplyr::select(Total.pagar, Compra) %>% 
  group_by(Compra) %>% 
  mutate(outlier = es_outlier(Total.pagar)) %>% 
  filter(outlier == FALSE) %>%
  ggplot(aes(Compra, Total.pagar, fill = Compra)) +
  geom_boxplot(outlier.colour="red")


datos_limpios %>% 
  dplyr::select(Gasto.promedio, Compra) %>% 
  group_by(Compra) %>% 
  mutate(outlier = es_outlier(Gasto.promedio)) %>% 
  filter(outlier == FALSE) %>%
  ggplot(aes(Compra, Gasto.promedio, fill = Compra)) +
  geom_boxplot(outlier.colour="red")


datos_limpios %>% 
  dplyr::select(Frecuencia.compra, Compra) %>% 
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
  dplyr::select(where(is.numeric), Compra) %>% 
  ggpairs(columns = 1:9, mapping = aes(color = Compra))


# Ver correlacion entre variables independientes y la dependiente

datos_limpios %>% dplyr::select( c(1, 5:12, 15)) %>%
  ggpairs(., 
          legend = 1, 
          mapping = ggplot2::aes(colour=Compra), 
          lower = list(continuous = wrap("smooth", alpha = 0.3, size=0.1))) +
  theme(legend.position = "bottom")  








# Escalado y variables dummy  (para las variables categoricas)

# Escalado de las variables continuas

datos_escalados <- datos_limpios %>%
  dplyr::select(where(is.numeric)) %>%
  #mutate_at(vars(Precio, Descuento, Calidad, Popularidad), scale)
  mutate((across(c(1:9), rescale, .names = 'nueva_{col}')))



# Dummy de las variables discretas (categoricas)

# Creación de variables dummy (a partir de 3 niveles)
datos_dummies <- datos_limpios %>%
  dummy_cols(select_columns = c("Nivel.educativo", "Producto", "Pago"), remove_first_dummy = FALSE)



# Numerizar -------------------------------------------------------------

datos_numerizados <- datos_dummies %>% mutate(nueva_Genero=ifelse(Genero == "Femenino", 1, 0), 
                                              nueva_Influencer=ifelse(Influencer == "Si", 1, 0),
                                              nueva_Compra =ifelse(Compra == "Si", 1, 0))



# Unir escalados+ dummies  + numerizados


ds_escalados <- datos_escalados %>% dplyr::select(starts_with("nueva", ignore.case = TRUE, vars = NULL))

ds_dummies <- datos_dummies %>% dplyr::select(starts_with(c("Nivel_", "Producto_","Pago_"), ignore.case = TRUE, vars = NULL))

ds_numerizados <- datos_numerizados %>% dplyr::select(nueva_Genero, nueva_Influencer, nueva_Compra)






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

# quitar espacios en nombre de variables

names(datos_final) <- make.names(names(datos_final), unique=TRUE)





###########################
# Etapa 4. Modelado ---------------------------------------------------------------------
###########################

# Existen varias técnicas de selección de características en R. 
# A continuación, se evaluarán tres modelos de selección de características:

# Boruta
# StepWise
#  Recursive Feature Elimination (RFE)

# Para evaluar la eficacia de estas técnicas, se utilizará un modelo de 
# clasificación de Regresión Logística con una base de datos de ejemplo. 
# La base de datos contiene 14 variables predictoras y una variable objetivo "Compra" con dos posibles valores: "Si" o "No". 
# El objetivo del modelo es predecir si un cliente realizará una compra o no en función de las características de su perfil.



# Variables a usar en análisis: Todas las independientes

# Verificamos que las variables categóricas esten como factores pero numéricos

datos_final$Genero <- as.factor(datos_final$Genero)  
datos_final$Influencer <- as.factor(datos_final$Influencer) 
datos_final$Compra <- as.factor(datos_final$Compra)
datos_final$Compra <- relevel(datos_final$Compra, ref = "1")

datos_final <- datos_final %>% dplyr::select(-c(Pago_Transferencia, Producto_Nintendo.Switch))
 
 


# Train / test ------------------------------------------------------------
 

entrenamiento_ids <- sample(1:nrow(datos_final), nrow(datos_final)*0.7, replace = FALSE)
ds_entrenamiento <- datos_final[entrenamiento_ids, ]
ds_prueba <- datos_final[-entrenamiento_ids, ]

# Ecuación de regresión ---------------------------------------------------


formula_base <- as.formula("Compra ~ .")

# Entrenar el modelo base (con todas las variables independientes)
modelo_base <- caret::train(formula_base, 
                            data = ds_entrenamiento, 
                            method="glm",
                            family = "binomial")
 
# Realizar predicciones
prediccion_base <- predict(modelo_base, newdata = ds_prueba)

# Verificiar rendimiento con matriz de confusion
caret::confusionMatrix(prediccion_base, ds_prueba$Compra)

# El modelo base ha obtenido una tasa de exactitud del 55% en el conjunto de prueba.


# Emplear todas las características disponibles, especialmente cuando algunas de ellas no son significativas, 
# en un modelo de aprendizaje automático puede provocar la aparición de sobreajuste.




############
# Boruta ---------------------------------------------------------------------
#############

# Es un método basado en la idea de comparar las variables observadas con variables aleatorias generadas a partir de permutaciones aleatorias de los valores de las variables.


 
modelo_boruta <- Boruta(formula_base, data = ds_entrenamiento, doTrace = 1)
plot(modelo_boruta, sort=TRUE, las = 2)

 
# obtener las variables seleccionadas
selected_features_boruta <- getSelectedAttributes(modelo_boruta, withTentative = FALSE)

# Crear un modelo de clasificación con las características seleccionadas

formula_boruta <- as.formula(paste("Compra ~", paste(selected_features_boruta, collapse = "+")))

modelo_boruta_final <- caret::train(formula_boruta,
                                    data = ds_entrenamiento, 
                                    method="glm",
                                    family = "binomial")

 

# Evaluar el modelo con el conjunto de prueba
prediccion_boruta <- predict(modelo_boruta_final, newdata = ds_prueba)

# Verificar rendimiento con matriz de confusion
caret::confusionMatrix(prediccion_boruta, ds_prueba$Compra) 



# El modelo BORUTA ha seleccionado las variables 
# "Precio.unidad" "Cantidad"      "Total.pagar"   "Rating"        "Tiempo.web" como las más 
# importantes para predecir la variable objetivo "Compra". 

# El modelo BORUTA con esta variable ha obtenido una tasa de exactitud del 57% en el conjunto de prueba.







#######################
# stepAIC ---------------------------------------------------------------------
#######################

#Selección hacia adelante (Forward selection): comienza con un modelo vacío y agrega una característica a la vez hasta que se alcanza un criterio de parada. El criterio de parada puede ser un cierto número de características o la mejora de la precisión del modelo se vuelve insignificante.

#Selección hacia atrás (Backward elimination): comienza con un modelo que incluye todas las características y elimina una característica a la vez hasta que se alcanza un criterio de parada. El criterio de parada puede ser un cierto número de características o la mejora de la precisión del modelo se vuelve insignificante.

#Selección por pasos (Stepwise selection): combina los dos métodos anteriores, agregando y eliminando características a la vez en cada paso, hasta que se alcanza un criterio de parada
# Basado en el criterio de información de Akaike (AIC, por sus siglas en inglés)

# Crear un modelo de regresión logística múltiple inicial que incluye todas las características

modelo_todo <- glm(formula_base, 
                   data = ds_entrenamiento, 
                   family = binomial)


# Realizar la selección de características utilizando stepAIC
step.model <- MASS::stepAIC(modelo_todo, direction = "both", trace = 0) #backward, forward, #both=Stepwise

# Imprimir el resumen del modelo resultante
summary(step.model)

# Ver los coeficientes de las características en el modelo resultante
var_relevantes <- coef(step.model)


# obtener las variables seleccionadas
selected_features_stepwise <- names(var_relevantes[2:length(var_relevantes)])

# Crear un modelo de clasificación con las características seleccionadas

formula_stepwise <- as.formula(paste("Compra ~", paste(selected_features_stepwise, collapse = "+")))

modelo_stepwise_final <- caret::train(formula_stepwise, 
                                      data = ds_entrenamiento,
                                      method="glm", 
                                      family = "binomial")



# Evaluar el modelo con el conjunto de prueba
prediccion_stepwise <- predict(modelo_stepwise_final, newdata = ds_prueba)

# Verificar rendimiento con matriz de confusion
caret::confusionMatrix(prediccion_stepwise, ds_prueba$Compra) 



# El modelo StepWise ha seleccionado las variables 
# "Rating "  "Tiempo.web" como las más 
# importantes para predecir la variable objetivo "Compra". 

# El modelo StepWise con esta variable ha obtenido una tasa de exactitud del 58% en el conjunto de prueba.




 


####################
# RFE ---------------------------------------------------------------------
####################

# Recursive Feature Elimination (RFE)
# Entrena un modelo con todas las variables predictoras y luego eliminar las variables menos importantes una a una, hasta que se alcance el número deseado de variables.

# Realizar la selección de características con Recursive Feature Elimination (RFE)
ctrl <- rfeControl(functions = caretFuncs, method = "cv", number = 5)
modelo_rfe <- rfe(x=ds_entrenamiento[, -19], 
                  y= as.factor(unlist(ds_entrenamiento[, 19])), 
                  sizes = c(1:18), rfeControl = ctrl)

# Imprimir los resultados de la selección de características
print(modelo_rfe)

# Crear un modelo de clasificación con las características seleccionadas
selected_features_rfe <- names(ds_entrenamiento[, -19])[names(ds_entrenamiento[, -19]) %in% modelo_rfe$optVariables]



formula_rfe <- as.formula(paste("Compra ~", paste(selected_features_rfe, collapse = "+")))
modelo_rfe_final <- train(formula_rfe, data = ds_entrenamiento, 
                          method = "glm", 
                          family = "binomial")

# Evaluar el modelo con el conjunto de prueba
prediccion_rfe <- predict(modelo_rfe_final, newdata = ds_prueba)

# Verificar rendimiento con matriz de confusion
caret::confusionMatrix(prediccion_rfe, ds_prueba$Compra) 

# El modelo con RFE ha seleccionado las variables 
# "Rating" como las más 
# importantes para predecir la variable objetivo "Compra". 

# El modelo rfe con esta variable ha obtenido una tasa de exactitud del 67.46% en el conjunto de prueba.




 
 
