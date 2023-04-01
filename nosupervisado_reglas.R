library("arulesViz")
library("arules")

# cargar los datos
data("Groceries")
# El conjunto de datos comestibles contiene 1 mes (30 días) de datos de transacciones de un punto de
# venta de una tienda mayorista de comestibles. El conjunto de datos consta de 9835 transacciones y los
# elementos se agregan a 169 categorías de productos.

# almacenar datos
ds_comestibles <- Groceries


# mostrar resumen de las 5 primeras transacciones
inspect(head(ds_comestibles, 5))


# Note que el conjunto de datos es un objeto de tipo transacción, con 9835 elementos y 169 categorías.
# Para ver la estructura de este tipo de objetos usamos la función str().

str(ds_comestibles)

#  objeto de tipo transacción tiene 3 slots: data, itemInfo y itemsetInfo.
# 1 El slot data contiene los datos de la transacción.
# 2 El slot itemInfo es un dataframe. La columna de labels almacena las etiquetas o los nombres de los elementos o productos.
# 3 El slot itemsetInfo es un dataframe inicialmente vacío, que no se rellenará con los itemsets hasta que se ejecute un algoritmo de asociación sobre este conjunto de datos.
 


# Muestra los primeros 20 nombre de productos.
ds_comestibles@itemInfo$labels[1:20]

# Enlista los indices de los 10 primeros productos que estan incluidos
# en cada transaccion, repetido desde la primera transacción hasta la última.
ds_comestibles@data@i[1:10]

# Indica la posición inicial para cada transacción al leer sus índices de productos 
# asociados de ds_comestibles@data@i[1:10]
ds_comestibles@data@p[1:10]



# Algoritmo A priori ------------------------------------------------------


# Ejecutamos el algoritmo a-priori indicando que las reglas deben ser de un
# largo de minimo 2 (al menos 1 en antecedente y 1 en consecuente) y como 
# umbrales 0.002 y 0.50 de soporte y confianza, respectivamente
reglas <- apriori(ds_comestibles, 
                  parameter = list(minlen=2, supp=0.002, conf=0.50))

# Se ordenan las reglas segun soporte
reglas.ordenadas <- sort(reglas, by="supp")

# Se muestran las 5 primeras reglas de las 1098 (writing ... [1098 rule(s)]) 
# totales obtenidas
inspect(head(reglas.ordenadas, 5))




################
# Redundantes -------------------------------------------------------------
#################

# Es común que en la generación de reglas de asociación existan varias reglas que son un subconjunto de
# otras reglas. Para evitar esta situación es posible “podar” estas, de modo que, finalmente, se obtengan
# reglas de asociación sin redundancias


# Se compara cada regla contra las otras, usando una matriz cuadrada de 
# subconjuntos. Donde se denota con TRUE si una regla es subconjunto de otra.
subset.matrix <- is.subset(reglas.ordenadas, reglas.ordenadas)
# Se convierte la diagonal inferior de la matriz en F (FALSE)
subset.matrix[lower.tri(subset.matrix, diag=T)] <- F
# Se identifica (con any) todos los casos con TRUE.
redundantes <- apply(subset.matrix, 2, any)
# Se filtra dejando solo reglas no redundantes.
reglas.podadas<- reglas.ordenadas[!redundantes]
# Contar reglas sin redundancia
reglas.podadas
# Se muestran las 5 ultimas de las 616 obtenidas luego de la poda.
inspect(head(reglas.podadas, 5))


################
# Visualizacion -----------------------------------------------------------
#################


# Diagrama de dispersion considerando las tres metricas de calidad principales
plot(head(reglas.podadas, 20))
# Metodo agrupacion considerando las tres metricas de calidad principales
plot(head(reglas.podadas, 90), method="grouped")
# Metodo grafos considerando las tres metricas de calidad principales
plot( head(reglas.podadas, 20) , method="graph" )
