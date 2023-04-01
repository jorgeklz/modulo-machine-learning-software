
moda <- function(x) {
  ux <- unique(x)
  # Chequea si hay o no moda 
  if (length(x)==length(ux)) {
    mode_x <- 'Moda no existe'
    return(mode_x)
  }
  
  # Encuentra el numero de ocurrencias
  tab <- tabulate(match(x, ux))
  
  # Chequea si hay o no moda 
  if (mean(tab)==max(tab)) {
    mode_x <- 'Moda no existe'
    return(mode_x)
  }
  
  # Encuentra y devuelve la moda
  idx <- which.max(tab)
  mode_x <- ux[idx]
  return(mode_x)
}


rango <- function(x) {
 
  return(diff(range(x)) )
}

 
es_outlier <- function(x) {
  return(x <= quantile(x, 0.25) - 1.5 * IQR(x) | x >= quantile(x, 0.75) + 1.5 * IQR(x))
}


quibble <- function(x, q = c(0.25, 0.5, 0.75, 1), dropNA = TRUE) {
  tibble(x = quantile(x, q, na.rm = dropNA), q = q)
}


