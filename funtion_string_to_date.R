tranf_fecha <- function(x){
  library(dplyr)
  library(lubridate)
  #Funcion completa
  identificacion_fecha <- apply(x, MARGIN = 2, 
                                FUN = function(y) all(grepl('^[[:digit:]]{4}-[[:digit:]]{2}-[[:digit:]]{2}', y) | grepl('^$', y))  & !all(grepl('^$', y)))
  
  #Esto sirve para buscar donde hay true en la variable y las que son true son las que son fecha
  identificacion_fecha <- names(identificacion_fecha[identificacion_fecha == TRUE])
  
  if (length(identificacion_fecha) > 0){
    for (i in 1:length(identificacion_fecha)) {
      #Porque es mejor na_if que ymd_hms, ya que si el usuario llega a poner 2 veces las fechas te las vuelve NA
      x[ , identificacion_fecha[i]] <- date(na_if(x[ , identificacion_fecha[i]], ""))
    }
    return(x)
  }
}