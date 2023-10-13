# install.packages("openxlsx") # Para importar/exportar excel
# install.packages("magrittr") # pipe
# install.packages("tidyverse") # Manipulación de datos y ggplot2

# Cargar la libreria a utilizar
library(openxlsx)
library(magrittr)
library(tidyverse)


# Leer el archivo de excel y asignarlo al objeto data_banco
data_banco <- read.xlsx(xlsxFile = "Data/Data_Banco.xlsx", sheet = "Data")
data_sucursal <- read.xlsx(xlsxFile = "Data/Data_Banco.xlsx", sheet = "Data_Sucursal")

# Se puede ver la estructura de un objeto con str()
str(data_banco)


x <- c(11, 12, 13, 14) # crea x
x  # presenta x
str(x)  # presenta x
# Seleccionar las columnas Transaccion, Tiempo_Servicio_seg del data.frame data_banco
# Note que como no se asignó, R evalúa la expresión y presenta el resultado
select( data_banco, Transaccion, Tiempo_Servicio_seg)


# Convertir el data_banco a un tibble
data_banco <- as_tibble( data_banco) 
# Muestra data_banco
data_banco


# Seleccionar las columnas Transaccion, Tiempo_Servicio_seg del data.frame data_banco
# Note que como no se asignó, R evalúa la expresión y presenta el resultado
select( data_banco, Transaccion, Tiempo_Servicio_seg)


# Con Pipe
data_banco %>% names %>% length
# Sin Pipe
length(names(data_banco))



# Seleccionar las columnas Transaccion, Tiempo_Servicio_seg del data.frame data_banco
# Note que como no se asignó, R evalúa la expresión y presenta el resultado
# Se lee, del data_banco, selecciona las columnas Transaccion y Tiempo_Servicio_seg
# data_banco[ , c("Transaccion", "Tiempo_Servicio_seg") ] ## Base de R
data_banco %>% select( Transaccion, Tiempo_Servicio_seg)


# Seleccionar todas las columnas cuyo nombre contenga el texto "Tra"
data_banco %>% select( contains("Tra"))

# Mostrar el resultado en el visor de RStudio
data_banco %>% select( contains("Tra")) %>% View


# Filtrar las filas correspondientes a la sucursal 62
data_banco %>% filter( Sucursal== 62 )
data_banco %>% filter( Sucursal== 62 ) %>% View

# Filtrar las filas correspondientes a la sucursal 62 y hayan durado más de 120 seg
data_banco %>% filter( Sucursal== 62 & Tiempo_Servicio_seg > 120 ) %>% View


# Filtrar las filas correspondientes a la sucursal 62,  hayan durado 
# más de 120 segundos y la evaluación a la satisfacción sea Bueno
data_banco %>% filter( Sucursal== 62 & Tiempo_Servicio_seg > 120 & 
                         Satisfaccion== "Muy Bueno") %>% View


# Filtrar las filas correspondientes a la sucursal 62 y 
# hayan durado más de 500 seg pero 
# muestra sólo la Sucursal, Transaccion y Tiempo
data_banco %>% 
  filter( Sucursal== 62 & Tiempo_Servicio_seg > 120 ) %>%
  select(Sucursal, Transaccion, Tiempo_Servicio_seg)


# Filtrar las filas correspondientes a la sucursal 62 y 
# hayan durado más de 500 seg pero 
# muestra sólo la Sucursal, Transaccion y Tiempo
select( filter( data_banco, Sucursal== 62 & Tiempo_Servicio_seg > 120 ),
        Sucursal, Transaccion, Tiempo_Servicio_seg)


# Ordenar por la satisfaccion
data_banco %>% arrange( Satisfaccion ) %>% View

# data_banco <- data_banco %>% arrange( Satisfaccion )


# Ordenar cada Transaccion y dentro de cada transaccion 
# de mayor a menor por tiempo de servicio
data_banco %>% arrange( Transaccion, desc(Tiempo_Servicio_seg) ) %>% View


## Eliminar transaccione que han durado menos de 30seg
data_banco_mod  <- data_banco %>% filter( Tiempo_Servicio_seg >= 60 ) 

# Crear una nueva columna con el tiempo en minutos
data_banco %>% mutate(Tiempo_Servicio_Min= Tiempo_Servicio_seg/60) %>% View
data_banco


# Crear una nueva columna con el tiempo en minutos
data_banco <- data_banco %>%
  mutate(Tiempo_Servicio_Min= Tiempo_Servicio_seg/60)
# Mostrar
data_banco


data_banco <- data_banco %>%
  mutate( Monto= str_replace(Monto, pattern = ",", replacement = ".") ) %>%
  mutate(Sucursal= as.character(Sucursal),
         Cajero = as.character(Cajero),
         Satisfaccion = parse_factor(Satisfaccion, 
                                     levels= c('Muy Malo', 'Malo', 'Regular', 
                                               'Bueno', 'Muy Bueno'), ordered = T),
         Monto= parse_number(Monto))

# Ver la estructura del data.frame
data_banco %>%  glimpse

# Obtener la medidas descriptivas del tiempo de servicio
data_banco %>% 
  summarise(
    MEDIA= mean(Tiempo_Servicio_seg, na.rm=TRUE), 
    MEDIA_ACOT= mean(Tiempo_Servicio_seg, na.rm = TRUE, trim = 0.05),
    DESV= sd(Tiempo_Servicio_seg, na.rm=TRUE), 
    RANGO= diff(range(Tiempo_Servicio_seg)),
    CANTIDAD= n() # n() permite contar el número de filas
  )





# Obtener la media del tiempo de servicio
data_banco %>% 
  summarise_at( vars(Tiempo_Servicio_seg, Monto),
                list(
                  MEDIA= ~mean(., na.rm=TRUE), 
                  MEDIA_ACOT= ~mean(., na.rm = TRUE, trim = 0.05),
                  DESV= ~sd(., na.rm=TRUE), 
                  RANGO= ~diff(range(.)),
                  CANTIDAD= ~n()
                )
  ) %>%  View



data_banco %>% 
  summarise_if( is.numeric,
                list(
                  MEDIA= ~mean(., na.rm=TRUE), 
                  MEDIA_ACOT= ~mean(., na.rm = TRUE, trim = 0.05)
                )
  ) %>%  View



# Obtener medidas de tendencia central para el tiempo de servicio para cada tipo de transaccion
data_banco %>% 
  group_by(Transaccion) %>%
  summarise( 
    MEDIA= mean(Tiempo_Servicio_seg, na.rm=TRUE), 
    MEDIA_ACOT= mean(Tiempo_Servicio_seg, na.rm = TRUE, trim = 0.05),
    DESV= sd(Tiempo_Servicio_seg, na.rm=TRUE), 
    CANTIDAD= n() 
  )


# Obtener medidas de tendencia central del 
# tiempo de servicio para cada combinación de 
# Transaccion y Nivel de Satisfaccion.
data_banco %>% 
  group_by(Transaccion, Satisfaccion) %>%
  summarise( 
    MEDIA= mean(Tiempo_Servicio_seg, na.rm=TRUE), 
    MEDIA_ACOT= mean(Tiempo_Servicio_seg, 
                     na.rm = TRUE, trim = 0.05),
    DESV= sd(Tiempo_Servicio_seg, na.rm=TRUE), 
    CANTIDAD= n() 
  )



# Obtener medidas de tendencia central
data_banco %>% 
  group_by(Transaccion) %>% 
  count(name = 'Frecuencia')


# Obtener medidas de tendencia central
data_banco %>% 
  group_by(Transaccion) %>% 
  count(name = 'Frecuencia') %>% 
  ungroup() %>% 
  mutate(
    F_Relativa= round(
      Frecuencia/sum(Frecuencia) ,4) *100
    )



# Límites
limites= seq(0, 7000, 1000)
limites

data_banco %>%
  mutate(
    Monto_Int= cut(Monto, limites, dig.lab= 5)) %>%
  group_by(Monto_Int) %>%
  count(name = 'Frecuencia')



data_banco %>%
  mutate(Monto_Int= cut(Monto, limites, dig.lab= 5 )) %>%
  group_by(Monto_Int) %>%
  count(name = 'Frecuencia') %>%
  ungroup() %>%
  mutate(
    F_Relativa= round(
      Frecuencia/sum(Frecuencia) ,4) *100,
    F_Acum= cumsum(Frecuencia),
    F_Rel_Acum= round(
      cumsum(Frecuencia)/sum(Frecuencia) ,4) *100
    )