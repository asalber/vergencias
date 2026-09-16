library(tidyverse)
library(lubridate)
library(janitor)

# Carga del dataframe original
df <- read_csv("data/datos.csv")
# Limpieza del dataframe
df <- df |> 
  # Eliminar filas y columnas vacías
  remove_empty(c("rows", "cols")) |> 
  # Eliminar columnas constantes
  remove_constant(na.rm = TRUE, quiet = FALSE) 

# Comprobamos que no hay duplicados
df |> get_dupes()

# Ordenamos el dataframe por paciente y número de revisión
df <- arrange(df, ID, REVISION)

# Creación de la columna de edad en años y en días
df <- df |> 
  mutate(FECHA.REVISION = mdy(FECHA.REVISION), 
         FECHA.NACIMIENTO = mdy(FECHA.NACIMIENTO),
         EDAD.PERIODO = as.period(interval(FECHA.NACIMIENTO,FECHA.REVISION)),
         EDAD.DIAS = days(EDAD.PERIODO),
         EDAD = year(EDAD.PERIODO))

# Cálculo de tiempo entre revisiones
df <- df |> 
  group_by(ID) |> 
  mutate(TIEMPO.ENTRE.REVISIONES = FECHA.REVISION - lag(FECHA.REVISION, default = first(FECHA.REVISION))) |> 
  select(ID, REVISION, FECHA.REVISION, TIEMPO.ENTRE.REVISIONES, everything())

# Cálculo de evoluciones
df <- df |> 
  group_by(ID) |> 
  mutate(across(where(is.numeric), function(x) x-lag(x, default = first(x)), .names = "{.col}.EVOL"))

# Guardar el conjunto de datos final
df |> 
  select(-c(REVISION.EVOL, EDAD.EVOL, EDAD.PERIODO.EVOL, EDAD.DIAS.EVOL)) |> 
  write_csv("data/datos.preprocesados.csv")  

# CREACIÓN CONJUNTO DE DATOS SIN MEDIDAS REPETIDAS

# Filtro de la primera y última revisión para pacientes con al menos dos revisiones
df <- df |> 
  group_by(ID) |> 
  mutate(REVISION.MAX = max(REVISION)) |>  
  filter(REVISION == 1 | REVISION == REVISION.MAX) |> 
  filter(n() > 1)


# Cálculo de tiempo entre revisiones
df <- df |> 
  group_by(ID) |> 
  mutate(TIEMPO.ENTRE.REVISIONES = FECHA.REVISION - lag(FECHA.REVISION, default = first(FECHA.REVISION))) |> 
  select(ID, REVISION, FECHA.REVISION, TIEMPO.ENTRE.REVISIONES, everything()) 

# Cálculo de evoluciones
df <- df |> 
  group_by(ID) |> 
  mutate(across(where(is.numeric), function(x) x-lag(x, default = first(x)), .names = "{.col}.EVOL"))

# Guardar el conjunto de datos final
df |> 
  select(-c(REVISION.EVOL, EDAD.EVOL, EDAD.PERIODO.EVOL, EDAD.DIAS.EVOL)) |> 
  filter(REVISION != 1) |> 
  write_csv("data/datos.preprocesados.sin.medidas.repetidas.csv")
