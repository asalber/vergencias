library(tidyverse)
library(lubridate)

# Carga del data frame original
df <- read_csv("data/datos.csv")
# Normalizar nombres de variables
names(df) <- make.names(names(df))

df <- arrange(df, ID, REVISION)

# Carga del data frame con las fechas de todas las revisiones
df2 <- read_csv("data/datos-fechas.csv")

# Selección de las columnas con fechas de revisión
df2 <- df2 %>% select(ID, starts_with("FECHA Revisión"))

# Conversión a formato largo y eliminación de filas sin fecha
df.fechas <- df2 %>%
  pivot_longer(-ID, names_to = "REVISION", values_to = "FECHA.REVISION") %>% 
  mutate(REVISION = as.numeric(str_replace(REVISION, "FECHA Revisión", ""))) %>% 
  drop_na()

# Fusión con el data frame original
df.final <- df %>% 
  select(-FECHA.REVISION) %>% 
  left_join(df.fechas, by=c("ID","REVISION")) %>% 
  select(ID, REVISION, FECHA.REVISION, everything()) %>% 
  arrange(ID, REVISION) 

# Creación de la columna de edad en años y en días
df.final <- df.final %>% 
  mutate(FECHA.REVISION = dmy(FECHA.REVISION), 
         FECHA.NACIMIENTO = dmy(FECHA.NACIMIENTO),
         EDAD.PERIDODO = as.period(interval(FECHA.NACIMIENTO,FECHA.REVISION)),
         EDAD.DIAS = days(EDAD.PERIDODO),
         EDAD = year(EDAD.PERIDODO))

# Cálculo de tiempo entre revisiones
df.final <- df.final %>% 
  group_by(ID) %>% 
  mutate(TIEMPO.ENTRE.REVISIONES = FECHA.REVISION - lag(FECHA.REVISION, default = first(FECHA.REVISION))) %>% 
  select(ID, REVISION, FECHA.REVISION, TIEMPO.ENTRE.REVISIONES, everything()) 

# Cálculo de evoluciones
df.final <- df.final %>% 
  group_by(ID) %>% 
  mutate(across(where(is.numeric), function(x) x-lag(x, default = first(x)), .names = "{.col}.EVOL"))

# Guardar el conjunto de datos final
df.final %>%
  select(-c(REVISION.EVOL, EDAD.EVOL, EDAD.PERIDODO.EVOL, EDAD.DIAS.EVOL)) %>% 
  write_csv("data/datos.preprocesados.csv")  
  

# CREACIÓN CONJUNTO DE DATOS SIN MEDIDAS REPETIDAS

# Fusión con el data frame original
df.final <- df %>% 
  select(-FECHA.REVISION) %>% 
  left_join(df.fechas, by=c("ID","REVISION")) %>% 
  select(ID, REVISION, FECHA.REVISION, everything()) %>% 
  arrange(ID, REVISION) 

# Filtro de la primera y última revisión para pacientes con al menos dos revisiones
df.final <- df.final %>% 
  group_by(ID) %>% 
  mutate(REVISION.MAX = max(REVISION)) %>% 
  filter(REVISION == 1 | REVISION == REVISION.MAX) %>% 
  filter(n() > 1)

# Creación de la columna de edad en años y en días
df.final <- df.final %>% 
  mutate(FECHA.REVISION = dmy(FECHA.REVISION), 
         FECHA.NACIMIENTO = dmy(FECHA.NACIMIENTO),
         EDAD.PERIDODO = as.period(interval(FECHA.NACIMIENTO,FECHA.REVISION)),
         EDAD.DIAS = days(EDAD.PERIDODO),
         EDAD = year(EDAD.PERIDODO))

# Cálculo de tiempo entre revisiones
df.final <- df.final %>% 
  group_by(ID) %>% 
  mutate(TIEMPO.ENTRE.REVISIONES = FECHA.REVISION - lag(FECHA.REVISION, default = first(FECHA.REVISION))) %>% 
  select(ID, REVISION, FECHA.REVISION, TIEMPO.ENTRE.REVISIONES, everything()) 

# Cálculo de evoluciones
df.final <- df.final %>% 
  group_by(ID) %>% 
  mutate(across(where(is.numeric), function(x) x-lag(x, default = first(x)), .names = "{.col}.EVOL"))

# Guardar el conjunto de datos final
df.final %>%
  select(-c(REVISION.EVOL, EDAD.EVOL, EDAD.PERIDODO.EVOL, EDAD.DIAS.EVOL)) %>% 
  filter(REVISION != 1) %>% 
  write_csv("data/datos.preprocesados.sin.medidas.repetidas.csv")  

