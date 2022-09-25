library(tidyverse)

# SUBE - Cantidad de transacciones (usos) por fecha
# https://datos.gob.ar/dataset/transporte-sube---cantidad-transacciones-usos-por-fecha

# Datos raw ---------------------------------------------------------------

url = 'https://archivos-datos.transporte.gob.ar/upload/Dat_Ab_Usos/dat-ab-usos-'

sube_2020 <- read_csv(paste0(url, '2020.csv')) %>%
  janitor::clean_names() %>%
  mutate(fecha = as.Date(dia_transporte)) %>%
  select(-dia_transporte) %>%
  group_by(fecha, provincia, municipio, tipo_transporte, linea) %>%
  summarise(n = sum(cantidad))


sube_2021 <- read_csv(paste0(url, '2021.csv')) %>%
  janitor::clean_names() %>%
  mutate(fecha = as.Date(dia_transporte)) %>%
  select(-dia_transporte) %>%
  group_by(fecha, provincia, municipio, tipo_transporte, linea) %>%
  summarise(n = sum(cantidad))


sube_2022 <- read_csv(paste0(url, '2022.csv')) %>%
  janitor::clean_names() %>%
  mutate(fecha = as.Date(dia_transporte)) %>%
  select(-dia_transporte) %>%
  group_by(fecha, provincia, municipio, tipo_transporte, linea) %>%
  summarise(n = sum(cantidad))


# Unión de datasets -------------------------------------------------------

df <- bind_rows(sube_2020,
                sube_2021,
                sube_2022)


# Datos filtrados ---------------------------------------------------------

lineas <- df %>%
  filter(tipo_transporte %in% c('TREN', 'COLECTIVO')) %>%
  group_by(linea) %>%
  summarise(n = sum(n)) %>% arrange(desc(n)) %>%
  head(4) %>%
  pull(linea)

df_filt <- df %>%
  filter(fecha >= '2021-01-01') %>%
  filter(linea %in% lineas) %>%
  group_by(fecha, linea) %>%
  summarise(n = sum(n)) %>%
  ungroup()


df_filt %>%
  group_by(linea) %>%
  timetk::plot_time_series(
    .date_var = fecha,
    .value = n,
    .interactive = FALSE,
    .facet_ncol = 2
  )

df_filt %>% write.csv('data/df_sube.csv', row.names = FALSE)




