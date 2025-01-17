#lista de pacotes
pacotes <- c("tidyverse", "data.table", "readr", 
             "scales", "readxl", "writexl", "purrr", "classInt",
             "lubridate", "foreign", "ggplot2", "tidyr", "dplyr", "patchwork",
             "sf", "geobr", "sp", "maptools", "rgdal", "gridExtra", "grid", "raster",
             "spatstat", "leaflet", "leaflet.extras")

#iniciar/instalar pacotes
if(sum(as.numeric(!pacotes %in% installed.packages())) != 0){
  instalador <- pacotes[!pacotes %in% installed.packages()]
  for(i in 1:length(instalador)) {
    install.packages(instalador, dependencies = T)
    break()}
  sapply(pacotes, require, character = T)
} else {
  sapply(pacotes, require, character = T)
}

#configurações iniciais
rm(list=ls()) # limpa  o ambiente
gc(T) # limpa memória
Sys.setlocale("LC_ALL","pt-BR.UTF-8")

#insira o caminho da pasta raiz.
setwd("F:/lucas.sanglard/OneDrive - Ministério da Saúde/Área de Trabalho/R_Walter/Shapes/")
getwd() #teste de getwd



# Ler os shapefiles
google_data <- st_read("GOOGLE_MAPS.shp")
arcgis_data <- st_read("ARCGIS.shp")

# Selecionar as colunas de latitude e longitude
google_coords <- google_data %>% dplyr::select(lat, long)
arcgis_coords <- arcgis_data %>% dplyr::select(lat, long)

# Criar o objeto sf a partir dos dados de coordenadas
google_coords_sf <- st_as_sf(google_coords, coords = c("long", "lat"), crs = 4326, remove = FALSE)

# Criar o objeto sf para ArcGIS
arcgis_coords_sf <- st_as_sf(arcgis_coords, coords = c("long", "lat"), crs = 4326, remove = FALSE)





#mapa sem delimitações territoriais
# Renderizar o mapa de calor dinâmico com leaflet
leaflet(data = google_coords_sf) %>%
  addTiles() %>%
  addHeatmap(
    lng = ~long, lat = ~lat, 
    intensity = ~1,  # Todos os pontos têm a mesma intensidade
    blur = 20,       # Controle do desfoque (ajuste conforme necessário)
    max = 0.05,      # Intensidade máxima relativa
    radius = 15      # Tamanho do raio dos pontos
  ) %>%
  setView(
    lng = mean(google_coords$long, na.rm = TRUE), 
    lat = mean(google_coords$lat, na.rm = TRUE), 
    zoom = 10
  )


# Renderizar o mapa de calor dinâmico com legenda para ArcGIS
# Renderizar o mapa de calor dinâmico com leaflet
leaflet(data = arcgis_coords_sf) %>%
  addTiles() %>%
  addHeatmap(
    lng = ~long, lat = ~lat, 
    intensity = ~1,  # Todos os pontos têm a mesma intensidade
    blur = 20,       # Controle do desfoque (ajuste conforme necessário)
    max = 0.05,      # Intensidade máxima relativa
    radius = 15      # Tamanho do raio dos pontos
  ) %>%
  setView(
    lng = mean(arcgis_coords$long, na.rm = TRUE), 
    lat = mean(arcgis_coords$lat, na.rm = TRUE), 
    zoom = 10
  )









#COM DELIMITAÇÃO TERRITORIAL - DISTRITO FEDERAL
# Carregar os limites estaduais do Brasil
estados <- read_state(year = 2020, simplified = TRUE)
setores_df <- st_read("F:/lucas.sanglard/OneDrive - Ministério da Saúde/Área de Trabalho/R_Walter/Shapes/Kernel_R/Setores Censitários DF.shp")

# Criar o objeto sf para Google Maps
google_coords_sf <- st_as_sf(google_coords, coords = c("long", "lat"), crs = 4326, remove = FALSE)

# Renderizar o mapa de calor com o shape de estados/setores ---- GOOOGLE
leaflet(data = google_coords_sf) %>%
  addTiles() %>%
  addHeatmap(
    lng = ~long, lat = ~lat, 
    intensity = ~1,  # Todos os pontos têm a mesma intensidade
    blur = 20,       # Controle do desfoque
    max = 0.05,      # Intensidade máxima relativa
    radius = 15      # Tamanho do raio dos pontos
  ) %>%
  addPolylines(
    data = setores_df, 
    color = "black",  # Cor das linhas
    weight = 1,       # Espessura das linhas
    opacity = 0.8     # Transparência
  ) %>%
  setView(
    lng = mean(google_coords$long, na.rm = TRUE), 
    lat = mean(google_coords$lat, na.rm = TRUE), 
    zoom = 10
  )

# Renderizar o mapa de calor com o shape de estados/setores ---- ARCGIS
leaflet(data = arcgis_coords_sf) %>%
  addTiles() %>%
  addHeatmap(
    lng = ~long, lat = ~lat, 
    intensity = ~1,  # Todos os pontos têm a mesma intensidade
    blur = 20,       # Controle do desfoque
    max = 0.05,      # Intensidade máxima relativa
    radius = 15      # Tamanho do raio dos pontos
  ) %>%
  addPolylines(
    data = setores_df, 
    color = "black",  # Cor das linhas
    weight = 1,       # Espessura das linhas
    opacity = 0.8     # Transparência
  ) %>%
  setView(
    lng = mean(arcgis_coords$long, na.rm = TRUE), 
    lat = mean(arcgis_coords$lat, na.rm = TRUE), 
    zoom = 10
  )










#COM DELIMITAÇÃO TERRITORIAL - PERNAMBUCO
# Carregar os limites estaduais do Brasil
estados <- read_state(year = 2020, simplified = TRUE)
setores_REC <- st_read("F:/lucas.sanglard/OneDrive - Ministério da Saúde/Área de Trabalho/R_Walter/Shapes/Kernel_R/Setores Censitários REC.shp")

# Criar o objeto sf para Google Maps
google_coords_sf <- st_as_sf(google_coords, coords = c("long", "lat"), crs = 4326, remove = FALSE)

# Renderizar o mapa de calor com o shape de estados/setores ---- GOOOGLE
leaflet(data = google_coords_sf) %>%
  addTiles() %>%
  addHeatmap(
    lng = ~long, lat = ~lat, 
    intensity = ~1,  # Todos os pontos têm a mesma intensidade
    blur = 20,       # Controle do desfoque
    max = 0.05,      # Intensidade máxima relativa
    radius = 15      # Tamanho do raio dos pontos
  ) %>%
  addPolylines(
    data = setores_REC, 
    color = "black",  # Cor das linhas
    weight = 1,       # Espessura das linhas
    opacity = 0.8     # Transparência
  ) %>%
  setView(
    lng = mean(google_coords$long, na.rm = TRUE), 
    lat = mean(google_coords$lat, na.rm = TRUE), 
    zoom = 10
  )

# Renderizar o mapa de calor com o shape de estados/setores ---- ARCGIS
leaflet(data = arcgis_coords_sf) %>%
  addTiles() %>%
  addHeatmap(
    lng = ~long, lat = ~lat, 
    intensity = ~1,  # Todos os pontos têm a mesma intensidade
    blur = 20,       # Controle do desfoque
    max = 0.05,      # Intensidade máxima relativa
    radius = 15      # Tamanho do raio dos pontos
  ) %>%
  addPolylines(
    data = setores_REC, 
    color = "black",  # Cor das linhas
    weight = 1,       # Espessura das linhas
    opacity = 0.8     # Transparência
  ) %>%
  setView(
    lng = mean(arcgis_coords$long, na.rm = TRUE), 
    lat = mean(arcgis_coords$lat, na.rm = TRUE), 
    zoom = 10
  )



