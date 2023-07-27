setwd("C:/Users/Juan/OneDrive - Camino del inca srl/RStudio")
main_dir <- getwd()
dir.create("clima2020-2023")
weather_dir_path <- main_dir |>
  paste0("/", "clima2020-2023")
setwd(weather_dir_path)

#instalar librerias y cargar
install.packages("devtools")
devtools::install_github("https://github.com/ErikKusch/KrigR")


#librerias

libs <- c(
    "KrigR","tidyr",
    "sf", "giscoR","classInt", 
    "RColorBrewer"
)
#instalar librerias perdidas



installed_libs = libs %in% rownames(installed.packages())
if(any(installed_libs == F)){
  install.packages(libs[installed_libs])
}
install.packages("giscoR")
Sys.setenv(R_REMOTES_NO_ERRORS_FROM_WARNINGS="true")
devtools::install_github("https://github.com/ErikKusch/KrigR")

install.packages("ggplot2")
install.packages("gganimate")
install.packages("gifski")
install.packages("av")
install.packages("ncdf4")
install.packages("tidyverse")
install.packages("lubridate")
installed.packages("dplyr")
install.packages("raster")
install.packages("sp")

# cargar librerias
invisible(lapply(libs,library,character.only =T))
library(giscoR)
library(sf)
library(dplyr)
library(ggplot2)
library(KrigR)
library(gganimate)
library(gifski)
library(av)
library(ncdf4)
library(tidyverse)
library(lubridate)
library(dplyr)
library(raster)
library(sp)
#1 query datos de temperatura

star_date= "2000-01-01"
end_date= "2023-01-01"

arg_sf = giscoR::gisco_get_countries(
  country = "AR",
  resolution = 10
)

arg_sf




#conectando al servicio de copernicus https://cds.climate.copernicus.eu/user/220152
my_api= 220152
my_key = "bddd0400-cc9c-4e7b-9008-5a80ebb5d7a6"

arg_temp <- KrigR::download_ERA(
  Variable = "2m_temperature",
  DataSet = "era5-land",
  DateStart = star_date,
  DateStop = end_date,
  TResolution = "month",
  TStep = 1,
  Dir = weather_dir_path,
  FileName = "arg_2m_temperature",
  Extent = as(arg_sf,"Spatial"),
  API_User = my_api,
  API_Key = my_key
)
arg_temp

arg_temp[["X2000.01.01.00.00.00"]] |>
  as.data.frame(xy = T, na.rm = T) |>
  ggplot() +
  geom_tile(aes(x=x , y=y, fill = X2000.01.01.00.00.00)) +
  coord_sf() +
  scale_fill_viridis_c(option = "plasma") +
  theme_void()

arg_temp2 <- raster("arg_2m_temperature.nc")

# archivo NC para el Dataframe
arg_temp_df <- as.data.frame(
  arg_temp, xy = T, na.rm =T
)

head(arg_temp_df)
print(names(arg_temp_df))


#--------------------
nuevos_nombres <- paste("X", 1:(ncol(arg_temp_df) - 2), sep = "")
columnas_a_mantener <- c("x", "y")
for (i in seq_along(nuevos_nombres)) {
  if (!(names(arg_temp_df)[i + 2] %in% columnas_a_mantener)) {
    names(arg_temp_df)[names(arg_temp_df) == names(arg_temp_df)[i + 2]] <- nuevos_nombres[i]
  }}

arg_temp_df


#convertir de formato corto a largo (extrapolacion o despivotar)
arg_temp_long <- arg_temp_df |>
  tidyr::pivot_longer(
    !c(x, y),
    names_to = "layer",
    values_to = "value"
  )
arg_temp_long
head(arg_temp_long)

#3. obtener fechas
arg_temp_long$ord <- sub("X" ,"", arg_temp_long$layer)

head(arg_temp_long) 

arg_temp_long$ord <- as.numeric(
  as.character(arg_temp_long$ord)
)
arg_temp_long

datum <- seq(
  as.Date(star_date),
  by = "months", length.out = max(
    arg_temp_long$ord
  ) 
)
datum

#crear vector ord
ord <- 1:max(arg_temp_long$ord)
#nuevo data frame ordenando fechas
dates_df <- data.frame(
  ord, datum
)
dates_df
#left join
arg_temp_dates <- arg_temp_long |>
  dplyr::left_join(dates_df, "ord") |>
  dplyr::mutate(celsius = value - 273.15) |>
  dplyr::select( -layer, -ord,-value)



arg_temp_dates
maximo = max(arg_temp_dates$celsius)
print(maximo)
# Crear una nueva columna "trimestre" y año que represente el trimestre (1 a 4) y año para cada fecha
datosporTrimestre <- arg_temp_dates %>%
  mutate(trimestre_y_año = paste(quarter(datum), year(datum), sep = "-"))
datosporTrimestre


write.csv(datosporTrimestre, file = "Arg_Temp_Final.csv", row.names = FALSE)

summarise(datosporTrimestre)


# Calcular el promedio por año y trimestre utilizando group_by y summarise

nuevo_dataset_resumen <- arg_temp_dates %>%
  group_by(año = year(datum), trimestre = quarter(datum)) %>% # Agrupar por año y trimestre
  summarise(promedio_trimestral = mean(celsius))              # Calcular el promedio trimestral

# Mostrar el nuevo dataset con el promedio trimestral por año
print(nuevo_dataset_resumen)

write.csv(nuevo_dataset_resumen, file = "temp2000-2023.csv", row.names = FALSE)
datos_agrupados <- read.csv("temp2000-2023.csv")

# Mostrar el contenido del data frame
print(datos_agrupados)


#4.breaks
vmin = min(arg_temp_dates$celsius)
vmax = max(arg_temp_dates$celsius)

breaks <- classInt::classIntervals(
  arg_temp_dates$celsius,
  n = 20,
  style = "pretty"
  )$brks
breaks

#5. colores
cols = colorRampPalette(rev(RColorBrewer::brewer.pal(
  11,"Spectral"
)))

#6. mapa
crsLAEA <-
  "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +datum=WGS84 +units=m +no_defs"

coordenadas <-
  "+proj=longlat +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +datum=WGS84 +units=m +no_defs"

proyeccion_wgs84 <- st_crs("+proj=longlat +datum=WGS84 +no_defs")

# Imprimir la información de la proyección WGS 84
print(proyeccion_wgs84)


arg_mapa <- ggplot(datosporTrimestre) +
  geom_tile(aes(x = x, y = y, fill = celsius)) +
  facet_wrap(~datum) +
  scale_fill_gradientn(
    name = "Celsius degree",
    colours = cols(15),
    limits = c(vmin, vmax),
    breaks = breaks
  ) +
  coord_sf(crs = proyeccion_wgs84) +
  guides(
    fill = guide_legend(
      direction = "horizontal",
      keyheight = unit(1.5, units = "mm"),
      keywidth = unit(10, units = "mm"),
      title.position = "top",
      label.position = "bottom",
      title.hjust = .5,
      label.hjust = .5,
      nrow = 1,
      byrow = T
    )
  ) +
  theme_minimal() +
  theme(
    axis.line = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    legend.position = "top",
    legend.title = element_text(
      size = 11, color = "grey10"
    ),
    legend.text = element_text(
      size = 10, color = "grey10"
    ),
    plot.title = element_text(
      size = 20, color = "grey10",
      hjust = .5, vjust = -3
    ),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.margin = unit(
      c(t = 1, r = 0, l = 0, b = 0), "lines"
    )
  ) +
  labs(
    x = "",
    y = "",
    title = "Temperatura Mensual en Argentina"
  )


print(arg_mapa)             


# 7. ANIMACION
#-----------

arg_mapa <- ggplot(arg_temp_dates) +
  geom_tile(aes(x = x, y = y, fill = celsius)) +
  scale_fill_gradientn(
    name = "Grados Celsius",
    colours = cols(15),
    limits = c(vmin, vmax),
    breaks = breaks
  ) +
  coord_sf(crs = proyeccion_wgs84) +
  guides(
    fill = guide_legend(
      direction = "horizontal",
      keyheight = unit(1.5, units = "mm"),
      keywidth = unit(10, units = "mm"),
      title.position = "top",
      label.position = "bottom",
      title.hjust = .5,
      label.hjust = .5,
      nrow = 1,
      byrow = T
    )
  ) +
  theme_minimal() +
  theme(
    axis.line = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    legend.position = "bottom",
    legend.title = element_text(
      size = 11, color = "grey10"
    ),
    legend.text = element_text(
      size = 10, color = "grey10"
    ),
    plot.title = element_text(
      size = 20, color = "grey10",
      hjust = .5, vjust = -3
    ),
    plot.subtitle = element_text(
      size = 40, color = "#c43c4e",
      hjust = .5, vjust = -1
    ),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.margin = unit(
      c(t = 1, r = 0, l = 0, b = 0), "lines"
    )
  ) +
  labs(
    x = "",
    y = "",
    title = "Temperatura Mensual en Argentina",
    subtitle = "{as.Date(frame_time)}"
  )
arg_mapa
timelapse_arg_map <- arg_mapa +
  transition_time(time = as.Date(datum)) +
  enter_fade() +
  exit_fade() +
  ease_aes("linear", interval = .2)

timelapse_arg_map


animated_temp_map <- gganimate::animate(
  timelapse_arg_map,
  nframes = 65,
  duration = 20,
  start_pause = 3,
  end_pause = 30,
  height = 7, #ver si el problema esta acá y en whidth
  width = 7,
  res = 300,
  fps = 15,
  renderer = gifski_renderer(loop = T)
)
gganimate::anim_save(
  "argentina_temperatura.gif", animated_temp_map
)     

timelapse_arg_map
