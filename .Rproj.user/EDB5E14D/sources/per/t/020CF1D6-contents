library(tidyverse)
library(purrr)
library(readr)
library(dplyr)

folder <- "data"

csv_files <- list.files(folder, pattern = "\\.csv$", full.names = TRUE)

data_list <- map(csv_files, read_csv, show_col_types = FALSE)

to_lower_colnames <- function(csv) {
  colnames(csv) <- tolower(colnames(csv))
  return(csv)
}

pivot_longer_transform <- function(csv) {
 csv %>%
    rename_with(~ gsub("^y", "", .)) %>% 
    pivot_longer(
      cols = matches("^[0-9]{4}$"),  # Especifica el rango de columnas
      names_to = "year",      # Nombre de la columna para los nombres originales de las columnas
      values_to = "temperature_change"     # Nombre de la columna para los valores
    )             
}

data_list<- map(data_list, to_lower_colnames)

head(data_list_long[[200]]) 

data_list_long <- map(data_list, pivot_longer_transform)

colnames(data_list_long[[200]]) #Verificamos si funcionó

data_list_long <- map(data_list_long, ~ {
  .x %>%
    mutate(
      months = match(tolower(months), tolower(month.name)),
      date = as.Date(paste(year, months, "01", sep = "-"))
    )
})



map(data_list_long, ~ class(.x$date))

final_data <- data_list_long %>%
  map_dfr(~ if (all(1961:2019 %in% .x$year)) {
    .x %>%
      select(area, months, year, temperature_change, date)
  } else {
    NULL
  })

final_data <- final_data %>%
  filter(!is.na(date) & !is.na(temperature_change))

class(final_data)

descriptive_stats <- final_data %>%
  summarise(
    count = n(),
    mean = mean(temperature_change, na.rm = TRUE),
    median = median(temperature_change, na.rm = TRUE),
    sd = sd(temperature_change, na.rm = TRUE),
    min = min(temperature_change, na.rm = TRUE),
    max = max(temperature_change, na.rm = TRUE),
    iqr = IQR(temperature_change, na.rm = TRUE)
  )


descriptive_stats

# Promediamos los cambios de temperatura para mejorar la legibilidad de la visualización

aggregated_data <- final_data %>%
  group_by(date) %>%
  summarise(mean_temperatura_change = mean(temperature_change, na.rm = TRUE))

ggplot(aggregated_data, aes(x = date, y = mean_temperatura_change)) +
  geom_line(color = "blue") +
  labs(title = "Cambio de Temperatura media en el tiempo (1960-2019) a nivel global",
       subtitle = "Se observa una clara tendencia al alza desde los inicios de la medición",
       x = "Fecha",
       y = "Media del Cambio de Temperatura",
       caption = "Elaboración propia a partir de Fao 2019") +
  theme_minimal()


