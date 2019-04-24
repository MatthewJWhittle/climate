rm(list = ls())

library(tidyverse)
library(lubridate)



temperature_data <- read_csv("data/raw/GlobalLandTemperaturesByCity.csv")
# https://www.co2.earth/historical-co2-datasets
co2_data <- read_csv("data/raw/co2.csv")

temperature_data <- 
  temperature_data %>%
  rename(
    date = dt,
    average_temperature = AverageTemperature,
    average_temperature_uncertainty = AverageTemperatureUncertainty
  ) %>%
  (function(x) {
    colnames(x) <- colnames(x) %>% str_to_lower()
    
    return(x)
    
  })



co2_data <- 
  co2_data %>% 
    mutate(date = str_extract(datetime, "[0-9]{2}[A-Za-z]+[0-9]{4}"),
      datetime = dmy_hms(datetime),
           date = date(datetime))

# Explore -------
str(temperature_data)


sample_df <- function(x, n){
  
  row_numbers <- c(1:nrow(x))
  
  index <- sample(row_numbers, n)
  
  return(x[index,])
  
}





temperature_data %>%
  group_by(date) %>%
  summarise(global_temp = mean(average_temperature, na.rm = T)) %>%
  mutate(month = factor(month(date, label = T)),
         year = year(date)) %>%
  group_by(year) %>%
  mutate(annual_max = max(global_temp),
         annual_mean = mean(global_temp)) %>%
 # sample_df(n = 50000) %>%
  ggplot(aes(x = date, y = global_temp)) + 
  geom_point(alpha = 0.4, shape = 21, aes(colour = month)) + 
  geom_line(aes(y = annual_max)) +
  geom_line(aes(y = annual_mean)) + 
  theme_minimal()



co2_data %>%
  ggplot(aes(date, data_mean_global)) + geom_point()
