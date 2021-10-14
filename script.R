library("tidyverse")
library("ggplot2")
library("rnaturalearth")
library("rnaturalearthdata")
library("sf")
library("raster")
library("caret")
library("rgeos")
# install.packages("rgdal")

source("function_script.R")

# Load data

NA_pollen <- readRDS("data/NA_pollen.RDS") %>% 
  filter(long > -95 & between(lat, 35, 55) & max.age > 5500) %>% 
  dplyr::select(-ref)

names(NA_pollen)
# First thing to do is extract the data for hemlock for a given time period.
# Determine a threshold for presence data (0.05) and then plot. 
tsuga_6k <- prep_spec_data(NA_pollen,
               .species.name = "Tsuga", 
               .threshold = 0.05, 
               .year = 6000 )

## .etc

### Mapping the data
quartz()
map_species(tsuga_6k, .var= "Tsuga", .title = "Tsuga Presence 6 kyr")


#### Getting the climate data
tsuga_6k_clim <- get_climate_data(.data = tsuga_6k, .year = "6000") %>% drop_na()

##. etc

# Fitting a model to test for the relationship between hemlock presence and temperature/ precipitation
# See the function: fit.model() 
model_results_6ky <- fit.model(.data =tsuga_6k_clim)

# map the model predictions
quartz()
map_species(model_results_6ky$training.data, .var = "predicted_pres_glm", .title = "Tsuga Presence 6 kyr Model Predictions")

# inspect the model using summary
model_glm_6ky <- model_results_6ky$model_glm
summary(model_glm_6ky)

### Use the Kappa statistic to evaluate the predictions of the model against the actual observed values (higher is better)
kappa <- caret::confusionMatrix(as.factor(model_results_6ky$validation.data$Tsuga),
                       as.factor(model_results_6ky$validation.data$predicted_pres_glm))$overall[2]

# It is also possible to inspect the response functions of the two variables
# See the function: calc_response_functions
calc_response_functions(.data = tsuga_6k_clim, .model = model_glm_6ky )


