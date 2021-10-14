get_species_pres_abs <- function(.x, .species.name, .threshold){
  x <- .x$filtered.counts[[1]]
  species.name <- .species.name
  threshold <- .threshold
  sample.id <- x$sample.id
  x <- x[,-1]
  row.sums <- rowSums(x)
  proportions <- x/ row.sums
  spec.list <- names(proportions)
  if(any(spec.list %in% species.name)) {
    species <-proportions %>%  dplyr::select(all_of(species.name))
    pres_abs <- species
    pres_abs[species >= threshold] <- 1
    pres_abs[species < threshold] <- 0
  } else {
    pres_abs <- data.frame(spec = rep(0, length(row.sums)))
    colnames(pres_abs) <- species.name
  }
  pres_abs
}


calc_year_pres_abs <- function(.x, .species.name,  .year){
  species.name <- .species.name
  year <- .year
  
  pres_abs_spec <- .x$pres.abs[[1]]
  age <- .x$list_ages[[1]]$ages
  
  spec_data <- bind_cols(age, pres_abs_spec) %>% 
    dplyr::select(age,species.name) %>% 
    # minimum_temperature, mean_temperature, precipitation,  relative_humidity, sea_level_pressure, specific_humidity, ) %>% 
    mutate(age.diff = (age - year)^2) %>% 
    filter(age.diff == min(age.diff)) %>% 
    dplyr::select(-age.diff) %>% 
    mutate(age = year)
  spec_data
}


prep_spec_data <- function(NA_pollen,
                           .species.name = "Tsuga", 
                           .threshold = 0.05, 
                           .year = 6000  ){
  # Get the presence absence data
  pres_abs_data <- NA_pollen %>% 
    group_by(dataset.id) %>% 
    nest() %>% 
    mutate(.species.name = .species.name, .threshold = .threshold) %>% 
    mutate(pres.abs = pmap(.l = list(data, .species.name, .threshold), 
                           .f = get_species_pres_abs)) %>% 
    dplyr::select(dataset.id, pres.abs) %>% 
    left_join(NA_pollen) %>% 
    group_by(dataset.id) %>% 
    nest()
  
  dataToPlot <- pres_abs_data %>%
    mutate(.species.name = .species.name, .year = .year ) %>%
    mutate(age_pres_abs = pmap(.l = list(data, .species.name, .year),
                               .f = calc_year_pres_abs)) %>%
    dplyr::select(dataset.id, age_pres_abs) %>%
    unnest(cols = c(age_pres_abs)) %>%
    left_join(NA_pollen) %>%
    dplyr::select(dataset.id, lat, long, elev,  age, all_of(.species.name))
  # minimum_temperature, mean_temperature, precipitation, relative_humidity, sea_level_pressure, specific_humidity)
  save(dataToPlot,
       file= paste0("output/dataToPlot_",.year,"_",.species.name, "_", .threshold, ".RData"))
  dataToPlot
  }

map_species <- function(.data = tsuga_6k, .var= "Tsuga", .title = "Tsuga Presence XXyr"){
  theme_set(theme_bw())
  world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")
  ggplot(data = world) +
    geom_sf() +
    xlab("Longitude") + ylab("Latitude") +
    coord_sf(xlim = c(-130, -60), ylim = c(20, 75), expand = FALSE) +
    geom_point(data = .data, aes(x = long, y = lat, col = eval(parse(text =.var)))) +
    labs(col = .var)+
    theme(legend.title = element_text(size= 10)) +
    ggtitle(.title)
  
}



####  map_species()  #### from Arnaud
# {
#   # the following function will be a lot faster than using quartz and ggplot. It also has the avantage to plot interactive map
#   
#   # install.packages("tmap")
#   # library(tmap)
#   
#   map_species <- function(.data, .var = "Tsuga") {
#     if (missing(.data)) {
#       stop('argument ".data" is missing with no default')
#     }
#     tmap_mode("view")
#     proj <- 4326
#     .data %>%
#       drop_na() %>%
#       st_as_sf(coords = c("long", "lat"), crs = proj) %>% {
#         tm_shape(filter(World, continent == "North America"), projection = proj) +
#           tm_format("World", inner.margins = 0) +
#           tm_fill(alpha = 0) +
#           tm_shape(.) +
#           tm_dots(size = .05, col = .var, style = "cont")
#       }
#   }
# }
# 



get_climate_data <- function(.data = tsuga_6k, .year = "6000"){
  tmin <- raster(x = paste0("data/CCSM/",.year, "BP/an_avg_TMIN.tif"))
  an_sum_AET <-raster(x = paste0("data/CCSM/",.year, "BP/an_sum_AET.tif"))
  an_sum_PRCP <-raster(x = paste0("data/CCSM/",.year, "BP/an_sum_PRCP.tif"))
  an_sum_GDD5 <-raster(x = paste0("data/CCSM/",.year, "BP/an_sum_GDD5.tif"))
  
  # create Raster stack
  climateStack <- stack(tmin, an_sum_AET, an_sum_PRCP, an_sum_GDD5)
  xy <- .data[,c("long", "lat")]
  sp_long_lat <- SpatialPoints(coords = xy, proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
  climateData <- raster::extract( climateStack, sp_long_lat ) %>% 
    as_tibble()
  
  # Combine with species data
  .data <- bind_cols(.data, climateData )
  return(.data)
}



fit.model <- function(.data =tsuga_4k_clim) {
  
  # First create a training dataset (75% of your data to fit model)
  length.data <- nrow(.data)
  length.training.set <- round(0.66*length.data)
  
  random.sample <- sample(1:length.data, size = length.training.set, replace= FALSE)
  training.data <- .data[random.sample,]
  validation.data <- .data[-random.sample,]
  
  model_glm <- glm(Tsuga ~ I(an_sum_GDD5^2) + an_sum_GDD5 
                   + I(an_sum_PRCP^2) + an_sum_PRCP, 
                   family= binomial(link= "logit") ,
                   data = training.data)
  # summary(model_glm)
  
  # GEt the model estimates of tsuga presence
  training.data$predicted_glm <- predict(model_glm, type = "response")
  training.data  <- training.data %>%
    mutate(predicted_pres_glm = 0) %>% 
    mutate(predicted_pres_glm =replace(predicted_pres_glm, predicted_glm >0.5, 1))
  
  # Now you can predict the values of the samples in the validation data
  validation.data$predicted_glm <- predict(model_glm, type = "response", 
                                           newdata =validation.data )
  validation.data  <- validation.data %>%
    mutate(predicted_pres_glm = 0) %>% 
    mutate(predicted_pres_glm =replace(predicted_pres_glm, predicted_glm >0.5, 1))
  
  return(list(training.data = training.data, validation.data= validation.data, model_glm = model_glm ))
}






calc_response_functions <- function(.data = tsuga_4k_clim, .model = model_glm_6ky ){
  
  # Create a varible holding precipitation/ GDD constant (at the mean value) and let GDD vary within the range seen in the data
  meanGDD <- mean(.data$an_sum_GDD5)
  minGDD <- min(.data$an_sum_GDD5)
  maxGDD <- max(.data$an_sum_GDD5)
  
  meanPRCP <- mean(.data$an_sum_PRCP)
  minPRCP <- min(.data$an_sum_PRCP)
  maxPRCP <- max(.data$an_sum_PRCP)
  
  # Calculate the Response functions for GDD
  new.data.GDD <- data.frame(an_sum_GDD5  = seq(from = minGDD, to = maxGDD, length.out = 200), 
                             an_sum_PRCP = rep(meanPRCP, length = 200))
  predict_GDD <- predict(.model, type = "response", newdata= new.data.GDD )
  
  # Calculate the Response functions for precipitation
  new.data.PRCP <- data.frame(an_sum_GDD5 = rep(meanGDD, length = 200),
                              an_sum_PRCP  = seq(from = minPRCP, to = maxPRCP, length.out = 200) )
  predict_PRCP <- predict(.model, type = "response", newdata= new.data.PRCP )
  
  # Plot the response functions
  par(mfrow = c(2,2))
  plot(new.data.GDD$an_sum_GDD5, predict_GDD, type = "l", col = "red", xlab = "Annual Sum Growing Degree Days", ylab = "Model Probability")
  plot(new.data.PRCP$an_sum_PRCP, predict_PRCP, type = "l", col = "blue", xlab = "Annual Sum Precipiation", ylab = "Model Probability")
}



