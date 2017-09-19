# # Training -------
# # This section of code trains a model m
# # The API assumes the model has already been fit and simply loads the model
# # and uses the predict function
# # Note that for this simple linear model, we could do the prediction easily with math
# 
# 
# library(tidyverse)
# library(mice)
# load("plumber/data/sdata.Rdat")
# 
# vehicle_df <- tibble(
#   name = map_chr(vehicles, "name"),
#   speed = map_chr(vehicles, "max_atmosphering_speed") %>% parse_number(),
#   crew = map_chr(vehicles, "crew") %>% parse_number(),
#   cost = map_chr(vehicles, "cost_in_credits") %>% parse_number(),
#   passengers = map_chr(vehicles, "passengers") %>% parse_number(),
#   cargo = map_chr(vehicles, "cargo_capacity") %>% parse_number(),
#   length = map_chr(vehicles, "length") %>% parse_number()
# )
# 
# # impute the missing values
# imputed <- vehicle_df %>%
#   select(-name) %>%
#   mice(seed = 500) %>%
#   complete(5)
# 
# # fit the linear model against the imputed dataset
# m <- lm(speed ~ ., imputed)
# 
# # save the averages for default values later
# avg <- colMeans(imputed)
# saveRDS(avg, file = "plumber/data/ship_avg.Rds")
# 
# # save the model for later use
# saveRDS(m, file = "plumber/data/model.Rds")


# API ----


#* @apiTitle Ship Speed Predictor
#* @apiDescription Predict the speed of star wars 
#*    spaceships with a model trained on SWAPI data

library(assertthat)
library(purrr)
library(readr)

m <- readRDS("data/model.Rds")
avg <- readRDS("data/ship_avg.Rds")
avg_field <- names(avg)
n_pred <- length(coef(m)) - 1

is_numberish <- function(vals) {
  classes <- map_chr(vals, guess_parser)
  tot_numeric <- sum(grepl(pattern = "double|integer", x = classes))
  tot_numeric == length(classes)
}

on_failure(is_numberish) <- function(call, env) {
  "Arguments contain non-numeric data!"
}

get_score <- function(vals) {
  sum(coef(m)*c(1, as.numeric(vals)))
}


#* Get predicted speed based on ship attributes
#* @param crew:int number of personnel needed to run or pilot this vehicle
#* @param cost:numeric cost of this vehicle new, in Galactic Credits
#* @param passengers:int number of non-essential people this vehicle can transport
#* @param cargo:numeric maximum number of kilograms that this vehicle can transport
#* @param length:numeric length of the ship in meters
#* @get /score-ship-named
#* @response 200 predicted speed of the ship

function(crew = avg[avg_field == "crew"],
         cost = avg[avg_field == "cost"],
         passengers = avg[avg_field == "passengers"],
         cargo =  avg[avg_field == "cargo"],
         length = avg[avg_field == "length"],
         req) {
  
  # check that inputs can be coereced to numeric
  inputs <- list(crew, cost, passengers, cargo, length)
  assert_that(is_numberish(inputs))

  # return the score
  get_score(inputs)
}

# Publish to RStudio Connect
# install.packages("plumber")
# r <- plumber::plumb("plumber/plumber.R")
# r$run(port=8000)
# rsconnect::deployAPI(api = "plumber")

