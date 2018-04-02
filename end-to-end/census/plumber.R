library(plumber)
library(babynames)
library(purrr)
library(jsonlite)
library(assertthat)

#read in saved model and profile data
m <- readRDS("data/logisticModel.rds")
profiles <- readr::read_csv("data/train.csv")

#' @apiTitle Predict Probability of Income > 50K from Profile
#' @apiDescription Use a logistic model to predict income from profile data (education, age, etc) trained on census data.

#' @get /income
#' @param id:numeric Id of profile
#' @param age:numeric Age to predict
#' @response 200 Returns probability income > 50K
#' @response 500 Bad Inputs
#' @response default Returns probability income > 50K
function(id, age){
  age <- as.numeric(age)
  id <- as.numeric(id)
  assert_that(length(id) == 1)
  assert_that(id > 0 && id < nrow(profiles))
 
  # scoring function 
  profile <-  profiles[id,]
  profile$age_buckets = age_buckets(age)
  predict(m, profile, type = "response")
 
}

# helper function
age_buckets <- function(age) {
  age <- ifelse(age < 17, 17, age)
  age <- ifelse(age > 90, 90, age)
  age <-  cut(age, c(16, 18, 25, 30, 35, 40, 45, 50, 55, 60, 65, 90)) 
  as.character(age)
}


#' @get /profile
#' @param id:numeric Id of profile
#' @response 200 Returns profile for id 
#' @response 500 Bad ID
#' @response default Returns profile for id
function(id) {
  id <- as.numeric(id)
  assert_that(id > 0 && id < nrow(profiles))
  profile <- profiles[id,] 
  opts <- babynames[babynames$sex == substr(profile$gender, 1,1), "name"]
  profile$name <- as.character(opts[id,])
  profile
}
