
m <- readRDS("model.Rdat")


#' @apiTitle Predict MPG
#' @apiDescription Awesome model for predicting MPG

#' Predict mpg from hypothetical hp using linear model fitted on mtcars
#' @get /
#' @param new_hp:numeric Horsepower of hypothetical car
#' @response 200 Predicted MPG for supplied HP value
#' @response 400 Bad HP value
#' @respomnse default Predicted MPG for HP of 300
function(new_hp = 300) {
  new_hp = as.numeric(new_hp)
  predict(m, data.frame(hp = new_hp))  
}

# For a demo:
# -----------

# rstudioapi::viewer("http://127.0.0.1:8000/__swagger__/")
# p <- plumber::plumb('plumber.R')$run(host = "0.0.0.0")

# Refresh the viewer by pressing the refresh arrow!

# "Try it out" doesn't work right now. Do:
# http://colorado.rstudio.com:8000/?new_hp=200
# you can change the 200 to whatever number

# Publish with:
# rsconnect::deployAPI(".", server = 'colorado.rstudio.com', 
#                      account = rstudioapi::askForPassword("Enter Connect Username:"))

