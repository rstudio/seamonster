
m <- readRDS("model.Rdat")


#' @apiTitle Predict MPG
#' @apiDescription Awesome model for predicting MPG
#' @apiHost colorado.rstudio.com:8000

#' Predict mpg from hypothetical hp using linear model fitted on mtcars
#' @get /mpg
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

# hello everyone 
# p <- plumber::plumb('plumber.R')$run(host = "0.0.0.0", port = 8000)

# Navigate to:
# http://colorado.rstudio.com:8000/__swagger__/
# "Try it out"


# Publish with:
# rsconnect::deployAPI(".", server = 'colorado.rstudio.com',
#                      account = rstudioapi::askForPassword("Enter Connect Username:"))

