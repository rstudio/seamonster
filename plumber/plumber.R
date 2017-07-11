
m <- readRDS("model.Rdat")


#' @apiTitle Predict MPG
#' @apiDescription Awesome model for predicting MPG

#' Predict mpg from hypothetical hp using linear model fitted on mtcars
#' @get /
#' @param hp:numeric Horsepower of hypothetical car
#' @response 200 Predicted MPG for supplied HP value
#' @response 400 Bad HP value
#' @respomnse default Predicted MPG for HP of 300
function(new_hp = 300) {
  new_hp = as.numeric(new_hp)
  predict(m, data.frame(hp = new_hp))  
}

# For a demo:
# -----------


# p <- plumber::plumb('plumber.R')
# p$run(swagger = TRUE)

# Navigate to:
# localhost:8000/__swagger__/

# Publish with:
# rsconnect::deployAPI(".", server = 'colorado.rstudio.com', 
#                      account = rstudioapi::askForPassword("Enter Connect Username:"))

