library(plumber)

#* @apiTitle Sample Plumber

#* Return data summaries of provided data
#* @post /data
function(req, res) {
  
  # If req$postBody isn't valid JSON or can't be serialized, return an appropriate
  # http error
  # TODO: is there a better way to do this?
  try(
    raw_data <- jsonlite::fromJSON(req$postBody),
    silent = TRUE
  )
  
  if (!exists("raw_data")) {
    res$status <- 400
    return(list(error = jsonlite::unbox("Data was not in valid JSON format")))
  }
  
  # Log details
  print(raw_data)
  print(str(raw_data))
  print(class(raw_data))
  
  head(raw_data, 3)
}
