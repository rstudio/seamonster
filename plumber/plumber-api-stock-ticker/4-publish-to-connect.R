

# To publish to RStudio Connect -------------------------------------------

rsconnect::deployAPI(
  api = ".", 
  server = 'colorado.rstudio.com',
  account = rstudioapi::askForPassword("Enter RSudio Connect username:")
)

# Navigate to:
# http://colorado.rstudio.com:8000/__swagger__/
# "Try it out"
