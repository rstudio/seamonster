# This API powers a [slash command](https://api.slack.com/slash-commands) in 
# slack. Instead of registering a different command for each endpoint, the 
# first argument provided to the slash command is the endpoint while the 
# second argument (if necessary) provides additional data to be passed to the
# specified endpoint.

# Packages ----
library(plumber)
library(magrittr)

# Data ----
# Load sample customer data. IRL this would likely be housed in a database



#* @apiTitle Slack plumber PoC

# Requests sent from Slack slash commands are sent as url encoded text in the
# postBody of the request. The text of the command is contained in the text
# parameter. Full details of what is included from Slack can be found at
# https://api.slack.com/slash-commands
#* Parse the incoming request and route it to the appropriate endpoint
#* @filter parse-endpoint
function(req, text = "") {
  # Identify endpoint
  split_text <- urltools::url_decode(text) %>%
    strsplit(" ") %>%
    unlist()
  
  if (length(split_text) >= 1) {
    endpoint <- split_text[[1]]
    
    # Modify request with updated endpoint and strip endpoint from text
    req$PATH_INFO <- paste0("/", endpoint)
    
    # Modify request with remaining commands from text
    req$ARGS <- split_text[-1] %>% 
      paste0(collapse = " ")
  } 
  
  # Forward request 
  forward()
}

# Lifted from https://www.rplumber.io/docs/routing-and-input.html#filters
#* Log some information about the incoming request
#* @filter logger
function(req){
  cat(as.character(Sys.time()), "-", 
      req$REQUEST_METHOD, req$PATH_INFO, "-", 
      req$HTTP_USER_AGENT, "@", req$REMOTE_ADDR, "\n")
  plumber::forward()
}

#* Echo back the input from a slack command
#* @post /echo
function(req, text) {
  # Debugging - log contents of req env
  # print(ls(req))
  
  # req$postBody is either decoded as JSON if it appears to be JSON or it is
  # decoded as a standard query string. Fields provided in the post body in
  # either format will be passed through as parameters into the function.
  # see https://www.rplumber.io/docs/routing-and-input.html#request-body
  list(
    # Register the response type - ephemeral indicates the response will only
    # be seen by the user who invoked the slash command.
    response_type = jsonlite::unbox("ephemeral"),
    orig_text = jsonlite::unbox(paste0("The message is: ", text)),
    text = jsonlite::unbox(paste0("The message is: ", req$ARGS)),
    attachments = list(
      list(
        image_url = jsonlite::unbox("http://colorado.rstudio.com/rsc/slack-plumber/plot")
      )
    )
  )
}

#* Plot a histogram
#* @png
#* @get /plot
function() {
    rand <- rnorm(100)
    hist(rand)
}
