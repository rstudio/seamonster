# This API powers a [slash command](https://api.slack.com/slash-commands) in 
# slack. Instead of registering a different command for each endpoint, the 
# first argument provided to the slash command is the endpoint while the 
# second argument (if necessary) provides additional data to be passed to the
# specified endpoint.

# Packages ----
library(plumber)
library(magrittr)
library(ggplot2)

# Data ----
# Load sample customer data. IRL this would likely be housed in a database
sim_data <- readr::read_rds("plumber/plumber-slack/slack-api/data/sim-data.rds")

#* @apiTitle Slack plumber PoC

# Requests sent from Slack slash commands are sent as url encoded text in the
# postBody of the request. The text of the command is contained in the text
# parameter. Full details of what is included from Slack can be found at
# https://api.slack.com/slash-commands

# req$postBody is either decoded as JSON if it appears to be JSON or it is
# decoded as a standard query string. Fields provided in the post body in
# either format will be passed through as parameters into the function.
# see https://www.rplumber.io/docs/routing-and-input.html#request-body

# This filter is responsible for parsing text, routing to the appropriate
# endpoint, and providing arguments to be consumed by that endpoint

#* Parse the incoming request and route it to the appropriate endpoint
#* @filter route-endpoint
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
#* Log information about the incoming request
#* @filter logger
function(req){
  cat(as.character(Sys.time()), "-", 
      req$REQUEST_METHOD, req$PATH_INFO, "-", 
      req$HTTP_USER_AGENT, "@", req$REMOTE_ADDR, "\n")
  plumber::forward()
}

# unboxedJSON is used b/c that is what Slack expects from the API
#* Return a message containing status details about the customer
#* @serializer unboxedJSON
#* @post /status
function(req) {
  # Debugging - log contents of req env
  # print(ls(req))
  
  # Check req$ARGS and match to customer - if no customer match is found, return
  # an error
  customer_ids <- unique(sim_data$id)
  customer_names <- unique(sim_data$name)
  
  if (!req$ARGS %in% customer_ids & !req$ARGS %in% customer_names) stop("Customer ", req$ARGS, " not found.")
  
  # Mutate request with CUSOTMER_ID and CUSTOMER_NAME
  if (req$ARGS %in% customer_ids) {
    customer_id <- req$ARGS
    customer_data <- dplyr::filter(sim_data, id == customer_id)
    customer_name <- unique(customer_data$name)
  } else {
    customer_name <- req$ARGS
    customer_data <- dplyr::filter(sim_data, name == customer_name)
    customer_id <- unique(customer_data$id)
  }
  
  # Simple heuristics for customer status
  total_customer_calls <- sum(customer_data$calls)
  
  customer_status <- dplyr::case_when(total_customer_calls > 250 ~ "danger",
                                      total_customer_calls > 130 ~ "warning",
                                      TRUE ~ "good")
  
  list(
    # Register the response type - ephemeral indicates the response will only
    # be seen by the user who invoked the slash command.
    response_type = "ephemeral",
    # Attachments is expected to be an array, hence the list within a list
    attachments = list(
      list(
        color = customer_status,
        title = paste0("Status update for ", customer_name),
        fallback = paste0("Status update for ", customer_name, " (", customer_id, ")"),
        # History plot
        image_url = paste0("http://colorado.rstudio.com/rsc/slack-plumber/plot/history/",
                           urltools::url_encode(customer_id)),
        fields = list(
          list(
            title = "Total Calls",
            value = sum(customer_data$calls),
            short = TRUE
          ),
          list(
            title = "DoB",
            value = unique(customer_data$dob),
            short = TRUE
          )
        )
      )
    )
  )
}

#* Plot customer weekly calls
#* @png
#* @get /plot/history/<cust_id>
function(cust_id) {
  # Filter data to customer id provided
  plot_data <- dplyr::filter(sim_data, id == cust_id)
  
  # Debug
  print(str(plot_data))
  
  # Customer name (id)
  customer_name <- paste0(unique(plot_data$name),
                          " (",
                          unique(plot_data$id),
                          ")"
  )
  
  # Debug
  print(customer_name)
  
  # Create plot
  history_plot <- plot_data %>%
    ggplot(aes(x = time, y = calls, col = calls)) +
    ggalt::geom_lollipop(show.legend = FALSE) +
    theme_light() +
    labs(
      title = paste("Weekly calls for", customer_name),
      x = "Week",
      y = "Calls"
    )
  
  print(history_plot)
}
