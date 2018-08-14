#' ---
#' title: Plumber and the Slack API
#' author: James Blair
#' ---
 
#+ setup, include=FALSE
knitr::opts_chunk$set(eval = FALSE)

#' This asset shows how [`plumber`](https://www.rplumber.io) can be used to 
#' build a [Slack slash command](https://api.slack.com/slash-commands). The API 
#' is built on top of a [simulated customer dataset](customer-data-sim.Rmd) that 
#' contains details about customer call history. The slash command provides 
#' access to customer status report as well as customer success rep reports 
#' directly from within Slack. The goal of this integration is to highlight the 
#' strengths of `plumber` and how it can be used to *reliably and securely 
#' integrate R with other products and services*.
#'
#' ![](images/slash-command-preview.png)
#' 
#' The API for this command is hosted on the [colorado demo server](http://colorado.rstudio.com/rsc/connect/#/apps/1292/access).
#' 
#' ## Usage
#' Instead of registering a different command for each endpoint, the first
#' argument provided to the slash command is the endpoint while the subsequent
#' argument(s) (if necessary) provide additional data to be passed to the
#' specified endpoint. This way, a single slash command serves multiple endpoints
#' without polluting the slash command namespace.
#' 
#' To access a customer status report, enter `/cs status <id>` in Slack, where
#' `id` is a valid customer ID from the simulated data. The customer status report
#' includes the customer name, total calls, date of birth, and a plot call totals
#' for the last 20 weeks. The color of the message is an indication of customer
#' health. Green indicates the customer has no issues while red indicates the
#' customer has a high volume of calls, indicating a potential problem.
#' 
#' Help for all available commands can be accessed by entering `/cs help` or
#' simplly `/cs` into Slack.
#' 
#' ---
#' 
#' ## Getting Started
#' In order to build a Slack app, you must have a Slack account and [follow the
#' directions](https://api.slack.com) for creating a Slack app. The app will be
#' tied to a specific workspace, so select a Slack workspace you anticipate
#' belonging to long term. By default, your app will only be available to this
#' workspace, although it's possible to expand access to the app later on.
#' 
#' Once the app has been created in Slack, create a new slash command through
#' which the end user will interact with the app.
#' 
#' ![](images/slash-command-creation.png)
#' 
#' Specific details for building slash commands can be found [here](https://api.slack.com/slash-commands).
#' This will be a helpful reference through the remainder of the walk through.
#' 
#' ## Plumbing the API
#' If you haven't already, install the `plumber` package via `install.packages
#' ("plumber")`. The [`plumber.R`](plumber.R) file uses `plumber` to define all
#' of the API filters and endpoints leveraged by the Slack app. Here, we'll go
#' through each piece of the API to describe the code and introduce helpful
#' resources. 
#' 
#' ### Setup
#+ api-setup
# Packages ----
library(plumber)
library(magrittr)
# Necessary workaround for RSC publish issues
library(dichromat)
library(ggplot2)

# Data ----
# Load sample customer data. IRL this would likely be housed in a database.
sim_data <- readr::read_rds("data/sim-data.rds")

# Config options ----
# Base URL for API requests
base_url <- config::get("base_url")

# Utils ----
slack_auth <- function(req) {
  # Verify request came from Slack ----
  if (is.null(req$HTTP_X_SLACK_REQUEST_TIMESTAMP)) {
    return("401")
  }
  
  base_string <- paste(
    "v0",
    req$HTTP_X_SLACK_REQUEST_TIMESTAMP,
    req$postBody,
    sep = ":"
  )
  
  # Slack Signing secret is available as environment variable
  # SLACK_SIGNING_SECRET
  computed_request_signature <- paste0(
    "v0=",
    openssl::sha256(base_string, Sys.getenv("SLACK_SIGNING_SECRET"))
  )
  
  # If the computed request signature doesn't match the signature provided in the
  # request, return an error
  if (!identical(req$HTTP_X_SLACK_SIGNATURE, computed_request_signature)) {
    "401"
  } else {
    "200"
  }
}

#* @apiTitle CS Slack Application API
#* @apiDescription API that interfaces with Slack slash command /cs

#+ comments-1, include=FALSE
#' Here we setup the environment for the API by loading the appropriate packages
#' and loading the simulated data. The [`config`](https://github.com/rstudio/config) 
#' package is used to store parameters that change based on the location of the
#' API (if it's local or deployed on RStudio Connect). `slack_auth()` is a helper
#' function that is used to confirm that incoming requests are indeed coming from
#' Slack and not an unauthorized source. Details about authenticating Slack requests
#' can be found in [Slack's documentation](https://api.slack.com/docs/verifying-requests-from-slack).

# Requests sent from Slack slash commands are sent as url encoded text in the
# postBody of the request. The text of the command is contained in the text
# field. Full details of what is sent from Slack can be found at
# https://api.slack.com/slash-commands

# req$postBody is either decoded as JSON if it appears to be JSON or it is
# decoded as a standard query string. Fields provided in the post body in
# either format will be matched to function parameters.
# see https://www.rplumber.io/docs/routing-and-input.html#request-body

# Slack uses the notion of signed secrets in order to verify that the request
# was actually made from Slack. This API checks the signed secret against
# a self computed signed secret to ensure they match. If not, an error is
# returned. Detailed instructions for verifying requests can be found
# at https://api.slack.com/docs/verifying-requests-from-slack
# TODO: Is it possible to reject requests from bad actors based on a history
# of incorrectly signed requests? Or is just rejecting the incorrectly signed
# request enough?
# Authentication happens at the endpoint level so that each endpoint MAY have
# it's own method of authentication.

# This filter is responsible for parsing text, routing to the appropriate
# endpoint, and providing arguments to be consumed by that endpoint

#' ### `@filter route-endpoint`
#+ filter-route-endpoint
#* Parse the incoming request and route it to the appropriate endpoint
#* @filter route-endpoint
function(req, text = "") {
  # Identify endpoint
  split_text <- urltools::url_decode(text) %>%
    strsplit(" ") %>%
    unlist()
  
  if (length(split_text) >= 1) {
    endpoint <- split_text[[1]]
    
    # Modify request with updated endpoint
    req$PATH_INFO <- paste0("/", endpoint)
    
    # Modify request with remaining commands from text
    req$ARGS <- split_text[-1] %>% 
      paste0(collapse = " ")
  }
  
  if (req$PATH_INFO == "/" & slack_auth(req) == "200") {
    # If no endpoint is provided (PATH_INFO is just "/") then forward to /help
    req$PATH_INFO <- "/help"
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
  
  # Forward request
  forward()
}

#* Help for /cs command
#* @serializer unboxedJSON
#* @post /help
function(req, res) {
  # Authorize request
  status <- slack_auth(req)
  if (status == "401") {
    res$status <- 401
    return(
      list(
        text = "Error: Invalid request."
      )
    )
  }
  
  list(
    # response type - ephemeral indicates the response will only be seen by the
    # user who invoked the slash command as opposed to the entire channel
    response_type = "ephemeral",
    # attachments is expected to be an array, hence the list within a list
    attachments = list(
      list(
        title = "/cs help",
        fallback = "/cs help",
        fields = list(
          list(
            title = "/cs status customer_id",
            value = "Customer summary",
            short = TRUE
          ),
          list(
            title = "/cs rep rep_name",
            value = "CS rep summary",
            short = TRUE
          ),
          list(
            title = "/cs region region_name",
            value = "Region report",
            short = TRUE
          )
        )
      )
    )
  )
}

# unboxedJSON is used b/c that is what Slack expects from the API
#* Return a message containing status details about the customer
#* @serializer unboxedJSON
#* @post /status
function(req, res) {
  # Authenticate request
  status <- slack_auth(req)
  if (status == "401") {
    res$status <- 401
    return(
      list(text = "Error: Invalid request.")
    )
  }
  
  # Check req$ARGS and match to customer - if no customer match is found, return
  # an error
  
  # TODO: Provide suggestions for names that are close in the event of mispellings
  
  customer_ids <- unique(sim_data$id)
  customer_names <- unique(sim_data$name)
  
  if (!req$ARGS %in% customer_ids & !req$ARGS %in% customer_names) {
    res$status <- 400
    return(
      list(
        response_type = "ephemeral",
        text = paste("Error: No customer found matching", req$ARGS)
      )
    )
  }
  
  # Filter data to customer data based on provided id / name
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
  
  # Build response
  list(
    # response type - ephemeral indicates the response will only be seen by the
    # user who invoked the slash command as opposed to the entire channel
    response_type = "ephemeral",
    # attachments is expected to be an array, hence the list within a list
    attachments = list(
      list(
        color = customer_status,
        title = paste0("Status update for ", customer_name, " (", customer_id, ")"),
        fallback = paste0("Status update for ", customer_name, " (", customer_id, ")"),
        # History plot
        # TODO: Can this be made aware of where this is deployed? Is there a way
        # to internally reference another endpoint?
        image_url = paste0(base_url, "/plot/history/", customer_id),
        # Fields provide a way of communicating semi-tabular data in Slack
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
#* @response 400 No customer with the given ID was found.
#* @get /plot/history/<cust_id>
function(cust_id, res) {
  # TODO: How to authenticate this endpoint / lock it down to requests from
  # Slack only?
  
  # Throw error if cust_id doesn't exist in data
  if (!cust_id %in% sim_data$id) {
    res$status <- 400
    stop("Customer id" , cust_id, " not found.")
  }
  
  # Filter data to customer id provided
  plot_data <- dplyr::filter(sim_data, id == cust_id)
  
  # Customer name (id)
  customer_name <- paste0(unique(plot_data$name), " (", unique(plot_data$id), ")")
  
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
  
  # print() is necessary to render plot properly
  print(history_plot)
}

#* Get summary of rep performance
#* @serializer unboxedJSON
#* @post /rep
function(req, res) {
  # Authenticate request
  status <- slack_auth(req)
  if (status == "401") {
    res$status <- 401
    return(
      list(text = "Error: Invalid request.")
    )
  }
  
  # Check to ensure rep exists in data
  if (!req$ARGS %in% unique(sim_data$rep)) {
    return(
      list(
        response_type = "ephemeral",
        text = paste("Error: No rep found matching", req$ARGS)
      )
    )
  }
  
  rep_data <- dplyr::filter(sim_data, rep == req$ARGS)
  n_clients <- length(unique(rep_data$name))
  
  list(
    # response type - ephemeral indicates the response will only be seen by the
    # user who invoked the slash command as opposed to the entire channel
    response_type = "ephemeral",
    # attachments is expected to be an array, hence the list within a list
    attachments = list(
      list(
        title = paste0("Rep: ", req$ARGS),
        fallback = paste0("Rep: ", req$ARGS),
        fields = list(
          list(
            title = "Total Clients",
            value = n_clients,
            short = TRUE
          ),
          list(
            title = "Calls / Client",
            value = sum(rep_data$calls) / n_clients,
            short = TRUE
          )
        )
      )
    )
  )
}

#* Summary of region performance
#* @serializer unboxedJSON
#* @post /region
function(req, res) {
  # Authorize request
  status <- slack_auth(req)
  if (status == "401") {
    res$status <- 401
    return(
      list(text = "Error: Invalid request.")
    )
  }
  
  # Check to ensure provided region value exists in data
  if (!req$ARGS %in% unique(sim_data$region)) {
    return(
      list(
        reponse_type = "ephemeral",
        text = paste("Error: No region found matching", req$ARGS)
      )
    )
  }
  
  region_data <- dplyr::filter(sim_data, region == req$ARGS)
  
  list(
    response_type = "ephemeral",
    attachments = list(
      list(
        title = paste0("Region: ", req$ARGS),
        fallback = paste0("Region: ", req$ARGS),
        image_url = paste0("http://colorado.rstudio.com/rsc/slack-plumber/plot/region/",
                           tolower(req$ARGS)),
        fields = list(
          list(
            title = "Total Clients",
            value = length(unique(region_data$name)),
            short = TRUE
          )
        )
      )
    )
  )
}

#* Plot region data
#* @png
#* @get /plot/<region>
function(region) {
  sim_data %>% 
    filter(region == "West") %>% 
    ggplot(aes(x = ))
}


