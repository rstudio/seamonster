# Plumber Bitcoin API

Plumber allows you to create a web API by merely decorating your existing R source code with special comments. Take a look at an example.

## Setup

Create a connection and define the helper function `currency` which pulls data from the database.

```
con <- dbConnect(odbc::odbc(), "Postgres (DSN)")
bitcoin <- tbl(con, "bitcoin")
currency <- function(code="USD"){
  bitcoin %>%
    filter(name == code) %>%
    select(timestamp, last, symbol) %>%
    collect
}
```

## Endpoint 1: Plot

Query the bitcoin table for a specific currency. Return a dygraph plot object.

* [/api/plot?code=JPY](http://54.149.163.100:3939/api/plot?code=JPY)

```
#' @get /plot
#' @param code Currency code (DCA; JPY; CNY; GBP)
#' @serializer htmlwidget
function(code="USD"){
  dat <- currency(code)
  tseries <- xts(dat$last, dat$timestamp)
  lab <- paste0("Bitcoin (", dat$symbol[1], ")")
  p <- dygraph(tseries, main = lab) %>%
    dyOptions(axisLineWidth = 1.5, 
              fillGraph = TRUE, 
              drawGrid = FALSE, 
              colors = "steelblue", 
              axisLineColor = "darkgrey", 
              axisLabelFontSize = 15) %>%
    dyRangeSelector(fillColor = "lightsteelblue", strokeColor = "white")
  print(p)
}
```

## Endpoint 2: Table

Query the bitcoin table for a specific currency. Return a data table object.

* [/api/table?code=JPY](http://54.149.163.100:3939/api/table?code=JPY)

```
function(code="USD"){
  currency(code) %>%
    collect %>%
    datatable
}
```

## Endpoint 3: Data

Query the bitcoin table for a specific currency. Return a JSON object.

* [/api/data?code=JPY](http://54.149.163.100:3939/api/data?code=JPY)

```
#' @get /data
#' @param code Currency code (DCA; JPY; CNY; GBP)
function(code="USD"){
  currency(code)
}
```


