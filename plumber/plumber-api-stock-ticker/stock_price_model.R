# Set some sensible options for quantmod
options(getSymbols.warning4.0 = FALSE)
options(getSymbols.auto.assign = FALSE)

# Load the required packages
suppressPackageStartupMessages({
  library(quantmod)
  library(DT)
  library(dygraphs)
  library(forecast)
  library(highcharter)
  library(stringr)
  library(memoise)
  library(highcharter)
  library(ggfortify)
})

# Input parameters
params <- list(
  symbol = "IBM",
  days = 90,
  forecast = 7,
  model = "1,1,1"
)


# memoize the call to retrieve data 
getSymbols <- memoise(quantmod::getSymbols, cache = cache_filesystem("cache"))

# Retrieve the data
prices <- getSymbols(params$symbol, auto.assign = FALSE, src = "yahoo", from = Sys.Date() - 4 * params$days)
close_price <- Cl(xts::last(prices, n = params$days))


# Plot using higchart
hchart(prices)


# Extract pattern for arima
ptn <- "\\d,\\d,\\d"
arima_coefs <- as.numeric(
  strsplit(str_extract(params$model, ptn), split = ",")[[1]]
  )

# Fit a model
model <- arima(close_price, arima_coefs)
fcast <- forecast(model, as.numeric(params$forecast), level = c(50, 90, 99))

# Use fortify to convert forecast data to data frame
df <- fortify(fcast)

# Replace index positions with date values
idx <- index(close_price)
fcast_dates <- c(
  idx, 
  seq(
    as.Date(tail(idx, n = 1)) + 1,
    length.out = params$forecast,
    by = 1
  )
)

# Round the forecast to 2 digits
library(dplyr)
df <- df %>% 
  mutate(Index = fcast_dates) %>% 
  mutate_if(is.numeric, ~round(., 2))

# Set highchart options
hc_opts <-  list(
  # align y-axis labels to left
  yAxis = list(
    labels = list(
      align = "left"
    )
  )
)

library(RColorBrewer)

# Create a palette of n colours from the ColorBrewer blues palette
display.brewer.pal(3, "Blues")
blues <- brewer.pal(3, "Blues")

# Final plot
highchart(type = "stock", hc_opts = hc_opts) %>% 
  hc_add_series(prices, type = "ohlc") %>% 
  hc_add_series(df, "line", hcaes(Index, Data), name = "Original") %>% 
  hc_add_series(df, "line", hcaes(Index, `Point Forecast`), name = "Forecast") %>% 
  hc_add_series(df, "arearange", hcaes(Index, low = `Lo 99`, high = `Hi 99`), 
                name = "99% interval", color = blues[1]) %>% 
  hc_add_series(df, "arearange", hcaes(Index, low = `Lo 90`, high = `Hi 90`), 
                name = "90% interval", color = blues[2]) %>% 
  hc_add_series(df, "arearange", hcaes(Index, low = `Lo 50`, high = `Hi 50`), 
                name = "50% interval", color = blues[3])
                                                  
