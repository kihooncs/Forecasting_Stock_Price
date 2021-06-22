library(quantmod)
library(prophet)
library(dplyr)

## function to plot predicted stock in next period
PredictStock <- function(stock, the_periods) {
  stock <- as.data.frame(stock)
  ds <-  as.Date(row.names(stock))
  y <- log(stock %>% select(y = ends_with("Close")))
  df <- data.frame(ds, y)
  m <- prophet(df)
  future <- make_future_dataframe(m, periods = the_periods)
  forecast <- predict(m,future)
  prophet_plot_components(m, forecast)
}

##  get the data of predicted stock in next period
GetPredictStockData <- function(stock, the_periods) {
  stock <- as.data.frame(stock)
  ds <-  as.Date(row.names(stock))
  y <- log(stock %>% select(y = ends_with("Close")))
  df <- data.frame(ds, y)
  m <- prophet(df)
  future <- make_future_dataframe(m, periods = the_periods)
  forecast <- predict(m,future)
  finaldata <- tail(forecast, the_periods)
  finaldata$yhat <- exp(finaldata$yhat)
  finaldata$yhat_lower <- exp(finaldata$yhat_lower)
  finaldata$yhat_upper <- exp(finaldata$yhat_upper)
  finaldata <- finaldata %>% select(ds, yhat, yhat_lower, yhat_upper)
}

## draw the chart for a single stock within the given plot type
StockChart <- function(symbolstock, plottype) {
  highchart(type = "stock") %>% 
    hc_exporting(enabled = TRUE) %>%
    hc_add_series(symbolstock, type = plottype)
}

## add an indicator to the given chart
AddIndicator <- function(chart, indicator_func_name, stock) {
  FUN <- match.fun(indicator_func_name)
  chart %>%
    hc_add_series(name = indicator_func_name, FUN(Cl(stock)))
}

## add a comparison company stock to the given chart
AddComparision <- function(chart, stock, plottype) {
  chart %>% 
    hc_add_series(stock, type = plottype)
}