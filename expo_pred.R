library(ggplot2)
library(patchwork)
library(astsa)
library(TSA)
set.seed(153)

# load data
stock = read.csv("data/TSLA.csv")
stock$Date = as.Date(stock$Date)
# Extract last 300 days
n = 300
t = 1:n
stock_300 = tail(stock, n)
stock_300 = stock_300[c('Date','Close')]
stock_300$t = t

# exponential filter 
alpha = 0.9
lag = 10
filter_weights = alpha^(1:lag)
filter_weights = filter_weights/sum(filter_weights)
filtered_stock = filter(stock_300$Close, filter_weights, sides = 1)

# Plot the original data and fitted values
plot(stock_300$t, stock_300$Close, type = 'l', main = "(a)", xlab = "Time", ylab = "Close Price")
lines(stock_300$t, filtered_stock, col = 'red')
filtered_stock = na.omit(filtered_stock)
cut_stock = stock_300$Close[-1:-(lag - 1)]
res = cut_stock - filtered_stock
plot(res, main = "(b)", xlab = "Time", ylab = "Residuals")

# seasonal differencing
diff_res = diff(res, lag = 5)
plot(diff_res, main = "(a)", ylab = "Differenced Data")

# cross-validation
start <- 200
total_error <- 0

for (i in 0:9) {
  # train-val set split
  train <- window(stock_300$Close, end = start + i * 10 - 0.01)
  val <- window(stock_300$Close, start = start + i * 10, end = start + (i + 1) * 10- 0.01)
  
  # get stationary residuals
  filtered_train = filter(train, filter_weights, sides = 1)
  filtered_train = na.omit(filtered_train)
  cut_train <- train[-1:-(lag - 1)]
  exp_res = cut_train - filtered_train
  
  # exponential smoothing prediction
  exp_pred = numeric(10)
  values = tail(train, 10) # store the values for prediction
  for (i in 1:10){
    exp_pred[i] = sum(tail(values, 10) * rev(filter_weights)) 
    values <- append(values, exp_pred[i])
  }
  
  # ARMA prediction
  arma_pred <- sarima.for(exp_res, n.ahead = 10,  p = 1, d = 0, q = 3, P = 0, D = 0, Q = 1, S = 5)

  m1_pred <- exp_pred + arma_pred$pred
  total_error <- total_error + mean((m1_pred - val)^2)
}

total_error / 10
