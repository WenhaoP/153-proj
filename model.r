# parametric trend + non-parametric seasonality 
data <- read.csv("data/TSLA.csv")
open <- data$Open
close <- data$Adj.Close
close_tail = tail(close, 300)
plot.ts(close_tail)
log_data = log(close_tail)
plot.ts(log_data)

# fit a parametric model - linear regression
t = 1:300
lm <- lm(log_data ~ t)
res <- resid(lm)
plot.ts(res)

# use differencing to pursue seasonality
diff = diff(res, lag=5)
second_diff = diff(diff, lag = 1)
plot.ts(second_diff)
