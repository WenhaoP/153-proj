# parametric trend + non-parametric seasonality 
t = 1:300
data <- read.csv("data/TSLA.csv")
open <- data$Open
close <- data$Adj.Close
close_tail = tail(close, 300)
plot.ts(close_tail)
log_data = log(close_tail)
log_data = log(log_data)
plot.ts(log_data)

diff = diff(log_data, lag=5)
plot.ts(diff)


# fit a parametric model - linear regression
lm <- lm(log_data ~ t)
res <- resid(lm)
plot.ts(res)

# use differencing to pursue seasonality
diff = diff(res, lag=5)
second_diff = diff(diff, lag = 1)
plot.ts(second_diff)


fit <-lm(log_data ~ poly(t, 3, raw=TRUE))
res <- resid(fit)
plot.ts(res)
diff = diff(res, lag=5)
second_diff = diff(diff, lag = 1)
plot.ts(second_diff)

library(astsa)
model1 <- sarima(log_data, p=0, d=1, q=0, P=0, D=1, Q=1, S=12)

