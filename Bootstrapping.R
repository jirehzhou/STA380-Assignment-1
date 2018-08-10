library(mosaic)
library(quantmod)
library(foreach)

# Import a few stocks
mystocks = c("SPY", "TLT", "LQD", "EEM", "VNQ")
getSymbols(mystocks)

for(ticker in mystocks) {
  expr = paste0(ticker, "a = adjustOHLC(", ticker, ")")
  eval(parse(text=expr))
}

initial_wealth = 100000
n_days = 20
#### Bootstrapping in 5 assets portfolio for 4 weeks ####
set.seed(2)
# Combine all the returns in a matrix
all_returns_5 = cbind(ClCl(SPYa),
                    ClCl(TLTa),
                    ClCl(LQDa),
                    ClCl(EEMa),
                    ClCl(VNQa))
head(all_returns_5)
all_returns_5 = as.matrix(na.omit(all_returns_5))

holding_mat_5 = matrix(0, 1000, 5)
sim1 = foreach(i=1:1000, .combine='rbind') %do% {
  total_wealth = initial_wealth
  weights = c(0.2, 0.2, 0.2, 0.2, 0.2)
  holdings = weights * total_wealth
  wealthtracker = rep(0, n_days)
  for(today in 1:n_days) {
    return.today = resample(all_returns_5, 1, orig.ids=FALSE)
    holdings = holdings + holdings*return.today
    total_wealth = sum(holdings)
    wealthtracker[today] = total_wealth
  }
  holding_mat_5[i,] = holdings
  wealthtracker
}

head(sim1)
hist(sim1[,n_days], breaks = 50)

# Profit/loss
mean(sim1[,n_days])
hist(sim1[,n_days]- initial_wealth, breaks=30)

# Calculate 5% value at risk
quantile(sim1[,n_days], 0.05) - initial_wealth

#Calculate mean of return for each assets
Mean_return = rep(0, 5)
for(i in 1:5){
  Mean_return[i] = mean(holding_mat_5[,i])- initial_wealth*0.2
}
Mean_return

# Calculate 5% value at risk for each assets
for(j in 1:5){
print(quantile(holding_mat_5[,j], 0.05) - initial_wealth*0.2)
}

#### Bootstrapping in 3 assets portfolio for 4 weeks ####
set.seed(2)
all_returns_3 = cbind(ClCl(SPYa),
                     ClCl(TLTa),
                     ClCl(LQDa))
all_returns_3 = as.matrix(na.omit(all_returns_3))
#Bootrapping this portfolio with 20 days for 1000 times
sim2 = foreach(i=1:1000, .combine='rbind') %do% {
  total_wealth = initial_wealth
  weights = c(0.3, 0.4, 0.3)
  holdings = weights * total_wealth
  wealthtracker = rep(0, n_days)
  for(today in 1:n_days) {
    return.today = resample(all_returns_3, 1, orig.ids=FALSE)
    holdings = holdings + holdings*return.today
    total_wealth = sum(holdings)
    wealthtracker[today] = total_wealth
  }
  wealthtracker
}

head(sim2)
hist(sim2[,n_days], breaks = 50)

# Profit/loss
mean(sim2[,n_days])
hist(sim2[,n_days]- initial_wealth, breaks=30)

# Calculate 5% value at risk
quantile(sim2[,n_days], 0.05) - initial_wealth

#### Bootstrapping in 2 assets portfolio for 4 weeks ####
set.seed(2)
all_returns_2 = cbind(ClCl(EEMa),
                      ClCl(VNQa))
all_returns_2 = as.matrix(na.omit(all_returns_2))
#Bootrapping this portfolio with 20 days for 1000 times
#In addition, we rebalanced the portfolio each day
sim3 = foreach(i=1:1000, .combine='rbind') %do% {
  total_wealth = initial_wealth
  weights = c(0.9, 0.1)
  holdings = weights * total_wealth
  wealthtracker = rep(0, n_days)
  for(today in 1:n_days) {
    return.today = resample(all_returns_2, 1, orig.ids=FALSE)
    holdings = holdings + holdings*return.today
    total_wealth = sum(holdings)
    holdings = c(total_wealth*0.9, total_wealth*0.1)
    wealthtracker[today] = total_wealth
  }
  wealthtracker
}

head(sim3)
hist(sim3[,n_days], breaks = 50)

# Profit/loss
mean(sim3[,n_days])
hist(sim3[,n_days]- initial_wealth, breaks=30)

# Calculate 5% value at risk
quantile(sim3[,n_days], 0.05) - initial_wealth
