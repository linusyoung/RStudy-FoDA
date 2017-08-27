# Wk 2
(1891-2000)/(251/sqrt(25))
pt(2.064, 24)
qt(0.05,6)
(861-900)/(59/sqrt(7))

# Pre-Lab
bull <- BullRiders
head(bull,1)

#Select bull riders from the US
USA <-bull[bull$Country=="USA",]

# Summarize the bull rider weights
mean(USA$Weight)
sd(USA$Weight)

# Visualize the weight distribution
hist(USA$Weight, main='Histogram of US Bull Rider Weights',xlab='Weight (lbs)')

# Run the single sample t-test
t.test(USA$Weight, mu=190)

# Lab
bull14 <- bull[bull$Events14 >= 5, ]
head(bull14)
mean(bull14$RidePer14)
sd(bull14$RidePer14)
hist(bull14$RidePer14)
t.test(bull14$RidePer14, mu = 0.5)

# Questions
# 1
earnings12 <- bull[bull$Earnings12>0, c("Events12","Earnings12")]
earnings_per <- earnings12$Earnings12/earnings12$Events12
hist(earnings_per)
earnings_per_log <- log(earnings_per)
hist(earnings_per_log)
mean(earnings_per_log)
t.test(earnings_per_log, mu = 8.85)
exp(8.572169)
exp(9.120605)

# 2
sw <- c(29.4,29.0,28.4,28.8,28.9,29.3,28.5,28.2)
mean(sw)
sd(sw)
t.test(sw, mu = 28.5)
qt(0.975, length(sw)-1)

# 3
(93.6-91)/(7.8/sqrt(25))
qt(0.95, 24)

# 4
qt(0.95, 11)
42.6 + qt(0.95, 11) # answer seems to use 0.99 (99.5% confident) instead of 0.95(90% confident)
42.6 - qt(0.99, 11)
