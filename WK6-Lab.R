library(SDSFoundations)
world <- WorldBankData

# Subset data for just the United States and name the new data frame "us"
us <- world[world$Country.Code == "USA",]

# Select the years from 1990 and name the new data frame "us_select"
us_select <- us[us$year >= 1990, ]

# Create a new variable in our datset called internet.mil to make the number of users more interpretable (into millions)
us_select$internet.mil <- us_select$internet.users / 1000000

# Create a new variable in our dataset called time that represents "years since 1990"
us_select$time <- us_select$year - 1990

# Select the first 10 years (from 1990 to 1999) and name the new data frame "us_select_10"
us_select_10 <- us_select[us_select$time < 10,]

# Use a function to fit an exponential and logistic model for 1990-1999
expFit(us_select_10$time, us_select_10$internet.mil)
logisticFit(us_select_10$time, us_select_10$internet.mil)

# Based on the prior model parameters, predict the number of internet users in 2006
e <- expFitPred(us_select_10$time, us_select_10$internet.mil, 16)
l <- logisticFitPred(us_select_10$time, us_select_10$internet.mil, 16)

#Primary Research Question
denmark <- world[world$Country.Code=='DNK',]
denyear1990 <- denmark[denmark$year>=1990,]
denmarkprop <- denyear1990$internet.users/denyear1990$population
y1990 <-denyear1990$year - 1990
expFit(y1990,denmarkprop)
logisticFit(y1990,denmarkprop)
log(((0.89668/0.7)-1)/308.8345)/log( 1.73124)

#Question 1

which(world$Country=='Brazil' & world$year=='2000')
world[1472,"mobile.users"]
brazil <- world[world$Country=='Brazil',]
which(brazil$mobile.user/1000000 >100)
brazil[48,"year"]
brazil1995 <- brazil[brazil$year>='1995',]
year1995 <-brazil1995$year - 1995
tripleFit(year1995,brazil1995$mobile.users)
logisticFit(year1995,brazil1995$mobile.users)
logisticFitPred(year1995,brazil1995$mobile.users,30)/1000000

#Question 2
367/257
76.64*1.46^14
3273.31/(1+43.59*(1/1.57)^14)
4379-76.64*1.46^14
4379-3273.31/(1+43.59*(1/1.57)^14)

#Question 3

18.65*1.34^10

#Question 4
2000/(1+152.10*(1/2.17)^9)
#another test from rstudio
