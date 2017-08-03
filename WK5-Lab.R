library(SDSFoundations)
WR <- WorldRecords
unique(WR$Event)

recent<-WR[WR$Year>=1990,]

#Pre Lab
#Subset the data
menshot <- WR[WR$Event=='Mens Shotput',]
womenshot <- WR[WR$Event=='Womens Shotput',] 

#Create scatterplots
plot(menshot$Year,menshot$Record,main='Mens Shotput World Records',xlab='Year',ylab='World Record Distance (m)',pch=16)
plot(womenshot$Year,womenshot$Record,main='Womens Shotput World Records',xlab='Year',ylab='World Record Distance (m)',pch=16)

#Run linear models
linFit(menshot$Year, menshot$Record)
linFit(womenshot$Year,womenshot$Record)

#Lab
#Subset the data
menmile <- WR[WR$Event=='Mens Mile',]
womenmile <- WR[WR$Event=='Womens Mile',] 

#Create scatterplots
plot(menmile$Year,menmile$Record,main='Mens Shotput World Records',xlab='Year',ylab='World Record Distance (m)',pch=16)
plot(womenmile$Year,womenmile$Record,main='Womens Shotput World Records',xlab='Year',ylab='World Record Distance (m)',pch=16)

#Run linear models
linFit(menmile$Year, menmile$Record)
linFit(womenmile$Year,womenmile$Record)

#Problem Set Question 1
menpolevault1970 <- WR[WR$Event=='Mens Polevault' & WR$Year>=1970,]

fivenum(menpolevault1970$Record)
plot(menpolevault1970$Year, menpolevault1970$Record)
linFit(menpolevault1970$Year, menpolevault1970$Record)
