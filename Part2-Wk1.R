# Part 2 Wk 1
#180	200	190	230	80	160	170
#130	140	220	110	120	100	170
r <- c(180,200,190,230,80,160,170,130,140,220,110,120,100,170)
mean(r)+1.96*48.5/sqrt(length(r))
1.96*48.5/sqrt(length(r))

# Pre Lab
survey <- StudentSurvey
table(head(survey$name_letters,10)>5)

# Calculate the population parameters
hist(survey$name_letters)
fivenum(survey$name_letters)
mean(survey$name_letters)
sd(survey$name_letters)


# Draw 1,000 samples of n=5 and find the mean of each sample.
xbar5 <-rep(NA, 1000)
for (i in 1:1000)
{x <-sample(survey$name_letters, size =5)
xbar5[i] <-  mean(x)}

hist(xbar5)
# Graph the histogram of 1,000 sample means.
hist(xbar5,xlim=c(2,10))


# Calculate the mean and sd of the sampling distribution.
mean(xbar5)
sd(xbar5)

# Compare to the std dev predicted by the CLT.
sd(survey$name_letters)/sqrt(5)


#Repeat for samples of size n=15
xbar15 <-rep(NA, 1000)
for (i in 1:1000)
{x <-sample(survey$name_letters, size =15)
xbar15[i] <- mean(x)}
hist(xbar15)
hist(xbar15,xlim=c(2,10))
mean(xbar15)
sd(xbar15)
sd(survey$name_letters)/sqrt(15)


#Repeat for samples of size n=25
xbar25 <-rep(NA, 1000)
for (i in 1:1000)
{x <-sample(survey$name_letters, size =25)
xbar25[i] <- mean(x)}
hist(xbar25,xlim=c(2,10))
mean(xbar25)
sd(xbar25)
sd(survey$name_letters)/sqrt(25)

# Lab
happy <- survey$happy
hist(happy)
mean(happy)
sdhappy <- sd(happy)

happy5 <- rep(NA, 1000)
for (i in 1:1000) {
  x <- sample(survey$happy, size = 5)
  happy5[i] <- mean(x)
}
mean(happy5)
sdhappy/sqrt(5)

happy15 <- rep(NA, 1000)
for (i in 1:1000) {
  x <- sample(survey$happy, size = 15)
  happy15[i] <- mean(x)
}
mean(happy15)
sdhappy/sqrt(15)

happy25 <- rep(NA, 1000)
for (i in 1:1000) {
  x <- sample(survey$happy, size = 25)
  happy25[i] <- mean(x)
}
mean(happy25)
sdhappy/sqrt(25)

# Problem set
# Question 1
austin <- survey$austin
hist(austin)
mean(austin)
sd(austin)

#expect standard deviation on size 10
sd(austin)/sqrt(10)

# Question 2
# a
1 - pnorm((3.2-3.08)/0.4)
# b
3.08 #mu for population
# c
0.4/sqrt(25)

#e
pnorm((3.2-3.08)/(0.4/sqrt(25))) - pnorm((2.9-3.08)/(0.4/sqrt(25)))

# Question 3
# b
11/sqrt(23)
# c
z <- (35.1-28)/(11/sqrt(23))
# d
1 - pnorm(z)

# Question 4
xbar = 471.46
# a
se <- 1.5/sqrt(15)

# b
me <- round(se * 1.96,3)
# c
xbar + me
xbar - me
