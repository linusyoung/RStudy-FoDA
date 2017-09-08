# Part 2 Wk 4

#2b
brand <- c(38, 28, 24)
ttl_brand <- sum(brand)
exp_brand <- round(ttl_brand*0.33,0)
exp_brand
chisq.test(brand)
qchisq(0.95,2)

#1d
jurors <- c(12, 36, 32)
exp_dist <- c(0.2, 0.45, 0.35)
exp_jurors <- sum(jurors)*exp_dist
exp_jurors
sum(jurors)

chi <- 0
for (i in 1:length(jurors)){
  chi <- chi + (jurors[i]-exp_jurors[i])^2/exp_jurors[i]
}
chi
chisq.test(jurors, p = exp_dist)
qchisq(0.95, 2)

#1a
fear_height <- matrix(c(68,94,109,89), nrow = 2, ncol = 2)
fear_height
prop.table(fear_height,margin = 2)
gender <- colSums(fear_height)
fear <- sum(fear_height[1,])
not_fear <- sum(fear_height[2,])

fear_exp <- fear/sum(gender)
not_fear_exp <- not_fear/sum(gender)
men_fear <- gender[1]*fear_exp
women_fear <- gender[2]*fear_exp
men_not_fear <- gender[1]*not_fear_exp
women_not_fear <- gender[2]*not_fear_exp
men_fear
women_fear
men_not_fear
women_not_fear
68/162
109/198
1

# Pre-Lab
acl <- AustinCityLimits
View(acl)

# Create a table of counts for Gender
gender_tab <-table(acl$Gender)
gender_tab

# Create vector of expected proportions
ExpGender <- c(.50, .50)

# Check expected counts assumption
chisq.test(gender_tab, p=ExpGender)$expected

# Run goodness of fit
chisq.test(gender_tab, p=ExpGender)

# Create two-way table
gender_top10 <-table(acl$Gender, acl$BB.wk.top10)
gender_top10

# Generate expected counts
chisq.test(gender_top10, correct=FALSE)$expected

# Run test of independence
chisq.test(gender_top10, correct=FALSE)

# Lab 
# goodness of fit
genre_tab <- table(acl$Genre)
genre_exp <- c(.25,.25,.25,.25)
chisq.test(genre_tab, p = genre_exp)$expected
chisq.test(genre_tab, p = genre_exp)

# test of independent
genre_twitter <- table(acl$Genre, acl$Twitter.100k)
genre_twitter
country <- 6/17
jazz <- 2/11
rock <- 26/59
singer <- 10/16
country
jazz
rock
singer
chisq.test(genre_twitter, correct = FALSE)$expected
chisq.test(genre_twitter, correct = FALSE)

# Question 1
acl$Recent[acl$Year < 2012] <- 0
acl$Recent[acl$Year >= 2012] <- 1
recent_female <- acl[acl$Gender == 'F' & acl$Recent == 1,]

gender_tab <- table(acl$Gender, acl$Recent)
gender_tab

chisq.test(gender_tab, correct = FALSE)$expected
chisq.test(gender_tab, correct = FALSE)

# Question 2
offspring <- c(152, 39, 14)
off_exp <- c(.75, .15, .1)
chisq.test(offspring, p = off_exp)$expected
chisq.test(offspring, p = off_exp)
qchisq(0.95, 2)

# Question 3
qchisq(0.95, 1)
hand_tab <- table(hand$`	Gender`, hand$`	Dominant Hand`)
chisq.test(hand_tab, correct = FALSE)$expected

# Question 4
area <- c(28, 42, 53)
access <- c(13, 35, 50)
no_access <- area - access
sum(access)/sum(area)
1 - sum(access)/sum(area)
survey_tab <- matrix(c(access, no_access),3)
colnames(survey_tab) <- c('access', 'no access')
rownames(survey_tab) <- c('rural', 'suburban', 'urban')
chisq.test(survey_tab, correct = FALSE)$expected
chisq.test(survey_tab, correct = FALSE)
