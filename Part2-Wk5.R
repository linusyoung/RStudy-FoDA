# Part 2 Wk 5

# Pre-Lab
film <- FilmData
View(film)
# Show how many films are in each group
table(film$Rating)

# Calculate avg film budget of each group
aggregate(Budget~Rating,film,mean)

# Calculate sd of film budget within each group
aggregate(Budget~Rating,film,sd)

# Visualize the group means and variability
boxplot(film$Budget~film$Rating, main= "Film Budgets by Rating",
        ylab= "Budget", xlab= "MPAA Rating")

# Run ANOVA
modelbud <- aov(film$Budget~film$Rating)
summary(modelbud)

# Run post-hoc test if F statistic is significant
TukeyHSD(modelbud)

# Calculate avg IMDB score of each group
aggregate(IMDB~Rating,film,mean)

#Calculate sd of IMDB scores within each group
aggregate(IMDB~Rating,film,sd)

# Visualize the group means and variability
boxplot(film$IMDB~film$Rating, main= "IMDB Scores by Rating",
        ylab= "IMDB Score", xlab= "MPAA Rating")

# Run ANOVA
modelscore <- aov(film$IMDB~film$Rating)
summary(modelscore)

# Run post-hod text if F statistic is significant
TukeyHSD(modelscore)

# Lab
# Question 1
table(film$Studio)

aggregate(Days~Studio, film, mean)

modelDays <- aov(film$Days~film$Studio)
summary(modelDays)

TukeyHSD(modelDays)

# Question 2
aggregate(Pct.Dom~Studio, film, mean)
modelPctDom <- aov(film$Pct.Dom ~ film$Studio)
summary(modelPctDom)
boxplot(film$Pct.Dom ~ film$Studio)

# Problem Sets
# 1
film$BudgetGroup[film$Budget < 100] <- 'low'
film$BudgetGroup[film$Budget >= 100 & film$Budget < 150] <- 'medium'
film$BudgetGroup[film$Budget >= 150] <- 'high'
table(film$BudgetGroup)
aggregate(Pct.Dom ~ BudgetGroup, film, mean)
boxplot(film$Pct.Dom ~ film$BudgetGroup)
modelBudgetGroup <- aov(film$Pct.Dom ~ film$BudgetGroup)
summary(modelBudgetGroup)
TukeyHSD(modelBudgetGroup)

# 2
qf(0.95, 2, 42)
q2f <- (2387.7/2)/((5949.1-2387.7)/42)
q2f

# 3
modelq3 <- aov(q3$ticket ~ q3$sections)
summary(modelq3)

# 4
MSw <- 1365/34
MSw
MSb <- 782/2
q4f <- MSb/round(MSw,2)
q4f
adjP <- 0.05/3
adjP
