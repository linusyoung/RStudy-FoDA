# Wk3
qt(0.95, 9)

gum <- c(79, 95, 85, 82)
nogum <- c(80, 94, 87, 84)
pt(mean(gum - nogum)/(sd(gum - nogum)/sqrt(4)), 3)
mean(gum - nogum)/(sd(gum - nogum)/sqrt(4))
qt(0.025,3)

# Pre-Lab
post <- PostSurvey

table(head(post$live_campus,10))
head(post,1)

# Lab Question 1

# Make a vector of happiness scores for each sample
underclass_happy <- post$happy[post$classification=='Freshman'|post$classification=='Sophomore']
upperclass_happy <- post$happy[post$classification=='Junior'|post$classification=='Senior']

mean(underclass_happy)
mean(upperclass_happy)

# Check the normality assumption
hist(underclass_happy, xlab='Underclassman Happiness', main='Percent of Time Happy')
hist(upperclass_happy, xlab='Upperclassman Happiness', main='Percent of Time Happy')

# Run independent t-test
t.test(underclass_happy, upperclass_happy)

# Lab Question 2

# Make a vector of difference scores
post$diff_happy <- post$happy - post$post_happy

# Check the normality assumption
hist(post$diff_happy, xlab= 'Difference in Happiness over the Semester', main = 'Happy-Post Happy')

# Run dependent t-test
t.test(post$happy, post$post_happy, paired=T)

# Lab
mean(post$hw_hours_college) - mean(post$hw_hours_HS)
t.test(post$hw_hours_college, post$hw_hours_HS, paired = TRUE)

greek <- post[post$greek=='yes',]
non_greek <- post[post$greek=='no',]
mean(greek$sleep_Sat)
mean(non_greek$sleep_Sat)
t.test(greek$sleep_Sat, non_greek$sleep_Sat, alternative = 'less')
hist(post$hw_hours_college - post$hw_hours_HS)
hist(greek$sleep_Sat)
hist(non_greek$sleep_Sat)

# Question 1
post$hw_hours_var <- post$hw_hours_college - post$hw_hours_HS
nursing <- post$hw_hours_var[post$major == 'Nursing']
biology <- post$hw_hours_var[post$major == 'Biology']
hist(nursing)
hist(biology)
t.test(nursing, biology)
# Question 2
qt(0.95, 25)
sqrt((5^2/26+6^2/32))
(80 - 74)/1.44
pt((80 - 74)/1.44, 25)

# Question 3
qt(0.95, 15)
# data from the questions page is save in q3 as a csv file without title.
q <- q3
cp_var <- q$X2 - q$X3
mean(cp_var)
sd(cp_var)
t.test(cp_var)
se <- sd(cp_var)/sqrt(length(cp_var))
se
