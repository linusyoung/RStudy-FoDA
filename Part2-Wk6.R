# Part 2 Wk 6
# Pre-Lab
res <- TempskiResilience

clin <- res[res$Group == "Clinical Sciences",]

#Intial Correlations
vars <- c("QoL", "BDI")
cor(clin[,vars])

#RQ1 Model
ov_mod <- lm(QoL ~ BDI, data=clin)
summary(ov_mod)
confint(ov_mod)

#Diagnostics
plot(ov_mod, which=1)
cutoff <- 4/(ov_mod$df) 
plot(ov_mod, which=4, cook.levels=cutoff)

#Initial correlations
vars <- c("MS.QoL", "DREEM.S.SP", "DREEM.A.SP", "Resilience", "BDI", "Age")
cor(clin[,vars], use="pairwise.complete.obs")

#Test the initial correlations
library(psych)
corr.test(clin[,vars], use="pairwise.complete.obs")

#RQ2 Model
ms_mod <- lm(MS.QoL ~ DREEM.S.SP + DREEM.A.SP + Resilience + BDI + Age, data=clin)
summary(ms_mod)
confint(ms_mod)

#Diagnostics
library(car)
vif(ms_mod)
plot(ms_mod, which=1)
cutoff <- 4/(ms_mod$df) 
plot(ms_mod, which=4, cook.levels=cutoff)

#Put model into context
lmBeta(ms_mod) 
round(pCorr(ms_mod), 4) 

# Part 2 Week 6
# Lab
res <- TempskiResilience
res_bsp <- res[res$Group == 'Basic Sciences',]
cor(res_bsp$MS.QoL, res_bsp$WHOQOL.PH)
cor(res_bsp$MS.QoL, res_bsp$WHOQOL.PSY)
cor(res_bsp$MS.QoL, res_bsp$WHOQOL.SOC)
cor(res_bsp$MS.QoL, res_bsp$WHOQOL.ENV)
model <- lm(MS.QoL ~ WHOQOL.PH + WHOQOL.PSY 
            + WHOQOL.SOC + WHOQOL.ENV, data = res_bsp)
summary(model)

lmBeta(model)
pCorr(model)


# Problem set
# Question 1
res_cli <- res[res$Group == 'Clinical Sciences',]
cli_model <- lm(BDI ~ Age + Female + State.Anxiety + Trait.anxiety, data = res_cli)
summary(cli_model)
lmBeta(cli_model)
pCorr(cli_model)

# Question 2
li_model <- lm(BDI ~ Age, data = res_cli)
summary(li_model)
10.92641/2.98874

(1848.76/69.22)*18
# F
-23.4325/12.74
# G
0.1528 * 8.32
# H
(8.32^2)/((8.32^2)+18)

# Question 3
qf(0.95, 4, 88)
# men
3526.4 + 722.5*1 + 90.02*12 + 1.269*10 + 23.406*15
# women
3526.4 + 722.5*0 + 90.02*12 + 1.269*10 + 23.406*15

