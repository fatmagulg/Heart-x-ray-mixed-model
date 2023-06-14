##### LMM Lab 1 ####
library(lme4)
library(ggplot2)
library(dplyr)

heart <- read.csv("heart.csv")

#the diagnostic ratio (ratio of width of the heart to width of the thorax) 
#was calculated from the heart X-rays of 15 cases by two randomly selected 
#observers on two occasions. The X-rays were presented to the ob- servers in random order.

# observer: random effect
# case : fixed effect
# dr : response

# Fit null model
modeln <- lm(DR ~ 1, data = heart)

# model with an overall intercept + random effect for observer
model1 <- lmer(DR ~ 1 + (1|Observer), data = heart, REML = FALSE)
summary(model1)

# if |t-value| < 2 then reject h0



#### Model comparison using Likelihood ratio test from bootstrap samples ####
lrstat <- numeric(1000)
for (i in 1:1000){
  y <- unlist(simulate(modeln))
  bnull <- lm(y ~ 1)
  balt <- lmer(y ~ 1 + (1|Observer), data = heart, REML = FALSE)
  lrstat[i] <- as.numeric(2*(logLik(balt)-logLik(bnull)))
}

mean(lrstat > as.numeric(2*(logLik(model1)-logLik(modeln))) )
# value of 0.359 means that 35.9% of the simulated bootstrap samples had their likelihood ratio test 
# statistic greater than the lr test statistic for the fitted values of the model
## this can be treated as a p-value => value of beneath 0.05 means reject h0
## reject h0: the larger model is appropriate; the effect of the extra term is significant


#### Model comparison using anova() function ####
model2 <- lmer(DR ~ 1 + (1|Case) + (1|Observer), data = heart, REML = FALSE)
model3 <- lmer(DR ~ 1 + (1|Case) + (1|Observer) + (1|Case:Observer), data = heart, REML = FALSE)

anova(model2, model3)
# the output gives aic, bic, and loglik. the pr(chisq) is a p value corresponding to the loglik
# significant p value for model3 shows that the extra term is significant; prefer model3


## Boostrap technique again
lrstat2 <- numeric(1000)
for (i in 1:1000){
  y <- unlist(simulate(model2))
  bnull <- lmer(y ~ 1 + (1|Case) + (1|Observer), data = heart)
  balt <- lmer(y ~ 1 + (1|Case) + (1|Observer) + (1|Case:Observer), data = heart, REML = FALSE)
  lrstat2[i] <- as.numeric(2*(logLik(balt)-logLik(bnull)))
}

mean(lrstat > as.numeric(2*(logLik(model3)-logLik(model2))) )
# 0.001 significant => reject h0
# model 3 is preferable

