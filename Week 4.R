# Week 4 - Binary regression
# Consider a binary outcome in the data and fit a model for it.

# Hypotheses (beta1 is the coefficient of training_load on the logit scale)
# H0: beta1 = 0, training load is not associated with the probability of injury
# H1: beta1 != 0, training load is associated with the probability of injury

# injured - binary: 1 = injured during the season, 0 = not injured
# training_load - score from 1 (low) to 5 (high)

# Read in the data (in the group workspace)
players_data <- read.csv("league_players.csv", header = T)

# Check the variable names are from the right data source
names(players_data)

# Create a smaller dataset with the two variables of interest
injury.data <- data.frame(
  injured = players_data$injured,
  training_load = players_data$training_load
)
head(injury.data)

# How many injured and non-injured players there are
table(injury.data$injured)
# The distribution of the training load
table(injury.data$training_load)

# 1. Fit the logistic regression
# logit(pi_i) = beta0 + beta1 * training_load_i
# where pi_i is the probability that player i is injured
injury.glm=glm(injured~training_load,family=binomial,data=injury.data)
summary(injury.glm)

# Answer: the estimated coefficient for training_load is 0.3513 (SE 0.1051)
# with p-value 0.00083 < 0.05, so we reject H0: training load is associated
# with the probability of injury.

# 2. Odds ratio and confidence intervals
# exp(beta1) is the factor by which the odds of injury are multiplied for each extra point of training load
exp(summary(injury.glm)$coef[2,1])

# 95% asymptotic and profile-likelihood confidence intervals for beta1
confint.default(injury.glm)
confint(injury.glm)

# and for the odds ratio
exp(confint.default(injury.glm))
exp(confint(injury.glm))

# Answer: exp(0.3513) = 1.42, so the odds of injury are multiplied by about 1.42 (about 42% higher) for each extra point of training load. 
# The 95% confidence interval for the coefficient (0.145 to 0.557 asymptotic, 0.147 to 0.560 profile) does not include 0, so the interval for the odds ratio (about 1.16 to 1.75) does not include 1. 
# This is an association, not a cause.

# 3. Alternatively, enter the data as binomial proportions (one row per training load)
tab <- table(players_data$training_load, players_data$injured)
tab
injury2.data <- data.frame("injured" = tab[,2], "notinjured" = tab[,1],
                           "training_load" = 1:5)
injury2.data

injury2.glm=glm(cbind(injured,notinjured)~training_load,family=binomial,data=injury2.data)
summary(injury2.glm)

# Answer: the coefficients are the same as in part 1. The residual deviance is not 0 here: this model has 5 groups and 2 parameters, so it is not the saturated model. 
# Its residual deviance (2.56 on 3 df) can be compared with a chi-squared distribution as a goodness-of-fit check: 
1 - pchisq(deviance(injury2.glm), df.residual(injury2.glm))
# p = 0.46, so there is no evidence that the model fits badly.

# 4. Plot the proportion of players injured against training load, with the
#    fitted curve. Larger symbols are proportions based on more players.
plot(injury2.data$training_load,
     injury2.data$injured/(injury2.data$injured+injury2.data$notinjured),
     xlab = "Training load",
     ylab = "Proportion of players injured",
     pch = 21,
     bg = "white",
     cex = (injury2.data$injured+injury2.data$notinjured)/60,
     xlim = c(0.5, 5.5),
     ylim = c(0, 0.5))

new.load <- seq(1, 5, length = 100)
newdf <- data.frame(training_load = new.load)
newP <- predict(injury.glm, newdata = newdf, type = "response")
lines(newP ~ new.load, lwd = 2, col = "blue")

# Answer: the fitted probability of injury rises from about 0.09 at training load 1 to about 0.29 at training load 5.



# Conclusion:
# The logistic regression of injury on training load gives a coefficient of 0.3513 (SE 0.1051, z = 3.34, p = 0.00083). 
# Since p < 0.05 we reject H0: beta1 = 0 and conclude that there is evidence that training load is associated with the probability of injury. 
# The odds ratio is 1.42 (95% CI 1.16 to 1.75): each extra point of training load is associated with the odds of injury being multiplied by about 1.42 (about 42% higher). 
# The fitted probability of injury rises from about 0.09 at training load 1 to about 0.29 at training load 5.
# The grouped model has a residual deviance of 2.56 on 3 df (p = 0.46), so there is no evidence that the straight-line logit model fits badly.
  
# Limitations: this is an association, not a cause. Players with a high training load may differ in other ways (age, previous injuries, minutes played)
# The model uses training load alone, and players are clustered within clubs, which the model does not account for. 
# Training load 1 has only 20 players, so its observed proportion is imprecise.
  