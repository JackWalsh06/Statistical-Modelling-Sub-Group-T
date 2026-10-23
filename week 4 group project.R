matches <- read.table("ashford_matches(in).csv", header = T, sep = ",")
#Reads the matches file into R and calls the table "matches"
#header = T the first row holds the columns names and sep ="," the columns seperated by , 
players <- read.table("league_players(in).csv", header = T, sep = ",")
#same thing but for players
injured_fittedmodel <- glm(injured ~ minutes_played, family = binomial, data = players)
# Builds the model and saves it under injured_fittedmodel
# glm is used instead of lm because being injured is a yes/no not a figure
summary(injured_fittedmodel)
# prints the results
# in the coefficients table, estimate is on the log-odds scale
exp(summary(injured_fittedmodel)$coef[2,1])
#takes the slop (row 2, column 1 of the coefficients table) and applies(exp)
#this gives the odds ratio: how the offs of injury change for 1 extra minute
#it will be very close to 1 because 1 minute is a tiny step
exp(100 * summary(injured_fittedmodel)$coef[2,1])
#the same thing for a bigger step of 100 minutes
#a value just above 1 means the odds of injury go up slightly for every extra 100 minutes
confint.default(injured_fittedmodel)
#a 95% confidence interval for each coefficient, using the asymptotic method
#if the interval for minutes_played contains 0, the odds ratio's interval contain 1
#which means there is no clear effect
confint(injured_fittedmodel)
#the same,but using the profile-likelihood method
#it gives very similar answers
drop1(injured_fittedmodel, test = "chisq")
# testing whether minutes_played is needed in the model, using the likelihood ratio test
#compares the model with and without minutes_played, using change in deviance
plot(players$minutes_played, players$injured,
     xlab = "Minutes played", ylab = "Injured (1 = yes, 0 = no)")
#draws every player as a dot. They sit in two rows: at 0 (not injured) and at 1 (injured)
newMins <- seq (0, 4000, length = 100)
# makes 100 evenly spaced minute values from 0 to 4000, to be able to draw a smooth cuurve through
newdf <- data.frame(minutes_played = newMins)
#puts those values in a small table
newP <- predict(injured_fittedmodel, newdata = newdf, type = "response")
#asks the model for the predicted probability of injury at each of those minute values
# type ="response" gives probabilities.
lines(newP ~ newMins, lwd=2)
#draws the fitted curve on top of the dots 
# i wanted to see whether the number of minutes a player played is related to whather they were injured
# the outcome injured is binary (1=injured, 0=not injured) and 115 of the 620 players were injured
# because the outcome is a yes or no, a lm is not suitable, so i fitted a regression using glm with family = binomial

