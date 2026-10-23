# Week 2 task, Simple Linear Regression
# Outcome: Goal difference (goals scored- goals conceded), predictor: Opponent rank, from data set Ashford_matches
# Upload the data set and set the variables

matches<-read.csv("ashford_matches.csv")
goal_difference <- matches$goals_scored - matches$goals_conceded

# Look at the data

str(matches)
summary(matches)

# Plot the data and our linear regression model

hist(goal_difference, main="Goal difference", xlab="goals scored - goals conceded")
plot(goal_difference~opponent_rank,data=matches,xlab="Opponent rank", ylab="Goal difference")

# Fit the model

model <- lm(goal_difference~opponent_rank, data=matches)
abline(model,col="red")


summary(model)

# Check assumptions

plot(fitted(model), rstudent(model), xlab="Fitted values", ylab="Studentised residuals", main="Fitted vs residuals")
abline(h=0)

qqnorm(rstudent(model), main="QQ plot")
qqline(rstudent(model))
