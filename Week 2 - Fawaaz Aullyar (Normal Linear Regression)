#Group Project 
#Week 2 - Normal Linear Regression
# - Read in both datasets, 
# - explore them by producing summary statistics and plots, 
# - classify each variable by type, and 
# - try a linear model for a continuous outcome.

# Continuous response - minutes_played 
# Explanatory variables - training_load and position
# minutes played=β0 +β1 (training load)+position effects+ε

# Questions to investigate:
#  - Are players' training load and playing position associated with the total number of minutes they play during the season?
# H0: Mean minutes played does not vary with training load or playing position
# H1: Training load, playing position, or both are associated with mean minutes played

# Read in the data
players_data <- read.table("league_players.csv",
                           header = TRUE,
                           sep = ",")

# Examine the data
str(players_data)
summary(players_data)

# Declare the categorical variables as factors
players_data$team <- as.factor(players_data$team)
players_data$position <- as.factor(players_data$position)
players_data$injured <- as.factor(players_data$injured)

# Summary statistics for minutes played
mean(players_data$minutes_played) #
sd(players_data$minutes_played)
range(players_data$minutes_played)

plot(players_data$training_load,
     players_data$minutes_played,
     xlab = "Training load",
     ylab = "Minutes played")

training_fit <- lm(minutes_played ~ training_load,
                   data = players_data)

summary(training_fit)
# Fitted equation is predicted minutes played = 2059.50 + 150.16(training load)
# P - value is 9.02e-08 < 0.05 so we reject Ho
# Therefore, there is very strong evidence of a positive linear relationship between training load and mean minutes played 
# R^2 is 4.5% so training load explains 4.5% of the observed variation in minutes played
# Adjusted R^2 is 4.37% - this penalises the model for more extra explanatory variables but since there is only 1 - it is very close to R^2

abline(training_fit, lwd = 2)

# Additive model
position_fit <- lm(minutes_played ~ training_load +
                     as.factor(position),
                   data = players_data)

summary(position_fit)
# predicted minutes played = 2266.09 + 143.54(training load) + position
# All the p - values < 0.05 and therefore we reject Ho
# Forwards are predicted to play ~ 621.74 fewer minutes than defenders with the same training load
# Midfielders are predicted to play 321.57 fewer minutes than defenders with the same training load 
# Training load and position explain 28% of the observed variation in minutes played
anova(training_fit, position_fit)

plot(position_fit$fitted,
     rstudent(position_fit),
     xlab = "Fitted values",
     ylab = "Studentised residuals")

abline(h = 0, lwd = 2)

qqnorm(rstudent(position_fit), main = NULL)
qqline(rstudent(position_fit), lwd = 2)
