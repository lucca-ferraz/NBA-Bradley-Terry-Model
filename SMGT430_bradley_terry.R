#reading in and cleaning data
nba <- read.csv("~/Downloads/nba_schedule_2324.csv")
nba <- nba[, -c(2, 7, 8, 11)]

# Convert winner and loser team and score to home and away team and score                         
game <- nba |>                                                                                   
  dplyr::mutate(                                                                                
    team_home = Home,                                   
    team_away = Visitor,                                   
    score_diff = HOME_PTS - AWAY_PTS#
  )                                                                                               

# Set up the regression matrix that encodes home team as +1, away team as -1                      
team_matrix_home <- model.matrix(~ team_home, data = game)  # this matrix encodes home team as +1 
team_matrix_away <- model.matrix(~ team_away, data = game)  # this matrix encodes away team as +1 
team_matrix <- team_matrix_home - team_matrix_away          # +1 for home team; -1 for away team  

# Convert regression matrix to dataframe                                                          
team_data <- as.data.frame(team_matrix[, -1])         # drop first column (intercept)             
names(team_data) <- sort(unique(game$team_home))[-1]  # attach clean team names to team_data      

#estimate linear Bradley-Terry model for score differential
linear_model <- lm(game$score_diff ~ ., data = team_data)                                         
summary(linear_model)                                                                             

#estimate logistic Bradley-Terry model for win-loss outcomes
logistic_model <- glm(game$score_diff > 0 ~ ., data = team_data, family = binomial())             
summary(logistic_model)                                                                           

#ranking linear model

# Extract team names and coefficients
team_coefficients_linear <- coef(linear_model)[-1]  # Exclude the intercept
# Create a data frame for easier sorting
sorted_teams_linear <- data.frame(team = names(team_coefficients_linear), coefficient = team_coefficients_linear)
# Sort by coefficients
sorted_teams_linear <- sorted_teams_linear[order(sorted_teams_linear$coefficient, decreasing = TRUE), ]
# Reset row names
rownames(sorted_teams_linear) <- NULL
# Print the sorted teams
sorted_teams_linear

#ranking logistic model

# Extract team names and coefficients
team_coefficients_logistic <- coef(logistic_model)[-1]  # Exclude the intercept
# Create a data frame for easier sorting
sorted_teams_logistic <- data.frame(team = names(team_coefficients_logistic), coefficient = team_coefficients_logistic)
# Sort by coefficients
sorted_teams_logistic <- sorted_teams_logistic[order(sorted_teams_logistic$coefficient, decreasing = TRUE), ]
# Reset row names
rownames(sorted_teams_logistic) <- NULL
# Print the sorted teams
sorted_teams_logistic

#predicting thunder v clippers
coef(linear_model)["`Oklahoma City Thunder`"] + coef(linear_model)[1] - coef(linear_model)["`Los Angeles Clippers`"]
eta <- coef(logistic_model)["`Oklahoma City Thunder`"] + coef(logistic_model)[1] - coef(logistic_model)["`Los Angeles Clippers`"]
exp(eta) / (1 + exp(eta))

#plotting thunder expectation against every other team
okc_coefficient <- team_coefficients_linear["`Oklahoma City Thunder`"]
road_coefficient <- coef(linear_model)[1]
#filter out the thunder
other_teams <- setdiff(names(team_coefficients), "`Oklahoma City Thunder`")
#find predicted score differentials
differences <- okc_coefficient - team_coefficients_linear[other_teams] - road_coefficient

# Create a dataframe for plotting
plot_data <- data.frame(team = names(team_coefficients_linear[other_teams]), difference = differences)

library(ggplot2)
#plot predicted score differential for thunder vs any nba team (on road)
ggplot(plot_data, aes(x = team, y = difference)) +
  geom_bar(stat = "identity", fill = "#007dc3", color = "#EF3B24") +
  labs(title = "Predicted Score Differential for Oklahoma City Thunder @ Each Team",
       x = "Opponent Team",
       y = "Predicted Score Differential") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))
