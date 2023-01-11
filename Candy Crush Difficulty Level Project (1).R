# This sets the size of plots to a good default.
options(repr.plot.width = 5, repr.plot.height = 4)

# Loading in packages
library(dplyr)
library(ggplot2)
library(readr)
# .... YOUR CODE FOR TASK 1 ....

# Reading in the data
data <- read_csv("datasets/candy_crush.csv")

# Printing out the first six rows
head(data)
# .... YOUR CODE FOR TASK 2 ....

# Count and display the number of unique players
print("Number of players:")
length(unique(data$player_id))

# Display the date range of the data
print("Period for which we have data:")
range(data$dt)

# Calculating level difficulty
difficulty <- data %>%
 group_by(level) %>%
 summarise(attempts = sum(num_attempts), wins = sum(num_success)) %>%
  mutate(p_win = wins / attempts)

# Printing out the level difficulty
difficulty

# Plotting the level difficulty profile
difficulty %>%
  ggplot(aes(x = level, y = p_win)) + 
    geom_line() + 
    scale_x_continuous(breaks = 1:15) +
    scale_y_continuous(label = scales::percent)

# Adding points and a dashed line
difficulty %>%
  ggplot(aes(x = level, y = p_win)) + 
    geom_line() + geom_point() +
    scale_x_continuous(breaks = 1:15) +
    scale_y_continuous(label = scales::percent) +
    geom_hline(yintercept = 0.1, linetype = 'dashed')

# Computing the standard error of p_win for each level
difficulty <- difficulty %>%
    mutate(error = sqrt(p_win * (1 - p_win) / attempts))

# Adding standard error bars
difficulty %>%
  ggplot(aes(x = level, y = p_win)) + 
    geom_line() + geom_point() +
    scale_x_continuous(breaks = 1:15) +
    scale_y_continuous(label = scales::percent) +
    geom_hline(yintercept = 0.1, linetype = 'dashed') +
    geom_errorbar(aes(ymin = p_win - error, ymax = p_win + error))


# The probability of completing the episode without losing a single time
p <- prod(difficulty$p_win)

# Printing it out
p

# Should our level designer worry about that a lot of 
# players will complete the episode in one attempt?
should_the_designer_worry = FALSE
