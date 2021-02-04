
# This sets the size of plots to a good default.
options(repr.plot.width = 5, repr.plot.height = 4)

# Loading in packages
library(readr)
library(dplyr)
library(ggplot2)
# .... YOUR CODE FOR TASK 1 ....

library(testthat) 
library(IRkernel.testthat)

run_tests({
    test_that("the packages are loaded", {
    expect_true( all(c("package:ggplot2", "package:readr", "package:dplyr") %in% search() ), 
        info = "The dplyr, readr and ggplot2 packages should be loaded using library().")
    })
})

# Reading in the data
data <- read_csv("datasets/candy_crush.csv")

# Printing out the first six rows
head(data)
# .... YOUR CODE FOR TASK 2 ....

run_tests({
    test_that("data is read in correctly", {
        correct_data <- read_csv("datasets/candy_crush.csv")
        expect_equal(correct_data, data, 
            info = "data should countain datasets/candy_crush.csv read in using read_csv")
        })
})

# Count and display the number of unique players
print("Number of players:")
length(unique(data$player_id))
# .... YOUR CODE FOR TASK 3 ....

# Display the date range of the data
print("Period for which we have data:")
range(data$dt)
# .... YOUR CODE FOR TASK 3 ....

run_tests({
    test_that("nothing", {
        expect_true(TRUE, info = "")
    })
})

# Calculating level difficulty
difficulty <- data %>% group_by(level) %>%

 # .... YOUR CODE FOR TASK 4 ....
 summarise(attempts = sum(num_attempts), wins = sum(num_success)) %>%
 # .... YOUR CODE FOR TASK 4 ....
mutate(p_win = wins/attempts)
# Printing out the level difficulty
print(difficulty)
# .... YOUR CODE FOR TASK 4 ....

run_tests({
    test_that("p_win is calculated correctly", {
        correct_difficulty <- data %>%
            group_by(level) %>%
            summarise(attempts = sum(num_attempts), wins = sum(num_success)) %>%
            mutate(p_win = wins / attempts)
        expect_equal(correct_difficulty$p_win, difficulty$p_win, 
            info = "difficulty$p_win should be estimated probability to pass each level in a single attempt")
        })
})

# Plotting the level difficulty profile
library(scales)

ggplot(difficulty,aes(x=level,y=p_win)) + geom_line() + scale_x_continuous(breaks=1:15) + scale_y_continuous(label = scales::percent)
# .... YOUR CODE FOR TASK 5 ....

run_tests({
    test_that("the student plotted a ggplot", {
    expect_true('ggplot' %in% class(last_plot()), 
        info = "You should plot difficulty using ggplot.")
    })
})

# Adding points and a dashed line

# .... YOUR CODE COPIED FROM TASK 5 ....
ggplot(difficulty,aes(x=level,y=p_win)) + geom_line() + geom_point() +
scale_x_continuous(breaks=1:15) + 
scale_y_continuous(label = scales::percent)+
# .... YOUR CODE FOR TASK 6 ....
geom_hline(yintercept=0.1, linetype="dashed", color='red')

run_tests({
    plot_layers <- sapply(last_plot()$layers, function(layer)  class(layer$geom)[1])
    test_that("the student has plotted lines, points and a hline", {
    expect_true(all(c('GeomLine', 'GeomPoint', 'GeomHline') %in%  plot_layers), 
        info = "The plot should include lines between the datapoints, points at the datapoints and a horisontal line.")
    })
})

# Computing the standard error of p_win for each level
difficulty <- difficulty %>% mutate(error= sqrt(p_win * (1 - p_win) / attempts))
    # .... YOUR CODE FOR TASK 7 HERE ....
difficulty

run_tests({
    test_that("error is correct", {
        correct_difficulty <- difficulty %>%
            mutate(error = sqrt(p_win * (1 - p_win) / attempts))
        expect_equal(correct_difficulty$error, difficulty$error,
            info = "difficulty$error should be calculated as sqrt(p_win * (1 - p_win) / attempts)")
    })
})

# Adding standard error bars
# .... YOUR CODE COPIED FROM TASK 6 ....
ggplot(difficulty,aes(x=level,y=p_win)) + geom_line() + geom_point() +
scale_x_continuous(breaks=1:15) + 
scale_y_continuous(label = scales::percent)+
geom_hline(yintercept=0.1, linetype="dashed", color='red') + 
# .... YOUR CODE FOR TASK 8 ....
geom_errorbar(difficulty,mapping=aes(level, ymin=p_win-error, ymax=p_win+error))

run_tests({
    plot_layers <- sapply(last_plot()$layers, function(layer)  class(layer$geom)[1])
    test_that("the student has plotted lines, points and a hline", {
    expect_true("GeomErrorbar" %in%  plot_layers, 
        info = "The plot should include error bats using geom_errorbar.")
    })
})

# The probability of completing the episode without losing a single time
p <- prod(difficulty$p_win)

# Printing it out
p

run_tests({
    test_that("p is correct", {
        correct_p <- prod(difficulty$p_win)
        expect_equal(correct_p, p,
            info = "p should be calculated as the product of difficulty$p_win .")
    })
})

# Should our level designer worry about that a lot of 
# players will complete the episode in one attempt?
should_the_designer_worry = FALSE # TRUE / FALSE

run_tests({
    test_that("should_the_designer_worry is FALSE", {
    expect_false(should_the_designer_worry,
        info = "The probability is really small, so I don't think the designer should worry that much...")
    })
})
