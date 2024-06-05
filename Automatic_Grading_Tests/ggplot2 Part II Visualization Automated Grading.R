# AUTOMATED TESTING & GRADING SCRIPT for ggplot2 VISUALIZATION PART II

# Script dependencies
library(testthat)
library(tidyverse)

# List all files in the directory
files <- list.files("students_code/cc1", pattern = "\\.R$", full.names = TRUE)

# Initialize counters for total tests
total_passed_tests <- 0
total_tests <- 0

# Define a function to run tests and return results
run_tests <- function(file) {
  # Read the first two lines of the file
  first_lines <- readLines(file, n = 2)
  student_id <- first_lines[1]
  group <- first_lines[2]
  
  # Source the file
  source(file)
  
  # Initialize counters for passed and failed tests
  passed_tests <- 0
  total_tests <- 7 # Total number of tests
  
  run_test <- function(expr) {
    tryCatch({
      eval(expr)
      passed_tests <<- passed_tests + 1
    }, error = function(e) {
    })
  }
  
  # Run tests for each file
  run_test(expect_is(x, "numerical"))
  run_test(expect_is(dat, "data.frame"))
  
  run_test(expect_is(plot1, "list"))
  run_test(expect_is(plot2, "list"))
  run_test(expect_is(plot3, "list"))
  run_test(expect_is(plot4, "list"))
  run_test(expect_is(plot5, "list"))
  
  # Return results
  return(list(student_id = student_id, group=group, tests_passed = passed_tests, total_tests = total_tests))
}

# Run tests for each file
results <- lapply(files, run_tests)

# Convert list to data frame
results_df <- as.data.frame(do.call(rbind, results))

# Convert tests_passed to numeric
results_df$tests_passed <- as.numeric(results_df$tests_passed)

# Print results
results_df


# Function to filter students who passed at least 18 tests and grade their answers
grade_students <- function(results_df) {
  # Filter students who passed at least 5 tests
  passed_students <- results_df[results_df$tests_passed >= 5, ]
  
  # Initialize grading list
  grading_list <- list()
  
  # Grade each student's answers
  for (i in 1:nrow(passed_students)) {
    student_id <- passed_students[i, "student_id"]
    group <- passed_students[i, "group"]
    source(paste0("students_code/cc1/", student_id, ".R"))
    score <- 0
    
    # Grade each question
    # i.
    tryCatch({
      expect_equal(x, seq(-6*pi, 6*pi, length.out = 100))
      score <- score + .5
    }, error = function(e) {})
    
    tryCatch({
      expect_equal(dat, data.frame(x = x, y = sin(x)/x))
      score <- score + .5
    }, error = function(e) {})
    
    # ii.
    tryCatch({
      expect_equal(plot1, ggplot(data = dat, mapping = aes(x = x, y = y)) + 
                     geom_line())
      score <- score + 1
    }, error = function(e) {})
    
    tryCatch({
      expect_equal(plot2, ggplot(cars, mapping = aes(x = speed, y = dist)) + 
                     geom_point() )
      score <- score + 2
    }, error = function(e) {})

    tryCatch({
      expect_equal(plot3, ggplot(cars, mapping = aes(x = speed, y = dist)) + 
                     geom_point(mapping = aes(color = dist > 80)))
      score <- score + 2
    }, error = function(e) {})

    tryCatch({
      expect_equal(plot4, ggplot(cars, mapping = aes(x = speed, y = dist)) + 
                     geom_point(mapping = aes(color = dist > 80)) + 
                     scale_color_manual(values = c("black", "red")))
      score <- score + 2
    }, error = function(e) {})

    tryCatch({
      expect_equal(plot5, ggplot(cars, mapping = aes(x = speed, y = dist)) + 
                     geom_point(mapping = aes(color = dist > 80)) + 
                     scale_color_manual(values = c("black", "red")) +
                     geom_smooth())
      score <- score + 2
    }, error = function(e) {})
    
    
    # Append student's information to the list including group number
    grading_list[[i]] <- list(
      student_id = student_id[[1]],
      group = group[[1]],
      score = score,
      max_score = 10)
  }
  
  return(grading_list)
}

# Get grading list
grading_list <- grade_students(results_df)

# Convert list to dataframe
grading_df <- do.call(rbind, grading_list)

# Print dataframe
grading_df


