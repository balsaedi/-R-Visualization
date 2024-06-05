# AUTOMATED TESTING & GRADING SCRIPT for Functions

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
  total_tests <- 5 # Total number of tests
  
  run_test <- function(expr) {
    tryCatch({
      eval(expr)
      passed_tests <<- passed_tests + 1
    }, error = function(e) {
    })
  }
  
  # Run tests for each file
  run_test(expect_is(sqdif, "function"))
  run_test(expect_is(top, "function"))
  run_test(expect_is(my_t_CI, "function"))
  run_test(data_sim(data_sim, "numeric"))
  
  run_test(expect_is(t_test, "list"))
  
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
  # Filter students who passed at least 10 out of 13 tests
  passed_students <- results_df[results_df$tests_passed >= 3, ]
  
  # Initialize grading list
  grading_list <- list()
  
  # Grade each student's answers
  for (i in 1:nrow(passed_students)) {
    student_id <- passed_students[i, "student_id"]
    group <- passed_students[i, "group"]
    source(paste0("students_code/cc1/", student_id, ".R"))
    score <- 0
    
    # Grade each question
    # a.
    tryCatch({
      expect_equal(sqdif, function(x = 2, y = 3) {
        if (!is.numeric(x) | !is.numeric(y)){
          stop("Both x and y must be numeric")
        } else return((x - y)^2)
      })
      score <- score + 2
    }, error = function(e) {})
    
    #b.
    tryCatch({
      expect_equal(top, function(mat, n = 5) {
        return(mat[1:n, 1:n])
      })
      score <- score + 1
    }, error = function(e) {})
    
    #c.
    tryCatch({
      expect_equal(my_t_CI, function(x) {
        x_bar = mean(x)
        n = length(x)
        z = qt(0.975, df = n-1) # approximately 1.96 for large n
        ci.lower = x_bar - z*sd(x) / sqrt(n)
        ci.upper = x_bar + z*sd(x) / sqrt(n)
        return(list(x_bar = x_bar, CI.lower = ci.lower, CI.upper = ci.upper))
      })
      score <- score + 3
    }, error = function(e) {})
    
    tryCatch({
      expect_equal(data_sim, rnorm(100))
      score <- score + 2
    }, error = function(e) {})
    
    tryCatch({
      expect_equal(t_test, t.test(data_sim))
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
















