# AUTOMATED TESTING & GRADING SCRIPT for BASE R

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
  total_tests <- 13 # Total number of tests
  
  run_test <- function(expr) {
    tryCatch({
      eval(expr)
      passed_tests <<- passed_tests + 1
    }, error = function(e) {
    })
  }
  
  # Run tests for each file
  run_test(expect_is(my.num, "numeric"))
  run_test(expect_is(my.num_4_prod, "numeric"))
  run_test(expect_is(my.char, "character"))
  run_test(expect_is(both, "character"))
  
  run_test(expect_is(length_both, "num"))
  
  run_test(expect_is(class_both, "character"))
  run_test(expect_is(division_vec, "numeric"))
  
  run_test(expect_is(x, "numeric"))
  
  run_test(expect_is(y, "numeric"))
  
  run_test(expect_is(is_valid, "logical"))
  run_test(expect_is(y, "numeric"))
  run_test(expect_equal(is_valid2, "logical"))
  
  run_test(expect_is(xy_prod, "numeric"))
  
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
  passed_students <- results_df[results_df$tests_passed >= 10, ]
  
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
      expect_equal(my.num, c(5,4,7,8,12,14))
      score <- score + 1
    }, error = function(e) {})
    
    #ii.
    tryCatch({
      expect_equal(my.num_4_prod, my.num * 4)
      score <- score + 1
    }, error = function(e) {})
    
    #iii.
    tryCatch({
      expect_equal(my.char, c("Robert", "Parker", "Robert", "Robert", "Parker"))
      score <- score + 1
    }, error = function(e) {})
    
    #iv.
    tryCatch({
      expect_equal(both, c(my.num, my.char))
      score <- score + 1
    }, error = function(e) {})
    
    # v.
    tryCatch({
      expect_equal(length_both, length(both))
      score <- score + 1
    }, error = function(e) {})
    
    #vi.
    tryCatch({
      expect_equal(class_both, class(both))
      score <- score + 1
    }, error = function(e) {})
    
    #vii.
    tryCatch({
      expect_equal(division_vec, both / 3)
      score <- score + 1
    }, error = function(e) {})
    
    # viii.
    tryCatch({
      expect_equal(x, c(1,2,3,4,5,6))
      score <- score + .5
    }, error = function(e) {})
    
    # ix
    tryCatch({
      expect_equal(y, c(10,20,30,40,50))
      score <- score + .5
    }, error = function(e) {})
    
    # x
    tryCatch({
      expect_equal(is_valid, "No")
      score <- score + .5
    }, error = function(e) {})
    
    # xi.
    tryCatch({
      expect_equal(y, c(y, 60))
      score <- score + .5
    }, error = function(e) {})
    
    #xii.
    tryCatch({
      expect_equal(is_valid2, "Yes")
      score <- score + 1
    }, error = function(e) {})
    
    #xiii.
    tryCatch({
      expect_equal(xy_prod, x * y)
      score <- score + 1
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




















