# AUTOMATED TESTING & GRADING SCRIPT for ggplot2 VISUALIZATION

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
  total_tests <- 20 # Total number of tests
  
  run_test <- function(expr) {
    tryCatch({
      eval(expr)
      passed_tests <<- passed_tests + 1
    }, error = function(e) {
    })
  }
  
  # Run tests for each file
  run_test(expect_is(patients, "data.frame"))
  
  run_test(expect_is(plot1, "list"))
  run_test(expect_is(plot2, "list"))
  run_test(expect_is(plot3, "list"))
  run_test(expect_is(plot4, "list"))
  run_test(expect_is(plot5, "list"))
  run_test(expect_is(plot6, "list"))
  run_test(expect_is(plot7, "list"))
  run_test(expect_is(plot8, "list"))
  run_test(expect_is(plot9, "list"))
  
  run_test(expect_is(plot10, "list"))
  run_test(expect_is(plot11, "list"))
  run_test(expect_is(plot12, "list"))
  run_test(expect_is(plot13, "list"))
  
  run_test(expect_is(plot14, "list"))
  run_test(expect_is(plot15, "list"))
  run_test(expect_is(plot16, "list"))
  run_test(expect_is(plot17, "list"))
  run_test(expect_is(plot18, "list"))
  run_test(expect_is(plot19, "list"))
  
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
  # Filter students who passed at least 18 tests
  passed_students <- results_df[results_df$tests_passed >= 18, ]
  
  # Initialize grading list
  grading_list <- list()
  
  # Grade each student's answers
  for (i in 1:nrow(passed_students)) {
    student_id <- passed_students[i, "student_id"]
    group <- passed_students[i, "group"]
    source(paste0("students_code/cc1/", student_id, ".R"))
    score <- 0
    
    # Grade each question
    # 1.
    tryCatch({
      expect_equal(patients, read_tsv("patient-data-cleaned.txt"))
      score <- score + .5
    }, error = function(e) {})
    
    # 2.
    tryCatch({
      expect_equal(plot1, ggplot(data = patients, mapping = aes(x = BMI, y = Weight, colour = Height)) +
                     geom_point())
      score <- score + .5
    }, error = function(e) {})
    
    # 3.
    tryCatch({
      expect_equal(plot2, ggplot(data = patients, mapping = aes(x = BMI, y = Weight, colour = Height)) +
                     geom_point() +
                     geom_smooth())
      score <- score + .5
    }, error = function(e) {})
    
    # 4.
    tryCatch({
      expect_equal(plot3, ggplot(data = patients, mapping = aes(x = BMI, y = Weight, colour = Height)) +
                     geom_point() +
                     geom_smooth(method = "lm", se = FALSE))
      score <- score + .5
    }, error = function(e) {})
    
    # 5.
    tryCatch({
      expect_equal(plot4, ggplot(data = patients, mapping = aes(x = Smokes, y = Score)) +
                     geom_boxplot())
      score <- score + .5
    }, error = function(e) {})
    
    # 6.
    tryCatch({
      expect_equal(plot5, ggplot(data = patients, mapping = aes(x = Smokes, y = Score, colour = Sex)) +
                     geom_boxplot())
      score <- score + .5
    }, error = function(e) {})
    
    # 7.
    tryCatch({
      expect_equal(plot6, ggplot(data = patients, mapping = aes(x = Sex, y = Score, fill = Age)) +
                     geom_violin())
      score <- score + .5
    }, error = function(e) {})
    
    # 8.
    tryCatch({
      expect_equal(plot7, ggplot(data = patients, mapping = aes(x = BMI)) +
                     geom_histogram(fill = "blue", binwidth = 0.5))
      score <- score + .5
    }, error = function(e) {})
    
    # 9.
    tryCatch({
      expect_equal(plot8, ggplot(data = patients, mapping = aes(x = BMI)) +
                     geom_density())
      score <- score + .5
    }, error = function(e) {})
    
    #10.
    tryCatch({
      expect_equal(plot9, ggplot(data = patients, mapping = aes(x = BMI)) +
                     geom_density(aes(fill = Sex), alpha = 0.5))
      score <- score + .5
    }, error = function(e) {})
    
    # 11.
    tryCatch({
      expect_equal(plot10, ggplot(data = patients, mapping = aes(x = BMI, y = Weight, colour = Height)) +
                     geom_point() +
                     facet_grid(Sex ~ Smokes))
      score <- score + .5
    }, error = function(e) {})
    
    # 12.
    tryCatch({
      expect_equal(plot11, ggplot(data = patients, mapping = aes(x = Smokes, y = BMI, fill = Sex)) +
                     geom_boxplot() +
                     facet_wrap(~ Age))
      score <- score + .5
    }, error = function(e) {})
    
    # 13.
    tryCatch({
      expect_equal(plot12, ggplot(data = patients, mapping = aes(x = Sex, y = BMI, fill = Age)) +
                     geom_boxplot() +
                     facet_wrap(~ Smokes))
      score <- score + .5
    }, error = function(e) {})
    
    # 14.
    tryCatch({
      expect_equal(plot13, ggplot(data = patients, mapping = aes(x = BMI, y = Weight)) +
                     geom_point())
      score <- score + .5
    }, error = function(e) {})
    
    # 15.
    tryCatch({
      expect_equal(plot14, ggplot(data = patients, mapping = aes(x = BMI, y = Weight)) +
                     geom_point() +
                     scale_x_continuous(breaks = c(20, 30, 40), limits = c(20, 40)) +
                     scale_y_continuous(breaks = seq(60, 100, by = 5), label = seq(60, 100, by = 5), name = "Weight (kg)")
      )
      score <- score + .5
    }, error = function(e) {})
    
    # 16.
    tryCatch({
      expect_equal(plot15, ggplot(data = patients, mapping = aes(x = Age, y = BMI, fill = Age)) +
                     geom_violin() +
                     scale_fill_brewer(palette = "Blues"))
      score <- score + .5
    }, error = function(e) {})
    
    # 17.
    tryCatch({
      expect_equal(plot16, ggplot(data = patients, mapping = aes(x = BMI, y = Weight, colour = Height)) +
                     geom_point() +
                     scale_colour_gradient2(low = "green", high = "red", mid = "grey", midpoint = mean(patients$Height))
      )
      score <- score + .5
    }, error = function(e) {})
    
    # 18.
    tryCatch({
      expect_equal(plot17, ggplot(data = patients, mapping = aes(x = BMI, y = Weight, colour = Age)) +
                     geom_point() +
                     geom_smooth(method = "lm", se = FALSE))
      score <- score + .5
    }, error = function(e) {})
    
    # 19.
    tryCatch({
      expect_equal(plot18, ggplot(data = patients, mapping = aes(x = BMI, y = Weight, colour = Age)) +
                     geom_point() +
                     geom_smooth(method = "lm", se = FALSE) +
                     theme(
                       legend.title = element_blank(),
                       legend.key = element_rect(fill = "white"),
                       legend.position = "bottom"
                     ))
      score <- score + .5
    }, error = function(e) {})
    
    # 20.
    tryCatch({
      expect_equal(plot19, ggplot(data = patients, mapping = aes(x = BMI, y = Weight, colour = Age)) +
                     geom_point() +
                     geom_smooth(method = "lm", se = FALSE) +
                     theme(
                       legend.title = element_blank(),
                       legend.key = element_rect(fill = "white"),
                       legend.position = "bottom",
                       panel.grid.minor = element_blank()
                     ) +
                     labs(title = "BMI vs Weight")
                   )
      score <- score + .5
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


