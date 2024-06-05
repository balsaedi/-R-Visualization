# AUTOMATED TESTING & GRADING SCRIPT

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
  total_tests <- 28 # Total number of tests
  
  run_test <- function(expr) {
    tryCatch({
      eval(expr)
      passed_tests <<- passed_tests + 1
    }, error = function(e) {
    })
  }
  
  # Run tests for each file
  run_test(expect_is(squid, "data.frame"))
  run_test(expect_is(squid_structure, "character"))
  run_test(expect_is(squid_summary, "character"))
  run_test(expect_is(squid$Fmaturity, "factor"))
  
  run_test(expect_is(squid$Fmonth, "factor"))
  
  run_test(expect_is(squid$Fyear, "factor"))
  run_test(expect_is(cont_table, "tibble"))
  
  run_test(expect_is(dotplot1, "function"))
  
  run_test(expect_is(index, "function"))
  run_test(expect_is(squid$nid.length[11], "numeric"))
  
  run_test(expect_is(plot1, "list"))
  run_test(expect_is(plot2, "list"))
  run_test(expect_is(plot3, "list"))
  run_test(expect_is(plot4, "list"))
  run_test(expect_is(summary1, "list"))
  run_test(expect_is(plot5, "list"))
  run_test(expect_is(plot6, "list"))
  run_test(expect_is(plot7, "list"))
  run_test(expect_is(plot8, "list"))
  run_test(expect_equal(plot9, "list"))
  
  run_test(expect_is(squid$weight.sqrt, "numeric"))
  
  run_test(expect_is(squid$weight.log, "numeric"))
  
  run_test(expect_is(plot10, "list"))
  run_test(expect_is(plot11, "list"))
  run_test(expect_is(plot12, "list"))
  run_test(expect_is(plot13, "list"))
  
  run_test(expect_is(plot14, "list"))
  run_test(expect_is(plot15, "list"))
  
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
    # ii.
    tryCatch({
      expect_equal(squid, read.table('squid1.txt', header =TRUE,stringsAsFactors = TRUE))
      score <- score + .35
    }, error = function(e) {})
    
    tryCatch({
      expect_equal(squid_structure, str(squid))
      score <- score + .35
    }, error = function(e) {})
    
    tryCatch({
      expect_equal(squid_summary, summary(squid))
      score <- score + .35
    }, error = function(e) {})
    
    tryCatch({
      expect_equal(squid$Fmaturity, factor(squid$maturity.stage))
      score <- score + .35
    }, error = function(e) {})
    
    tryCatch({
      expect_equal(squid$Fmonth, factor(squid$month))
      score <- score + .35
    }, error = function(e) {})
    
    tryCatch({
      expect_equal(squid$Fyear, factor(squid$year))
      score <- score + .35
    }, error = function(e) {})
    
    #iii.
    tryCatch({
      expect_equal(cont_table, table(squid$Fmonth, squid$Fyear))
      score <- score + .35
    }, error = function(e) {})
    
    # iv.
    tryCatch({
      expect_equal(dotplot1, dotplot(as.matrix(squid[,c("DML", "weight", "nid.length", "ovary.weight")]),
                           groups=FALSE,
                           strip = strip.custom(bg = 'white',
                                                par.strip.text = list(cex = 0.8)),
                           scales = list(x = list(relation = "free"),
                                         y = list(relation = "free"),
                                         draw = FALSE),
                           col=1, cex  =0.5, pch = 16,
                           xlab = "Value of the variable",
                           ylab = "Order of the data from text file"))
      score <- score + .35
    }, error = function(e) {})
    
    # v.
    tryCatch({
      expect_equal(index, which(squid$nid.length > 400))
      score <- score + .35
    }, error = function(e) {})
    
    tryCatch({
      expect_equal(squid$nid.length[11], 43.2)
      score <- score + .35
    }, error = function(e) {})
    
    # vi.
    tryCatch({
      expect_equal(plot1, hist(squid$DML, main="", xlab = "DML"))
      score <- score + .35
    }, error = function(e) {})
    
    tryCatch({
      expect_equal(plot2, hist(squid$weight, main="", xlab = "weight"))
      score <- score + .35
    }, error = function(e) {})
    
    tryCatch({
      expect_equal(plot3, hist(squid$eviscerate.weight, main="", xlab = "eviscerate weight"))
      score <- score + .35
    }, error = function(e) {})
    
    tryCatch({
      expect_equal(plot4, hist(squid$ovary.weight, main="", xlab = "ovary weight"))
      score <- score + .35
    }, error = function(e) {})
    
    tryCatch({
      expect_equal(summary1, summary(squid$DML))
      score <- score + .35
    }, error = function(e) {})
    
    tryCatch({
      expect_equal(plot5, hist(squid$DML, xlab = "DML", breaks = brk1, main = "brk: 20"))
      score <- score + .35
    }, error = function(e) {})
    
    tryCatch({
      expect_equal(plot6, hist(squid$DML, xlab = "DML", breaks = brk2, main = "brk: 10"))
      score <- score + .35
    }, error = function(e) {})
    
    tryCatch({
      expect_equal(plot7, hist(squid$DML, xlab = "DML", breaks = brk3, main = "brk: 5"))
      score <- score + .35
    }, error = function(e) {})
    
    tryCatch({
      expect_equal(plot8, hist(squid$DML, xlab = "DML", breaks = brk4, main = "brk: 2"))
      score <- score + .35
    }, error = function(e) {})
    
    # vii.
    tryCatch({
      expect_equal(plot9, plot(squid$DML, squid$weight))
      score <- score + .35
    }, error = function(e) {})
    
    tryCatch({
      expect_equal(squid$weight.sqrt, sqrt(squid$weight))
      score <- score + .35
    }, error = function(e) {})
    
    tryCatch({
      expect_equal(squid$weight.log, log(squid$weight))
      score <- score + .35
    }, error = function(e) {})
    
    # viii.
    tryCatch({
      expect_equal(plot10, boxplot(DML ~ Fmaturity, data = squid, xlab = "maturity stage", ylab = "DML"))
      score <- score + .35
    }, error = function(e) {})
    
    tryCatch({
      expect_equal(plot11, vioplot(DML ~ Fmaturity, data = squid, xlab = "maturity stage", ylab = "DML", col = "lightblue"))
      score <- score + .35
    }, error = function(e) {})
    
    #ix.
    tryCatch({
      expect_equal(plot12, coplot(weight.sqrt ~ DML | Fmaturity, data = squid))
      score <- score + .35
    }, error = function(e) {})
    
    tryCatch({
      expect_equal(plot13, xyplot(weight.sqrt ~ DML | Fmaturity, data = squid))
      score <- score + .35
    }, error = function(e) {})
    
    #x.
    tryCatch({
      expect_equal(plot14, pairs(squid[, c(5, 8, 9, 11, 12, 13)]))
      score <- score + .35
    }, error = function(e) {})
    
    tryCatch({
      expect_equal(plot15, pairs(squid[, c(5, 8, 9, 11, 12, 13)], diag.panel = panel.hist, upper.panel = panel.cor, lower.panel = panel.smooth))
      score <- score + .55
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













