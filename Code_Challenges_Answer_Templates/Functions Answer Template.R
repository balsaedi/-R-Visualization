
# Provide the correct answers by filling in the gaps.

# STUDENT ID: # write your student id
student_id<- 
# GROUP NAME: # write your group name
group<-  

    
# 2. Functions Code Challenge Answer Template
#a.
sqdif = function(x = 2, y = 3) {
  if (!is._(x) | !is._(y)){   # Fill in the two gaps
    stop("Both x and y must be _")  # Fill in the gap
  } else return((x - y)^2)
}
sqdif
sqdif(10, 5)
sqdif("5", 1)

#b.
top = function(mat, n = 5) {
  return(_[1:n, 1:n]) # fill in the gap
}
top(matrix(1:100, ncol=10))

top(matrix(1:25, ncol = 5), 2)

#c. 
my_t_CI = function(x) {
  x_bar = mean(x)
  n = length(x)
  z = qt(0.975, df = n-1) # approximately 1.96 for large n
  ci.lower = x_bar - z*sd(x) / sqrt(n)
  ci.upper = x_bar + z*sd(x) / sqrt(n)
  return(_(x_bar = _, CI.lower = _, CI.upper = _)) # fill in the gaps
}
set.seed(60)
data_sim = _(100) # we expect random number generation from a standard normal distribution
my_t_CI(data_sim)

t_test=t.test(_) # fill in the gap to compare the accuracy of your function with the built-in function
