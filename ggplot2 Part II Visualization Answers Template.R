
# STUDENT ID:
student_id<- # write your student id
# GROUP NAME:
group<-  # write your group name
  
# Provide the correct answers by filling in the gaps.
  

## 5. Data Visualization using ggplot2 Part II 

#i.
library(ggplot2)
#Create the data
x <- _(-6*pi, 6*pi, length.out = 100) # we expect a sequence
#ggplot2 needs the data to be in a data.frame or tibble with all the data.
dat <- _(x = x, y = sin(x)/x) # create a dataframe
ggplot(data = dat, mapping = aes(x = x, y = y)) + 
  _ # we expect a line graph

#ii.

data(cars)

#basic version with points
ggplot(cars, mapping = aes(x = speed, y = dist)) + 
  _ # we expect a scatter plot

#Add color as a mapping that is only used by geom_points
ggplot(cars, mapping = aes(x = speed, y = dist)) + 
  geom_point(mapping = aes(color = dist > 80))

# define the colors using a manual color scale
ggplot(cars, mapping = aes(x = speed, y = dist)) + 
  geom_point(mapping = aes(color = dist > 80)) + 
  _(values = c("black", "red")) # add the colours manually using appropriate scale function

# add a second geom that produces a smoothed line (default is a local polynomial regression)
ggplot(cars, mapping = aes(x = speed, y = dist)) + 
  geom_point(mapping = aes(color = dist > 80)) + 
  scale_color_manual(values = c("black", "red")) +
  _ # add a smooth line











