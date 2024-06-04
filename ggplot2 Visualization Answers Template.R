
# STUDENT ID:
student_id<- # write your student id
# GROUP NAME:
group<-  # write your group name
  
# Provide the correct answers by filling in the gaps.
  


# 4. Data Visualization with ggplot2

#1.
_ <- read_tsv("patient-data-cleaned.txt") # we expect the name of the dataframe

#2.
ggplot(data = patients, mapping = aes(x = BMI, y = Weight, colour = Height)) +
  _ # A scatterplot is expected

#3.
ggplot(data = patients, mapping = aes(x = BMI, y = Weight, colour = Height)) +
  geom_point() +
  _ # a smooth line is expected

#4.
ggplot(data = patients, mapping = aes(x = BMI, y = Weight, colour = Height)) +
  geom_point() +
  _ #  a smooth linear regression equation line is expected

#5.
ggplot(data = patients, mapping = aes(x = Smokes, y = Score)) +
  _# a boxplot is expected

#6.
ggplot(data = patients, mapping = aes(x = Smokes, y = Score, colour = _)) +
  _ # we expect a boxplot and colored by sex

#7.
patients$Age <- factor(patients$Age)
ggplot(data = patients, mapping = aes(x = Sex, y = Score, fill = Age)) +
  _ # we expect a violin plot

#8.
ggplot(data = patients, mapping = aes(x = BMI)) +
  geom_histogram(fill = "blue", _ = 0.5) # a histogram with a defined bin width of 0.5

#9.
ggplot(data = patients, mapping = aes(x = BMI)) +
  _ # a density plot

#10.
ggplot(data = patients, mapping = aes(x = BMI)) +
  geom_density(_(fill = Sex), alpha = 0.5) # fill in the gap

#11.
ggplot(data = patients, mapping = aes(x = BMI, y = Weight, colour = Height)) +
  geom_point() +
  _(Sex ~ Smokes) # facet the Sex by Smokes variable

#12.
ggplot(data = patients, mapping = aes(x = Smokes, y = BMI, fill = Sex)) +
  geom_boxplot() +
  _(~ Age) # facet by age

#13.
ggplot(data = patients, mapping = aes(x = Sex, y = BMI, fill = Age)) +
  geom_boxplot() +
  _(~ Smokes) # facet by Smokes

#14.
ggplot(data = patients, mapping = aes(x = BMI, y = Weight)) +
  _ # we expect a scatter plot.

#15.
ggplot(data = patients, mapping = aes(x = BMI, y = Weight)) +
  geom_point() +
  _(breaks = c(20, 30, 40), limits = c(20, 40)) + # use appropriate scale__x__ function
  _(breaks = seq(60, 100, by = 5), label = seq(60, 100, by = 5), name = "Weight (kg)") #  use appropriate scale__y__ function

#16.
ggplot(data = patients, mapping = aes(x = Age, y = BMI, fill = Age)) +
  geom_violin() +
  _(palette = "Blues") #  use appropriate scale____ function to fill using a sequential colour palette.

#17.
ggplot(data = patients, mapping = aes(x = BMI, y = Weight, colour = Height)) +
  geom_point() +
  _(low = "green", high = "red", mid = "grey", midpoint = mean(patients$Height)) #  use appropriate scale____ function to Make the colour scale  with a midpoint

#18.
ggplot(data = patients, mapping = aes(x = BMI, y = Weight, colour = Age)) +
  geom_point() +
  _(method = "lm", se = FALSE) # add a straight regression line

#19.
ggplot(data = patients, mapping = aes(x = BMI, y = Weight, colour = Age)) +
  geom_point() +
  _(method = "lm", se = FALSE) + #add a straight regression line
  theme(
    legend.title = element_blank(),
    legend.key = element_rect(fill = "white"),
    _ = "bottom" # position the legend at the bottom of the graph
  )


#20.
ggplot(data = patients, mapping = aes(x = BMI, y = Weight, colour = Age)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  theme(
    legend.title = element_blank(),
    legend.key = element_rect(fill = "white"),
    legend.position = "bottom",
    panel.grid.minor = element_blank()
  ) +
  labs(title = "BMI vs Weight")
_("_", units = "in", height = 7, width = 7) # save the plot as BMI_vs_Weight.png





