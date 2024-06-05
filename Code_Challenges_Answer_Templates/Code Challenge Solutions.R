
# Solutions

#1. Base R
#i.
my.num = c(5,4,7,8,12,14)
#ii.
my.num_4_prod=my.num * 4 
#iii.
my.char = c("Robert", "Parker", "Robert", "Robert", "Parker")
#iv.
both = c(my.num, my.char)
#v
length_both=length(both)
#vi.
class(both)
#vii.
both / 3
#viii
x = c(1,2,3,4,5,6)
#ix
y =  c(10,20,30,40,50)
#x
x + y
#Since y is shorter than x, it “recycles” the first element if y to make it long enough to match the length of x. This is can be a useful trick, but you must be careful when using R’s recycling. 11. append the value 60 onto the vector y (hint: you can use the c() function)
#xi.
y = c(y, 60)
#xii
x + y
#xiii
x * y


# 2. Functions
#a.
sqdif = function(x = 2, y = 3) {
  if (!is.numeric(x) | !is.numeric(y)){
    stop("Both x and y must be numeric")
  } else return((x - y)^2)
}
sqdif
sqdif(10, 5)
sqdif("5", 1)

#b.
top = function(mat, n = 5) {
  return(mat[1:n, 1:n])
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
  return(list(x_bar = x_bar, CI.lower = ci.lower, CI.upper = ci.upper))
}
set.seed(60)
data_sim = rnorm(100)
my_t_CI(data_sim)

t_test=t.test(data_sim)

# 3. Data Visualization

#ii.
squid <- read.table('squid1.txt', header =TRUE,stringsAsFactors = TRUE)
str(squid)
summary(squid)

# convert variables to factors
squid$Fmaturity <- factor(squid$maturity.stage)
squid$Fmonth <- factor(squid$month) 
squid$Fyear <- factor(squid$year)

str(squid)


#iii.
table(squid$Fmonth, squid$Fyear)
ftable(xtabs(~ Fyear + Fmaturity + Fmonth, data = squid))

#iv.
pdf('figures/ex4_dotplots.pdf')
par(mfrow = c(2, 2))
dotchart(squid$DML, main = "DML")
dotchart(squid$weight, main = "weight")
dotchart(squid$nid.length, main = "nid length")
dotchart(squid$ovary.weight, main = "ovary weight")
dev.off()

# alternative code using dotplot function from lattice package
library(lattice)
dotplot(as.matrix(squid[,c("DML", "weight", "nid.length", "ovary.weight")]),
        groups=FALSE,
        strip = strip.custom(bg = 'white',
                             par.strip.text = list(cex = 0.8)),
        scales = list(x = list(relation = "free"),
                      y = list(relation = "free"),
                      draw = FALSE),
        col=1, cex  =0.5, pch = 16,
        xlab = "Value of the variable",
        ylab = "Order of the data from text file")


# v.
which(squid$nid.length > 400)
squid$nid.length[11]
squid$nid.length[11] <- 43.2
squid$nid.length[11]
dotchart(squid$nid.length, main = "nid length")

# vi.
pdf('workshop/figures/ex4_hist.pdf')
par(mfrow = c(2,2))
hist(squid$DML, main="", xlab = "DML")
hist(squid$weight, main="", xlab = "weight")
hist(squid$eviscerate.weight, main="", xlab = "eviscerate weight")
hist(squid$ovary.weight, main="", xlab = "ovary weight")
dev.off()

# need to get the min and max values for DML to work out the limits for the breaks

summary(squid$DML)

# experimenting with different breaks
par(mfrow = c(2,2))
brk1 <- seq(from = 80, to = 340, by = 20)   
hist(squid$DML, xlab = "DML", breaks = brk1, main = "brk: 20")

brk2 <- seq(from = 80, to = 340, by = 10)   
hist(squid$DML, xlab = "DML", breaks = brk2, main = "brk: 10")

brk3 <- seq(from = 80, to = 340, by = 5)   
hist(squid$DML, xlab = "DML", breaks = brk3, main = "brk: 5")

brk4 <- seq(from = 80, to = 340, by = 2)   
hist(squid$DML, xlab = "DML", breaks = brk4, main = "brk: 2")


#vii.
# clearly not linear
plot(squid$DML, squid$weight)

# natural log and sqrt tranform weight
squid$weight.sqrt <- sqrt(squid$weight)
squid$weight.log <- log(squid$weight)

par(mfrow = c(1,2))
plot(squid$DML, squid$weight.sqrt)
plot(squid$DML, squid$weight.log)

# the square root transformation look most appropriate
jpeg('output/ex4_transf_plot.jpeg')
plot(squid$DML, squid$weight.sqrt)
dev.off()

png('output/ex4_transf_plot.png')
plot(squid$DML, squid$weight.sqrt)
dev.off()


#viii.
# note: Fmaturity is the recoded maturity.stage variable cerated in (ii)
boxplot(DML ~ Fmaturity, data = squid, xlab = "maturity stage", ylab = "DML")

# violin plot
library(vioplot)
vioplot(DML ~ Fmaturity, data = squid, xlab = "maturity stage", ylab = "DML", col = "lightblue")


#ix.
coplot(weight.sqrt ~ DML | Fmaturity, data = squid)

# using xyplot from the lattice package
library(lattice)
xyplot(weight.sqrt ~ DML | Fmaturity, data = squid)


#x. 
# vanilla pairs plot
pairs(squid[, c(5, 8, 9, 11, 12, 13)])

# customize the plot. You will need to define the panel.hist and panel.cor functions first. see the ?pairs help file

pairs(squid[, c(5, 8, 9, 11, 12, 13)], diag.panel = panel.hist, upper.panel = panel.cor, lower.panel = panel.smooth)


# 4. Data Visualization with ggplot2

#1.
patients <- read_tsv("patient-data-cleaned.txt")

#2.
ggplot(data = patients, mapping = aes(x = BMI, y = Weight, colour = Height)) +
  geom_point()

#3.
ggplot(data = patients, mapping = aes(x = BMI, y = Weight, colour = Height)) +
  geom_point() +
  geom_smooth()

#4.
ggplot(data = patients, mapping = aes(x = BMI, y = Weight, colour = Height)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)

#5.
ggplot(data = patients, mapping = aes(x = Smokes, y = Score)) +
  geom_boxplot()

#6.
ggplot(data = patients, mapping = aes(x = Smokes, y = Score, colour = Sex)) +
  geom_boxplot()

#7.
patients$Age <- factor(patients$Age)
ggplot(data = patients, mapping = aes(x = Sex, y = Score, fill = Age)) +
  geom_violin()

#8.
ggplot(data = patients, mapping = aes(x = BMI)) +
  geom_histogram(fill = "blue", binwidth = 0.5)

#9.
ggplot(data = patients, mapping = aes(x = BMI)) +
  geom_density()

#10.
ggplot(data = patients, mapping = aes(x = BMI)) +
  geom_density(aes(fill = Sex), alpha = 0.5)

#11.
ggplot(data = patients, mapping = aes(x = BMI, y = Weight, colour = Height)) +
  geom_point() +
  facet_grid(Sex ~ Smokes)

#12.
ggplot(data = patients, mapping = aes(x = Smokes, y = BMI, fill = Sex)) +
  geom_boxplot() +
  facet_wrap(~ Age)

#13.
ggplot(data = patients, mapping = aes(x = Sex, y = BMI, fill = Age)) +
  geom_boxplot() +
  facet_wrap(~ Smokes)

#14.
ggplot(data = patients, mapping = aes(x = BMI, y = Weight)) +
  geom_point()

#15.
ggplot(data = patients, mapping = aes(x = BMI, y = Weight)) +
  geom_point() +
  scale_x_continuous(breaks = c(20, 30, 40), limits = c(20, 40)) +
  scale_y_continuous(breaks = seq(60, 100, by = 5), label = seq(60, 100, by = 5), name = "Weight (kg)")

#16.
ggplot(data = patients, mapping = aes(x = Age, y = BMI, fill = Age)) +
  geom_violin() +
  scale_fill_brewer(palette = "Blues")

#17.
ggplot(data = patients, mapping = aes(x = BMI, y = Weight, colour = Height)) +
  geom_point() +
  scale_colour_gradient2(low = "green", high = "red", mid = "grey", midpoint = mean(patients$Height))

#18.
ggplot(data = patients, mapping = aes(x = BMI, y = Weight, colour = Age)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)

#19.
ggplot(data = patients, mapping = aes(x = BMI, y = Weight, colour = Age)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  theme(
    legend.title = element_blank(),
    legend.key = element_rect(fill = "white"),
    legend.position = "bottom"
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
ggsave("BMI_vs_Weight.png", units = "in", height = 7, width = 7)



## 5. Data Visualization using ggplot2 Part II 

#i.
library(ggplot2)
#Create the data
x <- seq(-6*pi, 6*pi, length.out = 100)
#ggplot2 needs the data to be in a data.frame or tibble with all the data.
dat <- data.frame(x = x, y = sin(x)/x)
ggplot(data = dat, mapping = aes(x = x, y = y)) + 
  geom_line()

#ii.

data(cars)

#basic version with points
ggplot(cars, mapping = aes(x = speed, y = dist)) + 
  geom_point() 

#Add color as a mapping that is only used by geom_points
ggplot(cars, mapping = aes(x = speed, y = dist)) + 
  geom_point(mapping = aes(color = dist > 80))

# define the colors using a manual color scale
ggplot(cars, mapping = aes(x = speed, y = dist)) + 
  geom_point(mapping = aes(color = dist > 80)) + 
  scale_color_manual(values = c("black", "red"))

# add a second geom that produces a smoothed line (default is a local polynomial regression)
ggplot(cars, mapping = aes(x = speed, y = dist)) + 
  geom_point(mapping = aes(color = dist > 80)) + 
  scale_color_manual(values = c("black", "red")) +
  geom_smooth()




