
# STUDENT ID:
student_id<- # write your student id
# GROUP NAME:
group<-  # write your group name
  
# Provide the correct answers by filling in the gaps.
  

# 3. Data Visualization with Base R

#ii.
squid <- read.table('squid1.txt', header =_,stringsAsFactors = TRUE) # we expect the headers to be returned
squid_structure=_(squid) # Get the structure of the dataframe
squid_summary=_(squid) # Get the summary of the dataframe

# convert variables to factors
squid$Fmaturity <- _(squid$maturity.stage) # fill in the gap to convert to a factor variable
squid$Fmonth <- _(squid$month) # fill in the gap to convert to a factor variable
squid$Fyear <- _(squid$year) # fill in the gap to convert to a factor variable

str(squid)


#iii.
cont_table=_(squid$Fmonth, squid$Fyear) # fill in the gap to get a contingency table
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
dotplot1<-dotplot(as.matrix(squid[,c("DML", "weight", "nid.length", "ovary.weight")]),
        groups=FALSE,
        strip = strip.custom(bg = 'white',
                             par.strip.text = list(cex = 0.8)),
        scales = list(x = list(relation = "free"),
                      y = list(relation = "free"),
                      draw = FALSE),
        col=1, cex  =0.5, pch = 16,
        _ = "Value of the variable", # the x-axis label
        _ = "Order of the data from text file") # the y-axis label


# v.
index=_(squid$nid.length > 400) # Fill in the gap with the correct code.
squid$nid.length[11]
squid$nid.length[11] <- _ # fill in the gap to replace 430.2 with 43.2
squid$nid.length[11]
dotchart(squid$nid.length, main = "nid length")

# vi.
pdf('workshop/figures/ex4_hist.pdf')
par(mfrow = c(2,2))
plot1=_(squid$DML, main="", xlab = "DML") # Plot a histogram
plot2=_(squid$weight, main="", xlab = "weight") # Plot a histogram
plot3=_(squid$eviscerate.weight, main="", xlab = "eviscerate weight") # Plot a histogram
plot4=_(squid$ovary.weight, main="", xlab = "ovary weight") # Plot a histogram
dev.off()

# need to get the min and max values for DML to work out the limits for the breaks

summary1=summary(squid__) # Get the summary of DML variable using the dollar sign indexing/referencing

# experimenting with different breaks
par(mfrow = c(2,2))
brk1 <- seq(from = 80, to = 340, by = 20)   
plot5=_(squid$DML, xlab = "DML", breaks = brk1, main = "brk: 20") #fill in the gap

brk2 <- seq(from = 80, to = 340, by = 10)   
plot6=_(squid$DML, xlab = "DML", breaks = brk2, main = "brk: 10") #fill in the gap

brk3 <- seq(from = 80, to = 340, by = 5)   
plot7=_(squid$DML, xlab = "DML", breaks = brk3, main = "brk: 5") #fill in the gap

brk4 <- seq(from = 80, to = 340, by = 2)   
plot8=_(squid$DML, xlab = "DML", breaks = brk4, main = "brk: 2") #fill in the gap


#vii.
# clearly not linear
plot9=plot(_, _) # plot DML against weight and use dollar sign referencing

# natural log and sqrt tranform weight
squid$weight.sqrt <- _(squid$weight) # find the square root
squid$weight.log <- _(squid$weight) # find the natural logarithm transformation

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
plot10=_(_ ~ Fmaturity, _ = squid, xlab = "maturity stage", ylab = "DML") # Plot a boxplot of DML against Fmaturity

# violin plot
library(vioplot)
plot11=vioplot(DML ~ Fmaturity, _ = squid, xlab = "maturity stage", ylab = "DML", col = "lightblue")


#ix.
plot12=coplot(weight.sqrt ~ DML | Fmaturity, _ = squid) # fill in the gap

# using xyplot from the lattice package
library(lattice)
plot13=xyplot(weight.sqrt ~ DML | Fmaturity, _ = squid) # fill in the gap


#x. 
# vanilla pairs plot
plot14=pairs(_[, _(5, 8, 9, 11, 12, 13)]) # fill in the gaps

# customize the plot. You will need to define the panel.hist and panel.cor functions first. see the ?pairs help file

plot15=pairs(_[, _(5, 8, 9, 11, 12, 13)], diag.panel = panel.hist, upper.panel = panel.cor, lower.panel = panel.smooth)  # fill in the gaps









