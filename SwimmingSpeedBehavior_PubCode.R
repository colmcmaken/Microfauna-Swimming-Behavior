###Trajectory Analysis
#Load library
library(trajr) #swimming behavior metrics
library(tidyverse) #contains ggplot2, dplyr, tidyr, readr, purrr, tibble, stringr, and forcats
library(ggpubr) #plot significance on ggplot
library(psych) #descriptive stats
library(rstatix) #Shapiro-Wilk test
library(pgirmess) #Krystal-Wallis multiple comparisons test
library(EnvStats) #Rosner outliers test

##Set working directory to file containing video .txt files 
#Not the full file, select the file within the main file
#In toolbar: Session > Set Working Directory > Choose Directory
#Example:
setwd("/Users/gribblelab/Library/CloudStorage/OneDrive-MarineBiologicalLaboratory/Swimming Speed & Behavior/RData_24Mar2023/Elamipretide")

##############Code for Calibration of Files

##Import files to be calibrated
#Instant Speed
Instant_Speed_1 = read.delim(file="Instant_Speed_1.txt", header=TRUE, sep="\t")
#Instant Acceleration
Instant_Accel_1 = read.delim(file="Instant_Accel_1.txt", header=TRUE, sep="\t")
#Tracking_RealSpace
Tracking_RealSpace_1 = read.delim(file="Tracking_RealSpace_1.txt", header=TRUE, sep="\t")

#Multiply Instant Speed by Calibration Factor (obtained from ImageJ)
Instant_Speed_1$Current.Speed.Calibrated=Instant_Speed_1$Current.Speed..mm.sec.*0.0011521
#Multiply Instant Acceleration by Calibration Factor
Instant_Accel_1$Current.Accel.Calibrated=Instant_Accel_1$Current.Accel..mm.s.2.*0.0011521
#Multiply Pos.X and Pos.Y by Calibration Factor
Tracking_RealSpace_1$Pos.X.Calibrated=Tracking_RealSpace_1$Pos..X..mm.*0.0011521
Tracking_RealSpace_1$Pos.Y.Calibrated=Tracking_RealSpace_1$Pos..Y..mm.*0.0011521

##Export Files as CSV (can be opened and used in Excel)
write.csv(Instant_Speed_1, file='Instant_Speed_1.csv')
write.csv(Instant_Accel_1, file='Instant_Accel_1.csv')
write.csv(Tracking_RealSpace_1, file='Tracking_RealSpace_1.csv')

##############Code for Swimming Speed and Behavior

#Upload X,Y position file
Tracking_RealSpace_1 = read.csv(file="Tracking_RealSpace_1.csv", header=TRUE)

#Create Empty Matrix
Trajr_Data = matrix(ncol = 12)
#Relabel Column Names
colnames(Trajr_Data) = c("Straightness", "Sinuosity", "Emax", "MeanDC", "SDDC", "Turning.Angle.Mean", "Turning.Angle.Max", "Direction.Ratio", "Distance", "Length", "Zero.Movement", "Moving.Percentage")


#####Trajr LOOP for each unique track
for (i in unique(Tracking_RealSpace_1$Track)) {
  Track = subset(Tracking_RealSpace_1, Track == i)
  Track_xy = Track[,c(8,9,2)]
  Track_xy$Time..sec.[1] = 0
  trj = TrajFromCoords(Track_xy, xCol = 1, yCol = 2, timeCol = 3, fps = 30, spatialUnits = "mm", timeUnits = "s")
  trj_smoothed = TrajSmoothSG(trj, p = 3, n = 31)
  Straightness = TrajStraightness(trj_smoothed)
  #value of 1.0 = straight
  Sinuosity = TrajSinuosity2(trj_smoothed, compass.direction = NULL)
  #value of 0.0 = straight
  Emax = TrajEmax(trj_smoothed, eMaxB = FALSE, compass.direction = NULL)
  #value of 0.0 = sinuous
  trj_DC = TrajDirectionalChange(trj_smoothed, nFrames = 1)
  MeanDC = mean(trj_DC)
  #value of 0.0 = linear
  SDDC = sd(trj_DC)
  #value of 0.0 = regular
  trj_TA = TrajAngles(trj_smoothed, lag = 1, compass.direction = NULL)
  Turning.Angle.Mean = mean(trj_TA)
  Turning.Angle.Max = max(abs(trj_TA)) #regardless of sign (+/-)
  Direction.Ratio = (length(trj_TA[trj_TA>0])/length(trj_TA[trj_TA<0])) 
  #value greater than 1 = more clockwise
  #value less than 1 = more counterclockwise
  Distance = TrajDistance(trj_smoothed, startIndex = 1, endIndex = nrow(trj_smoothed))
  Length = TrajLength(trj_smoothed, startIndex = 1, endIndex = nrow(trj_smoothed))
  trj_0M = TrajSpeedIntervals(
    trj_smoothed,
    fasterThan = -0.01,
    slowerThan = 0.01,
    interpolateTimes = TRUE,
    diff = c("backward", "central", "forward"))
  Zero.Movement = sum(trj_0M$duration)
  Moving.Percentage = ((30-Zero.Movement)/30)*100
  ##Adjust "30" to match time length of video recorded
  #Compile all data
  Compiled = data.frame(Straightness, Sinuosity, Emax, MeanDC, SDDC, Turning.Angle.Mean, Turning.Angle.Max, Direction.Ratio, Distance, Length, Zero.Movement, Moving.Percentage)
  #Save as Dataframe
  Trajr_Data = rbind(Trajr_Data, Compiled)
}

#Remove empty rows
Trajr_Data = Trajr_Data[-1,]
#Reset row names
rownames(Trajr_Data) = NULL


#Upload calibrated speed and acceleration files from ToxTrac
Instant_Speed_1 = read.csv(file="Instant_Speed_1.csv", header=TRUE)
Instant_Accel_1 = read.csv(file="Instant_Accel_1.csv", header=TRUE)

#Create Empty Matrices
Speed = matrix(ncol = 2)
Acceleration = matrix(ncol = 2)
#Relabel Column Names
colnames(Speed) = c("Speed.Mean", "Speed.Max")
colnames(Acceleration) = c("Acceleration.Mean", "Acceleration.Max")

#Speed LOOP for each unique track
for (i in unique(Instant_Speed_1$Track)) {
  Speed.Mean = mean(Instant_Speed_1$Current.Speed.Calibrated[Instant_Speed_1$Track == i])
  Speed.Max = max(Instant_Speed_1$Current.Speed.Calibrated[Instant_Speed_1$Track == i])
  #Compile all data
  Compiled = data.frame(Speed.Mean, Speed.Max)
  Speed = rbind(Speed, Compiled)
}

#Acceleration LOOP for each unique track
for (i in unique(Instant_Accel_1$Track)) {
  Acceleration.Mean = mean(Instant_Accel_1$Current.Accel.Calibrated[Instant_Accel_1$Track == i])
  Acceleration.Max = max(Instant_Accel_1$Current.Accel.Calibrated[Instant_Accel_1$Track == i])
  #Compile all data
  Compiled = data.frame(Acceleration.Mean, Acceleration.Max)
  Acceleration = rbind(Acceleration, Compiled)
}

#Compile Speed and Acceleration
Speed_Accel = data.frame(Speed, Acceleration)
#Remove empty rows
Speed_Accel = Speed_Accel[-1,]
#Reset row names
rownames(Speed_Accel) = NULL

##Compile all data into one data frame
Trajr_Data = data.frame(Speed_Accel, Trajr_Data)
##Export data frame to working directory
write.csv(Trajr_Data,"Trajr_Data.csv")


###LOOP Plot and Save Figures
for (i in unique(Tracking_RealSpace_1$Track)) {
  Track = subset(Tracking_RealSpace_1, Track == i)
  Track_xy = Track[,c(8,9,2)]
  Track_xy$Time..sec.[1] = 0
  trj = TrajFromCoords(Track_xy, xCol = 1, yCol = 2, timeCol = 3, fps = 30, spatialUnits = "mm", timeUnits = "s")
  trj_smoothed = TrajSmoothSG(trj, p = 3, n = 31)
  #Plot original track
  file_name1 = paste("Track", i, ".tiff", sep="")
  tiff(file_name1, height = 20, width = 30, units='cm', compression = "lzw", res = 300)
  plot(trj)
  dev.off()
  #Plot smoothed track
  file_name2 = paste("Track", i, "_smoothed", ".tiff", sep="")
  tiff(file_name2, height = 20, width = 30, units='cm', compression = "lzw", res = 300)
  plot(trj, lwd = 1, lty = 1)
  lines(trj_smoothed, col = "red", lwd = 2)
  dev.off()
  #Calculate speed and acceleration derivatives
  derivs = TrajDerivatives(trj_smoothed)
  #Plot change-in-speed and speed 
  file_name3 = paste("Track", i, "_SpeedAccel", ".tiff", sep="")
  tiff(file_name3, height = 20, width = 30, units='cm', compression = "lzw", res = 300)
  par(mar = c(5, 4, 4, 4) + 0.3) 
  plot(derivs$acceleration ~ derivs$accelerationTimes, type = 'l', col = 'red', 
       yaxt = 'n',
       xlab = 'Time (s)',
       ylab = expression(paste('Acceleration (', mm/s^2, ')')))
  axis(side = 2, col = "red")
  lines(derivs$speed ~ derivs$speedTimes, col = 'blue')
  axis(side = 4, col = "blue")
  mtext('Speed (mm/s)', side = 4, line = 3)
  abline(h = 0, col = 'lightGrey')
  title(main = paste("Track", i))
  dev.off()
}

#####Clear environment prior to setting new working directory :)






###############################################################################
####AFTER ALL VIDEOS ARE ANALYZED
#Compile all tracking data into one excel file (save as .csv)
#Make sure there are columns with metadata (identification factors)
##Set working directory to main file with compiled data
setwd()
##Upload compiled data
Compiled_Data = read.csv(file="Compiled_Data.csv", header=TRUE, row.names = 1)

####Descriptive Statistics for each tracking metric
descriptive.stats = describeBy(x=Compiled_Data[,-1], group=Compiled_Data$Factor, mat=TRUE)
write.csv(descriptive.stats, file = "Descriptive_Statistics.csv")


####Test for Outliers
#Check boxplots for outliers
for (col_name in colnames(Compiled_Data)[-1]) {
  boxplot<-ggplot(Compiled_Data, aes(x = Factor, y = Compiled_Data[, col_name])) +
    geom_boxplot() +
    labs(title = paste("Box Plot for", col_name), x = "Factor", y = col_name)
  plot(boxplot)
}
###Count how many potential outliers (dots beyond the minimum and maximum whiskers) there are in each plot.

#Rosner Test for Outliers: Determine if potential outliers need to be removed
##“Metric” represents the swimming behavior metric you want to analyze for significance.
##Change k value to equal the number of potential outliers identified in the boxplots.
##Copy the following code for each swimming behavior metric with potential outliers.

rosnerTest(Compiled_Data$Metric, k = 1)

#####Results Interpretation
#If Outlier column = TRUE --> remove outlier from data


#Remove outliers from dataset by converting them to “NA”
#Create a copy of the original data frame for reference or if issues occur
Compiled_Data_original = Compiled_Data

##From rosnerTest() output use the “Obs.Num” column for “row” location in matrix
##Use metric column within data matrix for “column”
Compiled_Data[row, column] <- NA



####Statistical Test Determination
##Shapiro-Wilk Test : Normality of Data
shapiro.pass = list()
for (z in colnames(Compiled_Data[,-1])){
    shapiro = Compiled_Data %>% group_by(Factor) %>% shapiro_test(z)
      shapiro.list = if (any(shapiro$p<=0.05)) { print("Nonparametric") } 
           else { print("Parametric") }
      shapiro.pass[z] = shapiro.list
}
shapiro.pass = t(as.data.frame(shapiro.pass))
colnames(shapiro.pass) = c("Pass")
##Bartlett's Test: Homogeneity of Variances
bartlett = lapply(Compiled_Data[,-1], bartlett.test, Compiled_Data$Factor)
bartlett.pvalue  = data.frame(sapply(bartlett, getElement, "p.value"))
colnames(bartlett.pvalue) = c("p.value") #Shorten long column name
##Creates dataframe to keep track of which test to use for each metric
determine.test = merge(shapiro.pass, bartlett.pvalue, by = "row.names")
determine.test[,4] = ifelse((determine.test$Pass == "Parametric")&(determine.test$p.value > 0.05), "OneWayANOVA", "Kruskal")
colnames(determine.test) = c("metric", "shapiro", "bartlett", "Test") #Shorten long column name






################################################################################
#For this section, use the given code for each metric that corresponds with the 
#"Test" column identified in "determine.test" dataframe
#Change the word "Metric" in the code to match the metric of interest


##Parametric Test: One-Way ANOVA
model_Metric = aov(Metric ~ Factor, data=Compiled_Data)
anova(model_Metric)
#####Results Interpretation
#If p-value is less than 0.05 = significant differences between groups = do pairwise test
#If p-value is greater than 0.05 = no significant differences between groups

##Pairwise Test for Significant One-Way ANOVA results: Tukey's Test
#Trajectory Length
TukeyHSD(model_Metric)


##Non-parametric Test: Kruskal-Wallis
#Speed.Mean
kruskal.test(Metric ~ Factor, data=Compiled_Data)
#####Results Interpretation
#If p-value is less than 0.05 = significant differences between groups = do pairwise test
#If p-value is greater than 0.05 = no significant differences between groups

##Pairwise Test for Significant Kruskal-Wallis results: Multiple-Comparison Test
#Speed
kruskalmc(Metric ~ Factor, data=Compiled_Data)


####Plotting Results
##Specify the comparisons you want in the graph
  ###Change "Control" and "Factor" to match experiment factor group labels
my_comparisons = list(c("Control", "Factor1"), c("Control", "Factor2"))
#Create Plots with Significance 
###Use tutorial/examples to help select what test/comparison type to add
###http://rpkgs.datanovia.com/ggpubr/reference/stat_compare_means.html 

#Parametric Data (One-Way ANOVA metrics)
ggdotplot(Compiled_Data, x = "Factor", y = "Metric") +
  geom_dotplot(binaxis="y", stackdir="center", dotsize = 1) + 
  ylab("Metric (unit)") +
  theme(axis.title.x = element_text(margin = margin(t = 15), size = 35), 
        axis.title.y = element_text(margin = margin(r = 15), size = 35), 
        axis.text.x = element_text(size = 35),
        axis.text.y = element_text(size = 30)) +
  stat_summary(fun.data = "median_q1q3", geom = "errorbar", size = 1.5, colour = "red", width = 0.3) +
  stat_summary(fun.y = "median", geom = "crossbar", size = 1, colour = "red", width = 0.4) +
  geom_pwc(ref.group = "Control",
           method = "t.test",
           label = "p.signif", 
           hide.ns = TRUE,
           tip.length = 0.03,
           size = 0.5, #bracket size
           label.size = 10,
           vjust = 0.5)
####Export > Save as PDF > US Letter for consistency

#Nonparametric Data (Kruskal-Wallis metrics)
ggdotplot(Compiled_Data, x = "Factor", y = "Metric") +
  geom_dotplot(binaxis="y", stackdir="center", dotsize = 1) + 
  ylab("Metric (unit)") +
  theme(axis.title.x = element_text(margin = margin(t = 15), size = 35), 
        axis.title.y = element_text(margin = margin(r = 15), size = 35), 
        axis.text.x = element_text(size = 35),
        axis.text.y = element_text(size = 30)) +
  stat_summary(fun.data = "median_q1q3", geom = "errorbar", size = 1.5, colour = "red", width = 0.3) +
  stat_summary(fun.y = "median", geom = "crossbar", size = 1, colour = "red", width = 0.4) +
  geom_pwc(ref.group = "Control",
           method = "wilcox.test",
           label = "p.signif", 
           hide.ns = TRUE,
           tip.length = 0.03,
           size = 0.5, #bracket size
           label.size = 10,
           vjust = 0.5)
####Export > Save as PDF > US Letter for consistency

