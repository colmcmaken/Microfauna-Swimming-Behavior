###############################################################################
####AFTER ALL VIDEOS ARE ANALYZED
#Compile all tracking data into one excel file (save as .csv)
#Make sure there are columns with metadata (identification factors)
##Set working directory to main file with compiled data
setwd("/Users/gribblelab/Library/CloudStorage/OneDrive-MarineBiologicalLaboratory/Swimming Speed & Behavior/RData_24Mar2023")
##Upload compiled data
Compiled_Data = read.csv(file="Compiled_Data.csv", header=TRUE, row.names = 1)

####Descriptive Statistics for each tracking metric
descriptive.stats = describeBy(x=Compiled_Data[,-1], group=Compiled_Data$Treatment, mat=TRUE)
write.csv(descriptive.stats, file = "Descriptive_Statistics.csv")


####Statistical Test Determination
##Shapiro-Wilk Test : Normality of Data
shapiro.pass = list()
for (z in colnames(Compiled_Data[,-1])){
  shapiro = Compiled_Data %>% group_by(Treatment) %>% shapiro_test(z)
  shapiro.list = if (any(shapiro$p<=0.05)) { print("Nonparametric") } 
  else { print("Parametric") }
  shapiro.pass[z] = shapiro.list
}
shapiro.pass = t(as.data.frame(shapiro.pass))
colnames(shapiro.pass) = c("Pass")
##Bartlett's Test: Homogeneity of Variances
bartlett = lapply(Compiled_Data[,-1], bartlett.test, Compiled_Data$Treatment)
bartlett.pvalue  = data.frame(sapply(bartlett, getElement, "p.value"))
colnames(bartlett.pvalue) = c("p.value") #Shorten long column name
##Creates dataframe to keep track of which test to use for each metric
determine.test = merge(shapiro.pass, bartlett.pvalue, by = "row.names")
determine.test[,4] = ifelse((determine.test$Pass == "Parametric")&(determine.test$p.value > 0.05), "OneWayANOVA", "Kruskal")
colnames(determine.test) = c("metric", "shapiro", "bartlett", "Test") #Shorten long column name


####Test for Outliers
#Check boxplots for outliers
for (col_name in colnames(Compiled_Data)[-1]) {
  boxplot<-ggplot(Compiled_Data, aes(x = Treatment, y = Compiled_Data[, col_name])) +
    geom_boxplot() +
    labs(title = paste("Box Plot for", col_name), x = "Treatment", y = col_name)
  plot(boxplot)
}
###Count how many potential outliers (dots beyond the minimum and maximum) there are in each plot

#Determine if potential outliers need to be removed
##Rosner Test for Outliers
rosnerTest(Compiled_Data$Speed.Max, k = 5)
rosnerTest(Compiled_Data$Acceleration.Max, k = 1)
rosnerTest(Compiled_Data$Sinuosity, k = 2)
rosnerTest(Compiled_Data$Acceleration.Max, k = 1)
rosnerTest(Compiled_Data$Emax, k = 5)   #4 TRUE
rosnerTest(Compiled_Data$MeanDC, k = 1)
rosnerTest(Compiled_Data$SDDC, k = 3)
rosnerTest(Compiled_Data$Turning.Angle.Max, k = 2)  #2 TRUE
rosnerTest(Compiled_Data$Direction.Ratio, k = 3) #2 TRUE
rosnerTest(Compiled_Data$Zero.Movement, k = 3) #3 TRUE
rosnerTest(Compiled_Data$Moving.Percentage, k = 3) #3 TRUE

#####Results Interpretation
#If Outlier column = TRUE --> remove outlier from data

##Convert the outliers to NA in each column
#From rosnerTest() output use the Obs.Num column for row location in matrix
#Create a copy of the original data frame for reference
Compiled_Data_original = Compiled_Data
#Emax
Compiled_Data[20, 8] <- NA
Compiled_Data[17, 8] <- NA
Compiled_Data[10, 8] <- NA
Compiled_Data[24, 8] <- NA
#Turning.Angle.Max
Compiled_Data[20, 12] <- NA
Compiled_Data[17, 12] <- NA
#Direction.Ratio
Compiled_Data[9, 13] <- NA
Compiled_Data[12, 13] <- NA
#Zero.Movement
Compiled_Data[19, 16] <- NA
Compiled_Data[3, 16] <- NA
Compiled_Data[6, 16] <- NA
#Moving.Percentage
Compiled_Data[19, 17] <- NA
Compiled_Data[3, 17] <- NA
Compiled_Data[6, 17] <- NA



##Parametric Test: One-Way ANOVA
#Trajectory Distance
model_distance = aov(Distance ~ Treatment, data=Compiled_Data)
anova(model_distance)
#Speed.Mean
model_speed.mean = aov(Speed.Mean ~ Treatment, data=Compiled_Data)
anova(model_speed.mean)
#Straightness
model_straightness = aov(Straightness ~ Treatment, data=Compiled_Data)
anova(model_straightness)

#####Results Interpretation
#If p-value is less than 0.05 = significant differences between groups = do pairwise test
#If p-value is greater than 0.05 = no significant differences between groups

##Pairwise Test for Significant One-Way ANOVA results: Tukey's Test
#Trajectory Length
TukeyHSD(model_speed.mean)


##Non-parametric Test: Kruskal-Wallis
#Acceleartion.Max
kruskal.test(Acceleration.Max ~ Treatment, data=Compiled_Data)
#Acceleartion.Mean
kruskal.test(Acceleration.Mean ~ Treatment, data=Compiled_Data)
#Direction.Ratio
kruskal.test(Direction.Ratio ~ Treatment, data=Compiled_Data)
#Emax
kruskal.test(Emax ~ Treatment, data=Compiled_Data)
#Trajectory Length
kruskal.test(Length ~ Treatment, data=Compiled_Data)
#Mean Directional Change
kruskal.test(MeanDC ~ Treatment, data=Compiled_Data)
#Moving Percentage
kruskal.test(Moving.Percentage ~ Treatment, data=Compiled_Data)
#SD Directional Change
kruskal.test(SDDC ~ Treatment, data=Compiled_Data)
#Sinuosity
kruskal.test(Sinuosity ~ Treatment, data=Compiled_Data)
#Speed.Max
kruskal.test(Speed.Max ~ Treatment, data=Compiled_Data)
#Turning.Angle.Max
kruskal.test(Turning.Angle.Max ~ Treatment, data=Compiled_Data)
#Turning.Angle.Mean
kruskal.test(Turning.Angle.Mean ~ Treatment, data=Compiled_Data)
#Zero.Movement
kruskal.test(Zero.Movement ~ Treatment, data=Compiled_Data)

#####Results Interpretation
#If p-value is less than 0.05 = significant differences between groups = do pairwise test
#If p-value is greater than 0.05 = no significant differences between groups

##Pairwise Test for Significant Kruskal-Wallis results: Multiple-Comparison Test
#Speed
kruskalmc(Acceleration.Max ~ Treatment, data=Compiled_Data)
kruskalmc(Acceleration.Mean ~ Treatment, data=Compiled_Data)
kruskalmc(Length ~ Treatment, data=Compiled_Data)
kruskalmc(Moving.Percentage ~ Treatment, data=Compiled_Data)
kruskalmc(Sinuosity ~ Treatment, data=Compiled_Data)
kruskalmc(Speed.Max ~ Treatment, data=Compiled_Data)
kruskalmc(Zero.Movement ~ Treatment, data=Compiled_Data)




####Plotting Results
##Specify the comparisons you want in the graph
###Change "Control" and "Factor" to match experiment factor group labels
my_comparisons = list(c("Instant Ocean", "Rotenone"), c("Instant Ocean", "Elamipretide"))
#Create Plots with Significance 
###Use tutorial/examples to help select what test/comparison type to add
###http://rpkgs.datanovia.com/ggpubr/reference/stat_compare_means.html 


##Parametric Data (One-Way ANOVA metrics)
#Distance
ggdotplot(Compiled_Data, x = "Treatment", y = "Distance") +
  geom_dotplot(binaxis="y", stackdir="center", dotsize = 1) + 
  ylab("Distance (mm)") +
  scale_y_continuous(limits = c(0, NA)) +
  theme(axis.title.x = element_text(margin = margin(t = 15), size = 35), 
        axis.title.y = element_text(margin = margin(r = 15), size = 35), 
        axis.text.x = element_text(size = 35),
        axis.text.y = element_text(size = 30)) +
  stat_summary(fun.data = "median_q1q3", geom = "errorbar", size = 1.5, colour = "red", width = 0.3) +
  stat_summary(fun = "median", geom = "crossbar", size = 1, colour = "red", width = 0.4) +
  geom_pwc(ref.group = "Instant Ocean",
           method = "t.test",
           label = "p.signif", 
           hide.ns = TRUE,
           tip.length = 0.03,
           size = 0.5, #bracket size
           label.size = 10,
           vjust = 0.5)
####Export > Save as PDF > US Letter for consistency

#Speed.Mean
ggdotplot(Compiled_Data, x = "Treatment", y = "Speed.Mean") +
  geom_dotplot(binaxis="y", stackdir="center", dotsize = 1) + 
  ylab("Speed (mm/s)") +
  scale_y_continuous(limits = c(0, NA)) +
  theme(axis.title.x = element_text(margin = margin(t = 15), size = 35), 
        axis.title.y = element_text(margin = margin(r = 15), size = 35), 
        axis.text.x = element_text(size = 35),
        axis.text.y = element_text(size = 30)) +
  stat_summary(fun.data = "median_q1q3", geom = "errorbar", size = 1.5, colour = "red", width = 0.3) +
  stat_summary(fun = "median", geom = "crossbar", size = 1, colour = "red", width = 0.4) +
  geom_pwc(ref.group = "Instant Ocean",
           method = "t.test",
           label = "p.signif", 
           hide.ns = TRUE,
           tip.length = 0.03,
           size = 0.5, #bracket size
           label.size = 10,
           vjust = 0.5)
####Export > Save as PDF > US Letter for consistency

#Straightness
ggdotplot(Compiled_Data, x = "Treatment", y = "Straightness") +
  geom_dotplot(binaxis="y", stackdir="center", dotsize = 1) + 
  ylab("Straightness Index") +
  scale_y_continuous(limits = c(0, NA)) +
  theme(axis.title.x = element_text(margin = margin(t = 15), size = 35), 
        axis.title.y = element_text(margin = margin(r = 15), size = 35), 
        axis.text.x = element_text(size = 35),
        axis.text.y = element_text(size = 30)) +
  stat_summary(fun.data = "median_q1q3", geom = "errorbar", size = 1.5, colour = "red", width = 0.3) +
  stat_summary(fun = "median", geom = "crossbar", size = 1, colour = "red", width = 0.4) +
  geom_pwc(ref.group = "Instant Ocean",
           method = "t.test",
           label = "p.signif", 
           hide.ns = TRUE,
           tip.length = 0.03,
           size = 0.5, #bracket size
           label.size = 10,
           vjust = 0.5)
####Export > Save as PDF > US Letter for consistency



##Nonparametric Data (Kruskal-Wallis metrics)
#Acceleration.Max
ggdotplot(Compiled_Data, x = "Treatment", y = "Acceleration.Max") +
  geom_dotplot(binaxis="y", stackdir="center", dotsize = 1) + 
  ylab("Acceleration (mm/s^2)") +
  scale_y_continuous(limits = c(0, NA)) +
  theme(axis.title.x = element_text(margin = margin(t = 15), size = 35), 
        axis.title.y = element_text(margin = margin(r = 15), size = 35), 
        axis.text.x = element_text(size = 35),
        axis.text.y = element_text(size = 30)) +
  stat_summary(fun.data = "median_q1q3", geom = "errorbar", size = 1.5, colour = "red", width = 0.3) +
  stat_summary(fun.y = "median", geom = "crossbar", size = 1, colour = "red", width = 0.4) +
  geom_pwc(ref.group = "Instant Ocean",
           method = "wilcox.test",
           label = "p.signif", 
           hide.ns = TRUE,
           tip.length = 0.03,
           size = 0.5, #bracket size
           label.size = 10,
           vjust = 0.5)
####Export > Save as PDF > US Letter for consistency

#Acceleration.Mean
ggdotplot(Compiled_Data, x = "Treatment", y = "Acceleration.Mean") +
  geom_dotplot(binaxis="y", stackdir="center", dotsize = 1) + 
  ylab("Acceleration (mm/s^2)") +
  scale_y_continuous(limits = c(0, NA)) +
  theme(axis.title.x = element_text(margin = margin(t = 15), size = 35), 
        axis.title.y = element_text(margin = margin(r = 15), size = 35), 
        axis.text.x = element_text(size = 35),
        axis.text.y = element_text(size = 30)) +
  stat_summary(fun.data = "median_q1q3", geom = "errorbar", size = 1.5, colour = "red", width = 0.3) +
  stat_summary(fun.y = "median", geom = "crossbar", size = 1, colour = "red", width = 0.4) +
  geom_pwc(ref.group = "Instant Ocean",
           method = "wilcox.test",
           label = "p.signif", 
           hide.ns = TRUE,
           tip.length = 0.03,
           size = 0.5, #bracket size
           label.size = 10,
           vjust = 0.5)
####Export > Save as PDF > US Letter for consistency

#Direction.Ratio
ggdotplot(Compiled_Data, x = "Treatment", y = "Direction.Ratio") +
  geom_dotplot(binaxis="y", stackdir="center", dotsize = 1) + 
  ylab("Direction Ratio") +
  scale_y_continuous(limits = c(0, NA)) +
  geom_hline(yintercept=1.0,linetype=2) +
  theme(axis.title.x = element_text(margin = margin(t = 15), size = 35), 
        axis.title.y = element_text(margin = margin(r = 15), size = 35), 
        axis.text.x = element_text(size = 35),
        axis.text.y = element_text(size = 30)) +
  stat_summary(fun.data = "median_q1q3", geom = "errorbar", size = 1.5, colour = "red", width = 0.3) +
  stat_summary(fun.y = "median", geom = "crossbar", size = 1, colour = "red", width = 0.4) +
  geom_pwc(ref.group = "Instant Ocean",
           method = "wilcox.test",
           label = "p.signif", 
           hide.ns = TRUE,
           tip.length = 0.03,
           size = 0.5, #bracket size
           label.size = 10,
           vjust = 0.5)
####Export > Save as PDF > US Letter for consistency

#Emax
ggdotplot(Compiled_Data, x = "Treatment", y = "Emax") +
  geom_dotplot(binaxis="y", stackdir="center", dotsize = 1) + 
  ylab("Maximum Expected Displacement") +
  scale_y_continuous(limits = c(0, NA)) +
  theme(axis.title.x = element_text(margin = margin(t = 15), size = 35), 
        axis.title.y = element_text(margin = margin(r = 15), size = 32), 
        axis.text.x = element_text(size = 35),
        axis.text.y = element_text(size = 30)) +
  stat_summary(fun.data = "median_q1q3", geom = "errorbar", size = 1.5, colour = "red", width = 0.3) +
  stat_summary(fun.y = "median", geom = "crossbar", size = 1, colour = "red", width = 0.4) +
  geom_pwc(ref.group = "Instant Ocean",
           method = "wilcox.test",
           label = "p.signif", 
           hide.ns = TRUE,
           tip.length = 0.03,
           size = 0.5, #bracket size
           label.size = 10,
           vjust = 0.5)
####Export > Save as PDF > US Letter for consistency

#Length
ggdotplot(Compiled_Data, x = "Treatment", y = "Length") +
  geom_dotplot(binaxis="y", stackdir="center", dotsize = 1) + 
  ylab("Length (mm)") +
  scale_y_continuous(limits = c(0, NA)) +
  theme(axis.title.x = element_text(margin = margin(t = 15), size = 35), 
        axis.title.y = element_text(margin = margin(r = 15), size = 35), 
        axis.text.x = element_text(size = 35),
        axis.text.y = element_text(size = 30)) +
  stat_summary(fun.data = "median_q1q3", geom = "errorbar", size = 1.5, colour = "red", width = 0.3) +
  stat_summary(fun.y = "median", geom = "crossbar", size = 1, colour = "red", width = 0.4) +
  geom_pwc(ref.group = "Instant Ocean",
           method = "wilcox.test",
           label = "p.signif", 
           hide.ns = TRUE,
           tip.length = 0.03,
           size = 0.5, #bracket size
           label.size = 10,
           vjust = 0.5)
####Export > Save as PDF > US Letter for consistency

#MeanDC
ggdotplot(Compiled_Data, x = "Treatment", y = "MeanDC") +
  geom_dotplot(binaxis="y", stackdir="center", dotsize = 1) + 
  ylab("Mean Directional Change (degrees/s)") +
  scale_y_continuous(limits = c(0, NA)) +
  theme(axis.title.x = element_text(margin = margin(t = 15), size = 35), 
        axis.title.y = element_text(margin = margin(r = 15), size = 30), 
        axis.text.x = element_text(size = 35),
        axis.text.y = element_text(size = 30)) +
  stat_summary(fun.data = "median_q1q3", geom = "errorbar", size = 1.5, colour = "red", width = 0.3) +
  stat_summary(fun.y = "median", geom = "crossbar", size = 1, colour = "red", width = 0.4) +
  geom_pwc(ref.group = "Instant Ocean",
           method = "wilcox.test",
           label = "p.signif", 
           hide.ns = TRUE,
           tip.length = 0.03,
           size = 0.5, #bracket size
           label.size = 10,
           vjust = 0.5)
####Export > Save as PDF > US Letter for consistency

#Moving.Percentage
ggdotplot(Compiled_Data, x = "Treatment", y = "Moving.Percentage") +
  geom_dotplot(binaxis="y", stackdir="center", dotsize = 1) + 
  ylab("Time Moving (%)") +
  scale_y_continuous(limits = c(70, NA)) +
  theme(axis.title.x = element_text(margin = margin(t = 15), size = 35), 
        axis.title.y = element_text(margin = margin(r = 15), size = 35), 
        axis.text.x = element_text(size = 35),
        axis.text.y = element_text(size = 30)) +
  stat_summary(fun.data = "median_q1q3", geom = "errorbar", size = 1.5, colour = "red", width = 0.3) +
  stat_summary(fun.y = "median", geom = "crossbar", size = 1, colour = "red", width = 0.4) +
  geom_pwc(ref.group = "Instant Ocean",
           method = "wilcox.test",
           label = "p.signif", 
           hide.ns = TRUE,
           tip.length = 0.03,
           size = 0.5, #bracket size
           label.size = 10,
           vjust = 0.5)
####Export > Save as PDF > US Letter for consistency

#SDDC
ggdotplot(Compiled_Data, x = "Treatment", y = "SDDC") +
  geom_dotplot(binaxis="y", stackdir="center", dotsize = 1) + 
  ylab("SD Directional Change (degrees/s)") +
  scale_y_continuous(limits = c(0, NA)) +
  theme(axis.title.x = element_text(margin = margin(t = 15), size = 35), 
        axis.title.y = element_text(margin = margin(r = 15), size = 30), 
        axis.text.x = element_text(size = 35),
        axis.text.y = element_text(size = 30)) +
  stat_summary(fun.data = "median_q1q3", geom = "errorbar", size = 1.5, colour = "red", width = 0.3) +
  stat_summary(fun.y = "median", geom = "crossbar", size = 1, colour = "red", width = 0.4) +
  geom_pwc(ref.group = "Instant Ocean",
           method = "wilcox.test",
           label = "p.signif", 
           hide.ns = TRUE,
           tip.length = 0.03,
           size = 0.5, #bracket size
           label.size = 10,
           vjust = 0.5)
####Export > Save as PDF > US Letter for consistency

#Sinuosity
ggdotplot(Compiled_Data, x = "Treatment", y = "Sinuosity") +
  geom_dotplot(binaxis="y", stackdir="center", dotsize = 1) + 
  ylab("Sinuosity Index") +
  scale_y_continuous(limits = c(0, NA)) +
  theme(axis.title.x = element_text(margin = margin(t = 15), size = 35), 
        axis.title.y = element_text(margin = margin(r = 15), size = 35), 
        axis.text.x = element_text(size = 35),
        axis.text.y = element_text(size = 30)) +
  stat_summary(fun.data = "median_q1q3", geom = "errorbar", size = 1.5, colour = "red", width = 0.3) +
  stat_summary(fun.y = "median", geom = "crossbar", size = 1, colour = "red", width = 0.4) +
  geom_pwc(ref.group = "Instant Ocean",
           method = "wilcox.test",
           label = "p.signif", 
           hide.ns = TRUE,
           tip.length = 0.03,
           size = 0.5, #bracket size
           label.size = 10,
           vjust = 0.5)
####Export > Save as PDF > US Letter for consistency

#Speed.Max
ggdotplot(Compiled_Data, x = "Treatment", y = "Speed.Max") +
  geom_dotplot(binaxis="y", stackdir="center", dotsize = 1) + 
  ylab("Speed (mm/s)") +
  scale_y_continuous(limits = c(0, NA)) +
  theme(axis.title.x = element_text(margin = margin(t = 15), size = 35), 
        axis.title.y = element_text(margin = margin(r = 15), size = 35), 
        axis.text.x = element_text(size = 35),
        axis.text.y = element_text(size = 30)) +
  stat_summary(fun.data = "median_q1q3", geom = "errorbar", size = 1.5, colour = "red", width = 0.3) +
  stat_summary(fun.y = "median", geom = "crossbar", size = 1, colour = "red", width = 0.4) +
  geom_pwc(ref.group = "Instant Ocean",
           method = "wilcox.test",
           label = "p.signif", 
           hide.ns = TRUE,
           tip.length = 0.03,
           size = 0.5, #bracket size
           label.size = 10,
           vjust = 0.5)
####Export > Save as PDF > US Letter for consistency

#Turning.Angle.Max
ggdotplot(Compiled_Data, x = "Treatment", y = "Turning.Angle.Max") +
  geom_dotplot(binaxis="y", stackdir="center", dotsize = 1) + 
  ylab("Angle (radians)") +
  scale_y_continuous(limits = c(0, NA)) +
  theme(axis.title.x = element_text(margin = margin(t = 15), size = 35), 
        axis.title.y = element_text(margin = margin(r = 15), size = 35), 
        axis.text.x = element_text(size = 35),
        axis.text.y = element_text(size = 30)) +
  stat_summary(fun.data = "median_q1q3", geom = "errorbar", size = 1.5, colour = "red", width = 0.3) +
  stat_summary(fun.y = "median", geom = "crossbar", size = 1, colour = "red", width = 0.4) +
  geom_pwc(ref.group = "Instant Ocean",
           method = "wilcox.test",
           label = "p.signif", 
           hide.ns = TRUE,
           tip.length = 0.03,
           size = 0.5, #bracket size
           label.size = 10,
           vjust = 0.5)
####Export > Save as PDF > US Letter for consistency

#Turning.Angle.Mean
ggdotplot(Compiled_Data, x = "Treatment", y = "Turning.Angle.Mean") +
  geom_dotplot(binaxis="y", stackdir="center", dotsize = 1) + 
  ylab("Angle (radians)") +
  geom_hline(yintercept=0,linetype=2) +
  theme(axis.title.x = element_text(margin = margin(t = 15), size = 35), 
        axis.title.y = element_text(margin = margin(r = 15), size = 35), 
        axis.text.x = element_text(size = 35),
        axis.text.y = element_text(size = 30)) +
  stat_summary(fun.data = "median_q1q3", geom = "errorbar", size = 1.5, colour = "red", width = 0.3) +
  stat_summary(fun.y = "median", geom = "crossbar", size = 1, colour = "red", width = 0.4) +
  geom_pwc(ref.group = "Instant Ocean",
           method = "wilcox.test",
           label = "p.signif", 
           hide.ns = TRUE,
           tip.length = 0.03,
           size = 0.5, #bracket size
           label.size = 10,
           vjust = 0.5)
####Export > Save as PDF > US Letter for consistency

#Zero.Movement
ggdotplot(Compiled_Data, x = "Treatment", y = "Zero.Movement") +
  geom_dotplot(binaxis="y", stackdir="center", dotsize = 1) + 
  ylab("Time (s)") +
  scale_y_continuous(limits = c(0, NA)) +
  theme(axis.title.x = element_text(margin = margin(t = 15), size = 35), 
        axis.title.y = element_text(margin = margin(r = 15), size = 35), 
        axis.text.x = element_text(size = 35),
        axis.text.y = element_text(size = 30)) +
  stat_summary(fun.data = "median_q1q3", geom = "errorbar", size = 1.5, colour = "red", width = 0.3) +
  stat_summary(fun.y = "median", geom = "crossbar", size = 1, colour = "red", width = 0.4) +
  geom_pwc(ref.group = "Instant Ocean",
           method = "wilcox.test",
           label = "p.signif", 
           hide.ns = TRUE,
           tip.length = 0.03,
           size = 0.5, #bracket size
           label.size = 10,
           vjust = 0.5)
####Export > Save as PDF > US Letter for consistency