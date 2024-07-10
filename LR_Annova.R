install.packages("lmtest")
install.packages("car")  # for additional diagnostic tests
install.packages("lmSupport") 
install.packages("ggstatsplot")
install.packages("corrplot")
install.packages("RColorBrewer")
install.packages("ggstatsplot")
install.packages("tidyverse")
install.packages("coin")
install.packages("rstatix")
install.packages("dplyr")
install.packages("ggstatsplot")
install.packages("report")

library(report)
library(dplyr)
library(tidyr)
library(lmtest)
library(car)
library(readr)
library(broom)
library(ggplot2)
library(scatterplot3d)
library(rgl)
library(carData)
library(car)
library("lmtest")
library(report)
library(lmSupport)
library(ggstatsplot)
library(dplyr)
#. •	We hypothesis that Chao is different between the different Antibiotics overtime.
# Can you perform comparison between the different groups, after assessing the assumptions
# and performing post-hoc testing (assuming normality and homoscedasticity).

# Test assumption 
# test of normality 
# using Histogram 
load("./AAD.RData")
AAD$D6.D1_Shannon_diff <- AAD$D1.Shannon.diversity-AAD$D6.Shannon.diversity
AAD$D6.D1_Chao1_diff <- AAD$D1.Chao1.diversity-AAD$D6.Chao1.diversity

par(mfrow = c(2, 2)) 
hist(AAD$D6.D1_Chao1_diff[AAD$Antibiotic.class=="PBL"],main="Chao difference in PBL group",xlab="Chao1 diffrence", col= 2 )
hist(AAD$D6.D1_Chao1_diff[AAD$Antibiotic.class=="OBL"],main="Chao difference in OBL group",xlab="Chao1 diffrence", col= 3 )
hist(AAD$D6.D1_Chao1_diff[AAD$Antibiotic.class=="FQN"], main="Chao difference in FQN group",xlab="Chao1 diffrence", col= 4)

# (B) using  Shapiro test

shapiro.test(AAD$D6.D1_Chao1_diff[AAD$Antibiotic.class=="PBL"]) # normal distributed
shapiro.test(AAD$D6.D1_Chao1_diff[AAD$Antibiotic.class=="OBL"]) # not normally distributed
shapiro.test(AAD$D6.D1_Chao1_diff[AAD$Antibiotic.class=="FQN"]) # not normally distributed 

ggplot(AAD) +
  aes(x = Antibiotic.class, y = D6.D1_Chao1_diff, color = Antibiotic.class) +
  geom_boxplot()

#To compare the differences in Chao diversity (D6.D1_Chao1_diff) across multiple antibiotic classes over time, 
# assuming non-normality and non-homogeneity of variances, 
# We can use two ways for solution.

# (A) independent Design =======> a non-parametric test l, Kruskal-Wallis test <============= 
     # followed by pairwise Wilcoxon signed-rank tests for post-hoc analysis.

kruskal.test(D6.D1_Chao1_diff ~ Antibiotic.class, data = AAD)

# Pairwise Wilcoxon signed-rank tests (post-hoc)
pairwise.wilcox.test(AAD$D6.D1_Chao1_diff, AAD$Antibiotic.class, p.adjust.method = "bonferroni")

ggbetweenstats(
  data = AAD,
  x = Antibiotic.class,
  y = D6.D1_Chao1_diff,
  type = "nonparametric", # ANOVA or Kruskal-Wallis
  var.equal = FALSE, # ANOVA or Welch ANOVA
  plot.type = "box",
  pairwise.comparisons = TRUE,
  pairwise.display = "all",
  centrality.plotting = F,
  bf.message = FALSE
)

#(B)  Dependent Design. =======> a non-parametric test
 #         Friedman test will be used 

 
# ===> convert Data to long Design
sub_data <- data.frame( PatientID =AAD$Patient.ID,Chao1.D1=AAD$D1.Chao1.diversity,
                        Chao1.D6=AAD$D6.Chao1.diversity,Antibiotic.class=AAD$Antibiotic.class)
long_data <- pivot_longer(
  data = sub_data, 
  cols = c(Chao1.D1, Chao1.D6),                      # Specify the columns to pivot
  names_to = "Day",                                 # Column to store day information
  names_prefix = ("Chao1."),                                # Prefix of the day columns
  values_to = "values"                               # Name of the column to store values
)


# Perform Friedman Test

GT = xtabs(values ~ Antibiotic.class+ Day ,
           data = long_data)
friedman.test(GT)

# Plot using ggplot2

ggplot(long_data, aes(x = Antibiotic.class, y = values, fill = Day)) +
  geom_boxplot(position = position_dodge(width = 0.8), alpha = 0.8) +  # Adjust width as needed
  labs(x = "Antibiotic Class", y = "Values", fill = "Day") +
  ggtitle("Comparison of Chao1 obetween  different
          Antibiotics overtime (D1&D6)") + 
  theme_minimal()

####   performing post-hoc testing (assuming normality and homoscedasticity).
model <- aov(values~ Day * Antibiotic.class +Error(factor(PatientID)), data = long_data)
summary(model)


## Plot using ggplot2 to present posthoc result
# create interaction variable ( Day_Antibiotic) to help in ranking data overtime
long_data <- long_data %>%
  mutate(Day = as.factor(Day),
         Antibiotic.class = as.factor(Antibiotic.class),
         Day.Antibiotic = interaction(Day, Antibiotic.class))

# plot function 
ggbetweenstats(
  data = long_data,
  x = Day.Antibiotic,
  y = values,
  fill = Antibiotic.class, # Specify the fill variable
  type = "parametric", # ANOVA or Kruskal-Wallis
  var.equal = TRUE, # ANOVA or Welch ANOVA
  plot.type = "box",
  pairwise.comparisons = TRUE,
  pairwise.display = "all",
  centrality.plotting = FALSE,
  bf.message = FALSE
)
#================================================================================
# ===============================================================================
# ===============================================================================

##•	Fit a linear regression to the data and interpret the regression coefficient
#(for the one of the hypotheses mentioned above)
# =====> (1) Fit a Model
## STEP 1: Draw a graph of the data to make sure the relationship make sense
plot(AAD$D6.D1_Chao1_diff, AAD$D6.D1_Shannon_diff, pch=16, cex=2)


## STEP 2: Do the regression
simple.regression <- lm(D6.D1_Shannon_diff ~ D6.D1_Chao1_diff, data=AAD)


## STEP 3: Look at the R^2, F-value and p-value
summary(simple.regression)

## STEP 4: draw a line 
plot(AAD$D6.D1_Chao1_diff, AAD$D6.D1_Shannon_diff, pch=16, cex=2)
abline(simple.regression, lwd=5, col="red")

ggplot(AAD, aes(x=D6.D1_Chao1_diff, y=D6.D1_Shannon_diff)) +
  geom_point() +
  geom_smooth(method="lm")

# Examine the model against the raw data
model.null <- lm(D6.D1_Shannon_diff ~ 1, 
                 data = AAD)
# “Alternative”" Model

model.alt <- lm(D6.D1_Shannon_diff ~ D6.D1_Chao1_diff, 
     data = AAD)
     
par(mfrow = c(1,2))
plot(D6.D1_Shannon_diff ~ 1, data = AAD,
     main = "D6.D1_Chao1_diff ~ 1"
)
abline(model.null, col = 2, lwd = 3)

plot(D6.D1_Shannon_diff ~ D6.D1_Chao1_diff, 
     data = AAD,
     main = "D6.D1_Shannon_diff ~ D6.D1_Chao1_diff"
)
abline(model.alt, col = 2, lwd = 3)

# =======> (2) Assess the model assumptions
# (1)The regression model is linear in the coefficients and the error term
#Model Diagnostics
par(mfrow = c(2,2))
plot(simple.regression)


# ======> (2) Check the fitted model and the Residual values.

# augment the data to add fitted values and residuals by using the function augment() 
model.diag.metrics <- augment(simple.regression)

ggplot(model.diag.metrics, aes(AAD$D6.D1_Chao1_diff, AAD$D6.D1_Shannon_diff)) +
  geom_point() +
  stat_smooth(method = lm, se = FALSE) +
  geom_segment(aes(xend = AAD$D6.D1_Chao1_diff, yend = .fitted), color = "red", size = 0.3)

#====(2) the curve shows Heteroscedasticity: Non-constant variance of error terms.
#                                     Presence of influential values.
#The error term has a population mean of zero
#The error term accounts for the variation in the dependent variable that
# the independent variables do not explain. Random chance should determine the values of the error term. For your model to be unbiased, the average value of the error term must equal zero.

# ===> 3: All independent variables are uncorrelated with the error term

#=====> 4: 5: The error term has a constant variance (no heteroscedasticity
# Extract residuals from the model

# Get model residuals
report(model.alt)
my.resid <- resid(model.alt)
# Histogram of residual
par(mfrow = c(1,1))
hist(my.resid, col = "skyblue")
shapiro.test(my.resid)
boxplot(my.resid , col="red" ,main= "Test for homoscedasticity using Breusch-Pagan test")
# # Test for homoscedasticity using Breusch-Pagan test
bp_test <- bptest(model.alt)
# Get model predictions
#Also called “Fitted values”. Mathematicall they are called “y.hat”

# BP = 12.57, df = 1, p-value = 0.0003921
# Interpretation: The p-value is less than 0.05,
# indicating that we reject the null hypothesis of homoscedasticity. 
#This suggests that there is evidence of heteroscedasticity in the model.

my.fitted <- fitted(model.alt)
#Diagnostic: Residual vs Fitted
#This tests the assumption of “constant variance”

plot(my.resid ~ my.fitted)
abline(h = 0, col  = 2, lwd = 2)
###########################################
# (2)•	Calculate and interpret a 95% confidence interval of the regression slope , 
 confint(model.alt, "D6.D1_Chao1_diff", level = 0.95)
# Interpretation:
#  D6.D1_Chao1_diff: The 95% confidence interval for the slope of D6.D1_Chao1_diff is
# [0.005441099, 0.0073306].
# This interval means that we are 95% confident that the true slope of D6.D1_Chao1_diff
# lies between 0.005441099 and 0.0073306



#(3) Fit a linear regression to the data and interpret the regression coefficient 
# (for the one of the hypotheses mentioned above), 
# taking into account repeated measures!

mod <- lm((AAD$D1.Chao1.diversity - AAD$D6.Chao1.diversity) ~ AAD$Antibiotic.class, 
          data = AAD)
avPlots(mod)
report(mod)
report(mod)
summary(mod)
model.diag.metrics <- augment(mod)

new_data <- data.frame(AAD.Antibiotic.class = factor("OBL", levels = levels(AAD$Antibiotic.class)))

res=predict.lm(mod, newdata =new_data , interval="confidence")
res=data.frame(res)
res
mean_fit <- mean(res$fit)
mean_lwr <- mean(res$lwr)
mean_upr <- mean(res$upr)

# Combine the means into a single data frame
mean_avg <- data.frame(mean_fit, mean_lwr, mean_upr)
mean_avg

# Aother Way using Annova Model
model <- aov(values~ Day * Antibiotic.class +Error(factor(PatientID)), data = long_data)
summary(model)
coef(model)

# Create the plot. 
ggplot(model.diag.metrics, aes(x = AAD$D1.Chao1.diversity , y = AAD$D1.Chao1.diversity - AAD$D6.Chao1.diversity,color = AAD$Antibiotic.class)) +
  geom_point() +
  stat_smooth(method = "lm", se = TRUE) +
  geom_segment(aes(xend = AAD$D1.Chao1.diversity, yend = .fitted), size = 0.3) +
  labs(x = "D1.Chao1.diversity", y = "Chao 1 Change", color = "Antibiotic Class") +
  theme_minimal()



# (4)•	Estimating the average CHAO change for with changing the Antibiotics

#Summary:
#  (Intercept): The average change in Chao1 diversity for the baseline antibiotic class is 45.39. This is the expected change in Chao1 diversity when no specific antibiotic class (other than the baseline) is applied.
#AAD$Antibiotic.classOBL: The average change in Chao1 diversity for the antibiotic class OBL is -14.66 units less than the baseline antibiotic class. However, this effect is not statistically significant (p = 0.344749).
#AAD$Antibiotic.classFQN: The average change in Chao1 diversity for the antibiotic class FQN is -28.15 units less than the baseline antibiotic class. This effect is borderline statistically significant (p = 0.054460), suggesting a potential decrease in Chao1 diversity when using this antibiotic class, but further investigation would be needed to confirm this result.
#Residual Standard Error:
#  Residual standard error: 94.53 on 332 degrees of freedom.
#R-squared:
#  Multiple R-squared: 0.01211. This means that approximately 1.2% of the variability in the change in Chao1 diversity is explained by the antibiotic class.
#Adjusted R-squared: 0.006162. This value adjusts the R-squared for the number of predictors in the model and is typically slightly lower than the multiple R-squared.
#F-statistic:
#  F-statistic: 2.035 on 2 and 332 DF, p-value = 0.1323. This suggests that the overall model is not statistically significant at the 0.05 level.
#Conclusion:
#  The model suggests that there are differences in the average change in Chao1 diversity associated with different antibiotic classes, with the baseline antibiotic class showing an average increase of 45.39 units.
#The antibiotic class OBL shows a decrease in Chao1 diversity by an average of 14.66 units compared to the baseline, but this is not statistically significant.
#The antibiotic class FQN shows a decrease in Chao1 diversity by an average of 28.15 units compared to the baseline, and this is borderline statistically significant.
#The overall model fit is weak, as indicated by the low R-squared values and the non-significant F-statistic.

