##########################################################
####### BEFORE TESTING DIFFERENT Classification Models
#         to classify extreme OSA cases
#               IAH <= 10 vs IAH >=30
#######
#######    try some EDA (Exploratory Data Analysis)
#

rm(list=ls())

########################################
#
#         - load the data from

Input_file <- "OSA_extreme_male.xlsx"

Data_Directory <- "D:\\OSA_CaseStudy\\DATA\\"

# Using readxl package to read an Excel file
# Install the readxl package is nor already installed

library(readxl)

df_OSA_male <- read_excel(paste(Data_Directory, Input_file, sep = ""))

# Define OSA column as a factor for being used be
# classification models
df_OSA_male$OSA = factor(df_OSA_male$OSA)

##########################################
#   Summary statistics of the data
#

# Statistics for the whole data frame
summary(df_OSA_male)

# Statistics by group or class "Healthy" / "Severe"
library(dplyr) 

subset(df_OSA_male, OSA=="Healthy") %>% summary()

subset(df_OSA_male, OSA=="Severe") %>% summary()

###################################################
##   To explore the discriminiatiove power of each feature
##   we can visualize the histogram of each feature per group or class
#

# set the plotting area into a 1*2 array
par(mfrow=c(1,2))

## Please NOTE that IAH is NOT a feature!!!
hist(subset(df_OSA_male, OSA=="Healthy")$IAH)
hist(subset(df_OSA_male, OSA=="Severe")$IAH)

# set the plotting area into a 1*2 array
par(mfrow=c(1,2))

hist(subset(df_OSA_male, OSA=="Healthy")$Cervical)
hist(subset(df_OSA_male, OSA=="Severe")$Cervical)



#############################################
### We can plot HISTOGRAMS by OSA Groups
### to explore they DISCRIMINATIVE power

################################################
###    ggplot2 
###       One of he best
###       R packages dedicated to data visualization

library(ggplot2)

ggplot(df_OSA_male, aes(x = BMI)) +
  geom_histogram(aes(color = OSA), fill = "white",
                 position = "identity", bins = 30, alpha = 0.1) +
  scale_color_manual(values = c("#00AF00", "#E7B800")) +
  scale_fill_manual(values = c("#00AF00", "#E7B800"))

### create a grid of plots (like subplot())

p1 <- ggplot(df_OSA_male, aes(x = BMI)) +
  geom_histogram(aes(color = OSA), fill = "white",
                 position = "identity", bins = 30, alpha = 0.1) +
  scale_color_manual(values = c("#00AF00", "#E7B800")) +
  scale_fill_manual(values = c("#00AF00", "#E7B800"))

p2 <- ggplot(df_OSA_male, aes(x = Height)) +
  geom_histogram(aes(color = OSA), fill = "white",
                 position = "identity", bins = 30, alpha = 0.1) +
  scale_color_manual(values = c("#00AF00", "#E7B800")) +
  scale_fill_manual(values = c("#00AF00", "#E7B800"))

library(gridExtra)
grid.arrange(p1, p2, ncol = 2)


### ... you can also use boxplots "by group"

par(mfrow=c(1,2))
attach(df_OSA_male)
boxplot(BMI ~ OSA)
boxplot(Height ~ OSA)

## ... you can also learn and use violin plots
#
#  See, for example:
# http://www.sthda.com/english/wiki/ggplot2-violin-plot-quick-start-guide-r-software-and-data-visualization

#### To have QUANTITATIVE information you can 
####    use some tests on the:
####     DISCRIMINATIVE POWER OF EACH FEATURE
####
### For example:

# http://www.r-tutor.com/elementary-statistics/non-parametric-methods/kruskal-wallis-test
# Kruskal-Wallis Test
# A collection of data samples are independent
# if they come from unrelated populations
# and the samples do not affect each other.
# Using the Kruskal-Wallis Test, we can decide
# whether the population distributions are identical
# without assuming them to follow the normal distribution. 


# The null hypothesis is that the BMI density are identical
# populations. To test the hypothesis, 

kruskal.test(BMI ~ OSA, data = df_OSA_male) 

kruskal.test(Height ~ OSA, data = df_OSA_male)

############################################
## We can also explore pairs of features
## using scatter plots per class
#
#  In R we can use lattice (see ?lattice):
#         lattice add-on package is a powerful
#         and elegant high-level data
#         visualization system with an
#         emphasis on multivariate data

library(lattice)

# Each group in a separate mini plot
xyplot(BMI ~ Age | OSA, data = df_OSA_male)

# All points in one class, different colors per class
xyplot(BMI ~ Age , 
       groups =  OSA, data = df_OSA_male,
       auto.key = list(corner = c(1, 1), cex = 0.7))


########################################################
### Finally, as we did in EDA for regression, we could also
### explore the correlation among predictors and IAH
#
#   .. although in this EDA for classification it could be
#   better the discriminative analysis using hypothesis tests
#
#  As our interest is to explore the correlation between numerical
#  predictors and a dicotomous varuiable OSA: Healthy/Severe) we should use
#  the Point Biserial Correlation Coefficient
#
### The point biserial correlation coefficient (rpb) is a correlation
#   coefficient used when one variable (e.g. Y) is dichotomous, se, for example:
#   https://en.wikipedia.org/wiki/Point-biserial_correlation_coefficient
#

cor(df_OSA_male[,3:7])

# for examle a visualization
library(corrplot)

correlations = cor(df_OSA_male[,3:7])
corrplot(correlations, method="number")

################################################
#
#   NOW we can illustrate how to train a Classification Model
#   
#   This is only "to illustrate" how easy is it, but we need to
#   study Classification in more detail as you may find in:
#             Clasification_OSA.R


# Let's see our "outcome" Healthy / Severe OSA
# Using contrasts you can see how the levels of
# the factors will be coded when fitting the model
contrasts(df_OSA_male$OSA)

##################################################
#### Let's start trying LOGISTIC REGRESSION ######
#

### First using only 2 features: Age and BMI
glm.fit=glm(OSA~BMI+Age,data=df_OSA_male,
            family=binomial)


summary(glm.fit)
coef(glm.fit)
summary(glm.fit)$coef


slope <- coef(glm.fit)[2]/(-coef(glm.fit)[3])
intercept <- coef(glm.fit)[1]/(-coef(glm.fit)[3])

# All points in one class, different colors per class
xyplot(BMI ~ Age , 
       groups =  OSA, data = df_OSA_male,
       panel=function(...){
         panel.xyplot(...)
         panel.abline(intercept , slope)},
       auto.key = list(corner = c(1, 1), cex = 0.7))


predict(glm.fit, newdata = df_OSA_male[,c("BMI","Age")], type="response")


# Install and load naivebayes  
library(naivebayes)

# Train a NB model
naive_b <- naive_bayes(OSA ~ BMI+Age, data = df_OSA_male)

# plot the model
plot(naive_b)

predict(naive_b, newdata = df_OSA_male[,c("BMI","Age")], type="class")

Accuracy_NB <- sum(predict(naive_b, newdata = df_OSA_male[,c("BMI","Age")], type="class") ==
  df_OSA_male$OSA) / nrow(df_OSA_male)

print(paste("Accuracy Naive bayes on Train data: ", Accuracy_NB*100))

#####################################################
# Now try a KNN

library(caret) 
# Caret is a tradictional ML package in R
#   https://topepo.github.io/caret/


KNN_model <- train(
  OSA ~ BMI + Age ,
  data = df_OSA_male,
  method = 'knn'
)

# Training, just execute the KNN_model object
KNN_model

# We see that caret automatically trained 
# over multiple values of hyper parameter K (in KNN). 

plot(KNN_model)

### DISCUSION about feature scaling
##
#   - What distance is used in our KNN training? Euclidean?
#
#   - Can we use other distances?
#
#   - Should we normalize (or pre-process, scale) the features?
#
#   - What types of scaling can we use?
#
#   Feature scaling: is it needed for ALL machine learning models?


KNN_model_scaled_features <- train(
  OSA ~ BMI + Age ,
  data = df_OSA_male,
  method = 'knn',
  preProcess = c("center", "scale")
)

KNN_model_scaled_features

#### What is Kappa?
#
#  Cohen's kappa statistic tell us how much better
#          your classifier is performing over the
#          performance of a classifier that simply
#          guesses at random according to the frequency of each class
#
# Kappa is less than or equal to 1. 
# Low values indicate that the classifier is useless. 
#
# 0-0.20 as slight, 0.21-0.40 as fair, 0.41-0.60 as moderate, 
# 0.61-0.80 as substantial, and 0.81-1 as almost perfect agreement



#######################
#
#  You can try using different subsets of coefficients
#  BUT read about Forward, Backward, RFE feature selection

glm.fit=glm(OSA~BMI+Age+Cervical,data=df_OSA_male,
            family=binomial)

# ... you can explore results following the ideas
#     in the text book

summary(glm.fit)
coef(glm.fit)
summary(glm.fit)$coef


#   Continue your study working with:
#             Clasification_OSA.R

#### Please, understand and USE these or other tools
#### like this and
#### add your comments in your Half Term report




