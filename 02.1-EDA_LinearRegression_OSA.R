####################################################
# OSA EDA (exploratory data analysis) for REGRESSION
#         - understanding the relation between independent and
#           depending variable (Chapter 2 PRDL)
#
#         - and using linear regression models
#

Input_file <- "OSA_DB_UPM.xlsx"

Data_Directory <- "C:/Users/pablo/Desktop/CODE_R-20230913/data/"

# Using readxl package to read an Excel file
# Install the readxl package is nor already installed

library(readxl)

df_OSA <- read_excel(paste(Data_Directory, Input_file, sep = ""))

df_OSA = as.data.frame(df_OSA)
names(df_OSA)
dim(df_OSA)

## We can use simple statistics using summary
summary(df_OSA)

## How is the distribution of variables (?)
hist(df_OSA$IAH)

### if we used attach we can simplify accessing
### columns names
attach(df_OSA)
hist(IAH)

### Try to explain why the IAH pdf looks like this!

# We can define Gender as a factor!
df_OSA$Gender = factor(df_OSA$Gender)
summary(df_OSA)

# But we are interested in the relations between the variables:
#     - the outcome (dependent) IAH and predictors (independent ?)
#     - between the predictors (why?)
#

# You can use scatter plots
plot(df_OSA$Age,df_OSA$IAH)

### if we used attach (as before) we can simplify accessing
### columns names
## (already executed) attach(df_OSA)
plot(Age,IAH)


# See relations between variables
# for using pairs, turn Gender as numeric
df_OSA$Gender = as.numeric(df_OSA$Gender)
attach(df_OSA)
pairs(~ IAH + Gender + Weight + Height + Cervical + Age)

#### Understand that obtaining correlation coefficients can
#### be useful
cor(Weight, IAH, method = "pearson")
cor(Weight, IAH, method = "spearman")

### explore several correlations... 
cor(Age, IAH, method = "spearman")
cor(Cervical, IAH, method = "spearman")

# FEATURE ENGINEERING:
# .... a "new" feature BMI could be interesting


## PLOT Correlation Matrix

# FIRST
# install corrplot and then load it
library(corrplot)
# back to as.numeric for including it..

df_OSA_C=df_OSA

df_OSA_C$Gender = as.numeric(df_OSA_C$Gender)

# select all variables EXCLUDING Patient
M <- cor(subset(df_OSA_C, select = - Patient))
corrplot(M, method="number")
corrplot(M, method="circle")

###### NEXT section is relateds to PRDL Chapter 3
###### So please review the concepts and examples
###### you can find in the textbook Introd. to Statistical Learning

# We can study the use of Simple and Multiple LR models

# A simple Linear Regression model using only one feature
 lm_one_feature = lm(IAH ~ Weight)
 
 summary(lm_one_feature)
 
 #Knwo the difference: correlation coeficient and linear regression
 cor.test(IAH,Weight)
 
 # It is easy to plot the regression line on a scatter plot using
 #       abline(a,b) : a, b the intercept and slope
 #                                 

plot(Weight,IAH)
abline(lm(IAH ~ Weight), col="red")

##############################################################
# Now we can train (fit) a Linear Regression using several features

lm.fit=lm(IAH~Weight+Height+Cervical+Age+Gender) # IAH: output || Elements beyond ~: predictors

summary(lm.fit)

## You can use your model for prediction
# see: LinearRegression_and_naive.R
#
#
# !!!! IMPORTANT NOTE:
#
# Be aware that you now: data TRAIN, DEVELOPMENT and TEST

IAH_predictions = predict(lm.fit,df_OSA) # We are making prediction from the training data!!!!!!!

#calculate MSE
mse_Predict <- mean((df_OSA$IAH - IAH_predictions)^2)
print(paste("MSE lm.model Predictor: ", mse_Predict))

# another way to obtain the MSE
mean(lm.fit$residuals^2)

mae_Predict <- mean(abs(df_OSA$IAH - IAH_predictions))

print(paste("MAE lm.model Predictor: ", mae_Predict))

plot(IAH_predictions,df_OSA$IAH)

# Study independently male and female populations

### Male population
df_OSA_male=subset(df_OSA_C, Gender==1)

# Another way
# df_OSA_male = df_OSA_C[df_OSA_C$Gender == 1, ]

names(df_OSA_male)
attach(df_OSA_male)

lm_male.fit=lm(IAH~Height+Cervical+Age+Weight)

summary(lm_male.fit)

############ Female population ################

df_OSA_female=subset(df_OSA_C, Gender==2)

# Another way
# df_OSA_female = df_OSA_C[df_OSA_C$Gender == 2, ]

names(df_OSA_female)
attach(df_OSA_female)

lm_female.fit=lm(IAH~Height+Cervical+Age+Weight)

summary(lm_female.fit)

# BMI add a column
df_OSA_C$BMI <- with(df_OSA_C, Weight / (Height/100.0)^2)

attach(df_OSA_C)

lm.fit=lm(IAH~BMI+Cervical+Age)

summary(lm.fit)


########################################
########################################
####   NEXT STEPS:
####
####  You can try:
####        - Regularization
####        - Feature selection
####        - other Regression models
####
####  BUT you can wait to see Classification Script
####           and use regression with some libraries
####           such as CARET, Tidymodels or mlr3
####



# Set seed for reproducibility
set.seed(123)

# Get the total number of rows in the DataFrame
total_rows <- nrow(df_OSA)

# Generate random row indices for the 80% and 20% splits
indices_80_percent <- sample(1:total_rows, 0.8 * total_rows)
indices_20_percent <- setdiff(1:total_rows, indices_80_percent)

# Split the DataFrame based on the generated indices
df_OSA_train <- df_OSA[indices_80_percent, , drop = FALSE]
df_OSA_test <- df_OSA[indices_20_percent, , drop = FALSE]

# Train model with training dataframe
attach(df_OSA_train)
lm.fit=lm(IAH~Weight+Height+Cervical+Age+Gender)

# Make predictions with testing data
IAH_predictions = predict(lm.fit,df_OSA_test) 

# Calculate MSE
mse_Predict <- mean((df_OSA_test$IAH - IAH_predictions)^2)
print(paste("MSE lm.model Predictor: ", mse_Predict))

# Calculate MAE
mae_Predict <- mean(abs(df_OSA_test$IAH - IAH_predictions))
print(paste("MAE lm.model Predictor: ", mae_Predict))

# Plot predicted data vs ground-truth data
plot(IAH_predictions,df_OSA_test$IAH)


