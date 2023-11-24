########################################
# OSA Use Case
#
# Simple ETL process on a single Excel file
#

# Clear the working space
rm(list=ls())

Input_file <- "Info_BDApnea_QuironMalaga.xlsx"
Output_file <- "OSA_DB_UPM.xlsx"

Data_Directory <- "data/"

# Using readxl package to read an Excel file
library(readxl)

df_tmp <- read_excel(paste(Data_Directory, Input_file, sep = ""))

typeof(df_tmp)
is.data.frame(df_tmp)

### NOTE: ##############################################
# df_tmp is NOT only a data frame!
# use as.data.frame to avoid later problems
class(df_tmp)
df_tmp = as.data.frame(df_tmp)
class(df_tmp)

# Renaming
names(df_tmp) <- c("Patient","Comments","Audios lying","Photos","Audio fq kHz","Gender","EPWORTH","AHI","AHI Supine","AHI Lateral","Weight","Height","BMI","Age","Cervical","Smoker","Snorer","Illnesses","Room/Noises","Image","Dialect","EXT EYES DISTANCE","DIST CHIN-LOB","Fatigue","Focus","BreaLossNight","HiperT","EstHOSP")

# Select only desired columns
library(dplyr)
df_tmp <- select(df_tmp, Patient, Gender, Weight, Smoker, Snorer, Illnesses, AHI, Height, BMI, Age, Cervical)


# Visualize dataframe
library(visdat)
vis_dat(df_tmp)

# Drop all NA
library(tidyr)
df_tmp <- df_tmp %>% drop_na()

vis_dat(df_tmp)

# Change Weight to numerical
# '$' character to access the column of the dataframe
df_tmp$Weight <- as.numeric(df_tmp$Weight)

vis_dat(df_tmp)

# Print info
summary(df_tmp)

# Drop again NAs (created when changing Weight to numeric)
df_tmp <- df_tmp %>% drop_na()

# Change gender, smoker, snorer and illnesses to factor
df_tmp$Gender <- factor(df_tmp$Gender)
df_tmp$Smoker <- factor(df_tmp$Smoker)
df_tmp$Snorer <- factor(df_tmp$Snorer)
df_tmp$Illnesses <- factor(df_tmp$Illnesses)

summary(df_tmp)

# Replace all -1 with NAs
library(naniar)
df_tmp <- replace_with_na_all(df_tmp,condition = ~.x == -1)

vis_dat(df_tmp)

summary(df_tmp)

# Calculate BMI
h_sqr <- (df_tmp$Height/100)^2
w <- df_tmp$Weight
bmi <- w/h_sqr

df_tmp$BMI <- bmi

df_tmp <- subset(df_tmp, select = -Weight)
df_tmp <- subset(df_tmp, select = -Height)

df_tmp <- df_tmp %>% drop_na()

vis_dat(df_tmp)

summary(df_tmp)

barplot(table(df_tmp$Smoker))

# Change ns to NA
df_tmp <- replace_with_na_all(df_tmp,condition = ~.x == 'ns')
vis_dat(df_tmp)

df_tmp <- subset(df_tmp, select=-Snorer)

vis_dat(df_tmp)
summary(df_tmp)

# Refactorize (to erase former values which appear in summary() with 0 occurrences)
df_tmp$Gender <- factor(df_tmp$Gender)
df_tmp$Smoker <- factor(df_tmp$Smoker)
df_tmp$Illnesses <- factor(df_tmp$Illnesses)
summary(df_tmp)

df_tmp <- df_tmp %>% drop_na()

vis_dat(df_tmp)

# Count number of unique values of each column
length(unique(df_tmp$Illnesses))

df_tmp <- subset(df_tmp, select=-Illnesses)

vis_dat(df_tmp)

# Change smoker column
df_tmp$Smoker <- replace(df_tmp$Smoker, (df_tmp$Smoker == "antiguo" | df_tmp$Smoker == "poco" | df_tmp$Smoker == "si (poco)"), "si")
df_tmp$Smoker <- factor(df_tmp$Smoker)
barplot(table(df_tmp$Smoker))

vis_dat(df_tmp)

# Write Output file
library(writexl)

df_final <- df_tmp
vis_dat(df_final)

write_xlsx(df_final,
           paste(Data_Directory, Output_file, sep = ""))

