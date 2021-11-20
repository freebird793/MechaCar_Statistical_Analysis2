# Module_15 Challenge

#Load dplyr package
library(dplyr)

# Deliverable 1
# Linear Regression to Predict MPG

# import csv to dataframe
mechacar_mpg_df <- read.csv(file='MechaCar_mpg.csv', check.names = F, stringsAsFactors = F)
head(mechacar_mpg_df)

#linear regression
lm(mpg~vehicle_length+vehicle_weight+spoiler_angle+ground_clearance+AWD, data = mechacar_mpg_df)

# determine p-value and r-squared value for the linear regression model
summary(lm(mpg~vehicle_length+vehicle_weight+spoiler_angle+ground_clearance+AWD, data = mechacar_mpg_df))

#Deliverable 2
# Supension Coil Lot Analysis

# read csv to dataframe
suspension_coil_df <- read.csv(file = 'Suspension_Coil.csv', check.names = F, stringsAsFactors = F)
head(suspension_coil_df)

# create a summary dataframe
total_summary <- suspension_coil_df %>% summarize(Mean = mean(PSI), Median = median(PSI), Variance = var(PSI), Std_Dev = sd(PSI))
View(total_summary)
# create summaries for each lot
lot_summary <- suspension_coil_df %>% group_by(Manufacturing_Lot) %>% summarize(Mean = mean(PSI), Median = median(PSI), Variance = var(PSI), Std_Dev = sd(PSI), .groups = 'keep')
View(lot_summary)

# Deliverable 3
# T-tests on suspension coils
t.test(suspension_coil_df$PSI, mu=1500)

t.test(subset(suspension_coil_df,Manufacturing_Lot=="Lot1")$PSI, mu = 1500)
t.test(subset(suspension_coil_df,Manufacturing_Lot=="Lot2")$PSI, mu = 1500)
t.test(subset(suspension_coil_df,Manufacturing_Lot=="Lot3")$PSI, mu = 1500)
