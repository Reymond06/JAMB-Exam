# Notes for non coders will have #** in front
# Must run lines 11-18 first before running the rest of the code or it will not work
# Exploratory Data Analysis starts on line 20 ends on line 115
# Model Selection and testing starts on line 120 where the full model is stated




#** Dataset has 17 variables
#** you have to describe all the variables in the dataset
attach(jamb_exam_results)
library(olsrr)
library(car)
library(Metrics)
df <- jamb_exam_results
df2 <- df[,-12]
print(df2)
dim(jamb_exam_results)

# Exploratory Data Analysis 
# tabling the categorical variables
table(Parent_Education_Level)
table(Parent_Involvement)
table(School_Location)
table(School_Type)
table(Extra_Tutorials)
table(Access_To_Learning_Materials)
table(IT_Knowledge)
table(Gender)
table(Socioeconomic_Status)

# Bar Charts for all categorical variables
EL_table <- table(Parent_Education_Level)
barplot(EL_table, xlab = "Parent Education Dummy variable", ylab = "Frequency")

PI_table <- table(Parent_Involvement)
barplot(PI_table, xlab = "Parent Involvement Dummy variable", ylab = "Frequency")

Loco_table <- table(School_Location)
barplot(Loco_table, xlab = "School Location Dummy variable", ylab = "Frequency")

School_Type_table <- table(School_Type)
barplot(School_Type_table, xlab = "School Type Dummy variable", ylab = "Frequency")

Tutorial_table <- table(Extra_Tutorials)
barplot(Tutorial_table, xlab = "Extra Tutorials Dummy variable", ylab = "Frequency")

Mats_table <- table(Access_To_Learning_Materials)
barplot(Mats_table, xlab = "Access to Learning Materials Dummy variable", ylab = "Frequency")

IT_table <- table(IT_Knowledge)
barplot(IT_table, xlab = "IT Knowledge Dummy variable", ylab = "Frequency")

Gender_table <- table(Gender)
barplot(Gender_table, xlab = "Gender Dummy variable", ylab = "Frequency")

Econ_table <- table(Socioeconomic_Status)
barplot(Econ_table, xlab = "Socioeconomic Status Dummy variable", ylab = "Frequency")

# Histograms for quantitative variables
hist(Study_Hours_Per_Week)
hist(Attendance_Rate)
hist(Teacher_Quality)
hist(Distance_To_School)
hist(Age)
hist(Assignments_Completed)

# scatterplot matrix
pairs(df[,c(1,2,3,4,5)])



# correlation
cor(df[,c(1,2,3,4,5)])

# Partial Regression Plots
initial_full_model <- lm(JAMB_Score ~ ., data=df)
crPlots(initial_full_model)
# no transformations needed for quantitative variables


# Answers

# Useful variables: study hours, attendance rate, teacher quality, distance to school
# School type, parent involvement, IT knowledge, Socioeconomic Status

# We will not use any high-order or transformations

# There are no obvious outliers

# None of the assumptions are clearly violated
plot(initial_full_model)

# Collinearity
# Collinearity could be an issue in this model for a couple variables, 
# attendance and study hours may run into some collinearity issues but it is
# unknown until we run complete diagnostics

# Nothing else of notice in the dataset

#** What we may include in the full model

# We are considering using the interaction terms of (Attendance_Rate)*(Teacher_Quality) + 
# (Parent_Involvement)*(Attendance_Rate) +
# Socioeconomic status * IT knowledge

# Attendance Rate and Teacher Quality is being considered because some students may
# be more likely to go to class if they have a higher quality teacher

# Attendance Rate and Parent Involvement is being considered because some students
#rely on their parents for transportation and if their parent is unable to take
#their child such as having to go to work or doesnt have a form of transportation,
#then their child will miss out on important instructional hours

# Socioeconomic status and IT knowledge are being considered because if a student's family
# has more money, they may have more access to computer's and other things that could lead
# to them gaining more IT Knowledge




#* Full Model Diagnostics

full_model <- lm(JAMB_Score ~ . + (Socioeconomic_Status)*(IT_Knowledge) + (Teacher_Quality)*(Attendance_Rate) + 
                   (Attendance_Rate)*(Parent_Involvement), data=df2)
summary(full_model)

# The significant terms in the model are Study Hours per week, attenance rate, teacher quality, distance to school,
# School_type, Extra tutorials, Access to learning materials, parent involvement, It knowledge, 
# Socioeconomic status, parent education level, and attendance rate * teacher quality

#* Checking model assumptions and outilers
library(MASS)
plot(full_model)

# Checking for Jackknife Residuals
t <- qt(.025, 5000-18-2, lower.tail = FALSE)
print(t) # Use t = 1.96

head(sort(studres(full_model)), n=70) # There are 66 violations here
tail(sort(studres(full_model)), n=160) # There are 152 violations here
# Total of 218 violation

#* We will not be removing any of the outliers since it is a large data set
#* that contains 5000 observations so some violations are expected
#* linearity, independence, existence, and Normality is not violated
#* Constant variance appears to be violated

# Checking for Leverage
# 2(16)/5000 = 0.0064
tail(sort(hatvalues(full_model)),n=1200) # 1,184 Violations

# Not removing violations

# Checking for Cook's Distance
tail(sort(cooks.distance(full_model)),n=10) # No violation

# No violation with Cook's Distance, so no points are removed
#* Model Selection
full <- df2

#Random select observation numbers for training set and test set.
#This is a large size so use a 80/20 testing/training split
set.seed(1)
train=sample(1:nrow(full),nrow(full)*0.20) #Put 20% into training set.
test=(-train) #Remaining observations are put into testing set.


#Split dataset into 2 sets: train_set and test_set.
train_set <- full[train,]
test_set <- full[test,]
a<- lm(JAMB_Score ~ ., data=train_set)


training_full_model <- lm(JAMB_Score ~ . + (Socioeconomic_Status)*(IT_Knowledge) + (Teacher_Quality)*(Attendance_Rate) + 
                   (Attendance_Rate)*(Parent_Involvement), data=train_set)
# Selection Types
attach(train_set)


######### Backwards Selection
######Backwards Stepwise- Use p-value as criteria
model_backward <- ols_step_backward_p(training_full_model, p_val  = .1,details=TRUE)

#*Variables Removed: 
# Assignments_Completed 
# Gender 
# Attendance_Rate:Teacher_Quality 
# School_Type 
# Age 
# Parent_Education_Level 
# IT_Knowledge:Socioeconomic_Status 
# Parent_Involvement 
# School_Location 
# Access_To_Learning_Materials 
# Attendance_Rate:Parent_Involvement 

#**** THIS IS THE FINAL MODEL AND ALL THE VARIABLES
final_model <- lm(JAMB_Score ~ Study_Hours_Per_Week + Attendance_Rate +
                    Teacher_Quality + Distance_To_School + Extra_Tutorials
                  +IT_Knowledge + Socioeconomic_Status, data=train_set)
crPlots(final_model)
plot(final_model)
# It appears that constant variance is violated
# Other assumptions are not violated

 
summary(final_model)
print(model_backward)
# Plot shows the criterion like AIC, BIC, and R^2
plot(model_backward)
#* No transformations needed in the final model


#* rechecking the assumptions
plot(final_model)
t <- qt(.025, 5000-10-2, lower.tail = FALSE)
print(t) # t = 1.96

head(sort(studres(final_model)), n=15) # 10 in violation
tail(sort(studres(final_model)), n=40) # 35 in violation 
# 45 Total violations from Leverage

# Checking for Leverage
# 2(16)/5000 = 0.0064
tail(sort(hatvalues(final_model)),n=930) # 920 Violations

# Not removing violations

# Checking for Cook's Distance
tail(sort(cooks.distance(final_model)),n=10) # No violation

#* We will not remove the outliers
#* linearity, independence, and existence are not violated
#* normality is not violated, 
#* constant variance appears to be violated



# Using Box Cox to find Best Transformation of Y
boxcox(final_model)
transformed_model <- lm(I(log(JAMB_Score)) ~ Study_Hours_Per_Week + Attendance_Rate +
                          Teacher_Quality + Distance_To_School + Extra_Tutorials
                        +IT_Knowledge + Socioeconomic_Status, data=train_set)
plot(transformed_model)
# Constant Variance still violated, so we will just use normal Y to keep model
#simple


#* MAE of the model

pred_train <- predict(final_model, train_set)
mae(train_set$JAMB_Score, pred_train)

pred_test <- predict(final_model, test_set)
mae(test_set$JAMB_Score, pred_test)


#* Model overfit on training data
# If Mae(train) << Mae(test) -> model is overfit

# MAE(train) is 31.00, MAE(test) is 31.62, they are almost equivalent
# so the model is a reliable model.

ols_coll_diag(final_model)
# No collinearity issue present within the model

