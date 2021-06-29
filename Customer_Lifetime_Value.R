## Problem Statement:- Predict Customer Life-time Value (CLV).

# CLV:- CLV is the total revenue the client will derive from their entire 
# relationship with a customer.

# Here Target column is continuous in nature.

#### Determining the type of Machine Learning:-   I need to create a 
# Supervised ML Regression model, as the target variable is Continuous.

################################################################################
################################################################################

###   library:
library(RColorBrewer)

################################################################################
################### Data Import || Remove Duplicate || Backup ##################

### Load the data.
CLV_Data = read.csv('D:\\Computer Course\\Ivy\\4. Statistics\\Z_Git_Hub\\Regression\\CLV_DATA.csv', na.strings = c(""," ","NA","NULL"), stringsAsFactors = T)

### Print the number of row.
print(paste('Number of row before deleting duplicate values:', nrow(CLV_Data)))

### Removing duplicate rows if any.
CLV_Data= CLV_Data[!duplicated(CLV_Data), ]

#### Print the number of row.
print(paste('Number of row after deleting duplicate values:', nrow(CLV_Data)))

### Data backup.
backup_CLV_Data = CLV_Data

################################################################################
########################### Basic Data Exploration #############################

### Sample rows.
head(CLV_Data)

### Internal structure of the data.
str(CLV_Data)

### Dimension of the data.
dim(CLV_Data)

################################################################################
############################# Creating Variable ################################

### Target Column.
TARGET_COLUMN_NAME = 'Customer.Lifetime.Value'

### All garbage column.
garbage_column_NAME = c("Customer","Effective.To.Date","Sales.Channel")

### All Continuous Column.
continuous_column_NAME = c('Income','Monthly.Premium.Auto','Months.Since.Last.Claim','Months.Since.Policy.Inception','Total.Claim.Amount')

### Data type correction.
# Type.of.Open.Complaints & Type.of.Policies should be Factor data type.

convert_to_factor = c('Type.of.Open.Complaints','Type.of.Policies')

for (i in convert_to_factor) {
  data_1[ ,i ] = as.factor(data_1[ ,i ])
  }

### All Categorical Column.
categorical_column_NAME = colnames(CLV_Data[ , !colnames(CLV_Data) %in% c(TARGET_COLUMN_NAME, garbage_column_NAME, continuous_column_NAME)])

TARGET_COLUMN_NAME
garbage_column_NAME
continuous_column_NAME
categorical_column_NAME

################################################################################
############################### Data Cleaning ##################################

### Remove garbage column.
CLV_Data[ ,garbage_column_NAME] = NULL

### Missing value treatment (If any).

colSums(is.na(CLV_Data))
# There is no Missing Value.

################################################################################
######################## Exploratory Data Analysis #############################

################ Univariate Analysis:-

### Visual Analysis of Target Column (Histogram).
par(mfrow = c(1,1))
hist(x = CLV_Data[ ,TARGET_COLUMN_NAME],main = paste('Histogram for: ',TARGET_COLUMN_NAME),col = brewer.pal(8,'Paired'),xlab = "")

## Explanation:-
# The data distribution of the target variable is satisfactory to proceed further.
# There are sufficient number of rows for each type of values to learn from. 
# It is slightly positively skewed. which is acceptable.

### Visual Analysis of Continuous Column (Histogram).

par(mfrow = c(2,3))

for (hist_col in continuous_column_NAME) {
    hist(x = CLV_Data[ ,hist_col],main = paste('Histogram for: ',hist_col),col = brewer.pal(8,'Paired'),xlab = "")
}

## Explanation:-
#                        Income:- Good distribution.
#          Monthly.Premium.Auto:- Slightly is positively skewed distribution.
#       Months.Since.Last.Claim:- Good distribution.
# Months.Since.Policy.Inception:- Good distribution.
#            Total.Claim.Amount:- Positively skewed distribution.

### Visual Analysis of Categorical Columns (Bar plot).

par(mfrow = c(3,5))

for (bar_col in categorical_column_NAME) {
    barplot(table(CLV_Data[ ,bar_col]),main = paste('Bar-Plot for: ',bar_col),col = brewer.pal(8,'Paired'),xlab = "")
}
## Explanation:-
# Distributions are good for all the Categorical Columns.

###  Visual Analysis  to finding Outlier.

par(mfrow = c(3,2))

for (box_col in continuous_column_NAME) {
boxplot(x = CLV_Data[ ,box_col], main = paste('Box-Plot for:',box_col), col = brewer.pal(8,'Paired'), horizontal = T)
}
## Explanation:-
# Outliers found in the column:- Monthly.Premium.Auto , Total.Claim.Amount

#### Outlier treatment.

### Outlier treatment for:- Monthly.Premium.Auto

# Boxplot to detect outliers.
boxplot(CLV_Data[ ,'Monthly.Premium.Auto'], horizontal = T)

# Find logical value to replace outliers.
quantiles= quantile(CLV_Data[ ,'Monthly.Premium.Auto'],c(0.991,0.99,0.993,0.9941,0.9942,0.9943,0.9945,0.9947,0.9948,0.995))
quantiles

quantiles_final= quantile(CLV_Data[ ,'Monthly.Premium.Auto'],0.9945)
quantiles_final

# Replacing outliers.
CLV_Data$Monthly.Premium.Auto = ifelse(CLV_Data$Monthly.Premium.Auto > quantiles_final , quantiles_final, CLV_Data$Monthly.Premium.Auto)

# Boxplot to check the changes.
boxplot(CLV_Data[ ,'Monthly.Premium.Auto'], horizontal = T)


### Outlier treatment for:- Total.Claim.Amount

# Boxplot to detect outliers.
boxplot(CLV_Data[ ,'Total.Claim.Amount'], horizontal = T)

# Find logical value to replace outliers.
quantiles= quantile(CLV_Data[ ,'Total.Claim.Amount'],c(0.993,0.995,0.996,0.997,0.998,0.9985,0.999))
quantiles

quantiles_final= quantile(CLV_Data[ ,'Total.Claim.Amount'],0.998)
quantiles_final

# Replacing outliers.
CLV_Data$Total.Claim.Amount = ifelse(CLV_Data$Total.Claim.Amount > quantiles_final , quantiles_final, CLV_Data$Total.Claim.Amount)

# Boxplot to check the changes.
boxplot(CLV_Data[ ,'Total.Claim.Amount'], horizontal = T)


### Statistical Measure (Mean, Median, Mode, SD, VAR, Range, Quartiles).

data.frame(sapply(CLV_Data[ ,continuous_column_NAME],mean))
data.frame(sapply(CLV_Data[ ,continuous_column_NAME],median))
data.frame(sapply(CLV_Data[ ,continuous_column_NAME],sd))
data.frame(sapply(CLV_Data[ ,continuous_column_NAME],var))
sapply(CLV_Data[ ,continuous_column_NAME],range)
sapply(CLV_Data[ ,continuous_column_NAME],quantile)

# UDF for Mode.
modal_value = function(input){
  names(table(input))[table(input)==max(table(input))]
}

# Mode.
data.frame(sapply(CLV_Data[ ,categorical_column_NAME],modal_value))


################ Bivariate Analysis:-

### Relationship exploration: Continuous Vs Continuous -- Scatter Charts.

#plot(CLV_Data[ ,continuous_column_NAME], col = 'Blue')
pairs(CLV_Data[ ,c(TARGET_COLUMN_NAME, continuous_column_NAME)], col = 'Blue')

### Relationship exploration: Categorical Vs Continuous -- Box Plots.

par(mfrow = c(3,5))

for (box_col in categorical_column_NAME) {
    boxplot(CLV_Data[ ,TARGET_COLUMN_NAME] ~ CLV_Data[ ,box_col],main = paste('Box-Plot for:',box_col), col = brewer.pal(8,'Paired'), xlab = "")
}

################################################################################
############################ Feature Selection #################################

#### Feature Selection:- Continuous Vs Continuous using Correlation value.

# Generating correlation matrix.
cor_data = cor(CLV_Data[ ,c(TARGET_COLUMN_NAME,continuous_column_NAME)])

# Filter out the columns.
cor_names = names(cor_data[TARGET_COLUMN_NAME, ][abs(cor_data[TARGET_COLUMN_NAME, ]) > 0.02])

# Removing target column.
Final_continuous_column_NAME = cor_names[cor_names!=TARGET_COLUMN_NAME]
Final_continuous_column_NAME


#### Feature Selection:- Continuous Vs Categorical using ANOVA test.

# ANOVA test.
for (aov_col in categorical_column_NAME) {
  aov_Result = summary(aov(CLV_Data[ ,TARGET_COLUMN_NAME] ~ CLV_Data[ ,aov_col]))
  cat('\n\n')
  cat('   ***  Anova Test for: ',aov_col,'\n')
  print(aov_Result)
}

Final_categorical_column_NAME = c('Coverage','Education','EmploymentStatus','Marital.Status','Type.of.Open.Complaints','Type.of.Policies','Renew.Offer.Type','Vehicle.Class')

# Final selected columns.
TARGET_COLUMN_NAME  
Final_continuous_column_NAME
Final_categorical_column_NAME

################################################################################
######################## Data for Machine Learning #############################

## Creating data with the selected columns. 

Final_Target_Variable      = CLV_Data[ ,TARGET_COLUMN_NAME]
Final_continuous_Variable  = CLV_Data[ ,Final_continuous_column_NAME]
Final_categorical_variable = CLV_Data[ ,Final_categorical_column_NAME]

## Creating data for Machine Learning.

Data_For_ML = data.frame(Final_Target_Variable, Final_continuous_Variable, Final_categorical_variable)
head(Data_For_ML)

## Dividing the data into Train(70%) & Test(30%).

set.seed(27)
Train_Data_Index = sample(x = c(1 : nrow(CLV_Data)), size = nrow(CLV_Data) * 0.70 )
Train_Data_Index

## Creating data for train and test.
Data_For_ML_Train = Data_For_ML[Train_Data_Index, ]
Data_For_ML_Test  = Data_For_ML[-Train_Data_Index, ]

## Checking the dimension.
dim(Data_For_ML)
dim(Data_For_ML_Train)
dim(Data_For_ML_Test)



################################################################################
######################## Linear Regression Model  ##############################

Reg_1 = lm(Final_Target_Variable ~. ,data = Data_For_ML_Train)
summary(Reg_1)

Reg_2 = lm(Final_Target_Variable ~ Monthly.Premium.Auto+I(Coverage == 'Premium') +
             I(Education == 'High School or Below') + I(EmploymentStatus == 'Medical Leave') + 
             I(Marital.Status == 'Single') + I(Type.of.Open.Complaints == 3) +
             I(Type.of.Open.Complaints == 4) + I(Type.of.Policies == 2) +
             I(Type.of.Policies == 3) + I(Type.of.Policies == 4) + I(Type.of.Policies == 5) +
             I(Type.of.Policies == 6) + I(Type.of.Policies == 7) + I(Type.of.Policies == 8) +
             I(Type.of.Policies == 9) + I(Renew.Offer.Type == 'Offer3') + 
             I(Vehicle.Class == 'Sports Car') + I(Vehicle.Class == 'SUV') ,data = Data_For_ML_Train)
summary(Reg_2)

Reg_3 = lm(Final_Target_Variable ~ Monthly.Premium.Auto + I(Marital.Status == 'Single') +
             I(Type.of.Open.Complaints == 4) + I(Type.of.Policies == 2) +
             I(Type.of.Policies == 3) + I(Type.of.Policies == 4) + I(Type.of.Policies == 5) +
             I(Type.of.Policies == 6) + I(Type.of.Policies == 7) + I(Type.of.Policies == 8) +
             I(Type.of.Policies == 9) + I(Vehicle.Class == 'Sports Car') ,data = Data_For_ML_Train)
summary(Reg_3)

Reg_4 = lm(Final_Target_Variable ~ Monthly.Premium.Auto + I(Marital.Status == 'Single') +
             I(Type.of.Policies == 2) + I(Type.of.Policies == 3) + I(Type.of.Policies == 4) +
             I(Type.of.Policies == 5) + I(Type.of.Policies == 6) + I(Type.of.Policies == 7) +
             I(Type.of.Policies == 8) + I(Type.of.Policies == 9) + I(Vehicle.Class == 'Sports Car')
           ,data = Data_For_ML_Train)
summary(Reg_4)

## Prediction:- Linear Regression Model.

Data_For_ML_Test$pred_LM = predict(Reg_4,Data_For_ML_Test)
head(Data_For_ML_Test)

## Accuracy Test for Linear Regression.

Data_For_ML_Test$LM_APE = 100 * ( abs(Data_For_ML_Test$Final_Target_Variable - Data_For_ML_Test$pred_LM) / Data_For_ML_Test$Final_Target_Variable )
head(Data_For_ML_Test)

MeanAPE = mean(Data_For_ML_Test$LM_APE)
MedianAPE = median(Data_For_ML_Test$LM_APE)

print(paste('### Mean Accuracy of Linear Regression Model is: ', 100 - MeanAPE))
print(paste('### Median Accuracy of Linear Regression Model is: ', 100 - MedianAPE))


################################################################################
########################## Decision Tree Model  ################################
#  library:
library(party)
library(caret)

ctree_data = ctree(Final_Target_Variable ~ ., data =   Data_For_ML_Train)
plot(ctree_data)

## Prediction:- Decision Tree Model.

Data_For_ML_Test$Pred_Ctree = as.numeric(predict(ctree_data,Data_For_ML_Test))


## Accuracy Test for Decision Tree.
Data_For_ML_Test$Ctree_APE = 100 * ( abs(Data_For_ML_Test$Final_Target_Variable - Data_For_ML_Test$Pred_Ctree) / Data_For_ML_Test$Final_Target_Variable )
head(Data_For_ML_Test)

MeanAPE_ctree = mean(Data_For_ML_Test$Ctree_APE)
MedianAPE_ctree = median(Data_For_ML_Test$Ctree_APE)

print(paste('### Mean Accuracy of Decision Tree Model is: ', 100 - MeanAPE_ctree))
print(paste('### Median Accuracy of Decision Tree Model is: ', 100 - MedianAPE_ctree))


################################################################################
########################## Random Forest Model  ################################
## Library.
library(randomForest)

Model_RF = randomForest(Final_Target_Variable ~ . ,data= Data_For_ML_Train, ntree=10,importance=TRUE)
Model_RF

## Prediction:- Random Forest Model.
Data_For_ML_Test$Pred_RF = predict(Model_RF,Data_For_ML_Test)
head(Data_For_ML_Test)


## Measuring Goodness of Fit using R2 value on TRAINING DATA
Orig = Data_For_ML_Train$Final_Target_Variable
Pred = predict(Model_RF,Data_For_ML_Train)

R2 = 1 - (sum((Orig-Pred)^2)/sum((Orig-mean(Orig))^2))

## Accuracy Test for Random Forest.
Data_For_ML_Test$RF_APE = 100 * ( abs(Data_For_ML_Test$Final_Target_Variable - Data_For_ML_Test$Pred_RF) / Data_For_ML_Test$Final_Target_Variable )
head(Data_For_ML_Test)

MeanAPE_RF = mean(Data_For_ML_Test$RF_APE)
MedianAPE_RF = median(Data_For_ML_Test$RF_APE)

print(paste('R2 Value is:',round(R2,4)))
print(paste('### Mean Accuracy of Random Forest Model is: ', 100 - MeanAPE_RF))
print(paste('### Median Accuracy of Random Forest Model is: ', 100 - MedianAPE_RF))


