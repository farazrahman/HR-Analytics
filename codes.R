#HR Analytics Case Study- merging in/out data as a derived variable as Average hour spent per day
#With no outliers treatment(reasons mentioned against each variable having outliers) and deletion of NA values 
#(Removed multicollinear variables first, kept 17 variables) KS-Statistic -54%, accuracy, sensitivity,specificity- 77%

# Install and Load the required packages

library(MASS)
library(car)
library(e1071)
library(caret)
library(ggplot2)
library(cowplot)
library(caTools)
library('corrplot')
library(dplyr)
library(tidyr)
library(lubridate)

# Loading all files
general_data<- read.csv("general_data.csv", stringsAsFactors = F)
employee_survey<- read.csv("employee_survey_data.csv", stringsAsFactors = F)
manager_survey<- read.csv("manager_survey_data.csv", stringsAsFactors = F)
in_time<- read.csv("in_time.csv", stringsAsFactors = F)
out_time <- read.csv("out_time.csv", stringsAsFactors = F)

#Checking the structure of the data

str(general_data) #4410 obs. of  24 variables
str(employee_survey) #4410 obs. of  4 variables
str(manager_survey) #4410 obs. of  3 variables
str(in_time) #4410 obs. of  262 variables
str(out_time) #4410 obs. of  262 variables

# Checking the unique employee ids in all datasets
length(unique(general_data$EmployeeID))    # 4410 employee ids 
length(unique(employee_survey$EmployeeID)) # 4410 employee ids
length(unique(manager_survey$EmployeeID)) # 4410 employee ids
length(unique(in_time$X)) #X is the unique employee ID in this case
length(unique(out_time$X)) #X is the unique employee ID in this case

setdiff(general_data$EmployeeID,employee_survey$EmployeeID) # Identical employeeID across these datasets
setdiff(general_data$EmployeeID,manager_survey$EmployeeID) # Identical employeeID across these datasets
setdiff(general_data$EmployeeID, in_time$X) #Identical employeeID across these datasets
setdiff(general_data$EmployeeID,out_time$X) #Identical employeeID across these datasets

#Merging the datasets into one master file

#Merging the general data with employee survey
merge1 <- merge(general_data, employee_survey, by= "EmployeeID", all = F)

#Merging the manager survey 

merge2 <- merge(merge1, manager_survey, by= "EmployeeID", all = F)

#Getting additional parameter from IN/OUT data

in_time[, c(2:262)] <- data.frame(lapply(in_time[,c(2:262)], as.POSIXct, format= "%Y-%m-%d %H:%M:%S")) #converts the timestamp into date format
in_time[, c(2:262)] <- data.frame(lapply(in_time[, c(2:262)], format, format= "%H:%M:%S")) #Extracts only the time 
in_time[, c(2:262)] <- data.frame(lapply(in_time[, c(2:262)], hms, format= "%H:%M:%S")) #converts the time into hour,minute & seconds

#same operation for out time
out_time[, c(2:262)] <- data.frame(lapply(out_time[,c(2:262)], as.POSIXct, format= "%Y-%m-%d %H:%M:%S"))
out_time[, c(2:262)] <- data.frame(lapply(out_time[, c(2:262)], format, format= "%H:%M:%S"))
out_time[, c(2:262)] <- data.frame(lapply(out_time[, c(2:262)], hms, format= "%H:%M:%S"))

#Assigning where 0 to in/out time are NA
in_time[is.na(in_time)] <- 0
out_time[is.na(out_time)] <- 0

M <- merge(out_time,in_time,by="X") #Merging the IN/OUT time by employee ID 

S <- M[,grepl("*\\.x$",names(M))] - M[,grepl("*\\.y$",names(M))] #Subtracting the In time by out time to get hour spent each day

S <- data.frame(lapply(S, period_to_seconds)) #Converting hour and minutes into seconds

S <- S/3600 #converting the seconds into hour

S[is.na(S)] <- 0 #Assigning 0 to all NA values

S <- transform(S, sum=rowMeans(S)) #Calculating the mean hour spent per day for each employee 

T <- cbind(M[,1,drop=FALSE],S[,262]) #merging the employee ID with the average hr per day column 

colnames(T) <- c("EmployeeID", "Avg_hr_perday")

merge3 <- merge(merge2, T, by = "EmployeeID", all = F) #Merging the data with main file

master_file <- merge3
################################################################

### Data Preparation & Exploratory Data Analysis

# Understanding the structure of the collated file
str(master_file) #4410 obs. of 30 variables;
summary(master_file)

# Attrition to be changed to integer
# Business travel, department, education field, gender, job role & marital status to be changed to integer


# Barcharts for categorical features with stacked attrition information
bar_theme1<- theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))

plot_grid(ggplot(master_file, aes(x=BusinessTravel,fill=Attrition))+ geom_bar(position = "fill")+bar_theme1+ coord_flip(), 
          ggplot(master_file, aes(x=Department,fill=Attrition))+ geom_bar(position = "fill")+bar_theme1+ coord_flip(),
          ggplot(master_file, aes(x=EducationField,fill=Attrition))+ geom_bar(position = "fill")+bar_theme1+ coord_flip(),
          ggplot(master_file, aes(x=Gender,fill=Attrition))+ geom_bar(position = "fill")+bar_theme1+ coord_flip(),
          align = "h")   

#We find:
#25% of employee who travel frequently are more likely to leave as compared to who travel rarely or don't travel at all
#More than 25% of the employee from HR are leaving, followed by R&D and Sales
#Almost 40% of employess with HR background are leaving
#Attrition rate in male and female is almost same

plot_grid(ggplot(master_file, aes(x=JobRole,fill=Attrition))+ geom_bar(position = "fill")+bar_theme1+ coord_flip(),
          ggplot(master_file, aes(x=MaritalStatus,fill=Attrition))+ geom_bar(position = "fill")+bar_theme1+ coord_flip(),
          ggplot(master_file, aes(x=Over18,fill=Attrition))+ geom_bar(position = "fill")+bar_theme1+ coord_flip(),
          ggplot(master_file, aes(x=factor(Education),fill=Attrition))+ geom_bar(position = "fill")+bar_theme1+ coord_flip(),
          align = "h")   

#We find:
#Research directors have highest percent of attrition(around 25%) amongst other job roles
#Single employees are more likely to leave the company followed by married employees
#Over18 has only one level of data we can get rid of it
#College level education has high attrition rate amongst others 

plot_grid(ggplot(master_file, aes(x=JobLevel,fill=Attrition))+ geom_bar(position = "fill")+bar_theme1+ coord_flip(),
          ggplot(master_file, aes(x=EnvironmentSatisfaction,fill=Attrition))+ geom_bar(position = "fill")+bar_theme1+ coord_flip(),
          ggplot(master_file, aes(x=JobSatisfaction,fill=Attrition))+ geom_bar(position = "fill")+bar_theme1+ coord_flip(),
          ggplot(master_file, aes(x=WorkLifeBalance,fill=Attrition))+ geom_bar(position = "fill")+bar_theme1+ coord_flip(),
          align = "h")   

#We find:
#Job level 2 have high attrition rate
#Attrition is high in case of low environment satisfaction & low job satisfaction
#Attrition is high in case of low work life balance


plot_grid(ggplot(master_file, aes(x=JobInvolvement,fill=Attrition))+ geom_bar(position = "fill")+bar_theme1+ coord_flip(),
          ggplot(master_file, aes(x=factor(PerformanceRating),fill=Attrition))+ geom_bar(position = "fill")+bar_theme1+ coord_flip(),
          ggplot(master_file, aes(x=factor(StockOptionLevel),fill=Attrition))+ geom_bar(position = "fill")+bar_theme1+ coord_flip(),
          align = "h")   

#We find:
#Job involvement level 1 has high attrition rate
#performance rating of 4 has slightly higher attrition rate
#Slightly higher attrition rate is seen in case of stock option 0 & 2 level


# Histogram and Boxplots for numeric variables 
box_theme<- theme(axis.line=element_blank(),axis.title=element_blank(), 
                  axis.ticks=element_blank(), axis.text=element_blank())

box_theme_y<- theme(axis.line.y=element_blank(),axis.title.y=element_blank(), 
                    axis.ticks.y=element_blank(), axis.text.y=element_blank(),
                    legend.position="none")

plot_grid(ggplot(master_file, aes(Age, fill = Age))+ geom_histogram(binwidth = 5,fill = "steelblue1", color = "black"),
          ggplot(master_file, aes(x="",y=Age))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1)
#Most of the employees are in 30-40 range

plot_grid(ggplot(master_file, aes(DistanceFromHome, fill = DistanceFromHome))+ geom_histogram(binwidth = 2,fill = "steelblue1", color = "black"),
          ggplot(master_file, aes(x="",y=DistanceFromHome))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1)
#Most of the employees stay within 2 km from office

plot_grid(ggplot(master_file, aes(MonthlyIncome, fill = MonthlyIncome))+ geom_histogram(binwidth = 500,fill = "steelblue1", color = "black"),
          ggplot(master_file, aes(x="",y=MonthlyIncome))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1)

#Income range above 150000 are outliers

plot_grid(ggplot(master_file, aes(factor(NumCompaniesWorked) , fill = NumCompaniesWorked ))+ geom_bar(fill = "steelblue1", color = "black"),
          ggplot(master_file, aes(x="",y=NumCompaniesWorked ))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1)


plot_grid(ggplot(master_file, aes(PercentSalaryHike , fill = PercentSalaryHike))+ geom_histogram(binwidth = 1,fill = "steelblue1", color = "black"),
          ggplot(master_file, aes(x="",y=PercentSalaryHike))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1)
#Most of the employees get a hike between 10-15%

plot_grid(ggplot(master_file, aes(factor(StandardHours), fill =StandardHours))+ geom_bar(fill = "steelblue1", color = "black"),
          ggplot(master_file, aes(x="",y=StandardHours))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1)
#only one level of data, we can get rid of it

plot_grid(ggplot(master_file, aes(TotalWorkingYears, fill =TotalWorkingYears))+ geom_histogram(binwidth = 1,fill = "steelblue1", color = "black"),
          ggplot(master_file, aes(x="",y=TotalWorkingYears))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1)
#People with 10 yrs of experience are more. Outliers are present with 30 plus years

plot_grid(ggplot(master_file, aes(TrainingTimesLastYear, fill =TrainingTimesLastYear))+ geom_histogram(binwidth = 1,fill = "steelblue1", color = "black"),
          ggplot(master_file, aes(x="",y=TrainingTimesLastYear))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1)


#Most of the employees have got 2-3 times training during the year. outliers are there

plot_grid(ggplot(master_file, aes(YearsAtCompany, fill =YearsAtCompany ))+ geom_histogram(binwidth = 1,fill = "steelblue1", color = "black"),
          ggplot(master_file, aes(x="",y=YearsAtCompany))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1)

#Most of the employees have been in the company between 0-10 years. Outliers are present in case of 15 years and above

plot_grid(ggplot(master_file, aes(YearsSinceLastPromotion, fill =YearsSinceLastPromotion ))+ geom_histogram(binwidth = 1,fill = "steelblue1", color = "black"),
          ggplot(master_file, aes(x="",y=YearsSinceLastPromotion))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1)

#Most of the employees are given promotion between 0-3 years. outliers are there in case of more than 7 years since last promotion

plot_grid(ggplot(master_file, aes(YearsWithCurrManager, fill =YearsWithCurrManager))+ geom_histogram(binwidth = 1,fill = "steelblue1", color = "black"),
          ggplot(master_file, aes(x="",y=YearsWithCurrManager))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1)
#Most of the employees stay between 0-2 years with current manager. outliers are present

plot_grid(ggplot(master_file, aes(Avg_hr_perday, fill =YearsWithCurrManager))+ geom_histogram(binwidth = 1,fill = "steelblue1", color = "black"),
          ggplot(master_file, aes(x="",y=Avg_hr_perday))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1)
#Most of the employees stay between 6-7 hours per day. outliers are present for employees who work more than 9+ hours

# Boxplots of numeric variables w.r.t attrition
plot_grid(ggplot(master_file, aes(x=Attrition,y=Age, fill=Attrition))+ geom_boxplot(width=0.2)+ 
            coord_flip() +theme(legend.position="none"),
          ggplot(master_file, aes(x=Attrition,y=DistanceFromHome, fill=Attrition))+ geom_boxplot(width=0.2)+
            coord_flip() + box_theme_y,
          ggplot(master_file, aes(x=Attrition,y=MonthlyIncome, fill=Attrition))+ geom_boxplot(width=0.2)+
            coord_flip() + box_theme_y,
          ggplot(master_file, aes(x=Attrition,y=NumCompaniesWorked , fill=Attrition))+ geom_boxplot(width=0.2)+
            coord_flip() + box_theme_y,
          align = "v",nrow = 1)

plot_grid(ggplot(master_file, aes(x=Attrition,y=PercentSalaryHike, fill=Attrition))+ geom_boxplot(width=0.2)+ 
            coord_flip() +theme(legend.position="none"),
          ggplot(master_file, aes(x=Attrition,y=TotalWorkingYears, fill=Attrition))+ geom_boxplot(width=0.2)+
            coord_flip() + box_theme_y,
          ggplot(master_file, aes(x=Attrition,y=TrainingTimesLastYear, fill=Attrition))+ geom_boxplot(width=0.2)+
            coord_flip() + box_theme_y,
          ggplot(master_file, aes(x=Attrition,y=YearsAtCompany, fill=Attrition))+ geom_boxplot(width=0.2)+
            coord_flip() + box_theme_y,
          align = "v",nrow = 1)


plot_grid(ggplot(master_file, aes(x=Attrition,y=YearsSinceLastPromotion, fill=Attrition))+ geom_boxplot(width=0.2)+ 
            coord_flip() +theme(legend.position="none"),
          ggplot(master_file, aes(x=Attrition,y=YearsWithCurrManager, fill=Attrition))+ geom_boxplot(width=0.2)+
            coord_flip() + box_theme_y,
          ggplot(master_file, aes(x=Attrition,y=Avg_hr_perday, fill=Attrition))+ geom_boxplot(width=0.2)+
            coord_flip() + box_theme_y,
          align = "v",nrow = 1)

#The median 'Avg_hr_perday' is high for employees who have left the company.

# Correlation between numeric variables
library(GGally)
ggpairs(master_file[, c("Age", "DistanceFromHome", "MonthlyIncome", "NumCompaniesWorked", "PercentSalaryHike", 
                        "TotalWorkingYears", "TrainingTimesLastYear", "YearsAtCompany", "YearsSinceLastPromotion",
                        "YearsWithCurrManager", "Avg_hr_perday")])

nr <- data.frame(master_file[, c("Age", "DistanceFromHome", "MonthlyIncome", "NumCompaniesWorked", "PercentSalaryHike", 
                                 "TotalWorkingYears", "TrainingTimesLastYear", "YearsAtCompany", "YearsSinceLastPromotion",
                                 "YearsWithCurrManager", "Avg_hr_perday")])

corrplot(cor(nr[,unlist(lapply(nr, is.numeric))], use = "complete.obs"), type = "lower", method = "number")

#years at company and years with current manager have a correlation of 0.77

################################################################
### Data Preparation

# De-Duplication: There are no duplicate rows present in the data


# Bringing the variables in the correct format
#From the str of master file we find the following,
#1. Target variable Attrition can be converted to 1/0 instead of "Yes"/"No"
#2. BusinessTravel, Department, EducationField, Gender, JobRole, MaritalStatus 
#are in character format they can be converted to factor variables and can be converted to dummy variables afterwards
#3. EmployeeCount, Over18 and StandardHours have only one level of data, we can get rid of them
#4. Dummy variables to be created for Education, JobLevel, StockOptionLevel, EnvironmentSatisfaction, 
#JobSatisfaction, WorkLifeBalance, JobInvolvement, PerformanceRating. 
#These variables are in int format, but are actually categorical variables.

#Converting target variable Attrition into 1/0

master_file$Attrition <- ifelse(master_file$Attrition == "Yes", 1, 0)

#Converting the chr variables which are actually factor into correct format

chr <- c("BusinessTravel", "Department", "EducationField", "Gender", "JobRole", "MaritalStatus")

master_file[chr] <- lapply(master_file[chr], as.factor)

#Converting the int variables which are actually factor into correct format

int <- c("Education", "JobLevel", "StockOptionLevel", "EnvironmentSatisfaction", 
         "JobSatisfaction", "WorkLifeBalance", "JobInvolvement", "PerformanceRating")

master_file[int] <- lapply(master_file[int], as.factor)

#Removing the columns having only one level of data: EmployeeCount, Over18, StandardHours

master_file <- master_file[, -c(9,16,18)]



# Outlier treatment and imputing missing value
OutVals = boxplot(master_file[,c("Age","DistanceFromHome","MonthlyIncome", "NumCompaniesWorked", "PercentSalaryHike", 
                                 "TotalWorkingYears", "TrainingTimesLastYear", "YearsAtCompany", "YearsSinceLastPromotion",
                                 "YearsWithCurrManager")])$out

#1.Age
boxplot(master_file$Age)$out #No outlier

#2. Distance from home
boxplot(master_file$DistanceFromHome)$out #No outlier

#3. Monthly Income
boxplot(master_file$MonthlyIncome, plot = FALSE)$out #outliers are present
View(subset(master_file, MonthlyIncome >= 193280))

#The high monthly income employees belong to only sales and Research & Development department
#which may mean that the average salary of these department is high. 
#Also we found some pattern in the monthly income
#Such as reserach director, research scientist have high salaries
#Sales Representative and Labrotary technician with medical and technical degree are having high salaries
#Capping them may lead to faulty representation of the data.Hence, not capping these values

#4. Number of companies worked 
boxplot(master_file$NumCompaniesWorked)$out #outliers are present
View(subset(master_file, NumCompaniesWorked >= 9))
##The values here are not much farther from the upper fence, so we will retain them

#5. Percent salary hike
boxplot(master_file$PercentSalaryHike)$out #No outliers are present

#6. Total working years
boxplot(master_file$TotalWorkingYears, plot=FALSE)$out #outliers are present
a <- subset(master_file, TotalWorkingYears >= 29)
min(master_file$Age) #minimum age is 18 years
min(a$Age) #minimum age of people having 29 years plus exp is 47
##This means that people who would have joined at the age of 18 years, would have got 29 years of experience by now
##We will not cap the high values


#7. Training times last year
boxplot(master_file$TrainingTimesLastYear)$out #outliers are present
#A lot of people have got 0, 5, 6 trainings per year which may happen in an working environment
#We will not treat this as an outlier


#8. Years at company
boxplot(master_file$YearsAtCompany)$out #outliers are present
b <- subset(master_file, YearsAtCompany >= 19)
min(master_file$Age) #minimum age is 18 years
min(b$Age) #minimum age of employees who have spent more than 19 years in the company is 38 years
##This means that people who would have joined at the age of 18-19 years 
##and have not left the company than, they have spent minimum 18-19 years in the company, which is possible
##We will not cap the high values

#9. Years since last prmotion
boxplot(master_file$YearsSinceLastPromotion)$out #outliers are present
c <- subset(master_file, YearsSinceLastPromotion >= 8)

ifelse(c$YearsAtCompany >= c$YearsSinceLastPromotion, TRUE, FALSE) 
#The total years at company is greater than years since last promotion in all cases means there are no outliers
#May be some people are not given promotion

#10. Years with current manager
boxplot(master_file$YearsWithCurrManager)$out #outliers are present
#The values here are not much farther from the upper fence, so we will retain them


#11. Average hour per day
boxplot(master_file$Avg_hr_perday)$out #outliers are present
d <- subset(master_file, Avg_hr_perday > 9)

sum(d$Attrition)/nrow(d) #30% attrition rate is present in case of employees who work more than 9 hours

#Hence, we cannot treat them as outlier or cap them
#Capping them means we will miss out on one of the most important behaiviour of employees who leave the company.

# Missing value
sapply(master_file, function(x) sum(is.na(x))) 
# Missing values in the following
#1. NumCompaniesWorked - 19
#2. TotalWorkingYears - 9
#3. EnvironmentSatisfaction - 25
#4. JobSatisfaction - 20
#5. WorkLifeBalance - 38

View(subset(master_file, is.na(NumCompaniesWorked)))
View(subset(master_file, is.na(TotalWorkingYears)))
View(subset(master_file, is.na(EnvironmentSatisfaction))) 
View(subset(master_file, is.na(JobSatisfaction)))
View(subset(master_file, is.na(WorkLifeBalance)))
#We dont find any pattern in missing values here
#We can delete the data which is around 2.5% of total data

master_file <- na.omit(master_file)

#This file has 11 numeric variables
#1 target variable, 1 unique employee ID column
#14 factor variables

################################################################

# Feature standardisation

#creating a data frame for all numeric variables

num_var <- master_file[, c("Age","DistanceFromHome","MonthlyIncome", "NumCompaniesWorked", "PercentSalaryHike", 
                           "TotalWorkingYears", "TrainingTimesLastYear", "YearsAtCompany", "YearsSinceLastPromotion",
                           "YearsWithCurrManager", "Avg_hr_perday")]

# Scaling continuous features 
num_var <- data.frame(scale(num_var))

# Checking churn rate of prospect customer

Attrition_rate <- sum(master_file$Attrition)/nrow(master_file)
Attrition_rate # 16.16% attrition rate. 

# creating a dataframe of factor features
fact_var <- master_file[,c("BusinessTravel", "Department", "EducationField", "Gender",
                           "JobRole", "MaritalStatus","Education", "JobLevel", "StockOptionLevel", 
                           "EnvironmentSatisfaction", 
                           "JobSatisfaction", "WorkLifeBalance", "JobInvolvement", "PerformanceRating" )]

# creating dummy variables for factor attributes
dummies<- data.frame(sapply(fact_var, function(x) data.frame(model.matrix(~x-1,data =fact_var))[,-1]))

# Final dataset
master_final<- cbind(master_file[,c(1,3)], num_var, dummies) 
View(master_final) #4300 obs. of  56 variables including employee id

########################################################################
# splitting the data between train and test
set.seed(100)

indices = sample.split(master_final$Attrition, SplitRatio = 0.7)

train = master_final[indices,]

test = master_final[!(indices),]

########################################################################
# Logistic Regression: 

#Initial model
model_1 = glm(Attrition ~ ., data = train[,-1], family = "binomial")
summary(model_1) #AIC 2270.5....nullDev 2661.4 ...resDev 2160.5 

# Stepwise selection

step <- stepAIC(model_1, direction="both")

step

#Executing the model_2(the last model of step)

model_2 =  glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                 TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                 Avg_hr_perday + BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
                 EducationField.xLife.Sciences + EducationField.xMarketing + 
                 EducationField.xMedical + EducationField.xOther + EducationField.xTechnical.Degree + 
                 JobRole.xHuman.Resources + JobRole.xManager + JobRole.xManufacturing.Director + 
                 JobRole.xResearch.Director + JobRole.xSales.Executive + MaritalStatus.xMarried + 
                 MaritalStatus.xSingle + Education.x5 + JobLevel.x2 + JobLevel.x5 + 
                 StockOptionLevel.x1 + StockOptionLevel.x3 + EnvironmentSatisfaction.x2 + 
                 EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 + 
                 JobSatisfaction.x2 + JobSatisfaction.x3 + JobSatisfaction.x4 + 
                 WorkLifeBalance.x2 + WorkLifeBalance.x3 + WorkLifeBalance.x4 + 
                 JobInvolvement.x3, family = "binomial", data = train[, -1])

summary(model_2) #AIC: 2097.6

sort(vif(model_2))

#EducationField.xLife.Sciences, EducationField.xMedical , EducationField.xMarketing have high vif, 
# but they are highly significant also. 


#Removing EducationField.xLife.Sciences  due to high vif

model_3 = glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                Avg_hr_perday + BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
                EducationField.xMarketing + 
                EducationField.xMedical + EducationField.xOther + EducationField.xTechnical.Degree + 
                JobRole.xHuman.Resources + JobRole.xManager + JobRole.xManufacturing.Director + 
                JobRole.xResearch.Director + JobRole.xSales.Executive + MaritalStatus.xMarried + 
                MaritalStatus.xSingle + Education.x5 + JobLevel.x2 + JobLevel.x5 + 
                StockOptionLevel.x1 + StockOptionLevel.x3 + EnvironmentSatisfaction.x2 + 
                EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 + 
                JobSatisfaction.x2 + JobSatisfaction.x3 + JobSatisfaction.x4 + 
                WorkLifeBalance.x2 + WorkLifeBalance.x3 + WorkLifeBalance.x4 + 
                JobInvolvement.x3, family = "binomial", data = train[, -1])

summary(model_3) #AIC: 2111.7

sort(vif(model_3))

#WorkLifeBalance.x3 , BusinessTravel.xTravel_Frequently, BusinessTravel.xTravel_Rarely have high vif
#Removing BusinessTravel.xTravel_Rarely due to low significance amongst other high vif parameters

model_4 =glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
               TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
               Avg_hr_perday + BusinessTravel.xTravel_Frequently + EducationField.xMarketing + 
               EducationField.xMedical + EducationField.xOther + EducationField.xTechnical.Degree + 
               JobRole.xHuman.Resources + JobRole.xManager + JobRole.xManufacturing.Director + 
               JobRole.xResearch.Director + JobRole.xSales.Executive + MaritalStatus.xMarried + 
               MaritalStatus.xSingle + Education.x5 + JobLevel.x2 + JobLevel.x5 + 
               StockOptionLevel.x1 + StockOptionLevel.x3 + EnvironmentSatisfaction.x2 + 
               EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 + 
               JobSatisfaction.x2 + JobSatisfaction.x3 + JobSatisfaction.x4 + 
               WorkLifeBalance.x2 + WorkLifeBalance.x3 + WorkLifeBalance.x4 + 
               JobInvolvement.x3, family = "binomial", data = train[, -1])

summary(model_4) #AIC: 2119.1

sort(vif(model_4))

#Removing WorkLifeBalance.x2 due to high vif

model_5 = glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                Avg_hr_perday + BusinessTravel.xTravel_Frequently + EducationField.xMarketing + 
                EducationField.xMedical + EducationField.xOther + EducationField.xTechnical.Degree + 
                JobRole.xHuman.Resources + JobRole.xManager + JobRole.xManufacturing.Director + 
                JobRole.xResearch.Director + JobRole.xSales.Executive + MaritalStatus.xMarried + 
                MaritalStatus.xSingle + Education.x5 + JobLevel.x2 + JobLevel.x5 + 
                StockOptionLevel.x1 + StockOptionLevel.x3 + EnvironmentSatisfaction.x2 + 
                EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 + 
                JobSatisfaction.x2 + JobSatisfaction.x3 + JobSatisfaction.x4 +WorkLifeBalance.x3 + WorkLifeBalance.x4 + 
                JobInvolvement.x3, family = "binomial", data = train[, -1])


summary(model_5) #AIC: 2134.5

sort(vif(model_5))


#Removing MaritalStatus.xMarried  due to low significance 

model_6 = glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                Avg_hr_perday + BusinessTravel.xTravel_Frequently + EducationField.xMarketing + 
                EducationField.xMedical + EducationField.xOther + EducationField.xTechnical.Degree + 
                JobRole.xHuman.Resources + JobRole.xManager + JobRole.xManufacturing.Director + 
                JobRole.xResearch.Director + JobRole.xSales.Executive + 
                MaritalStatus.xSingle + Education.x5 + JobLevel.x2 + JobLevel.x5 + 
                StockOptionLevel.x1 + StockOptionLevel.x3 + EnvironmentSatisfaction.x2 + 
                EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 + 
                JobSatisfaction.x2 + JobSatisfaction.x3 + JobSatisfaction.x4 +WorkLifeBalance.x3 + WorkLifeBalance.x4 + 
                JobInvolvement.x3, family = "binomial", data = train[, -1])

summary(model_6) #AIC: 2135.8

sort(vif(model_6))

#Now checking for the variables having low significance
#Removing EducationField.xMarketing due to low significance 

model_7 =  glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                 TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                 Avg_hr_perday + BusinessTravel.xTravel_Frequently +
                 EducationField.xMedical + EducationField.xOther + EducationField.xTechnical.Degree + 
                 JobRole.xHuman.Resources + JobRole.xManager + JobRole.xManufacturing.Director + 
                 JobRole.xResearch.Director + JobRole.xSales.Executive + 
                 MaritalStatus.xSingle + Education.x5 + JobLevel.x2 + JobLevel.x5 + 
                 StockOptionLevel.x1 + StockOptionLevel.x3 + EnvironmentSatisfaction.x2 + 
                 EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 + 
                 JobSatisfaction.x2 + JobSatisfaction.x3 + JobSatisfaction.x4 +WorkLifeBalance.x3 + WorkLifeBalance.x4 + 
                 JobInvolvement.x3, family = "binomial", data = train[, -1])

summary(model_7) #AIC: 2135

sort(vif(model_7))


#Removing EducationField.xMedical   due to low significance 

model_8 = glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                Avg_hr_perday + BusinessTravel.xTravel_Frequently + EducationField.xOther + EducationField.xTechnical.Degree + 
                JobRole.xHuman.Resources + JobRole.xManager + JobRole.xManufacturing.Director + 
                JobRole.xResearch.Director + JobRole.xSales.Executive + 
                MaritalStatus.xSingle + Education.x5 + JobLevel.x2 + JobLevel.x5 + 
                StockOptionLevel.x1 + StockOptionLevel.x3 + EnvironmentSatisfaction.x2 + 
                EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 + 
                JobSatisfaction.x2 + JobSatisfaction.x3 + JobSatisfaction.x4 +WorkLifeBalance.x3 + WorkLifeBalance.x4 + 
                JobInvolvement.x3, family = "binomial", data = train[, -1])

summary(model_8) #AIC: 2134.1

sort(vif(model_8))


#Removing EducationField.xOther due to low significance 

model_9 =  glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                 TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                 Avg_hr_perday + BusinessTravel.xTravel_Frequently + EducationField.xTechnical.Degree + 
                 JobRole.xHuman.Resources + JobRole.xManager + JobRole.xManufacturing.Director + 
                 JobRole.xResearch.Director + JobRole.xSales.Executive + 
                 MaritalStatus.xSingle + Education.x5 + JobLevel.x2 + JobLevel.x5 + 
                 StockOptionLevel.x1 + StockOptionLevel.x3 + EnvironmentSatisfaction.x2 + 
                 EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 + 
                 JobSatisfaction.x2 + JobSatisfaction.x3 + JobSatisfaction.x4 +WorkLifeBalance.x3 + WorkLifeBalance.x4 + 
                 JobInvolvement.x3, family = "binomial", data = train[, -1])

summary(model_9) #AIC: 2134

sort(vif(model_9))


#Removing StockOptionLevel.x3 due to low significance 

model_10 = glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                 TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                 Avg_hr_perday + BusinessTravel.xTravel_Frequently + EducationField.xTechnical.Degree + 
                 JobRole.xHuman.Resources + JobRole.xManager + JobRole.xManufacturing.Director + 
                 JobRole.xResearch.Director + JobRole.xSales.Executive + 
                 MaritalStatus.xSingle + Education.x5 + JobLevel.x2 + JobLevel.x5 + 
                 StockOptionLevel.x1 + EnvironmentSatisfaction.x2 + 
                 EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 + 
                 JobSatisfaction.x2 + JobSatisfaction.x3 + JobSatisfaction.x4 +WorkLifeBalance.x3 + WorkLifeBalance.x4 + 
                 JobInvolvement.x3, family = "binomial", data = train[, -1])

summary(model_10) #AIC: 2134


#Removing WorkLifeBalance.x4  due to low significance 

model_11 = glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                 TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                 Avg_hr_perday + BusinessTravel.xTravel_Frequently + EducationField.xTechnical.Degree + 
                 JobRole.xHuman.Resources + JobRole.xManager + JobRole.xManufacturing.Director + 
                 JobRole.xResearch.Director + JobRole.xSales.Executive + 
                 MaritalStatus.xSingle + Education.x5 + JobLevel.x2 + JobLevel.x5 + 
                 StockOptionLevel.x1 + EnvironmentSatisfaction.x2 + 
                 EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 + 
                 JobSatisfaction.x2 + JobSatisfaction.x3 + JobSatisfaction.x4 +WorkLifeBalance.x3 +  
                 JobInvolvement.x3, family = "binomial", data = train[, -1])


summary(model_11) #AIC: 2134.2

sort(vif(model_11))


#Removing JobLevel.x5  due to low significance 

model_12 = glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                 TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                 Avg_hr_perday + BusinessTravel.xTravel_Frequently + EducationField.xTechnical.Degree + 
                 JobRole.xHuman.Resources + JobRole.xManager + JobRole.xManufacturing.Director + 
                 JobRole.xResearch.Director + JobRole.xSales.Executive + 
                 MaritalStatus.xSingle + Education.x5 + JobLevel.x2 +  StockOptionLevel.x1 + EnvironmentSatisfaction.x2 + 
                 EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 + 
                 JobSatisfaction.x2 + JobSatisfaction.x3 + JobSatisfaction.x4 +WorkLifeBalance.x3 +  
                 JobInvolvement.x3, family = "binomial", data = train[, -1])

summary(model_12) #AIC: 2134.4

sort(vif(model_12))



#Removing EducationField.xTechnical.Degree  due to low significance 

model_13 =  glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                  TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                  Avg_hr_perday + BusinessTravel.xTravel_Frequently +  
                  JobRole.xHuman.Resources + JobRole.xManager + JobRole.xManufacturing.Director + 
                  JobRole.xResearch.Director + JobRole.xSales.Executive + 
                  MaritalStatus.xSingle + Education.x5 + JobLevel.x2 +  StockOptionLevel.x1 + EnvironmentSatisfaction.x2 + 
                  EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 + 
                  JobSatisfaction.x2 + JobSatisfaction.x3 + JobSatisfaction.x4 +WorkLifeBalance.x3 +  
                  JobInvolvement.x3, family = "binomial", data = train[, -1])

summary(model_13) #AIC: 2134.7

sort(vif(model_13))


#Removing JobRole.xHuman.Resources due to low significance 

model_14 = glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                 TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                 Avg_hr_perday + BusinessTravel.xTravel_Frequently +JobRole.xManager + JobRole.xManufacturing.Director + 
                 JobRole.xResearch.Director + JobRole.xSales.Executive + 
                 MaritalStatus.xSingle + Education.x5 + JobLevel.x2 +  StockOptionLevel.x1 + EnvironmentSatisfaction.x2 + 
                 EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 + 
                 JobSatisfaction.x2 + JobSatisfaction.x3 + JobSatisfaction.x4 +WorkLifeBalance.x3 +  
                 JobInvolvement.x3, family = "binomial", data = train[, -1])

summary(model_14) #AIC: 2135.7

sort(vif(model_14))

#Removing Education.x5  due to low significance 

model_15 = glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                 TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                 Avg_hr_perday + BusinessTravel.xTravel_Frequently +JobRole.xManager + JobRole.xManufacturing.Director + 
                 JobRole.xResearch.Director + JobRole.xSales.Executive + 
                 MaritalStatus.xSingle +JobLevel.x2 +  StockOptionLevel.x1 + EnvironmentSatisfaction.x2 + 
                 EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 + 
                 JobSatisfaction.x2 + JobSatisfaction.x3 + JobSatisfaction.x4 +WorkLifeBalance.x3 +  
                 JobInvolvement.x3, family = "binomial", data = train[, -1])

summary(model_15) #AIC: 2137.7


#Removing StockOptionLevel.x1  due to low significance 

model_16 =glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                Avg_hr_perday + BusinessTravel.xTravel_Frequently +JobRole.xManager + JobRole.xManufacturing.Director + 
                JobRole.xResearch.Director + JobRole.xSales.Executive + 
                MaritalStatus.xSingle +JobLevel.x2 + EnvironmentSatisfaction.x2 + 
                EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 + 
                JobSatisfaction.x2 + JobSatisfaction.x3 + JobSatisfaction.x4 +WorkLifeBalance.x3 +  
                JobInvolvement.x3, family = "binomial", data = train[, -1])

summary(model_16) #AIC: 2140.6 


#Removing JobRole.xSales.Executive due to low significance 

model_17 = glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                 TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                 Avg_hr_perday + BusinessTravel.xTravel_Frequently +JobRole.xManager + JobRole.xManufacturing.Director + 
                 JobRole.xResearch.Director + MaritalStatus.xSingle +JobLevel.x2 + EnvironmentSatisfaction.x2 + 
                 EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 + 
                 JobSatisfaction.x2 + JobSatisfaction.x3 + JobSatisfaction.x4 +WorkLifeBalance.x3 +  
                 JobInvolvement.x3, family = "binomial", data = train[, -1])


summary(model_17) #AIC: 2143.7 

#Removing JobLevel.x2   due to low significance 

model_18 = glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                 TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                 Avg_hr_perday + BusinessTravel.xTravel_Frequently +JobRole.xManager + JobRole.xManufacturing.Director + 
                 JobRole.xResearch.Director + MaritalStatus.xSingle + EnvironmentSatisfaction.x2 + 
                 EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 + 
                 JobSatisfaction.x2 + JobSatisfaction.x3 + JobSatisfaction.x4 +WorkLifeBalance.x3 +  
                 JobInvolvement.x3, family = "binomial", data = train[, -1])

summary(model_18) #AIC: 2147

#Removing JobRole.xResearch.Director due to low significance 

model_19 = glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                 TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                 Avg_hr_perday + BusinessTravel.xTravel_Frequently +JobRole.xManager + JobRole.xManufacturing.Director + 
                 MaritalStatus.xSingle + EnvironmentSatisfaction.x2 + 
                 EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 + 
                 JobSatisfaction.x2 + JobSatisfaction.x3 + JobSatisfaction.x4 +WorkLifeBalance.x3 +  
                 JobInvolvement.x3, family = "binomial", data = train[, -1])

summary(model_19) #AIC: 2150.1

#Removing JobInvolvement.x3  due to low significance 

model_20 = glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                 TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                 Avg_hr_perday + BusinessTravel.xTravel_Frequently +JobRole.xManager + JobRole.xManufacturing.Director + 
                 MaritalStatus.xSingle + EnvironmentSatisfaction.x2 + 
                 EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 + 
                 JobSatisfaction.x2 + JobSatisfaction.x3 + JobSatisfaction.x4 +WorkLifeBalance.x3,
               family = "binomial", data = train[, -1])

summary(model_20) #AIC: 2154.5

#Removing JobRole.xManager due to low significance 

model_20 = glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                 TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                 Avg_hr_perday + BusinessTravel.xTravel_Frequently +JobRole.xManufacturing.Director + 
                 MaritalStatus.xSingle + EnvironmentSatisfaction.x2 + 
                 EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 + 
                 JobSatisfaction.x2 + JobSatisfaction.x3 + JobSatisfaction.x4 +WorkLifeBalance.x3,
               family = "binomial", data = train[, -1])

summary(model_20) #AIC: 2161.5 #This model has all significant variable with three *** & 17 variables


########################################################################
# With 17 significant variables in the model

final_model<- model_20

#######################################################################

### Model Evaluation

### Test Data ####

#predicted probabilities of Attrition 1 for test data

test_pred = predict(final_model, type = "response", 
                    newdata = test[,-c(1,2)])


# summary 

summary(test_pred)

test$prob <- test_pred
View(test)
# Let's use the probability cutoff of 50%.

test_pred_attr <- factor(ifelse(test_pred >= 0.50, "Yes", "No"))
test_actual_attr <- factor(ifelse(test$Attrition==1,"Yes","No"))


table(test_actual_attr,test_pred_attr)


#######################################################################

library(e1071)

test_conf <- confusionMatrix(test_pred_attr, test_actual_attr, positive = "Yes")
test_conf
#######################################################################

# Let's Choose the cutoff value. 
# 

# Let's find out the optimal probalility cutoff 

perform_fn <- function(cutoff) 
{
  predicted_attr <- factor(ifelse(test_pred >= cutoff, "Yes", "No"))
  conf <- confusionMatrix(predicted_attr, test_actual_attr, positive = "Yes")
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  out <- t(as.matrix(c(sens, spec, acc))) 
  colnames(out) <- c("sensitivity", "specificity", "accuracy")
  return(out)
}

# Creating cutoff values from 0.0002342 to 0.8560726 for plotting and initiallizing a matrix of 100 X 3.

# Summary of test probability

summary(test_pred)

s = seq(.01,.80,length=100)

OUT = matrix(0,100,3)


for(i in 1:100)
{
  OUT[i,] = perform_fn(s[i])
} 


plot(s, OUT[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s,OUT[,2],col="darkgreen",lwd=2)
lines(s,OUT[,3],col=4,lwd=2)
box()
legend(0,.50,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy"))


cutoff <- s[which(abs(OUT[,1]-OUT[,2])<0.01)]


# Let's choose a cutoff value of 0.19 for final model

test_cutoff_attr <- factor(ifelse(test_pred >=0.19, "Yes", "No"))

conf_final <- confusionMatrix(test_cutoff_attr, test_actual_attr, positive = "Yes")

acc <- conf_final$overall[1]

sens <- conf_final$byClass[1]

spec <- conf_final$byClass[2]

acc

sens

spec

View(test)
##################################################################################################
### KS -statistic - Test Data ######

test_cutoff_attr <- ifelse(test_cutoff_attr=="Yes",1,0)
test_actual_attr <- ifelse(test_actual_attr=="Yes",1,0)


library(ROCR)
#on testing  data
pred_object_test<- prediction(test_cutoff_attr, test_actual_attr)

performance_measures_test<- performance(pred_object_test, "tpr", "fpr")

ks_table_test <- attr(performance_measures_test, "y.values")[[1]] - 
  (attr(performance_measures_test, "x.values")[[1]])

max(ks_table_test)


####################################################################
# Lift & Gain Chart 

# plotting the lift chart

# Loading dplyr package 
require(dplyr)
library(dplyr)

lift <- function(labels , predicted_prob,groups=10) {
  
  if(is.factor(labels)) labels  <- as.integer(as.character(labels ))
  if(is.factor(predicted_prob)) predicted_prob <- as.integer(as.character(predicted_prob))
  helper = data.frame(cbind(labels , predicted_prob))
  helper[,"bucket"] = ntile(-helper[,"predicted_prob"], groups)
  gaintable = helper %>% group_by(bucket)  %>%
    summarise_at(vars(labels ), funs(total = n(),
                                     totalresp=sum(., na.rm = TRUE))) %>%
    
    mutate(Cumresp = cumsum(totalresp),
           Gain=Cumresp/sum(totalresp)*100,
           Cumlift=Gain/(bucket*(100/groups))) 
  return(gaintable)
}

Churn_decile = lift(test_actual_attr, test_pred, groups = 10)
