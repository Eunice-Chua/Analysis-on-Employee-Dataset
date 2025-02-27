#EUNICE CHUA
#TP067893

# Data import (Read file)
EmployeeData<-read.csv("C:\\Users\\hp\\Documents\\PFDA Assignment\\employee_attrition (New).csv",header=TRUE)
EmployeeData

# Install packages and library
install.packages("dplyr")
library(dplyr)

install.packages("ggplot2")
library(ggplot2)

# Data Cleaning
# Identify null values
lapply(EmployeeData,function(x){length(which(is.na(x)))})

# DATA PREPROCESSING

# Cleaned dataset stored in variable named "empData"
empData<-EmployeeData

# Assigning headers
names(empData)=c("EmployeeID","RecordDate_key","bday_key","OrigHireDate_key","TerminationDate_key",
                 "Age","LengthOfService","CityName","DepartmentName","JobTitle","StoreNo","GenderShort",
                 "GenderFull","TermReason_desc","TermType_desc","StatusYear","Status","BusinessUnit")

# Removing duplicate Employee ID
empData=empData%>%
  group_by(EmployeeID)%>%
  filter(StatusYear==max(StatusYear))%>%
  filter(RecordDate_key==max(RecordDate_key))

# Change the spelling 
## 1.
empData$TermReason_desc<-ifelse(empData$TermReason_desc=="Resignaton","Resignation",empData$TermReason_desc)
## 2.
empData$JobTitle<-ifelse(empData$JobTitle=="CHief Information Officer","Chief Information Officer",empData$JobTitle)
## 3.
empData$CityName<-ifelse(empData$CityName=="New Westminister","New Westminster",empData$CityName)


# Check which terminated employees have the wrong data
empData%>%filter(Status=="ACTIVE")%>%
  filter(TerminationDate_key!="1/1/1900")%>%
  View()
## Employee ID: 3008, 3401, 7007, 7023, 8296

# Check for original records for terminate reason and type

# Employee ID 3008
EmployeeData%>%
  filter(EmployeeID==3008)%>%
  View()
# Updating
empData$TermType_desc[empData$EmployeeID==3008]<-"Voluntary"
empData$TermReason_desc[empData$EmployeeID==3008]<-"Retirement"
empData$Status[empData$EmployeeID==3008]<-"TERMINATED"

# Employee ID 3401
EmployeeData%>%
  filter(EmployeeID==3401)%>%
  View()
# Updating
empData$TermType_desc[empData$EmployeeID==3401]<-"Voluntary"
empData$TermReason_desc[empData$EmployeeID==3401]<-"Retirement"
empData$Status[empData$EmployeeID==3401]<-"TERMINATED"

# Employee ID 7007
EmployeeData%>%
  filter(EmployeeID==7007)%>%
  View()
# Updating
empData$TermType_desc[empData$EmployeeID==7007]<-"Voluntary"
empData$TermReason_desc[empData$EmployeeID==7007]<-"Resignation"
empData$Status[empData$EmployeeID==7007]<-"TERMINATED"

# Employee ID 7023
EmployeeData%>%
  filter(EmployeeID==7023)%>%
  View()
# Updating
empData$TermType_desc[empData$EmployeeID==7023]<-"Voluntary"
empData$TermReason_desc[empData$EmployeeID==7023]<-"Resignation"
empData$Status[empData$EmployeeID==7023]<-"TERMINATED"

# Employee ID 8296
EmployeeData%>%
  filter(EmployeeID==8296)%>%
  View()
# Updating
empData$TermType_desc[empData$EmployeeID==8296]<-"Voluntary"
empData$TermReason_desc[empData$EmployeeID==8296]<-"Resignation"
empData$Status[empData$EmployeeID==8296]<-"TERMINATED"

# Change termination date of active employees to Not Applicable
empData$TerminationDate_key <- ifelse(empData$Status == "ACTIVE", NA, empData$TerminationDate_key)

# Delete excessive columns
empData["GenderShort"]=NULL
empData["bday_key"]=NULL

# Convert to date data type 
empData$RecordDate_key<-as.Date(empData$RecordDate_key,format="%m/%d/%Y")
empData$OrigHireDate_key <- as.Date(empData$OrigHireDate_key, format = "%m/%d/%Y")
# "origin" required when converting date values from numeric to character format.
empData$TerminationDate_key<-as.Date(empData$TerminationDate_key,format="%m/%d/%Y", origin = "01/01/1970")

# Convert to factor data type
empData$CityName<-as.factor(empData$CityName)
empData$DepartmentName<-as.factor(empData$DepartmentName)
empData$JobTitle<-as.factor(empData$JobTitle)
empData$StoreNo<-as.numeric(empData$StoreNo)
empData$GenderFull<-as.factor(empData$GenderFull)
empData$TermReason_desc<-as.factor(empData$TermReason_desc)
empData$TermType_desc<-as.factor(empData$TermType_desc)
empData$Status<-as.factor(empData$Status)
empData$BusinessUnit<-as.factor(empData$BusinessUnit)

# DATA EXPLORATION

# View headers' name
names(empData)

# Class of empData
class(empData)

# No of columns in empData
ncol(empData)

# No of rows in empData
nrow(empData)

# Structure of empData
str(empData)

# Summary of empData
summary(empData)

# Display data
# First 10 lines
head(empData,10)
# Last 10 lines
tail(empData,10)

# Question 1: What is the reason employees leave the company voluntary?

# Analysis 1.1: How many employees terminate voluntary?
terminateVoluntary<-empData[(empData$TermType_desc=="Voluntary"),]
totalTerminateVoluntary=nrow(terminateVoluntary)
totalTerminateVoluntary
## 1270 employees terminate voluntary

# Analysis 1.2: Find the rate of terminate voluntary.
TotalEmployeesinTerminatedStatus <- nrow(empData[empData$Status == "TERMINATED",])
TotalEmployeesinTerminatedStatus

Terminate_Voluntary_RATE <- round((totalTerminateVoluntary / TotalEmployeesinTerminatedStatus) * 100, digits = 2)
Terminate_Voluntary_RATE

terminateInvoluntary <- empData[empData$TermType_desc == "Involuntary", ]
totalterminateInvoluntary <- nrow(terminateInvoluntary)
totalterminateInvoluntary

# Create data for the pie chart
terminationCounts <- c(totalTerminateVoluntary, totalterminateInvoluntary)
terminationLabels <- c("Voluntary", "Involuntary")

# Create a pie chart
pie(terminationCounts, labels = paste(terminationCounts, "(", 
                                      round((terminationCounts / TotalEmployeesinTerminatedStatus) * 100, digits = 2), "%)"),
    radius = 1.5, main = "Termination Type", col = c("#83BD75", "#B4E197"),cex.main=2.5,border = c("#83BD75", "#B4E197"))

# Add a legend
legend(x="bottom",y=0, legend = terminationLabels, fill = c("#83BD75", "#B4E197"), 
       title = "Termination Type", cex = 0.8, horiz = TRUE)

## Total employees in terminated status are 1485.
## Terminate voluntary rate is 85.52%.
## From the pie chart, we can see that rate of terminate voluntary is higher than terminate involuntary.

# Analysis 1.3: Relationship between termination reason and termination type which is voluntary
voluntary<-empData$TermType_desc=="Voluntary"
reason<-empData$TermReason_desc[voluntary]
nlevels(factor(reason))
levels(factor(reason))

# Analysis 1.4: Relationship between age and the terminated voluntary employees
Age_of_Terminated_voluntary<-terminateVoluntary$Age
round(summary(Age_of_Terminated_voluntary))
age_terminatedvoluntary_counts <- table(Age_of_Terminated_voluntary)
View(age_terminatedvoluntary_counts)

# Create a histogram of age distribution
hist(Age_of_Terminated_voluntary, breaks = 10, col = "#FFA500", border = "white",
     xlab = "Age", ylab = "Frequency", main = "Age Distribution of Terminated Voluntary Employees")

# Calculate the frequency counts for each bin
hist_obj <- hist(Age_of_Terminated_voluntary, breaks = 10, plot = FALSE)
bin_counts <- hist_obj$counts

# Add labels to the histogram bars
text(hist_obj$mids, bin_counts, labels = bin_counts, pos = 3)

# Filter the data for terminated voluntary employees and specific termination reasons
age_termVoluntary <- empData %>%
  filter(Status == "TERMINATED") %>%
  filter(TermType_desc == "Voluntary") %>%
  filter(TermReason_desc %in% c("Retirement", "Resignation"))

# Create a box plot of age against termination reason (retirement and resignation) using ggplot2
ggplot(age_termVoluntary, aes(x = TermReason_desc, y = Age, fill = TermReason_desc)) +
  geom_boxplot() +
  labs(x = "Termination Reason", y = "Age", 
       title = "Age Distribution by Termination Reason (Retirement and Resignation)") +
  geom_jitter(color="black",size=0.4,alpha=0.9)+
  theme(plot.title = element_text(hjust = 0.5, vjust = 0.5))+
  scale_fill_manual(values = c("Retirement" = "#577D86", "Resignation" = "#569DAA")) 

## Highest age of terminated voluntary employees are 65 years old.
## Lowest age of terminated voluntary employees are 19 years old.
## Average age of terminated voluntary employees are 53 years old.


# Analysis 1.5: How many employees resign and retire?
Resign_voluntary<-empData[(empData$TermReason_desc=="Resignation")&(empData$TermType_desc=="Voluntary"),]
Total_Resign_voluntary<-nrow(Resign_voluntary)
Total_Resign_voluntary

Retire_voluntary<-empData[(empData$TermReason_desc=="Retirement")&(empData$TermType_desc=="Voluntary"),]
Total_Retire_voluntary<-nrow(Retire_voluntary)
Total_Retire_voluntary

# Create a bar chart
barplot(c(Total_Resign_voluntary, Total_Retire_voluntary), names.arg = c("Resignation", "Retirement"),
        xlab = "Termination Reason", ylab = "Number of Employees", col = c("#83BD75", "#B4E197"),
        main = "Voluntary Termination: Resignation vs Retirement")

# Add text labels for the values
text(1, Total_Resign_voluntary, Total_Resign_voluntary, pos = 1)
text(2, Total_Retire_voluntary, Total_Retire_voluntary, pos = 1)
## 385 employees terminate voluntary is because of resignation
## 885 employees terminate voluntary is because of retirement

# Analysis 1.6: Relationship between length of service and the terminated voluntary employees
Length_of_Service_terminatedvoluntary<-terminateVoluntary$LengthOfService
round(summary(Length_of_Service_terminatedvoluntary))

length_of_service_terminatedvoluntary_counts <- table(Length_of_Service_terminatedvoluntary)
length_of_service_terminatedvoluntary_counts <- as.data.frame(length_of_service_terminatedvoluntary_counts)

# Create the bar chart
ggplot(data = length_of_service_terminatedvoluntary_counts, 
       aes(x = Length_of_Service_terminatedvoluntary, y = Freq)) +
  geom_bar(stat = "identity", fill = "blue", color = "black") +
  labs(x = "Length of Service", y = "Count") +
  ggtitle("Bar Chart of Length of Service for Terminated Voluntary Employees")+
  geom_text(aes(label = Freq), vjust = -0.5)+
  theme(plot.title = element_text(hjust = 0.5))
## Highest length of service is 25 years
## Lowest length of service is 0 years
## Average length of service is 11 years.

# Analysis 1.7: Relationship between department and the terminated voluntary employees
dept_terminatedvoluntary<-terminateVoluntary$DepartmentName
dept_counts <- table(dept_terminatedvoluntary)
most_dept_count <- max(dept_counts)
most_dept <- names(dept_counts)[dept_counts == most_dept_count]
most_dept
least_dept_count <- min(dept_counts)
least_dept <- names(dept_counts)[dept_counts == least_dept_count]
least_dept


# Create a data frame with department and termination reason information
dept_reason_terminatedvoluntary <- data.frame(Department = dept_terminatedvoluntary, Reason = reason)

# Count the occurrences of each department and termination reason combination
dept_reason_counts <- count(dept_reason_terminatedvoluntary, Department, Reason)
dept_reason_counts

# Create the grouped bar chart
ggplot(dept_reason_counts, aes(x = Department, y=n, fill = Reason)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(x = "Department", y = "Count", fill = "Reason") +
  ggtitle("Number of Terminated Voluntary Employees by Department and Termination Reason") +
  theme(plot.title = element_text(hjust = 0.5, vjust = 0.5)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  coord_flip()

## The department that has the most terminated voluntary employees is Meats department while the least
## terminated voluntary employees is Executive department.

# Analysis 1.8: Relationship between year and the terminated voluntary employees
year_terminatedvoluntary<-terminateVoluntary$StatusYear
year_counts <- table(year_terminatedvoluntary)
most_year_count <- max(year_counts)
most_year <- names(year_counts)[year_counts == most_year_count]
most_year

# Create a bar chart with facet wrap
ggplot(terminateVoluntary, aes(x = Status)) +
  geom_bar(fill = "#F7DB6A") +
  labs(x = "Status", y = "Count") +
  ggtitle("Terminated Voluntary Employees by Year") +
  facet_wrap(~ StatusYear) +
  geom_text(stat = "count", aes(label = after_stat(count)), vjust = 1.5) +
  theme(plot.title = element_text(hjust = 0.5))

## The year that has the most terminated voluntary employees is in year 2008 which has 164 employees
## terminated voluntary.

# Subset the data for termination reasons retirement and resignation
terminateVoluntaryReason_chartData <- empData[empData$TermReason_desc %in% c("Retirement", "Resignation"), ]
# Create a bar chart
ggplot(terminateVoluntaryReason_chartData, aes(x = StatusYear, fill = TermReason_desc)) +
  geom_bar(position = "dodge") +
  geom_text(stat="count",aes(label=stat(count)),position=position_dodge(width=0.9),vjust=-0.5)+
  labs(x = "Year", y = "Count") +
  ggtitle("Termination Voluntary Reasons: Retirement vs. Resignation")+
  theme(plot.title = element_text(hjust = 0.5, vjust = 0.5))+
  scale_x_continuous(breaks = unique(terminateVoluntaryReason_chartData$StatusYear), 
                     labels = unique(terminateVoluntaryReason_chartData$StatusYear))
## Employees terminate voluntary are based on 2 reasons, either is resignation or retirement.


# Analysis 1.9: Relationship between business unit and the terminated voluntary employees
BusinessUnit_terminatedvoluntary<-terminateVoluntary$BusinessUnit
BusinessUnit_counts <- table(BusinessUnit_terminatedvoluntary)
most_BusinessUnit_count <- max(BusinessUnit_counts)
most_BusinessUnit <- names(BusinessUnit_counts)[BusinessUnit_counts == most_BusinessUnit_count]
most_BusinessUnit

dept_businessunit_terminatedvoluntary <- data.frame(Department = dept_terminatedvoluntary, BusinessUnit = BusinessUnit_terminatedvoluntary, Reason = reason)
dept_businessunit_counts <- count(dept_businessunit_terminatedvoluntary, Department, BusinessUnit, Reason)

ggplot(dept_businessunit_counts, aes(x = Department, y = n, fill = Reason)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(x = "Department", y = "Count", fill = "Reason") +
  ggtitle("Number of Terminated Voluntary Employees by Department, Business Unit and Termination Reason") +
  theme(plot.title = element_text(hjust = 0.5, vjust = 0.5)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_wrap(~ BusinessUnit) +
  coord_flip()
## The business unit that has the most terminated voluntary employees is Stores with 1201 employees.

# Analysis 1.10: Relationship between store name and the terminated voluntary employees
StoreNo_terminatedvoluntary<-terminateVoluntary$StoreNo
StoreNo_counts <- table(StoreNo_terminatedvoluntary)
most_StoreNo_count <- max(StoreNo_counts)
most_StoreNo <- names(StoreNo_counts)[StoreNo_counts == most_StoreNo_count]
most_StoreNo
View(StoreNo_counts)

# Convert StoreNo to numeric
StoreNo <- as.numeric(names(StoreNo_counts))
# Create a data frame with StoreNo and Count
store_counts <- data.frame(StoreNo, Count = as.numeric(StoreNo_counts))
# Create a line chart
ggplot(data = store_counts, aes(x = StoreNo, y = Count)) +
  geom_line(color = "steelblue") +
  geom_point(color = "steelblue") +
  labs(x = "StoreNo", y = "Count") +
  ggtitle("Number of Terminated Voluntary Employees by StoreNo") +
  theme(plot.title = element_text(hjust = 0.5))
## Store 35 has the most terminated voluntary employees with 207 terminated voluntary employees.

### Conclusion: Most of the employees terminated voluntary in the year of 2008 and most of the reason
###             they terminated voluntary is because of retirement while others is because of resignation. 
###             The average age for terminated voluntary is 53 years old with the average of 11 years 
###             length of service in this company. Besides, most of the voluntary terminated employees 
###             are from Meats department and Store 35 which business unit is Stores.

########################################     END OF QUESTION 1     ########################################

# Question 2: What are the reason employees leave the company involuntary?

# Analysis 2.1: How many employees terminate involuntary?
terminateInvoluntary<-empData[(empData$TermType_desc=="Involuntary"),]
totalterminateInvoluntary=nrow(terminateInvoluntary)
totalterminateInvoluntary
## 215 employees terminate involuntary.

# Analysis 2.2: Find the rate of terminate involuntary.
TotalEmployeesinTerminatedStatus<-nrow(empData[empData$Status=="TERMINATED",])
TotalEmployeesinTerminatedStatus
Terminate_Involuntary_RATE=round((totalterminateInvoluntary/TotalEmployeesinTerminatedStatus)*100,digits=2)
Terminate_Involuntary_RATE
## Total employees in terminated status are 1485.
## Terminate involuntary rate is 14.48%.

# Analysis 2.3: Relationship between termination reason and termination type which is involuntary
involuntary<-empData$TermType_desc=="Involuntary"
involuntaryreason<-empData$TermReason_desc[involuntary]
nlevels(factor(involuntaryreason))
levels(factor(involuntaryreason))
## Employees terminate involuntary is because of layoff.
## This means that all of the employees who terminated involuntary is due to layoff.

# Analysis 2.4: Find the rate of layoff of this company.
TotalEmployees=nrow(empData)
TotalEmployees
LayoffRATE=round((totalterminateInvoluntary/TotalEmployees)*100,digits = 2)
LayoffRATE
## Total employees are 6284.
## Layoff rate of this company is 3.42%.

# Analysis 2.5: Relationship between age and the terminated involuntary employees
Age_of_Terminated_involuntary<-terminateInvoluntary$Age
Gender_terminatedinvoluntary <- data.frame(GenderFull = terminateInvoluntary$GenderFull)
Age_df<-data.frame(Age_of_Terminated_involuntary)
round(summary(Age_of_Terminated_involuntary))
involuntary_age_counts <- table(Age_of_Terminated_involuntary)
most_involuntary_age_count <- max(involuntary_age_counts)
most_involuntary_age <- names(involuntary_age_counts)[involuntary_age_counts == most_involuntary_age_count]
most_involuntary_age
View(involuntary_age_counts)
  
Age_Gender_terminatedinvoluntary <- subset(terminateInvoluntary, select = c(Age, GenderFull))
Age_Gender_counts <- table(Age_Gender_terminatedinvoluntary)
Age_Gender_counts
# Convert the table to a data frame
Age_Gender_df <- as.data.frame(Age_Gender_counts)
# Create the bar chart
ggplot(Age_Gender_df, aes(x = Age, y = Freq, fill = GenderFull)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(x = "Age", y = "Count", fill = "Gender") +
  ggtitle("Number of Terminated Involuntary Employees by Age and Gender") +
  theme(plot.title = element_text(hjust = 0.5, vjust = 0.5)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
   
## Highest age of terminated involuntary employees are 64 years old.
## Lowest age of terminated involuntary employees are 20 years old.
## Average age of terminated involuntary employees are 41 years old.
## Most of the layoff employees are 64 years old

# Analysis 2.6: Relationship between length of service and the terminated involuntary employees
Length_of_Service_terminatedinvoluntary<-terminateInvoluntary$LengthOfService
round(summary(Length_of_Service_terminatedinvoluntary))
## Highest length of service of terminated involuntary employees are 25 years
## Lowest length of service of terminated involuntary employees are 1 years
## Average length of service of terminated involuntary employees are 12 years.

# Create a data frame with the counts for each length of service
counts <- data.frame(LengthOfService = as.factor(Length_of_Service_terminatedinvoluntary))
counts <- data.frame(table(counts))

# Create the histogram with ggplot2 and geom_bar
ggplot(counts, aes(x = LengthOfService, y = Freq)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  xlab("Length of Service (years)") +
  ylab("Frequency") +
  ggtitle("Distribution of Length of Service for Terminated Involuntary Employees") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0, hjust = 1, vjust = 0.5))+
  geom_text(aes(label = Freq), position = position_dodge(width = 0.9), vjust = -0.5)+
  theme(plot.title = element_text(hjust = 0.5, vjust = 0.5)) 

## Most of the layoffs employees have the length of service of 2 years and 7 years.

# Analysis 2.7: Relationship between department and the terminated involuntary employees
dept_terminatedinvoluntary<-terminateInvoluntary$DepartmentName
involuntary_dept_counts <- table(dept_terminatedinvoluntary)
most_involuntary_dept_count <- max(involuntary_dept_counts)
most_involuntary_dept <- names(involuntary_dept_counts)[involuntary_dept_counts == most_involuntary_dept_count]
most_involuntary_dept
View(involuntary_dept_counts)


# Create a data frame with the department names and counts
dept_counts <- data.frame(Department = names(involuntary_dept_counts),
                          Count = as.numeric(involuntary_dept_counts))

# Create the bar chart with ggplot2 and geom_bar
ggplot(dept_counts, aes(x = Department, y = Count, fill = Department)) +
  geom_bar(stat = "identity") +
  xlab("Department") +
  ylab("Number of Terminated Involuntary Employees") +
  ggtitle("Terminated Involuntary Employees by Department") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
  geom_text(aes(label = Count), position = position_dodge(width = 0.9), vjust = -0.5)+
  theme(plot.title = element_text(hjust = 0.5, vjust = 0.5)) +
  scale_fill_manual(values = rainbow(length(dept_counts$Department)))

## The department that has the most terminated involuntary employees is Customer Service department.

# Analysis 2.8: Relationship between the year and the terminated involuntary employees
year_terminatedinvoluntary<-terminateInvoluntary$StatusYear
year_involuntary_counts <- table(year_terminatedinvoluntary)
most_year_involuntary_count <- max(year_involuntary_counts)
most_year_involuntary <- names(year_involuntary_counts)[year_involuntary_counts == most_year_involuntary_count]
most_year_involuntary

# Create a data frame with the years and counts
year_counts <- data.frame(Year = names(year_involuntary_counts),
                          Count = as.numeric(year_involuntary_counts))

# Create the bar chart with ggplot2 and geom_bar
ggplot(year_counts, aes(x = Year, y = Count, fill = Year)) +
  geom_bar(stat = "identity") +
  xlab("Year") +
  ylab("Number of Terminated Involuntary Employees") +
  ggtitle("Terminated Involuntary Employees by Year") +
  theme_minimal() +
  geom_text(aes(label = Count), position = position_dodge(width = 0.9), vjust = -0.5)+
  theme(plot.title = element_text(hjust = 0.5, vjust = 0.5)) +
  scale_fill_manual(values = rainbow(length(year_counts$Year)))

## Layoff happened in the year 2014 and 2015.
## The year that has the most terminated voluntary employees is in year 2014 which have 142 employees 
## terminated involuntary.

# Analysis 2.9: Relationship between business unit and the terminated involuntary employees
BusinessUnit_terminatedinvoluntary<-terminateInvoluntary$BusinessUnit
BusinessUnit_involuntary_counts <- table(BusinessUnit_terminatedinvoluntary)
most_BusinessUnit_involuntary_count <- max(BusinessUnit_involuntary_counts)
most_BusinessUnit_involuntary <- names(BusinessUnit_involuntary_counts)[BusinessUnit_involuntary_counts == most_BusinessUnit_involuntary_count]
most_BusinessUnit_involuntary

# Create a data frame with the business units and counts
bu_counts <- data.frame(BusinessUnit = names(BusinessUnit_involuntary_counts),
                        Count = as.numeric(BusinessUnit_involuntary_counts))

# Create the bar chart with ggplot2 and geom_bar
ggplot(bu_counts, aes(x = BusinessUnit, y = Count, fill = BusinessUnit)) +
  geom_bar(stat = "identity") +
  xlab("Business Unit") +
  ylab("Number of Terminated Involuntary Employees") +
  ggtitle("Terminated Involuntary Employees by Business Unit") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0, hjust = 1, vjust = 0.5)) +
  geom_text(aes(label = Count), position = position_dodge(width = 0.9), vjust = -0.5)+
  theme(plot.title = element_text(hjust = 0.5, vjust = 0.5)) +
  scale_fill_manual(values = rainbow(length(bu_counts$BusinessUnit)))

## The one and only business unit has terminated involuntary employees is Stores.

# Analysis 2.10: Relationship between store name and the terminated involuntary employees
StoreNo_terminatedinvoluntary<-terminateInvoluntary$StoreNo
StoreNo_layoff_counts <- table(StoreNo_terminatedinvoluntary)
View(StoreNo_layoff_counts)

# Create a data frame with the store names and counts
store_counts <- data.frame(StoreNo = names(StoreNo_layoff_counts),
                           Count = as.numeric(StoreNo_layoff_counts))

# Reorder the store names based on the count in descending order
store_counts$StoreNo <- factor(store_counts$StoreNo,
                               levels = store_counts$StoreNo[order(store_counts$Count, decreasing = TRUE)])

# Create the bar chart with ggplot2 and geom_bar
ggplot(store_counts, aes(x = StoreNo, y = Count, fill = StoreNo)) +
  geom_bar(stat = "identity") +
  xlab("Store Name") +
  ylab("Number of Terminated Involuntary Employees") +
  ggtitle("Terminated Involuntary Employees by Store Name") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0, hjust = 1, vjust = 0.5)) +
  geom_text(aes(label = Count), position = position_dodge(width = 0.9), vjust = -0.5)+
  theme(plot.title = element_text(hjust = 0.5, vjust = 0.5)) +
  scale_fill_manual(values = rainbow(length(store_counts$StoreNo)))

## Store 11 has the most terminated voluntary employees with 39 terminated involuntary employees.

# Analysis 2.11: Relationship between job title, gender and the terminated involuntary employees
JobTitle_Gender_terminatedinvoluntary <- subset(terminateInvoluntary, select = c(JobTitle, GenderFull))
JobTitle_Gender_counts <- table(JobTitle_Gender_terminatedinvoluntary)
JobTitle_Gender_counts
## Cashier have more terminated involuntary employees than others job title and most of them are female.

# Convert the table to a data frame
JobTitle_Gender_df <- as.data.frame(JobTitle_Gender_counts)
# Filter out job titles with count 0
JobTitle_Gender_filtered <- JobTitle_Gender_df[JobTitle_Gender_df$Freq > 0, ]
# Create the grouped bar chart
ggplot(JobTitle_Gender_filtered, aes(x = JobTitle, y = Freq, fill = GenderFull)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Job Title", y = "Count", fill = "Gender") +
  ggtitle("Number of Terminated Involuntary Employees by Job Title and Gender") +
  geom_text(aes(label = Freq), position = position_dodge(width = 0.9), vjust = -0.5)+
  theme(plot.title = element_text(hjust = 0.5, vjust = 0.5)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


### Conclusion: Most of the employees terminated involuntary in the year of 2014 and all of the reasons
###             are layoff. The average age for terminated involuntary is 41 years old with the average 
###             of 12 years length of service in this company. Besides, most of the involuntary terminated
###             employees are from the job title of Cashier which is in the Customer Service department 
###             and on Store 11 which business unit is Stores.

########################################     END OF QUESTION 2     ########################################

# Question 3: Which year has the highest retirement rate?

# Analysis 3.1: Find the rate of retirement of this company.
TotalEmployees=nrow(empData)
TotalEmployees
RetiremenRATE=round((Total_Retire_voluntary/TotalEmployees)*100,digits = 2)
RetiremenRATE
## Total employees are 6284.
## Retirement rate of this company is 14.08%.


# Analysis 3.2: Relationship between the year and the retired employees
year_retired<-Retire_voluntary$StatusYear
year_retired_counts <- table(year_retired)
most_year_retired_count <- max(year_retired_counts)
most_year_retired <- names(year_retired_counts)[year_retired_counts == most_year_retired_count]
most_year_retired
least_year_retired_count <- min(year_retired_counts)
least_year_retired <- names(year_retired_counts)[year_retired_counts == least_year_retired_count]
least_year_retired
View(year_retired_counts)

year_retired_df <- as.data.frame(year_retired_counts)
year_retired_df$StatusYear <- as.integer(as.character(year_retired_df$year_retired))

# Create a bar chart
ggplot(year_retired_df, aes(x = StatusYear, y = Freq)) +
  geom_col(fill="#569DAA")+
  geom_text(aes(label = Freq), vjust = -0.5) +
  labs(x = "Year", y = "Number of Retired Employees",
       title = "Number of Retired Employees by Year") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_continuous(breaks = unique(year_retired_df$StatusYear))
## The year that has the most retired employees is in year 2008 which have 138 retired employees 
## while the least is in year 2011 which have 41 retired employees.

# Analysis 3.3: Relationship between the year 2008 and the retirement rate
retiredEmployees_2008<-Retire_voluntary$StatusYear=="2008"
Total_retiredEmployees_2008<-sum(retiredEmployees_2008)
retirementRATE_2008<-round((Total_retiredEmployees_2008/Total_Retire_voluntary)*100,digits=2)
retirementRATE_2008
## 138 employees retired in the year 2008.
## Retirement rate in the year 2008 is 15.59%.

# Analysis 3.4: Relationship between age and the retired employees in 2008.
retiredEmp2008<-empData[(empData$TermReason_desc=="Retirement")&(empData$StatusYear=="2008"),]
retiredEmp2008_age<-retiredEmp2008$Age
retiredEmp2008_age
round(summary(retiredEmp2008_age))

# Calculate the average age of retired employees by year
average_age_by_year <- aggregate(Age ~ StatusYear, data = Retire_voluntary, FUN = mean)

# Create the line chart with data labels
ggplot(average_age_by_year, aes(x = StatusYear, y = Age,color=StatusYear)) +
  geom_line() +
  geom_point()+
  geom_text(aes(label = round(Age)), vjust = -1.5) +
  labs(x = "Year", y = "Average Age", title = "Average Age of Retired Employees by Year") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))+
  scale_x_continuous(breaks = unique(year_retired_df$StatusYear))
## Lowest age of retired employees in 2008 is 60 years old.
## Highest age of retired employees in 2008 is 65 years old.
## Average age of retired employees in 2008 is 63 years old.

# Analysis 3.5: Relationship between length of service and the retired employees in 2008.
LOS_retired2008<-retiredEmp2008$LengthOfService
LOSretired2008_counts <- table(LOS_retired2008)
most_LOSretired2008_count <- max(LOSretired2008_counts)
most_LOSretired2008_retired <- names(LOSretired2008_counts)[LOSretired2008_counts == most_LOSretired2008_count]
most_LOSretired2008_retired

# Convert table to data frame
LOSretired_data <- data.frame(LengthOfService = as.factor(names(LOSretired2008_counts)),
                              Count = as.numeric(LOSretired2008_counts))

# Create the bar chart with labels and colors
ggplot(LOSretired_data, aes(x = LengthOfService, y = Count, fill = LengthOfService)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = Count), vjust = -0.5) +
  labs(x = "Length of Service", y = "Count", title = "Length of Service of Retired Employees in 2008") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_fill_brewer(palette = "Set3") +
  coord_flip()

## The length of service of most of the retired employees in 2008 is 13 years which have 73 retired employees.

# Analysis 3.6: Relationship between department and the retired employees in 2008.
dept_of_retired2008<-retiredEmp2008$DepartmentName
DeptRetired2008_counts<-table(dept_of_retired2008)
most_DeptRetired2008_count <- max(DeptRetired2008_counts)
most_DeptRetired2008_retired <- names(DeptRetired2008_counts)[DeptRetired2008_counts == most_DeptRetired2008_count]
most_DeptRetired2008_retired

# Create the chart for department of retired employees in 2008
ggplot(data = data.frame(DeptRetired2008_counts), 
       aes(x = reorder(names(DeptRetired2008_counts), -DeptRetired2008_counts), 
           y = DeptRetired2008_counts, fill = names(DeptRetired2008_counts))) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = DeptRetired2008_counts), vjust = -0.5) +
  labs(x = "Department", y = "Count", title = "Retired Employees by Department in 2008") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_fill_viridis_d(option = "c", direction = -1) +
  coord_flip()
## The Meats department has the most retired employees in 2008 which have 67 retired employees.

# Analysis 3.7: Relationship between city and the retired employees in 2008.
city_retired2008<-retiredEmp2008$CityName
CityRetired2008_counts<-table(city_retired2008)
most_CityRetired2008_count <- max(CityRetired2008_counts)
most_CityRetired2008_retired <- names(CityRetired2008_counts)[CityRetired2008_counts == most_CityRetired2008_count]
most_CityRetired2008_retired

# Create the chart for city of retired employees in 2008
ggplot(data = data.frame(CityRetired2008_counts), 
       aes(x = reorder(names(CityRetired2008_counts), -CityRetired2008_counts), 
           y = CityRetired2008_counts, fill = names(CityRetired2008_counts))) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = CityRetired2008_counts), vjust = -0.5) +
  labs(x = "City", y = "Count", title = "Retired Employees by City in 2008") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_fill_viridis_d(option = "B", direction = -1) +
  coord_flip()
## Vancouver has the most retired employees in 2008 with the total of 28 retired employees.

# Analysis 3.8: Relationship between job title and the retired employees in 2008.
jobtitle_retired2008<-retiredEmp2008$JobTitle
JobtitleRetired2008_counts<-table(jobtitle_retired2008)
most_JobtitleRetired2008_count <- max(JobtitleRetired2008_counts)
most_JobtitleRetired2008_retired <- names(JobtitleRetired2008_counts)[JobtitleRetired2008_counts == most_JobtitleRetired2008_count]
most_JobtitleRetired2008_retired

# Filter out job titles with 0 terminated employees
nonzero_jobtitles <- JobtitleRetired2008_counts[JobtitleRetired2008_counts > 0]

# Create a data frame for job title and count
jobtitle_counts <- data.frame(JobTitle = names(nonzero_jobtitles), Count = as.numeric(nonzero_jobtitles))

# Create the chart for job title of retired employees in 2008
ggplot(data = jobtitle_counts, aes(x = reorder(JobTitle, -Count), y = Count, fill = JobTitle)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = Count), vjust = -0.5) +
  labs(x = "Job Title", y = "Count", title = "Retired Employees by Job Title in 2008") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_fill_brewer(palette = "Set3") +
  coord_flip()
## Most of the retired employees in 2008 are in the position of meat cutter.

# Analysis 3.9: Relationship between gender and the retired employees in 2008.
gender_retired2008<-retiredEmp2008$GenderFull
GenderRetired2008_counts<-table(gender_retired2008)
most_GenderRetired2008_count <- max(GenderRetired2008_counts)
most_GenderRetired2008_retired <- names(GenderRetired2008_counts)[GenderRetired2008_counts == most_GenderRetired2008_count]
most_GenderRetired2008_retired

# Create the chart for gender of retired employees in 2008
ggplot(data = data.frame(GenderRetired2008_counts), 
       aes(x = reorder(names(GenderRetired2008_counts), -GenderRetired2008_counts),
           y = GenderRetired2008_counts, fill = names(GenderRetired2008_counts))) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = GenderRetired2008_counts), vjust = -0.5) +
  labs(x = "Gender", y = "Count", title = "Retired Employees by Gender in 2008") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_fill_brewer(palette = "Set2") +
  coord_flip()
## Most of the female employees retired in 2008.

# Analysis 3.10: Age of retirement employees and year between gender

# Create data frame for count of retired employees by age, gender, and year
retired_count <- data.frame(
  Age = Retire_voluntary$Age,
  Gender = Retire_voluntary$GenderFull,
  Year = Retire_voluntary$StatusYear
)

# Aggregate the data by age, gender, and year
retired_count <- retired_count %>%
  group_by(Age, Gender, Year) %>%
  summarise(Count = n())

# Convert Age column to numeric
retired_count$Age <- as.numeric(as.character(retired_count$Age))

# Create a line chart
line_chart <- ggplot(retired_count, aes(x = Year, y = Age, color = Gender, group = Gender)) +
  geom_line() +
  geom_point() +
  labs(title = "Age of Retired Employees by Year", x = "Year", y = "Age") +
  scale_color_manual(values = c("#FEA1A1", "steelblue")) +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_continuous(breaks = unique(retired_count$Year))

# Display the line chart
line_chart

# Analysis 3.11: Average age of all of the retired employees.
Age_of_Retire_voluntary<-Retire_voluntary$Age
round(summary(Age_of_Retire_voluntary))
AVG_ageRetired<-round(mean(Age_of_Retire_voluntary))
AVG_ageRetired

# Convert "StatusYear" to factor
Retire_voluntary$StatusYear <- factor(Retire_voluntary$StatusYear)

# Calculate the average age for each year
average_age_by_year <- aggregate(Age ~ StatusYear, data = Retire_voluntary, FUN = mean)

# Create the chart
ggplot(average_age_by_year, aes(x = StatusYear, y = Age, fill = StatusYear)) +
  geom_col() +
  geom_text(aes(label = round(Age)), vjust = -0.5) +
  labs(x = "Year", y = "Average Age", title = "Average Age of Retired Employees by Year") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_fill_brewer(palette = "Paired")
## Highest age of retired employees are 65 years old.
## Lowest age of retired employees are 60 years old.
## Average age of terminate voluntary with the reason of retirement is 63 years old.

# Analysis 3.12: Prediction of retirement rate in next 3 years (2016,2017,2018) based on average age
## Average age of retirement: 63
empData$AgeIn2016<-NA
empData$AgeIn2017<-NA
empData$AgeIn2018<-NA
for (i in 1:nrow(empData)) {
    empData$AgeIn2016[i] <- empData$Age[i] + 1
    empData$AgeIn2017[i] <- empData$Age[i] + 2
    empData$AgeIn2018[i] <- empData$Age[i] + 3
} 
View(empData)

Prediction_retired2016=0
Prediction_retired2017=0
Prediction_retired2018=0

for(i in 1:nrow(empData)){
  if( empData$AgeIn2016[i] >= AVG_ageRetired)
  {
    Prediction_retired2016 = Prediction_retired2016 + 1
  }else if(empData$AgeIn2017[i] >= AVG_ageRetired)
  {
    Prediction_retired2017 = Prediction_retired2017 + 1 
  }else if(empData$AgeIn2018[i] >= AVG_ageRetired)
  {
    Prediction_retired2018 = Prediction_retired2018 + 1 
  }
}

#nrow(empData[(empData$AgeIn2018>=AVG_ageRetired),])

Prediction_retired2016
Prediction_retired2017
Prediction_retired2018

next3years <- c("2016", "2017", "2018")
PredictionRetired <- c(Prediction_retired2016, Prediction_retired2017, Prediction_retired2018)
bar <- barplot(as.numeric(PredictionRetired), xlab = "Year", ylab = "Retired employees", col = "green",
               main = "Prediction of retire employees")
axis(1, at = bar, labels = next3years)
text(x = bar, y = PredictionRetired, labels = PredictionRetired, pos = 1)
## Based on the bar chart can see that most of the employees(915 employees) may be retire in the year 2016.

########################################     END OF QUESTION 3     ########################################

# Question 4: What is the reason of resignation?

# Analysis 4.1: Relationship between age and resignation.
Age_ResignEmp<-Resign_voluntary$Age
round(summary(Age_ResignEmp))

# Create a histogram of age for resigned employees
ggplot(Resign_voluntary, aes(x = Age)) +
  geom_histogram(fill = "#FFA559", color = "white", bins = 10) +
  labs(x = "Age", y = "Count", title = "Age Distribution of Resigned Employees") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))
## Lowest age of resigned employees is 19 years old
## Highest age of resigned employees is 63 years old.
## Average age of resigned employees is 30 years old.

# Analysis 4.2: Relationship between year, average age and resignation.
years_resign_employees<-Resign_voluntary$StatusYear
Total_resignedEmployees<-table(years_resign_employees)
View(Total_resignedEmployees)

# Calculate the average age for each year
avg_age <- aggregate(Age ~ years_resign_employees, data = Resign_voluntary, FUN = mean)

# Convert the result to a data frame
resigned_data <- data.frame(Year = as.numeric(names(Total_resignedEmployees)),
                            Count = as.numeric(Total_resignedEmployees),
                            Average_Age = round(avg_age$Age, ))
resigned_data

# Create the combination chart
ggplot(resigned_data, aes(x = Year)) +
  geom_bar(aes(y = Count), stat = "identity", fill = "#B3C99C",alpha=0.7) +
  geom_line(aes(y = Average_Age), color = "#569DAA", size = 1.5) +
  geom_text(aes(y = Count, label = Count), vjust = -0.5, size = 3) +
  geom_text(aes(y = Average_Age, label = round(Average_Age, )), vjust = -0.5, size = 3, color = "#569DAA") +
  geom_point(aes(y = Average_Age), color = "#569DAA", size = 3) +
  labs(x = "Year", y = "Count / Average Age", title = "Number of Resigned Employees and Average Age by Year") +
  scale_x_continuous(breaks = min(resigned_data$Year):max(resigned_data$Year)) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

## Most of the employees resigned in the year of 2012 with the average of 29.

# Analysis 4.3: Relationship between department and resignation.
dept_resignEmployees<-Resign_voluntary$DepartmentName
DeptResign_counts<-table(dept_resignEmployees)
most_DeptResign_count <- max(DeptResign_counts)
most_DeptResign_retired <- names(DeptResign_counts)[DeptResign_counts == most_DeptResign_count]
most_DeptResign_retired
## Most of the employees resigned in the year of 2012.
## Customer Service department has the most resigned employees which have 179 retired employees.

chartData_dept_resign <- empData[empData$TermReason_desc %in% c("Resignation"), ]
ggplot(chartData_dept_resign, aes(x = DepartmentName)) +
  geom_bar(colour = "orange", fill = "orange") +
  geom_text(stat = "count", aes(label = stat(count)), vjust = -0.5)+
  labs(x = "Department", y = "Count", title = "Number of Resignations by Department")+
  theme(plot.title = element_text(hjust = 0.5, vjust = 0.5))

# Analysis 4.4: Relationship between business unit and the resignation employees
BusinessUnit_resign<-Resign_voluntary$BusinessUnit
BusinessUnit_resign_counts <- table(BusinessUnit_resign)
View(BusinessUnit_resign_counts)
most_BusinessUnit_resign_count <- max(BusinessUnit_resign_counts)
most_BusinessUnit_resign <- names(BusinessUnit_resign_counts)[BusinessUnit_resign_counts == most_BusinessUnit_resign_count]
most_BusinessUnit_resign

# Create a data frame with business unit and count of resignations
business_unit_data <- data.frame(BusinessUnit = names(BusinessUnit_resign_counts),
                                 Count = as.numeric(BusinessUnit_resign_counts))

# Sort the data frame in descending order of resignation counts
business_unit_data <- business_unit_data[order(-business_unit_data$Count), ]

# Create the bar plot with labels
ggplot(business_unit_data, aes(x = BusinessUnit, y = Count, fill = BusinessUnit)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = Count), vjust = -0.5, size = 3) +
  labs(x = "Business Unit", y = "Count", title = "Number of Resigned Employees by Business Unit") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))
## The Business Unit that has the most resign employees is Stores (384 employees) and only 1 resigned employee is from Headoffice.


# Analysis 4.5: Relationship between city and resignation.
city_resignEmployees<-Resign_voluntary$CityName
cityResign_counts<-table(city_resignEmployees)
most_cityResign_count <- max(cityResign_counts)
most_cityResign_retired <- names(cityResign_counts)[cityResign_counts == most_cityResign_count]
most_cityResign_retired

# Create a data frame with city and count of resignations
city_resign_data <- data.frame(City = names(cityResign_counts),
                               Count = as.numeric(cityResign_counts))

# Sort the data frame by count in descending order
city_resign_data <- city_resign_data[order(-city_resign_data$Count), ]

# Create the bar plot with labels
ggplot(city_resign_data, aes(x = City, y = Count, fill = City)) +
  geom_bar(stat = "identity",position="dodge") +
  geom_text(aes(label = Count), vjust = -0.5, size = 3) +
  labs(x = "City", y = "Count", fill="City")+
  ggtitle("Number of Resigned Employees by City") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(plot.title = element_text(hjust = 0.5))+
  scale_fill_viridis_d(option = "A", direction = -1) 

## Vancouver is the city that have more resigned employees compared to others city with the total of 71
## resigned employees.

# Analysis 4.6: Relationship between job title, gender and the resignation employees
JobTitle_Gender_resignvoluntary <- subset(Resign_voluntary, select = c(JobTitle, GenderFull))
JobTitle_Gender_resignvoluntary_counts <- table(JobTitle_Gender_resignvoluntary)
JobTitle_Gender_resignvoluntary_counts
## Cashier have more resignation employees than others job title and most of them are male.

chartData_resignation <- empData[empData$TermReason_desc %in% c("Resignation"), ]
ggplot(chartData_resignation, aes(x = JobTitle, fill = GenderFull)) +
  geom_bar(position = "dodge") +
  geom_text(stat="count",aes(label=stat(count)),position=position_dodge(width=0.9),vjust=0.2)+
  facet_wrap(~ DepartmentName) +
  labs(x = "Job Title", y = "Count") +
  ggtitle("The number of resignation based on job title")+
  coord_flip() +
  theme(plot.title = element_text(hjust = 0.5, vjust = 0.5))

# Analysis 4.7: Relationship between year and numbers of resigned cashier 
numbersofcashier_resignvoluntary <- subset(Resign_voluntary,JobTitle=="Cashier", select = c(StatusYear,GenderFull))
numbersofcashier_resignvoluntary_counts <- table(numbersofcashier_resignvoluntary)
numbersofcashier_resignvoluntary_counts

chartData_cashier_resign <- Resign_voluntary[Resign_voluntary$JobTitle %in% c("Cashier"), ]
ggplot(chartData_cashier_resign, aes(x = StatusYear, fill = GenderFull)) +
  geom_bar(position = "dodge") +
  geom_text(stat = "count", aes(label = stat(count)), position = position_dodge(width = 0.9), vjust = -0.5) +
  labs(x = "Year", y = "Count", title = "Number of Resigned Cashiers by Years", fill = "Gender") +
  theme(plot.title = element_text(hjust = 0.5, vjust = 0.5)) +
  scale_x_continuous(breaks = unique(terminateVoluntaryReason_chartData$StatusYear), 
                     labels = unique(terminateVoluntaryReason_chartData$StatusYear))

# Analysis 4.8: Relationship between length of service and numbers of resigned cashier 
los_resigncashier <- subset(Resign_voluntary,JobTitle=="Cashier", select = c(LengthOfService,GenderFull))
los_resigncashier_counts <- table(los_resigncashier)
los_resigncashier_counts

ggplot(chartData_cashier_resign, aes(x = LengthOfService, fill = GenderFull)) +
  geom_bar(position = "dodge")+
  geom_text(stat="count",aes(label=stat(count)),position=position_dodge(width=0.9),vjust=-0.5)+
  labs(x = "Length of Service", y = "Count", title = "Number of Resigned Cashiers by Length of Service",fill="Gender")+
  theme(plot.title = element_text(hjust = 0.5, vjust = 0.5))+
  scale_x_continuous(breaks = unique(terminateVoluntaryReason_chartData$LengthOfService), 
                     labels = unique(terminateVoluntaryReason_chartData$LengthOfService))

# Analysis 4.9: Relationship between store number and numbers of resigned cashier
storeNo_resigncashier <- subset(Resign_voluntary,JobTitle=="Cashier", select = c(StoreNo))
storeNo_resigncashier_counts <- table(storeNo_resigncashier)
storeNo_resigncashier_counts

ggplot(chartData_cashier_resign, aes(x = StoreNo)) +
  geom_bar(position = "dodge",colour="#FEA1A1",fill="#FEA1A1")+
  geom_text(stat="count",aes(label=stat(count)),position=position_dodge(width=0.9),vjust=-0.5)+
  labs(x = "Store Number", y = "Count", title = "Number of Resigned Cashiers by Store Number")+
  theme(plot.title = element_text(hjust = 0.5, vjust = 0.5))

########################################     END OF QUESTION 4     ########################################

# Question 5: Which department has the highest termination rate and why?

# Analysis 5.1: Relationship between termination reason and terminate employees.
terminateEMP<-empData[(empData$Status=="TERMINATED"),]
totalterminateEMP=nrow(terminateEMP)
totalterminateEMP
## Total of 1485 terminated employees.

# Analysis 5.2: Number of terminated employees each year.
NoOfTerminatedEmp_eachYear<-terminateEMP$StatusYear
terminateEMPEachYear<-table(NoOfTerminatedEmp_eachYear)
View(terminateEMPEachYear)

# Create data frame for number of terminated employees
terminate_emp_data <- data.frame(
  Year = as.character(names(terminateEMPEachYear)),
  NumTerminatedEmployees = terminateEMPEachYear
)

# Create bar chart
bar_chart <- ggplot(terminate_emp_data, aes(x = Year, y = NumTerminatedEmployees, fill = Year)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = NumTerminatedEmployees), vjust = -1) +
  labs(title = "Number of Terminated Employees by Year", x = "Year", y = "Number of Terminated Employees") +
  theme(plot.title = element_text(hjust = 0.5))

# Display bar chart
bar_chart

## Year 2014 has 253 terminated employees which is the most.
## Year 2013 has 105 terminated employees which is the least.


# Analysis 5.3: Relationship between termination rate in each year.
# Calculate termination rate
terminate_rate <- round(terminateEMPEachYear / sum(terminateEMPEachYear) * 100, digits = 2)
terminate_rate

# Create data frame
terminate_data <- data.frame(
  Year = as.character(names(terminateEMPEachYear)),
  TerminationRate = terminate_rate
)
# Create line chart
line_chart <- ggplot(terminate_data, aes(x = Year, y = TerminationRate, group = 1)) +
  geom_line() +
  geom_point() +
  geom_text(aes(label = TerminationRate), vjust = -1) +  # Add labels
  labs(title = "Termination Rate by Year", x = "Year", y = "Termination Rate (%)") +
  theme(plot.title = element_text(hjust = 0.5))
# Display line chart
line_chart

combined_chart <- line_chart +
  geom_col(data = terminate_emp_data, aes(x = Year, y = NumTerminatedEmployees, fill = Year), alpha = 0.5) +
  labs(title = "Number of terminated employees and termination rate by year",x="Year",y="Number of terminated employees")+
  theme(legend.position = "bottom")

# Display combined chart
combined_chart

## Year 2014 has the highest terminate rate which is 17.04%
## Year 2013 has the highest terminate rate which is 7.07%

# Analysis 5.4: Number of terminated employees in each city.
# Calculate the number of terminated employees in each city
terminated_counts <- empData %>%
  filter(Status == "TERMINATED") %>%
  group_by(CityName) %>%
  summarize(Terminated_Count = n())

# Create the bar plot
ggplot(terminated_counts, aes(x = reorder(CityName,-Terminated_Count), y = Terminated_Count, fill = CityName)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = Terminated_Count), vjust = -0.5) +
  labs(x = "City", y = "Terminated Count", fill = "City") +
  ggtitle("Number of Terminated Employees in Each City") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_viridis_d(option = "A", direction = -1)
## Vancouver city has the highest terminated employees.

# Analysis 5.5: Relationship between termination rate of each department in Vancouver.
# Filter the data for terminated employees in Vancouver
vancouver_data <- empData %>%
  filter(CityName == "Vancouver") %>%
  filter(Status == "TERMINATED")

# Calculate the termination rate of each department in Vancouver
termination_rates <- vancouver_data %>%
  group_by(DepartmentName) %>%
  summarize(Termination_Rate = n() / nrow(vancouver_data))

# Sort the departments by termination rate in descending order
termination_rates <- termination_rates %>%
  arrange(desc(Termination_Rate))

# Round the termination rates to two decimal places
termination_rates$Termination_Rate <- round(termination_rates$Termination_Rate, 2)

# Create the bar plot with labels
ggplot(termination_rates, aes(x = reorder(DepartmentName, -Termination_Rate), y = Termination_Rate, fill = DepartmentName)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = Termination_Rate), vjust = -0.5, size = 3) +  # Add labels with rounded termination rates
  labs(x = "Department", y = "Termination Rate", fill = "Department") +
  ggtitle("Termination Rate of Each Department in Vancouver") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_viridis_d(option = "A", direction = -1)

# Analysis 5.6: Relationship between gender of terminated employees and meats department in Vancouver.
# Filter the data for terminated employees in the Meats department in Vancouver
vancouver_meats_data <- empData %>%
  filter(CityName == "Vancouver") %>%
  filter(DepartmentName == "Meats") %>%
  filter(Status == "TERMINATED")

# Calculate the number of terminated employees by gender
gender_counts <- vancouver_meats_data %>%
  group_by(GenderFull) %>%
  summarize(Terminated_Count = n())
View(gender_counts)

# Create the stacked column chart
ggplot(gender_counts, aes(x = GenderFull, y = Terminated_Count, fill = GenderFull)) +
  geom_col() +
  geom_text(aes(label = round(Terminated_Count, 2)), vjust = -0.5, size = 3) +
  labs(x = "Gender", y = "Terminated Count", fill = "Gender") +
  ggtitle("Terminated Employees in the Meats Department (Vancouver) by Gender") +
  theme(plot.title = element_text(hjust = 0.5))


# Analysis 5.7: Relationship of age, gender and terminated employees in meats department in Vancouver.
ggplot(vancouver_meats_data, aes(x = GenderFull, y = Age, color = GenderFull)) +
  geom_jitter(width = 0.2, height = 0, alpha = 0.7) +
  labs(x = "Gender", y = "Age", color = "Gender") +
  ggtitle("Relationship of Age, Gender, and Terminated Employees in Meats Department (Vancouver)") +
  theme(plot.title = element_text(hjust = 0.5))

# Analysis 5.8: Number of terminated employees in meats department in Vancouver each year.
terminated_counts <- vancouver_meats_data %>%
  group_by(StatusYear) %>%
  summarize(Terminated_Count = n())

ggplot(terminated_counts, aes(x = factor(StatusYear), y = Terminated_Count)) +
  geom_bar(stat = "identity", fill = "#F7DB6A") +
  geom_text(aes(label = Terminated_Count), vjust = -0.5, size = 3) +
  labs(x = "Year", y = "Terminated Count") +
  ggtitle("Number of Terminated Employees in Meats Department (Vancouver) Each Year") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_discrete(labels = terminated_counts$StatusYear)
## 2012 has the highest terminated employees.

# Analysis 5.9: Average age of terminated employees in Meats department Vancouver in 2012.
meats_vancouver_terminated2012 <- empData %>%
  filter(CityName == "Vancouver") %>%
  filter(DepartmentName == "Meats") %>%
  filter(Status == "TERMINATED") %>%
  filter(StatusYear == 2012)

average_age_2012 <- round(mean(meats_vancouver_terminated2012$Age),)
average_age_2012

avg_age_by_year <- vancouver_meats_data %>%
  group_by(StatusYear) %>%
  summarize(Average_Age = round(mean(Age),))
View(avg_age_by_year)

# Create the plot
ggplot(data = avg_age_by_year, aes(x = factor(StatusYear), y = Average_Age, label = round(Average_Age, ))) +
  geom_point(color = "#569DAA", size = 3) +
  geom_text(vjust = -0.5, size = 3) +
  labs(x = "Year", y = "Average Age", title = "Average Age of Terminated Employees in Meats Department (Vancouver) by Year") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1))
## Average of terminated employees in Meats department Vancouver in 2012 is 61 years old.

# Analysis 5.10: Number of employees in each job title in meats department.
# Filter the data for the Meats department
meats_data <- empData %>%
  filter(DepartmentName == "Meats")
# Calculate the count of employees in each job title
job_title_counts <- meats_data %>%
  group_by(JobTitle) %>%
  summarize(Employee_Count = n())
# Print the counts
View(job_title_counts)

# Create the bar plot with labels
ggplot(job_title_counts, aes(x = JobTitle, y = Employee_Count)) +
  geom_bar(stat = "identity", fill = "#569DAA") +
  geom_text(aes(label = Employee_Count), vjust = -0.5, size = 3) +
  labs(x = "Job Title", y = "Employee Count", title = "Number of Employees in Each Job Title (Meats Department)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1))

# Analysis 5.11: Termination reason of terminated employees in Meats department in Vancouver in 2012.
# Calculate the count of termination reasons
termination_reason_counts <- meats_vancouver_terminated2012 %>%
  group_by(TermReason_desc) %>%
  summarize(Termination_Count = n())
View(termination_reason_counts)

# Create the bar chart
ggplot(termination_reason_counts, aes(x = TermReason_desc, y = Termination_Count)) +
  geom_bar(stat = "identity", fill = "#D14D72") +
  labs(x = "Termination Reason", y = "Count", 
       title = "Termination Reasons of Terminated Employees in Meats Department (Vancouver, 2012)") +
  geom_text(aes(label = Termination_Count), vjust = -0.5, size = 3) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1))

########################################     END OF QUESTION 5     ########################################



