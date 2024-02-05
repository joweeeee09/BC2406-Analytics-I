library("rstudioapi")            # Load rstudioapi package
setwd(dirname(getSourceEditorContext()$path)) # Automatically sets working directory, assuming R script is same directory as database.csv (not using getwd() as it is buggy).

library(data.table)
library(lubridate)
library(ggplot2)
library(scales)
library(corrplot)
library(rpart)
library(rpart.plot)	
library(caTools)
library(Metrics)
library(randomForest)
library(dplyr)
library(randomForest)
library(ranger)
library(caret)
library(datawizard)
library(car)
library(ehaGoF)
set.seed(2023)

#===============================================================================
# Data Cleaning (DC) & Exploratory Data Analysis (EDA)
#===============================================================================

# Load database.csv into data table and make strings as factor data type
safety_dt<-fread("database.csv",stringsAsFactors = T)
summary(safety_dt)

# DC: Remove redundant columns: Report Number(1), Supplemental Number(2), Operator ID(5), Operator Name(6), Pipeline/Facility Name(7),
#Accident City(13), Accident County(14), Accident State(15)
safety_dt<-safety_dt[,-c(1,2,5,6,7,13,14,15)]

# DC: Change data type of Accident Year from integer to factor
safety_dt$`Accident Year`<-factor(safety_dt$`Accident Year`)
class(safety_dt$`Accident Year`)

# DC: Converting accident date/time to POSIXct format
accident <- subset(safety_dt, !is.na(`Accident Date/Time`), select =`Accident Date/Time`)
accident$`Accident Date/Time` <- mdy_hm(accident$`Accident Date/Time`)
safety_dt$`Accident Date/Time` <- as.POSIXct.Date(safety_dt$`Accident Date/Time`)
safety_dt[!is.na(`Accident Date/Time`), `Accident Date/Time` := accident$`Accident Date/Time`]
class(safety_dt$`Accident Date/Time`)

# EDA: If pipeline location is offshore, the pipeline type column would not be filled.
type_empty<-subset(safety_dt,safety_dt$`Pipeline Type`=='')
location_offshort<-subset(safety_dt,safety_dt$`Pipeline Location`=='OFFSHORE')
identical(type_empty,location_offshort)

# EDA: Plot PDF and histogram for Net Loss
plot(density(safety_dt$`Net Loss (Barrels)`))
hist(safety_dt$`Net Loss (Barrels)`)
hist(log(safety_dt$`Net Loss (Barrels)`)) #right skew
summary(safety_dt$`Net Loss (Barrels)`)
boxplot(safety_dt$`Net Loss (Barrels)`,data=safety_dt)
#Upon exploring the data, we realised that there were errors in the Net Loss (Barrels) column and some NAs in Intentional Release
#The correct values follow this equation: Unintentional Release + Intentional Release - Liquid Recovery = Net Loss

# DC: Clean `Intentional Release (Barrels)` by replacing NA values with correct values. Assuming that net loss, unintentional release and liquid recovery values are correct for these observations
#Take the net loss value when Intentional Release is NA
net_loss <- subset(safety_dt, is.na(`Intentional Release (Barrels)`), select = `Net Loss (Barrels)`)
#Take the Liquid recovery value when Intentional Release is NA
liquid_recovery <- subset(safety_dt, is.na(`Intentional Release (Barrels)`), select = `Liquid Recovery (Barrels)`)
#Take the unintentional release value when Intentional Release is NA
unintentional_release <- subset(safety_dt, is.na(`Intentional Release (Barrels)`), select = `Unintentional Release (Barrels)`)
#Filling in the correct values of Intentional Release for rows with Intentional Release as NA.
safety_dt[is.na(`Intentional Release (Barrels)`), `Intentional Release (Barrels)` := net_loss + liquid_recovery - unintentional_release]
#Rounding values in Intentional Release column to 2dp.
safety_dt$`Intentional Release (Barrels)` <- round(safety_dt$`Intentional Release (Barrels)`, 2)

# DC: To correct the data in Net Loss column, we create a new Net Loss column to prevent overwriting of data in original column
safety_dt[,`Updated Net Loss (Barrels)`:=`Intentional Release (Barrels)`+`Unintentional Release (Barrels)`-`Liquid Recovery (Barrels)`]
#Check if the data has been corrected
result <- safety_dt[round(`Updated Net Loss (Barrels)`, 2) != round(`Unintentional Release (Barrels)` + `Intentional Release (Barrels)` - `Liquid Recovery (Barrels)`, 2)]
#result does not contain any rows, therefore data has been corrected successfully.
dim(result)

# DC: Liquid Name = X if cell is empty as there will only be liquid name if in Liquid Subtype column is ‘Other’ or ‘Other HVL’
safety_dt<-safety_dt[`Liquid Name`=="",`Liquid Name`:="X"]
table(safety_dt$`Liquid Name`)
#Drop unused levels of Liquid Name
safety_dt$`Liquid Name`=droplevels(safety_dt$`Liquid Name`)

#Fill in as X for missing values and Drop unused levels of Liquid Subtype
safety_dt[`Liquid Subtype`=="", `Liquid Subtype`:= "X"]
safety_dt$`Liquid Subtype`=droplevels(safety_dt$`Liquid Subtype`)
levels(safety_dt$`Liquid Subtype`)

# DC: Clean `Pipeline Shutdown` by replacing NA values and "" with "NO"
levels(safety_dt$`Pipeline Shutdown`)
safety_dt[is.na(`Pipeline Shutdown`), `Pipeline Shutdown` := "NO"]
safety_dt[`Pipeline Shutdown`=="",`Pipeline Shutdown` := "NO"]
safety_dt$`Pipeline Shutdown`=droplevels(safety_dt$`Pipeline Shutdown`)

# EDA: Plot the net loss based on whether the pipeline has shut down or not. (ylim can be any value, this graph just shows that when shutdown = Yes, Net Loss a lot higher)
ggplot(safety_dt,aes(x=`Pipeline Shutdown`,y=`Updated Net Loss (Barrels)`)) + ylim(0,1000)+  geom_boxplot()+labs(title='Net Loss (Barrels) across Pipeline Shutdown Status')

#DC: Winsorize All Costs: Replace outliers with 5th and 95th percentile
safety_dt[,newcost:=winsorize(safety_dt$`All Costs`)]
summary(safety_dt$newcost)

# EDA: Plot newcost (winsorized all cost) based on whether pipeline has shut down or not.
ggplot(safety_dt,aes(x=`Pipeline Shutdown`,y=newcost)) +geom_boxplot(aes(fill=`Pipeline Shutdown`))+
  scale_fill_manual(values = c("orange", "lightblue"))+
  labs(title='All Costs across Pipeline Shutdown Status',y="All Costs")+
  theme(
    text = element_text(size = 14),             # Set the base text size
    axis.title = element_text(size = 12),       # Increase axis title font size
    axis.text = element_text(size = 12)       # Increase axis tick label font size
  )
summary(safety_dt)

# DC: Upon inspecting the All Costs column, Values are accurate as it is the total costs of all the different costs added up.
#Thus we assume that if there is no cost in the cell, it is 0.
safety_dt[is.na(`Property Damage Costs`), `Property Damage Costs`:=0]
safety_dt[is.na(`Lost Commodity Costs`), `Lost Commodity Costs`:=0]
safety_dt[is.na(`Public/Private Property Damage Costs`), `Public/Private Property Damage Costs`:=0]
safety_dt[is.na(`Emergency Response Costs`), `Emergency Response Costs`:=0]
safety_dt[is.na(`Environmental Remediation Costs`), `Environmental Remediation Costs`:=0]
safety_dt[is.na(`Other Costs`), `Other Costs`:=0]

# DC: If no Restart Date/Time or Shutdown Date/Time, fill in as NA
safety_dt[`Restart Date/Time` == ""]$`Restart Date/Time` <- NA
safety_dt[`Shutdown Date/Time` == ""]$`Shutdown Date/Time` <- NA
# EDA: There are 49 entries where a shutdown date/time exists but a restart date/time does not
View(safety_dt[!is.na(`Shutdown Date/Time`) & is.na(`Restart Date/Time`)])
# EDA: There are no entries where restart date/time exists but shutdown date/time doesn't
View(safety_dt[is.na(`Shutdown Date/Time`) & !is.na(`Restart Date/Time`)])

# EDA and DC: Converting Shutdown and Restart Date/Time to POSIXct format and making a new column which calculates number of hours shut down 
# Format `Shutdown Date/Time` and `Restart Date/Time` using lubridate
shutdown <- subset(safety_dt, !is.na(`Shutdown Date/Time`), select =`Shutdown Date/Time`)
shutdown$`Shutdown Date/Time` <- mdy_hm(shutdown$`Shutdown Date/Time`)
restart <- subset(safety_dt, !is.na(`Restart Date/Time`), select = `Restart Date/Time`)
restart$`Restart Date/Time` <- mdy_hm(restart$`Restart Date/Time`)

safety_dt$`Shutdown Date/Time` <- as.POSIXct.Date(safety_dt$`Shutdown Date/Time`)
safety_dt[!is.na(`Shutdown Date/Time`), `Shutdown Date/Time` := shutdown$`Shutdown Date/Time`]
safety_dt$`Restart Date/Time` <- as.POSIXct.Date(safety_dt$`Restart Date/Time`)
safety_dt[!is.na(`Restart Date/Time`), `Restart Date/Time` := restart$`Restart Date/Time`]

# Creation of `Shutdown Duration (Hours)` column rounded to 2 d.p, if no shutdown -> shutdown duration = 0
safety_dt[!is.na(`Restart Date/Time`) & !is.na(`Shutdown Date/Time`), `Shutdown Duration (Hours)` := round(as.numeric(interval(`Shutdown Date/Time`, `Restart Date/Time`), "hours"), 2)]
safety_dt[is.na(`Shutdown Duration (Hours)`),`Shutdown Duration (Hours)`:=0]
# Deleting columns for Shutdown and restart date/time 
safety_dt<-safety_dt[,-c(19,20)]
View(safety_dt)

## DC: Convert Fatalities and Injuries and Public evacuation to factor as they are very serious and we care more about wether they occur and less about how many they are + too little occurences to predict exact number with accuracy
# Fatalities
safety_dt[,Fatalities:=ifelse(`All Fatalities`>=1,TRUE,FALSE)]
safety_dt[is.na(`Fatalities`),Fatalities:=FALSE]
class(safety_dt$Fatalities)
# Injuries
safety_dt[,Injuries:=ifelse(`All Injuries`>=1,TRUE,FALSE)]
safety_dt[is.na(`Injuries`),Injuries:=FALSE]
class(safety_dt$Injuries)
# Public Evacuation
safety_dt[,Evacuations:=ifelse(`Public Evacuations`>=1,TRUE,FALSE)]
safety_dt[is.na(`Evacuations`),Evacuations:=FALSE]
class(safety_dt$Evacuation)
# Delete all other columns relating to injuries and accidents and evacuation
safety_dt<-safety_dt[,-c(19,20,21,22,23,24,25,26,27,28,29,30,31)]

# EDA: Plots of shutdown duration (ylim/xlim=1000) - most of the data is less than 250 shutdown hours
boxplot(safety_dt$`Shutdown Duration (Hours)`,ylim=c(0,1000))
ggplot(data=safety_dt, aes(`Shutdown Duration (Hours)`))+geom_density()+xlim(0,1000)
ggplot(data=safety_dt, aes(`Shutdown Duration (Hours)`))+geom_histogram()+xlim(0,1000)

# DC: Rearranging columns:
setcolorder(safety_dt, c("Accident Year", 
                         "Accident Date/Time", "Pipeline Location", "Pipeline Type", 
                         "Liquid Type", "Liquid Subtype", "Liquid Name", "Accident Latitude", "Accident Longitude", 
                         "Cause Category", "Cause Subcategory", "Unintentional Release (Barrels)", 
                         "Intentional Release (Barrels)", "Liquid Recovery (Barrels)", "Updated Net Loss (Barrels)","Net Loss (Barrels)",
                         "Liquid Ignition", "Liquid Explosion", "Pipeline Shutdown", 
                         "Shutdown Duration (Hours)", "Evacuations", "Injuries","Fatalities", "Property Damage Costs", 
                         "Lost Commodity Costs", "Public/Private Property Damage Costs", 
                         "Emergency Response Costs", "Environmental Remediation Costs", 
                         "Other Costs", "All Costs", "newcost")); View(safety_dt)

# EDA: Bar chart which shows the cause category which has the most accidents.
# From the bar chart, we can tell that MATERIAL/WELD/EQUIP FAILURE has the most number of accidents.
ggplot(safety_dt, aes(x = `Cause Category`)) + 
  geom_bar(aes(fill = `Cause Subcategory`), stat = "count") +
  scale_x_discrete(labels = label_wrap(5)) + 
  guides(fill = guide_legend(ncol = 1)) +
  theme(
    text = element_text(size=9),
    legend.text = element_text(size=9),
    legend.key.size = unit(0.1, "cm")  # Adjust this value as needed
  ) +
  ylab("Accidents Reported")+labs(title='Accidents Reported Across Cause Category')

# EDA:However, the subtype of Corrosion: Internal has the most number of accidents
# count_internal = 362 count_external = 230
count_internal=subset(safety_dt,safety_dt$`Cause Subcategory`=="INTERNAL")
count_external=subset(safety_dt,safety_dt$`Cause Subcategory`=="EXTERNAL")
count_pump=subset(safety_dt,safety_dt$`Cause Subcategory`=="PUMP OR PUMP-RELATED EQUIPMENT") #296

# EDA: MATERIAL/WELD/EQUIP FAILURE subcategory plot that shows the number of accidents for each subcategory
# This bar char shows that 
material_data<-subset(safety_dt,safety_dt$`Cause Category`=="MATERIAL/WELD/EQUIP FAILURE")
summary(material_data)
ggplot(data=material_data,aes(x=`Cause Subcategory`,fill=`Cause Subcategory`))+geom_bar(stat="count")+labs(title="Accidents Reported Across Subcategories of MATERIAL/WELD/EQUIP FAILURE")+
  scale_x_discrete(labels = label_wrap(5)) + 
  guides(fill = guide_legend(ncol = 1)) +
  theme(text = element_text(size=10),legend.position = "none") +
  ylab("Accidents Reported")


# EDA: Bar chart which shows the total unintentional release, ordered by cause category 
unintentional_release_summary <- safety_dt[, .(total_unintentional_release = sum(`Unintentional Release (Barrels)`)), by = .(`Cause Category`)]
ggplot(unintentional_release_summary, aes(x = `Cause Category`, y = total_unintentional_release), fill = `Cause Category`) + geom_bar(stat = "identity", position = "dodge") + labs(title = "Total Unintentional Release by Cause Category", x = "Cause Category", y = "Total Unintentional Release") + theme(axis.text.x = element_text(angle = 45, hjust = 1), text = element_text(size=8))

table(safety_dt$`Cause Category`)

# EDA: Bar chart which shows the average unintentional release, ordered by cause category - shows that highest average unintentional release - excavation damage
unintentional_release_avg <- safety_dt[, .(avg_unintentional_release = mean(`Unintentional Release (Barrels)`)), by = .(`Cause Category`)]
ggplot(unintentional_release_avg, aes(x = `Cause Category`, y = avg_unintentional_release), fill = `Cause Category`) +
  geom_bar(stat = "identity", position = "dodge") + 
  labs(title = "Average Unintentional Release by Cause Category", x = "Cause Category", y = "Average Unintentional Release") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1), text = element_text(size=8))

# EDA: Bar chart which shows the NET LOSS UPDATED, ordered by cause category - shows that NET LOSS highest - other outside force damage
netloss_avg <- safety_dt[, .(avg_netloss = mean(`Updated Net Loss (Barrels)`)), by = .(`Cause Category`)]
ggplot(netloss_avg, aes(x = `Cause Category`, y = avg_netloss), fill = `Cause Category`) + 
  geom_bar(stat = "identity", position = "dodge",fill="lightblue") + 
  labs(title = "Average Net Loss by Cause Category", x = "Cause Category", y = "Average Net Loss") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1), text = element_text(size=8))

# EDA: Accidents by year - generally increasing
barplot(table(safety_dt$`Accident Year`),ylab = "Accidents Reported", xlab="Year",col = "lightblue",main='Accident Reports Per Year')

# EDA: Percentage of fatalities
prop.table(table(safety_dt$Fatalities)) # 0.29%
prop.table(table(safety_dt$Injuries)) # 0.43%

# EDA: Plot of incidents distributed by year, month, and hour of the day
# Extract the hour from the AccidentDateTime
class(accident$`Accident Date/Time`)
accident[, Hour:=hour(`Accident Date/Time`)]
numacc_hour <- accident[, .N, by = Hour]
# Rename the columns
setnames(numacc_hour, c("Hour", "Count"))
# Plot
ggplot(data = numacc_hour, aes(x = Hour, y = Count)) + geom_line() + labs(title = "Accidents Per Hour", x = "Hour", y = "No. of Cases")

#EDA: Plot of the different costs  
cost_sums <- safety_dt[, lapply(.SD, sum, na.rm = TRUE), .SDcols = c('Property Damage Costs', 'Lost Commodity Costs', 'Public/Private Property Damage Costs', 'Emergency Response Costs', 'Environmental Remediation Costs', 'Other Costs', 'All Costs')]
cost_sums$`All Costs` <- sum(safety_dt$`All Costs`, na.rm = TRUE)
long_cost_data <- reshape2::melt(cost_sums, id.vars = "All Costs", variable.name = "Cost Type", value.name = "Total")
ggplot(long_cost_data, aes(x = `Cost Type`, y = Total, fill = `Cost Type`)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), text = element_text(size=9), legend.text = element_text(size=10), legend.key.size = unit(0.3, "cm")) +
  labs(title = "Total Amount Based On Specific Cost Category")

#EDA: Environmental Remediation Costs with Liquid Recovery
ggplot(safety_dt[`Environmental Remediation Costs`!=0 &`Liquid Recovery (Barrels)`!=0 ],aes(x=log(`Liquid Recovery (Barrels)`+1,10),y=log(`Environmental Remediation Costs`+1,10)))+geom_point()+
  geom_smooth(method = "lm", se = FALSE, color = "red")+
  labs(title='Relationship Between Environmental Remediation Costs and Liquid Recovered on a Logarithmic Scale',x="Liquid Recovery (Barrels)",y="Environmental Remediation Costs")+
  theme(
    plot.title = element_text(size = 15),  # Adjust main title size
    axis.title.x = element_text(size = 14),               # Adjust X axis title size
    axis.title.y = element_text(size = 14)                # Adjust Y axis title size
  )

#Correlation matrix for numerical variables
numeric_columns <- sapply(safety_dt, is.numeric)
# Calculate correlation matrix for numeric columns only
correlation_matrix <- safety_dt[, cor(.SD,use = "pairwise.complete.obs"), .SDcols = numeric_columns]
# Print the correlation matrix
print(correlation_matrix)
corrplot(correlation_matrix,method = 'color', tl.cex = 0.5, tl.col = "black")


#EDA: All Cost vs Liquid Recovery
ggplot(safety_dt[`All Costs`!=0 & `Liquid Recovery (Barrels)`!=0],aes(x=log(`Liquid Recovery (Barrels)`+1,10),y=log(`All Costs`+1,10)))+geom_point()+
  geom_smooth(method = "lm", se = FALSE, color = "red")+labs(title='Relationship Between All Costs and Liquid Recovered on a Logarithmic Scale',x="Liquid Recovery (Barrels)",y="All Costs")+
  theme(
    plot.title = element_text(size = 15),  # Adjust main title size
    axis.title.x = element_text(size = 14),               # Adjust X axis title size
    axis.title.y = element_text(size = 14)                # Adjust Y axis title size
  )

#EDA: Lost commodity cost vs net loss
ggplot(safety_dt[`Lost Commodity Costs`!=0 & `Updated Net Loss (Barrels)`!=0],aes(x=log(`Updated Net Loss (Barrels)`+1,10),y=log(`Lost Commodity Costs`+1,10)))+geom_point()+
  geom_smooth(method = "lm", se = FALSE, color = "red")+labs(title='Relationship Between Lost Commodity Costs and Net Loss on a Logarithmic Scale',x="Net Loss (Barrels)",y="Lost Commodity Costs")+
  theme(
    plot.title = element_text(size = 15),  # Adjust main title size
    axis.title.x = element_text(size = 14),               # Adjust X axis title size
    axis.title.y = element_text(size = 14)                # Adjust Y axis title size
  )

#===============================================================================
# Models: Linear Regression, Logistic Regression, CART & Random Forest
#===============================================================================
#-------------------------------------------------------------------------------
# Logistic Regression: Fatalities as Y variable
#-------------------------------------------------------------------------------
set.seed(2023)
weight_for_0 <- sum(safety_dt$Fatalities == 1) / nrow(safety_dt)
weight_for_1 <- sum(safety_dt$Fatalities == 0) / nrow(safety_dt)
safety_dt$fatalityweights <- ifelse(safety_dt$Fatalities == 0, weight_for_0, weight_for_1) # Assign weights to each observation based on its class
# train-test split Y variable: Fatalities (factor)
train_fatalities<-sample.split(Y=safety_dt$Fatalities, SplitRatio = 0.7)
trainset_fatalities<-subset(safety_dt,train_fatalities==T)
testset_fatalities<-subset(safety_dt, train_fatalities==F)

#Developing logreg on train data
logreg_fatalities <- glm(Fatalities ~`Cause Category` + `Pipeline Location` + `Pipeline Type` + `Liquid Type` +
                           `Liquid Subtype`+`Shutdown Duration (Hours)`+`Liquid Ignition`+ `Liquid Explosion`+`Evacuations`+
                           `Injuries`+`Updated Net Loss (Barrels)`, weights=fatalityweights,data = trainset_fatalities) 
summary(logreg_fatalities) #significant factors:other outside force damage
prob_fatalities <- predict(logreg_fatalities, type = 'response')
threshold <- 0.5;
y.hat.fatalities <- ifelse(prob_fatalities > threshold, 1, 0)
table(trainset_fatalities$Fatalities, y.hat.fatalities, deparse.level = 2)
mean(y.hat.fatalities == trainset_fatalities$Fatalities) #Overall accuracy on trainset data = 0.9897803

predictprob_fatalities <- predict(logreg_fatalities, newdata = testset_fatalities)
y.hat <- ifelse(predictprob_fatalities > threshold, 1, 0)
table(testset_fatalities$Fatalities, y.hat, deparse.level = 2)
mean(y.hat == testset_fatalities$Fatalities) #Overall accuracy on testset data = 0.9821002

#-------------------------------------------------------------------------------
# CART: Y-variable = Fatalities
#-------------------------------------------------------------------------------
#Using same trainset and testset as the logreg for fatalities above

cart_fatalities<-rpart(Fatalities ~`Cause Category` + `Pipeline Location` + `Pipeline Type` + `Liquid Type` +
                         `Liquid Subtype`+`Shutdown Duration (Hours)`+`Liquid Ignition`+ `Liquid Explosion`+`Evacuations`+
                         `Injuries`+`Updated Net Loss (Barrels)`, weights=fatalityweights, data=trainset_fatalities, 
                       control=rpart.control(minsplit=50,cp=0), method="class")

printcp(cart_fatalities)
plotcp(cart_fatalities)
rpart.plot(cart_fatalities,nn=T,compress=FALSE,faclen = 6)
CVerror.cap <- cart_fatalities$cptable[which.min(cart_fatalities$cptable[,"xerror"]), "xerror"] + cart_fatalities$cptable[which.min(cart_fatalities$cptable[,"xerror"]), "xstd"]
# Find optimal CP region whose CV error is just below CVerror.cap in maximal tree cart1.
i <- 1; j<- 4
while (cart_fatalities$cptable[i,j] > CVerror.cap) {i <- i + 1}
# Geometric mean of 2 identified CP values in optimal region if optimal tree has at least 1 split.
cp.opt = ifelse(i > 1, sqrt(cart_fatalities$cptable[i,1] * cart_fatalities$cptable[i-1,1]), 1)
opt_cartfatalities<-prune(cart_fatalities,cp=cp.opt)
rpart.plot(opt_cartfatalities,nn=T)

table(trainset_fatalities$Fatalities, predict(opt_cartfatalities,newdata = trainset_fatalities,type="class"), deparse.level = 2)
mean(predict(opt_cartfatalities,newdata = trainset_fatalities,type="class") == trainset_fatalities$Fatalities) # accuracy on trainset data = 0.9683189

cart_predfatalities<-predict(opt_cartfatalities,newdata=testset_fatalities,type="class")
table(testset_fatalities$Fatalities, cart_predfatalities, deparse.level = 2)
mean(cart_predfatalities == testset_fatalities$Fatalities) #accuracy on testset data =  0.9642005
print(opt_cartfatalities)
opt_cartfatalities$variable.importance #updated net loss variable importance highest
summary(opt_cartfatalities)

#-------------------------------------------------------------------------------
# Logistic Regression: Injuries as Y variable
#-------------------------------------------------------------------------------
set.seed(2023)
weight_for_0 <- sum(safety_dt$Injuries == 1) / nrow(safety_dt)
weight_for_1 <- sum(safety_dt$Injuries == 0) / nrow(safety_dt)
safety_dt$injuryweights <- ifelse(safety_dt$Injuries == 0, weight_for_0, weight_for_1) # Assign weights to each observation based on its class
#train-test split Y variable: Injuries (factor)
train_injuries<-sample.split(Y=safety_dt$Injuries, SplitRatio = 0.7)
trainset_injuries<-subset(safety_dt,train_injuries==T)
testset_injuries<-subset(safety_dt, train_injuries==F)

#Developing logreg on train data
logreg_injuries <- glm(Injuries ~ `Cause Category` + `Pipeline Location` + `Pipeline Type` + `Liquid Type` +
                         `Liquid Subtype`+`Shutdown Duration (Hours)`+`Liquid Ignition`+ `Liquid Explosion`+`Evacuations`+
                         `Fatalities`+`Updated Net Loss (Barrels)`,weights=injuryweights ,data = trainset_injuries) 
summary(logreg_injuries) #significant factors: incorrect operation and co2 /hvl or other flammable or toxic fluid,gas
prob_injuries <- predict(logreg_injuries, type = 'response')
threshold <- 0.5;
y.hat.injuries <- ifelse(prob_injuries > threshold, 1, 0)
table(trainset_injuries$Injuries, y.hat.injuries, deparse.level = 2)
mean(y.hat.injuries == trainset_injuries$Injuries) #Overall accuracy on trainset data = 0.958589

predictprob_injuries <- predict(logreg_injuries, newdata = testset_injuries)
y.hat <- ifelse(predictprob_injuries > threshold, 1, 0)
table(testset_injuries$Injuries, y.hat, deparse.level = 2)
mean(y.hat == testset_injuries$Injuries) #Overall accuracy on testset data = 0.942789

#-------------------------------------------------------------------------------
# CART: Y-variable = Injuries
#-------------------------------------------------------------------------------
#Using same trainset and testset as the logreg for injuries below
set.seed(2023); cart_injuries<-rpart(Injuries ~ `Cause Category` + `Pipeline Location` + `Pipeline Type` + `Liquid Type` +
                                       `Liquid Subtype`+`Shutdown Duration (Hours)`+`Liquid Ignition`+ `Liquid Explosion`+`Evacuations`+
                                       `Fatalities`+`Updated Net Loss (Barrels)`,weights=injuryweights ,data = trainset_injuries, 
                                     control=rpart.control(minsplit=50,cp=0), method="class")

printcp(cart_injuries)
plotcp(cart_injuries)
rpart.plot(cart_injuries,nn=T,faclen = 6)
CVerror.cap <- cart_injuries$cptable[which.min(cart_injuries$cptable[,"xerror"]), "xerror"] + cart_injuries$cptable[which.min(cart_injuries$cptable[,"xerror"]), "xstd"]
# Find optimal CP region whose CV error is just below CVerror.cap in maximal tree cart1.
i <- 1; j<- 4
while (cart_injuries$cptable[i,j] > CVerror.cap) {i <- i + 1}
# Geometric mean of 2 identified CP values in optimal region if optimal tree has at least 1 split.
cp.opt = ifelse(i > 1, sqrt(cart_injuries$cptable[i,1] * cart_injuries$cptable[i-1,1]), 1)
opt_cartinjuries<-prune(cart_injuries,cp=cp.opt)
rpart.plot(opt_cartinjuries,nn=T)

table(trainset_injuries$Injuries, predict(opt_cartinjuries,newdata = trainset_injuries,type="class"), deparse.level = 2)
mean(predict(opt_cartinjuries,newdata = trainset_injuries,type="class") == trainset_injuries$Injuries) #0.8169734

cart_predinjuries<-predict(opt_cartinjuries,newdata=testset_injuries,type="class")
table(testset_injuries$Injuries, cart_predinjuries, deparse.level = 2)
mean(cart_predinjuries == testset_injuries$Injuries) #accuracy on testset data = 0.7771156
print(opt_cartinjuries)
opt_cartinjuries$variable.importance #Cause cat highest variable importance

#-------------------------------------------------------------------------------
# CART: Y-variable = newcost (Winsorized `All Costs`)
#-------------------------------------------------------------------------------
# Train-test split (70-30) - All Costs
set.seed(2023); train_newcost<-sample.split(Y=safety_dt$newcost, SplitRatio = 0.7)
trainset_newcost<-subset(safety_dt,train_newcost == T)
testset_newcost<-subset(safety_dt, train_newcost == F)

trainset_newcost <- trainset_newcost %>% mutate(newcost_Percentile =ntile(newcost, 100))
formula<-newcost_Percentile~`Cause Category` + `Pipeline Location` + `Pipeline Type` + `Liquid Type` +
  `Liquid Subtype`+`Shutdown Duration (Hours)`+`Liquid Ignition`+ `Liquid Explosion`+`Evacuations`+
  `Injuries`+`Fatalities`+`Updated Net Loss (Barrels)`+`Pipeline Shutdown`

tree_model <- rpart(formula, data = trainset_newcost, method = "anova",control = rpart.control(minsplit=50,cp = 0))
print(tree_model)
printcp(tree_model) #55 splits in maximal tree
plotcp(tree_model) 

CVerror.cap <- tree_model$cptable[which.min(tree_model$cptable[,"xerror"]), "xerror"] + tree_model$cptable[which.min(tree_model$cptable[,"xerror"]), "xstd"]
# Find optimal CP region whose CV error is just below CVerror.cap in maximal tree cart1.
i <- 1; j<- 4
while (tree_model$cptable[i,j] > CVerror.cap) {i <- i + 1}
# Geometric mean of 2 identified CP values in optimal region if optimal tree has at least 1 split.
cp.opt = ifelse(i > 1, sqrt(tree_model$cptable[i,1] * tree_model$cptable[i-1,1]), 1)
# Prune tree
opt_tree_model <- prune(tree_model, cp = cp.opt)

# Print Optimal Tree and Optimal Tree CP
printcp(opt_tree_model)
print(opt_tree_model)
summary(opt_tree_model)
rpart.plot(opt_tree_model, nn = T, main = "Optimal Tree")
opt_tree_model$variable.importance

# RMSE for trainset
rmse(trainset_newcost$newcost_Percentile, predict(opt_tree_model, trainset_newcost)) #RMSE(percentile)=23.26396
converted <- quantile(trainset_newcost$newcost, probs = predict(opt_tree_model, trainset_newcost)/100.0)
rmse(trainset_newcost$newcost, converted) #RMSE(converted): 57263.92
# R-squared for trainset
gofRSq(trainset_newcost$newcost_Percentile, predict(opt_tree_model, trainset_newcost)) #R-squared (percentile) = 0.347
gofRSq(trainset_newcost$newcost, converted) #R-squared (converted) = 0.217

#RMSE for testset
testset_newcost <- testset_newcost %>% mutate(newcost_Percentile = ntile(newcost, 100))
pred_newcost<-predict(opt_tree_model,newdata = testset_newcost)
rmse(testset_newcost$newcost_Percentile, pred_newcost) ##RMSE(percentile) = 22.50679
converted_pred <- quantile(testset_newcost$newcost, probs = pred_newcost/100)
rmse(testset_newcost$newcost,converted_pred) #RMSE (converted) = 69148.81
# R-squared for testset
gofRSq(testset_newcost$newcost_Percentile, pred_newcost) #R-squared (percentile) = 0.399
gofRSq(testset_newcost$newcost,converted_pred) #R-squared (converted) = 0.217

#-------------------------------------------------------------------------------
# Linear regression: Y-variable = newcost (Winsorized `All Costs`)
#-------------------------------------------------------------------------------
set.seed(2023)
linreg_newcost<-lm(formula,data=trainset_newcost)
summary(linreg_newcost)
AIC_linear<-step(linreg_newcost)
summary(AIC_linear) # R-squared for trainset = 0.2734
vif(AIC_linear) # All variables below threshold for multi-collinearity

par(mfrow=c(2,2))
plot(AIC_linear)
par(mfrow=c(1,1))

# RMSE for trainset
predict.newcostP.linear.train <- predict(AIC_linear, newdata = trainset_newcost);
predict.newcostP.linear.train[predict.newcostP.linear.train > 100] <- 100
rmse(trainset_newcost$newcost_Percentile, predict.newcostP.linear.train) # RMSE (percentile) = 24.5157
converted.newcostP.linear.train <- quantile(trainset_newcost$newcost, probs = (predict.newcostP.linear.train/100))
rmse(trainset_newcost$newcost, converted.newcostP.linear.train) #RMSE (converted) = 61339.96
# R-squared for trainset
gofRSq(trainset_newcost$newcost_Percentile, predict.newcostP.linear.train) #R-squared (percentile) = 0.275
gofRSq(trainset_newcost$newcost, converted.newcostP.linear.train) #R-squared = 0.101

# RMSE for testset
predict.newcostP.linear.test <- predict(AIC_linear, newdata = testset_newcost);
predict.newcostP.linear.test[predict.newcostP.linear.test > 100] <- 100
rmse(testset_newcost$newcost_Percentile, predict.newcostP.linear.test) #RMSE (percentile) = 23.6277
converted.newcostP.linear.test <- quantile(testset_newcost$newcost, probs = (predict.newcostP.linear.test/100))
rmse(testset_newcost$newcost, converted.newcostP.linear.test) #RMSE (converted) = 74270.22
# R-squared for testset
gofRSq(testset_newcost$newcost_Percentile, predict.newcostP.linear.test) #R-squared (percentile) = 0.337
gofRSq(testset_newcost$newcost, converted.newcostP.linear.test) #R-squared = 0.097

results_linearnewcost<-data.table(testset_newcost$newcost_Percentile,predict.newcostP)
ggplot(results_linearnewcost,aes(x=V1, y=predict.newcostP))+geom_point()+geom_smooth(method = "lm", se = FALSE, color = "red")+
  labs(y="Predicted Percentile", x="Actual Percentile", title='Actual vs Predicted')