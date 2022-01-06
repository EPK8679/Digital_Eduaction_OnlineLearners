#Libraries for dataset
library(ggplot2)
library(dplyr)
library(formattable)
library(plyr)
library(gridExtra)

#Setting the location path 
getwd()
setwd("Desktop/R_CSV/Interview_Task/")

#Reading the dataset course_meberships
course_Learners <- read.csv("course_memberships.csv",header = TRUE)

#Displaying the information 
head(course_Learners)
summary(course_Learners)
tail(course_Learners)
View(course_Learners)


#Question 1 

#Converting class character to Date format
course_Learners$course_membership_ts <- as.Date(format(as.Date(course_Learners$course_membership_ts, "%d/%m/%Y"), "%m/%d/%y"), format = "%m/%d/%y")
course_Learners

#Filtering the Learners
course_learners_filter <- filter(course_Learners,course_membership_role =="LEARNER")

#Filtering the date 01-07-2018 to 30=06-2019
course_learners_filter_date <- filter(course_learners_filter,course_learners_filter$course_membership_ts >="2018-07-01" & course_learners_filter$course_membership_ts <="2019-06-30")
course_learners_filter_date

#Ordering the dates
course_learners_filter_date <- course_learners_filter_date[order(as.Date(course_learners_filter_date$course_membership_ts, format="%m/%d/%Y")),]
View(course_learners_filter_date)
class(course_learners_filter_date$user_id)

#Removing the duplicate learners
course_learners_filter_date_unique <- course_learners_filter_date[!duplicated(course_learners_filter_date[,c('user_id')]),]
course_learners_filter_date_unique
course_learners_filter_date_unique <- course_learners_filter_date_unique[order(as.Date(course_learners_filter_date_unique$course_membership_ts, format="%m/%d/%Y")),]

#Finding the Frequency of lerners in each month
Learners_frequency <-  as.data.frame(table(format(course_learners_filter_date_unique$course_membership_ts,"%d/%m/%Y")))
Learners_frequency<- Learners_frequency[order(as.Date(Learners_frequency$Var1, format="%d/%m/%Y")),]
class(Learners_frequency$Var1)
Learners_frequency

#Converting the factor class to date
Learners_frequency$Var1 <- as.Date(format(as.Date(Learners_frequency$Var1, "%d/%m/%Y"), "%m/%d/%y"), format = "%m/%d/%y")
class(Learners_frequency$Var1)
Learners_frequency

#converting the date to mm/yyyy
Learners_frequency$Var1 <- format(as.Date(Learners_frequency$Var1),"%Y-%m")

#Adding up the frequnecy in each month
Learners_frequency_mmyy <- ddply(Learners_frequency,"Var1",numcolwise(sum))
Learners_frequency_mmyy

colnames(Learners_frequency_mmyy)

#Renaming the column names
names(Learners_frequency_mmyy)[names(Learners_frequency_mmyy)=="Var1"] <- "Month"
names(Learners_frequency_mmyy)[names(Learners_frequency_mmyy)=="Freq"] <- "Number_of_Learners"
formattable(Learners_frequency_mmyy, 
            align =c("l","c","c","c","c", "c", "c", "c", "r"), 
            list(`Indicator Name` = formatter( "span", style = ~ style(color = "grey",font.weight = "bold")))
) 
summary(Learners_frequency_mmyy)
#Plotiing the graph for Total Participants Enrolled as Learners
plot1 <- ggplot(Learners_frequency_mmyy, aes(x=Month, y=Number_of_Learners)) +
  geom_point(size=1.5, shape=23, color= "blue") +
  ggtitle("Total Participants Enrolled as Learners" )

plot1

#Question 2 

#Reading the course grades dataset
course_grade <- read.csv("course_grades.csv",header = TRUE)
View(course_grade)

#Merging the datasets course learners and course grade 
course_learners_grade <- merge(course_learners_filter,course_grade,by="user_id",stringAsFactors =T)
View(course_learners_grade)

course_learners_grade

#Converting the learners acquired grade timestamp to date class
course_learners_grade$course_grade_ts <- as.Date(format(as.Date(course_learners_grade$course_grade_ts, "%d/%m/%Y"), "%m/%d/%y"), format = "%m/%d/%y")

#Filtering the course completed learners
course_learners_grade_filter <- subset(course_learners_grade,course_passing_state_id == 2)

#Filtering the dates  of learners acquired grades from 01-07-2018 to 30-06-2019
course_learners_grade_filter_date <- subset(course_learners_grade_filter, course_learners_grade_filter$course_grade_ts >="2018-07-01" & course_learners_grade_filter$course_grade_ts <="2019-06-30")
course_learners_grade_filter_date

#Ordering the dates 
course_learners_grade_filter_date <- course_learners_grade_filter_date[order(as.Date(course_learners_grade_filter_date$course_grade_ts, format="%m/%d/%Y")),]
course_learners_grade_filter_date

#Finding the unique completed learners
course_learners_grade_filter_date_unique <- subset(course_learners_grade_filter_date,!duplicated(course_learners_grade_filter_date$user_id))
course_learners_grade_filter_date_unique


#Frequency table each month for completed learners
Learners_grade_frequency <-  as.data.frame(table(format(course_learners_grade_filter_date_unique$course_grade_ts,"%Y-%m-%d")))
Learners_grade_frequency<- Learners_grade_frequency[order(as.Date(Learners_grade_frequency$Var1, format="%Y-%m-%d")),]
Learners_grade_frequency
class(Learners_grade_frequency$Var1)
Learners_grade_frequency$Var1[1]

#Converting the date to mm/yyyy
Learners_grade_frequency$Var1 <- format(as.Date(Learners_grade_frequency$Var1),"%Y-%m")
Learners_grade_frequency

#Summing up the frequency for each month
Learners_frequency_grade_mmyy <- ddply(Learners_grade_frequency,"Var1",numcolwise(sum))
Learners_frequency_grade_mmyy

#Setting the colum names
colnames(Learners_frequency_grade_mmyy)
names(Learners_frequency_grade_mmyy)[names(Learners_frequency_grade_mmyy)=="Var1"] <- "Month"
names(Learners_frequency_grade_mmyy)[names(Learners_frequency_grade_mmyy)=="Freq"] <- "Number_of_Course_Completed_Learners"
formattable(Learners_frequency_grade_mmyy, 
            align =c("l","c","c","c","c", "c", "c", "c", "r"), 
            list(`Indicator Name` = formatter( "span", style = ~ style(color = "grey",font.weight = "bold")))
) 
summary(Learners_frequency_grade_mmyy)

#Ploting the graph for Number_of_Course_Completed_Learners
plot2 <- ggplot(Learners_frequency_grade_mmyy, aes(x=Month, y=Number_of_Course_Completed_Learners)) + 
  geom_point(size=1.5, shape=23, color= "red") + 
  labs(title="Number of Learners Completed Courses", x="Month", y="Number_of_Course_Completed_Learners")
plot2

#Visualising plot 1 and plot 2 
grid.arrange(plot1, plot2, ncol = 2, nrow = 1)



#Merging participants enrolled as learners and the course completed learners

course_learners_grade_graph_merge <- merge(Learners_frequency_mmyy,Learners_frequency_grade_mmyy,by="Month",stringAsFactors =T)
course_learners_grade_graph_merge

formattable(course_learners_grade_graph_merge, 
            align =c("l","c","c","c","c", "c", "c", "c", "r"), 
            list(`Indicator Name` = formatter( "span", style = ~ style(color = "grey",font.weight = "bold")))
) 

colors <- c("Number of Learners" = "red", "Number of Course Completed Learners" = "green")

#Visualising the graph
ggplot(course_learners_grade_graph_merge, aes(Month)) +                    # basic graphical object
  geom_point(aes(y=Number_of_Learners,color="Number of Learners"), size=2) +  # first layer
  geom_point(aes(y=Number_of_Course_Completed_Learners,color="Number of Course Completed Learners"), shape="triangle",size=2) + # second layer
  labs(x="Month",y="Number of Participants", title = "Analysis of Course Particpants in DES",color="Legend")+
  scale_color_manual(values = colors)


#Question 3

#Reading the users dataset
users <- read.csv("users.csv",header = TRUE)
View(users)
course_learners_grade_filter_date_unique

#Merging users and course_learners_grade
users_course_completers <- merge(users,course_learners_grade_filter_date_unique,by="user_id",stringAsFactors =T)
users_course_completers

#Finding the frequency of course completer employment status
users_course_completers_employed <- as.data.frame(table(format(users_course_completers$employment_status)))
users_course_completers_employed <- users_course_completers_employed[order(users_course_completers_employed$Freq),]
users_course_completers_employed

#Finding cumulitative and realtive frequency (%)
users_course_completers_employed <- mutate(users_course_completers_employed,
                                          Cumilative_Frequency= cumsum(users_course_completers_employed$Freq))

users_course_completers_employed <- mutate(users_course_completers_employed, Relative_Frequency= (users_course_completers_employed$Freq/sum(users_course_completers_employed$Freq))*100)

#Changing the column names
names(users_course_completers_employed)[names(users_course_completers_employed)=="Var1"] <- "Employment_Status"
names(users_course_completers_employed)[names(users_course_completers_employed)=="Freq"] <- "Number of Course Completed Learners"
names(users_course_completers_employed)[names(users_course_completers_employed)=="Relative_Frequency"] <- "Relative Frequency(%)"
 print(users_course_completers_employed, row.names = FALSE)
formattable(users_course_completers_employed , row.names=  FALSE,
            align =c("l","c","c","c","c", "c", "c", "c", "r"), 
            list(`Indicator Name` = formatter( "span", style = ~ style(color = "grey",font.weight = "bold")))
)

#plotting the graph for Employent Status of course Completers
pie <- ggplot(users_course_completers_employed, aes(x="", y=`Relative Frequency(%)`, fill=	Employment_Status)) + 
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0) +
  labs(x = NULL, y = NULL, fill = NULL, title = "Employment Status of Course Completers")
pie

