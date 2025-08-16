library(tidyverse)
library(tidyr)
library (skimr)
library (janitor)
library (ggplot2)

Employee_detail1 <- read_csv("Ed.csv")

Employee_detail <- Employee_detail1 %>%
  distinct() %>%
  drop_na()

head(Employee_detail)

View(Employee_detail)
## Attrition vs. Gender 

Gender_Wise <- Employee_detail %>%
  group_by(Gender) %>%
  summarise(total_employee=n(), attritions=sum(Attrition == "TRUE" ), Attrition_rate=attritions/total_employee*100)

View(Gender_Wise)

ggplot(data = Gender_Wise) +
  geom_bar(mapping = aes(x = Gender, y = Attrition_rate , fill = Gender), 
           stat = "identity", 
  ) + geom_text(aes(x = Gender, y = Attrition_rate, 
                    label = paste0(round(Attrition_rate, 2), "%"), 
                    color = Gender), 
                vjust = -0.5, size = 4, fontface = "bold", show.legend = FALSE) +
  labs(title = "Attrition Rate by Gender", 
       y = "Attrition Rate (%)", 
       x = "Gender") +
  theme_minimal() + theme(plot.title =element_text(face = "bold" , size = 16 , colour = "lightblue"),axis.title = element_text(face = "bold",size = 12),axis.text = element_text(face = "bold"))

## Attrition vs. age

Age_wise <- Employee_detail %>%
  group_by(Age) %>%
  summarise(total_employee=n(), attritions=sum(Attrition=="TRUE"), Attrition_rate= attritions/total_employee*100)

View(Age_wise)

ggplot(data = Age_wise)+geom_histogram(mapping = aes(x=Age,y = Attrition_rate),stat = "identity",bins =  40 ,fill = "blue",color= "black") +
  labs(title="Attrition rate per Age",x="Age",y="Attrition Rate(%)")+
  theme_minimal() + 
  theme(plot.title = element_text(face ="bold",size = 16),axis.title = element_text(face = "bold",size = 12))

## Attrition vs. Marital_status 
Marital_wise <- Employee_detail %>%
  group_by(MaritalStatus) %>%
  summarise(total_employee=n(), attritions=sum(Attrition=="TRUE"), Attrition_rate= attritions/total_employee*100)

View(Marital_wise)          

ggplot(Marital_wise, aes(x = reorder(MaritalStatus, Attrition_rate), y = Attrition_rate)) +
  geom_segment(mapping = aes(xend = MaritalStatus, y = 0, yend = Attrition_rate), 
               size = 2, color = "grey") +
  geom_point(size = 6, color = "blue") +
  geom_text(aes(label = paste0(round(Attrition_rate, 1), "%")),
            vjust = -1.0) +
  labs(title = "Attrition Rate by Marital Status",
       x = "Marital Status", y = "Attrition Rate (%)") +
  theme_minimal()+ 
  theme(plot.title = element_text(face = "bold",size = 16 ),axis.title = element_text(face = "bold",size = 12),axis.text=element_text(face = "bold"))

## Attrition and Job Details 

Jd <- read_csv("Job.csv")

View(Jd)

Job_detail <- Jd %>%
  distinct() %>%
  drop_na()
## Attrition Department wise 

Department_Wise <- Job_detail %>%
  group_by(Department) %>%
  summarise(total_employee = n(),Attritions = sum(Attrition == TRUE),Attrition_rate=Attritions/total_employee*100)
View(Department_Wise)
ggplot(data = Department_Wise) + geom_col(mapping = aes(reorder(Department,Attrition_rate),y=Attrition_rate,fill= Department)) +
  geom_text(mapping = aes(reorder(Department,Attrition_rate),y=Attrition_rate,label=paste0(round(Attrition_rate,1),"%")),vjust = -0.2, fontface = "bold") +
  labs(title = "Attrition Rate By Department",x="Department",y="Attrition Rate (%)") + theme_minimal() + 
  theme(plot.title = element_text(face = "bold",size=16),axis.title=element_text(face ="bold",size= 12))

## Attrition job role wise

Role_Wise <- Job_detail %>%
  group_by(JobRole) %>%
  summarise(total_employee = n(),Attritions = sum(Attrition == TRUE),Attrition_rate=Attritions/total_employee*100)

View(Role_Wise)

ggplot(Role_Wise,aes(reorder(JobRole,Attrition_rate),y=Attrition_rate))+
  geom_segment(aes(xend=JobRole,y=0,yend=Attrition_rate),color ="blue",size=1.5) + 
  geom_point(size =5 , color = "red") +
  labs(title = "Attrition Rate By Job Role",x="Job Role", y = "Attrition Rate (%)") +
  theme_minimal() + 
  theme(plot.title = element_text(face = "bold",size = 16),axis.title =element_text(face = "bold",size =12),axis.text.x=element_text(face= "bold",angle =30 , hjust =1))

## Job level vs Attrition 

Job_wise <- Job_detail %>%
  group_by(JobLevel) %>%
  summarize (total_employee=n(), Attritions = sum(Attrition==TRUE), Attrition_rate=Attritions/total_employee*100)

View(Job_wise)

ggplot(Job_wise,aes(x=reorder(JobLevel, Attrition_rate),y=Attrition_rate, size = Attrition_rate,fill = JobLevel))+
  geom_bar(stat = "identity") +
  scale_fill_viridis_b() +
  labs(title="Attrition Rate by Job Level",x="Job Level",y= "Attrition Rate(%)", size="Attrition Rate",color="Job Level") +
  theme_minimal() +
  theme(plot.title = element_text(face="bold", size=16),axis.title = element_text(face="bold", size=12),axis.text =element_text(face="bold"),legend.title  = element_text(face = "bold")) 

## Attrition vs business travel 
Travel_Wise <- Job_detail %>%
  group_by(BusinessTravel) %>%
  summarise(total_employee = n(),Attritions = sum(Attrition == TRUE),Attrition_Rate = Attritions/total_employee*100)

View(Travel_Wise)

ggplot(Travel_Wise , aes (x = reorder(BusinessTravel, Attrition_Rate),y = Attrition_Rate, fill = BusinessTravel)) + 
  geom_bar(stat= "identity") +
  coord_flip() +
  labs(title = "Attrition Rate  By Business Travel ", x = "Business Travel", y = "Attrition Rate (%)",fill = "Business Travel") +
  geom_text(aes (x = reorder(BusinessTravel, Attrition_Rate),y = Attrition_Rate,label=paste0(round(Attrition_Rate,1),"%")),hjust = -0.3, fontface = "bold",color = "black") +
  scale_fill_ordinal(labels = c("Non-Travel" = "No Travel", "Travel_Frequently" = "Too Much Travel", "Travel_Rarely" = "Rarely Travel"))  +
  scale_x_discrete(labels = c("Non-Travel" = "No Travel", "Travel_Frequently" = "Too Much Travel", "Travel_Rarely" = "Rarely Travel")) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold",size = 16 ),axis.title = element_text(face="bold",size = 12),axis.text = element_text(face = "bold") )

## Attrition vs. Distance From Home 

Distance_Wise <- Job_detail %>%
  group_by(DistanceFromHome) %>%
  summarise(total_employee = n(),Attritions = sum(Attrition == TRUE),Attrition_Rate = round(Attritions/total_employee*100,2))

View(Distance_Wise)

ggplot(Distance_Wise , aes(x= DistanceFromHome,y=Attrition_Rate,size = Attrition_Rate,color = DistanceFromHome)) +
  geom_point()  +
  scale_colour_viridis_b() +
  labs (title = "Attrition By Distance From Home",x="Distance From Home",y="Attrition Rate",color = "Distance",size="Attrition Rate") +
  theme_light() +
  theme(plot.title = element_text(face = "bold",size = 16,color = "orange"),axis.title = element_text(face = "bold", size = 12))
## Attrition BY job compensation
Job_Comp <- read_csv("Job_income.csv")

view(Job_Comp)

ggplot(Job_Comp,aes(x="",y=Attrition_Rate,fill = Monthly_Income)) +
  geom_col() +
  coord_polar("y") +
  geom_text(aes(label = paste0(Attrition_Rate, "%")),position = position_stack(vjust = 0.5) , fontface = "bold",color = "white",size = 4) +
  labs(title = "Attrition By Income",x ="Income",y="Attrition Rate(%)",fill = "Monthly Income") +
  scale_fill_manual(values = c("#092c5c", "#ff3232", "#f5d130"))+
  theme_light() +
  theme(plot.title = element_text(face= "bold",size =16),axis.title = element_text(face= "bold",size =12),axis.text = element_blank(),panel.grid = element_blank())

# satisfaction score vs Attrition 
Sts <- read_csv("Sat_score.csv")
View(Sts)

sat_score <- Sts %>%
  distinct()

View(sat_score)

## Job satisfaction vs Attrition

Jsat_Wise <- sat_score %>%
  group_by(JobSatisfaction) %>%
  summarise(total_employee = n(),Attritions = sum(Attrition == TRUE),Attrition_Rate = round(Attritions/total_employee*100,2))

glimpse(Jsat_Wise)

ggplot(Jsat_Wise,aes(x="",y = Attrition_Rate, fill = JobSatisfaction))+
  geom_col(width = .25) + 
  scale_fill_gradient2() +
  coord_flip() +
  geom_text(aes(label = paste0(Attrition_Rate,"%")),position = position_stack(vjust = 0.5) , fontface = "bold",color = "white",size = 4) +
  labs(title = "Attrition By Job Satisfaction" ,x = "Job Satisfaction",y = "Attrition Rate (%)",fill = "Job Satisfaction Score") +
  theme(plot.title = element_text(face= "bold",size =16,color = "#ffa600"),axis.title = element_text(face= "bold",size =12))

## Attrition vs work life 

Wlb_Wise <- sat_score %>%
  group_by(WorkLifeBalance) %>%
  summarise(total_employee = n(),Attritions = sum(Attrition == TRUE),Attrition_Rate = round(Attritions/total_employee*100,2))

glimpse(Wlb_Wise)

ggplot(Wlb_Wise,aes(x=WorkLifeBalance ,y=Attrition_Rate)) +
  geom_segment(aes(xend=WorkLifeBalance,y=0,yend=Attrition_Rate),color ="#1982c4",size=1.5) + 
  geom_point(size =6 , color = "#f50538") +
  geom_text(aes(WorkLifeBalance,y=Attrition_Rate,label = paste0(round(Attrition_Rate,1),"%")),vjust = -0.9,fontface = "bold",size =4 , color = "black") +
  labs(title = "Attriton By Work life Balance",x="Work Life Balance Score", y = "Attrition Rate (%)") +
  theme(plot.title = element_text(face= "bold",size =16,color = "#ffa600"),axis.title = element_text(face= "bold",size =12))
