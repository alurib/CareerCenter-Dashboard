library(tidyverse)
library(readxl)
library(stringr)
library(rlang)

#########################################################################
########## Analysis of career center data for Undergraduates ###########
########################################################################

students18 <- read_xls("engr_students.xls" )

students_short <- students18 %>% 
  select(`Email Address UVA`,`Academic Career`,`Ethnicity IPEDS`,
         `Cumulative GPA`,Gender)
  
summary(students18)

careerfair18 <- read_xlsx("2017-2018 Career Fair Attendees.xlsx")
applicationsOG18 <- read_xlsx("2017-2018 On Ground Interview Applications.xlsx")
interviewsOG18 <- read_xlsx("2017-2018 On Grounds Interviews.xlsx")
appointments18 <- read_xlsx("Junior Senior appointments.xlsx")



applications_f1 <- right_join(students_short,applicationsOG18, by = c("Email Address UVA"="Applicant (student) Email"))
interviews_f1 <- right_join(students_short,interviewsOG18, by = c("Email Address UVA"="Applicant (student) Email"))
appointments_f1 <- right_join(students18,appointments18, by = c("Email Address UVA"="Student Email"))
careerfairf1 <- right_join(students_short, careerfair18, by = c("Email Address UVA"= "Student Attendees Email"))                              

summary(applications_f1)

## Need to divide the double major guys by "," and create new column
# SEAS vs Non-SEAS appointmnets

applications_f2 <- applications_f1 %>% 
  separate(`Student Majors Name List`,c("Major.1","Major.2","Major.3"),",")%>% 
  mutate(DoubleMajor = ifelse(is.na(Major.2),"No","Yes"),
         Student.Year = `Student School Years Name`,
         Cumulative.GPA = cut(`Cumulative GPA`,c(-Inf,3.00,3.25,3.5,3.75,4.0),
                              ordered_result = TRUE)) %>% 
  mutate_if(is.character, as.factor) %>% 
  select(-Major.3)
summary(applications_f2)


interviewss_f2 <- interviews_f1 %>% 
  separate(`Student Majors Name List`,c("Major.1","Major.2","Major.3"),",")%>% 
  mutate(DoubleMajor = ifelse(is.na(Major.2),"No","Yes"),
         Student.Year = `Student School Years Name`,
         Cumulative.GPA = cut(`Cumulative GPA`,c(-Inf,3.00,3.25,3.5,3.75,4.0))) %>% 
  mutate_if(is.character, as.factor) %>% 
  select(-Major.3)

appointments_f2 <- appointments_f1 %>% 
  select(`Email Address UVA`,`Academic Career`,`Ethnicity IPEDS`,`Cumulative GPA`,
         Gender, `Student School Year (at Appt. Time) Name`,`Appointment Type Name`,
         `Appointments Start Date Date`,`Staff Member First Name`,`Student Majors (at Appt. Time) Name List`) %>% 
  rename(Student.Year = `Student School Year (at Appt. Time) Name`) %>% 
  separate(`Student Majors (at Appt. Time) Name List`,c("Major.1","Major.2","Major.3"),",", extra = "drop")%>% 
  mutate(DoubleMajor = ifelse(is.na(Major.2),"No","Yes"),
         SEAS_Appointment = ifelse(str_detect(`Appointment Type Name`,"SEAS"),"Yes","No"),
         Cumulative.GPA = cut(`Cumulative GPA`,c(-Inf,3.00,3.25,3.5,3.75,4.0)),
         Drop_In = ifelse(str_detect(`Appointment Type Name`,"Drop In"),"Yes","No")) %>% 
  mutate_if(is.character, as.factor) %>% 
  select(-Major.3)

careerfair_f2 <- careerfairf1 %>% 
  separate(`Student Attendee Majors Name List`,c("Major.1","Major.2","Major.3"),",")%>% 
  mutate(DoubleMajor = ifelse(is.na(Major.2),"No","Yes"),
         Student.Year = `Student Attendee School Years Name`,
         Cumulative.GPA = cut(`Cumulative GPA`,c(-Inf,3.00,3.25,3.5,3.75,4.0)),
         Major.1 = ifelse(Major.1 %in% c(
           "Economics","Engineering - Aerospace Engineering",
           "Engineering - Biomedical Engineering","Engineering - Chemical Engineering",
           "Engineering - Civil Engineering","Engineering - Computer Engineering",
           "Engineering - Computer Science","Engineering - Electrical Engineering",
           "Engineering - Engineering Science","Engineering - Materials Science & Engineering",
           "Engineering - Mechanical & Aerospace Engineering","Engineering - Mechanical Engineering",
           "Engineering - Systems Engineering","Mathematics","Physics","Statistics"),
           Major.1,"Other"
         )) %>% 
  mutate_if(is.character, as.factor) %>% 
  select(-Major.3)

ugrads18 <- students_short %>% 
  filter(`Academic Career`=="UGRD") %>% 
  mutate(`Cumulative GPA` = cut(`Cumulative GPA`,c(-Inf,3.00,3.25,3.5,3.75,4.0))) %>% 
  mutate_if(is.character, as.factor)
  

write.csv(ugrads18,"ugrads.csv")
write.csv(applications_f2,"application_final.csv")
write.csv(appointments_f2, "appointments_final.csv")
write.csv(interviewss_f2, "interviews_final.csv")
write.csv(careerfair_f2,"careerfair_final.csv")










