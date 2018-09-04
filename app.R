
library(shiny)
library(tidyverse)
library(stringr)
library(readr)
library(rlang)

### Reading in the files whilch were saved after making changes from R file.
applications <- read.csv("application_final.csv", stringsAsFactors = FALSE)
appointments <- read.csv("appointments_final.csv", stringsAsFactors = FALSE)
interviews <- read.csv("interviews_final.csv", stringsAsFactors = FALSE)
careerfair <- read.csv("careerfair_final.csv", stringsAsFactors = FALSE)
ugrd <- read.csv("ugrads.csv", stringsAsFactors = FALSE)


### Creating a dashboard

ui <- navbarPage(
  
  ## Title of the App
  title = "Career Center Stats",
  
  ## Student proportion in Engineering School
  tabPanel("UGRD Student Stats",
           wellPanel(
             
             selectInput(inputId = "ugrd1", label = "Variable 1",
                         choices = c("Gender" = "Gender",
                                     "Ethnicity" = "Ethnicity.IPEDS",
                                     "Cumulative GPA" = "Cumulative.GPA"))
             # ,
             # 
             # selectInput(inputId = "ugrd2", label = "Variable 2 (Optional)",
             #             choices = c("Gender" = "Gender",
             #                         "Ethnicity" = "Ethnicity.IPEDS",
             #                         "Cumulative GPA" = "Cumulative.GPA"),
             #             selected = NULL)
             
           ),
           htmlOutput(outputId = "ugrd_text"),
           plotOutput(outputId = "ugrd_plot"),
           verbatimTextOutput(outputId = "ugrd_table")
  ),
  
  ## New Tab - Appointments
  tabPanel("Appointments",
           wellPanel(
             
             selectInput(inputId = "appointments1", label = "Variable 1",
                         choices = c("Gender" = "Gender",
                                     "Ethnicity" = "Ethnicity.IPEDS",
                                     "Cumulative GPA" = "Cumulative.GPA",
                                     "Major" = "Major.1",
                                     "Student Year" = "Student.Year"))
             # ,
             # 
             # selectInput(inputId = "appointments2", label = "Variable 2 (Optional)",
             #             choices = c("Gender" = "Gender",
             #                         "Ethnicity" = "Ethnicity.IPEDS",
             #                         "Cumulative GPA" = "Cumulative.GPA",
             #                         "Major" = "Major1",
             #                         "Student Year" = "Student.School.Year",
             #                         NULL),
             #             selected = NULL)
             
           ),
           htmlOutput(outputId = "appointments_text"),
           plotOutput(outputId = "appointments_plot"),
           plotOutput(outputId = "appointments_plot2"),
           verbatimTextOutput(outputId = "appointments_table")
  ),
  
  ########################################
  
  ## New Tab - On-Ground Job Applications
  tabPanel("OG Applications",
           wellPanel(
             
             selectInput(inputId = "applications1", label = "Variable 1",
                         choices = c("Gender" = "Gender",
                                     "Ethnicity" = "Ethnicity.IPEDS",
                                     "Cumulative GPA" = "Cumulative.GPA",
                                     "Major" = "Major.1",
                                     "Student Year" = "Student.School.Years.Name")
             )
             # ,
             # 
             # selectInput(inputId = "applications2", label = "Variable 2 (Optional)",
             #             choices = c("Gender" = "Gender",
             #                         "Ethnicity" = "Ethnicity.IPEDS",
             #                         "Cumulative GPA" = "Cumulative.GPA",
             #                         "Major" = "Major1",
             #                         "Student Year" = "Student.School.Years.Name",
             #                         NULL),
             #             selected = NULL)
             
           ),
           htmlOutput(outputId = "applications_text"),
           plotOutput(outputId = "applications_plot"),
           verbatimTextOutput(outputId = "applications_table")
  ),
  
  #######################################
  
  ## New Tab - On-Ground Interviews
  tabPanel("OG Interviews",
           wellPanel(
             
             selectInput(inputId = "interviews1", label = "Variable 1",
                         choices = c("Gender" = "Gender",
                                     "Ethnicity" = "Ethnicity.IPEDS",
                                     "Cumulative GPA" = "Cumulative.GPA",
                                     "Major" = "Major.1",
                                     "Student Year" = "Student.School.Years.Name")
             )
             # ,
             # 
             # selectInput(inputId = "interviews2", label = "Variable 2 (Optional)",
             #             choices = c("Gender" = "Gender",
             #                         "Ethnicity" = "Ethnicity.IPEDS",
             #                         "Cumulative GPA" = "Cumulative.GPA",
             #                         "Major" = "Major1",
             #                         "Student Year" = "Student.School.Years.Name",
             #                         NULL),
             #             selected = NULL)
             # 
           ),
           htmlOutput(outputId = "interviews_text"),
           plotOutput(outputId = "interviews_plot"),
           verbatimTextOutput(outputId = "interviews_table")
  ),
  
  #########################################
  
  ## New Tab - On-Ground Job Applications
  tabPanel("Career Fair",
           wellPanel(
             
             selectInput(inputId = "careerfair1", label = "Variable 1",
                         choices = c("Career Fair Name (Recommended)" = "Career.Fair.Name",
                                     "Gender" ,
                                     "Ethnicity" = "Ethnicity.IPEDS",
                                     "Cumulative GPA" = "Cumulative.GPA",
                                     "Major" = "Major.1",
                                     "Student Year" = "Student.Attendee.School.Years.Name")
             )
             # ,
             # 
             # selectInput(inputId = "careerfair2", label = "Variable 2",
             #             choices = c("Career Fair" = "Career.Fair.Name",
             #                         "Gender" = "Gender",
             #                         "Ethnicity" = "Ethnicity.IPEDS",
             #                         "Major" = "Major.1",
             #                         "Student Year" = "Student.Attendee.School.Years.Name",
             #                         NULL),
             #             selected = NULL)
             
           ),
           htmlOutput(outputId = "careerfair_text"),
           plotOutput(outputId = "careerfair_plot"),
           verbatimTextOutput(outputId = "careerfair_table")
  )
  
)

#########################################################################################

server <- function(input, output,session) {
  
  ######################################################################################
  ############################### Tab 0 - UGRD Stats ###################################
  
  vals_ugrd <- reactiveValues()
  
  observe({
    
    input$ugrd1; #input$ugrd2;
    
    ## Grouping by Variable 1 and 2
    ## Else If Condition If Variable 2 is Used
    
    
    #if(is.null(input$ugrd2) ){
    
    ugrd_percent <-ugrd %>%
      filter(!is.na(!!parse_quosure(input$ugrd1))) %>% 
      group_by_(input$ugrd1) %>% 
      summarise(Count = n()) %>%
      mutate_(x=input$ugrd1) %>% 
      mutate(y= factor(x),
             x = factor(x,levels = y[length(y):1]),
             cumulative = cumsum(Count),
             midpoint = cumulative - Count / 2,
             proportion.count= paste0(round((Count / sum(Count)) * 100, 1),"%"," (",Count, ") ")) %>%
      mutate_if(is.character, as.factor)
    
    ## Pie Chart
    p_ugrd <- ggplot(ugrd_percent, aes( x= "", weight = Count, fill = x)) +
      geom_bar(width = 1, position = "stack") +
      coord_polar(theta = "y", start = 0) +
      labs(x = "", y = "", title = paste0("Piechart of UGRD Students by ",input$ugrd1),
           fill = input$ugrd1) + 
      geom_text(aes(x = 1.2, y = midpoint , label = proportion.count), color="black",
                fontface = "bold") +
      theme(plot.title = element_text(hjust = 0.5,face="bold", size = 18), 
            legend.title = element_text(hjust = 0.5, face="bold", size = 15),
            legend.text = element_text(size = 10))  
    
    # }else{
    #   
    #   ugrd_percent <-ugrd %>%
    #     group_by_(input$ugrd1,input$ugrd2) %>% 
    #     summarise(Count = n()) %>% 
    #     mutate(cumulative = cumsum(Count),
    #            midpoint = (cumulative - Count) / 2,
    #            labels= paste0(round((Count / sum(Count)) * 100, 1),"%"," (",Count, ") "))
    #   
    #   ## Pie Chart
    #   p_ugrd <-ugrd_percent %>% 
    #     ggplot( aes_(x = "", y = "Count", fill = input$ugrd1)) +
    #     geom_bar(width = 1, stat = "identity") +
    #     coord_polar(theta = "y", start = 0) +
    #     labs(x = "", y = "", title = paste0("Piechart of UGRD Students"),
    #          subtitle = paste0("The Total UGRD Students of ",sum(ugrd_percent$Count)),
    #          fill = input$ugrd1) + 
    #     geom_text(aes(x = 1.2, y = midpoint , label = labels), color="black",
    #               fontface = "bold") +
    #     theme(plot.title = element_text(hjust = 0.5), 
    #           legend.title = element_text(hjust = 0.5, face="bold", size = 10))+
    #     facet_wrap(~input$ugrd2)
    #   
    # }
    
    
    vals_ugrd$pdata <- p_ugrd
    vals_ugrd$ugrd <- ugrd_percent[,c(input$ugrd1,"proportion.count")]
    vals_ugrd$total <- sum(ugrd_percent[,"Count"])
    
  })
  
  
  observeEvent(vals_ugrd$pdata,{
    output$ugrd_plot <- renderPlot({
      isolate(vals_ugrd$pdata)
    })
  })
  output$ugrd_table <- renderPrint({
    vals_ugrd$ugrd
  })
  output$ugrd_text <- renderText({
    paste0("<b>","Total Number of UGRAD Students in the year 2017-18 : ", vals_ugrd$total,"</b>")
  })
  
  
  ######################################################################################
  ############################### Tab 1 - Appointments ###################################
  
  vals_appointments <- reactiveValues()
  
  observe({
    
    input$appointments1; #input$appointments2;
    
    ## Grouping by Variable 1 and 2
    ## Else If Condition If Variable 2 is Used
    
    
    #if(is.null(input$appointments2) ){
    
    appointments_percent <-appointments %>%
      filter(!is.na(!!parse_quosure(input$appointments1))) %>% 
      group_by_(input$appointments1) %>% 
      summarise(Count = n()) %>%
      mutate_(x=input$appointments1) %>% 
      mutate(y= factor(x),
             x = factor(x,levels = y[length(y):1]),
             cumulative = cumsum(Count),
             midpoint = cumulative - Count / 2,
             proportion.count= paste0(round((Count / sum(Count)) * 100, 1),"%"," (",Count, ") ")) %>%
      mutate_if(is.character, as.factor)
    
    ## Pie Chart
    p_appointments <- ggplot(appointments_percent, aes( x= "", weight = Count, fill = x)) +
      geom_bar(width = 1, position = "stack") +
      coord_polar(theta = "y", start = 0) +
      labs(x = "", y = "", title = paste0("Piechart of Appointments by ",input$appointments1),
           fill = input$appointments1) + 
      geom_text(aes(x = 1.2, y = midpoint , label = proportion.count), color="black",
                fontface = "bold") +
      theme(plot.title = element_text(hjust = 0.5,face="bold", size = 18), 
            legend.title = element_text(hjust = 0.5, face="bold", size = 15),
            legend.text = element_text(size = 10))  
    
    # }else{
    #   
    #   appointments_percent <-appointments %>%
    #     group_by_(input$appointments1,input$appointments2) %>% 
    #     summarise(Count = n()) %>% 
    #     mutate(cumulative = cumsum(Count),
    #            midpoint = (cumulative - Count) / 2,
    #            labels= paste0(round((Count / sum(Count)) * 100, 1),"%"," (",Count, ") "))
    #   
    #   ## Pie Chart
    #   p_appointments <-appointments_percent %>% 
    #     ggplot( aes_(x = "", y = "Count", fill = input$appointments1)) +
    #     geom_bar(width = 1, stat = "identity") +
    #     coord_polar(theta = "y", start = 0) +
    #     labs(x = "", y = "", title = paste0("Piechart of appointments"),
    #          subtitle = paste0("The Total appointments of ",sum(appointments_percent$Count)),
    #          fill = input$appointments1) + 
    #     geom_text(aes(x = 1.2, y = midpoint , label = labels), color="black",
    #               fontface = "bold") +
    #     theme(plot.title = element_text(hjust = 0.5), 
    #           legend.title = element_text(hjust = 0.5, face="bold", size = 10))+
    #     facet_wrap(~input$appointments2)
    #   
    # }
    
    ###### Stacked Bar Chart for Drop-in vs Regular Appointments ##########
    
    appointments_plt2 <- appointments %>% 
      filter(!is.na(!!parse_quosure(input$appointments1))) %>% 
      ggplot(aes_string(input$appointments1))+
      geom_bar(aes(fill = Drop_In), position = "stack")+
      labs(x = input$appointments1, y = "Count", title = paste0("Barchart of Appointment Type by ",input$appointments1),
           fill = "Drop-In")+
      theme(plot.title = element_text(hjust = 0.5,face="bold", size = 18), 
            legend.title = element_text(hjust = 0.5, face="bold", size = 15),
            legend.text = element_text(size = 10),
            axis.text.x = element_text(angle = 60, hjust = 1))  
    
    
    vals_appointments$pdata <- p_appointments
    vals_appointments$appointments <- appointments_percent[,c(input$appointments1,"proportion.count")]
    vals_appointments$total <- sum(appointments_percent[,"Count"])
    vals_appointments$plt2 <- appointments_plt2
    
  })
  
  
  observeEvent(vals_appointments$pdata,{
    output$appointments_plot <- renderPlot({
      isolate(vals_appointments$pdata)
    })
  })
  output$appointments_plot2 <- renderPlot({
    vals_appointments$plt2
  })
  output$appointments_table <- renderPrint({
    vals_appointments$appointments
  })
  output$appointments_text <- renderText({
    paste0("<b>","Total Number of OG appointments scheduled in the year 2017-18 : ", vals_appointments$total,"</b>")
  })
  
  ######################################################################################
  ############################### Tab 2 - OG Applications ###################################
  
  vals_applications <- reactiveValues()
  
  observe({
    
    input$applications1; #input$applications2;
    
    ## Grouping by Variable 1 and 2
    ## Else If Condition If Variable 2 is Used
    
    
    #if(is.null(input$applications2) ){
    
    applications_percent <-applications %>%
      filter(!is.na(!!parse_quosure(input$applications1))) %>% 
      group_by_(input$applications1) %>% 
      summarise(Count = sum(Interview.Schedule.Applications.Count)) %>%
      mutate_(x=input$applications1) %>% 
      mutate(y= factor(x),
             x = factor(x,levels = y[length(y):1]),
             cumulative = cumsum(Count),
             midpoint = cumulative - Count / 2,
             proportion.count= paste0(round((Count / sum(Count)) * 100, 1),"%"," (",Count, ") ")) %>%
      mutate_if(is.character, as.factor)
    
    ## Pie Chart
    p_applications <- ggplot(applications_percent, aes( x= "", weight = Count, fill = x)) +
      geom_bar(width = 1, position = "stack") +
      coord_polar(theta = "y", start = 0) +
      labs(x = "", y = "", title = paste0("Piechart of Applications by ",input$applications1),
           fill = input$applications1) + 
      geom_text(aes(x = 1.2, y = midpoint , label = proportion.count), color="black",
                fontface = "bold") +
      theme(plot.title = element_text(hjust = 0.5,face="bold", size = 18), 
            legend.title = element_text(hjust = 0.5, face="bold", size = 15),
            legend.text = element_text(size = 10))  
    
    # }else{
    #   
    #   applications_percent <-applications %>%
    #     group_by_(input$applications1,input$applications2) %>% 
    #     summarise(Count = sum(Interview.Schedule.Applications.Count)) %>% 
    #     mutate(cumulative = cumsum(Count),
    #            midpoint = (cumulative - Count) / 2,
    #            labels= paste0(round((Count / sum(Count)) * 100, 1),"%"," (",Count, ") "))
    #   
    #   ## Pie Chart
    #   p_applications <-applications_percent %>% 
    #     ggplot( aes_(x = "", y = "Count", fill = input$applications1)) +
    #     geom_bar(width = 1, stat = "identity") +
    #     coord_polar(theta = "y", start = 0) +
    #     labs(x = "", y = "", title = paste0("Piechart of applications"),
    #          subtitle = paste0("The Total applications of ",sum(applications_percent$Count)),
    #          fill = input$applications1) + 
    #     geom_text(aes(x = 1.2, y = midpoint , label = labels), color="black",
    #               fontface = "bold") +
    #     theme(plot.title = element_text(hjust = 0.5), 
    #           legend.title = element_text(hjust = 0.5, face="bold", size = 10))+
    #     facet_wrap(~input$applications2)
    #   
    # }
    
    
    vals_applications$pdata <- p_applications
    vals_applications$applications <- applications_percent[,c(input$applications1,"proportion.count")]
    vals_applications$total <- sum(applications_percent[,"Count"])
    
  })
  
  
  observeEvent(vals_applications$pdata,{
    output$applications_plot <- renderPlot({
      isolate(vals_applications$pdata)
    })
  })
  output$applications_table <- renderPrint({
    vals_applications$applications
  })
  output$applications_text <- renderText({
    paste0("<b>","Total Number of OG job applications in the year 2017-18 : ", vals_applications$total,"</b>")
  })
  
  
  ######################################################################################
  ############################### Tab 3 - OG INTERVIEWS ###################################
  
  vals_interviews <- reactiveValues()
  
  observe({
    
    input$interviews1; #input$interviews2;
    
    ## Grouping by Variable 1 and 2
    ## Else If Condition If Variable 2 is Used
    
  
    #if(is.null(input$interviews2) ){
      
    interviews_percent <-interviews %>%
      filter(!is.na(!!parse_quosure(input$interviews1))) %>% 
      group_by_(input$interviews1) %>% 
      summarise(Count = sum(Interview.Schedule.Applications.Count)) %>%
      mutate_(x=input$interviews1) %>% 
      mutate(y= factor(x),
             x = factor(x,levels = y[length(y):1]),
             cumulative = cumsum(Count),
             midpoint = cumulative - Count / 2,
             proportion.count= paste0(round((Count / sum(Count)) * 100, 1),"%"," (",Count, ") ")) %>%
      mutate_if(is.character, as.factor)
    
    ## Pie Chart
    p_interviews <- ggplot(interviews_percent, aes( x= "", weight = Count, fill = x)) +
      geom_bar(width = 1, position = "stack") +
      coord_polar(theta = "y", start = 0) +
      labs(x = "", y = "", title = paste0("Piechart of Interviews by ",input$interviews1),
           fill = input$interviews1) + 
      geom_text(aes(x = 1.2, y = midpoint , label = proportion.count), color="black",
                fontface = "bold") +
      theme(plot.title = element_text(hjust = 0.5,face="bold", size = 18), 
            legend.title = element_text(hjust = 0.5, face="bold", size = 15),
            legend.text = element_text(size = 10))  
      
    # }else{
    #   
    #   interviews_percent <-interviews %>%
    #     group_by_(input$interviews1,input$interviews2) %>% 
    #     summarise(Count = sum(Interview.Schedule.Applications.Count)) %>% 
    #     mutate(cumulative = cumsum(Count),
    #            midpoint = (cumulative - Count) / 2,
    #            labels= paste0(round((Count / sum(Count)) * 100, 1),"%"," (",Count, ") "))
    #   
    #   ## Pie Chart
    #   p_interviews <-interviews_percent %>% 
    #     ggplot( aes_(x = "", y = "Count", fill = input$interviews1)) +
    #     geom_bar(width = 1, stat = "identity") +
    #     coord_polar(theta = "y", start = 0) +
    #     labs(x = "", y = "", title = paste0("Piechart of Interviews"),
    #          subtitle = paste0("The Total Interviews of ",sum(interviews_percent$Count)),
    #          fill = input$interviews1) + 
    #     geom_text(aes(x = 1.2, y = midpoint , label = labels), color="black",
    #               fontface = "bold") +
    #     theme(plot.title = element_text(hjust = 0.5), 
    #           legend.title = element_text(hjust = 0.5, face="bold", size = 10))+
    #     facet_wrap(~input$interviews2)
    #   
    # }
    
    
    vals_interviews$pdata <- p_interviews
    vals_interviews$interviews <- interviews_percent[,c(input$interviews1,"proportion.count")]
    vals_interviews$total <- sum(interviews_percent[,"Count"])
    
  })
  
  
  observeEvent(vals_interviews$pdata,{
    output$interviews_plot <- renderPlot({
      isolate(vals_interviews$pdata)
    })
  })
  output$interviews_table <- renderPrint({
    vals_interviews$interviews
  })
  output$interviews_text <- renderText({
    paste0("<b>","Total Number of OG Interviews scheduled in the year 2017-18 : ", vals_interviews$total,"</b>")
  })
  
  
  ######################################################################################
  ############################### Tab 4 - Career Fair ###################################
  
  vals_careerfair <- reactiveValues()
  
  observe({
    
    input$careerfair1; #input$careerfair2;
    
    ## Grouping by Variable 1 and 2
    ## Else If Condition If Variable 2 is Used
    
    
    #if(is.null(input$careerfair2) ){
    
    careerfair_percent <-careerfair %>%
      filter(!is.na(!!parse_quosure(input$careerfair1))) %>% 
      group_by_(input$careerfair1) %>% 
      summarise(Count = n()) %>%
      mutate_(x=input$careerfair1) %>% 
      mutate(y= factor(x),
             x = factor(x,levels = y[length(y):1]),
             cumulative = cumsum(Count),
             midpoint = cumulative - Count / 2,
             proportion.count= paste0(round((Count / sum(Count)) * 100, 1),"%"," (",Count, ") ")) %>%
      mutate_if(is.character, as.factor)
    
    ## Pie Chart
    p_careerfair <- ggplot(careerfair_percent, aes( x= "", weight = Count, fill = x)) +
      geom_bar(width = 1, position = "stack") +
      coord_polar(theta = "y", start = 0) +
      labs(x = "", y = "", title = paste0("Distribution of Career Fair Attendance by ",input$careerfair1),
           fill = input$careerfair1) + 
      geom_text(aes(x = 1.2, y = midpoint , label = proportion.count), color="black",
                fontface = "bold") +
      theme(plot.title = element_text(hjust = 0.5,face="bold", size = 18), 
            legend.title = element_text(hjust = 0.5, face="bold", size = 15),
            legend.text = element_text(size = 10))  
    
    # }else{
    #   
    #   careerfair_percent <-careerfair %>%
    #     group_by_(input$careerfair1,input$careerfair2) %>% 
    #     summarise(Count = n()) %>% 
    #     mutate(cumulative = cumsum(Count),
    #            midpoint = (cumulative - Count) / 2,
    #            labels= paste0(round((Count / sum(Count)) * 100, 1),"%"," (",Count, ") "))
    #   
    #   ## Pie Chart
    #   p_careerfair <-careerfair_percent %>% 
    #     ggplot( aes_(x = "", y = "Count", fill = input$careerfair1)) +
    #     geom_bar(width = 1, stat = "identity") +
    #     coord_polar(theta = "y", start = 0) +
    #     labs(x = "", y = "", title = paste0("Distribution of Career Fair Attendance")
    #          fill = input$careerfair1) + 
    #     geom_text(aes(x = 1.2, y = midpoint , label = labels), color="black",
    #               fontface = "bold") +
    #     theme(plot.title = element_text(hjust = 0.5), 
    #           legend.title = element_text(hjust = 0.5, face="bold", size = 10))+
    #     facet_wrap(~input$careerfair2)
    #   
    # }
    
    
    vals_careerfair$pdata <- p_careerfair
    vals_careerfair$careerfair <- careerfair_percent[,c(input$careerfair1,"proportion.count")]
    vals_careerfair$total <- sum(careerfair_percent[,"Count"])
    
  })
  
  
  observeEvent(vals_careerfair$pdata,{
    output$careerfair_plot <- renderPlot({
      isolate(vals_careerfair$pdata)
    })
  })
  output$careerfair_table <- renderPrint({
    vals_careerfair$careerfair
  })
  output$careerfair_text <- renderText({
    paste0("<b>","Total Number of students registered for Career Fair on Handshake in the year 2017-18 : ", vals_careerfair$total,"</b>")
  })
  
}

shinyApp(ui, server)

