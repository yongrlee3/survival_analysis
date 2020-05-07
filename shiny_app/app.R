library(shiny)
library(tidyverse)
library(shinythemes)
library(plotly)
library(stargazer)
library(scales)
library(lmtest)
library(gganimate)
library(patchwork)
library(shinydashboard)
library(gt)

# load datasets
data_all <- read_rds("raw-data/data_all.rds")
data_students <- read_rds("raw-data/data_students.rds")
data <- read_rds("raw-data/data.rds")
data_pp <- read_rds("raw-data/data_pp.rds")

ui <- navbarPage(
  theme = shinytheme("sandstone"),
  "Synchronous Lecture Attendance in Online Courses",
  tabPanel(
    "About",
    tabsetPanel(
        tabPanel("Background",
                 br(),
    p("The transition to online learning in the midst of COVID-19 has disrupted traditional in-person classrooms, raising concerns about student engagement in online learning. The goal of this project is to examine differential patterns of attrition in synchronous attendance between different online courses. This analysis focuses on two online courses: GOV1005 and S052. GOV1005 is an introductory data science course taught by David Kane, a preceptor for Harvard College's Goverment department. S052 is an advanced statistical methods course taught by Andrew Ho, a professor of education at the Harvard Graduate School of Education."),
    p("While both courses are led by engaging instructors who have seamlessly transitioned to online learning, differing attitudes towards synchronous lecture attendance offers the unique opportunity to test whether stricter attendance policies result in better attendance outcomes. GOV1005 offers no recorded lectures and requires synchronous lecture attendance as part of students' participation grades, whereas S052 accomodates a more lenient attendance policy that allows students to attend asynchronously through recorded sessions."),
    p("It is important to note that this analysis helps to quantify the effect of online attendance policies on synchronous lecture attendance, and not student learning outcomes or even student lecture attendance overall. In addition, this project explores differential patterns of attrition across gender, as well as periods throughout the semester where students are most susceptible to attrition. In doing so, this analysis aims to inform pedagogical strategies that can help address challenges in online learning related to course philosophy, semester seasonality, and student groups.")
        ),
    tabPanel("About Me",
             br(),
    p("I am a former finance professional studying Education Policy and Management at the Harvard Graduate School of Education. I am passionate about leveraging data to inform strategic insights; namely in the fields of education, finance, and technology."),
    p(
        "You can reach me at ",
        a("yonglee@gse.harvard.edu",
          href = "mailto: yonglee@gse.harvard.edu",
        ),
        "or ",
        a("LinkedIn",
          href = "https://www.linkedin.com/in/yong-lee-034b39b8/"
        ),
        "."
    )
  )
        )
    ),
  tabPanel(
    "Data",
    tabsetPanel(
      tabPanel(
        "Collection & Manipulation",
        br(),
        p("Both classes conducted biweekly lectures on Tuesdays and Thursdays, hosting 11 lectures in total. Lecture attendance data was collected via Zoom for students in both classes, who have been anonymized for privacy purposes. Additionally, student characteristics data collected from Canvas' course roster was used to supplement the existing attendance data. Future extensions of this analysis may involve examining differential patterns of attrition across additional predictors like registration type, student year, or school given a larger dataset of more than two courses."),
        p("Because members of the teaching staff operate under different incentives than students, their attendance data was excluded from this survival analysis. Additionally, students who never attended online lecture synchronously throughout the 11 lecture period were also excluded from this analysis. These students were assumed to have been excused from synchronous lecture attendance as a result of case-specific challenges like time-zone differences or learning accomodations. Only students who attended at least one online lecture synchronously were considered for this analysis. A student was defined as dropping out of online lectures if they failed to synchronously attend four or more consecutive lectures after having attended one previously. Given the 11 lecture timeline, a threshold of five missed lectures was too conservative while three missed lectures was too aggressive considering the biweekly nature of both courses.")
      ),
      tabPanel(
        "Data Censoring",
        br(),
        p("Lecture attendance was tracked on a relative basis to account for students whose transition to online learning was interupted by travel and other extenuating circumstances. As a result, if certain students began attending online lectures synchronously after the third lecture, their attendance in the fourth lecture was recorded for their first lecture. To keep lecture periods consistent on an absolute basis, these students were censored in the dataset so that only their attendance in lectures 4-11 were included in the analysis."),
        p("Censoring addresses a problem intrinsic to survival data: the fact that not every student experiences the drop out event within the duration of the study. While students who attended all 11 lectures may drop out after 15 or 20 lectures, the timeline for data collection limits this information. Therefore there is no way to know the drop out period for students in the sample who may have the longest times-to-event. Additionally, students who stopped attending after lecture 8 cannot be defined as having dropped out, since their attendance data after lecture 11 does not exist. While censored students present incomplete data, removing them completely would bias the results towards students in the sample who dropped out within the observation period. Therefore, data collected prior to censoring for students who began attending lectures late, continued attending beyond the cutoff, or stopped attending near the cutoff was included in the analysis."),
        p(
          "You can find the data and code for this project on my ",
          a("Github",
            href = "https://github.com/yongrlee3/survival_analysis"
          ),
          "."
        )
      ),
      tabPanel(
        "Censorship Plot",
        br(),
        h3("Lecture Attendance by Censorship"),
        p("Plot reflects data for students who attended at least one online lecture synchronously"),
        plotOutput("censor_plot")
      )
    )
  ),
  tabPanel(
    "Methods",
    tabsetPanel(
      tabPanel(
        "Classical Survival Analysis",
        br(),
        h4("Classical Survival Analysis"),
        p("Survival analysis is a statistical method commonly used to answer whether and when individuals experience a certain event. In this analysis, the event is defined as failing to synchronously attend four or more consecutive online lectures after having attended one previously. This project estimates the hazard and survival functions for synchronous online lecture attendance and uses logistic regression to test for differences in the rates of attrition across time periods, between courses, and across genders."),
        h4("Hazard and Survival Functions"),
        p("The hazard function defines the probability that the subject will experience an event of interest in a given interval. The survival function defines the probability that the event of interest will not occurr in a given interval")
      ),
      tabPanel(
        "Discrete-Time Survival Analysis",
        br(),
        h4("Discrete-Time Survival Analysis"),
        p("While classical survival analysis assumes a continuous time interval, discrete-time survival analysis analyzes time in discrete chunks during which the event of interest can occur. Because lectures are discrete periods, this analysis models the survival function using a Kaplan-Meier curve. Rather than modeling the probability of survival at continuous points in time (i.e. lecture 7.5), the Kaplan-Meier curve applies a step-wise function to estimate the survival probability after discrete periods."),
        h4("Logistic Regression"),
        p("When modeling dichotomous outcomes, the logistic regression serves as a credible and interpretable method for representing the relationship between the underlying probability of an event and different predictors. Beginning with a baseline logistic regression model that estimates the unconditional hazard probability of synchronous online lecture attendance, the analysis tests for statistically significant associations between the hazard probability and predictors like time period, course, and gender."),
        h4("Maximum Log Likelihood"),
        p("The maximum likelihood tests the statistical significance of these associations by comparing the difference in maximum log-likelihood between models. Because the log-likelihood describes the overall goodness-of-fit of the model, its difference can be compared across models to assess whether the inclusion of different predictors significantly affected the estimated outcome.")
      ),
      tabPanel(
        "Person-Period Data",
        br(),
        h4("Converting from Person-Level to Person-Period Data"),
        p("Because hazard probabilities cannot be modeled from person-level data, the data must be converted to person-period data, where each individual contributes one row of data for each time-period. Data records continue until the time-period in which individuals either experience the event of interest, or are censored."),
        selectInput("student",
          "Student ID: ",
          c(1:255),
          selected = 9
        ),
        column(
          6,
          h3("Person-Level Data"),
          tableOutput("person_level")
        ),
        column(
          6,
          h3("Person-Period Data"),
          tableOutput("person_period")
        )
      )
    )
  ),
  tabPanel(
    "Analysis",
    tabsetPanel(
      tabPanel(
        "Data Exploration",
        br(),
        sidebarPanel(
          selectInput("data",
            "Data:",
            c(
              "All Data" = "data_all",
              "Student Data" = "data_students",
              "Attended Student Data" = "data"
            ),
            selected = "data"
          ),
          selectInput("xvar",
            "Variable of Interest:",
            c(
              "Role",
              "Course",
              "Gender",
              "Outcome"
            ),
            selected = "Course"
          ),
          selectInput("groupvar",
            "Grouping Variable:",
            c(
              "Role",
              "Course",
              "Gender",
              "Outcome"
            ),
            selected = "Outcome"
          )
        ),
        mainPanel(
          plotOutput("eda_plot")
        )
      ),
      tabPanel(
        "Survival Analysis",
        br(),
        sidebarPanel(
          selectInput(
            "function_type",
            "Function Type:",
            c(
              "Hazard",
              "Survival",
              "Survival: Kaplan-Meier"
            )
          )
        ),
        mainPanel(
          plotOutput("function_plot")
        )
      ),
      tabPanel(
        "Logistic Regression",
        br(),
        column(
          6,
          h3("Regression Output Taxonomy Table")
        ),
        column(
          2,
          numericInput(
            "intercept",
            "Intercept",
            -5.209
          )
        ),
        column(
          2,
          numericInput(
            "coefficient",
            "Coefficient",
            1.021
          )
        ),
        column(
          2,
          infoBoxOutput("calculation")
        ),
        column(
          12,
          verbatimTextOutput("taxonomy")
        )
      ),
      tabPanel(
        "Maximum Likelihood Tests",
        br(),
        column(
          6,
          selectInput(
            "model1",
            "Select Model",
            c("Model 1" = "mod1", "Model 2" = "mod2", "Model 3" = "mod3", "Model 4" = "mod4", "Model 5" = "mod5", "Model 6" = "mod6"),
            selected = "mod2"
          )
        ),
        column(6, selectInput(
          "model2",
          "Select Comparison Model",
          c("Model 1" = "mod1", "Model 2" = "mod2", "Model 3" = "mod3", "Model 4" = "mod4", "Model 5" = "mod5", "Model 6" = "mod6"),
          selected = "mod3"
        )),
        verbatimTextOutput("comparison")
      ),
      tabPanel(
        "Fitted Hazard Functions",
        br(),
        selectInput(
          "model",
          "Select Model",
          c("Model 1" = "model1", "Model 2" = "model2", "Model 3" = "model3", "Model 4" = "model4", "Model 5" = "model5", "Model 6" = "model6"),
          selected = "model3"
        ),
        plotOutput("model")
      )
    )
  ),
  tabPanel(
    "Results",
    tabsetPanel(
      tabPanel(
        "Findings",
        br(),
        p("Schools have been forced to quickly transition to online learning as a result of the COVID-19 crisis. Has this transition impacted school engagement for specific student groups? Specifically, are there differential patterns of attrition in synchronous online lecture attendance across different courses? This study tracks the attendance outcomes of 107 S052 and 120 GOV1005 students throughout the 11 lectures conducted in an online setting. Students were defined as having exited if they failed to synchronously attend four or more consecutive lectures after having attended one previously. Because 22 of the 227 students never attended lectures synchronously, they were excluded from the analysis."),
        p("The analysis fits a discrete-time hazard model for lecture absences in each week using course as our key predictor. Through logistic regression, the analysis tests whether the hazard probability of dropping out of attending online lectures synchronously is different for S052 students compared to that of GOV1005 students. The maximum likelihood tests the null hypothesis that there is no difference in the log-odds of dropping out between S052 and GOV1005 students who operate under contrasting attendance policies. The difference was statistically significant and I therefore reject the null hypothesis that there were no differing levels of attrition between courses."),
        p("The estimated log-odds of S052 students dropping out is 1.021 (0.951%) higher compared to that of GOV1005 students controlling for time period. Therefore it appears that course attendance policy significantly influences student engagement within the context of synchronous online lecture attendance. Gender and the interaction between course and gender were introduced as additional covariates, but were not found to be statistically significant.")
      ),
      tabPanel(
        "Limitations",
        br(),
        p("Although a strong academic exercise, it is difficult to generalize the findings from this analysis given the small sample size. Additionally, student characteristic predictors like enrollment type, grade, and school were unavailable for this iteration of the analysis. Data collection on a larger, longer, and more comprehensive scale may yield more robust results that are generalizable to the population."),
        p("Even with more observations, the findings from this analysis may not apply to student attendance in all online classrooms. Given the unique circumstances of the COVID-19 crisis, instructors have likely adopted looser guidelines for course attendance. Additionally, student attendance during this crisis may not reflect regular online student behavior in more normal times. Given that this project only analyzes two courses that not only occur at different times but are also composed of different types of students, any observed differences may simply be a result of these external factors. It is crucial that similar analyses in the future analyze similarly structured courses that only differ in attendance policy."),
        p("Lastly, the designations made for censoring and defining the event of interest were thoroughly considered, but remain unsupported by data. A different censoring method, as well as a more conservative or aggressive event threshold may prove to be more appropriate in conducting this survival analysis.")
      ),
      tabPanel(
        "Acknowledgements",
        br(),
        p("Thank you Preceptor Kane, Professor Ho, and all other members of the teaching staff for your instruction and guidance this semester. You adjusted with grace and understanding while remaining steadfast in the mission to motivate, teach, and serve. There is no better time than during this moment of uncertainty to appreciate the value of statistical methods and data science.")
      )
    )
  )
)

server <- function(input, output, session) {
  output$censor_plot <- renderPlot({

    # plot lecture attendance by censorship
    # fill by censorship status
    # make density plots transparent for overlap visibility

    data %>%
      ggplot(aes(as.factor(Lectures), group = Censor, fill = Censor)) +
      geom_density(alpha = 0.25) +
      theme_classic() +
      labs(
        x = "Lectures Attended",
        y = "Density",
        fill = "Censor Status"
      )
  })

  output$person_level <- renderTable({
    
    # assign IDs of teaching staff to list
    
    teacher <- c(1:8, 116:135)
    
    # generate message if teaching staff ID is selected

    if (input$student %in% teacher) {
      print("This ID belongs to a member of the teaching staff")
    }
    
    # for given student ID, show person-level data

    else {
      data %>%
        filter(ID == input$student) %>%
        select(ID, Course, Gender, Lectures, Censor, Event) %>%
        gt()
    }
  })

  output$person_period <- renderTable({
    
    # assign IDs of teaching staff to list
    
    teacher <- c(1:8, 116:135)
    
    # generate message if teaching staff ID is selected
    
    if (input$student %in% teacher) {
      print("This ID belongs to a member of the teaching staff")
    }

    # for given student ID, show person-period data
    
    else {
      data_pp %>%
        filter(ID == input$student) %>%
        select(ID, Course, Gender, Censor, Period, Exit) %>%
        gt()
    }
  })
  
  output$eda_plot <- renderPlot({
    
    # create reactive plot for users to conduct own exploratory data analysis
    # else if statements used to distinguish between datasets
    # all other reactive input logic is identical across datasets
    
    if (input$data == "data_all") {
      data_all %>%
        ggplot(aes(
          x = data_all[[input$xvar]],
          fill = data_all[[input$groupvar]]
        )) +
        geom_bar(position = "dodge") +
        geom_text(
          stat = "count",
          aes(label = ..count..),
          position = position_dodge(width = 1),
          size = 4,
          vjust = 1
        ) +
        theme_classic() +
        labs(x = input$xvar, y = "Count", fill = input$groupvar)
    }

    else if (input$data == "data_students") {
      data_students %>%
        ggplot(aes(x = data_students[[input$xvar]], fill = data_students[[input$groupvar]])) +
        geom_bar(position = "dodge") +
        geom_text(
          stat = "count",
          aes(label = ..count..),
          position = position_dodge(width = 1),
          size = 4,
          vjust = 1
        ) +
        theme_classic() +
        labs(x = input$xvar, y = "Count", fill = input$groupvar)
    }

    else if (input$data == "data") {
      data %>%
        ggplot(aes(x = data[[input$xvar]], fill = data[[input$groupvar]])) +
        geom_bar(position = "dodge") +
        geom_text(
          stat = "count",
          aes(label = ..count..),
          position = position_dodge(width = 1),
          size = 4,
          vjust = 1
        ) +
        theme_classic() +
        labs(x = input$xvar, y = "Count", fill = input$groupvar)
    }
  })

  output$function_plot <- renderPlot({
    
    # else if statements used to disntiguish different functions
    
    if (input$function_type == "Hazard") {
      
      # generate the conditional hazard probabilities for each period

      hazard <- data_pp %>%
        group_by(Course, Period) %>%
        do(data.frame(sum(.$Exit) / nrow(.))) %>%
        select(Course, Period, Hazardp = sum...Exit..nrow...)

      # plot the fitted conditional hazard function by course

      hazard %>%
        ggplot(aes(Period, Hazardp)) +
        geom_point(aes(color = Course)) +
        geom_line(aes(color = Course, group = Course)) +
        scale_y_continuous(limits = c(0, 0.06), labels = percent) +
        theme_classic() +
        labs(
          x = "Lectures Attended",
          y = "Conditional Hazard Probability",
          color = input$groupvar
        )
    }

    else if (input$function_type == "Survival") {
      
      # generate the conditional survival probabilities for each period

      survival <- data_pp %>%
        group_by(Course, Period) %>%
        do(data.frame(1 - sum(.$Exit) / nrow(.))) %>%
        select(Course, Period, Surv = X1...sum...Exit..nrow...)

      survival$Survivalp <- 1
      for (i in 2:11) {
        survival$Survivalp[i] <- survival$Survivalp[i - 1] * survival$Surv[i]
        survival$Survivalp[12] <- 1
        for (i in 13:22) {
          survival$Survivalp[i] <- survival$Survivalp[i - 1] * survival$Surv[i]
        }
      }

      # plot the fitted conditional survival function by course

      survival %>%
        ggplot(aes(Period, Survivalp)) +
        geom_point(aes(color = Course)) +
        geom_line(aes(color = Course, group = Course)) +
        scale_y_continuous(limits = c(0, 1), labels = percent) +
        theme_classic() +
        labs(
          x = "Lectures Attended",
          y = "Conditional Survival Probability",
          color = "Course"
        )
    }

    else if (input$function_type == "Survival: Kaplan-Meier") {
      
      # generate the conditional survival probabilities for each period

      survival <- data_pp %>%
        group_by(Course, Period) %>%
        do(data.frame(1 - sum(.$Exit) / nrow(.))) %>%
        select(Course, Period, Surv = X1...sum...Exit..nrow...)

      survival$Survivalp <- 1
      for (i in 2:11) {
        survival$Survivalp[i] <- survival$Survivalp[i - 1] * survival$Surv[i]
        survival$Survivalp[12] <- 1
        for (i in 13:22) {
          survival$Survivalp[i] <- survival$Survivalp[i - 1] * survival$Surv[i]
        }
      }

      # plot the discrete-time conditional survival function by course
      ## geom_step allows for Kaplan-Meier curve

      survival %>%
        ggplot(aes(Period, Survivalp)) +
        geom_point(aes(color = Course)) +
        geom_step(aes(color = Course, group = Course)) +
        scale_y_continuous(limits = c(0, 1), labels = percent) +
        theme_classic() +
        labs(
          x = "Lectures Attended",
          y = "Conditional Survival Probability",
          color = "Course"
        )
    }
  })

  output$calculation <- renderInfoBox({
    
    # create logit to probability conversion function
    
    logit2prob <- function(logit) {
      odds <- exp(logit)
      prob <- odds / (1 + odds)
      return(prob)
    }

    # difference in probability between intercept and intercept + coefficient
    
    intercept <- logit2prob(input$intercept)
    updated <- logit2prob(input$intercept + input$coefficient)
    delta <- updated - intercept

    infoBox(
      "Calculation",
      delta %>%
        percent()
    )
  })

  output$taxonomy <- renderPrint({
    
    # generate logistic regression models
    
    # model 1: unconditional model 
    
    mod1 <- glm(Exit ~ 1, data = data_pp, family = "binomial")
    data_pp <- data_pp %>%
      mutate(pred_m1 = predict(mod1, type = "response"))

    # model 2: exit on period
    
    mod2 <- glm(Exit ~ Period, data = data_pp, family = "binomial")
    data_pp <- data_pp %>%
      mutate(pred_m2 = predict(mod2, type = "response"))

    # model 3: exit on period and course
    
    mod3 <- glm(Exit ~ Period + Course, data = data_pp, family = "binomial")
    data_pp <- data_pp %>%
      mutate(pred_m3 = predict(mod3, type = "response"))

    # model 4: exit on period and gender
    
    mod4 <- glm(Exit ~ Period + Gender, data = data_pp, family = "binomial")
    data_pp <- data_pp %>%
      mutate(pred_m4 = predict(mod4, type = "response"))

    # model 5: exit on period, course, and gender
    
    mod5 <- glm(Exit ~ Period + Course + Gender, data = data_pp, family = "binomial")
    data_pp <- data_pp %>%
      mutate(pred_m5 = predict(mod5, type = "response"))

    # model 6: exit on period, course, gender, and interaction between course and gender
    
    mod6 <- glm(Exit ~ Period + Course * Gender, data = data_pp, family = "binomial")
    data_pp <- data_pp %>%
      mutate(pred_m6 = predict(mod6, type = "response"))

    # generate taxonomy table of fitted models
    
    stargazer(mod1, mod2, mod3, mod4, mod5, mod6,
      type = "text",
      dep.var.labels = "Log-Odds of Dropping Synchronous Lecture Attendance",
      covariate.labels = c("Lecture 2", "Lecture 3", "Lecture 4", "Lecture 5", "Lecture 6", "Lecture 7", "Lecture 8", "Lecture 9", "Lecture 10", "Lecture 11", "Course: S052", "Gender: Male", "Interaction: CoursexGender")
    )
  })

  output$comparison <- renderPrint({
    
    # store fitted models
    
    mod1 <- glm(Exit ~ 1, data = data_pp, family = "binomial")
    mod2 <- glm(Exit ~ Period, data = data_pp, family = "binomial")
    mod3 <- glm(Exit ~ Period + Course, data = data_pp, family = "binomial")
    mod4 <- glm(Exit ~ Period + Gender, data = data_pp, family = "binomial")
    mod5 <- glm(Exit ~ Period + Course + Gender, data = data_pp, family = "binomial")
    mod6 <- glm(Exit ~ Period + Course * Gender, data = data_pp, family = "binomial")

    # conditional statements that determine output given input
    
    if (input$model1 == "mod1" & input$model2 == "mod2") {
      lrtest(mod1, mod2)
    }
    else if (input$model1 == "mod1" & input$model2 == "mod3") {
      lrtest(mod1, mod3)
    }
    else if (input$model1 == "mod1" & input$model2 == "mod4") {
      lrtest(mod1, mod4)
    }
    else if (input$model1 == "mod1" & input$model2 == "mod5") {
      lrtest(mod1, mod5)
    }
    else if (input$model1 == "mod1" & input$model2 == "mod6") {
      lrtest(mod1, mod6)
    }

    else if (input$model1 == "mod2" & input$model2 == "mod1") {
      lrtest(mod1, mod2)
    }
    else if (input$model1 == "mod2" & input$model2 == "mod3") {
      lrtest(mod2, mod3)
    }
    else if (input$model1 == "mod2" & input$model2 == "mod4") {
      lrtest(mod2, mod4)
    }
    else if (input$model1 == "mod2" & input$model2 == "mod5") {
      lrtest(mod2, mod5)
    }
    else if (input$model1 == "mod2" & input$model2 == "mod6") {
      lrtest(mod2, mod6)
    }

    else if (input$model1 == "mod3" & input$model2 == "mod1") {
      lrtest(mod1, mod3)
    }
    else if (input$model1 == "mod3" & input$model2 == "mod2") {
      lrtest(mod2, mod3)
    }
    else if (input$model1 == "mod3" & input$model2 == "mod4") {
      lrtest(mod3, mod4)
    }
    else if (input$model1 == "mod3" & input$model2 == "mod5") {
      lrtest(mod3, mod5)
    }
    else if (input$model1 == "mod3" & input$model2 == "mod6") {
      lrtest(mod3, mod6)
    }

    else if (input$model1 == "mod4" & input$model2 == "mod1") {
      lrtest(mod1, mod4)
    }
    else if (input$model1 == "mod4" & input$model2 == "mod2") {
      lrtest(mod2, mod4)
    }
    else if (input$model1 == "mod4" & input$model2 == "mod3") {
      lrtest(mod3, mod4)
    }
    else if (input$model1 == "mod4" & input$model2 == "mod5") {
      lrtest(mod4, mod5)
    }
    else if (input$model1 == "mod4" & input$model2 == "mod6") {
      lrtest(mod4, mod6)
    }

    else if (input$model1 == "mod5" & input$model2 == "mod1") {
      lrtest(mod1, mod5)
    }
    else if (input$model1 == "mod5" & input$model2 == "mod2") {
      lrtest(mod2, mod5)
    }
    else if (input$model1 == "mod5" & input$model2 == "mod3") {
      lrtest(mod3, mod5)
    }
    else if (input$model1 == "mod5" & input$model2 == "mod4") {
      lrtest(mod4, mod5)
    }
    else if (input$model1 == "mod5" & input$model2 == "mod6") {
      lrtest(mod5, mod6)
    }

    else if (input$model1 == "mod6" & input$model2 == "mod1") {
      lrtest(mod1, mod6)
    }
    else if (input$model1 == "mod6" & input$model2 == "mod2") {
      lrtest(mod2, mod6)
    }
    else if (input$model1 == "mod6" & input$model2 == "mod3") {
      lrtest(mod3, mod6)
    }
    else if (input$model1 == "mod6" & input$model2 == "mod4") {
      lrtest(mod4, mod6)
    }
    else if (input$model1 == "mod6" & input$model2 == "mod5") {
      lrtest(mod5, mod6)
    }

    else {
      print("Please select two different models")
    }
  })

  output$model <- renderPlot({
    
    # generate logistic regression models
    
    # model 1: unconditional model 
    
    mod1 <- glm(Exit ~ 1, data = data_pp, family = "binomial")
    data_pp <- data_pp %>%
      mutate(pred_m1 = predict(mod1, type = "response"))
    
    # model 2: exit on period
    
    mod2 <- glm(Exit ~ Period, data = data_pp, family = "binomial")
    data_pp <- data_pp %>%
      mutate(pred_m2 = predict(mod2, type = "response"))
    
    # model 3: exit on period and course
    
    mod3 <- glm(Exit ~ Period + Course, data = data_pp, family = "binomial")
    data_pp <- data_pp %>%
      mutate(pred_m3 = predict(mod3, type = "response"))
    
    # model 4: exit on period and gender
    
    mod4 <- glm(Exit ~ Period + Gender, data = data_pp, family = "binomial")
    data_pp <- data_pp %>%
      mutate(pred_m4 = predict(mod4, type = "response"))
    
    # model 5: exit on period, course, and gender
    
    mod5 <- glm(Exit ~ Period + Course + Gender, data = data_pp, family = "binomial")
    data_pp <- data_pp %>%
      mutate(pred_m5 = predict(mod5, type = "response"))
    
    # model 6: exit on period, course, gender, and interaction between course and gender
    
    mod6 <- glm(Exit ~ Period + Course * Gender, data = data_pp, family = "binomial")
    data_pp <- data_pp %>%
      mutate(pred_m6 = predict(mod6, type = "response"))
    
    # conditional statements that determine hazard function given input

    if (input$model == "model1") {
      prob <- data_pp %>%
        unique(.) %>%
        ggplot(aes(Period, pred_m1)) +
        geom_point() +
        geom_line() +
        scale_y_continuous(limits = c(0, 0.06), labels = percent) +
        theme_classic() +
        labs(title = "Fitted Hazard Probabilities", x = "Lecture", y = "Hazard Probability")

      logit <- data_pp %>%
        mutate(pred_m1_logit = log(data_pp$pred_m1 / (1 - data_pp$pred_m1))) %>%
        unique(.) %>%
        ggplot(aes(Period, pred_m1_logit)) +
        geom_point() +
        geom_line() +
        theme_classic() +
        labs(title = "Fitted Hazard Logits", x = "Lecture", y = "Hazard Logit")

      prob + logit
    }

    else if (input$model == "model2") {
      prob <- data_pp %>%
        unique(.) %>%
        ggplot(aes(Period, pred_m2)) +
        geom_point() +
        geom_line() +
        scale_y_continuous(limits = c(0, 0.06), labels = percent) +
        theme_classic() +
        labs(title = "Fitted Hazard Probabilities", x = "Lecture", y = "Hazard Probability")

      logit <- data_pp %>%
        mutate(pred_m2_logit = log(data_pp$pred_m2 / (1 - data_pp$pred_m2))) %>%
        unique(.) %>%
        ggplot(aes(Period, pred_m2_logit)) +
        geom_point() +
        geom_line() +
        theme_classic() +
        labs(title = "Fitted Hazard Logits", x = "Lecture", y = "Hazard Logit")

      prob + logit
    }

    else if (input$model == "model3") {
      prob <- data_pp %>%
        unique(.) %>%
        ggplot(aes(Period, pred_m3)) +
        geom_point(aes(color = Course)) +
        geom_line(aes(group = Course, color = Course)) +
        scale_y_continuous(limits = c(0, 0.06), labels = percent) +
        theme_classic() +
        labs(title = "Fitted Hazard Probabilities", x = "Lecture", y = "Hazard Probability")

      logit <- data_pp %>%
        mutate(pred_m3_logit = log(data_pp$pred_m3 / (1 - data_pp$pred_m3))) %>%
        unique(.) %>%
        ggplot(aes(Period, pred_m3_logit)) +
        geom_point(aes(color = Course)) +
        geom_line(aes(group = Course, color = Course)) +
        theme_classic() +
        labs(title = "Fitted Hazard Logits", x = "Lecture", y = "Hazard Logit")

      prob + logit
    }

    else if (input$model == "model4") {
      prob <- data_pp %>%
        unique(.) %>%
        ggplot(aes(Period, pred_m4)) +
        geom_point(aes(color = Gender)) +
        geom_line(aes(group = Gender, color = Gender)) +
        scale_y_continuous(limits = c(0, 0.06), labels = percent) +
        theme_classic() +
        labs(title = "Fitted Hazard Probabilities", x = "Lecture", y = "Hazard Probability")

      logit <- data_pp %>%
        mutate(pred_m4_logit = log(data_pp$pred_m4 / (1 - data_pp$pred_m4))) %>%
        unique(.) %>%
        ggplot(aes(Period, pred_m4_logit)) +
        geom_point(aes(color = Gender)) +
        geom_line(aes(group = Gender, color = Gender)) +
        theme_classic() +
        labs(title = "Fitted Hazard Logits", x = "Lecture", y = "Hazard Logit")

      prob + logit
    }

    else if (input$model == "model5") {
      prob <- data_pp %>%
        unique(.) %>%
        ggplot(aes(Period, pred_m5, color = Gender, shape = Course, group = interaction(Gender, Course))) +
        geom_point() +
        geom_line() +
        scale_y_continuous(limits = c(0, 0.06), labels = percent) +
        theme_classic() +
        labs(title = "Fitted Hazard Probabilities", x = "Lecture", y = "Hazard Probability")

      logit <- data_pp %>%
        mutate(pred_m5_logit = log(data_pp$pred_m5 / (1 - data_pp$pred_m5))) %>%
        unique(.) %>%
        ggplot(aes(Period, pred_m5_logit, color = Gender, shape = Course, group = interaction(Gender, Course))) +
        geom_point() +
        geom_line() +
        theme_classic() +
        labs(title = "Fitted Hazard Logits", x = "Lecture", y = "Hazard Logit")

      prob + logit
    }

    else if (input$model == "model6") {
      prob <- data_pp %>%
        unique(.) %>%
        ggplot(aes(Period, pred_m6, color = Gender, shape = Course, group = interaction(Gender, Course))) +
        geom_point() +
        geom_line() +
        scale_y_continuous(limits = c(0, 0.06), labels = percent) +
        theme_classic() +
        labs(title = "Fitted Hazard Probabilities", x = "Lecture", y = "Hazard Probability")

      logit <- data_pp %>%
        mutate(pred_m6_logit = log(data_pp$pred_m6 / (1 - data_pp$pred_m6))) %>%
        unique(.) %>%
        ggplot(aes(Period, pred_m6_logit, color = Gender, shape = Course, group = interaction(Gender, Course))) +
        geom_point() +
        geom_line() +
        theme_classic() +
        labs(title = "Fitted Hazard Logits", x = "Lecture", y = "Hazard Logit")

      prob + logit
    }
  })
}

shinyApp(ui, server)
