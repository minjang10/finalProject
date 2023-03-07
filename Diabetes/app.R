library(shiny)
library(tidyverse)
library(reactable)

diabetes <- read_delim("diabetes.csv")

ui <- fluidPage(
  tabsetPanel(
    ############################################################################
    tabPanel("About",
             
             fluidRow(
               column(8,
                      wellPanel(
                        style = "background-color: #C0C0C0;
                                 border-color: #060000;
                                 height: 100vh;
                                 font-size: 20px",
                        
                        h2("About Type II Diabetes"),
                        
                        p("Type II diabetes, generally called diabetes, is
                           one of the most widespread chronic diseases in
                           the United States. It impacts millions of
                           Americans every year."),
                        
                        p("Diabetes is a chronic disease in which an
                           individual is unable to effectively regulate
                           the levels of glucose in their blood. Diabetes
                           is characterized by the body's inability to
                           create enough insulin or its inability to
                           effectively use the insuline to regulate blood
                           sugar."
                        ),
                        
                        p("In this project, we aim to analyze the frequency
                           of diabetes in patients with respect to ",
                          em("Underlying Health Conditions,"),
                          em("Smoking and Drinking Habits,"),
                          "and ",
                          em("Age Groups"),
                          "."
                        )
                      )
                      ),
               
               column(4,
                      wellPanel(
                        style = "background-color: #7AA1BF;
                                 border-color: #060000;
                                 height: 100vh;
                                 font-size: 20px",
                        h2("The Data"),
                        
                        p("The data used for this project has been taken
                           from the ",
                          a("Kaggle Diabetes Health Indicator Dataset",
                            href =
                              "https://www.kaggle.com/datasets/alexteboul/diabetes-health-indicators-dataset"),
                            ". The data includes the responses of 253,680 individuals
                            who took the",
                          strong("Behavioral Risk Factor Surveillance
                                  System (BRFSS) survey"),
                            "in 2015."
                        )
                      ))
             )),
    ############################################################################
    tabPanel("Underlying Health Conditions",
             fluidRow(
               column(6,
                      wellPanel(
                        style = "background-color: #7AA1BF;
                                 border-color: #060000;
                                 height: 25vh",
                        fluidRow(
                          column(6,
                                 p(
                                   strong("To change the background color of the
                                           plot, choose a color from the list:")
                                 ),
                                 
                                 radioButtons(
                                   "conditionsPlotColor",
                                   "",
                                   choices = c(gray = "lightgray",
                                               orange = "darkorange",
                                               green = "lightgreen",
                                               pink = "lightpink",
                                               khaki = "darkkhaki")
                                 )
                                 ),
                          column(6,
                                 p(
                                   strong("Select the underlying conditions you
                                          wish to include on the plot:")
                                 ),
                                 
                                 uiOutput("conditions")
                                 )
                                 )
                          
                        ),
                        
                      wellPanel(
                        style = "background-color: #7AA1BF;
                                 border-color: #060000;
                                 height: 75vh",
                        
                        plotOutput("conditionsPlot"),
                        
                        textOutput("conditionsPlotMessage")
                      )
                      ),
               
               column(6,
                      wellPanel(
                        style = "background-color: #C0C0C0;
                                 border-color: #060000;
                                 height: 25vh",
                        fluidRow(
                          column(6,
                                 
                                 p(
                                   strong("To change the background color of the
                                          tables, choose a color from the list")
                                 ),
                                 
                                 radioButtons(
                                   "conditionsTableColor",
                                   "",
                                   choices = c(gray = "lightgray",
                                               orange = "darkorange",
                                               green = "lightgreen",
                                               pink = "lightpink",
                                               khaki = "darkkhaki")
                                 )),
                          column(6,
                                 
                                 p(
                                   strong("Select the stage of diabetes you wish
                                          to make a table for:")
                                 ),
                                 
                                 radioButtons("type",
                                              "",
                                              choices = c(Prediabetes = "1",
                                                          diabetes = "2")
                                              )
                                 )
                        )
                      ),
                      wellPanel(
                        style = "background-color: #C0C0C0;
                                 border-color: #060000;
                                 height: 75vh",
                        
                        p(
                          strong("The table below shows the number of patients
                                 with each underlying condition based on the 
                                 stage of diabetes select by you:")
                        ),
                        
                        reactableOutput("conditionsTable1"),
                        
                        p(
                          strong("The table below shows the percentage of
                                 patients with each underlying condition based
                                 on the stage of diabetes selected by you:")
                        ),
                        
                        reactableOutput("conditionsTable2"),
                        
                        textOutput("conditionsTableMessage")
                      )
                      )
             )),
    ############################################################################
    tabPanel("Smoking and Drinking"),
    ############################################################################
    tabPanel("Age Groups",
             fluidRow(
               column(6,
                      wellPanel(
                        style = "background-color: #7AA1BF;
                                 border-color: #060000;
                                 height: 25vh",
                        fluidRow(
                          column(6,
                                 p(
                                   strong("The interactive graphic below shows
                                          the risk of diabetes corresponding to 
                                          the selected age group:")
                                 ),
  )
)

server <- function(input, output) {
  
  conditionsData <- reactive({
    diabetes %>%
      filter(Diabetes_012 != 0) %>%
      select(Diabetes_012,
             HighBP,
             HighChol,
             Stroke, 
             HeartDiseaseorAttack) %>%
      group_by(Diabetes_012) %>%
      summarize(High_BP = sum(HighBP),
                High_Chol = sum(HighChol),
                Stroke = sum(Stroke),
                Heart_Disease_or_Attack = sum(HeartDiseaseorAttack))
    })
    
    output$conditions <- renderUI({
      checkboxGroupInput("chooseConditions",
                         "",
                         choices = names(subset(conditionsData(),
                                                select = -c(Diabetes_012)
                                                )))
    })
    
    sample <- reactive({
      if(is.null(input$chooseConditions)) {
        s1 <- data.frame(matrix(ncol = 1, nrow = 0))
      } else {
        s1 <- conditionsData() %>%
          gather(., 
                 key = "Conditions",
                 value = "Count",
                 all_of(input$chooseConditions))
      }
    })
    
    tableDf <- reactive({
      diabetes %>%
        filter(Diabetes_012 == strtoi(input$type)) %>%
        select(Diabetes_012,
               HighBP,
               HighChol,
               Stroke, 
               HeartDiseaseorAttack)
    })
    
    output$conditionsPlot <- renderPlot({
      if(nrow(sample()) == 0) {
        p <- sample() %>%
          ggplot(aes()) +
          geom_blank() +
          theme(
            plot.background = element_rect(fill = input$conditionsPlotColor),
            text = element_text(size = 15, color = "black"),
            axis.text = element_text(size = 13, color = "black")
          ) +
          ggtitle("Please select at least one condition")
        
        p
      }
      
      else {
        p <- sample() %>%
          ggplot(aes(Conditions, Count)) +
          geom_bar(stat = "identity",
                   aes(fill = factor(Diabetes_012)),
                   position = "dodge") +
          coord_flip() +
          theme(
            plot.background = element_rect(fill = input$conditionsPlotColor),
            text = element_text(size = 15, color = "black"),
            axis.text = element_text(size = 13, color = "black")
          ) +
          ggtitle("Number of prediabetic(1) and diabetic(2) patients for
                  each condition")
        
        p
      }
    })
    
    output$conditionsPlotMessage <- renderText({
      paste("Number of conditions considered :",
            length(input$chooseConditions))
    })
    
    output$conditionsTable1 <- renderReactable({
      options(
        reactable.theme = reactableTheme(
          backgroundColor = input$conditionsTableColor,
          borderColor = "black",
          borderWidth = "2px"
        )
      )
      
      reactable(
        tableDf() %>%
          summarize(
            High_BP = sum(HighBP),
            High_Chol = sum(HighChol),
            Stroke = sum(Stroke),
            Heart_Disease_or_Attack = sum(HeartDiseaseorAttack)
          )
      )
    })
    
    output$conditionsTable2 <- renderReactable({
      options(
        reactable.theme = reactableTheme(
          backgroundColor = input$conditionsTableColor,
          borderColor = "black",
          borderWidth = "2px"
        )
      )
      
      reactable(
        tableDf() %>%
          summarize(
            High_BP = round((sum(HighBP) / length(Diabetes_012)) * 100, 2),
            High_Chol = round((sum(HighChol) / length(Diabetes_012)) * 100, 2),
            Stroke = round((sum(Stroke) / length(Diabetes_012)) * 100, 2),
            Heart_Disease_or_Attack = round((sum(HeartDiseaseorAttack) /
                                               length(Diabetes_012)) * 100, 2)
          )
      )
    })
    
    output$conditionsTableMessage <- renderText({
      displayPatients <- tableDf() %>%
        summarize(Total_Patients = length(Diabetes_012))
      
      paste("Total number of patients: ",
            displayPatients$Total_Patients)
    })
  }

  shinyApp(ui = ui, server = server)