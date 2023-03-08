
library(shiny)
library(tidyverse)
library(reactable)

# Loading the data 
diabetes <- read_delim("diabetes.csv")

# Making UI for the shiny app
ui <- fluidPage(
  titlePanel("Diabetes Analysis"),
  tabsetPanel(
    ############################################################################
    # Describing the UI elements for the 'About' page of the shiny app.
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
    # Describing the UI elements for the 'Underlying Health Conditions' page of
    # the shiny app.
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
    # Describing the UI elements for the 'Smoking and Drinking' page of
    # the shiny app.
    tabPanel("Smoking and Drinking",
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
                                   "smokerPlotColor",
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
                                   strong("Have you smoked at least 100 
                                    cigarettes or 5 packs in your lifetime?")
                                 ),
                                 
                                 uiOutput("smoker")
                          )
                        )
                        
                      ),
                      
                      wellPanel(
                        style = "background-color: #7AA1BF;
                                 border-color: #060000;
                                 height: 75vh",
                        
                        plotOutput("smokerPlot"),
                        
                        textOutput("smokerPlotMessage")
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
                                   "drinkerTableColor",
                                   "",
                                   choices = c(gray = "lightgray",
                                               orange = "darkorange",
                                               green = "lightgreen",
                                               pink = "lightpink",
                                               khaki = "darkkhaki")
                                 )),
                          column(6,
                                 
                                 p(
                                   strong("Do you consume more than 14 drinks
                                          per week as an adult male or 7 drinks 
                                          per week as an adult female?")
                                 ),
                                 
                                 uiOutput("drinker")
                          )
                        )
                      ),
                      wellPanel(
                        style = "background-color: #C0C0C0;
                                 border-color: #060000;
                                 height: 75vh",
                        
                        plotOutput("drinkerPlot"),
                        
                        textOutput("drinkerPlotMessage")
                      )
               )
             )),
    ############################################################################
    # Describing the UI elements for the 'Age Groups' page of the shiny app.
    tabPanel("Age Groups",
             sidebarLayout(
               sidebarPanel(
                 style = "background-color: #C0C0C0;
                   border-color: #060000;
                   height: 100vh",
                 sliderInput(
                   "age",
                   "Select Age Group",
                   min = 1,
                   max = 13,
                   value = 6
                 ),
                 radioButtons(
                   "agePlotColor",
                   "Select Color of Bins:",
                   choices = c(gray = "lightgray",
                               orange = "darkorange",
                               green = "lightgreen",
                               pink = "lightpink",
                               khaki = "darkkhaki")
                 )
               ),
               mainPanel(
                 wellPanel(
                   style = "background-color: #7AA1BF;
                          border-color: #060000;
                          height: 100vh",
                   plotOutput("ageplot"),
                   textOutput("ageplotText")
                 )
               )
             )),
    ############################################################################
    # Describing the UI elements for the 'Conclusion' page of the shiny app.
    tabPanel("Conclusion",
             h2("Conclusion"),
             
             h3("Underlying Health Conditions"),
             p("In this interactive application, we analyzed and evaluated three
               categories of risk factors with respect to the corresponding
               frequency of diabetes in patients who aligned with those 
               categories. In the scope of underlying health conditions, our
               analysis encompassed four conditions: high blood pressure, high
               cholesterol, stroke, and heart disease or heart attack. Upon 
               examination of all four conditions as plotted in correlation to 
               the freqency of diabetes in patients, one can observe relatively
               higher frequencies of diabetes in patients with high blood 
               pressure and in patients with high cholesterol levels. While we
               cannot explicitly attribute these trends to a causal relationship,
               we can conclude a relatively strong and positive correlation
               with diabetes in relation to blood pressure and cholesterol.
               Stroke showed the least correlation to diabetes while the plot
               for heart disease/heart attack illustrated a slightly stronger 
               correlation. This is fairly reasonable, given, high blood pressure
               and high cholesterol are both major risk factors associated with
               heart disease/heart attacks."),
             
             h3("Smoking and Drinking"),
             p("Additionally, we examined the relationship between the frequency
              of diabetes in patients and certain lifestyle aspects, namely, 
              smoking and drinking activity. In this dataset, individuals'
              lifestyle habits are evaluated on a binary scale from 0-1. One is
              considered a heavy drinker if they surpass 14 drinks per week as 
              an adult male or 7 drinks per week as an adult female. For smoking
              if the individual has smoked at least 100 cigarettes or 5 packs in
              their lifetime they are considered a regular smoker. Users may 
              select 1 if the described conditions align with their lifestyle 
              or 0 if they do not. "),
             p("In the visualization for smoking activity and diabetes rates, 
               one can observe that the non-smoker data has a much higher count 
               of non-diabetic patients at slightly under 125,000 patients while 
               the smoker data has a much lower count of non-diabetic patients
               at only around 90,000 patients. This is indicative of a positive 
               correlation between smoking and diabetic rates and possibly 
               suggests that smoking increases risk of diabetes. The CDC
               reports that regular smokers are 30-40% more likely to get type-2
               diabetes than non-smokers. They attribute this risk to the
               rise in blood pressure that nicotine causes."),
             p("In our assessment of drinking activity and diabetes rates, one
               can observe a similar pattern to the trend present in smoking
               activity. In both visualizations, the most significant difference
               in increased smoking and drinking actiivty is that of the
               counts of non-diabetic patients. For individuals with lighter
               alcohol consumption, the count of non-diabetic patients is around 
               200,000 while the count of non-diabetic patients for heavy 
               alcohol consumers falls nearly 25% to roughly 140,000. Much like 
               the smoking data, the drinking data illustrates a positive 
               relationship between alcohol consumption and diabetic rates in 
               patients. The American Diabetes Association advises against heavy
               alcohol consumption as it increases one's risk of getting type-2 
               diabetes stating that excessive drinking can lower the body's 
               sensitivity to insulin."),
             
             h3("Age Groups"),
             p("As age increases, there is an upward trend in the number of
               patients diagnosed with type-2 diabetes. There is also a downward
               trend in non-diabetic patients as the age group increases. Many 
               sources acknowledge a strong positive correlation between age 
               and type-2 diabetes diagnoses. This correlation is commonly
               attributed to the combined effects of increasing insulin
               resistance and impaired pancreatic function with age."),
             
             h3("Broader Implications"),
             p("Data analysis is instrumental in the development and advancement
               of general healthcare. Such information is not only utilized by 
               medical professionals and officials but is also highly valuable
               to the public. The general population may not be equipped with
               the knowledge to interpret data effectively, especially in vast
               amounts. In many cases, relevant terminology is often not in our 
               vernacular, further hindering our comprehension. Simple 
               interactive visualizations such as the ones included in this
               project aids in the accessibility, readability, and functionality
               of data sets such as the one we examined. Healthcare facilities
               may use such applications to educate their patients and encourage
               certain lifestyle changes."),
             
             h3("Data Quality"),
             p("Our data maintains a high level or reasonable quality as it 
               includes several relevant and principal variables that are 
               associated with increased diabetic risk. Amongst the factors 
               evaluated in correspondence with diabetic rates in patients, are 
               those that are representative of biological, socioeconomic, 
               environmental, and lifestyle factors. The data set hollistically
               incorporates these factors on bases of sex and age as well as 
               both income and education. We observed no outstanding bias 
               towards certain groups as the dataset integrates statistics 
               regarding access to healthcare professionals as well as 
               healthcare coverage."),
             
             h3("Future Ideas"),
             p("For future research and development, we would be interested in 
               exploring and implementing a calculator feature that would walk
               the user through a series of questions. The questions would be
               fashioned around the relevant factors included in the dataset.
               Providing a calculator feature would allow users to get a
               personalized analysis of their risk. Hopefully, this feature, 
               would provide users with a more meaningful and straightforward
               experience."),
    )
  )
)

# Making the server function for the shiny app
server <- function(input, output) {
  
  # Reactive data that only contains the Diabetes_012, HighBP, HighChol,
  # Stroke, and HeartDiseaseorAttack columns. It also summarizes the 
  # number of cases for each of those underlying conditions
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
  
  # Makes the list of conditions that the user can choose from
  output$conditions <- renderUI({
    checkboxGroupInput("chooseConditions",
                       "",
                       choices = names(subset(conditionsData(),
                                              select = -c(Diabetes_012)
                       )))
  })
  
  # Gathers the conditionsData into two columns, Conditions and Count. 
  # This way it describes the conditions and the number 
  # of patients corresponding to each in a single place.
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
  
  # Reactive data that includes the data for the type of diabetes chosen by
  # the user 
  tableDf <- reactive({
    diabetes %>%
      filter(Diabetes_012 == strtoi(input$type)) %>%
      select(Diabetes_012,
             HighBP,
             HighChol,
             Stroke, 
             HeartDiseaseorAttack)
  })
  
  # Makes the plot for the conditions chosen by the user
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
  
  # Writes a message on the screen, giving the users the number of conditions
  # chosen by them
  output$conditionsPlotMessage <- renderText({
    paste("Number of conditions considered :",
          length(input$chooseConditions))
  })
  
  # Makes a table that summarizes the number of patients for each condition
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
  
  # Makes a table that summarizes the percentage of patients with prediabetes
  # or diabetes that also have one or more of the underlying conditions.
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
  
  # Writes a message to the screen showing the users the total number of 
  # patients with prediabetes or diabetes
  output$conditionsTableMessage <- renderText({
    displayPatients <- tableDf() %>%
      summarize(Total_Patients = length(Diabetes_012))
    
    paste("Total number of patients: ",
          displayPatients$Total_Patients)
  })
  
  # Makes a plot that describes the number of patients with no diabetes, 
  # prediabetes, and diabetes. 
  output$ageplot <- renderPlot({
    ggplot(data = diabetes[diabetes$Age == input$age, ]) +
      geom_histogram(stat = "count", mapping = aes(x = factor(Diabetes_012)),
                     fill = input$agePlotColor) +
      xlab("Diabetes Type (0 = No Diabetes)") +
      ylab("count")
  })
  
  # Writes a message on the screen that describes the age group chosen by the 
  # user
  output$ageplotText <- renderText({
    if (input$age == 1) {
      ageRange <- "18-24"
    } else if (input$age == 2) {
      ageRange <- "25-29"
    } else if (input$age == 3) {
      ageRange <- "30-34"
    } else if (input$age == 4) {
      ageRange <- "35-39"
    } else if (input$age == 5) {
      ageRange <- "40-44"
    } else if (input$age == 6) {
      ageRange <- "45-49"
    } else if (input$age == 7) {
      ageRange <- "50-54"
    } else if (input$age == 8) {
      ageRange <- "55-59"
    } else if (input$age == 9) {
      ageRange <- "60-64"
    } else if (input$age == 10) {
      ageRange <- "65-69"
    } else if (input$age == 11) {
      ageRange <- "70-74"
    } else if (input$age == 12) {
      ageRange <- "75-79"
    } else if (input$age == 13) {
      ageRange <- "80+"
    }
    paste("Current age group range:", ageRange)
    
  })
  
  # Reactive data that summarizes the number of patients with different stages
  # of diabetes with respect to whether they smoke or not
  smokerData <- reactive({
    diabetes %>%
      filter(Diabetes_012 !=0) %>%
      select(Diabetes_012,
             Smoker) %>%
      group_by(Diabetes_012) %>%
      summarize(Smoker = sum(smoker))
    
  })
  
  # Makes the yes/no choice button to ask the user if they smoke or not
  output$smoker <- renderUI({
    radioButtons('d_smoker', '', 
                 choices = c("No", "Yes")
    )
  })
  
  # Makes the plot for diabetes patients depending on whether the user smokes 
  # or not. 
  output$smokerPlot <- renderPlot({
    
    if (input$d_smoker == "No") {
      smoker01 <- 0.0
    } else if(input$d_smoker == "Yes") {
      smoker01 <- 1.0
    }
    
    ggplot(data = diabetes[diabetes$Smoker == smoker01, ]) +
      geom_histogram(stat = "count", mapping = aes(x = factor(Diabetes_012)),
                     fill = input$smokerPlotColor) +
      xlab("Diabetes Type (0 = No Diabetes)") +
      ylab("count")
  })
  
  # Reactive data that summarizes the number of patients with different stages 
  # of diabetes with respect to whether they drink excessively or not. 
  drinkerData <- reactive({
    diabetes %>%
      filter(Diabetes_012 !=0) %>%
      select(Diabetes_012,
             HvyAlcoholConsump) %>%
      group_by(Diabetes_012) %>%
      summarize(HvyAlcoholConsump = sum(drinker))
    
  })
  
  # Makes the yes/no choice button to ask the user if they drink excessively
  # or not.
  output$drinker <- renderUI({
    radioButtons('d_drinker', '', 
                 choices = c("No", "Yes")
    )
  })
  
  output$drinkerPlot <- renderPlot({
    
    if (input$d_drinker == "No") {
      drinker01 <- 0.0
    } else if(input$d_drinker == "Yes") {
      drinker01 <- 1.0
    }
    
    ggplot(data = diabetes[diabetes$HvyAlcoholConsump == drinker01, ]) +
      geom_histogram(stat = "count",mapping = aes(x = factor(Diabetes_012)),
                     fill = input$drinkerTableColor) +
      xlab("Diabetes Type (0 = No Diabetes)") +
      ylab("count")
  })
  
}

shinyApp(ui = ui, server = server)