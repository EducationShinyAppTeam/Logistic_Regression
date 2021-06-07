library(shiny)
library(shinydashboard)
library(png)
library(shinyBS) 
library(V8)
library(shinyjs) 

#library(discrimARTs)
library(leaflet)
library(raster)
library(DT)

library(RColorBrewer) 
#library(car)
#library(rgdal)
library(shinyWidgets)
library(ggplot2)
library(plotly)
library(rlocker)

library(dplyr)
library(shinycssloaders)

source("helpers.R") 

ui <- dashboardPage(
  skin = 'yellow',
  dashboardHeader(title = "Logistic Regression",
                  tags$li(class="dropdown",
                          actionLink("info", icon("info"), class="myClass")),
                  tags$li(class = "dropdown",
                          boastUtils::surveyLink(name = "App_Template")),
                  tags$li(class="dropdown",
                          tags$a(href="https://shinyapps.science.psu.edu/",
                                 icon("home", lib="font-awesome"))),
                  titleWidth = 200),
  
  #adding prereq pages and game pages
  dashboardSidebar(
    width = 250,
    sidebarMenu(
      id = "pages",
      menuItem("Prerequisites", tabName = "prereq", icon = icon("book")),
      menuItem("Overview",tabName = "instruction", icon = icon("dashboard")),
      menuItem("Explore",tabName = "explore", icon = icon("wpexplorer")),
      menuItem("Game", tabName = "qqq", icon = icon("gamepad")),
      menuItem("References", tabName = "references", icon = icon("leanpub"))
    ),
    
    tags$div(
      style = "position: absolute; bottom: 0;",
      class = "sidebar-logo",
      boastUtils::sidebarFooter()
    )
  ),
  
  ####################### Button and slider bar color edits ######################################
  ######### Could be combined but left separate so easily understood#####################
  
  dashboardBody(
    tags$head(
      tags$style(HTML('#go{background-color: #ffa500')),
      tags$style(HTML('#go1{background-color: #ffa500')),
      tags$style(HTML('#go2{background-color: #ffa500')),
      tags$style(HTML('#start{background-color: #ffa500')),
      tags$style(HTML('#submit{color: white')),
      tags$style(HTML('#goMul{background-color: #ffa500')),
      tags$style(HTML('#goButton{background-color: #ffa500')),
      tags$style(HTML('#goButtonMul{background-color: #ffa500')),
      tags$style(HTML('#submitD{background-color: #ffa500')),
      tags$style(HTML('#start{border-color:#ffa500')),
      tags$style(HTML('#go{border-color: #ffa500')),
      tags$style(HTML('#go1{border-color: #ffa500')),
      tags$style(HTML('#go2{border-color: #ffa500')),
      tags$style(HTML('#goMul{border-color: #ffa500')),
      tags$style(HTML('#goButton{border-color: #ffa500')),
      tags$style(HTML('#goButtonMul{border-color: #ffa500')),
      tags$style(HTML('#submitD{border-color: #ffa500')),
      tags$style(HTML('#begin{background-color: #ffa500')),
      tags$style(HTML('#begin{border-color: #ffa500')),
      tags$style(HTML('#challenge{background-color: #ffa500')),
      tags$style(HTML('#challenge{border-color: #ffa500')),
      tags$style(HTML('#answer{background-color: #ffa500')),
      tags$style(HTML('#answer{border-color: #ffa500')),
      tags$style(HTML('#submit{background-color: #ffa500')),
      tags$style(HTML('#restart{background-color: #ffa500')),
      tags$style(HTML('#nextq{background-color: #ffa500')),#D01C1C
      tags$style(HTML('#submit{border-color: #ffa500')),
      tags$style(HTML('#nextq{border-color: #ffa500')),
      tags$style(HTML('#restart{border-color: #ffa500')),
      tags$style(HTML('#nextButton{background-color: #ffa500')),
      tags$style(HTML('#nextButton{border-color: #ffa500')),
      tags$style(HTML('#reset{background-color: #ffa500')),
      tags$style(HTML('#reset{border-color: #ffa500')),
      tags$style(HTML(".js-irs-0 .irs-single, .js-irs-0 .irs-bar-edge, .js-irs-0 .irs-bar {background: #ffc04d}")),
      tags$style(HTML(".js-irs-1 .irs-single, .js-irs-1 .irs-bar-edge, .js-irs-1 .irs-bar {background: #ffc04d")),
      tags$style(HTML(".js-irs-2 .irs-single, .js-irs-2 .irs-bar-edge, .js-irs-2 .irs-bar {background: #ffc04d")),
      tags$style(HTML(".js-irs-3 .irs-single, .js-irs-3 .irs-bar-edge, .js-irs-3 .irs-bar {background: #ffc04d")),
      tags$style(HTML(".js-irs-4 .irs-single, .js-irs-4 .irs-bar-edge, .js-irs-4 .irs-bar {background: #ffc04d")),
      tags$style(HTML(".js-irs-5 .irs-single, .js-irs-5 .irs-bar-edge, .js-irs-5 .irs-bar {background: #ffc04d")),
      tags$style(HTML(".js-irs-6 .irs-single, .js-irs-6 .irs-bar-edge, .js-irs-6 .irs-bar {background: #ffc04d")),
      tags$style(HTML(".js-irs-7 .irs-single, .js-irs-7 .irs-bar-edge, .js-irs-7 .irs-bar {background: #ffc04d")),
      tags$style(HTML(".js-irs-8 .irs-single, .js-irs-8 .irs-bar-edge, .js-irs-8 .irs-bar {background: #ffc04d")),
      tags$style(HTML(".js-irs-0 .irs-single, .js-irs-0 .irs-bar-edge, .js-irs-0 .irs-bar {border-color: #ffc04d")),
      tags$style(HTML(".js-irs-1 .irs-single, .js-irs-1 .irs-bar-edge, .js-irs-1 .irs-bar {border-color: #ffc04d")),
      tags$style(HTML(".js-irs-2 .irs-single, .js-irs-2 .irs-bar-edge, .js-irs-2 .irs-bar {border-color: #ffc04d")),
      tags$style(HTML(".js-irs-3 .irs-single, .js-irs-3 .irs-bar-edge, .js-irs-3 .irs-bar {border-color: #ffc04d")),
      tags$style(HTML(".js-irs-4 .irs-single, .js-irs-4 .irs-bar-edge, .js-irs-4 .irs-bar {border-color: #ffc04d")),
      tags$style(HTML(".js-irs-5 .irs-single, .js-irs-5 .irs-bar-edge, .js-irs-5 .irs-bar {border-color: #ffc04d")),
      tags$style(HTML(".js-irs-6 .irs-single, .js-irs-6 .irs-bar-edge, .js-irs-6 .irs-bar {border-color: #ffc04d")),
      tags$style(HTML(".js-irs-7 .irs-single, .js-irs-7 .irs-bar-edge, .js-irs-7 .irs-bar {border-color: #ffc04d")),
      tags$style(HTML(".js-irs-8 .irs-single, .js-irs-8 .irs-bar-edge, .js-irs-8 .irs-bar {border-color: #ffc04d"))
    ),
    tabItems(
      #Adding pre-requisites page to remove background from instructions page
      
      tabItem(tabName="prereq",
              h3(strong("Logistic Regression Analysis")),
              br(),
              h4(tags$li("The logistic regression model explains the relationship 
                                                between one (or more) explanatory variable and the binary outcome.")),
              
              br(),
              withMathJax(),
              h4(tags$li("In the logistic regression the constant \\(\\beta_0\\)
                                                moves the curve left and right and the slope
                                                \\(\\beta_1\\) defines the steepness of the curve.")),
              div(style="font-size: 1.6em", helpText('$${ln({p\\over1-p})} = {\\beta_0+\\beta_1x}$$')),
              # h4(tags$li("Empirical logit plot is used to check the linearity for datasets")),
              # div(style="font-size: 1.6em", helpText('$$logit ( \hat {p} )=log(\frac{\hat p}{1-\hat p})$$')),
              withMathJax(),
              h4(tags$li("Empirical logit plot is used to check the linearity for datasets.")),
              div(style="font-size: 1.6em", helpText('$$ {logit ( \\hat p )=log({ \\hat p\\over1-\\hat p})}$$')),
              h4(tags$li("Deviance Residual and Pearson Residual check the model fit. Best 
                                                results are no patterns or no extremely large residuals ")),
              h4(tags$li("Hosmer and Lemeshow test check the goodness of fit in the model 
                                                where data is divided into recommended 10 groups. The p-value can 
                                                determine the significance of the result.")),
              br(),
              h4(tags$li("Hosmer-Lemeshow Test Statstics")),
              div(style="font-size: 1.6em", helpText('$${\\sum_{i=1}^g}{\\sum_{j=1}^2}{{(obs_{ij} - exp_{ij})^2} 
                                                                            \\over exp_{ij}}$$')),
              br(),
              br(),
              div(style = "text-align: center",bsButton("start","Go to the overview",
                                                        icon("bolt"),style = "danger",
                                                        size = "large",class="circle grow"))
              
      ),
      
      tabItem(tabName = "instruction",
              
              tags$a(href='http://stat.psu.edu/',tags$img(src='logo.png', align = "left", width = 180)),
              br(),br(),br(),
              h3(strong("About:")),
              h4("This app allows you to explore how different factors can affect the outcome of the Logistic Regression Model and Empirical Logit Plot."),
              br(),
              h3(strong("Instructions:")),
              h4(tags$li("This app includes Single Logistic Regression with simulated data and the Empirical Logit Plot with real datasets.")),
              #h4(tags$li("For each model, adjust the sliders to change the sample size and corresponding beta coefficients.")),
              h4(tags$li("Click New Sample button to generate plot. Watch the change of plot when drag the slider of confidence interval.")),
              h4(tags$li("In Empirical Logit Plot, select interested predictors from the menu and see how the plot changes accordingly")),
              h4(tags$li("After working with the Explore section, you can start the game to test your understanding of the concepts.")),
              h4(tags$li("Practice the questions in Game Section. For each question you get right, you would get a chance to roll the dice.")),
              h4(tags$li("If the cumulative total for your dice roll reaches 20 within 10 questions, YOU WIN!")),
              br(),
              div(style = "text-align: center",
                  bsButton(inputId = "go", label =  "Explore", icon("bolt"), style= "danger", size= "large", class='circle grow')
              ),
              
              br(),
              h3(strong("Acknowledgements:")),
              h4("This app was developed and coded by Yiyun Gong and Ruisi Wang."),
              
              br(),
              h3(strong("About the data:")),
              h4("The datasets and the procedures for the empirical logit plot are adopted from Stat2: Models for a World of Data 
                                       by Cannon, Cobb, Hartlaub, Legler, Lock, Moore, Rossman, and Witmer.  ")
      ),
      
      
      
      tabItem(tabName = "explore",
              
              # div(style="display: inline-block;vertical-align:top;",
              #     tags$a(href='https://shinyapps.science.psu.edu/',tags$img(src='homebut.PNG', width = 19)),
              #     circleButton("infoex",icon = icon("info"), status = "myClass",size = "xs")
              # ),
              
              tabsetPanel(
                type = 'tabs',
                tabPanel(
                  ######Single Regression
                  'Single Logistic Regression',
                  # h4(tags$li("For each model, ")),
                  # h4(tags$li("")),
                  # h4(tags$li("Each Logistic Regression plot is made on a random sample.")),
                  # h4(tags$li("After working with the explore section, you can start the game to test your understanding of the concepts.")),
                  h3(strong("Single Logistic Regression")),
                  h4(tags$li("Adjust the sliders to change the sample size and corresponding 
                                                 beta coefficients.")),
                  h4(tags$li("Click 'New Sample' button to generate plot.")),
                  br(),
                  
                  sidebarLayout(
                    sidebarPanel(
                      sliderInput(
                        inputId = "sampleSize", 
                        label = "Set sample size:",
                        min = 2, 
                        max = 300, 
                        value = 150, 
                        step = 1
                      ),
                      
                      sliderInput(
                        inputId = "b0", 
                        label = "β0 (intercept):",
                        min = -10, 
                        max = 10, 
                        value = 0
                      ),
                      
                      sliderInput(
                        inputId = "b1", 
                        label = "β1 (coefficient):",
                        min = -10, 
                        max = 10, 
                        value = 3
                      ),
                      
                      # sliderInput("ci", "confidence interval level",
                      #             min = 0, max = 0.999, value = 0.95),
                      
                      sliderInput(
                        inputId = "ci", 
                        label = "confidence interval level:",
                        min = 0, 
                        max = 0.99, 
                        value = 0.95, 
                        step = 0.01,  
                      ),
                      
                      selectInput(
                        inputId="residualType", 
                        label = "Residual Type",
                        choices = c("deviance", "pearson"),
                        selected="deviance"),
                      
                      br(),
                      actionButton("goButton", "New Sample", icon("paper-plane"),
                                   class = "btn btn-lg", style="color: #fff", class="circle grow"),
                      br()
                      
                    ),
                    mainPanel(
                      plotlyOutput("logplot", width = "98%", height = "300px")%>% withSpinner(color="#ffa500"),
                      
                      br(),
                      tableOutput("citable"),
                      plotOutput("residualPlot", width = "100%",height = "330px")%>% withSpinner(color="#ffa500"),
                      tags$style(type='text/css', '#lemeshowTest, #obsexp {background-color: rgba(249, 105, 14, 1);  
                                                      color: yellow; text-align: center}', '#title{color: blackl; padding-left:2.5em; font-size: 22px}'), 
                      #rgba(219,193,195,0.20)
                      br(),
                      h3(strong(id='title', "Hosmer and Lemeshow goodness of fit test")),
                      #br(),
                      tableOutput("lemeshowDF"),
                      tableOutput("obsexpDF"),
                      #verbatimTextOutput("lemeshowTest"),
                      #verbatimTextOutput("obsexp"),
                      bsPopover("lemeshowDF"," ","The Hosmer-Lemeshow Test is a goodness of fit test for the logistic model. Here is the result of the Hosmer-Lemeshow Test for ten groups. Number of subgroups, g, usually uses the formula g > P + 1. P is number of covariates. Degree of freedom equals g-2. ", trigger = "hover",placement = "left"),
                      bsPopover("obsexpDF"," ","There are 10 rows meaning g=10.", trigger = "hover",placement = "left")
                    )
                  ),
                  #set continue button
                  div(style = "text-align: center",
                      bsButton(inputId = "go1", label =  "Play the game!", icon("bolt"), style= "danger", size= "large", class='circle grow')
                  )
                ),
                
                tabPanel(
                  ###Emperical Logit Plot
                  'Emperical Logit Plot',
                  h3(strong("Emperical Logit Plot")),
                  #h4(tags$li("Adjust the sliders to change the sample size and corresponding 
                  #           beta coefficients.")),
                  #h4(tags$li("Click 'New Sample' button to generate plot.")),
                  # fluidRow(column(width=12,                                        wellPanel(
                  #   style = "background-color: #ffb6c1",
                  #   tags$div(
                  #     h4("Process of creating an empirical logit plot for quantitative predictors"),
                  #     tags$ul(
                  #       tags$li("Divide the range of the predictor into intervals with roughly equal numbers of cases."),
                  #       tags$li("Compute the mean value of the predictor and the empirical logit for each interval."),
                  #       tags$li("Plot logit versus the mean value of the predictor, with one point for each interval.")
                  #     ),
                  #     style = "background-color: #ffb6c1")))),
                  
                  #                                        box(width =4,background = "maroon", 
                  #                                            title = "Process of creating an empirical logit plot for quantitative predictors", "
                  # 1) Divide the range of the predictor into intervals with roughly equal numbers of cases. 
                  # 2) Compute the mean value of the predictor and the empirical logit for each interval. 
                  # 3) Plot logit versus the mean value of the predictor, with one point for each interval."),
                  #   
                  
                  h3("Process of creating an empirical logit plot for quantitative predictors"),
                  h4("1. Divide the range of the predictor into intervals with roughly equal numbers of cases."),
                  h4("2. Compute the mean value of the predictor and the empirical logit for each interval."),
                  h4("3. Plot logit versus the mean value of the predictor, with one point for each interval."),
                  br(),
                  #                                        box(width = 6, background = "maroon", title = "How many intervals should we use for the Empirical Logit Plot?",
                  #                                            "Two intervals give you a sense of the direction and size of the relationship.
                  # Three intervals give you an indication of departures from linearity. 
                  # Four or five intervals is better when you have enough cases. "),
                  #                                        br(),
                  
                  
                  
                  sidebarLayout(
                    sidebarPanel(
                      ####select datasets
                      selectInput(inputId="datatable", label="Select Dataset:", 
                                  choices= c('MedGPA', 'Titanic', 'Leukemia'), 
                                  selected = 'MedGPA'),
                      
                      ####variable options for 'MedGPA' dataset
                      conditionalPanel(
                        condition = "input.datatable == 'MedGPA'",
                        selectInput(inputId="MedYvar", label="Select Response Variable Y",
                                    choices = c("Acceptance"),
                                    selected = 'Acceptance'),
                        selectInput(inputId="MedXvar", label="Select Quantitative Predictor X",
                                    choices = c("GPA", "MCAT", "BCPM"),
                                    selected = 'GPA')
                      ),
                      
                      ####variable option for 'Titanic' dataset
                      conditionalPanel(
                        condition = "input.datatable == 'Titanic'",
                        selectInput(inputId="TitanicYvar", label="Select Response Variable Y",
                                    choices = c("Survived"),
                                    selected = 'Survived'),
                        selectInput(inputId="TitanicXvar", label="Select Quantitative Predictor X",
                                    choices = c("Age"),
                                    selected = 'Age')
                      ),
                      
                      ####variable option for 'Leukemia' dataset
                      conditionalPanel(
                        condition = "input.datatable == 'Leukemia'",
                        selectInput(inputId="LeukemiaYvar", label="Select Response Variable Y",
                                    choices = c("Status"),
                                    selected = 'Status'),
                        selectInput(inputId="LeukemiaXvar", label="Select Quantitative Predictor X",
                                    choices = c("Blasts", "Age", "Infil(perceptage of infiltrate)"),
                                    selected = "Blasts")
                      ),
                      
                      ###number of groups
                      sliderInput("ngroups", "Number of Groups (Intervals):",
                                  min = 2, max = 8,
                                  value = 4, step = 1),
                      
                      br()
                      
                    ),
                    mainPanel(
                      plotOutput("empericalLogitPlot", width = "100%")%>% withSpinner(color="#ffa500")
                    )
                  )
                )
                
                # tabPanel("Multiple Logistic Regression",
                #          h3(strong("Multiple Logistic Regression")),
                #          h4(tags$li("Adjust the sliders to change the sample size and corresponding 
                #             beta coefficients.")),
                #          h4(tags$li("After working with the explore section, you can start the game to test your understanding.")),
                #          br(),
                #          
                #          sidebarLayout(
                #            sidebarPanel(
                #              sliderInput2("sampleSize2", "Sample Size:",
                #                           min = 0, 
                #                           max = 300, 
                #                           value = 150, 
                #                           step = 1, 
                #                           from_min = 10
                #              ),
                #              sliderInput("b02", "β0 (intercept):",
                #                          min = -10, max = 10, value = 2
                #              ),
                #              sliderInput("b12", "β1 (coefficient):",
                #                          min = -10, max = 10, value = 8
                #              ),
                #              sliderInput("b2", "β2 (coefficient):",
                #                          min = -10, max = 10, value = -8
                #              ),
                #              sliderInput3("ci2", "confidence interval level:",
                #                           min = 0, 
                #                           max = 1, 
                #                           value = 0.95, 
                #                           step = 0.01,  
                #                           from_max = 0.99
                #              ),
                #              
                #              br(),
                #              actionButton("goButtonMul", "New Sample", icon("paper-plane"),
                #                                               class = "btn btn-lg", style="color: #fff", class="circle grow"),
                #              br(),
                #              br(),
                #              bsButton(inputId = "begin", label="Game Time!", icon("gamepad"), 
                #                       class='btn btn-lg', style= "danger", class="circle grow")
                #            ),
                #            
                #            mainPanel(
                #              plotOutput("emplogit
                #                         ")%>% withSpinner(color="#ffb6c1"),
                #              br(),
                #              br(),
                #              plotlyOutput("mulPlot", height = "300px")%>% withSpinner(color="#ffb6c1"),
                #              br(),
                #              br(),
                #              plotOutput("multix")%>% withSpinner(color="#ffb6c1")
                #              
                #              # br(),
                #              # tags$style(type='text/css', '#lemeshowTest2, #obsexp2 {background-color: rgba(219,193,195,0.20); 
                #              #            color: maroon;text-align: center}'), 
                #              # # br(),
                #              # div(style="text-align: center", h3(id='title', "Hosmer and Lemeshow goodness of fit (GOF) test")),
                #              # br(),
                #              # tableOutput("lemeshowDF2"),
                #              # tableOutput("obsexpDF2")
                #              )
                #          )
                #          )
              )
      ),
      
      ##Game page
      tabItem(tabName = "qqq",
              h2(strong("Game Section")),
              # wellPanel(
              #   style = "background-color: #ffd0d7; border:1px solid #ffb6c1",
              #   tags$li("Practice the following questions. For each question you get right, you would get a chance to roll the dice."),
              #   tags$li("If the cumulative total for your dice roll reaches 20 within 10 questions, YOU WIN!"),
              # ),
              br(),
              # h3(strong("Problems")),
              sidebarLayout(
                sidebarPanel(
                  id="sidebar",
                  tags$head(tags$style(
                    HTML("#sidebar{background-color:ffa500; border:1px solid #ffa500}")
                  )),
                  width = 6,
                  uiOutput("question"),
                  uiOutput("options"),
                  br(),
                  
                  selectInput("answer", "Select your answer from below", c("","A", "B", "C")),
                  uiOutput("mark"),
                  br(),
                  uiOutput("Feedback"),
                  br()
                ),
                
                mainPanel(
                  width = 6,
                  br(),
                  tags$head(tags$style(HTML(mycss))),
                  fluidRow(
                    column(12, align="center", uiOutput('gamescore')),
                    column(12, align="center", div(id = "plot-container",
                                                   tags$img(src = "spinner.gif",
                                                            id = "loading-spinner"),
                                                   uiOutput("dice", width = "100%")
                    ))
                  ),
                  
                  # br(),
                  # fluidRow(
                  #   # column(6, actionButton("roll", "roll")),
                  #   # column(12, align="center", actionButton("stop", "stop"))
                  #   # column(5, align="left", bsButton("restart", "restart", style="danger", disabled = TRUE))
                  #   ),
                  # br(),
                  br()
                )
              ),
              fluidRow(
                column(6, align="center",
                       div(style="display: inline-block", actionButton(inputId = 'submit', label = 'Submit')),
                       div(style="display: inline-block;vertical-align:top; width: 30px;",HTML("<br>")),
                       div(style="display: inline-block", bsButton(inputId = "nextq",label = "Next", style='danger', disabled = TRUE)),
                       div(style="display: inline-block;vertical-align:top; width: 30px;",HTML("<br>")),
                       div(style="display: inline-block", bsButton(inputId = "restart",label = "Restart", style="danger"))
                       
                       
                )
              ),
              width = 300,
              
              div(style = "text-align: right",
                  bsButton(inputId = "go2", label =  "Continue", icon("bolt"), style= "danger", size= "large", class='circle grow')
              )
      ),
      
      #References page
      tabItem(
        tabName = "references",
        withMathJax(),
        h2("References"),
        p(
          class = "hangingindent",
          "Chang, W. and Borges Ribeio, B. (2018). shinydashboard: Create
                             dashboards with 'Shiny', R Package. Available from
                             https://CRAN.R-project.org/package=shinydashboard"
        ),
        
        p(
          class = "hangingindent",
          "Wickham, H. (2011), “The Split-apply-combine strategy for data 
                             analysis.” Journal of Statistical Software, 40, pp. 1-29. 
                             Available at http://www.jstatsoft.org/v40/i01/."
        ),
        
        p(
          class = "hangingindent",
          "Carey, R. (2019). boastUtils: BOAST Utilities, R Package.
                             Available from https://github.com/EducationShinyAppTeam/boastUtils"
        )
      ) #end of tabItem
    )
  )
)

###server
# Define server logic required to draw a histogram
server <- function(input, output,session) {
  
  # Learning Locker Statement Generation
  .generateStatement <- function(session, verb = NA, object = NA, description = NA, value = NA) {
    if (is.na(object)) {
      object <- paste0("#shiny-tab-", session$input$pages)
    } else {
      object <- paste0("#", object)
    }
    
    stmt <- list(
      verb = verb,
      object = list(
        id = paste0(boastUtils::getCurrentAddress(session), object),
        name = paste0(APP_TITLE),
        description = description
      )
    )
    
    if (!is.na(value)) {
      stmt$result <- list(
        response = paste(value)
      )
    }
    
    statement <- rlocker::createStatement(stmt)
    response <- rlocker::store(session, statement)
    
    return(response)
  }
  
  .generateAnsweredStatement <- 
    function(session, 
             verb = NA, 
             object = NA, 
             description = NA, 
             interactionType = NA, 
             response = NA, 
             success = NA, 
             completion = FALSE) {
      statement <- rlocker::createStatement(list(
        verb = verb,
        object = list(
          id = paste0(getCurrentAddress(session), "#", object),
          name = paste0(APP_TITLE),
          description = paste0("Identify the distribution of given text: ", description),
          interactionType = interactionType
        ),
        result = list(
          success = success,
          response = response,
          completion = completion
        )
      ))
      
      return(rlocker::store(session, statement))
    }
  
  ##########################Go buttons##################################### 
  observeEvent(input$infoex,{
    sendSweetAlert(
      session = session,
      title = "Instructions:",
      text = " Move the sliders to see their effect on the diagnostic plots.",
      type = NULL
    )
  })
  
  observeEvent(input$info,{
    sendSweetAlert(
      session = session,
      title = "Instructions:",
      text = "This app explores Simple Logistic Regression with simulated data and real data",
      type = NULL
    )
  })
  observeEvent(input$go,{
    updateTabItems(session,"pages","explore")
  })
  
  observeEvent(input$start,{
    updateTabItems(session,"pages","instruction")
  })
  
  observeEvent(input$go1,{
    updateTabItems(session,"pages","qqq")
  })
  
  observeEvent(input$go2,{
    updateTabItems(session,"pages","references")
  })
  
  observeEvent(input$begin,{
    updateTabItems(session, "pages", "qqq")
  })
  
  observeEvent(input$goMul,{
    updateTabItems(session,"pages","Multiple")
  })
  
  #####################Processing sign#######################
  observeEvent(input$goButtonMul, {
    # When the button is clicked, wrap the code in a call to `withBusyIndicatorServer()`
    withBusyIndicatorServer("goButtonMul", {
      Sys.sleep(1)
    })
  })
  
  observeEvent(input$goButtonMul, {
    # When the button is clicked, wrap the code in a call to `withBusyIndicatorServer()`
    withBusyIndicatorServer("go1Button", {
      Sys.sleep(1)
    })
  })
  
  observeEvent(input$goButtonMul, {
    # When the button is clicked, wrap the code in a call to `withBusyIndicatorServer()`
    withBusyIndicatorServer("go2Button", {
      Sys.sleep(1)
    })
  })
  
  observeEvent(input$goButton, {
    # When the button is clicked, wrap the code in a call to `withBusyIndicatorServer()`
    withBusyIndicatorServer("goButton", {
      Sys.sleep(1)
    })
  })
  
  ############################Gray out buttons###############################
  # observeEvent(input$start, {
  #   updateButton(session, "answer", disabled = TRUE)
  # })
  
  # observeEvent(input$challenge, {
  #   updateButton(session, "answer", disabled = FALSE)
  # })
  
  # observeEvent(input$answer, {
  #   updateButton(session, "answer", disabled=TRUE)
  # })
  
  
  
  #############################plot outputs#################################
  df<-function(b0, b1, sampleSize){
    intercept <-as.numeric(b0)
    bet <- as.numeric(b1)
    x <- rnorm(as.numeric(sampleSize))
    pr <- exp(x * bet) / (1 + exp(x * bet))
    y <- rbinom(as.numeric(sampleSize), 1, pr)
    df = data.frame(x,y)
    return(df)
  }
  
  ##########common objects
  commonDf<-reactive({
    df(input$b0, input$b1, input$sampleSize)
  })
  
  output$logplot<-renderPlotly({
    input$goButton
    df = isolate(commonDf())
    theme_set(theme_bw())
    p <- ggplot(aes(x =x, y = y),data = df)+
      geom_smooth(formula = y ~ x, aes(linetype="fitted probability"),method = 'glm', size = 1, color="orange", 
                  method.args=list(family='binomial'), se=FALSE)+
      geom_ribbon(aes(linetype="confidence\n interval"),stat="smooth", method="glm", alpha=0.15, 
                  level=input$ci, method.args=list(family='binomial'))+
      geom_point()+
      ylab('Observed Bernoulli')+
      xlab('explanatory variables')+
      ggtitle("Logistic Regression Model \n")+
      scale_linetype_manual(values=c("fitted probability", "confidence interval"))+
      theme(
        plot.title = element_text(color="black", size=15, face="bold"),
        axis.text = element_text(color="black", size = 12),
        axis.title.x = element_text(color="black", size = 15),
        axis.title.y = element_text(color="black", size = 15)
      )
    
    p<-
      ggplotly(p)%>%
      layout(legend = list(x = 0.7, y = 0.15))
  })
  
  output$residualPlot<-renderPlot({
    input$goButton
    df = isolate(commonDf())
    logit <- glm(y ~ x, family=binomial, data=df)
    if(input$residualType == "pearson"){
      plot(residuals(logit, type="pearson"), type="b", 
           main="Pearson Res- logit", ylab= "Pearson Residual", 
           cex.axis = 1.3, cex.lab = 1.5, 
           cex.main =1.5, pch=16, las=1)
    }
    else{
      plot(residuals(logit, type="deviance"), 
           type="b", main="Deviance Res- logit", ylab= "Deviance Residual", 
           cex.axis = 1.3, cex.lab = 1.5, 
           cex.main =1.5, pch=16,las=1)  
    }
  })
  
  ##### goodness of fit#####
  HLresult<-function(){
    input$goButton
    df = isolate(commonDf())
    mod <- glm (y~x, data=df, family = binomial)
    hl <- hoslem.test(mod$y, fitted(mod))
    return(hl)
  }
  
  output$lemeshowTest<-renderPrint({
    hl<-HLresult()
    hl
  }
  )
  
  output$lemeshowDF<-renderTable({
    hl<-HLresult()
    hs<-data.frame(hl$statistic, hl$parameter, hl$p.value)
    names(hs)<-c('χ2', 'df', 'p-value')
    rownames(hs)<-NULL
    hs
  }, striped = TRUE, width = "100%", align = 'c', hover = TRUE, bordered = TRUE)
  
  output$obsexpDF<-renderTable({
    hl<-HLresult()
    hob<-data.frame(cbind(hl$expected, hl$observed))
    hob<-setDT(hob, keep.rownames = TRUE)[]
    names(hob)<-c("interval","number of 0s expected", "number of 1s expected", 
                  "number of 0s in group", "number of 1s in group")
    hob
  }, striped = TRUE, width = "100%", align = 'c', hover = TRUE, bordered = TRUE, rownames = TRUE)
  
  output$obsexp<-renderPrint({
    hl<-HLresult()
    cbind(hl$expected, hl$observed)
  })
  
  
  
  #####Multiple Graph
  df2<-function(b0, b1, b2, sampleSize){
    x1 = rnorm(sampleSize)           
    x2 = rnorm(sampleSize)
    z = b0+b1*x1+b2*x2        # linear combination with a bias
    pr = 1/(1+exp(-z))         # pass through an inv-logit function
    y = rbinom(sampleSize,1,pr)      # bernoulli response variable
    df = data.frame(y=y,x1=x1,x2=x2)
    return(df)
  }
  
  ##########common objects
  commonDf2<-reactive({
    df2(input$b02, input$b12, input$b2, input$sampleSize2)
  })
  
  #####read in datatable############
  data(MedGPA)
  data("Titanic")
  data("Leukemia")
  ###empircal logit plot############
  output$empericalLogitPlot<-
    renderPlot({
      if (input$datatable == 'MedGPA'){
        if (input$MedYvar == 'Acceptance'){
          if(input$MedXvar == 'GPA'){
            emplogitplot1(Acceptance~GPA, ngroups=input$ngroups, out=TRUE, data=MedGPA, main="Empirical Logit Plot")
          }
          else if(input$MedXvar == 'MCAT'){
            emplogitplot1(Acceptance~MCAT, ngroups=input$ngroups, out=TRUE, data=MedGPA, main="Empirical Logit Plot")
          }
          else if(input$MedXvar == 'BCPM'){
            emplogitplot1(Acceptance~BCPM, ngroups=input$ngroups, out=TRUE, data=MedGPA, main="Empirical Logit Plot")
          }
        }
      }
      else if (input$datatable == 'Titanic'){
        if (input$TitanicYvar == 'Survived'){
          if(input$TitanicXvar == 'Age'){
            emplogitplot1(Survived~Age,ngroups=input$ngroups, out=TRUE, data=Titanic, main="Empirical Logit Plot")
          }
        }}
      else if (input$datatable == 'Leukemia'){
        if (input$LeukemiaYvar == 'Status'){
          if(input$LeukemiaXvar == 'Blasts'){
            emplogitplot1(Status~Blasts, ngroups=input$ngroups, out=TRUE, data=Leukemia, main="Empirical Logit Plot")
          }
          else if(input$LeukemiaXvar == 'Age'){
            emplogitplot1(Status~Age, ngroups=input$ngroups, out=TRUE, data=Leukemia, main="Empirical Logit Plot")
          }
          else if(input$LeukemiaXvar == 'Infil(perceptage of infiltrate)'){
            emplogitplot1(Status~Infil, ngroups=input$ngroups, out=TRUE, data=Leukemia, main="Empirical Logit Plot")
          }
        }
      }
      
    })
  
  output$mulPlot<-renderPlotly({
    input$goButtonMul
    df = isolate(commonDf2())
    theme_set(theme_bw())
    p <- ggplot(aes(x=x1,y=y),data = df)+
      geom_smooth(formula = y ~ x1,aes(linetype="X1's fitted\n probability"),method = 'glm', size = 1, color="maroon", 
                  method.args=list(family='binomial'), se=FALSE)+
      geom_smooth(formula = y ~ x2,aes(x=x2,y=y, linetype="X2's fitted\n probability "), data=df, method = 'glm', size = 1, color="lightblue",
                  method.args=list(family='binomial'), se=FALSE)+
      geom_ribbon(aes(linetype="confidence\n interval"),stat="smooth", method="glm", alpha=0.15, 
                  level=input$ci2, method.args=list(family='binomial'))+
      geom_point(color="maroon")+
      # geom_smooth(aes(x=x2,y=y, linetype="fitted probability "), data=df, method = 'glm', size = 1, color="lightblue",
      #             method.args=list(family='binomial'), se=FALSE)+
      geom_ribbon(aes(x=x2,y=y, linetype="confidence\n interval"), data=df,stat="smooth", method="glm", alpha=0.15,
                  level=input$ci2, method.args=list(family='binomial'))+
      geom_point(aes(x=x2,y=y), data=df, color="lightblue", alpha=0.4)+
      ylab('Observed Bernoulli')+
      xlab('explanatory variables')+
      ggtitle("Multiple Logistic Regression \n")+
      scale_linetype_manual(values=c("X1's fitted\n probability","X2's fitted\n probability" ,"confidence\n interval"))+
      theme(
        plot.title = element_text(color="black", size=15, face="bold"),
        axis.text = element_text(color="black", size = 12),
        axis.title.x = element_text(color="black", size = 15),
        axis.title.y = element_text(color="black", size = 15)
      )
    
    p<-
      ggplotly(p)%>%
      layout(legend = list("left"))
  })
  
  output$multix<-renderPlot({
    input$goButtonMul
    df<-isolate(commonDf2())
    p<-glm(y~x1+x2,data=df,family="binomial")
    par(mfrow=c(1,3))
    # plot(p,which=c(4,2,1), add.smooth = getOption("add.smooth"), 
    #      las=1, cex.caption=1.5, cex.axis=1.3, cex.lab=1.7)
    plot(p,which=1, add.smooth = getOption("add.smooth"), 
         las=1, cex.caption=1.5, cex.axis=1.3, cex.lab=1.7)
    legend("topleft", legend="fitted line", col="red", lty=1:2, cex=1.5, box.lty=0)
    #second and third plot
    plot(p,which=c(4,2), las=1, cex.caption=1.5, cex.axis=1.3, cex.lab=1.7)
  })
  
  #####Multiple Goodness of fit
  HLresult2<-function(){
    input$goButtonMul
    df<-isolate(commonDf2())
    mod<-glm(y~x1+x2,data=df,family="binomial")
    hl<-hoslem.test(mod$y, fitted(mod), g=10)
    return(hl)
  }
  
  output$lemeshowDF2<-renderTable({
    hl<-HLresult2()
    hs<-data.frame(hl$statistic, hl$parameter, hl$p.value)
    names(hs)<-c('χ2', 'df', 'p-value')
    rownames(hs)<-NULL
    hs
  }, striped = TRUE, width = "100%", align = 'c', hover = TRUE, bordered = TRUE)
  
  output$obsexpDF2<-renderTable({
    hl<-HLresult2()
    hob<-data.frame(cbind(hl$expected, hl$observed))
    hob<-setDT(hob, keep.rownames = TRUE)[]
    names(hob)<-c("interval","number of 0s expected", "number of 1s expected", 
                  "number of 0s in group", "number of 1s in group")
    hob
  }, striped = TRUE, width = "100%", align = 'c', hover = TRUE, bordered = TRUE, rownames = TRUE)
  
  
  
  ######TIMER########
  timer<-reactiveVal(1)
  active<-reactiveVal(FALSE)
  
  # observer that invalidates every second. If timer is active, decrease by one.
  observe({
    invalidateLater(1000, session)
    isolate({
      if(active())
      {
        timer(timer()-1)
        if(timer()<1)
        {
          active(FALSE)
          
          randnum<-sample(1:6, 1)
          newvalue<-score()+isolate(randnum)
          score(newvalue)
          
          if(as.numeric(score())>=20){
            output$dice<-renderUI({
              Sys.sleep(1)
              img(src = "congrats.png", width = '60%')
            })
            updateButton(session, "nextq", disabled = TRUE)
            updateButton(session,"submit", disabled = TRUE)
            updateButton(session, "restart", disabled = FALSE)
          }
          
          else{
            updateButton(session, "nextq", disabled = FALSE)
            if(randnum == 1){
              output$dice<-renderUI({
                Sys.sleep(1)
                img(src = "21.png",width = '30%')
              })
            }
            else if(randnum == 2){
              output$dice<-renderUI({
                Sys.sleep(1)
                img(src = "22.png",width = '30%')
              })
            }
            else if(randnum == 3){
              output$dice<-renderUI({
                Sys.sleep(1)
                img(src = "23.png",width = '30%')
              })
            }
            else if(randnum == 4){
              output$dice<-renderUI({
                Sys.sleep(1)
                img(src = "24.png",width = '30%')
              })
            }
            else if(randnum == 5){
              output$dice<-renderUI({
                Sys.sleep(1)
                img(src = "25.png",width = '30%')
              })
            }
            else if(randnum == 6){
              output$dice<-renderUI({
                Sys.sleep(1)
                img(src = "26.png",width = '30%')
              })
            }
            
          }
          
          
        }
      }
    })
  })
  
  #####Rlocker observe Event##
  # Gets current page address from the current session

  
  # Pulls corresponding answer values from question bank and returns its text
  #bank for question
  
  bank <- read.csv("questionbank.csv")
  bank = data.frame(lapply(bank, as.character), stringsAsFactors = FALSE)
  bank$Feedback = as.character(bank$Feedback)
  sapply(bank, class)
  
  getResponseText <- function(index, answer){
    if(answer == 'A'){
      key = 3
    } else if(answer == 'B'){
      key = 4
    } else {
      key = 5
    }
    return(bank[index, key])
  }
  
  observeEvent(input$ci,{
    interacted_statement <- rlocker::createStatement(
      list(
        verb = list(
          display = "interacted"
        ),
        object = list(
          id = paste0(getCurrentAddress(session)),
          name = 'confidence interval',
          description = 'single logistic Regression'
        ),
        result = list(
          success = NULL,
          response = input$ci
          # response = paste(paste('SampleSize:',input$sampleSize, "beta0:",
          #                        input$b0, "beta1:",input$b1, "confidence interval:", input$ci))
        )
      )
    )
    
    # Store statement in locker and return status
    status <- rlocker::store(session, interacted_statement)
    
    # print(interacted_statement) # remove me
    # print(status) # remove me
  })
  
  #####Buttons Handle#######
  observeEvent(input$nextq,{
    value$answerbox <- value$index
    index_list$list=index_list$list[-1]   
    value$index<-index_list$list[1]
    value$answerbox<-value$index
    
    
    updateButton(session, "nextq", disabled = TRUE)
    updateButton(session,"submit", disabled = FALSE)
    
    
    if(value$index %in% c(11:16)){
      updateSelectInput(session,"answer", "pick an answer from below", c("","A", "B"))
    }
    else{
      updateSelectInput(session,"answer", "pick an answer from below", c("","A", "B", "C"))
    }
    
    output$mark <- renderUI({
      img(src = NULL,width = 30)
    })
    output$Feedback <- renderUI({
      img(src = NULL,width = 30)
    })
  })
  
  
  observeEvent(input$submit,{
    updateButton(session,"submit", disabled = TRUE)
    answer<-isolate(input$answer)
    if (any(answer == ans[value$index,1])){
      output$dice<-renderUI({
        img(src = "newdice1.gif", width = '30%')
      })
      active(TRUE)
    }
    else{
      if(length(index_list$list) == 1){
        updateButton(session, "nextq", disabled = TRUE)
        updateButton(session,"submit", disabled = TRUE)
      }
      else{
        updateButton(session,"submit", disabled = TRUE)
        updateButton(session, "nextq", disabled = FALSE)
      }
    }
    
    ##Mark
    output$mark <- boastUtils::renderIcon(
      icon = ifelse(
        any(answer == ans[value$index,1]),
        yes = "correct",
        no = "incorrect"
      ),
      width = 36
    )
    
    #Feedback
    output$Feedback <- renderUI({
      if (any(answer == ans[value$index,1])){
        HTML(paste("Congrats !", bank[value$index,7], collapse = "\n"))
      }
      else{
        HTML(paste("Don't give up, try again !", bank[value$index,7], collapse = "\n")
        )
      }
    })
    
  })
  
  renderIcon()
  
  
  observeEvent(input$submit,{
    answer<-isolate(input$answer)
    statement <- rlocker::createStatement(
      list(
        verb = list(
          display = "answered"
        ),
        object = list(
          id = paste0(getCurrentAddress(session), "#", value$index),
          name = paste('Question', value$index),
          description = bank[value$index, 2]
        ),
        result = list(
          success = any(answer == ans[value$index,1]),
          response = paste(getResponseText(value$index, answer))
        )
      )
    )
    
    # Store statement in locker and return status
    status <- rlocker::store(session, statement)
    
    # print(statement) # remove me
    # print(status) # remove me
  })
  
  observeEvent(input$restart,{
    updateButton(session, "submit", disabled = FALSE)
    updateButton(session,"restart",disable =FALSE)
    updateSelectInput(session,"answer", "pick an answer from below", c("","A", "B", "C"))
    index_list$list<-c(index_list$list,sample(2:14,13,replace=FALSE))
    value$index <- 1
    value$answerbox = value$index
    ans <- as.matrix(bank[1:16,6])
    index_list<-reactiveValues(list=sample(1:16,10,replace=FALSE))
    output$mark <- renderUI({
      img(src = NULL,width = 30)
    })
    output$Feedback <- renderUI({
      img(src = NULL,width = 30)
    })
  })
  
  ####mark at the beginning
  output$mark <- renderUI({
    img(src = NULL,width = 30)
  })
  output$Feedback <- renderUI({
    img(src = NULL,width = 30)
  })
  
  
  #####Question Part########
  value <- reactiveValues(index =  1, mistake = 0,correct = 0)
  ans <- as.matrix(bank[1:16,6])
  index_list<-reactiveValues(list=sample(1:16,10,replace=FALSE))
  
  output$question <- renderUI({
    value$num <- sample(1:16,1,replace = FALSE)
    h4(bank[value$index, 2])
  })
  ###question choice
  output$options <- renderUI({
    if(value$index == 11){
      str1 <- paste("A.", bank[value$index, 3])
      str2 <- paste("B.", bank[value$index, 4])
      HTML(paste(str1, str2, sep = '<br/>'))
    }
    else if(value$index %in% c(12:16)){
      Apic<-
        img(src = bank[value$index, 3], 
            width = "50%")
      Bpic<-
        img(src = bank[value$index, 4], 
            width = "50%")
      str1 <- paste("A.", Apic)
      str2 <- paste("B.", Bpic)
      HTML(paste(str1, str2, sep = '<br/>'))
    }
    
    else if(value$index %in% c(1:10)){
      str1 <- paste("A.", bank[value$index, 3])
      str2 <- paste("B.", bank[value$index, 4])
      str3 <- paste("C.", bank[value$index, 5])
      HTML(paste(str1, str2, str3, sep = '<br/>'))
    }
    else{
      h4("reach the end")
    }
  })
  
  output$gameplot1<-renderUI(
    img(src = bank[value$index, 3], 
        width = "100%", height = "107%", style = "text-align: center")
  )
  
  ##### Draw the Hangman Game#####
  
  score <- reactiveVal(0)  
  
  output$dice<-renderUI({
    img(src = "21.png",width = '30%')
  })
  
  output$gamescore<-renderUI({
    h2("Your cumulative score is", score())
  })
  
  output$feedback <- renderUI({
    div(style = "text-align: center", tags$h4(bank$Feedback[value$num]))
  })
  
  observeEvent(input$restart,{
    newvalue<-score()-score()
    score(newvalue)
    output$dice<-renderUI({
      img(src = "21.png", width = '30%')
    })
    # updateButton(session, "roll", disabled = FALSE)
    # updateButton(session, "stop", disabled = TRUE)
    # updateButton(session, "restart", disabled = TRUE)
  })
  
}

boastUtils::boastApp(ui = ui, server = server)