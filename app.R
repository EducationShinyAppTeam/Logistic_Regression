# Load Package ----
library(boastUtils)
library(ggplot2)
library(plotly)
library(dplyr)
library(shinycssloaders)
library(Stat2Data)
library(ResourceSelection)
library(data.table)
library(shinyBS)
library(shinydashboard)
library(shinyWidgets)
library(shinyjs)
library(withr)

# Load Data ----
data("MedGPA")
data("Titanic")
data("Leukemia")

# Fix issue with missing values and non-finite values
Titanic <- Titanic[complete.cases(Titanic), ]

# Import helper functions
source("helpers.R")

# Define UI for App ----
## Create the app page ----
ui <- dashboardPage(
  skin = "yellow",
  ## Create the app header ----
  dashboardHeader(
    title = "Logistic Regression",
    titleWidth = 250,
    tags$li(class = "dropdown", actionLink("info", icon("info"))),
    tags$li(
      class = "dropdown",
      boastUtils::surveyLink(name = "Logistic_Regression")
    ),
    tags$li(
      class = "dropdown",
      tags$a(href = 'https://shinyapps.science.psu.edu/',
             icon("home")
      )
    )
  ),
  ### Create the sidebar/left navigation menu ----
  dashboardSidebar(
    width = 250,
    sidebarMenu(
      id = "pages",
      menuItem("Overview", tabName = "overview", icon = icon("tachometer-alt")),
      menuItem("Prerequisites", tabName = "prereq", icon = icon("book")),
      menuItem("Explore", tabName = "explore", icon = icon("wpexplorer")),
      menuItem("Game", tabName = "game", icon = icon("gamepad")),
      menuItem("References", tabName = "references", icon = icon("leanpub"))
    ),
    tags$div(
      class = "sidebar-logo",
      boastUtils::sidebarFooter()
    )
  ),
  ### Create the content ----
  dashboardBody(
    tabItems(
      #### Set up the Overview Page ----
      tabItem(
        tabName = "overview",
        h1("Logistic Regression"),
        p("This app allows you to explore how different factors can affect the 
          outcome of the Logistic Regression Model and Empirical Logit Plot."),
        br(),
        h2("Instructions"),
        tags$ol(tags$li("This app includes Single Logistic Regression with simulated 
                        data and the Empirical Logit Plot with real datasets."),
        tags$li("Click the New Sample button to generate the plot. Watch the change 
                of the plot when dragging the slider of confidence interval."),
        tags$li("In the Empirical Logit Plot, select desired predictors from 
                the menu and see how the plot changes accordingly."),
        tags$li("After working with the Explore section, you can start the game 
                to test your understanding of the concepts."),
        tags$li("Practice the questions in the Game Section. For each question you 
                get right, you will get a chance to roll the dice."),
        tags$li("If the cumulative total for your dice roll reaches 20 within 
                10 questions, YOU WIN!")
        ),
        br(),
        div(
          style = "text-align: center",
          bsButton(
            inputId = "goToPrereq", 
            label = "Prerequisites!", 
            icon = icon("book"), 
            size = "large", 
            class = "circle grow"
          )
        ),
        br(),
        h2("About the data"),
        p("The datasets and the procedures for the empirical logit plot are adopted 
          from Stat2: Models for a World of Data by Cannon, Cobb, Hartlaub, Legler, 
          Lock, Moore, Rossman, and Witmer."),
        br(),
        br(),
        h2("Acknowledgements"),
        p("This app was developed and coded by Yiyun Gong and Ruisi Wang. 
          This app was further updated by Wanyi Su and Sean Burke. Special thanks 
          to Hatfield, Neil J."),
        br(),
        br(),
        br(),
        "Cite this app as:",
        br(),
        citeApp(),
        br(),
        br(),
        div(class = "updated", "Last Update: 05/24/2023 by SB.")
      ),
      #### Set up the Prerequisites Page ----
      tabItem(
        tabName = "prereq",
        withMathJax(),
        h2("Logistic Regression Analysis"),
        br(),
        tags$ul(
          tags$li("The logistic regression model explains the relationship 
                  between one (or more) explanatory 
                  variable and the binary outcome."),
          tags$li("In the logistic regression the constant \\(\\beta_0\\) moves 
                  the curve left and right and the slope \\(\\beta_1\\) defines 
                  the steepness of the curve."),
          div("\\[{ln({p\\over1-p})} = {\\beta_0+\\beta_1x}\\]"),
          tags$li("Empirical logit plot is used to check the linearity for datasets."),
          div("\\[{logit ( \\hat p )=log({ \\hat p\\over1-\\hat p})}\\]"),
          tags$li("Deviance Residual and Pearson Residual check the model fit. 
                  Best results are no patterns or no extremely large residuals "),
          tags$li("Hosmer and Lemeshow test check the goodness of fit in the model 
                  where data is divided into recommended 10 groups. The p-value 
                  can determine the significance of the result."),
          tags$li("Hosmer-Lemeshow Test Statstics"),
          div("\\[{\\sum_{i=1}^g}{\\sum_{j=1}^2}{{(obs_{ij} - exp_{ij})^2}}\\]"),
        ),
        div(
          style = "text-align: center",
            bsButton(
              inputId = "goToExplore", 
              label = "Explore", 
              icon = icon("bolt"),
              size = "large", 
              class = "circle grow"
          )
        )
      ),
      #### Set up an Explore Page ----
      tabItem(
        tabName = "explore",
        tabsetPanel(
          type = "tabs",
          tabPanel(
            ###### Single Regression
            "Single Logistic Regression",
            h2("Single Logistic Regression"),
            tags$ul(tags$li("Adjust the sliders to change the sample size and 
                            corresponding beta coefficients."),
                    tags$li("Click the 'New Sample' button to generate a new plot.")),
            br(),
            fluidRow(
              column(
                width = 5,
                wellPanel(
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
                  sliderInput(
                    inputId = "ci",
                    label = "confidence interval level:",
                    min = 0,
                    max = 0.99,
                    value = 0.95,
                    step = 0.01
                  ),
                  selectInput(
                    inputId = "residualType",
                    label = "Residual Type",
                    choices = c("deviance", "pearson"),
                    selected = "deviance"
                  ),
                  br(),
                  actionButton(
                    inputId = "newSample", 
                    label = "New Sample", 
                    icon = icon("paper-plane"),
                    class = "btn btn-lg", 
                    style = "color: #fff", 
                    class = "circle grow"
                  ),
                  br()
                )
              ),
              column(
                width = 7,
                plotlyOutput("logPlot", width = "98%", height = "300px") %>% 
                  withSpinner(color = boastUtils::psuPalette[4]),
                br(),
                tableOutput("citable"),
                plotOutput("residualPlot", width = "100%", height = "330px") %>% 
                  withSpinner(color =  boastUtils::psuPalette[4]),
                tags$style(type = "text/css", "#lemeshowTest, #obsExp 
                           {background-color: rgba(249, 105, 14, 1); color: yellow; 
                           text-align: center}", "#title{color: blackl; 
                           padding-left:2.5em; font-size: 22px}"),
                br(),
                h3(strong(id = "title", "Hosmer and Lemeshow goodness of fit test")),
                tableOutput("lemeshowDF"),
                tableOutput("obsexpDF"),
                bsPopover("lemeshowDF", " ", "The Hosmer-Lemeshow Test is a goodness 
                          of fit test for the logistic model. Here is the result 
                          of the Hosmer-Lemeshow Test for ten groups. Number of 
                          subgroups, g, usually uses the formula g > P + 1. P is 
                          number of covariates. Degree of freedom equals g-2. ", 
                          trigger = "hover", placement = "left"),
                bsPopover("obsexpDF", " ", "There are 10 rows meaning g=10.", 
                          trigger = "hover", placement = "left")
              )
            ),
            # set continue button
            div(
              style = "text-align: center",
              bsButton(
                inputId = "goToGame",
                label = "Play the game!",
                icon = icon("bolt"), 
                size = "large", 
                class = "circle grow"
              )
            )
          ),
          ##### Empirical Logit Plot ----
          tabPanel(
            "Empirical Logit Plot",
            h2("Empirical Logit Plot"),
            h3("Process of creating an empirical logit plot for quantitative predictors"),
            tags$ol(
              tags$li("Divide the range of the predictor into intervals 
                            with roughly equal numbers of cases."),
              tags$li("Compute the mean value of the predictor and the 
                            empirical logit for each interval."),
              tags$li("Plot logit versus the mean value of the predictor, 
                            with one point for each interval.")),
            br(),
            fluidRow(
              column(
                width = 5,
                wellPanel(
                  #### select data sets
                  selectInput(
                    inputId = "dataTable", label = "Select Dataset:",
                    choices = c("MedGPA", "Titanic", "Leukemia"),
                    selected = "MedGPA"
                  ),
                  selectInput(
                    inputId = "yVar",
                    label = "Select Response Y",
                    choices = c("default1")
                  ),
                  selectInput(
                    inputId = "xVar",
                    label = "Select Quantitave Predictor",
                    choices = c("default1", "default2", "default3")
                  ),
                  ### number of groups
                  sliderInput("ngroups", "Number of Groups (Intervals):",
                              min = 2, max = 8,
                              value = 4, step = 1
                  ),
                  br()
                )
              ),
              column(
                width = 7,
                plotOutput(outputId = "empiricalLogitPlot", width = "100%") %>% 
                  withSpinner(color =  boastUtils::psuPalette[4])
              )
            )
          )
        )
      ),
      #### Set up a Game page ----
      tabItem(
        tabName = "game",
        h2("Game Section"),
        p("Answer the questions below and reach a score of at least 20 within 10 
          questions to win!"),
        br(),
        fluidRow(
          column(
            width = 6,
            wellPanel(
              uiOutput("questNum"),
              br(),
              uiOutput("question"),
              uiOutput("options"),
              br(),
              selectInput(
                inputId = "answer", 
                label = "Select your answer from below", 
                choices = c("", "A", "B", "C")
              ),
              uiOutput("mark"),
              br(),
              uiOutput("Feedback"),
              br()
            ),
          ),
          column(
            width = 6,
            tags$head(tags$style(HTML(mycss))),
            fluidRow(
              column(
                width = 12, 
                align = "center", 
                uiOutput("gameScore")
              ),
              column(
                width = 12, 
                align = "center", 
                div(
                  uiOutput("dice", width = "100%")
                )
              )
            ),
            br()
          )
        ),
        fluidRow(
          column(
            width = 6,
            align = "center",
            div(
              style = "display: inline-block", 
              actionButton(
                inputId = "submit", 
                label = "Submit"
              )
            ),
            div(
              style = "display: inline-block;vertical-align:top; width: 30px;", 
              HTML("<br>")
            ),
            div(
              style = "display: inline-block",
              bsButton(
                inputId = "nextQuestion", 
                label = "Next", 
                disabled = TRUE
              )
            ),
            div(
              style = "display: inline-block;vertical-align:top; width: 30px;", 
              HTML("<br>")
            ),
            div(
              style = "display: inline-block",
              bsButton(
                inputId = "restart", 
                label = "Restart"
              )
            )
          )
        )
      ),
      #### Set up a References page ----
      tabItem(
        tabName = "references",
        withMathJax(),
        h2("References"),
        p(
          class = "hangingindent",
          "Attali, D.(2020). 
            shinyjs: Easily Improve the User Experience of Your Shiny Apps in Seconds. R package version 2.0.0 [R Package]. 
            Available from https://CRAN.R-project.org/package=shinyjs"
        ),
        p(
          class = "hangingindent",
          "Bailey, E. (2015). shinyBS: Twitter bootstrap components for shiny.
            (v0.61). [R package]. Available from
            https://CRAN.R-project.org/package=shinyBS"
        ),
        p(
          class = "hangingindent",
          "Carey, R. (2019). boastUtils: BOAST Utilities, R Package. 
          Available from https://github.com/EducationShinyAppTeam/boastUtils"
        ),
        p(
          class = "hangingindent",
          "Chang, W. and Borges Ribeio, B. (2018). shinydashboard: Create 
          dashboards with 'Shiny', R Package. Available from 
          https://CRAN.R-project.org/package=shinydashboard"
        ),
        p(
          class = "hangingindent",
          "Diez, David M., Christopher D. Barr, and Mine Çetinkaya-Rundel. (2021).
           Stat2Data: Datasets for Stat2. R package version 2.0.0. Available from
           https://CRAN.R-project.org/package=Stat2Data."
        ),
        p(
          class = "hangingindent",
          "Dowle, Matt, and Arun Srinivasan. (2021). data.table: Extension of data.frame. 
          R package version 1.14.8. Available from https://CRAN.R-project.org/package=data.table."
          
        ),
        p(
          class = "hangingindent",
          "Dice PNG (2022). Klipartz.
          Available from https://www.klipartz.com/en/search?q=dice"
        ),
        p(
          class = "hangingindent",
          "Molnar, C. (2022). Interpretable machine learning.
          5.2 Logistic Regression. Available from https://christophm.github.io/
          interpretable-ml-book/logistic.html "
        ),
        p(
          class = "hangingindent",
          "Hosmer, D. W., and Stanley Lemeshow. (2000). Applied Logistic Regression. 
          John Wiley & Sons."
          ),
        p(
          class = "hangingindent",
          "Niedballa, Jürgen, and Matthias Lindenborn. (2016). resourceselection: 
          Resource Selection (Probability) Functions for Use-Availability Data. 
          R package version 0.3-5. Available from 
          https://CRAN.R-project.org/package=resourceselection."
        ),
        p(
          class = "hangingindent",
          "Perrier, V., Meyer, F., and Granjon, D. (2020). shinyWidgets: 
            Custom Inputs Widgets for Shiny. R package version 0.5.3. Available 
            from https://CRAN.R-project.org/package=shinyWidgets"
        ),
        p(
          class = "hangingindent",
          "R DATA ANALYSIS EXAMPLES. UCLA. LOGIT REGRESSION. Available from 
          https://stats.idre.ucla.edu/r/dae/logit-regression/"
        ),
        p(
          class = "hangingindent",
          "Wickham, H. (2011), “The Split-apply-combine strategy for data 
          analysis.” Journal of Statistical Software, 40, pp. 1-29.Available 
          from http://www.jstatsoft.org/v40/i01/."
        ),
        p(
          class = "hangingindent",
          "Wickham, H., Chang, W., Henry, L., Pedersen, T.L., Takahashi, K., 
            Wilke, C., Woo, K., Yutani, H., Dunnington, D.  (2020). ggplot2: 
            Create Elegant Data Visualisations Using the Grammar of Graphics. R package
            version 3.3.3. Available from https://CRAN.R-project.org/package=ggplot2"
        ),
        p(
          class = "hangingindent",
          "Wickham, H., François, R., Henry, L., Müller, K. (2021). dplyr: A 
            Grammar of Data Manipulation. R package version 1.0.6. Available from
            https://CRAN.R-project.org/package=dplyr"
        ),
        p(
          class = "hangingindent",
          "Sali, A., and Attali, D. (2020), shinycssloaders: Add Loading
            Animations to a 'shiny' Ouput While It's Recalculating. (v. 1.0.0)
            [R Package] Available from https://CRAN.R-project.org/package=shinycssloaders"
        ),
        p(
          class = "hangingindent",
          "Sievert, C. (2020). plotly: Create Interactive Web Graphics via 'plotly.js'.
          R package version 4.10.1. Available from  https://CRAN.R-project.org/package=plotly."
          ),
        p(
          class = "hangingindent",
          "Ushey, Kevin, and Hadley Wickham. 2021. withr: Run Code 'With' Temporarily 
          Modified Global State. R package version 2.4.2. Available from
          https://CRAN.R-project.org/package=withr."
        ),
        br(),
        br(),
        br(),
        boastUtils::copyrightInfo()
      ) 
    )
  )
)

# Define server logic ----
server <- function(input, output, session) {

  ## Set up info button ----
  observeEvent(
    eventExpr = input$info,
    handlerExpr = {
      sendSweetAlert(
        session = session,
        type = "info",
        title = "Information",
        text = "This app explores Simple Logistic Regression with  both simulated 
        and real data."
      )
    }
  )

  observeEvent(
    eventExpr = input$goToExplore,
    handlerExpr = {
      updateTabItems(
        session = session,
        inputId = "pages",
        selected = "explore"
      )
    }
  )

  observeEvent(
    eventExpr = input$goToPrereq,
    handlerExpr = {
      updateTabItems(
        session = session,
        inputId = "pages",
        selected = "prereq"
      )
    }
  )

  observeEvent(
    eventExpr = input$goToGame,
    handlerExpr = {
      updateTabItems(
        session = session,
        inputId = "pages",
        selected = "game"
      )
    }
  )

  observeEvent(
    eventExpr = input$go2,
    handlerExpr = {
      updateTabItems(
        session = session,
        inputId = "pages",
        selected = "references"
      )
    }
  )

  observeEvent(
    eventExpr = input$goMul,
    handlerExpr = {
      updateTabItems(
        session = session,
        inputId = "pages",
        selected = "Multiple"
      )
    }
  )
  
  ## Update Response Options for empirical logit plot ----
  observeEvent(
    eventExpr = input$dataTable, 
    handlerExpr = {
      if (input$dataTable == 'MedGPA') {
        updateSelectInput(
          session = session, 
          inputId = "yVar", 
          label = "Select Response Y",
          choices = c("Acceptance")
        ) 
      } else if (input$dataTable == "Titanic") {
        updateSelectInput(
          session = session, 
          inputId = "yVar", 
          label = "Select Response Y",
          choices = c("Survived")
        ) 
      } else if (input$dataTable == "Leukemia") {
        updateSelectInput(
          session = session,
          inputId = "yVar", 
          label = "Select Response Y",
          choices = c("Status")
        ) 
      }
    }
  )
  
  ## Update Predictor Options for empirical logit plot ----
  observeEvent(
    eventExpr = input$dataTable, 
    handlerExpr = {
      if (input$dataTable == 'MedGPA') {
        updateSelectInput(
          session = session, 
          inputId = "xVar", 
          label = "Select Quantitave Predictor X",
          choices = c("GPA", "MCAT", "BCPM")
        ) 
      } else if (input$dataTable == "Titanic") {
        updateSelectInput(
          session = session, 
          inputId = "xVar", 
          label = "Select Quantitave Predictor X",
          choices = c("Age")
        ) 
      } else if (input$dataTable == "Leukemia") {
        updateSelectInput(
          session = session,
          inputId = "xVar", 
          label = "Select Quantitave Predictor X",
          choices = c("Blasts", "Age", "Infil")
        ) 
      }
    }
  )

  ## Processing sign ----
  observeEvent(
    eventExpr = input$goButtonMul,
    handlerExpr = {
      withBusyIndicatorServer(
        "goButtonMul", 
        {Sys.sleep(1)}
      )
    }
  )

  observeEvent(
    eventExpr = input$goButtonMul,
    handlerExpr = {
      withBusyIndicatorServer(
        "goToGameButton", 
        {Sys.sleep(1)}
      )
    }
  )

  observeEvent(
    eventExpr = input$goButtonMul, 
    handlerExpr = {
      withBusyIndicatorServer(
        "go2Button",
        {Sys.sleep(1)}
      )
    }
  )

  observeEvent(
    eventExpr = input$newSample,
    handlerExpr = {
      withBusyIndicatorServer(
        "newSample",
        {Sys.sleep(1)}
      )
    }
  )

  ## Plot outputs ----
  df <- function(b0, b1, sampleSize) {
    intercept <- as.numeric(b0)
    bet <- as.numeric(b1)
    x <- rnorm(as.numeric(sampleSize))
    pr <- exp(x * bet) / (1 + exp(x * bet))
    y <- rbinom(as.numeric(sampleSize), 1, pr)
    df <- data.frame(x, y)
    return(df)
  }

  ## common objects ----
  commonDf <- reactive(
    x = {
      df(input$b0, input$b1, input$sampleSize)
    }
  )
  
  ## Logistic Plot ----
  output$logPlot <- renderPlotly(
    expr = {
      input$newSample
      df <- isolate(commonDf())
      theme_set(theme_bw())
      p <- ggplot(
        mapping = aes(x = x, y = y), 
        data = df
      ) +
        geom_smooth(
          formula = y ~ x, 
          method = "glm", 
          linewidth = 1, 
          color = boastUtils::psuPalette[4],
          method.args = list(family = "binomial"), 
          se = FALSE
        ) +
        geom_ribbon(
          mapping = aes(linetype = "confidence interval"),
          stat = "smooth", 
          method = "glm", 
          alpha = 0.15,
          level = input$ci, 
          method.args = list(family = "binomial"),
          formula = y ~ x
        ) +
        geom_point() +
        ylab("Observed Bernoulli") +
        xlab("explanatory variable") +
        ggtitle("Logistic Regression Model \n") +
        scale_linetype_manual(name = "", values = c("confidence interval")) +
        theme(
          text = element_text(size = 12)
        )
      p <- with_options(list(digits = 1), ggplotly(p)) %>%
        layout(legend = list(x = 0.05, y = 0.9))
    }
  )
  
  output$residualPlot <- renderPlot(
    expr = {
      input$newSample
      df <- isolate(commonDf())
      logit <- glm(
        formula = y ~ x,
        family = "binomial",
        data = df
      )
      if (input$residualType == "pearson") {
        p <- plot(
          residuals(logit, type = "pearson"),
          type = "b",
          main = "Pearson Res- logit",
          ylab = "Pearson Residual",
          cex.axis = 1.3,
          cex.lab = 1.5,
          cex.main = 1.5, 
          pch = 16,
          las = 1
        )
      } else {
        p <- plot(
          residuals(logit, type = "deviance"),
          type = "b", 
          main = "Deviance Res- logit", 
          ylab = "Deviance Residual",
          cex.axis = 1.3, 
          cex.lab = 1.5,
          cex.main = 1.5, 
          pch = 16,
          las = 1
        )
      }
      p
    },
    alt = reactive(
      paste0(
        "This ",
        if (input$residualType == "pearson") {
          "pearson"
        } else {
          "deviance"
        },
        " plot displays ",
        input$sampleSize,
        " points whose residuals appear to fall randomly around 0."
      )
    )
  )

  ## Goodness of fit ----
  hlResult <- function() {
    input$newSample
    df <- isolate(commonDf())
    mod <- glm(
      formula = y ~ x,
      data = df,
      family = "binomial"
    )
    hl <- hoslem.test(mod$y, fitted(mod))
    return(hl)
  }

  output$lemeshowTest <- renderPrint(
    expr = {
      hl <- hlResult()
      hl
    }
  )
  
  output$lemeshowDF <- renderTable(
    {
      hl <- hlResult()
      hs <- data.frame(hl$statistic, hl$parameter, hl$p.value)
      names(hs) <- c("χ2", "df", "p-value")
      rownames(hs) <- NULL
      hs
    },
    striped = TRUE,
    width = "100%",
    align = "c",
    hover = TRUE,
    bordered = TRUE
  )

  output$obsexpDF <- renderTable(
    {
      hl <- hlResult()
      hob <- data.frame(cbind(hl$expected, hl$observed))
      hob <- setDT(hob, keep.rownames = TRUE)[]
      names(hob) <- c(
        "interval", "number of 0s expected", "number of 1s expected",
        "number of 0s in group", "number of 1s in group"
      )
      hob
    },
    striped = TRUE,
    width = "100%",
    align = "c",
    hover = TRUE,
    bordered = TRUE,
    rownames = TRUE
  )

  output$obsExp <- renderPrint(
    expr = {
      hl <- hlResult()
      cbind(hl$expected, hl$observed)
    }
  )
  
  ## Set the Data Collection ----
  dataCollection <- eventReactive(
    eventExpr = input$dataTable,
    valueExpr = {
      switch(
        EXPR = input$dataTable,
        MedGPA = MedGPA,
        Titanic = Titanic,
        Leukemia = Leukemia
      )
    }
  )

  ### Empirical logit plot ----
  observeEvent(
    eventExpr = c(input$dataTable, input$yVar, input$xVar, input$ngroups),
    handlerExpr = {
      output$empiricalLogitPlot <- renderPlot(
        expr = {
          validate(
            need(
              input$yVar %in% names(dataCollection()),
              message = "No Y var"
            ),
            need(
              input$xVar %in% names(dataCollection()),
              message = "No X var"
            )
          )
          breaks <- quantile(
            x = dataCollection()[, input$xVar],
            probs = (0:input$ngroups)/input$ngroups,
            na.rm = TRUE
          )
          xGroups <- cut(
            x = dataCollection()[, input$xVar], 
            breaks = breaks, 
            labels = 1:input$ngroups,
            include.lowest = TRUE,
            right = FALSE
          )
          tempData <- cbind(
              dataCollection(),
              xGroups = xGroups
            )
          empLogitData <- tempData %>%
            dplyr::group_by(xGroups) %>%
            summarize(
              xMean = mean(.data[[input$xVar]]),
              cases = n(),
              yeses = sum(.data[[input$yVar]])
            ) %>%
            dplyr::mutate(
              adjProp = (yeses + 0.5)/(cases + 1),
              logit = log(adjProp/(1 - adjProp))
            )
          ggplot(
            data = empLogitData,
            mapping = aes(x = xMean, y = logit)
          ) +
            geom_point(size = 2) +
            geom_smooth(
              formula = y ~ x,
              method = "lm",
              se = FALSE
            ) +
            theme_bw() +
            ylab(paste0("Log Odds(", input$yVar, ")")) +
            xlab(input$xVar) +
            ggtitle("Empirical Logit Plot") +
            theme(
              text = element_text(size = 16)
            )
        },
        alt = paste0(
          "This Empirical logit plot displays the relationship between Log Odds(",
          input$yVar,
          ") and ",
          input$xVar,
          ", along with ",
          input$ngroups, 
          " intervals on the plot."
        )
      )
    }
 )
  
  ## Timer for Dice and Success ----
  timer <- reactiveVal(1)
  active <- reactiveVal(FALSE)

  # observer that invalidates every second. If timer is active, decrease by one.
  observe(
    x = {
      invalidateLater(1000, session)
      isolate(
        expr = {
          if (active()) {
            timer(timer() - 1)
            if (timer() < 1) {
              active(FALSE)
              randNum <- sample(1:6, 1)
              newValue <- score() + isolate(randNum)
              score(newValue)
              if (as.numeric(score()) >= 20) {
                output$dice <- renderUI(
                  expr = {
                    Sys.sleep(1)
                    sendSweetAlert(
                      session = session,
                      title = "Congratulations!",
                      text = paste0(
                        "You've successfully reached a score of ", 
                        score(),
                        " within 10 questions. Click the restart button to play 
                        again."
                      ),
                      type = "success"
                    )
                  }
                )
                updateButton(
                  session = session,
                  inputId = "nextQuestion",
                  disabled = TRUE
                )
                updateButton(
                  session = session,
                  inputId = "submit",
                  disabled = TRUE
                )
                updateButton(
                  session = session,
                  inputId = "restart",
                  disabled = FALSE
                )
              } else {
                updateButton(
                  session = session,
                  inputId = "nextQuestion",
                  disabled = FALSE
                )
                if (randNum == 1) {
                  output$dice <- renderUI(
                    expr = {
                      Sys.sleep(1)
                      img(
                        src = "21.png",
                        width = "30%",
                        alt = "The dice rolled a 1."
                      )
                    }
                  )
                } else if (randNum == 2) {
                  output$dice <- renderUI(
                    expr = {
                      Sys.sleep(1)
                      img(
                        src = "22.png",
                        width = "30%",
                        alt = "The dice rolled a 2."
                      )
                    }
                  )
                } else if (randNum == 3) {
                  output$dice <- renderUI(
                    expr = {
                      Sys.sleep(1)
                      img(
                        src = "23.png",
                        width = "30%",
                        alt = "The dice rolled a 3."
                      )
                    }
                  )
                } else if (randNum == 4) {
                  output$dice <- renderUI(
                    expr = {
                      Sys.sleep(1)
                      img(
                        src = "24.png",
                        width = "30%",
                        alt = "The dice rolled a 4."
                      )
                    }
                  )
                } else if (randNum == 5) {
                  output$dice <- renderUI(
                    expr = {
                      Sys.sleep(1)
                      img(
                        src = "25.png",
                        width = "30%",
                        alt = "The dice rolled a 5."
                      )
                    }
                  )
                } else if (randNum == 6) {
                  output$dice <- renderUI(
                    expr = {
                      Sys.sleep(1)
                      img(
                        src = "26.png",
                        width = "30%",
                        alt = "The dice rolled a 6."
                      )
                    }
                  )
                }
              }
            }
          }
        }
      )
    }
  )
  
  # Pulls corresponding answer values from question bank and returns its text
  # bank for question

  bank <- read.csv("questionbank.csv")
  bank <- data.frame(lapply(bank, as.character), stringsAsFactors = FALSE)
  bank$Feedback <- as.character(bank$Feedback)
  sapply(bank, class)

  getResponseText <- function(index, answer) {
    if (answer == "A") {
      key <- 3
    } else if (answer == "B") {
      key <- 4
    } else {
      key <- 5
    }
    return(bank[index, key])
  }

  ## Question Counter ----
  questionCount <- reactiveVal(1)
  
  ## Buttons Handle ----
  observeEvent(
    eventExpr = input$nextQuestion, 
    handlerExpr = {
      if (questionCount() == 10) {
        updateButton(
          session = session,
          inputId = "nextQuestion",
          disabled = TRUE
        )
        updateButton(
          session = session,
          inputId = "submit",
          disabled = TRUE
        )
        sendSweetAlert(
          session = session,
          title = "Try Again",
          text = "You've run out of questions. Click the restart button to try again.",
          type = "warning"
        )
      } else {
        indexList$list <- indexList$list[!indexList$list %in% value$index]
        value$index <- indexList$list[1]
        value$answerBox <- value$index
        
        updateButton(
          session = session,
          inputId = "nextQuestion",
          disabled = TRUE
        )
        updateButton(
          session = session,
          inputId = "submit",
          disabled = FALSE
        )
        if (value$index %in% c(11:16)) {
          updateSelectInput(
            session = session,
            inputId = "answer",
            label = "Select your answer from below", 
            choices = c("", "A", "B")
          )
        } else {
          updateSelectInput(
            session = session, 
            inputId = "answer",
            label = "Select your answer from below", 
            choices = c("", "A", "B", "C")
          )
        }
        output$mark <- renderUI(
          expr = {
            img(src = NULL, width = 30) #clears correction mark
          }
        )
        output$Feedback <- renderUI(
          expr = {
            img(src = NULL, width = 30) #clears feedback
          }
        )
        questionCount(questionCount() + 1)
      }
    }
  )
  

  observeEvent(
    eventExpr = input$submit,
    handlerExpr = {
      updateButton(
        session = session,
        inputId = "submit",
        disabled = TRUE
      )
      answer <- isolate(input$answer)
      if (any(answer == ans[value$index, 1])) {
        output$dice <- renderUI(
          expr = {
            img(
              src = "newdice1.gif",
              width = "30%",
              alt = "The dice is rolling"
            )
          }
        )
        active(TRUE)
      }
      
      if (questionCount() >= 10) {
        updateButton(
          session = session,
          inputId = "nextQuestion",
          disabled = FALSE
        )
        updateButton(
          session = session,
          inputId = "submit",
          disabled = TRUE
        )
      } else {
        updateButton(
          session = session,
          inputId = "submit",
          disabled = TRUE
        )
        updateButton(
          session = session,
          inputId = "nextQuestion",
          disabled = FALSE
        )
      }
      
      ## Mark
      output$mark <- boastUtils::renderIcon(
        icon = ifelse(
          any(answer == ans[value$index, 1]),
          yes = "correct",
          no = "incorrect"
        ),
        width = 36
      )
      
      # Feedback
      output$Feedback <- renderUI(
        expr = {
          if (any(answer == ans[value$index, 1])) {
            HTML(paste("Congrats!", bank[value$index, 7], collapse = "\n"))
          } else {
            HTML(paste("Sorry, that is incorrect!", bank[value$index, 7], collapse = "\n"))
          }
        }
      )
    }
  )
  
  renderIcon()

  observeEvent(
    eventExpr = input$restart, 
    handlerExpr = {
      updateButton(
        session = session, 
        inputId = "submit", 
        disabled = FALSE
      )
      updateButton(
        session = session, 
        inputId = "restart", 
        disabled = FALSE
      )
      updateSelectInput(
        session = session, 
        inputId = "answer", 
        label = "Select your answer from below",
        choices = c("", "A", "B", "C")
      )
      indexList$list <- c(indexList$list, sample(2:14, 13, replace = FALSE))
      value$index <- 1
      value$answerBox <- value$index
      ans <- as.matrix(bank[1:16, 6])
      indexList <- reactiveValues(list = sample(1:16, 10, replace = FALSE))
      output$mark <- renderUI( 
        expr = {
          img(src = NULL, width = 30) #clears correction marks
        }
      )
      output$Feedback <- renderUI(
        expr = {
          img(src = NULL, width = 30) #clears Feedback
        }
      )
    }
  )

  ## Question Part ----
  value <- reactiveValues(index = 1, mistake = 0, correct = 0)
  ans <- as.matrix(bank[1:16, 6])
  indexList <- reactiveValues(list = sample(1:16, 10, replace = FALSE))
  
  output$question <- renderUI(
    expr = {
      value$num <- sample(1:16, 1, replace = FALSE)
      h4(bank[value$index, 2])
    }
  )
  
  ### Plot Image Alt Text ----
  plotAltText <- function(i) {
    altText <- if (bank[value$index, i] %in% c("b1pos1.png", "b1pos2.png", "b1pos3.png")) {
      "This plot indicates a positive slope."
    } else if (bank[value$index, i] %in% c("b1neg1.png", "b1neg2.png", "b1neg3.png")) {
      "This plot indicates a negative slope."
    } else if (bank[value$index, i] %in% c("largersample1.png", "largersample2.png")) {
      "This plot displays a large amount of points."
    } else if (bank[value$index, i] %in% c("smallersample1.png", "smallersample2.png")) {
      "This plot displays a small amount of points."
    } else {
      ""
    }
    paste0(altText)
  }
  
  ### question choice ----
  output$options <- renderUI(
    expr = {
      if (value$index == 11) {
        str1 <- paste("A.", bank[value$index, 3])
        str2 <- paste("B.", bank[value$index, 4])
        HTML(paste(str1, str2, sep = "<br/>"))
      } else if (value$index %in% c(12:16)) {
        picA <-
          img(
            src = bank[value$index, 3],
            width = "50%",
            alt = plotAltText(3)
          )
        picB <-
          img(
            src = bank[value$index, 4],
            width = "50%",
            alt = plotAltText(4)
          )
        str1 <- paste("A.", picA)
        str2 <- paste("B.", picB)
        HTML(paste(str1, str2, sep = "<br/>"))
      } else if (value$index %in% c(1:10)) {
        str1 <- paste("A.", bank[value$index, 3])
        str2 <- paste("B.", bank[value$index, 4])
        str3 <- paste("C.", bank[value$index, 5])
        HTML(paste(str1, str2, str3, sep = "<br/>"))
      } else {
        h4("reach the end")
      }
    }
  )

  ## Dice Icon for quiz  ----
  
  score <- reactiveVal(0)
  
  output$dice <- renderUI(
    expr = {
      img(
        src = "21.png",
        width = "30%",
        alt = "The dice currently displays a 1."
      )
    }
  )
  
   output$questNum <- renderUI(
    expr = {
      h2("Question ", questionCount())
    }
  )
  output$gameScore <- renderUI(
    expr = {
      h2("Your cumulative score is", score())
    }
  )
  
  output$feedback <- renderUI(
    expr = {
      div(
        style = "text-align: center", 
        tags$h4(bank$Feedback[value$num]))
    }
  )
  
  observeEvent(
    eventExpr = input$restart, 
    handlerExpr = {
      newValue <- score() - score()
      score(newValue)
      questionCount(1)
      output$dice <- renderUI(
        expr = {
          img(
            src = "21.png",
            width = "30%",
            alt = "The dice currently displays a 1." 
          )
        }
      )
    }
  )
}

# Boast App Call ----
boastUtils::boastApp(ui = ui, server = server)
