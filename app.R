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
library(shinyjs)
library(withr)

# Load Data ----
data("MedGPA")
data("Titanic")
data("Leukemia")

# Import helper functions
source("helpers.R")

# Define UI for App ----
## Create the app page ----
ui <- dashboardPage(
  skin = "yellow",
  ### Create the app header ----
  dashboardHeader(
    title = "Logistic Regression",
    tags$li(
      class = "dropdown",
      actionLink("info", icon("info"), class = "myClass")
    ),
    tags$li(
      class = "dropdown",
      boastUtils::surveyLink(name = "App_Template")
    ),
    tags$li(
      class = "dropdown",
      tags$a(
        href = "https://shinyapps.science.psu.edu/",
        icon("home", lib = "font-awesome")
      )
    ),
    titleWidth = 250
  ),

  ### Create the sidebar/left navigation menu ----
  dashboardSidebar(
    width = 250,
    sidebarMenu(
      id = "pages",
      menuItem("Overview", tabName = "instruction", icon = icon("tachometer-alt")),
      menuItem("Prerequisites", tabName = "prereq", icon = icon("book")),
      menuItem("Explore", tabName = "explore", icon = icon("wpexplorer")),
      menuItem("Game", tabName = "qqq", icon = icon("gamepad")),
      menuItem("References", tabName = "references", icon = icon("leanpub"))
    ),
    tags$div(
      style = "position: absolute; bottom: 0;",
      class = "sidebar-logo",
      boastUtils::sidebarFooter()
    )
  ),

  ### Create the content ----
  dashboardBody(
    tabItems(
      #### Set up the Overview Page ----
      tabItem(
        tabName = "instruction",
        h1("Logistic Regression"),
        p("This app allows you to explore how different factors can affect the 
          outcome of the Logistic Regression Model and Empirical Logit Plot."),
        br(),
        h2("Instructions"),
        tags$ol(tags$li("This app includes Single Logistic Regression with simulated 
                        data and the Empirical Logit Plot with real datasets."),
        tags$li("Click New Sample button to generate plot. Watch the change of 
                plot when drag the slider of confidence interval."),
        tags$li("In Empirical Logit Plot, select interested predictors from the 
                menu and see how the plot changes accordingly"),
        tags$li("After working with the Explore section, you can start the game 
                to test your understanding of the concepts."),
        tags$li("Practice the questions in Game Section. For each question you 
                get right, you would get a chance to roll the dice."),
        tags$li("If the cumulative total for your dice roll reaches 20 within 
                10 questions, YOU WIN!")
        ),
        br(),
        div(
          style = "text-align: center",
          bsButton(inputId = "start", label = "GO!", icon("bolt"), size = "large", 
                   class = "circle grow")
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
          This app was updated by Wanyi Su. Special thanks to Hatfield, Neil J."),
        br(),
        br(),
        br(),
        div(class = "updated", "Last Update: 06/16/2022 by WS.")
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
        div(style = "text-align: center",bsButton(inputId = "go", 
                                                  label = "Explore", 
                                                  icon("bolt"),
                                                  size = "large", 
                                                  class = "circle grow")
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
                    tags$li("Click 'New Sample' button to generate plot.")),
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
                actionButton("goButton", "New Sample", icon("paper-plane"),
                  class = "btn btn-lg", style = "color: #fff", class = "circle grow"
                ),
                br()
              ),
              mainPanel(
                plotlyOutput("logplot", width = "98%", height = "300px") %>% 
                  withSpinner(color = "#ffa500"),
                br(),
                tableOutput("citable"),
                plotOutput("residualPlot", width = "100%", height = "330px") %>% 
                  withSpinner(color = "#ffa500"),
                tags$style(type = "text/css", "#lemeshowTest, #obsexp 
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
              bsButton(inputId = "go1", label = "Play the game!", icon("bolt"), 
                       size = "large", class = "circle grow")
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
            sidebarLayout(
              sidebarPanel(
                #### select datasets
                selectInput(
                  inputId = "datatable", label = "Select Dataset:",
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
              ),
              
              mainPanel(
                plotOutput(outputId = "empiricalLogitPlot", width = "100%") %>% 
                  withSpinner(color = "#ffa500")
              )
            )
          )
        )
      ),

      #### Set up a Game page ----
      tabItem(
        tabName = "qqq",
        h2("Game Section"),
        br(),
        sidebarLayout(
          sidebarPanel(
            id = "sidebar",
            width = 6,
            uiOutput("question"),
            uiOutput("options"),
            br(),
            selectInput("answer", "Select your answer from below", 
                        c("", "A", "B", "C")),
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
              column(12, align = "center", uiOutput("gamescore")),
              column(12, align = "center", div(
                id = "plot-container",
                tags$img(
                  src = "spinner.gif",
                  id = "loading-spinner"
                ),
                uiOutput("dice", width = "100%")
              ))
            ),
            br()
          )
        ),
        fluidRow(
          column(6,
            align = "center",
            div(style = "display: inline-block", actionButton(inputId = "submit", 
                                                              label = "Submit")),
            div(style = "display: inline-block;vertical-align:top; width: 30px;", 
                HTML("<br>")),
            div(style = "display: inline-block", bsButton(inputId = "nextq", 
                                                          label = "Next", 
                                                          disabled = TRUE)),
            div(style = "display: inline-block;vertical-align:top; width: 30px;", 
                HTML("<br>")),
            div(style = "display: inline-block", bsButton(inputId = "restart", 
                                                          label = "Restart"))
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
          "R DATA ANALYSIS EXAMPLES. UCLA. LOGIT REGRESSION. Available from 
          https://stats.idre.ucla.edu/r/dae/logit-regression/"
        ),
        p(
          class = "hangingindent",
          "Wickham, H. (2011), “The Split-apply-combine strategy for data 
          analysis.” Journal of Statistical Software, 40, pp. 1-29.Available 
          from http://www.jstatsoft.org/v40/i01/."
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
  observeEvent(input$infoex, {
    sendSweetAlert(
      session = session,
      title = "Instructions:",
      text = " Move the sliders to see their effect on the diagnostic plots.",
      type = NULL
    )
  })

  observeEvent(input$info, {
    sendSweetAlert(
      session = session,
      title = "Instructions:",
      text = "This app explores Simple Logistic Regression with simulated data 
      and real data",
      type = NULL
    )
  })
  observeEvent(input$go, {
    updateTabItems(session, "pages", "explore")
  })

  observeEvent(input$start, {
    updateTabItems(session, "pages", "prereq")
  })

  observeEvent(input$go1, {
    updateTabItems(session, "pages", "qqq")
  })

  observeEvent(input$go2, {
    updateTabItems(session, "pages", "references")
  })

  observeEvent(input$begin, {
    updateTabItems(session, "pages", "qqq")
  })

  observeEvent(input$goMul, {
    updateTabItems(session, "pages", "Multiple")
  })
  
  ## Update Response Options for empirical logit plot ----
  observeEvent(
    eventExpr = input$datatable, 
    handlerExpr = {
      if (input$datatable == 'MedGPA') {
        updateSelectInput(
          session = session, 
          inputId = "yVar", 
          label = "Select Response Y",
          choices = c("Acceptance")
        ) 
      } else if (input$datatable == "Titanic") {
        updateSelectInput(
          session = session, 
          inputId = "yVar", 
          label = "Select Response Y",
          choices = c("Survived")
        ) 
      } else if (input$datatable == "Leukemia") {
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
    eventExpr = input$datatable, 
    handlerExpr = {
      if (input$datatable == 'MedGPA') {
        updateSelectInput(
          session = session, 
          inputId = "xVar", 
          label = "Select Quantitave Predictor X",
          choices = c("GPA", "MCAT", "BCPM")
        ) 
      } else if (input$datatable == "Titanic") {
        updateSelectInput(
          session = session, 
          inputId = "xVar", 
          label = "Select Quantitave Predictor X",
          choices = c("Age")
        ) 
      } else if (input$datatable == "Leukemia") {
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
  observeEvent(input$goButtonMul, {
    withBusyIndicatorServer("goButtonMul", {
      Sys.sleep(1)
    })
  })

  observeEvent(input$goButtonMul, {
    withBusyIndicatorServer("go1Button", {
      Sys.sleep(1)
    })
  })

  observeEvent(input$goButtonMul, {
    withBusyIndicatorServer("go2Button", {
      Sys.sleep(1)
    })
  })

  observeEvent(input$goButton, {
    withBusyIndicatorServer("goButton", {
      Sys.sleep(1)
    })
  })


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
  commonDf <- reactive({
    df(input$b0, input$b1, input$sampleSize)
  })

  output$logplot <- renderPlotly({
    input$goButton
    df <- isolate(commonDf())
    theme_set(theme_bw())
    p <- ggplot(aes(x = x, y = y), data = df) +
      geom_smooth(
        formula = y ~ x, method = "glm", size = 1, color = "orange",
        method.args = list(family = "binomial"), se = FALSE
      ) +
      geom_ribbon(aes(linetype = "confidence interval"),
        stat = "smooth", method = "glm", alpha = 0.15,
        level = input$ci, method.args = list(family = "binomial")
      ) +
      geom_point() +
      ylab("Observed Bernoulli") +
      xlab("explanatory variables") +
      ggtitle("Logistic Regression Model \n") +
      scale_linetype_manual(name = "", values = c("confidence interval")) +
      theme(
        text = element_text(size = 12)
      )

    p <-
      with_options(list(digits = 1), ggplotly(p)) %>%
      layout(legend = list(x = 0.05, y = 0.9))
  })

  output$residualPlot <- renderPlot({
    input$goButton
    df <- isolate(commonDf())
    logit <- glm(y ~ x, family = binomial, data = df)
    if (input$residualType == "pearson") {
      plot(residuals(logit, type = "pearson"),
        type = "b",
        main = "Pearson Res- logit", ylab = "Pearson Residual",
        cex.axis = 1.3, cex.lab = 1.5,
        cex.main = 1.5, pch = 16, las = 1
      )
    } else {
      plot(residuals(logit, type = "deviance"),
        type = "b", main = "Deviance Res- logit", ylab = "Deviance Residual",
        cex.axis = 1.3, cex.lab = 1.5,
        cex.main = 1.5, pch = 16, las = 1
      )
    }
  })

  ## Goodness of fit ----
  HLresult <- function() {
    input$goButton
    df <- isolate(commonDf())
    mod <- glm(y ~ x, data = df, family = binomial)
    hl <- hoslem.test(mod$y, fitted(mod))
    return(hl)
  }

  output$lemeshowTest <- renderPrint({
    hl <- HLresult()
    hl
  })

  output$lemeshowDF <- renderTable(
    {
      hl <- HLresult()
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
      hl <- HLresult()
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

  output$obsexp <- renderPrint({
    hl <- HLresult()
    cbind(hl$expected, hl$observed)
  })



  ##### Multiple Graph
  df2 <- function(b0, b1, b2, sampleSize) {
    x1 <- rnorm(sampleSize)
    x2 <- rnorm(sampleSize)
    z <- b0 + b1 * x1 + b2 * x2 # linear combination with a bias
    pr <- 1 / (1 + exp(-z)) # pass through an inv-logit function
    y <- rbinom(sampleSize, 1, pr) # bernoulli response variable
    df <- data.frame(y = y, x1 = x1, x2 = x2)
    return(df)
  }

  ########## common objects
  commonDf2 <- reactive({
    df2(input$b02, input$b12, input$b2, input$sampleSize2)
  })
  
  ## Set the Data Collection ----
  dataCollection <- eventReactive(
    eventExpr = input$datatable,
    valueExpr = {
      switch(
        EXPR = input$datatable,
        MedGPA = MedGPA,
        Titanic = Titanic,
        Leukemia = Leukemia
      )
    }
  )

  ### Empirical logit plot ----
  
  observeEvent(
    eventExpr = c(input$datatable, input$yVar, input$xVar, input$ngroups),
    handlerExpr = {
      output$empiricalLogitPlot <- renderPlot(
        expr = {
          validate(
            need(input$yVar %in% names(dataCollection()),
                 message = "No Y var"
            ),
            need(input$xVar %in% names(dataCollection()),
                 message = "No X var")
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
        alt = "FILL ME IN!!"
      )
          
    }
 )

  ### Logistic Regression model ----
  output$mulPlot <- renderPlotly({
    input$goButtonMul
    df <- isolate(commonDf2())
    theme_set(theme_bw())
    p <- ggplot(aes(x = x1, y = y), data = df) +
      geom_smooth(
        formula = y ~ x1, aes(linetype = "X1's fitted\n probability"), 
        method = "glm", size = 1, color = "maroon",
        method.args = list(family = "binomial"), se = FALSE
      ) +
      geom_smooth(
        formula = y ~ x2, aes(x = x2, y = y, linetype = "X2's fitted\n probability"), 
        data = df, method = "glm", size = 1, color = "lightblue",
        method.args = list(family = "binomial"), se = FALSE
      ) +
      geom_ribbon(aes(linetype = "confidence\n interval"),
        stat = "smooth", method = "glm", alpha = 0.15,
        level = input$ci2, method.args = list(family = "binomial")
      ) +
      geom_point(color = "maroon") +
      geom_ribbon(aes(x = x2, y = y, linetype = "confidence\n interval"),
        data = df, stat = "smooth", method = "glm", alpha = 0.15,
        level = input$ci2, method.args = list(family = "binomial")
      ) +
      geom_point(aes(x = x2, y = y), data = df, color = "lightblue", alpha = 0.4) +
      ylab("Observed Bernoulli") +
      xlab("explanatory variables") +
      ggtitle("Multiple Logistic Regression \n") +
      scale_linetype_manual(values = c("X1's fitted\n probability", 
                                       "X2's fitted\n probability", 
                                       "confidence\n interval")) +
      theme(
        plot.title = element_text(color = "black", size = 15, face = "bold"),
        axis.text = element_text(color = "black", size = 12),
        axis.title.x = element_text(color = "black", size = 15),
        axis.title.y = element_text(color = "black", size = 15)
      )

    p <-
      ggplotly(p) %>%
      layout(legend = list("left"))
  })

  output$multix <- renderPlot({
    input$goButtonMul
    df <- isolate(commonDf2())
    p <- glm(y ~ x1 + x2, data = df, family = "binomial")
    par(mfrow = c(1, 3))
    plot(p,
      which = 1, add.smooth = getOption("add.smooth"),
      las = 1, cex.caption = 1.5, cex.axis = 1.3, cex.lab = 1.7
    )
    legend("topleft", legend = "fitted line", col = "red", lty = 1:2, cex = 1.5, 
           box.lty = 0)
    # second and third plot
    plot(p, which = c(4, 2), las = 1, cex.caption = 1.5, cex.axis = 1.3, 
         cex.lab = 1.7)
  })

  ### Multiple Goodness of fit ----
  HLresult2 <- function() {
    input$goButtonMul
    df <- isolate(commonDf2())
    mod <- glm(y ~ x1 + x2, data = df, family = "binomial")
    hl <- hoslem.test(mod$y, fitted(mod), g = 10)
    return(hl)
  }

  output$lemeshowDF2 <- renderTable(
    {
      hl <- HLresult2()
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

  output$obsexpDF2 <- renderTable(
    {
      hl <- HLresult2()
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



  ## TIMER ----
  timer <- reactiveVal(1)
  active <- reactiveVal(FALSE)

  # observer that invalidates every second. If timer is active, decrease by one.
  observe({
    invalidateLater(1000, session)
    isolate({
      if (active()) {
        timer(timer() - 1)
        if (timer() < 1) {
          active(FALSE)

          randnum <- sample(1:6, 1)
          newvalue <- score() + isolate(randnum)
          score(newvalue)

          if (as.numeric(score()) >= 20) {
            output$dice <- renderUI({
              Sys.sleep(1)
              img(src = "congrats.png", width = "60%")
            })
            updateButton(session, "nextq", disabled = TRUE)
            updateButton(session, "submit", disabled = TRUE)
            updateButton(session, "restart", disabled = FALSE)
          } else {
            updateButton(session, "nextq", disabled = FALSE)
            if (randnum == 1) {
              output$dice <- renderUI({
                Sys.sleep(1)
                img(src = "21.png", width = "30%")
              })
            } else if (randnum == 2) {
              output$dice <- renderUI({
                Sys.sleep(1)
                img(src = "22.png", width = "30%")
              })
            } else if (randnum == 3) {
              output$dice <- renderUI({
                Sys.sleep(1)
                img(src = "23.png", width = "30%")
              })
            } else if (randnum == 4) {
              output$dice <- renderUI({
                Sys.sleep(1)
                img(src = "24.png", width = "30%")
              })
            } else if (randnum == 5) {
              output$dice <- renderUI({
                Sys.sleep(1)
                img(src = "25.png", width = "30%")
              })
            } else if (randnum == 6) {
              output$dice <- renderUI({
                Sys.sleep(1)
                img(src = "26.png", width = "30%")
              })
            }
          }
        }
      }
    })
  })

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

  observeEvent(input$ci, {
  })

  ## Buttons Handle ----
  observeEvent(input$nextq, {
    index_list$list <- index_list$list[!index_list$list %in% value$index]
    value$index <- index_list$list[1]
    value$answerbox <- value$index

    updateButton(session, "nextq", disabled = TRUE)
    updateButton(session, "submit", disabled = FALSE)

    if (value$index %in% c(11:16)) {
      updateSelectInput(session, "answer", "pick an answer from below", 
                        c("", "A", "B"))
    } else {
      updateSelectInput(session, "answer", "pick an answer from below", 
                        c("", "A", "B", "C"))
    }

    output$mark <- renderUI({
      img(src = NULL, width = 30)
    })
    output$Feedback <- renderUI({
      img(src = NULL, width = 30)
    })
  })


  observeEvent(input$submit, {
    updateButton(session, "submit", disabled = TRUE)
    answer <- isolate(input$answer)
    if (any(answer == ans[value$index, 1])) {
      output$dice <- renderUI({
        img(src = "newdice1.gif", width = "30%")
      })
      active(TRUE)
    }

    if (length(index_list$list) == 1) {
      updateButton(session, "nextq", disabled = TRUE)
      updateButton(session, "submit", disabled = TRUE)

      sendSweetAlert(
        session = session,
        title = "Try Again",
        text = "You've run out of questions, click the restart button to try again.",
        type = "warning"
      )
    } else {
      updateButton(session, "submit", disabled = TRUE)
      updateButton(session, "nextq", disabled = FALSE)
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
    output$Feedback <- renderUI({
      if (any(answer == ans[value$index, 1])) {
        HTML(paste("Congrats !", bank[value$index, 7], collapse = "\n"))
      } else {
        HTML(paste("Don't give up, try again !", bank[value$index, 7], collapse = "\n"))
      }
    })
  })

  renderIcon()


  observeEvent(input$restart, {
    updateButton(session, "submit", disabled = FALSE)
    updateButton(session, "restart", disable = FALSE)
    updateSelectInput(session, "answer", "pick an answer from below", 
                      c("", "A", "B", "C"))
    index_list$list <- c(index_list$list, sample(2:14, 13, replace = FALSE))
    value$index <- 1
    value$answerbox <- value$index
    ans <- as.matrix(bank[1:16, 6])
    index_list <- reactiveValues(list = sample(1:16, 10, replace = FALSE))
    output$mark <- renderUI({
      img(src = NULL, width = 30)
    })
    output$Feedback <- renderUI({
      img(src = NULL, width = 30)
    })
  })

  #### mark at the beginning
  output$mark <- renderUI({
    img(src = NULL, width = 30)
  })
  output$Feedback <- renderUI({
    img(src = NULL, width = 30)
  })


  ## Question Part ----
  value <- reactiveValues(index = 1, mistake = 0, correct = 0)
  ans <- as.matrix(bank[1:16, 6])
  index_list <- reactiveValues(list = sample(1:16, 10, replace = FALSE))

  output$question <- renderUI({
    value$num <- sample(1:16, 1, replace = FALSE)
    h4(bank[value$index, 2])
  })
  ### question choice
  output$options <- renderUI({
    if (value$index == 11) {
      str1 <- paste("A.", bank[value$index, 3])
      str2 <- paste("B.", bank[value$index, 4])
      HTML(paste(str1, str2, sep = "<br/>"))
    } else if (value$index %in% c(12:16)) {
      Apic <-
        img(
          src = bank[value$index, 3],
          width = "50%"
        )
      Bpic <-
        img(
          src = bank[value$index, 4],
          width = "50%"
        )
      str1 <- paste("A.", Apic)
      str2 <- paste("B.", Bpic)
      HTML(paste(str1, str2, sep = "<br/>"))
    } else if (value$index %in% c(1:10)) {
      str1 <- paste("A.", bank[value$index, 3])
      str2 <- paste("B.", bank[value$index, 4])
      str3 <- paste("C.", bank[value$index, 5])
      HTML(paste(str1, str2, str3, sep = "<br/>"))
    } else {
      h4("reach the end")
    }
  })

  output$gameplot1 <- renderUI(
    img(
      src = bank[value$index, 3],
      width = "100%", height = "107%", style = "text-align: center"
    )
  )

  ## Draw the Hangman Game  ----

  score <- reactiveVal(0)

  output$dice <- renderUI({
    img(src = "21.png", width = "30%")
  })

  output$gamescore <- renderUI({
    h2("Your cumulative score is", score())
  })

  output$feedback <- renderUI({
    div(style = "text-align: center", tags$h4(bank$Feedback[value$num]))
  })

  observeEvent(input$restart, {
    newvalue <- score() - score()
    score(newvalue)
    output$dice <- renderUI({
      img(src = "21.png", width = "30%")
    })
  })
}

# Boast App Call ----
boastUtils::boastApp(ui = ui, server = server)
