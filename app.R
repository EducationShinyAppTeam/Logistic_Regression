# Load Packages ----
library(boastUtils)
library(ggplot2)
library(DT)
library(dplyr)
library(Stat2Data)
library(ResourceSelection)
library(shinyBS)
library(shinydashboard)
library(shinyWidgets)

# Global Constants ----
MAXTRIES <- 10
TARGETSCORE <- 20

# Load Data ----
## Coming from the Stat2Data package
data("MedGPA")
data("Titanic")
data("Leukemia")

# Fix issue with missing values and non-finite values
Titanic <- Titanic[complete.cases(Titanic), ]

bank <- read.csv("questionbank.csv")

# Define Helper functions ----
gRule <- function(sampleSize) {
  if (sampleSize > 30) {
    g <- 10
  } else {
    g <- floor(sampleSize/3)
  }
  return(g)
}

hlResult <- function(data, sampleSize) {
  mod <- glm(
    formula = y ~ x,
    data = data,
    family = "binomial"
  )
  # g formula for function
  hl <- hoslem.test(mod$y, fitted(mod), gRule(sampleSize))
  return(hl)
}

# Define UI for App ----
ui <- dashboardPage(
  skin = "yellow",
  ## Header ----
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
  ## Sidebar ----
  dashboardSidebar(
    width = 250,
    sidebarMenu(
      id = "pages",
      menuItem("Overview", tabName = "overview", icon = icon("gauge-high")),
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
  dashboardBody(
    tabItems(
      ## Overview Page ----
      tabItem(
        tabName = "overview",
        h1("Logistic Regression"),
        p("This app allows you to explore how different factors can affect the
          outcome of the Logistic Regression Model and Empirical Logit Plot."),
        br(),
        h2("Instructions"),
        tags$ol(
          tags$li("This app includes Simple Logistic Regression with simulated
                  data and the Empirical Logit Plot with real datasets."),
          tags$li("On the Explore page, click the New Sample button to generate
                the plot. Watch the change of the plot when dragging the slider
                of confidence interval."),
          tags$li("In the Empirical Logit Plot, select desired predictors from the
                menu and see how the plot changes accordingly."),
          tags$li("After working with the Explore section, you can start the game
                to test your understanding of the concepts."),
          tags$li("Practice the questions in the Game Section. For each question you
                get right, you will get a chance to roll the dice."),
          tags$li("If the cumulative total for your dice roll reaches 20 within
                10 questions, YOU WIN!")
        ),
        div(
          style = "text-align: center;",
          bsButton(
            inputId = "goToPrereq",
            label = "Prerequisites!",
            icon = icon("book"),
            size = "large"
          )
        ),
        br(),
        br(),
        h2("Acknowledgements"),
        p("This app was developed and coded by Yiyun Gong and Ruisi Wang.
          This app was further updated by Wanyi Su, Sean Burke, and Davis Jiwoo Im.
          Special thanks to Neil Hatfield."),
        br(),
        br(),
        br(),
        "Cite this app as:",
        br(),
        citeApp(),
        br(),
        br(),
        div(class = "updated", "Last Update: 07/12/2024 by DJI.")
      ),
      ## Prerequisites Page ----
      tabItem(
        tabName = "prereq",
        withMathJax(),
        h2("Logistic Regression Analysis"),
        br(),
        tags$ul(
          tags$li(
            "The logistic regression model explains the relationship between one
            or more explanatory variables and a binary outcome."
          ),
          tags$li(
            "In the logistic regression the constant \\(\\beta_0\\) moves the
            curve left and right and the slope \\(\\beta_1\\) defines the
            steepness of the curve."
          ),
          tags$li(
            "Logistic regression models the relationship between the log-odds
             of an event with the linear model:
             \\[\\log\\left(\\frac{p}{1-p}\\right) = \\beta_0 +\\beta_1*x\\]"
          ),
          tags$li(
            "The log of the odds of the binary outcome is called the logit. An
            empirical logit is based on data using the following formula
            \\[\\text{logit}\\left(\\widehat{p}\\right)=
            \\log\\left(\\frac{\\widehat{p}}{1-\\widehat{p}}\\right)\\]"
          ),
          tags$li(
            "The empirical logit plot is used to check the linearity assumption
            for datasets."
          ),
          tags$li(
            "Deviance Residuals and Pearson Residuals are used to check the model
            fit. Best results are no patterns or no extremely large residuals."
          ),
          tags$li(
            "The Hosmer and Lemeshow test is used to check the goodness of fit in
            the model where data is divided into g groups; g is often recommended
            to equal 10. Because of the arbitrary nature of picking g, the test
            has very low power with small sample sizes. With this app, the
            following rules are put in place to determine g:",
            tags$ol(
              tags$li("The minimum sample size is 10 (\\(n=10\\))."),
              tags$li("Set \\(g = 10\\) when \\(n \\gt 30\\)."),
              tags$li("Set \\(g = \\left\\lfloor n/3 \\right\\rfloor\\) when
                      \\(10 \\leq n \\leq 30\\).")
            ),
            "The p-value can be approximated using a Chi-squared distribution
            with g-2 degrees of freedom, when g is greater than the number of
            covariates plus one."
          ),
          tags$li(
            "Hosmer-Lemeshow Test Statstic is defined as
            \\[\\sum_{i=1}^g\\sum_{j=1}^2
            \\frac{\\left(obs_{ij} - exp_{\\,ij}\\right)^2}{exp_{\\,ij}}\\]"
          ),
        )
      ),
      ## Explore Page ----
      tabItem(
        tabName = "explore",
        withMathJax(),
        h2("Explore Logistic Regression"),
        p("Explore Logistic Regression by looking first exploring how model
          parameters impact the various plots for Simple Logistic Regression.
          Then explore the empirical logit plot using some real-world data."),
        tabsetPanel(
          type = "tabs",
          tabPanel(
            ### Simple Logistic Tab ----
            title = "Simple Logistic Regression",
            br(),
            p("Adjust the sliders to change the sample size and corresponding
              beta coefficients. When ready, click the 'New Sample' button to
              simulate new data."),
            p("You can add a confidence band to the logistic regression model
              plot as well as change what type of residual is shown."),
            fluidRow(
              column(
                width = 4,
                wellPanel(
                  sliderInput(
                    inputId = "sampleSize",
                    label = "Set sample size",
                    min = 10,
                    max = 300,
                    value = 150,
                    step = 1
                  ),
                  sliderInput(
                    inputId = "b0",
                    label = "\\(\\beta_0\\) (intercept)",
                    min = -10,
                    max = 10,
                    value = 0
                  ),
                  sliderInput(
                    inputId = "b1",
                    label = "\\(\\beta_1\\) (coefficient)",
                    min = -10,
                    max = 10,
                    value = 3
                  ),
                  bsButton(
                    inputId = "newSample",
                    label = "New sample",
                    icon = icon("retweet"),
                    size = "large"
                  ),
                  br(),
                  br(),
                  sliderInput(
                    inputId = "ci",
                    label = "Confidence level",
                    min = 0.6,
                    max = 0.99,
                    value = 0.95,
                    step = 0.01
                  ),
                  checkboxInput(
                    inputId = "showCI",
                    label = "Show confidence interval",
                    value = TRUE
                  ),
                  selectInput(
                    inputId = "residualType",
                    label = "Residual type",
                    choices = c("Deviance", "Pearson"),
                    selected = "Deviance"
                  )
                )
              ),
              column(
                width = 8,
                plotOutput(outputId = "logPlot"),
                plotOutput(
                  outputId = "residualPlot",
                  width = "100%",
                  height = "330px"
                )
              )
            ),
            h3("Hosmer and Lemeshow Goodness of Fit Test"),
            DT::DTOutput(outputId = "obsexpDF", width = "75%"),
            DT::DTOutput(outputId = "lemeshowDF", width = "75%"),
            conditionalPanel(
              condition = "input.sampleSize < 100",
              p(tags$strong("Caution:"), "The Hosmer-Lemeshow test has very low
                power in this situation")
            )
          ),
          ### Empirical Logit Plot ----
          tabPanel(
            title = "Empirical Logit Plot",
            br(),
            p("The process for creating an empirical logit plot for quantitative
              predictors can be thought of in three steps."),
            tags$ol(
              tags$li("Divide the range of the predictor into intervals with
                      roughly equal numbers of cases."),
              tags$li("Compute the mean value of the predictor and the empirical
                      logit for each interval."),
              tags$li("Plot logit versus the mean value of the predictor, with
                      one point for each interval.")
            ),
            fluidRow(
              column(
                width = 4,
                wellPanel(
                  selectInput(
                    inputId = "dataTable",
                    label = "Select a data collection",
                    choices = c("MedGPA", "Titanic", "Leukemia"),
                    selected = "MedGPA"
                  ),
                  selectInput(
                    inputId = "yVar",
                    label = "Select response, Y",
                    choices = c("default1")
                  ),
                  selectInput(
                    inputId = "xVar",
                    label = "Select quantitave predictor, X",
                    choices = c("default1", "default2", "default3")
                  ),
                  sliderInput(
                    inputId = "ngroups",
                    label = "Number of groups/intervals",
                    min = 2,
                    max = 8,
                    value = 4,
                    step = 1
                  )
                )
              ),
              column(
                width = 8,
                plotOutput(outputId = "empiricalLogitPlot")
              )
            )
          )
        )
      ),
      ## Game page ----
      tabItem(
        tabName = "game",
        withMathJax(),
        h2("Game Section"),
        p("Answer the questions below and reach a score of at least", TARGETSCORE,
          "within", MAXTRIES, "questions to win! Your score for each correct
          answer is determined by the roll of a die."),
        fluidRow(
          column(
            width = 6,
            wellPanel(
              uiOutput("questNum"),
              br(),
              uiOutput("question"),
              uiOutput("options"),
              selectInput(
                inputId = "answer",
                label = "Your answer",
                choices = c("Select an option", "A", "B", "C") #,
                # width = "50%"
              ),
              uiOutput("mark"),
              uiOutput("Feedback"),
              bsButton(
                inputId = "submit",
                label = "Submit",
                size = "large"
              ),
              bsButton(
                inputId = "nextQuestion",
                label = "Next",
                disabled = TRUE,
                size = "large"
              ),
              bsButton(
                inputId = "restart",
                label = "Restart",
                size = "large",
                icon = icon("triangle-exclamation"),
                style = "danger"
              )
            )
          ),
          column(
            width = 6,
            uiOutput(outputId = "gameScore"),
            uiOutput(outputId = "dice", align = "center")
          )
        ),
        hr(),
        ### NEW layout----
        h2("New Layout"),
        fluidRow(
          column(
            width = 8,
            offset = 0,
            uiOutput(outputId = "questionText"),
            uiOutput(outputId = "additionalItems"),
            selectInput(
              inputId = "userChoice",
              label = "Your answer",
              choices = c("Select an option", "A", "B", "C")
            ),
            bsButton(
              inputId = "submitNew",
              label = "Submit",
              size = "large"
            ),
          ),
          column(
            width = 4,
            offset = 0,
            p("score mark"),
            uiOutput(outputId = "scoreMark"),
            p("feedback text"),
            uiOutput(outputId = "feedbackNew"),
            p("Die Image"),
            uiOutput(outputId = "dieRoll"),
            uiOutput(outputId = "playerScore"),
            uiOutput(outputId = "questionCounter")
          )
        ),
        bsButton(
          inputId = "nextQuestionNew",
          label = "Next question",
          size = "large"
        ),
        bsButton(
          inputId = "reset",
          label = "Restart",
          icon = icon("triangle-exclamation"),
          style = "danger",
          size = "large"
        )
      ),
      ## References page ----
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
          "Cannon, A., Cobb, G. W., Hartlaub, B. A., Legler, J. M., Lock, R. H.,
          Moore, T. L., Rossman, A. J., Witmer, J. A. (2019). STAT2: Modeling with
          Regression and ANOVA. W.H. Freeman/Macmillan Learning."
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
          "Ushey, Kevin, and Hadley Wickham. 2021. withr: Run Code 'With' Temporarily
          Modified Global State. R package version 2.4.2. Available from
          https://CRAN.R-project.org/package=withr."
        ),
        p(
          class = "hangingindent",
          "Xie, Y., Cheng, J., Tan, X., Allaire, J., Girlich, M., Ellis, G.F.,
            and Rauh, J. (2020), DT: A Wrapper of the JavaScript Library
            'DataTables', R Package. Available from
            https://cran.r-project.org/web/packages/DT/index.html"
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

  ## Prereq's button ----
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

  ## Explore Simple Logistic Regression ----
  ### Create data for logistic plot ----
  logisticData <- eventReactive(
    eventExpr = input$newSample,
    valueExpr = {
      if (input$newSample == 0) {
        return(NULL)
      } else {
        x <- rnorm(n = input$sampleSize, mean = 0, sd = 1)
        probSuccess <- exp(input$b0 + x * input$b1) / (1 + exp(input$b0 + x * input$b1))
        y <- rbinom(n = input$sampleSize, size = 1, prob = probSuccess)
        return(data.frame(x = x, y = y))
      }
    },
    ignoreNULL = FALSE
  )

  ### Logistic Plot ----
  output$logPlot <- renderPlot(
    expr = {
      validate(
        need(expr = !is.null(logisticData()), message = "Generate a sample")
      )
      p <- ggplot(
        data = logisticData(),
        mapping = aes(x = x, y = y)
      ) +
        geom_point() +
        geom_smooth(
          formula = y ~ x,
          method = "glm",
          linewidth = 1.5,
          color = boastUtils::psuPalette[4],
          method.args = list(family = "binomial"),
          se = FALSE
        ) +
        labs(
          x = "Explanatory Variable",
          y = "Observed Bernoulli",
          title = "Logistic Regression Model"
        ) +
        theme_bw(base_size = 20) +
        theme(
          legend.position = "bottom"
        )
      #### Add confidence band
      if (input$showCI == TRUE) {
        p <- p + geom_ribbon(
          stat = "smooth",
          method = "glm",
          alpha = 0.15,
          level = input$ci,
          method.args = list(family = "binomial"),
          formula = y ~ x
        )
      }
      p
    },
    alt = reactive(
      paste0(
        "This logistic plot ",
        if (input$b1 > 0) {"displays a negative slope. "} else if (input$b1 < 0) {
          "displays a positive slope. "
        } else {"displays a slope of 0. "},
        "There are ", input$sampleSize, " points that are either 0 or 1."
      )
    )
  )

  ### Residual Plot ----
  output$residualPlot <- renderPlot(
    expr = {
      validate(
        need(expr = !is.null(logisticData()), message = "Generate a sample")
      )
      logitModel <- glm(
        formula = y ~ x,
        family = "binomial",
        data = logisticData()
      )
      residuals <- data.frame(
        resids = residuals(
          object = logitModel,
          type = ifelse(
            test = input$residualType == "Pearson",
            yes = "pearson",
            no = "deviance"
          )
        )
      ) %>%
        mutate(
          index = row_number()
        )

      ggplot(
        data = residuals,
        mapping = aes(x = index, y = resids)
      ) +
        geom_point() +
        geom_path() +
        theme_bw(base_size = 20) +
        labs(
          x = "Index",
          y = paste(
            ifelse(
              test = input$residualType == "Pearson",
              yes = "Pearson",
              no = "Deviance"),
            "Residual"
          ),
          title = "Residual-Logit"
        )
    },
    alt = reactive(
      paste0(
        "This ", input$residualType, " Residual plot displays ", input$sampleSize,
        " points whose residuals appear to fall randomly around 0."
      )
    )
  )

  ### H-L Test Table ----
  output$lemeshowDF <- DT::renderDT(
    expr = {
      validate(
        need(expr = !is.null(logisticData()), message = "Generate a sample")
      )
      hl <- hlResult(data = logisticData(), sampleSize = input$sampleSize)
      data.frame(
        X2 = round(hl$statistic, digits = 2),
        df = round(hl$parameter, digits = 2),
        `p-value` = round(hl$p.value, digits = 2)
      )
    },
    caption = "Results of Hosmer & Lemeshow Test",
    style = "bootstrap4",
    rownames = FALSE,
    colnames = c("Chi-squared Value", "Degrees of Freedom", "P-value"),
    options = list(
      responsive = TRUE,
      scrollX = TRUE,
      paging = FALSE,  # Set to False for small tables
      searching = FALSE,  # Set to False to turn of the search bar
      ordering = FALSE,
      info = FALSE,
      columnDefs = list(
        list(className = "dt-center", targets = "_all")
      ),
      margin = "auto"
    )
  )

  ### Obs-Exp by Bin Table ----
  output$obsexpDF <- DT::renderDT(
    expr = {
      validate(
        need(expr = !is.null(logisticData()), message = "Generate a sample")
      )
      hl <- hlResult(data = logisticData(), sampleSize = input$sampleSize)
      hob <- as.data.frame(
        round(cbind(hl$expected, hl$observed), digits = 2)
      )
      cbind(
        "gBins" = rownames(hob),
        data.frame(hob, row.names = NULL)
      )
    },
    style = "bootstrap4",
    caption = "Observed and Expected Frequncies by Bin",
    rownames = FALSE,
    colnames = c("Bin", "Exp. Failure", "Exp. Success", "Obs. Failure", "Obs. Success"),
    options = list(
      responsive = TRUE,
      scrollX = TRUE,
      paging = FALSE,  # Set to False for small tables
      searching = FALSE,  # Set to False to turn of the search bar
      ordering = FALSE,
      info = FALSE,
      columnDefs = list(
        list(className = "dt-center", targets = c(1:4))
      )
    )
  )

  ## Explore Empirical Logit Plot ----
  dataCollection <- reactiveVal(value = NULL, label = "Empirical Data")
  ### Respond to Data Selection ----
  observeEvent(
    eventExpr = input$dataTable,
    handlerExpr = {
      #### Update Response Options ----
      if (input$dataTable == 'MedGPA') {
        updateSelectInput(
          session = session,
          inputId = "yVar",
          choices = c("Acceptance")
        )
      } else if (input$dataTable == "Titanic") {
        updateSelectInput(
          session = session,
          inputId = "yVar",
          choices = c("Survived")
        )
      } else if (input$dataTable == "Leukemia") {
        updateSelectInput(
          session = session,
          inputId = "yVar",
          choices = c("Status")
        )
      }

      #### Update Predictor Options ----
      if (input$dataTable == 'MedGPA') {
        updateSelectInput(
          session = session,
          inputId = "xVar",
          choices = c("GPA", "MCAT", "BCPM")
        )
      } else if (input$dataTable == "Titanic") {
        updateSelectInput(
          session = session,
          inputId = "xVar",
          choices = c("Age")
        )
      } else if (input$dataTable == "Leukemia") {
        updateSelectInput(
          session = session,
          inputId = "xVar",
          choices = c("Blasts", "Age", "Infil")
        )
      }

      #### Update data collection ----
      dataCollection(
        switch(
          EXPR = input$dataTable,
          MedGPA = MedGPA,
          Titanic = Titanic,
          Leukemia = Leukemia
        )
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
              message = "Response not in data"
            ),
            need(
              input$xVar %in% names(dataCollection()),
              message = "Predictor not in data"
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
            theme_bw(base_size = 20) +
            labs(
              y = paste0("Log Odds(", input$yVar, ")"),
              x = input$xVar,
              title = "Empirical Logit Plot"
            )
        },
        alt = paste0(
          "This Empirical logit plot displays the relationship between the
          Log Odds(", input$yVar, ") and ", input$xVar, ", along with ",
          input$ngroups, " intervals on the plot."
        )
      )
    }
  )

  ## Game Code ----
  ### Key Variables ----
  playerBank <- reactiveVal(
    value = bank[sample(x = 1:nrow(bank), size = nrow(bank), replace = FALSE), ]
  )
  score <- reactiveVal(0)
  questionCount <- reactiveVal(1)


  value <- reactiveValues(index = 1, mistake = 0, correct = 0)
  ans <- as.matrix(bank[1:16, 6])
  indexList <- reactiveValues(list = sample(1:16, 10, replace = FALSE))

  ### Question Display ----
  #### Additional Graphs
  #### Select Answer

  ### Question Display NEW----
  observeEvent(
    eventExpr = questionCount(),
    handlerExpr = {
      #### Text
      output$questionText <- renderUI(
        expr = {
          p(class = "largerFont", playerBank()$question[questionCount()])
        }
      )

      #### Additional Elements
      if (playerBank()$responses[questionCount()] == "graph") {
        output$additionalItems <- renderUI(
          expr = {
            tagList(
              tags$figure(
                tags$figcaption(tags$strong("Graph A")),
                tags$img(
                  src = playerBank()$A[questionCount()],
                  alt = "TBA",
                  width = "50%"
                )
              ),
              br(),
              tags$figure(
                tags$figcaption(tags$strong("Graph B")),
                tags$img(
                  src = playerBank()$B[questionCount()],
                  alt = "TBA",
                  width = "50%"
                )
              )
            )
          }
        )
      } else {
        output$additionalItems <- renderUI({NULL})
      }

      if (playerBank()$responses[questionCount()] == "graph") {
        currentChoices <- c("Select an option", "Graph A", "Graph B")
      } else if (playerBank()$responses[questionCount()] == "logical") {
        currentChoices <- c("Select an Option", "True", "False")
      } else {
        currentChoices <- c(
          "Select an Option",
          sample(
            x = c(
              playerBank()$A[questionCount()],
              playerBank()$B[questionCount()],
              playerBank()$C[questionCount()]
            ),
            size = 3,
            replace = FALSE
          )
        )
      }

      #### Update Choices
      updateSelectInput(
        session = session,
        inputId = "userChoice",
        choices = currentChoices
      )
    }
  )


  #### Question Number ----
  output$questNum <- renderUI(
    expr = {
      h2("Question ", questionCount())
    }
  )

  #### Prompt ----
  output$question <- renderUI(
    expr = {
      value$num <- sample(1:16, 1, replace = FALSE)
      h4(bank[value$index, 2])
    }
  )

  #### Answer options ----
  output$options <- renderUI(
    expr = {
      if (value$index == 11) {
        str1 <- paste("A.", bank[value$index, 3])
        str2 <- paste("B.", bank[value$index, 4])
        HTML(paste(str1, str2, sep = "<br/>"))
      } else if (value$index %in% c(12:16)) {
        picA <- img(
            src = bank[value$index, 3],
            width = "50%",
            alt = plotAltText(3)
          )
        picB <- img(
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

  ### Submit Button ----
  observeEvent(
    eventExpr = input$submit,
    handlerExpr = {
      #### Update buttons
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

      #### Answer Check
      answer <- isolate(input$answer)
      if (any(answer == ans[value$index, 1])) {
        ##### Roll Die
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
      ##### Feedback--Text and Mark
      output$mark <- boastUtils::renderIcon(
        icon = ifelse(
          any(answer == ans[value$index, 1]),
          yes = "correct",
          no = "incorrect"
        ),
        width = 36
      )

      output$Feedback <- renderUI(
        expr = {
          if (any(answer == ans[value$index, 1])) {
            HTML(paste("Congrats!", bank[value$index, 7], collapse = "\n"))
          } else {
            HTML(paste("Sorry, that is incorrect!", bank[value$index, 7], collapse = "\n"))
          }
        }
      )

      # OLD?
      output$feedback <- renderUI(
        expr = {
          div(
            style = "text-align: center",
            tags$h4(bank$Feedback[value$num]))
        }
      )

      ##### Update Score
    }
  )

  ### Next Button ----
  observeEvent(
    eventExpr = input$nextQuestion,
    handlerExpr = {
      #### Check for available questions
      if (questionCount() == 10) {
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
        #### Update question display
        indexList$list <- indexList$list[!indexList$list %in% value$index]
        value$index <- indexList$list[1]
        value$answerBox <- value$index
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

        questionCount(questionCount() + 1)
      }

      #### Clear feedback
      output$mark <- renderIcon()
      output$Feedback <- renderUI({NULL})

      #### Update next button
      updateButton(
        session = session,
        inputId = "nextQuestion",
        disabled = TRUE
      )
    }
  )

  ### Restart Button ----
  observeEvent(
    eventExpr = input$restart,
    handlerExpr = {
      #### Update Score
      score(0)

      #### Other resets ????
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

      #### Clear feedback
      output$mark <- renderIcon()
      output$Feedback <- renderUI({NULL})

      #### Update buttons
      updateButton(
        session = session,
        inputId = "submit",
        disabled = FALSE
      )
      updateButton(
        session = session,
        inputId = "nextQuestion",
        disabled = TRUE
      )

      #### Update question display
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
    }
  )

  ### Score Message ----
  output$gameScore <- renderUI(
    expr = {
      p(class = "largerFont", "Your cumulative score:", score())
    }
  )

  ### Game Results Displays ----
  output$playerScore <- renderUI(
    expr = {
      p(class = "largerFont", "Your cumulative score:", score())
    }
  )

  output$questionCounter <- renderUI(
    expr = {
      p(class = "largerFont", "You've attempted", questionCount(), "of",
        MAXTRIES, "questions.")
    }
  )

  ### Game Check ----

  # OLD ----
  ### Timer for Dice and Success ----
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



  ## Dice Icon for quiz  ----
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

# Boast App Call ----
boastUtils::boastApp(ui = ui, server = server)
