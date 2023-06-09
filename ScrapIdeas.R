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
commonDf2 <- reactive(
  x = {
    df2(input$b02, input$b12, input$b2, input$sampleSize2)
  }
)

### Multiple Logistic Regression model ----
output$mulPlot <- renderPlotly(
  expr = {
    input$goButtonMul
    df <- isolate(commonDf2())
    theme_set(theme_bw())
    p <- ggplot(
      mapping = aes(x = x1, y = y),
      data = df
    ) +
      geom_smooth(
        formula = y ~ x1, 
        mapping = aes(linetype = "X1's fitted\n probability"), 
        method = "glm",
        linewidth = 1,
        color = "maroon",
        method.args = list(family = "binomial"),
        se = FALSE
      ) +
      geom_smooth(
        formula = y ~ x2, 
        mapping = aes(x = x2, y = y, linetype = "X2's fitted\n probability"), 
        data = df, 
        method = "glm", 
        linewidth = 1,
        color = "lightblue",
        method.args = list(family = "binomial"),
        se = FALSE
      ) +
      geom_ribbon(
        mapping = aes(linetype = "confidence\n interval"),
        stat = "smooth", 
        method = "glm", 
        alpha = 0.15,
        level = input$ci2,
        method.args = list(family = "binomial")
      ) +
      geom_point(color = "maroon") +
      geom_ribbon(
        mapping = aes(x = x2, y = y, linetype = "confidence\n interval"),
        data = df,
        stat = "smooth",
        method = "glm", 
        alpha = 0.15,
        level = input$ci2,
        method.args = list(family = "binomial")
      ) +
      geom_point(
        mapping = aes(x = x2, y = y),
        data = df,
        color = "lightblue",
        alpha = 0.4
      ) +
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
  }
)

output$multix <- renderPlot(
  expr = {
    input$goButtonMul
    df <- isolate(commonDf2())
    p <- glm(
      formula = y ~ x1 + x2,
      data = df,
      family = "binomial"
    )
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
  }
)

### Multiple Goodness of fit ----
HLresult2 <- function() {
  input$goButtonMul
  df <- isolate(commonDf2())
  mod <- glm(
    formula = y ~ x1 + x2,
    data = df,
    family = "binomial"
  )
  hl <- hoslem.test(mod$y, fitted(mod), g = 10)
  return(hl)
}

output$lemeshowDF2 <- renderTable(
  expr =  {
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
  expr = {
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