
# Select which columns to include in the analysis. Hit the run button and build a new model.

library(tidyverse)
library(shiny)

calcLift <- function(scored_data, cutoff = 0.5) {
  scored_data %>%
    mutate(bin = ntile(desc(pred), 10),
           prediction = ifelse(pred > cutoff, 1, 0)) %>%
    group_by(bin) %>%
    summarize(count = sum(prediction == label)) %>%
    mutate(prop = count / sum(count)) %>%
    arrange(bin) %>%
    mutate(prop = cumsum(prop)) %>%
    select(-count)
}

train <- read_csv("train.csv")
test <- read_csv("test.csv")


features <- tbl_vars(test) %>%
  .[-(. == "label")]

ui <- pageWithSidebar(
  HTML("<center><h1> Predicting Income From Census Data</h1></center> \n <center><h2> Is Your Income >50K? </h2></center>"),
  sidebarPanel(
    selectizeInput('selfeatures', 'Select Features:', features, multiple = TRUE),
    sliderInput('cutoff', 'Cutoff Value', min = 0, max = 1, value = 0.5),
    actionButton('fit', "Build Model")
  ),
  mainPanel(
    plotOutput("lift"),
    div(id = "models")
  )
)

server <- function(input, output, session) {

  result <<- NULL

  observeEvent(input$fit, {
    withProgress(message = "Working...", value = 0.1, {
      incProgress(0.25, detail = "Fitting GLM Model")
      f <- paste("label ~ ", paste(input$selfeatures, collapse= "+"))
      model <- glm(as.formula(f), binomial, train)

      incProgress(0.5, detail = "Scoring Model")
      scored <- predict(model, test, type = "response")
      new <- test
      new$pred <- scored

      incProgress(0.75, detail = "Evaluating Model")
      label <- paste0(input$fit, "model")

      insertUI(selector = "#models",
        ui = HTML(paste(h3(f), tableOutput(label), collapse = "\n"))
      )

      output[[label]]  <- renderTable(rownames = TRUE, {
        new$prediction <- ifelse(new$pred > input$cutoff, 1, 0)
        conf_matrix <- matrix(data = rep(0,4), nrow = 2, ncol = 2)
        conf_matrix[1,1] <- sum(new$prediction == 0 & new$label == 0 )
        conf_matrix[1,2] <- sum(new$prediction == 1 & new$label == 0 )
        conf_matrix[2,1] <- sum(new$prediction == 0 & new$label == 1 )
        conf_matrix[2,2] <- sum(new$prediction == 1 & new$label == 1 )
        colnames(conf_matrix) <- c("Predicted <=50K", "Predicted >50K")
        rownames(conf_matrix) <- c("Actual <=50K", "Actual >50K")
        conf_matrix
      })

      output$lift <- renderPlot({

        result <<- new %>%
          calcLift(input$cutoff) %>%
          mutate(model = f) %>%
          rbind(result)

        result %>%
          ggplot(aes(x = bin, y = prop, color = model)) +
            geom_point() + geom_line() +
            labs(
              title = "Lift Chart",
              x = "",
              y = ""
            ) +
            geom_abline(slope = 1/10, intercept = 0, color = "black")
      })

    })
  })
}

shinyApp(ui = ui, server = server)
