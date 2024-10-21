library(tidyverse)
library(shiny)

machine1 <- data.frame(outcomes = 0:5, 
                       probs = c(0.2, 0.4, 0.1, 0.1, 0, 0.2))

machine2 <- data.frame(outcomes = 0:5,
                       probs = c(0.3, 0.1, 0, 0.3, 0.3, 0))

machine3 <- data.frame(outcomes = 0:5,
                       probs = c(0.1, 0.3, 0.2, 0.1, 0, 0.1))

ucb <- function(vec, total) mean(vec) + sqrt((2*log(length(vec)))/length(total))

set.seed(sum(utf8ToInt("590")))

ui <- fluidPage(
  
    titlePanel("Multi Armed Bandit Simulator"),
    
    fluidRow(
      
      column(3, 
        wellPanel(
           textInput("seed", "Enter Seed:", "590"), 
           actionButton("reset", "Reset")
        ),
        wellPanel(
          h4("Press Button to Roll Machine"),
          actionButton("m1", "Roll Machine 1", style = "color: black; background-color: #FFFFB3; border-color: #FFFFB3"),
          actionButton("m2", "Roll Machine 2", style = "color: black; background-color: #BEBADA; border-color: #BEBADA"),
          actionButton("m3", "Roll Machine 3", style = "color: black; background-color: #80B1D3; border-color: #80B1D3"),
          textOutput("uses"),
        )
      ),
      
      column(9, plotOutput("totalhist")),
    ),
    
    fluidRow(
      
      column(4, plotOutput("m1hist")),
      
      column(4, plotOutput("m2hist")), 
      
      column(4, plotOutput("m3hist"))
      
    ),
    
    fluidRow(
      column(3,
        wellPanel(
          h4("Epsilon Greedy Exploration vs. Exploitation Machine Selecter"),
          sliderInput("gprob", "Greedy Probability", min = 0, max = 1, value = 0.3, step = 0.01),
          actionButton("sim", "Simulate"),
          textOutput("outcome")
          )
        ),
      
      column(9, 
             wellPanel(
               h4("Thompson Sampling Machine Selecter (Beta Binomial)"),
               fluidRow(
                 column(4, textInput("a1", "Machine 1 Alpha:")),
                 column(4, textInput("b1", "Machine 1 Beta:"))
               ),
               fluidRow(
                 column(4, textInput("a2", "Machine 2 Alpha:")),
                 column(4, textInput("b2", "Machine 2 Beta:"))
               ),
               fluidRow(
                 column(4, textInput("a3", "Machine 3 Alpha:")),
                 column(4, textInput("b3", "Machine 3 Beta:"))
               ),
               actionButton("sim_ts", "Simulate"),
               textOutput("ev")
             ))

    )
)

server <- function(input, output) {
  
  observe(set.seed(sum(utf8ToInt(input$seed))))
  
  obj <- reactiveValues(tc = c(), c1 = c(), c2 = c(), c3 = c())
  
  res <- reactiveVal(TRUE)
  
  observeEvent(input$m1, {
    
    newval <- sample(machine1$outcomes, 1, prob = machine1$probs)
    
    obj$tc <- c(obj$tc, newval)
    
    output$totalhist <- renderPlot({
      req(length(obj$tc) != 0)
      
      ggplot(data.frame(t = obj$tc), aes(x = t)) +
        geom_histogram(binwidth = 1, color = "black", fill = "#8DD3C7") +
        scale_x_continuous(limits = c(-1, 6), breaks = seq(0, 5, by = 1)) +
        scale_y_continuous(breaks = seq(0, 100, by = 1)) + 
        labs(x = "Total Outputs", 
             title = "Total Results Histogram", 
             subtitle =  paste0("Mean: ", round(mean(obj$tc), 2), ", ", "SD: ", round(sd(obj$tc), 2), ", Total: ", sum(obj$tc), ", Most Recent: ", tail(obj$tc, 1))) + 
        theme_bw()
    })
    
    obj$c1 <- c(obj$c1, newval)
    
    output$m1hist <- renderPlot({
      req(length(obj$c1) != 0)
      
      ggplot(data.frame(m1 = obj$c1), aes(x = m1)) +
        geom_histogram(binwidth = 1, color = "black", fill = "#FFFFB3") +
        geom_vline(xintercept = ucb(obj$c1, obj$tc), color = "red", linetype = "dashed", size = 1) + 
        annotate("text", x = ucb(obj$c1, obj$tc) + 0.2, y = 0.1, label = "UCB", color = "red") +
        scale_x_continuous(limits = c(-1, 6), breaks = seq(0, 5, by = 1)) +
        scale_y_continuous(breaks = seq(0, 100, by = 1)) + 
        labs(x = "Machine 1 Outputs", 
             title = "Machine 1 Results Histogram", 
             subtitle =  paste0("Mean: ", round(mean(obj$c1), 2), ", SD: ", round(sd(obj$c1), 2), ", UCB: ", round(ucb(obj$c1, obj$tc), 2), " , Count: ", length(obj$c1), ", Most Recent: ", tail(obj$c1, 1))) + 
        theme_bw()
    })
    
    output$uses <-  renderText({
      req(length(obj$tc) != 0)
      paste("Total Number of Rolls:", length(obj$tc))
    })
    
  })
  
  observeEvent(input$m2, {
    
    newval <- sample(machine2$outcomes, 1, prob = machine2$probs)
    
    obj$tc <- c(obj$tc, newval)
    
    output$totalhist <- renderPlot({
      req(length(obj$tc) != 0)
      
      ggplot(data.frame(t = obj$tc), aes(x = t)) +
        geom_histogram(binwidth = 1, color = "black", fill = "#8DD3C7") +
        scale_x_continuous(limits = c(-1, 6), breaks = seq(0, 5, by = 1)) +
        scale_y_continuous(breaks = seq(0, 100, by = 1)) + 
        labs(x = "Total Outputs", 
             title = "Total Results Histogram", 
             subtitle =  paste0("Mean: ", round(mean(obj$tc), 2), ", ", "SD: ", round(sd(obj$tc), 2), ", Total: ", sum(obj$tc), ", Most Recent: ", tail(obj$tc, 1))) + 
        theme_bw()
    })
  
    
    obj$c2 <- c(obj$c2, newval)
    
    output$m2hist <- renderPlot({
      req(length(obj$c2) != 0)
      
      ggplot(data.frame(m2 = obj$c2), aes(x = m2)) +
        geom_histogram(binwidth = 1, color = "black", fill = "#BEBADA") +
        geom_vline(xintercept = ucb(obj$c2, obj$tc), color = "red", linetype = "dashed", size = 1) + 
        annotate("text", x = ucb(obj$c2, obj$tc) + 0.2, y = 0.1, label = "UCB", color = "red") +
        scale_x_continuous(limits = c(-1, 6), breaks = seq(0, 5, by = 1)) +
        scale_y_continuous(breaks = seq(0, 100, by = 1)) + 
        labs(x = "Machine 2 Outputs", 
             title = "Machine 2 Results Histogram", 
             subtitle =  paste0("Mean: ", round(mean(obj$c2), 2), ", SD: ", round(sd(obj$c2), 2), ", UCB: ", round(ucb(obj$c2, obj$tc), 2), " , Count: ", length(obj$c2), ", Most Recent: ", tail(obj$c2, 1))) + 
        theme_bw()
    })
    
    output$uses <-  renderText({
      req(length(obj$tc) != 0)
      paste("Total Number of Rolls:", length(obj$tc))
    })
    
  })
  
  observeEvent(input$m3, {
    
    newval <- sample(machine3$outcomes, 1, prob = machine3$probs)
    
    obj$tc <- c(obj$tc, newval)
    
    output$totalhist <- renderPlot({
      req(length(obj$tc) != 0)
      
      ggplot(data.frame(t = obj$tc), aes(x = t)) +
        geom_histogram(binwidth = 1, color = "black", fill = "#8DD3C7") +
        scale_x_continuous(limits = c(-1, 6), breaks = seq(0, 5, by = 1)) +
        scale_y_continuous(breaks = seq(0, 100, by = 1)) + 
        labs(x = "Total Outputs", 
             title = "Total Results Histogram", 
             subtitle =  paste0("Mean: ", round(mean(obj$tc), 2), ", ", "SD: ", round(sd(obj$tc), 2), ", Total: ", sum(obj$tc), ", Most Recent: ", tail(obj$tc, 1))) + 
        theme_bw()
    })
    
    
    obj$c3 <- c(obj$c3, newval)
    
    output$m3hist <- renderPlot({
      req(length(obj$c3) != 0)
      
      ggplot(data = data.frame(m3 = obj$c3), aes(x = m3)) +
        geom_histogram(binwidth = 1, color = "black", fill = "#80B1D3") +
        geom_vline(xintercept = ucb(obj$c3, obj$tc), color = "red", linetype = "dashed", size = 1) + 
        annotate("text", x = ucb(obj$c3, obj$tc) + 0.2, y = 0.1, label = "UCB", color = "red") +
        scale_x_continuous(limits = c(-1, 6), breaks = seq(0, 5, by = 1)) +
        scale_y_continuous(breaks = seq(0, 100, by = 1)) + 
        labs(x = "Machine 3 Outputs", 
             title = "Machine 3 Results Histogram", 
             subtitle =  paste0("Mean: ", round(mean(obj$c3), 2), ", SD: ", round(sd(obj$c3), 2), ", UCB: ", round(ucb(obj$c3, obj$tc), 2), " , Count: ", length(obj$c3), ", Most Recent: ", tail(obj$c3, 1))) + 
        theme_bw()
    })
    
    output$uses <-  renderText({
      req(length(obj$tc) != 0)
      paste("Total Number of Rolls:", length(obj$tc))
      })
    
  })
  
  observeEvent(input$reset, {
    
    obj$tc <- c()
    obj$c1 <- c()
    obj$c2 <- c()
    obj$c3 <- c()
    
  })
  
  observeEvent(input$sim, {
    output$outcome <- renderText(ifelse(input$gprob < runif(1), paste("Explore by picking:", sample(c("Machine 1", "Machine 2", "Machine 3"), 1)), "Exploit"))

  })
  
  observeEvent(input$sim_ts, {
    m1val <- rbeta(1, as.numeric(input$a1), as.numeric(input$b1)) 
    m2val <- rbeta(1, as.numeric(input$a2), as.numeric(input$b2)) 
    m3val <- rbeta(1, as.numeric(input$a3), as.numeric(input$b3)) 
    
    if (m1val > m2val & m1val > m3val) output$ev <- renderText("Choose Machine 1")
    else if (m2val > m1val & m2val > m3val) output$ev <- renderText("Choose Machine 2")
    else if (m3val > m1val & m3val > m2val) output$ev <- renderText("Choose Machine 3")
  })

}

# Run the application 
shinyApp(ui = ui, server = server)
