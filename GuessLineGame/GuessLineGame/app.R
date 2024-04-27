# Galactic Linear Guessing Odyssey 
# 
# 🌟 Linear Regression Trendline 🌟
# 
# 🔮 Created by : SHREYASH SINGH (ZELZAR) 🧠
#


# Required lib 
library(shiny)
library(ggplot2)

# Function to generate random data
generate_random_data <- function(seed) {
  set.seed(seed)  
  data.frame(
    x = seq(1, 30, by = 1),
    y = 2 * seq(1, 30, by = 1) + rnorm(30, mean = 0, sd = 7)  
  )
}


# Initial slope value corresponding to 180 degrees
initial_slope <- tan(180 * pi / 180)

# Define UI
ui <- fluidPage(
  
  tags$head(
    tags$audio( #BGM for the app
      src = "wideSpace.mp3",
      type = "audio/mp3",
      autoplay = "autoplay",
      loop = "loop",
      style = "display:none;"
    )
  ),
  div(
    hr(),  # Add a horizontal line
    style = "text-align: center;",  # Center the content
    color ="#72A0C1",
    titlePanel("Welcome to the Galactic Guessing Odyssey! 🌌"),
  ),
    div(
      style ="text-align: center;",
      color ="#E6E6FA",
    h4("🎮 Unleash your inner Data Explorer 🎮"),
    h4("🔮 Created by: ZELZAR 🔮"),
    ),
 
  
  hr(),  # Add a horizontal line
  
  div(
    style = "text-align: center;",  # Center the content
    h4("🚀 Game Rules:"),
    p("1. The universe has scattered mysterious data points across the cosmos."),
    p("2. Your mission: Decipher the hidden pattern to find the best trend line"),
    p("3. Analyze the stars, unveil the secrets, and make your boldest guess for the elusive equation."),
    p("4. May the force of the dataset be with you as you venture into the unknown!")
  ),
  
  hr(),  # Add a horizontal line
  hr(),  # Add a horizontal line
  hr(),  # Add a horizontal line
  
  style = "background-color: #2E294E; color: #FFFFFF;",  # Set app background color
  
  sidebarLayout(
    sidebarPanel(
      style = "background-color: #3C366B; border-color: #2E294E; color: #FFFFFF;",  # Set sidebar panel colors
      sliderInput("slope", "Guess the Slope:", min = -5, max = 5, value = initial_slope, step = 0.1),
      sliderInput("intercept", "Guess the Intercept:", min = -5, max = 5, value = 0, step = 0.1),
      actionButton("submit_button", "Submit Guess"),
      actionButton("reset_button", "Reset Game"),
      tags$div(style = "height: 20px;"),  
      textOutput("feedback")
    ),
    
    mainPanel(
      style = "background-color: #3C366B; border-color: #2E294E; color: #FFFFFF;",  # Set sidebar panel colors
      
      plotOutput("scatterPlot"),
      
    )
  )
)

# Define server
server <- function(input, output, session) {
  # Reactive to store and update data
  
  data <- reactiveVal(generate_random_data(1))
  
  
  best_fit <- reactive({
    lm(y ~ x, data = data())
  })
  
  text_color <- reactiveVal("blue")  # Initialize text color
  output$feedback <- renderText({"ANSWER WILL BE SHOWN HERE"})  # Reset feedback text
  text_color("blue")
  
  observeEvent(input$reset_button, {
    data(generate_random_data(input$seed))
    output$feedback <- renderText({"ANSWER WILL BE SHOWN HERE"})  # Reset feedback text
    text_color("blue")  # Reset text color
    initial_slope <- tan(180 * pi / 180)
    
  })
  observeEvent(input$submit_button, {
    output$feedback <- renderText({
      if (input$submit_button > 0) {
        slope_error <- abs(input$slope - coef(best_fit())[2])
        intercept_error <- abs(input$intercept - coef(best_fit())[1])
        # Check if the guess is close
        if (slope_error < 0.5 && intercept_error < 0.5) {
          text_color("green")  # Green color for close guess
          feedback_text <- paste("YAY! That's correct! Reset for another GAME!\n",
                      "\nCorrect Slope:", round(coef(best_fit())[2], 2), "\n",
                       "Correct Intercept:", round(coef(best_fit())[1], 2))
        } else {
          text_color("red")  # Red color for not close guess
          feedback_text <- paste("Sorry, not CLOSE enough!!\n",
                "Correct Slope:", round(coef(best_fit())[2], 2), "\n",
                "Correct Intercept:", round(coef(best_fit())[1], 2))
        }
        
        # Return feedback text
        feedback_text
      } else {
        "ANSWER WILL BE SHOWN HERE"  # Hide feedback text after reset or changing seed
      }
    })
  })

  
  
  
  output$scatterPlot <- renderPlot({
    ggplot(data(), aes(x = x, y = y)) +
      geom_point(color = "white", size = 3, alpha = 0.8, shape = 18) +
      geom_abline(aes(intercept = input$intercept, slope = input$slope), color = text_color(), linetype = "solid", size = 1.5) +
      labs(title = "Linear Regression Trend Guessing Game",
           x = "X", y = "Y") +
      theme_minimal() +
      theme(panel.background = element_rect(fill = "#000000"),  # Set plot background color to black
            axis.text = element_text(color = "#3C366B"),  
            plot.title = element_text(color = "#3C366B", size = 20, face = "bold", hjust = 0.5),  # Centered title
            text = element_text(color = "#3C366B"))  
  })
}

# Run the application
shinyApp(ui, server)
