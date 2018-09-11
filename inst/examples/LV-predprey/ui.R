################################
# Lotka Volterra Predator-Prey #
################################
# ui.R file for Shiny app
# gaurav kandlikar, gaurav.kandlikar@gmail.com

# start defining the page
shinyUI(fluidPage(
  
  titlePanel("Modeling Lotka Volterra Predator-Prey relations"),
  
  sidebarLayout(
    sidebarPanel(
        tabPanel(title="Lotka Volterra Predator-Prey",
                 helpText(h3("Set parameters for the L-V predator-prey model.")),
                 
                 helpText(h4("Set starting population sizes")),
                 numericInput("N",label=p("Select a value for", span("N", style = "color:black"),"(starting pop of prey)"),value=25,min=1,max=50),
                 numericInput("P",label=p("Select a value for", span("P", style = "color:red"), "(starting pop of predator)"),value=10,min=1,max=25),
                 br(),
                 helpText(h4("Set prey carrying capacity")),
                 radioButtons("Prey_K", label="",
                              choices = list("No prey carrying capacity" = 1,
                                             "Set prey carrying capacity" = 2),
                              selected = 1),
                 # If users select "Prey carrying capacity" above, then generate the input option
                 htmlOutput("UIpreyk"),
                 br(),
                 helpText(h4("Set demographic parameters")),
                 sliderInput("r", 
                             label = "Choose a value for r (prey intrinsic growth rate)",
                             min = 0.01, max = 1.99, value=1.0, step = NULL),
                 sliderInput("a", 
                             label = "Choose a value for a (predation efficiency)",
                             min = .0001, max = .5, value=.1, step = NULL),
                 sliderInput("d", 
                             label = "Choose a value for d (predator death rate)",
                             min = 0.01, max = 1, value=0.6, step = NULL),
                 sliderInput("b", 
                             label = "Choose a value for b (conversion efficiency)",
                             min = 0.01, max = 1, value=.5, step = NULL),
                 
                 numericInput("time",label="Number of time steps to run the model",value = 100, min=1),
                 actionButton("goButton", "Go!")
                 )
      ),
    mainPanel(fluidRow(
                   column(width = 6,
                          plotOutput("plot1", height = 400)),
                   column(width = 6,
                          plotOutput("plot2", height = 400))
  )
))))