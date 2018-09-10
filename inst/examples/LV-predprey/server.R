# Lotka volterra predator prey shiny app
# server.R
# gaurav kandlikar, gaurav.kandlikar@gmail.com

library(deSolve) # package for solving diferential equation

######################################################## 
# Global options - define the predator - prey function #
######################################################## 

lvpp <- function(pp.time,pp.init,pp.params) {
  with (as.list(c(pp.time,pp.init,pp.params)), {
    
    # Parameters
    # N = prey population size; P = predator population size
    # r = intrinsic growth rate of prey
    # a = predation efficiency
    # b = conversion efficiency of prey into predator
    # d = intrinsic dseath rate of predator
    # prey_k = carrying capacity for prey; only used if user-defined

    # if no carrying capacity for prey, use this equation
    if (pp.params["prey_k"] == -9999 ){
      dNdt <- (r*N) - (a*N*P)
    }
    
    # else use the defined prey carrying capacity to compute the values.
    else {
      dNdt <- ((r*N)*(1-(N/prey_k))) - (a*N*P)
    }
    
    dPdt <- (b*a*N*P) - (d*P)
    
    # return the vectors as a list
    return(list(c(dNdt,dPdt)))
  })
}

### Reactive code

pp.init<- c()
pp.params <- c()
shinyServer(
  function(input, output) {
    
  
    # Generate carrying capacity option if user selects 'set carrying capacity' option
    output$UIpreyk <- renderUI({
      if (input$Prey_K == 2){
      numericInput("K",label=h6("Select a value for K (prey's carrying capacity)."),value=500,min=1)}
      else {""}      
    })
    
    # Global setup ----------
    pp.params <- reactive({
      input$goButton
      isolate(c("r" = input$r, "a" = input$a, "d" = input$d, "b" = input$b, "prey_k" = ifelse(input$Prey_K == 2, input$K, -9999)))
    })
    
    pp.init <- reactive({
      input$goButton
      isolate(c("N" = input$N, "P" = input$P))
    })
    
    pp.time <- reactive({
      input$goButton
      isolate(seq(0,input$time,by=1))
    })
    
    param_text <- reactive({
      pp.params <- pp.params()
      pp.init <- pp.init()
      
      paste("N = ", pp.init["N"], ", P = ", pp.init["P"], ", r = ", pp.params["r"], ", a = ", pp.params["a"], ", d = ", pp.params["d"], ", b = ", pp.params["b"], ", Prey K = ", ifelse(input$Prey_K == 2, pp.params["prey_k"], "None"), sep = "")
    })
    
    full_table<- reactive({
      pp.time <- pp.time()
      pp.params <- pp.params()
      pp.init <- pp.init()

      # Return the table!
      floor(as.data.frame(ode(func=lvpp,y=pp.init,parms=pp.params,times=pp.time)))
      
    })

    # Render outputs -----------    
    
    # Table tab
    output$table1 <- renderTable({      
      generated_df <- full_table()
      generated_df
    })

    # Plots
    ranges <- reactiveValues(x = NULL, y = NULL)
    
    plot1 <- reactive({
      pp.time <- pp.time()
      pp.params <- pp.params()
      pp.init <- pp.init()
      lvout <- full_table()
      # Begin plotting!
      
      plot(lvout$P~lvout$N,ylim=c(0,max(lvout$P)*1.25),xlim=c(0,max(lvout$N)*1.25),type="l",lwd=1.5,
           xlab="Prey population size",ylab="Predator population size", cex.lab = 1.25)
      points(x=pp.init["N"],y=pp.init["P"],col="#0072B2",pch=18,cex=2)
      
      abline(v=pp.params["d"]/(pp.params["b"]*pp.params["a"]))
      if (pp.params["prey_k"] == -9999) {
        abline(h=pp.params["r"]/pp.params["a"])
      }
      if (pp.params["prey_k"] != -9999) {
        abline (b=-(pp.params["r"]/(pp.params["prey_k"]*pp.params["a"])), a = pp.params["r"]/pp.params["a"])
      }
      
    })
    plot2 <- reactive({  
      pp.out <- full_table() %>% as.data.frame %>% gather(., which, population, -time)
      ggplot(pp.out) + 
        geom_line(aes(x = time, y = population, color = which), size = 1.5) + 
        scale_color_manual(values = c("#D55E00", "#0072B2")) + 
        theme_bw()
      
    })

    output$plot1<- renderPlot({plot1()})
    output$plot2 <- renderPlot({plot2()})

  }
)
    
