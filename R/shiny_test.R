library(shiny)

ui <- fluidPage(
  
  titlePanel("title"),
  
  sidebarLayout(
    sidebarPanel(
      checkboxInput("EF", "Efficient Frontier"),
      actionButton("Go", "Go", style="color: #fff; background-color: #337ab7; border-color: #2e6da4; margin: auto")
    ),
    
    mainPanel(
      fluidRow(
        align = "center",
        plotOutput("GraphEF")),
      tableOutput("EFWeightsTable")
    )
  )
)

server <- function(input, output) {
  
  OPw <- reactiveValues()
  observeEvent(input$Go, {
    
    if(input$EF){
      showModal(modalDialog("Loading... Please Wait", footer=NULL)) 
      OPw$LIST1 <- X(5,10,20)
    }
    removeModal() 
  })
  
  output$GraphEF <- renderPlot({ 
    OPw$LIST1[[1]]
  },height = 550, width = 700)
  
  output$EFWeightsTable <- renderTable(
    {
      # Here it seem OP use the wrong reference [[1]] where it should be [[2]]
      OPw$LIST1[[2]]
    }, colnames = TRUE
  )
  
  #Function
  X <- function(a,b,c){
    # Base R plot doesn't return a value but will always output to panel plot
    plot(c(1,2),c(3,4), type="l")
    # To save it to object for later use recordPlot() function after plot function
    PLOT1 <- recordPlot()
    DF1 <- data.frame(c(1,2),c(3,4))
    # Here only use list(...) instead of list(c(...))
    # c(...) convert all objects passed to it into vector - you don't want that.
    return(list(PLOT1,DF1))
  }
  
}

shinyApp(ui = ui, server = server)