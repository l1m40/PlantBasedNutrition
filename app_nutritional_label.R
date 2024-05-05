
source("R/shiny_nutritional_label.R",local=T)
options(shiny.port = 3941)
options(shiny.host = "0.0.0.0")
shinyApp(ui = ui, server = server)