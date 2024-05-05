








if (!require("pacman")) install.packages("pacman")
pacman::p_load(pacman,tidyverse,shiny,tableHTML,
               rvest,tidyquant,httr,jsonlite,gridExtra,grid,scales)


# setwd("/Users/marcoslima/Zion/R/PlantBasedNutrition")

if(!exists("init_time")){
  cat("Loading scripts\n")
  source("R/recipes_nutrition.R")
  cat("Loading SR27 data\n")
  load_data_from_SR27()
  cat("Loading REF data\n")
  assign("ref_df",readRDS("data/ref.rds"),envir=.GlobalEnv)
  assign("recipe_df",readRDS("data/recipe_homus.rds"),envir=.GlobalEnv)
  assign("init_time",Sys.time(),envir=.GlobalEnv)
} 


ui <- fluidPage(
  tags$head(tags$style(HTML(" body { background-color: black; } 
        .recalculating {opacity: 1.0;} 
      /* this will affect only the pre elements under the class myclass */
      .prompt_style pre {
        color: lime;
        background-color: black;
        font-family: 'Lucida Console', 'Courier New', monospace;
        border: none;
        outline-width: 0;
      }"))),
  tabsetPanel(
    tabPanel("| | |", imageOutput("dummy")),
    
    tabPanel("Input",
             textAreaInput("recipe_input", "Ingredients and weights",
                           paste0("Write down your recipe and weights:\n","chickpea,200\n"),height="200px",width="1000px"),
             div(class="prompt_style",verbatimTextOutput("value")),
             tableHTML_output("ingredients_table_HTML")),
    
    
    tabPanel("Nutritional Label",plotOutput("nutritional_label",height="800px",width="300px"))
    
  )
)




server <- function(input, output, session) {
  
  # setwd("/Users/marcoslima/Zion/R/PlantBasedNutrition")
  # print(getwd())
  
  # recipe_df <- readRDS("data/recipe_homus.rds")
  
  
  output$value <- renderText({ 
    # input$caption
    # input$caption="Write down your recipe and weights:\nchickpea,200\n"
    # print(input$caption)
    # data tratment to input recepy
    
    lines <- strsplit(input$recipe_input,"\n")[[1]]
    input_treatment <- NULL
    for(i in seq(lines)){
      if(grepl(",",lines[i])){
        current_line <- lines[i]
        s <- strsplit(current_line," ")[[1]]
        current_line <- paste(toupper(substring(s,1,1)),substring(s,2),sep="",collapse=" ")
        s <- strsplit(current_line,",")[[1]]
        current_line <- paste(trimws(s[1]),gsub("\\D","",s[2]),sep=",")
        input_treatment <- append(input_treatment,current_line)
      }
    }
    if(length(input_treatment)>0){
      split_elements <- strsplit(input_treatment,",")
      assign("recipe_df",
             suppressWarnings(data.frame(Food=sapply(split_elements,`[`,1),Size=as.numeric(sapply(split_elements,`[`,2)))) %>% 
               mutate(Group="Recipe") %>% 
               left_join(ref_df %>% select(Food,Reference),by="Food"),
             envir=.GlobalEnv)
    }
    paste(input_treatment,colapse="\n")
  })
  
  
  output$ingredients_table <- renderTable({ref_df %>% select(Group,Food,Reference)},colnames=T)
  output$ingredients_table_HTML <- render_tableHTML({
    ref_df %>% select(Group,Food,Reference) %>% 
      tableHTML(rownames=F) %>% 
      add_css_row(css = list(c('background-color', 'border'), c('antiquewhite', 'gray'))) %>% 
      # add_css_conditional_column(conditional = 'between',
      #                            between = c(0, 5),
      #                            css = list(c('background-color'),
      #                                       c('#f6f6f6')),
      #                            columns = 1:ncol(ref_df)) %>% 
      add_css_conditional_column(columns=3,conditional = 'contains',
                                 value = "Chickpea",#c("Chickpea","Lentil"),
                                 css = list(c('background-color'),
                                            c('lightblue')))
  })
  
  
  
  
  
  output$nutritional_label <- renderPlot({
    print(input$recipe_input)
    recipe_df %>% plot_nutrition_label(1,nutrition_df,T)
  })
  
}


# runApp(shinyApp(ui = ui, server = server), host = "0.0.0.0")
# shinyApp(ui = ui, server = server)