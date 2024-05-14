








if (!require("pacman")) install.packages("pacman")
pacman::p_load(pacman,dplyr,ggplot2,shiny,tableHTML)


# setwd("/Users/marcoslima/Zion/R/PlantBasedNutrition")

if(!exists("init_time")){
  cat("Loading scripts\n")
  source("R/recipes_nutrition.R")
  cat("Loading SR27 data\n")
  load_data_from_SR27()
  cat("Loading reference data\n")
  assign("ref_df",readRDS("data/ref.rds"),envir=.GlobalEnv)
  # nutrition_df %>% choose_nutrition_df(ref_df) %>% saveRDS("data/nutrition.rds")
  # nutrition_df %>% inner_join(ref_df %>% rename(food_des=Reference) %>% select(food_des),by="food_des") %>% saveRDS("data/nutrition.rds")
  # cat("Loading nutrition data\n")
  # assign("nutrition_df",readRDS("data/nutrition.rds"),envir=.GlobalEnv)
  assign("recipe_df",readRDS("data/recipe_homus.rds"),envir=.GlobalEnv)
  assign("init_time",Sys.time(),envir=.GlobalEnv)
} 

# body { 
#   background-color: black; 
#   color: white; 
# } 
ui <- fluidPage(
  tags$head(tags$style(HTML("       body { background-color: black; color: white; }
      /* this will affect only the pre elements under the class myclass */
      .prompt_style pre {
        color: lime;
        background-color: black;
        font-family: 'Lucida Console', 'Courier New', monospace;
        border: none;
        outline-width: 0;
      }
      .table>tbody>tr>td, .table>tbody>tr>th, .table>tfoot>tr>td, .table>tfoot>tr>th, .table>thead>tr>td, .table>thead>tr>th {
        padding: 8px;
        line-height: 0.8;
        vertical-align: top;
        background-color: antiquewhite;
        color: black;
        border: 2px gray; 
      }"))),
  tabsetPanel(
    tabPanel("| | |", imageOutput("dummy")),
    
    tabPanel("Reference",tableHTML_output("ingredients_table_HTML")),
             # tableHTML_output("ingredients_table_search_HTML")),
    
    tabPanel("Recipe",
             textAreaInput("recipe_input", "Ingredients and weights",
                           paste0("Write down your recipe and weights:\n","chickpea;200\nEggs;50;Egg, whole, cooked, fried\n"),height="200px",width="1000px"),
             div(class="prompt_style",verbatimTextOutput("value")),
             fluidRow(
               column(6,textInput("ingredients_search_input","Search",width="200px")),
               column(5,textInput("ingredients_recipe_name","Recipe Name",width="100%")),
               column(1,div(style="margin-top: 25px;",actionButton("ingredients_recipe_save","Save")))),
             #actionButton("ingredients_search_submit","Go"),
             hr(),tableOutput("ingredients_table_search_plot")),
    
    
    # tabPanel("Nutritional Label",plotOutput("nutritional_label",height="800px",width="300px"))
    tabPanel("Nutritional Label",uiOutput("nutritional_label_UI"))
    
  )
)



server <- function(input, output, session) {
  
  output$ingredients_table <- renderTable({ref_df %>% select(Group,Food,Reference)},colnames=T)
  output$ingredients_table_HTML <- render_tableHTML({
    ref_df %>% select(Group,Food,Reference) %>% 
      tableHTML(rownames=F) %>% 
      add_css_row(css = list(c('background-color', 'border'), c('antiquewhite', 'gray'))) %>% 
      add_css_conditional_column(columns=3,conditional = 'contains',
                                 value = "Chickpea",#c("Chickpea","Lentil"),
                                 css = list(c('background-color'),
                                            c('lightblue')))
  })
  
  
  
  
  ingredients_table_filtered <- eventReactive(input$ingredients_search_input,{
    # cat(paste(input$ingredients_search_submit,"\n"))
    # cat(paste("input search =",trimws(input$ingredients_search_input),"\n"))
    if(trimws(input$ingredients_search_input)=="") { df <- data.frame(food_des="Write your search word") #data.frame(message="Write your search word and click Go button") %>% tableHTML(rownames=F,headers="",border=0)
    } else {
      words <- strsplit(input$ingredients_search_input," ")[[1]]
      df <- nutrition_df %>% filter(grepl(words[1],food_des,ignore.case=T)) %>% distinct(food_des)
      # cat(paste("Search found",nrow(df),"records for the first word",words[1],"\n"))
      if(length(words)>=2) for(i in 2:length(words)) df <- df %>% filter(grepl(words[i],food_des,ignore.case=T))
    }
    # cat(paste("Search returned",nrow(df),"records","\n"))
    df %>% rename(Reference=food_des)
  })
  # output$ingredients_table_search_HTML <- render_tableHTML({
  #   # hist(ingredients_table_filtered())
  #   cat(paste("render_tableHTML","\n"))
  #   cat(paste(input$ingredients_search_submit,"\n"))
  #   ingredients_table_filtered() %>% tableHTML(rownames=F) %>% add_css_row(css = list(c('background-color', 'border'), c('antiquewhite', 'gray')))
  # })
  output$ingredients_table_search_plot <- renderTable({ ingredients_table_filtered() })
  
  
  
  
  output$value <- renderText({ 
    # input$recipe_input="Write down your recipe and weights:\nchickpea;200\n"
    # print(input$caption)
    # data tratment to input recepy
    
    lines <- strsplit(input$recipe_input,"\n")[[1]]
    input_treatment <- NULL
    for(i in seq(lines)){
      if(grepl(";",lines[i])){
        current_line <- lines[i]
        # s <- strsplit(current_line," ")[[1]]
        # current_line <- paste(toupper(substring(s,1,1)),substring(s,2),sep="",collapse=" ")
        s <- strsplit(current_line,";")[[1]]
        f <- strsplit(s[1]," ")[[1]]
        food <- trimws(paste(toupper(substring(f,1,1)),substring(f,2),sep="",collapse=" "))
        size=gsub("\\D","",s[2])
        if(length(s)>2 & trimws(s[3])!="") { ref=(nutrition_df %>% filter(food_des==trimws(s[3])) %>% head(1))$food_des 
        } else { ref=(ref_df %>% filter(Food==food) %>% head(1))$Reference }
        if(length(ref)==0) ref="Reference not found"
        current_line <- paste(food,size,ref,sep=";")
        input_treatment <- append(input_treatment,current_line)
      }
    }
    if(length(input_treatment)>0){
      split_elements <- strsplit(input_treatment,";")
      assign("recipe_df",
             suppressWarnings(data.frame(Food=sapply(split_elements,`[`,1),Size=as.numeric(sapply(split_elements,`[`,2)),Reference=(sapply(split_elements,`[`,3)))) %>% 
               mutate(Group="Recipe"),# %>% left_join(ref_df %>% select(Food,Reference),by="Food"),
             envir=.GlobalEnv)
    }
    paste(input_treatment,colapse="\n")
  })
  
  
  
  
  
  
  
  
  # white;50;Egg, white, raw, fresh
  # yolk;50;Egg, yolk, raw, fresh
  # whole;50;Egg, whole, raw, fresh
  # Eggs;50;Egg, whole, cooked, fried
  
  output$nutritional_label <- renderPlot({ 
    print(input$recipe_input)
    suppressWarnings(recipe_df %>% plot_nutrition_label(1,nutrition_df,T)) })
  output$nutritional_heatmap <- renderPlot({ 
    print(input$recipe_input)
    choose_nutrition_df(nutrition_df,recipe_df) %>% mutate(Group_factor=Group) %>% plot_nutrition_value_heatmap() })
  nutritional_heatmap_height <- eventReactive(input$recipe_input,{ as.numeric(pmin(2000,nrow(recipe_df %>% distinct(Food))*10+200)) })
  output$nutritional_label_UI <- renderUI({ 
    print(nutritional_heatmap_height())
    fluidRow(
    column(5,plotOutput("nutritional_label",height="800px",width="300px")),
    column(7,plotOutput("nutritional_heatmap",height=paste0(nutritional_heatmap_height(),"px")))
  )})
  
}


# runApp(shinyApp(ui = ui, server = server), host = "0.0.0.0")
# shinyApp(ui = ui, server = server)