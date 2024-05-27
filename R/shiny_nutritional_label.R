








if (!require("pacman")) install.packages("pacman")
pacman::p_load(pacman,dplyr,ggplot2,shiny,tableHTML)


# setwd("/Users/marcoslima/Zion/R/PlantBasedNutrition")

# if(!exists("data_path")) data_path="data"

if(!exists("init_time")){
  cat("Loading scripts\n")
  source("R/recipes_nutrition.R")
  cat("Loading SR27 data\n")
  load_data_from_SR27()
  cat("Loading reference data\n")
  load_reference_data()
  # assign("ref_df",load_reference_data(),envir=.GlobalEnv)
  # nutrition_df %>% choose_nutrition_df(ref_df) %>% saveRDS("data/nutrition.rds")
  # nutrition_df %>% inner_join(ref_df %>% rename(food_des=Reference) %>% select(food_des),by="food_des") %>% saveRDS("data/nutrition.rds")
  # cat("Loading nutrition data\n")
  # assign("nutrition_df",readRDS("data/nutrition.rds"),envir=.GlobalEnv)
  assign("recipe_df",readRDS(file.path(data_path,"shiny_recipes/recipe_homus.rds")),envir=.GlobalEnv)
  assign("init_time",Sys.time(),envir=.GlobalEnv)
  
  cat("Initialization done!\n")
}
recipe_vector <- function(){ return(c("",str_remove(dir(file.path(data_path,"shiny_recipes")),".rds"))) }







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
      .rightAlign { float:right; }
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
    
    tabPanel("Recipe",
             fluidRow(
               column(6,div()),
               column(6,div(class="rightAlign",fluidRow(
                 column(9,selectInput("ingredients_recipe_load","Load recipe:",choices=recipe_vector())),
                 column(1,div(style="margin-top: 25px;",actionButton("ingredients_recipe_load_action","Load")))),
                 div(class="prompt_style",verbatimTextOutput("ingredients_recipe_load_selection"))))),
             textAreaInput("recipe_input", "Ingredients and weights",
                           paste0("Write down your recipe and weights:\n","chickpea;200\nEggs;50;Egg, whole, cooked, fried\n"),height="200px",width="1000px"),
             div(class="prompt_style",verbatimTextOutput("value")),
             fluidRow(
               column(6,textInput("ingredients_search_input","Search",width="200px")),
               column(6,div(class="rightAlign",fluidRow(
                 column(7,textInput("ingredients_recipe_name","Recipe Name",width="100%")),
                 column(1,div(style="margin-top: 25px;",actionButton("ingredients_recipe_save","Save")))),
                 div(class="prompt_style",verbatimTextOutput("ingredients_recipe_save_message"))))),
             hr(),tableOutput("ingredients_table_search_plot")),
    
    tabPanel("Nutritional Label",
             sliderInput("nutritional_label_serving_slider","Select Serving Size",0.1,2,1,0.05),
             uiOutput("nutritional_label_UI"),br(),br(),hr())
    
  )
)



server <- function(input, output, session) {
  
  output$ingredients_table <- renderTable({ref_df %>% select(Group,Food,Reference)},colnames=T)
  output$ingredients_table_HTML <- render_tableHTML({
    ref_df %>% select(Group,Food,Reference) %>% 
      tableHTML(rownames=F) %>% 
      add_css_row(css = list(c('background-color','border','color'), c('antiquewhite','gray','black'))) %>% 
      add_css_conditional_column(columns=3,conditional = 'contains',
                                 value = "Chickpea",#c("Chickpea","Lentil"),
                                 css = list(c('background-color'),
                                            c('lightblue')))
  })
  
  
  
  
  ingredients_table_filtered <- eventReactive(input$ingredients_search_input,{
    if(trimws(input$ingredients_search_input)=="") { df <- data.frame(food_des="Write your search word") #data.frame(message="Write your search word and click Go button") %>% tableHTML(rownames=F,headers="",border=0)
    } else {
      words <- strsplit(input$ingredients_search_input," ")[[1]]
      df <- nutrition_df %>% filter(grepl(words[1],food_des,ignore.case=T)) %>% distinct(food_des)
      if(length(words)>=2) for(i in 2:length(words)) df <- df %>% filter(grepl(words[i],food_des,ignore.case=T))
    }
    df %>% rename(Reference=food_des)
  })
  output$ingredients_table_search_plot <- renderTable({ ingredients_table_filtered() })
  
  
  
  
  
  
  
  
  ingredients_recipe_load <- eventReactive(input$ingredients_recipe_load_action,{
    filename=paste0(input$ingredients_recipe_load,".rds")
    print(paste("load from",filename))
    df <- readRDS(file.path(data_path,"shiny_recipes",paste0(input$ingredients_recipe_load,".rds")))
    updateTextAreaInput(session,"recipe_input",value=paste(paste(df$Food,df$Size,df$Reference,sep=";"),collapse="\n"))
    return(input$ingredients_recipe_load)
  })
  output$ingredients_recipe_load_selection <- renderText({ ingredients_recipe_load() })
  ingredients_recipe_save <- eventReactive(input$ingredients_recipe_save,{
    print(nrow(recipe_df))
    if(nrow(recipe_df)==0) return("no data to be")
    recipe_df %>% saveRDS(file.path(data_path,"shiny_recipes",paste0(input$ingredients_recipe_name,".rds")))
    updateSelectInput(session,"ingredients_recipe_load",choices=recipe_vector())
    return(input$ingredients_recipe_name)
  })
  output$ingredients_recipe_save_message <- renderText({ paste(ingredients_recipe_save(),"saved") })
  
  
  
  
  
  
  
  
  
  
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
    paste(input_treatment,collapse="\n")
  })
  
  
  
  
  
  
  
  
  output$nutritional_size <- renderPlot({ 
    recipe_df %>% summarise(Size=sum(Size)) %>% ggplot()+
      geom_text(color="white",vjust=0,hjust=0,size=6,fontface=2,aes(x=1,y=1,label=paste0("Total Size: ",sprintf("%0.0f",Size),"g")))+
      geom_text(color="white",vjust=0,hjust=1,size=6,fontface=2,aes(x=2,y=1,label=paste0("Serving Size: ",sprintf("%0.0f",Size*input$nutritional_label_serving_slider),"g")))+
      theme_void()+theme(plot.background=element_rect(fill="black",colour="black"))
  })
  output$nutritional_ingredients <- renderPlot({ 
    recipe_df %>% mutate(ratio=Size/max(Size)) %>% arrange((ratio)) %>% mutate(i=row_number()) %>% 
      ggplot(aes(x=i))+geom_col(aes(y=ratio),alpha=.3)+
      geom_text(aes(y=0.1,label=paste0(Food," ",sprintf("%0.0f",Size),"g")),hjust=0)+
      coord_flip()+theme_void()+theme(legend.position="none")
  })
  output$nutritional_label <- renderPlot({ 
    print(input$recipe_input)
    suppressWarnings(recipe_df %>% plot_nutrition_label(input$nutritional_label_serving_slider,nutrition_df,T)) })
  output$nutritional_heatmap <- renderPlot({ 
    print(input$recipe_input)
    choose_nutrition_df(nutrition_df,recipe_df) %>% mutate(Group_factor=Group,order=-Size) %>% filter(!is.na(value_zscore)) %>%  
      plot_nutrition_value_heatmap()+labs(fill="strenght")+theme(legend.position="bottom")
  })
  nutritional_heatmap_height <- eventReactive(input$recipe_input,{ as.numeric(pmin(2000,nrow(recipe_df %>% distinct(Food))*15)) })
  output$nutritional_label_UI <- renderUI({ 
    #print(nutritional_heatmap_height())
    #fluidRow(br(),br())
    
    fluidRow(
      plotOutput("nutritional_size"         ,width="350px",height="50px"),
      plotOutput("nutritional_label"        ,width="350px",height="800px"),
      plotOutput("nutritional_heatmap"      ,width="350px",height=paste0(nutritional_heatmap_height()+180,"px")),
      plotOutput("nutritional_ingredients"  ,width="350px",height=paste0(nutritional_heatmap_height(),"px"))
      )
    
    # fluidRow(
    # column(1,plotOutput("")),
    # column(5,plotOutput("nutritional_label"  ,width="300px",height="800px")),
    # column(6,plotOutput("nutritional_heatmap",width="300px",height=paste0(nutritional_heatmap_height(),"px"))))
  })
  
}


# runApp(shinyApp(ui = ui, server = server), host = "0.0.0.0")
# shinyApp(ui = ui, server = server)