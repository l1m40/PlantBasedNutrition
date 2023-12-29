# File:   
# Description: 

# INSTALL AND LOAD PACKAGES ################################
#library(datasets)  # Load base packages manually
# Installs pacman ("package manager") if needed
if (!require("pacman")) install.packages("pacman")
# Use pacman to load add-on packages as desired
# Packages I load every time; uses "pacman"
pacman::p_load(pacman,rio,tidyverse,grid,gridExtra) 

# CODING ###################################################

source("R/sr27_source.R")


plot_sample_recipe <- function() {
  if(!exists("nutrition_df")) load_data_from_SR27()
  if(!exists("daily_df")) load_daily_value_nutrient()
  
  recipe_df <- import("data/recipes/naturalchef_vegan_quiche.txt") %>% mutate(Group="Recipe")
  recipe_df %>% plot_nutrition_label(1/12,nutrition_df)
}



plot_nutrition_label <- function(recipe_df,serving_size=(1/8),nutrition_df) {
  
  # main_nutr_def         <- c("Energy",  "Fat",      "Fatty acids, total saturated","Fatty acids, total trans","Cholesterol","Sodium, Na","Carbo",             "Fiber, total dietary","Sugars, total","Protein")
  # main_nutritrion_facts <- c("Calories","Total Fat","Saturated Fat",               "Trans Fat",               "Cholesterol","Sodium",    "Total Carbohydrate","Dietary Fiber",       "Sugars",       "Protein")
  main_nutr_def         <- c("Fat",      "Fatty acids, total saturated","Fatty acids, total trans","Cholesterol","Sodium, Na","Carbo",             "Fiber, total dietary","Sugars, total","Protein")
  main_nutritrion_facts <- c("Total Fat","Saturated Fat",               "Trans Fat",               "Cholesterol","Sodium",    "Total Carbohydrate","Dietary Fiber",       "Sugars",       "Protein")
  bad__nutritrion_facts <- c("Total Fat","Saturated Fat","Trans Fat","Cholesterol","Sodium")
  good_nutritrion_facts <- c("Dietary Fiber")
  
  df <- choose_nutrition_df(nutrition_df,recipe_df %>% select(Size,Food,Reference,Group)) %>% 
    filter(Group=="Recipe") %>% group_by(nutr_def,metric) %>% summarise(Value=sum(Value),portion=Value*serving_size,.groups="keep") %>% ungroup()
  
  plot_header <- recipe_df %>% summarise(Size=sum(Size)) %>% 
    ggplot()+geom_hline(aes(yintercept=+0.5),linewidth=3)+
    geom_text(aes(x=1,y=1,label=paste0("",sprintf("%0.0f",Size),"g")),hjust=0)+
    geom_text(aes(x=2,y=1,label=paste0("Serving Size: ",sprintf("%0.0f",Size*serving_size),"g")),hjust=1)+
    annotate("text",x=1.5,y=2,label="Nutrition Facts",vjust=1,size=9,fontface=2)+theme_void()
  
  plot_main_nutr_def <- df %>% filter(nutr_def %in% main_nutr_def) %>% 
    left_join(daily_df,by="nutr_def") %>% mutate(daily_percent=portion/daily_value) %>% 
    mutate(Nutrient=main_nutritrion_facts[match(nutr_def, main_nutr_def)]) %>%
    mutate(Nutrient=factor(Nutrient,levels=main_nutritrion_facts)) %>% arrange(Nutrient) %>% 
    mutate(bar_fill=factor(ifelse(Nutrient %in% bad__nutritrion_facts,2,ifelse(Nutrient %in% good_nutritrion_facts,1,0)))) %>% 
    mutate(i=row_number()) %>% 
    ggplot(aes(x=-1,y=-i))+
    geom_rect(aes(xmin=-daily_percent-ifelse(daily_percent==0,-.01,.1),xmax=.01,ymin=-i+.3,ymax=-i-.3,fill=bar_fill),alpha=.1)+
    scale_fill_manual(values=c("0"="gray","1"="green","2"="red"))+
    geom_hline(aes(yintercept=-i+.5),linetype="dotted",alpha=.5)+
    geom_hline(aes(yintercept=+0.5),linewidth=3)+
    geom_hline(aes(yintercept=-9.8),linewidth=3)+
    annotate("text",x=0,y=0,label="% Daily Value*",size=3,hjust=1)+
    geom_text(aes(x=ifelse(Nutrient %in% c("Saturated Fat","Trans Fat","Dietary Fiber","Sugars"),.1,0)-1,
                  #label=paste0("'",Nutrient,"', bold('",sprintf("%0.1f",portion),metric,"')")),
                  label=paste0(Nutrient,"   ",sprintf("%0.1f",portion),metric)),
              hjust=0,parse=F)+
    geom_text(aes(x=0,label=ifelse(is.na(daily_percent),"",sprintf("%0.0f%%",daily_percent*100))),hjust=1)+
    theme_void()+theme(legend.position="none")
  
  grid.arrange(
    plot_header,
    df %>% filter(nutr_def=="Energy") %>% ggplot()+geom_text(aes(x=2,y=0,label=sprintf("%0.0f",portion)),size=5,fontface=2,hjust=1,vjust=1)+annotate("text",x=1,y=0,label="Calories",size=5,fontface=2,hjust=0,vjust=1)+theme_void(),
    plot_main_nutr_def,
    heights=c(1,.5,5)
    
  )
  
  if(F){
    
    data.frame(from=main_nutr_def,to=main_nutritrion_facts)
    
    choose_nutrition_df(nutrition_df,recipe_df %>% select(Size,Food,Reference,Group)) %>% 
      filter(grepl("Sodium",nutr_def)) %>% arrange(-Value) %>% 
      select(Food,Value,Size,food_des,
             food_id,group_name,nutr_def,nutr_value,category) %>% head(20)
    
    
    
    df %>% #distinct(nutr_def) %>% filter(grepl("Fat",nutr_def))
      filter(!nutr_def %in% main_nutritrion_facts) %>% 
      group_by(nutr_def,metric) %>% summarise(Value=sum(Value)*.6,portion=Value/8) %>% arrange(desc(Value))
  }
  
  
  
}