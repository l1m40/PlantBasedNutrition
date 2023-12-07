# File:   
# Description: 

# INSTALL AND LOAD PACKAGES ################################
#library(datasets)  # Load base packages manually
# Installs pacman ("package manager") if needed
if (!require("pacman")) install.packages("pacman")
# Use pacman to load add-on packages as desired
# Packages I load every time; uses "pacman"
pacman::p_load(pacman,rio,tidyverse) 

# CODING ###################################################


load_data_from_SR27 <- function(){
  # Source: https://www.ars.usda.gov/northeast-area/beltsville-md-bhnrc/beltsville-human-nutrition-research-center/methods-and-application-of-food-composition-laboratory/mafcl-site-pages/sr11-sr28/
  # User Guide: https://www.ars.usda.gov/ARSUserFiles/80400535/DATA/sr27/sr27_doc.pdf
  sr27_path="data/sr27asc/"
  fd_group_df <- import(paste0(sr27_path,"FD_GROUP.txt"),sep="~")
  colnames(fd_group_df)=c("a1","group_id","b1","group_name","c1")
  food_des_df <- import(paste0(sr27_path,"FOOD_DES.txt"),sep="~")
  colnames(food_des_df)=c("a1","food_id","b1","group_id","c1","food_des","d1","food_des_2","z1","z2","z3","z4","z5","flag1","y1","y2","refuse","y4","y5")
  food_des_df$category <- sapply(food_des_df$food_des,function(input){return(trimws(strsplit(input,",")[[1]][1]))})
  #food_des_df %>% group_by(category) %>% summarise(N=n())
  
  nut_data_df <- import(paste0(sr27_path,"NUT_DATA.txt"),sep="~")
  colnames(nut_data_df)=c("a1","food_id","b1","nutr_id","data","nutr_int1","d1","nutr_acrn1","e1","e2","e3","e4","data_2","f1","f2")
  
  nutr_def_df <- import(paste0(sr27_path,"NUTR_DEF.txt"),sep="~")
  colnames(nutr_def_df)=c("a1","nutr_id","b1","metric","c1","def_capital","d1","nutr_def","e1","int1","f1","int2","z1")
  
  nutrition_df <<- nut_data_df %>% 
    left_join(food_des_df,by="food_id") %>% 
    left_join(nutr_def_df,by="nutr_id") %>% 
    left_join(fd_group_df,by="group_id") %>% 
    mutate(nutr_value=sapply(str_split(data,"\\^"),`[`, 2)) %>% mutate(nutr_value=as.double(nutr_value)) %>% 
    mutate(other_compositions=((nutr_def=="Energy" & metric=="kcal") | (nutr_def %in% c("Water","Sugars, total","Fiber, total dietary")))) %>% 
    mutate(macronutrients=(nutr_def %in% c("Protein","Total lipid (fat)","Carbohydrate, by difference"))) %>% mutate(nutr_def=ifelse(nutr_def=="Total lipid (fat)","Fat",ifelse(nutr_def=="Carbohydrate, by difference","Carbo",nutr_def))) %>% 
    mutate(cbo=(nutr_def %in% c("Calcium, Ca","Iron, Fe","Magnesium, Mg","Phosphorus, P","Potassium, K","Sodium, Na","Zinc, Zn","Copper, Cu","Fluoride, F","Manganese, Mn","Selenium, Se"))) %>% 
    mutate(vitamin=grepl("Vitamin",nutr_def)) %>% 
    mutate(composition=ifelse(macronutrients,"Macro",ifelse(cbo,"CBO",ifelse(vitamin,"Vitamin",ifelse(other_compositions,"Other","NA"))))) %>% filter(composition!="NA") %>% 
    mutate(composition=factor(composition,levels=c("NA","Other","Macro","CBO","Vitamin"))) %>% 
    ungroup()
}

load_reference_data <- function(){ ref_df <<- import("data/food_reference_data.txt") }





