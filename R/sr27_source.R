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
    mutate(other_compositions=((nutr_def=="Energy" & metric=="kcal") | (nutr_def %in% c("Water","Sugars, total","Fiber, total dietary","Cholesterol")) | grepl("Fatty acids",nutr_def))) %>% 
    mutate(macronutrients=(nutr_def %in% c("Protein","Total lipid (fat)","Carbohydrate, by difference"))) %>% mutate(nutr_def=ifelse(nutr_def=="Total lipid (fat)","Fat",ifelse(nutr_def=="Carbohydrate, by difference","Carbo",nutr_def))) %>% 
    mutate(cbo=(nutr_def %in% c("Calcium, Ca","Iron, Fe","Magnesium, Mg","Phosphorus, P","Potassium, K","Sodium, Na","Zinc, Zn","Copper, Cu","Fluoride, F","Manganese, Mn","Selenium, Se"))) %>% 
    mutate(vitamin=grepl("Vitamin",nutr_def)) %>% 
    mutate(composition=ifelse(macronutrients,"Macro",ifelse(cbo,"CBO",ifelse(vitamin,"Vitamin",ifelse(other_compositions,"Other","NA"))))) %>% filter(composition!="NA") %>% 
    mutate(composition=factor(composition,levels=c("NA","Other","Macro","CBO","Vitamin"))) %>% 
    ungroup()
}

load_reference_data <- function(){ ref_df <<- import("data/food_reference_data.txt") }

load_daily_value_nutrient <- function() {
  # Source https://www.fda.gov/media/135301/download?attachment
  daily_df <<- import("data/daily_value_nutrient.txt") %>% 
    filter(nutr_def!="") %>% 
    select(nutr_def,daily_value,daily_metric)
  
  if(F){ # data transformations kept only for coding studies
    df <- import("data/daily_value_nutrient_rawdata.txt",header=F)
    nutr_def_df <- import(paste0("data/sr27asc/","NUTR_DEF.txt"),sep="~")
    colnames(nutr_def_df)=c("a1","nutr_id","b1","metric","c1","def_capital","d1","nutr_def","e1","int1","f1","int2","z1")
    df <- df %>% mutate(nutr=V1,daily_value=gsub('[[:alpha:]]+', '', df$V2),daily_metric=gsub('[^[:alpha:]]', '', df$V2))
    df$nutr_def <- sapply(df$V1, function(x) nutr_def_df$nutr_def[which(grepl(x, nutr_def_df$nutr_def))[1]])
    df %>% export("data/daily_value_nutrient_1st_transform.txt")
    
    nutr_def_df %>% filter(grepl("alt",nutr_def_df$nutr_def))
    
    food_des_df %>% filter(grepl("alt",food_des))
    
    grepl("Calcium",nutr_def_df$nutr_def)
    
    df %>% mutate(x=nutr_def_df$nutr_def[min(grepl(V1,nutr_def_df$nutr_def))])
    
    nutr_def_df$nutr_def[match(V1, nutr_def_df$nutr_def)]
    
    df <- data.frame(search_for=c("Protein","Calcium","Vitamin A","does not exists","Folate"))
    nutr_list_str <- "Protein;Total lipid (fat);Carbohydrate, by difference;Ash;Energy;Starch;Sucrose;Glucose (dextrose);Fructose;Lactose;Maltose;Alcohol, ethyl;Water;Adjusted Protein;Caffeine;Theobromine;Energy;Sugars, total;Galactose;Fiber, total dietary;Calcium, Ca;Iron, Fe;Magnesium, Mg;Phosphorus, P;Potassium, K;Sodium, Na;Zinc, Zn;Copper, Cu;Fluoride, F;Manganese, Mn;Selenium, Se;Vitamin A, IU;Retinol;Vitamin A, RAE;Carotene, beta;Carotene, alpha;Vitamin E (alpha-tocopherol);Vitamin D;Vitamin D2 (ergocalciferol);Vitamin D3 (cholecalciferol);Vitamin D (D2 + D3);Cryptoxanthin, beta;Lycopene;Lutein + zeaxanthin;Tocopherol, beta;Tocopherol, gamma;Tocopherol, delta;Tocotrienol, alpha;Tocotrienol, beta;Tocotrienol, gamma;Tocotrienol, delta;Vitamin C, total ascorbic acid;Thiamin;Riboflavin;Niacin;Pantothenic acid;Vitamin B-6;Folate, total;Vitamin B-12;Choline, total;Menaquinone-4;Dihydrophylloquinone;Vitamin K (phylloquinone);Folic acid;Folate, food;Folate, DFE;Betaine;Tryptophan;Threonine;Isoleucine;Leucine;Lysine;Methionine;Cystine;Phenylalanine;Tyrosine;Valine;Arginine;Histidine;Alanine;Aspartic acid;Glutamic acid;Glycine;Proline;Serine;Hydroxyproline;Vitamin E, added;Vitamin B-12, added;Cholesterol;Fatty acids, total trans;Fatty acids, total saturated;4:0;6:0;8:0;10:0;12:0;14:0;16:0;18:0;20:0;18:1 undifferentiated;18:2 undifferentiated;18:3 undifferentiated;20:4 undifferentiated;22:6 n-3 (DHA);22:0;14:1;16:1 undifferentiated;18:4;20:1;20:5 n-3 (EPA);22:1 undifferentiated;22:5 n-3 (DPA);Phytosterols;Stigmasterol;Campesterol;Beta-sitosterol;Fatty acids, total monounsaturated;Fatty acids, total polyunsaturated;15:0;17:0;24:0;16:1 t;18:1 t;22:1 t;18:2 t not further defined;18:2 i;18:2 t,t;18:2 CLAs;24:1 c;20:2 n-6 c,c;16:1 c;18:1 c;18:2 n-6 c,c;22:1 c;18:3 n-6 c,c,c;17:1;20:3 undifferentiated;Fatty acids, total trans-monoenoic;Fatty acids, total trans-polyenoic;13:0;15:1;18:3 n-3 c,c,c (ALA);20:3 n-3;20:3 n-6;20:4 n-6;18:3i;21:5;22:4;18:1-11 t (18:1t n-7)"
    nutr_list <- unlist(strsplit(nutr_list_str,";"))
    
    df
    df$first_occurrence <- sapply(df$search_for, function(x) str_extract(nutr_list_str, paste0("\\b", x, "\\b")))
    
    df$first_occurrence <- sapply(df$search_for, function(x) nutr_list[which(nutr_list %in% x)[1]])
    
    df$first_occurrence <- sapply(df$search_for, function(x) nutr_list[which(grepl(x, nutr_list))[1]])
    
    
    
  }
}


