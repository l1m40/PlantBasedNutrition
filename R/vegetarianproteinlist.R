# File:   
# Description: 

# https://www.vegetarianproteinlist.com/


# INSTALL AND LOAD PACKAGES ################################
#library(datasets)  # Load base packages manually
# Installs pacman ("package manager") if needed
if (!require("pacman")) install.packages("pacman")
# Use pacman to load add-on packages as desired
# Packages I load every time; uses "pacman"
pacman::p_load(pacman,rio,tidyverse,rvest) 

# CODING ###################################################

a <- "https://www.vegetarianproteinlist.com/" %>% read_html() %>% 
  html_nodes(xpath='//*[@id="vegList"]')
#html_nodes(xpath='//*[@id="wsite-content"]/div/div/div/div/div/div[5]/div')

df <- a %>% html_table() %>% data.frame()
colnames(df)=c("Food","Cup Serving","Protein Grams")

#veganproteinlist_df <- 
df %>% 
  filter(!Food %in% c("Pumpkin SeedProtein Powder","Pinto Beans")) %>% 
  left_join(import("data/vegetarianproteinlist_food_groups.txt"),by="Food") %>% 
  mutate(cup=(ifelse(`Cup Serving`=="1",1,ifelse(`Cup Serving`=="1/2",.5,ifelse(`Cup Serving`=="1/8",.125,NA))))) %>% 
  mutate(cup_vz=(ifelse(`Cup Serving`=="1","OO",ifelse(`Cup Serving`=="1/2","O",ifelse(`Cup Serving`=="1/8","•",NA))))) %>% 
  mutate(mean_protein=mean(`Protein Grams`)) %>%
  mutate(Group=as.factor(Group)) %>% 
  group_by(Group) %>% mutate(mean_protein_by_group=mean(`Protein Grams`)) %>% ungroup() %>% 
  group_by(Food) %>% mutate(mean_protein_by_food=mean(`Protein Grams`)) %>% ungroup() %>% 
  arrange(mean_protein_by_group,mean_protein_by_food) %>% mutate(order=row_number()) %>% #arrange(desc(order))
  #export(paste0(files_path,"vegan_protein.txt"),sep=";")
  #mutate(order=c(desc(mean_protein_by_group),mean_protein_by_food)) %>% 
  #pivot_longer(cols = Group, names_to = 'pivot_name') %>% 
  #pivot_longer(Group) %>% 
  # ggplot(aes(area=`Protein Grams`,fill=Group,subgroup=Group,subgroup2=cup))+ # Tree map
  # geom_treemap(linetype="blank",alpha=.7)+
  # geom_treemap_text(aes(label=cup_vz),size=8,colour="black",alpha=.5,place="top",padding.y=grid::unit(6,"mm"))+
  # geom_treemap_text(aes(label=Food),colour="black",alpha=.5,place="top")
  ggplot(aes(y=reorder(Food,order)))+
  #ggplot(aes(y=reorder(Food,desc(`Protein Grams`))))+
  #geom_point(aes(x=(-1),size=cup,color=cup))+
  #geom_bar(aes(x=-cup*2,fill=cup),stat="identity")+
  # geom_point(size=3,shape=15,aes(x=(-1.0),color=as.factor("1")))+
  # geom_point(size=3,shape=15,aes(x=(-1.5),color=as.factor(ifelse(cup>.125,"1","0"))))+
  # geom_point(size=3,shape=15,aes(x=(-2.0),color=as.factor(ifelse(cup>.250,"1","0"))))+
  # geom_point(size=3,shape=15,aes(x=(-2.5),color=as.factor(ifelse(cup>.375,"1","0"))))+
  # geom_point(size=3,shape=15,aes(x=(-3.0),color=as.factor(ifelse(cup>.500,"1","0"))))+
  # geom_point(size=3,shape=15,aes(x=(-3.5),color=as.factor(ifelse(cup>.625,"1","0"))))+
  # geom_point(size=3,shape=15,aes(x=(-4.0),color=as.factor(ifelse(cup>.750,"1","0"))))+
  # geom_point(size=3,shape=15,aes(x=(-4.5),color=as.factor(ifelse(cup>.875,"1","0"))))+
  geom_point(size=5,shape=15,aes(x=(-1.0),color=as.factor(cup)))+
  geom_point(size=5,shape=15,aes(x=(-1.5),color=as.factor(ifelse(cup>.125,cup,0))))+
  geom_point(size=5,shape=15,aes(x=(-2.0),color=as.factor(ifelse(cup>.250,cup,0))))+
  geom_point(size=5,shape=15,aes(x=(-2.5),color=as.factor(ifelse(cup>.375,cup,0))))+
  geom_point(size=5,shape=15,aes(x=(-3.0),color=as.factor(ifelse(cup>.500,cup,0))))+
  geom_point(size=5,shape=15,aes(x=(-3.5),color=as.factor(ifelse(cup>.625,cup,0))))+
  geom_point(size=5,shape=15,aes(x=(-4.0),color=as.factor(ifelse(cup>.750,cup,0))))+
  geom_point(size=5,shape=15,aes(x=(-4.5),color=as.factor(ifelse(cup>.875,cup,0))))+
  scale_color_brewer(palette="Greys")+
  #scale_color_manual("cup",values=c(".125"="yellow",".5"="blue","1"="black","0"="white"))+
  #geom_vline(aes(xintercept=mean_protein))+
  geom_bar(aes(x=`Protein Grams`,fill=Group),stat="identity",alpha=.3)+xlab("Protein grams")+
  geom_text(aes(x=0,label=Food),hjust=0)+scale_x_continuous(breaks=seq(0,20,by=5))+
  #coord_polar(theta="y")+scale_x_continuous(breaks=NULL)+
  #facet_grid(rows=vars(Group))+
  theme_minimal()+theme(axis.title.y=element_blank(),axis.text.y=element_blank(),
                        panel.grid.minor=element_blank(),
                        panel.grid.major.y=element_blank(),
                        #panel.grid.major.x=element_line(color="gray8",linetype=2),
                        legend.position="none")

df %>% summary



