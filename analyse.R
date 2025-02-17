# sets the working directory to the folder containing this script
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# libraries 
library(terra)
library(tidyterra)
library(geodata)
library(ggplot2)
library(readxl)
library(ggpubr)
library(dplyr)
library(ggtext)
library(stringr)

##########
# preparation of the shapefiles for the European NUTS-regions
##########
# read the shapefile of the European NUTS-regions https://ec.europa.eu/eurostat/de/web/gisco/geodata/statistical-units/territorial-units-statistics
nuts <- vect("data/NUTS_RG_20M_2024_4326.shp")

# subset different nuts levels to be able to merge them with the shapes of the countries not included in the European NUTS-shapefile
nuts0<-subset(nuts, nuts$LEVL_CODE == 0)
nuts1<-subset(nuts, nuts$LEVL_CODE == 1)
nuts2<-subset(nuts, nuts$LEVL_CODE == 2)
nuts3<-subset(nuts, nuts$LEVL_CODE == 3)

# read shapes of countries not included in the European NUTS-shapefile
UKR_lvl0 <- gadm('GADM', country = 'UKR', level = 0)
MDA_lvl0 <- gadm('GADM', country = 'MDA', level = 0)
BLR_lvl0 <- gadm('GADM', country = 'BLR', level = 0)
XKO_lvl0 <- gadm('GADM', country = 'XKO', level = 0)
BIH_lvl0 <- gadm('GADM', country = 'BIH', level = 0)
lvl0 <- rbind(UKR_lvl0, MDA_lvl0, BLR_lvl0, XKO_lvl0, BIH_lvl0)
lvl0$LEVL_CODE <- 0
lvl0$FID <- paste("FID1", 1:nrow(lvl0), sep = "_")

UKR_lvl1 <- gadm('GADM', country = 'UKR', level = 1)
MDA_lvl1 <- gadm('GADM', country = 'MDA', level = 0)
BLR_lvl1 <- gadm('GADM', country = 'BLR', level = 1)
XKO_lvl1 <- gadm('GADM', country = 'XKO', level = 0)
BIH_lvl1 <- gadm('GADM', country = 'BIH', level = 0)
lvl1 <- rbind(UKR_lvl1, MDA_lvl1, BLR_lvl1, XKO_lvl1, BIH_lvl1)
lvl1$LEVL_CODE <- 1
lvl1$FID <- paste("FID1", 1:nrow(lvl1), sep = "_")

UKR_lvl2 <- gadm('GADM', country = 'UKR', level = 1)
MDA_lvl2 <- gadm('GADM', country = 'MDA', level = 0)
BLR_lvl2 <- gadm('GADM', country = 'BLR', level = 1)
XKO_lvl2 <- gadm('GADM', country = 'XKO', level = 1)
BIH_lvl2 <- gadm('GADM', country = 'BIH', level = 1)
lvl2 <- rbind(UKR_lvl2, MDA_lvl2, BLR_lvl2, XKO_lvl2, BIH_lvl2)
lvl2$LEVL_CODE <- 2
lvl2$FID <- paste("FID2", 1:nrow(lvl2), sep = "_")

UKR_lvl3 <- gadm('GADM', country = 'UKR', level = 2)
MDA_lvl3 <- gadm('GADM', country = 'MDA', level = 1)
BLR_lvl3 <- gadm('GADM', country = 'BLR', level = 2)
XKO_lvl3 <- gadm('GADM', country = 'XKO', level = 2)
BIH_lvl3 <- gadm('GADM', country = 'BIH', level = 3)
lvl3 <- rbind(UKR_lvl3, MDA_lvl3, BLR_lvl3, XKO_lvl3, BIH_lvl3)
lvl3$LEVL_CODE <- 3
lvl3$FID <- paste("FID3", 1:nrow(lvl3), sep = "_")

# merge the NUTS-shapes with the shapes of the countries included and not included in the European NUTS-shapefile
nuts_list <- list()

nuts_list[[1]] <- rbind(nuts0, lvl0)
nuts_list[[2]] <- rbind(nuts1, lvl1)
nuts_list[[3]] <- rbind(nuts2, lvl2)
nuts_list[[4]] <- rbind(nuts3, lvl3)

# name the list elements per NUTS-level
names(nuts_list) <- c("low",   "medium",  "high", "very high")

# read the excel file on Dirofilara in vertebrates
file <- read_excel("data/Dirofilaria_surv_euro_ch.xlsx", sheet = 1) # read excel file

es <- unique(file[c("PMID", "Years")])

ogo <- strsplit(es$Years, ",")
as <- table(as.numeric(unlist(ogo)))

zwei <- data.frame(year = as.numeric(names(as)),
                   n = as.numeric(as))

jpeg(file = "figs/studies.jpeg",width = 6, height= 4,
     units = 'in', res =1000)
ggplot(zwei, aes(x = year, y = n)) +
  geom_point()+
  geom_line() +
  xlab("Year") +
  ylab("Number of studies covering the year") +
  xlim(c(1960, 2021)) +
  theme_bw()
dev.off() 

# revalue for maps
library(plyr)
file$Host_species2 <- revalue(file$Host_species, c("human female" = "human",
                                                   "human male" = "human",
                                                   "humane male" = "human",
                                                   "cat"= "other",                   
                                                   "red fox" = "other",                   
                                                   "Eurasian otter" = "other",               
                                                   "racoon dog" = "other",            
                                                   "ferret" = "other",                  
                                                   "golden jackal"  = "other",        
                                                   "African leopard" = "other",  
                                                   "wolf" = "other",   
                                                   "California sea lion" = "other",  
                                                   "common seal" = "other",            
                                                   "grey seal" = "other",              
                                                   "South African fur seal"= "other",  
                                                   "beech marten"  = "other",           
                                                   "brown bear" = "other",              
                                                   "Eurasian lynx"   = "other",        
                                                   "European badger"   = "other",       
                                                   "European mink"     = "other",       
                                                   "European pine marten" = "other",  
                                                   "European polecat"   = "other",     
                                                   "least weasel"   = "other",          
                                                   "stoat"       = "other",             
                                                   "wildcat"      = "other",            
                                                   "badger"      = "other",            
                                                   "otter"      = "other",              
                                                   "stone marten"       = "other",      
                                                   "European otter"     = "other",      
                                                   "pine marten"    = "other",         
                                                   "African lion"= "other"))

# revalue for analysis of vertebrate hosts
file$Host_species3 <- revalue(file$Host_species, c("human female" = NA,
                                                   "human male" = NA,
                                                   "humane male" = NA,
                                                   "African leopard" = "Panthera pardus pardus",
                                                   "African lion"= "Panthera leo",
                                                   "badger"      = "Meles meles", 
                                                   "beech marten"  = "Martes foina",           
                                                   "brown bear" = "Urs arctos",
                                                   "cat"= "Felis catus",                   
                                                   "red fox" = "Vulpes vulpes",                   
                                                   "Eurasian otter" = "Lutra lutra",               
                                                   "racoon dog" = "Nyctereutes procyonoides",            
                                                   "ferret" = "Mustela furo",                  
                                                   "golden jackal"  = "Canis aureus",        
                                                   "wolf" = "Canis lupus",   
                                                   "California sea lion" = "Zalophus californianus",  
                                                   "common seal" = "Phoca vitulina",            
                                                   "grey seal" = "Halichoerus grypus",              
                                                   "South African fur seal"= "Arctocephalus pusillus",  
              
                                                   "Eurasian lynx"   = "Lynx lynx",        
                                                   "European badger"  = "Meles meles",       
                                                   "European mink"     = "Mustela lutreola",       
                                                   "European pine marten" = "Martes martes",  
                                                   "European polecat"   = "Mustela putorius",     
                                                   "least weasel"   = "Mustela nivalis",          
                                                   "stoat"       = "Mustela erminea",             
                                                   "wildcat"      = "Felis silvestris",            
                                                     
                                                   "otter"      = "Lutra lutra",              
                                                   "stone marten"  =     "Martes foina",      
                                                   "European otter" =   "Lutra lutra",      
                                                   "pine marten"    = "Martes martes"))
detach("package:plyr", unload = TRUE)

file %>%
  filter(Host_species3 == "Felis catus") %>%
  summarise(u = length(unique(PMID)))

ogo <- strsplit(file$Years, ",")
maximum_val <- as.numeric(unlist(lapply(ogo, function(x) max(x))))
minimum_val <- as.numeric(unlist(lapply(ogo, function(x) min(x))))
file$time = ifelse(maximum_val < 2001, "before",
       ifelse(maximum_val > 2000 & minimum_val > 2000, "after", "both"))

file2 <- subset(file, file$Travel_history %in% c("unremarkable") & 
                  file$Infection_status %in% "infected")
#file2 <- subset(file, file$Travel_history %in% c("unremarkable", "unknown", "unkown") & 
#                  file$Infection_status %in% "infected")

full <- file2[,c("Dirofilaria_species", "Host_species2", "accuracy","longitude","latitude","time")]

time_list <- list()
time_list[[1]] <- c("before", "both")
time_list[[2]] <- c("after", "both")
names(time_list) <- c("before", "after")

# produce SpatVector of sampling sites
vetori <- vect(full, geom=c("longitude", "latitude"),  crs="epsg:4326")

# create a list for storage in the following loop
f <- list()

# loop over the different species, time periods, host species and accuracy levels
for(i in c("repens", "immitis")){
  sub_species <- vetori[vetori$Dirofilaria_species == i, ]
  
  for(h in c("before", "after")){
    sub_species2 <- sub_species[sub_species$time %in% time_list[[h]], ]
    
    for(j in c("dog", "human", "other")){
      sub_species3 <- sub_species2[sub_species2$Host_species2 == j, ]
      
      for(k in c( "low",   "medium",  "high", "very high")){
        sibi <- (sub_species3[sub_species3$accuracy == k,])
        if(length(sibi) == 0){
          break
        }
        shapes_with_point <-  nuts_list[[k]][is.related(nuts_list[[k]], sibi, "intersects") ]
        print("i")
        
        f[[i]][[h]][[j]][[k]] <- shapes_with_point
      }
    }
  }
}

# list for storing the figures
fig_list <- list()

# loop over the different species, time periods and host species to produce the figures
for(i in c("repens", "immitis")){
  for(h in c("before","after")){
    for(j in c("dog", "human", "other")){
      
      fig_list[[i]][[h]][[j]] <- ggplot() +
        geom_spatvector(data = nuts_list[[1]], fill = NA, color = "black", size = 1.5) +
        geom_spatvector(data = f[[i]][[h]][[j]][["low"]], fill = "#56B4E9", color = "black", size = 1.5) +
        geom_spatvector(data = f[[i]][[h]][[j]][["medium"]], fill = "#009E73", color = "black", size = 1.5) +
        geom_spatvector(data = f[[i]][[h]][[j]][["high"]], fill = "#F0E442", color = "black", size = 1.5) +
        geom_spatvector(data = f[[i]][[h]][[j]][["very high"]], fill = "#E69F00", color = "black", size = 1.5) +
        ggtitle(paste(ifelse(h == "before", "Until 2001", "Since 2001"),
                      ifelse(j == "human", "Human", ifelse(j == "dog", "Dog", "Other")), 
                      sep = ","),
                subtitle = "NUTS-<span style='color:#56B4E9;'>0</span>,
                <span style='color:#009E73;'>1</span>,
                <span style='color:#F0E442;'>2</span>,
                <span style='color:#E69F00;'>3</span>") +
        
        coord_sf(xlim = c(-10, 35), ylim = c(35, 65)) +
        theme_bw()+
        theme(plot.subtitle = element_markdown())
    }
  }
}


jpeg(file = "figs/Figurer5_repens.jpg",width = 6, height= 7,
     units = 'in', res =1000)
ggarrange(
  fig_list[[1]][[1]][[2]],
  fig_list[[1]][[2]][[2]],
  fig_list[[1]][[1]][[1]],
  fig_list[[1]][[2]][[1]],
  fig_list[[1]][[1]][[3]],
  fig_list[[1]][[2]][[3]],
  ncol = 2,nrow =3,
  legend = "bottom",
  common.legend = TRUE)
dev.off()

jpeg(file = "figs/Figurer4_immitis.jpg",width = 6, height= 7,
     units = 'in', res =1000)
ggarrange(
  fig_list[[2]][[1]][[2]],
  fig_list[[2]][[2]][[2]],
  fig_list[[2]][[1]][[1]],
  fig_list[[2]][[2]][[1]],
  fig_list[[2]][[1]][[3]],
  fig_list[[2]][[2]][[3]],
  ncol = 2,nrow =3)
dev.off()

##########
# Figure2_mosquitoes
##########

# read the excel file on Dirofilara in mosquitoes
vect <- read_excel("data/Vector_surv_dirofilaria_ch.xlsx", sheet = 1) # read excel file

unique(vect$Mosquito_species)
# revalue mosquito names
library(plyr)
vect$Mosquito_species <- revalue(vect$Mosquito_species, 
                                 c("Culseta annulata" = "Culiseta annulata",
                                   "Coquilletnsdia richardii" = "Coquillettidia richardii",
                                   "Coquillettidia richiardii" = "Coquillettidia richardii",
                                   "Aedes annulipes" = "Aedes annulipes s.l.",
                                   "Aedes cinereus" = "Aedes cinereus/geminus",
                                   "Aedes cinereus s.l." = "Aedes cinereus/geminus",
                                   "Aedes detritus s.l." = "Aedes detritus",
                                   "Aedes detrius"= "Aedes detritus",
                                   "Culex theleri" = "Culex theileri",
                                   "Anohpeles hycranus" = "Anopheles hyrcanus",
                                   "Anopheles hycranus" = "Anopheles hyrcanus",
                                   "Culex pipiens s.l." = "Culex pipiens s.l., Culex torrentium",
                                   "Culex pipiens" = "Culex pipiens s.l., Culex torrentium",
                                   "Anohpeles maculipennis s.l." = "Anopheles maculipennis s.l."))
detach("package:plyr", unload = TRUE)

sdf1 <- vect %>% 
  dplyr::group_by(Mosquito_species, Country, Dirofilaria_species, Infection_status)  %>%
  summarize(sum1 = sum(sum, na.rm = T))  %>%
  dplyr::group_by(Mosquito_species, Country, Dirofilaria_species)  %>%
  summarize(sum = sum(sum1, na.rm = T),
            inf = ifelse(sum(str_detect(Infection_status, "(?<![:alpha:])infected")) > 0,
                         "infected", "uninfected")) %>%
  dplyr::group_by(Mosquito_species, Country)   %>%
  mutate(sum_all = sum(sum, na.rm = T))  %>%
  filter(inf %in% "infected") %>%
  dplyr::group_by(Mosquito_species, Country, sum_all)  %>%
  summarize(es = ifelse(sum(str_detect(Dirofilaria_species, c("immitis", "repens"))) > 1, 
                        "both", Dirofilaria_species))
data.frame(sdf1)

sdf2 <- vect %>% 
  dplyr::group_by(Mosquito_species, Country, Dirofilaria_species, Infection_status)  %>%
  summarize(sum1 = sum(sum, na.rm = T))  %>%
  dplyr::group_by(Mosquito_species, Country, Dirofilaria_species)  %>%
  summarize(sum = sum(sum1, na.rm = T),
            inf = ifelse(sum(str_detect(Infection_status, "uninfected")) > 0, "uninfected", "infected")) %>%
  dplyr::group_by(Mosquito_species, Country)   %>%
  mutate(sum_all = sum(sum, na.rm = T)) %>%
  filter(inf %in% "uninfected")

sdf2$es <- "none"
sdf1$combi <- paste0(sdf1$Mosquito_species, sdf1$Country)
sdf2$combi <- paste0(sdf2$Mosquito_species, sdf2$Country)

sdf2_2 <- sdf2[!(sdf2$combi %in% sdf1$combi),]

fulli <- rbind(sdf1[,c(1:4)], sdf2_2[,c(1,2,6,7)])

unique(fulli$Mosquito_species)
unique(fulli$Country)
unique(subset(fulli, fulli$es %in% c("immitis", "both"))$Mosquito_species)
unique(subset(fulli, fulli$es %in% c("immitis", "both"))$Country)
(table(subset(fulli, fulli$es %in% c("immitis", "both"))$Mosquito_species))

unique(subset(fulli, fulli$es %in% c("repens", "both"))$Mosquito_species)
unique(subset(fulli, fulli$es %in% c("repens", "both"))$Country)
(table(subset(fulli, fulli$es %in% c("repens", "both"))$Mosquito_species))

# in both
unique(subset(fulli, fulli$es %in% c("immitis", "both"))$Mosquito_species)[unique(subset(fulli, fulli$es %in% c("immitis", "both"))$Mosquito_species) %in% 
  unique(subset(fulli, fulli$es %in% c("repens", "both"))$Mosquito_species)]

unique(subset(fulli, fulli$es %in% c("immitis"))$Mosquito_species)[!unique(subset(fulli, fulli$es %in% c("immitis"))$Mosquito_species) %in% 
                                                                             unique(subset(fulli, fulli$es %in% c("repens", "both"))$Mosquito_species)]

unique(subset(fulli, fulli$es %in% c("repens"))$Mosquito_species)[!unique(subset(fulli, fulli$es %in% c("repens"))$Mosquito_species) %in% 
                                                                             unique(subset(fulli, fulli$es %in% c("immitis", "both"))$Mosquito_species)]


# plot the figure
jpeg(file = "figs/Figure2_mosquitoes.jpeg",width = 8, height= 8,
     units = 'in', res =1000)
ggplot(na.omit(fulli)) +
  geom_point(aes(x = Country , y =  Mosquito_species,
                 fill  = es,size = ifelse(sum_all ==0, NA, sum_all)), shape = 22) +
  scale_fill_manual(breaks = c("both", "immitis", "repens", "none"),
                    values=(c("black","#56B4E9","#E69F00", "white"))) +
    theme_bw() +
  ylab("Mosquito species") +
  guides(size = guide_legend(title = "Specimens", order = 1),
         fill = guide_legend(title = "Dirofilaria detected", order = 2)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  theme(axis.text.y = element_text(face ="italic"))
dev.off()

##########
# Figure3_other_hosts
##########

# 
asl <- file %>% 
  filter(!(Host_species3 %in% c("dog", "human", NA)))  %>% 
  filter(!(Dirofilaria_species %in% c(NA)))  %>% 
  dplyr::group_by(Host_species3, Country, Dirofilaria_species)  %>%
  summarize(tested = sum(No_tested, na.rm = T),
            positive = sum(No_positive, na.rm = T))

# 
asl2 <- pivot_longer(asl, cols= c("tested", "positive"), 
                     names_to= c("l"))

# revalue the species names
library(plyr)
asl2$Dirofilaria_species <- revalue(asl2$Dirofilaria_species, 
        c("immitis" = "Dirofilaria immitis",
          "repens" = "Dirofilaria repens"))
detach("package:plyr", unload = TRUE)

# plot the figure
jpeg(file = "figs/Figure3_other_hosts.jpeg",width = 6, height= 6,
     units = 'in', res =1000)
ggplot(asl2) +
  geom_point(aes(x =  Country, y =  Host_species3, size = ifelse(value ==0, NA, value),
                 fill = l), shape=22) +
  facet_wrap(~Dirofilaria_species, nrow = 2) +
  theme_bw() +
  scale_size_continuous(breaks = c(1, 10, 100, 4000)) +
  xlab("Host species") +
  guides(size = guide_legend(title = "Specimens", order = 1), 
         fill = guide_legend(title = " ", order = 2)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, face ="italic"),
        strip.text = element_text(face = "italic"))
dev.off()