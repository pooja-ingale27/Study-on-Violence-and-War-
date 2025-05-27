library(tidyverse)
library(ggplot2)
library(ggstream)
library(ggsankey)
library(viridis)

########### Chart 1. Deaths in Conflicts, by world region ########################################
battle<-read.csv(file = "war_peace_battle.csv")
View(battle)

battle<-battle %>% 
  mutate(decade = floor(Year/10)*10) %>% 
  group_by(decade)


ggplot(battle, aes(x = Year, y = deaths, fill = Entity)) +
  geom_stream( color = 1, linewidth = 0.25, extra_span = 0.3, true_range = "none",
               alpha = 0.3) +
  guides(fill = guide_legend(title = "Regions"))+
  labs(x = "Year", y = "Number of Deaths Due to War",title = "Deaths in Conflicts, by world region")+
  scale_fill_manual(values = c("#FFB400", "#5F5FF9", "#C20008", "#76F6CF", "#13AFEF", "#F2AFED"))+
  theme_bw()

###########################################################################################s
##################### Chart 3. Country Position on nuclear weapons #########################
weapons_data<-read.csv(file="war_peace_nuclear_weapons.csv")

weapons1<-weapons_data[(weapons_data$Weapons != 0) & 
                         (weapons_data$Country %in% c("United States",
                                                      "India",
                                                      "United Kingdom",
                                                      "Russia",
                                                      "China",
                                                      "Australia",
                                                      "Brazil")) &
                         (weapons_data$Year %in% c("1950", "1960","1970", "1980", "1990","2000", "2010", "2020")),]

weapons_long <- weapons1 %>%
  make_long(Country, Year, Weapons)

ggplot(weapons_long, aes(x = x
                         , next_x = next_x
                         , node = node
                         , next_node = next_node
                         , fill = factor(node)
                         , label = node))+
  geom_sankey(flow.alpha = 0.3
              , node.color = "white"
                ,show.legend = FALSE)+
  geom_sankey_label(size = 3, color = "black", fill= "white",, hjust = 1) +
  theme_bw()+
  theme(legend.position = "none") +
  theme(axis.title = element_blank()
        , axis.text.y = element_blank()
        , axis.ticks = element_blank()
        , panel.grid = element_blank())+
  scale_colour_viridis_d(alpha = 0.1) +
  labs(title = "Country position on nuclear weapons") +
  theme_sankey(base_size = 12)

###############################################################################################

############################# Chart 2. Military Expenditure as a share of GDP ################

mil_exp <-read.csv(file ="war_peace_military_expenditure.csv")

mil_exp1<-mil_exp[(mil_exp$exp != 0) & 
                    (mil_exp$Entity %in% c("United States",
                                           "India",
                                           "United Kingdom",
                                           "Russia",
                                           "China",
                                           "Australia",
                                           "Brazil")) &
                    (mil_exp$Year %in% c("1950", "1960","1970", "1980", "1990","2000", "2010", "2020")),]

View(mil_exp1)
ggplot(mil_exp1, aes(mil_exp1$Entity, mil_exp1$Year, fill= mil_exp1$exp)) + 
  geom_tile()+
  scale_fill_viridis(trans = 'reverse', option="plasma")+
  scale_fill_distiller(palette = "Blues", direction = 1) +
  theme(axis.text.x = element_text(angle = 60, hjust = 1),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank())+
  labs(x= "Country" , y =" Year", title = "Military expenditure as a share of GDP")+
  guides(fill = guide_legend(title = "Military Expenditure"))+ 
  theme(legend.key = element_rect(fill = "white", colour = "black"),
        legend.box.background = element_rect(color="black", size=1),
        legend.box.margin = margin(10, 1, 1, 1))



#################################################################################################
################################ Chart 4. Number of United Nations peacekeeping missions and active forces. ######################################
peace_keep<-read.csv(file = "war_peace_keeping.csv")
View(peace_keep)
peace_keep_long <- pivot_longer(peace_keep, cols=4, names_to = "Entity_name", values_to = "Number_Values")
View(peace_keep_long)

UN_forces<-read.csv(file = "war_peace_forces.csv")
View(UN_forces)
UN_forces_long <- pivot_longer(UN_forces, cols=4, names_to = "Entity_name", values_to = "Number_Values")
View(UN_forces_long)
peace_force<-bind_rows(peace_keep_long,UN_forces_long)
View(peace_force)

peace_force<-peace_force[peace_force$Year %in% c("1950", "1960","1970", "1980", "1990","2000", "2010", "2020"),] 
View(peace_force) 

Entity_name.labs <- c("Active Peace Missions in UN", "Active Forces in UN")
names(Entity_name.labs) <- c("peace", "forces")

ggplot(peace_force, aes(x = Year, y = Number_Values , fill = Entity_name)) + 
  geom_bar(color = "white", stat = "identity") +
  facet_grid(~Entity_name, scales = "free", labeller = labeller(Entity_name = Entity_name.labs)) + 
  coord_flip()+
  theme_bw()+
  labs(x = "Year" , y= "Count" , title = "Number of United Nations peacekeeping missions and Total size of forces active")+
  theme(strip.text.x = element_text(size = 10, color = "blue", face = "italic"),
        strip.background = element_rect(color="black", fill="white", size=1, linetype="solid"), legend.position = "none")+
  scale_fill_manual(values=c("#5F5FF9",
                                      "#F2AFED"))
###################################################################################################