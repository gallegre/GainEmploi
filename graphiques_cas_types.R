library(haven)
library(glue)
library(tidyverse)
library(readr)
library(dplyr)
library(reshape2)
library(MetricsWeighted)
library(questionr)
library(xlsx)
library(patchwork)
library(ggplot2)
library(ggrepel)
library(blorr)


castyp<-read.xlsx(file="cas types smic jeune enfant.xlsx",sheetName = "synthese") 

#  Isolé 1 enfant
smic=1398.7
nbuc=1.3
seuilpm=1309*nbuc

base<-castyp %>% 
  filter(isocou=="isolé" & nbenf==1) %>% 
  mutate(typrev=case_when(
    activite==0 ~ "Sans revenu",
    activite==0.5 ~ "Avec un mi-temps \n au Smic",
    activite==1 & Salaire==smic ~ "Avec un temps plein\n au Smic",
    TRUE ~ "Avec un temps plein \n à 1,5 Smic")
  ) %>% 
  select(typrev,CGNET,Salaire,PF,PL,RSA,PA, RDISP_NET_CG)	

basegr<-base %>% 
  pivot_longer(cols=c(CGNET,Salaire,PF,PL,RSA,PA, RDISP_NET_CG)) 



basegr$typrev<-factor(basegr$typrev,levels=c("Sans revenu","Avec un mi-temps \n au Smic","Avec un temps plein\n au Smic","Avec un temps plein \n à 1,5 Smic"))

basegr$name<-factor(basegr$name, levels=rev(c("CGNET","Salaire", "PF", "PL", "RSA", "PA", "RDISP_NET_CG")))

graphI1 <- ggplot(basegr %>% filter(name !='RDISP_NET_CG'), aes(fill = name,
                                                       y = value, x = typrev)) + 
  scale_fill_manual(
    values = c("CGNET"="darkred","Salaire"= "#482677FF", "PF" = "#39568CFF", "PL" = "#238A8DFF", 
               "RSA" = "#55C667FF", "PA" = "#95D840FF"),
    labels = c("CGNET"="Coût net de la garde",
               "PA" = "Prime d'activité",
               "RSA" = "Revenu de solidarité active",
               "PF" = "Prestations familiales",
               "PL" = "Prestations logement",
               "Salaire" = "Salaire" 
    ),
    # guide = guide_legend(reverse = TRUE)  
  ) +
  geom_bar(
    position = "stack", 
    stat = "identity", 
    aes()
  ) +
  geom_hline(
    yintercept = seuilpm, 
    linetype = "dashed", 
    color = "lightgrey", 
    linewidth = 1
  ) +
  geom_text(
    aes(x = 0.5, y = seuilpm + 100, label = "Seuil de pauvreté (à 60%)"),
    size = 3, 
    hjust = 0, # Left-justified
    colour = "lightgrey"
  ) +
  geom_hline(
    yintercept = (4/6)*seuilpm, 
    linetype = "dashed", 
    color = "lightgrey", 
    linewidth = 1
  ) +
  geom_text(
    aes(x = 0.5, y = (4/6)*seuilpm + 100, label = "Seuil de grande \npauvreté (40%)"),
    size = 3, 
    hjust = 0, # Left-justified
    colour = "lightgrey"
  ) +
  geom_point(
    data = basegr %>% filter(name == 'RDISP_NET_CG'), 
    aes(
      x = typrev, y = round(value, 2), 
      colour = "Revenu disponible net du coût de la garde", 
    ), 
    size = 3
  ) +
  theme(
    panel.background = element_rect(fill = "white"), 
    plot.background = element_rect(fill = "white"),
    legend.position =  "right",
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    legend.text = element_text(size = rel(0.7)),
    legend.title = element_blank(),
    plot.title = element_text(size = rel(0.8), hjust = 0.7),
    plot.caption = element_text(size = rel(0.5)),
    plot.margin =  margin(t = 20, r = 10, b = 20, l = 10),
    axis.ticks.y = element_blank(),
    axis.line = element_line(color = "black", linewidth = 0.2, linetype = "solid"),
    axis.text.x = element_text(size = rel(0.9), angle = 40, hjust = 1,vjust = 1)
  ) +
  labs(title="Isolé 1 enfant",size=5)+
  # scale_y_continuous(expand = c(0, 0), limits = c(0, 3000), breaks = seq(0, 3000, by = 500)) + 
  theme(panel.border = element_blank())

graphI1

#  Isolé 2 enfants
smic=1398.7
nbuc=1.6
seuilpm=1309*nbuc

base<-castyp %>% 
  filter(isocou=="isolé" & nbenf==2) %>% 
  mutate(typrev=case_when(
    activite==0 ~ "Sans revenu",
    activite==0.5 ~ "Avec un mi-temps \n au Smic",
    activite==1 & Salaire==smic ~ "Avec un temps plein\n au Smic",
    TRUE ~ "Avec un temps plein \n à 1,5 Smic")
  ) %>% 
  select(typrev,CGNET,Salaire,PF,PL,RSA,PA, RDISP_NET_CG)	

basegr<-base %>% 
  pivot_longer(cols=c(CGNET,Salaire,PF,PL,RSA,PA, RDISP_NET_CG)) 



basegr$typrev<-factor(basegr$typrev,levels=c("Sans revenu","Avec un mi-temps \n au Smic","Avec un temps plein\n au Smic","Avec un temps plein \n à 1,5 Smic"))

basegr$name<-factor(basegr$name, levels=rev(c("CGNET","Salaire", "PF", "PL", "RSA", "PA", "RDISP_NET_CG")))

graphI2 <- ggplot(basegr %>% filter(name !='RDISP_NET_CG'), aes(fill = name,
                                                                y = value, x = typrev)) + 
  scale_fill_manual(
    values = c("CGNET"="darkred","Salaire"= "#482677FF", "PF" = "#39568CFF", "PL" = "#238A8DFF", 
               "RSA" = "#55C667FF", "PA" = "#95D840FF"),
    labels = c("CGNET"="Coût net de la garde",
               "PA" = "Prime d'activité",
               "RSA" = "Revenu de solidarité active",
               "PF" = "Prestations familiales",
               "PL" = "Prestations logement",
               "Salaire" = "Salaire" 
    ),
    # guide = guide_legend(reverse = TRUE)  
  ) +
  geom_bar(
    position = "stack", 
    stat = "identity", 
    aes()
  ) +
  geom_hline(
    yintercept = seuilpm, 
    linetype = "dashed", 
    color = "lightgrey", 
    linewidth = 1
  ) +
  geom_text(
    aes(x = 0.5, y = seuilpm + 100, label = "Seuil de pauvreté (à 60%)"),
    size = 3, 
    hjust = 0, # Left-justified
    colour = "lightgrey"
  ) +
  geom_hline(
    yintercept = (4/6)*seuilpm, 
    linetype = "dashed", 
    color = "lightgrey", 
    linewidth = 1
  ) +
  geom_text(
    aes(x = 0.5, y = (4/6)*seuilpm + 100, label = "Seuil de grande \npauvreté (40%)"),
    size = 3, 
    hjust = 0, # Left-justified
    colour = "lightgrey"
  ) +
  geom_point(
    data = basegr %>% filter(name == 'RDISP_NET_CG'), 
    aes(
      x = typrev, y = round(value, 2), 
      colour = "Revenu disponible net du coût de la garde", 
    ), 
    size = 3
  ) +
  theme(
    panel.background = element_rect(fill = "white"), 
    plot.background = element_rect(fill = "white"),
    legend.position =  "right",
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    legend.text = element_text(size = rel(0.7)),
    legend.title = element_blank(),
    plot.title = element_text(size = rel(0.8), hjust = 0.7),
    plot.caption = element_text(size = rel(0.5)),
    plot.margin =  margin(t = 20, r = 10, b = 20, l = 10),
    axis.ticks.y = element_blank(),
    axis.line = element_line(color = "black", linewidth = 0.2, linetype = "solid"),
    axis.text.x = element_text(size = rel(0.9), angle = 40, hjust = 1,vjust = 1)
  ) +
  labs(title="Isolé 2 enfants",size=5)+
  # scale_y_continuous(expand = c(0, 0), limits = c(0, 3000), breaks = seq(0, 3000, by = 500)) + 
  theme(panel.border = element_blank())

graphI2



#  Couple biactif - conjoint au smic - 1 enfant
smic=1398.7
nbuc=1.8
seuilpm=1309*nbuc

base<-castyp %>% 
  filter(isocou=="couple" & actconj=="1 smic" & nbenf==1) %>% 
  mutate(typrev=case_when(
    activite==0 ~ "Sans revenu",
    activite==0.5 ~ "Avec un mi-temps \n au Smic",
    activite==1 & Salaire==smic ~ "Avec un temps plein\n au Smic",
    TRUE ~ "Avec un temps plein \n à 1,5 Smic")
  ) %>% 
  select(typrev,CGNET,Ract_conj,Salaire,PF,PL,RSA,PA, RDISP_NET_CG)	

basegr<-base %>% 
  pivot_longer(cols=c(CGNET,Ract_conj,Salaire,PF,PL,RSA,PA, RDISP_NET_CG)) 


basegr$typrev<-factor(basegr$typrev,levels=c("Sans revenu","Avec un mi-temps \n au Smic","Avec un temps plein\n au Smic","Avec un temps plein \n à 1,5 Smic"))

basegr$name<-factor(basegr$name, levels=rev(c("CGNET","Ract_conj","Salaire", "PF", "PL", "RSA", "PA", "RDISP_NET_CG")))

graphC1_S <- ggplot(basegr %>% filter(name !='RDISP_NET_CG'), aes(fill = name,
                                                                y = value, x = typrev)) + 
  scale_fill_manual(
    values = c("CGNET"="darkred",'Ract_conj' = '#440154FF',"Salaire"= "#482677FF", "PF" = "#39568CFF", "PL" = "#238A8DFF", 
               "RSA" = "#55C667FF", "PA" = "#95D840FF"),
    labels = c("CGNET"="Coût net de la garde",
               "PA" = "Prime d'activité",
               "RSA" = "Revenu de solidarité active",
               "PF" = "Prestations familiales",
               "PL" = "Prestations logement",
               "Salaire" = "Salaire" ,
               "Ract_conj"="Salaire conjoint"
    ),
    # guide = guide_legend(reverse = TRUE)  
  ) +
  geom_bar(
    position = "stack", 
    stat = "identity", 
    aes()
  ) +
  geom_hline(
    yintercept = seuilpm, 
    linetype = "dashed", 
    color = "lightgrey", 
    linewidth = 1
  ) +
  geom_text(
    aes(x = 0.5, y = seuilpm + 100, label = "Seuil de pauvreté (à 60%)"),
    size = 3, 
    hjust = 0, # Left-justified
    colour = "lightgrey"
  ) +
  geom_hline(
    yintercept = (4/6)*seuilpm, 
    linetype = "dashed", 
    color = "lightgrey", 
    linewidth = 1
  ) +
  geom_text(
    aes(x = 0.5, y = (4/6)*seuilpm + 100, label = "Seuil de grande \npauvreté (40%)"),
    size = 3, 
    hjust = 0, # Left-justified
    colour = "lightgrey"
  ) +
  geom_point(
    data = basegr %>% filter(name == 'RDISP_NET_CG'), 
    aes(
      x = typrev, y = round(value, 2), 
      colour = "Revenu disponible net du coût de la garde", 
    ), 
    size = 3
  ) +
  theme(
    panel.background = element_rect(fill = "white"), 
    plot.background = element_rect(fill = "white"),
    legend.position =  "right",
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    legend.text = element_text(size = rel(0.7)),
    legend.title = element_blank(),
    plot.title = element_text(size = rel(0.8), hjust = 0.7),
    plot.caption = element_text(size = rel(0.5)),
    plot.margin =  margin(t = 20, r = 10, b = 20, l = 10),
    axis.ticks.y = element_blank(),
    axis.line = element_line(color = "black", linewidth = 0.2, linetype = "solid"),
    axis.text.x = element_text(size = rel(0.9), angle = 40, hjust = 1,vjust = 1)
  ) +
  labs(title="Couple 1 enfant - conjoint au smic",size=5)+
  # scale_y_continuous(expand = c(0, 0), limits = c(0, 3000), breaks = seq(0, 3000, by = 500)) + 
  theme(panel.border = element_blank())

graphC1_S


#  Couple biactif - conjoint au smic - 2 enfants
smic=1398.7
nbuc=2.1
seuilpm=1309*nbuc

base<-castyp %>% 
  filter(isocou=="couple" & actconj=="1 smic" & nbenf==2) %>% 
  mutate(typrev=case_when(
    activite==0 ~ "Sans revenu",
    activite==0.5 ~ "Avec un mi-temps \n au Smic",
    activite==1 & Salaire==smic ~ "Avec un temps plein\n au Smic",
    TRUE ~ "Avec un temps plein \n à 1,5 Smic")
  ) %>% 
  select(typrev,CGNET,Ract_conj,Salaire,PF,PL,RSA,PA, RDISP_NET_CG)	

basegr<-base %>% 
  pivot_longer(cols=c(CGNET,Ract_conj,Salaire,PF,PL,RSA,PA, RDISP_NET_CG)) 



basegr$typrev<-factor(basegr$typrev,levels=c("Sans revenu","Avec un mi-temps \n au Smic","Avec un temps plein\n au Smic","Avec un temps plein \n à 1,5 Smic"))

basegr$name<-factor(basegr$name, levels=rev(c("CGNET","Ract_conj","Salaire", "PF", "PL", "RSA", "PA", "RDISP_NET_CG")))

graphC2_S <- ggplot(basegr %>% filter(name !='RDISP_NET_CG'), aes(fill = name,
                                                                  y = value, x = typrev)) + 
  scale_fill_manual(
    values = c("CGNET"="darkred",'Ract_conj' = '#440154FF',"Salaire"= "#482677FF", "PF" = "#39568CFF", "PL" = "#238A8DFF", 
               "RSA" = "#55C667FF", "PA" = "#95D840FF"),
    labels = c("CGNET"="Coût net de la garde",
               "PA" = "Prime d'activité",
               "RSA" = "Revenu de solidarité active",
               "PF" = "Prestations familiales",
               "PL" = "Prestations logement",
               "Salaire" = "Salaire" ,
               "Ract_conj"="Salaire conjoint"
    ),
    # guide = guide_legend(reverse = TRUE)  
  ) +
  geom_bar(
    position = "stack", 
    stat = "identity", 
    aes()
  ) +
  geom_hline(
    yintercept = seuilpm, 
    linetype = "dashed", 
    color = "lightgrey", 
    linewidth = 1
  ) +
  geom_text(
    aes(x = 0.5, y = seuilpm + 100, label = "Seuil de pauvreté (à 60%)"),
    size = 3, 
    hjust = 0, # Left-justified
    colour = "lightgrey"
  ) +
  geom_hline(
    yintercept = (4/6)*seuilpm, 
    linetype = "dashed", 
    color = "lightgrey", 
    linewidth = 1
  ) +
  geom_text(
    aes(x = 0.5, y = (4/6)*seuilpm + 100, label = "Seuil de grande \npauvreté (40%)"),
    size = 3, 
    hjust = 0, # Left-justified
    colour = "lightgrey"
  ) +
  geom_point(
    data = basegr %>% filter(name == 'RDISP_NET_CG'), 
    aes(
      x = typrev, y = round(value, 2), 
      colour = "Revenu disponible net du coût de la garde", 
    ), 
    size = 3
  ) +
  theme(
    panel.background = element_rect(fill = "white"), 
    plot.background = element_rect(fill = "white"),
    legend.position =  "right",
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    legend.text = element_text(size = rel(0.7)),
    legend.title = element_blank(),
    plot.title = element_text(size = rel(0.8), hjust = 0.7),
    plot.caption = element_text(size = rel(0.5)),
    plot.margin =  margin(t = 20, r = 10, b = 20, l = 10),
    axis.ticks.y = element_blank(),
    axis.line = element_line(color = "black", linewidth = 0.2, linetype = "solid"),
    axis.text.x = element_text(size = rel(0.9), angle = 40, hjust = 1,vjust = 1)
  ) +
  labs(title="Couple 2 enfants - conjoint au smic",size=5)+
  # scale_y_continuous(expand = c(0, 0), limits = c(0, 3000), breaks = seq(0, 3000, by = 500)) + 
  theme(panel.border = element_blank())

graphC2_S



#  Couple biactif - conjoint à 1,5 smic - 1 enfant
smic=1398.7
nbuc=1.6
seuilpm=1309*nbuc

base<-castyp %>% 
  filter(isocou=="couple" & actconj=="1,5 smic" & nbenf==1) %>% 
  mutate(typrev=case_when(
    activite==0 ~ "Sans revenu",
    activite==0.5 ~ "Avec un mi-temps \n au Smic",
    activite==1 & Salaire==smic ~ "Avec un temps plein\n au Smic",
    TRUE ~ "Avec un temps plein \n à 1,5 Smic")
  ) %>% 
  select(typrev,CGNET,Ract_conj,Salaire,PF,PL,RSA,PA, RDISP_NET_CG)	

basegr<-base %>% 
  pivot_longer(cols=c(CGNET,Ract_conj,Salaire,PF,PL,RSA,PA, RDISP_NET_CG)) 


basegr$typrev<-factor(basegr$typrev,levels=c("Sans revenu","Avec un mi-temps \n au Smic","Avec un temps plein\n au Smic","Avec un temps plein \n à 1,5 Smic"))

basegr$name<-factor(basegr$name, levels=rev(c("CGNET","Ract_conj","Salaire", "PF", "PL", "RSA", "PA", "RDISP_NET_CG")))

graphC1_15S <- ggplot(basegr %>% filter(name !='RDISP_NET_CG'), aes(fill = name,
                                                                  y = value, x = typrev)) + 
  scale_fill_manual(
    values = c("CGNET"="darkred",'Ract_conj' = '#440154FF',"Salaire"= "#482677FF", "PF" = "#39568CFF", "PL" = "#238A8DFF", 
               "RSA" = "#55C667FF", "PA" = "#95D840FF"),
    labels = c("CGNET"="Coût net de la garde",
               "PA" = "Prime d'activité",
               "RSA" = "Revenu de solidarité active",
               "PF" = "Prestations familiales",
               "PL" = "Prestations logement",
               "Salaire" = "Salaire" ,
               "Ract_conj"="Salaire conjoint"
    ),
    # guide = guide_legend(reverse = TRUE)  
  ) +
  geom_bar(
    position = "stack", 
    stat = "identity", 
    aes()
  ) +
  geom_hline(
    yintercept = seuilpm, 
    linetype = "dashed", 
    color = "lightgrey", 
    linewidth = 1
  ) +
  geom_text(
    aes(x = 0.5, y = seuilpm + 100, label = "Seuil de pauvreté (à 60%)"),
    size = 3, 
    hjust = 0, # Left-justified
    colour = "lightgrey"
  ) +
  geom_hline(
    yintercept = (4/6)*seuilpm, 
    linetype = "dashed", 
    color = "lightgrey", 
    linewidth = 1
  ) +
  geom_text(
    aes(x = 0.5, y = (4/6)*seuilpm + 100, label = "Seuil de grande \npauvreté (40%)"),
    size = 3, 
    hjust = 0, # Left-justified
    colour = "lightgrey"
  ) +
  geom_point(
    data = basegr %>% filter(name == 'RDISP_NET_CG'), 
    aes(
      x = typrev, y = round(value, 2), 
      colour = "Revenu disponible net du coût de la garde", 
    ), 
    size = 3
  ) +
  theme(
    panel.background = element_rect(fill = "white"), 
    plot.background = element_rect(fill = "white"),
    legend.position =  "right",
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    legend.text = element_text(size = rel(0.7)),
    legend.title = element_blank(),
    plot.title = element_text(size = rel(0.8), hjust = 0.7),
    plot.caption = element_text(size = rel(0.5)),
    plot.margin =  margin(t = 20, r = 10, b = 20, l = 10),
    axis.ticks.y = element_blank(),
    axis.line = element_line(color = "black", linewidth = 0.2, linetype = "solid"),
    axis.text.x = element_text(size = rel(0.9), angle = 40, hjust = 1,vjust = 1)
  ) +
  labs(title="Couple 1 enfant - conjoint à 1,5 smic",size=5)+
  # scale_y_continuous(expand = c(0, 0), limits = c(0, 3000), breaks = seq(0, 3000, by = 500)) + 
  theme(panel.border = element_blank())

graphC1_15S


#  Couple biactif - conjoint à 1,5 smic - 2 enfants
smic=1398.7
nbuc=2.1
seuilpm=1309*nbuc

base<-castyp %>% 
  filter(isocou=="couple" & actconj=="1,5 smic" & nbenf==2) %>% 
  mutate(typrev=case_when(
    activite==0 ~ "Sans revenu",
    activite==0.5 ~ "Avec un mi-temps \n au Smic",
    activite==1 & Salaire==smic ~ "Avec un temps plein\n au Smic",
    TRUE ~ "Avec un temps plein \n à 1,5 Smic")
  ) %>% 
  select(typrev,CGNET,Ract_conj,Salaire,PF,PL,RSA,PA, RDISP_NET_CG)	

basegr<-base %>% 
  pivot_longer(cols=c(CGNET,Ract_conj,Salaire,PF,PL,RSA,PA, RDISP_NET_CG)) 


basegr$typrev<-factor(basegr$typrev,levels=c("Sans revenu","Avec un mi-temps \n au Smic","Avec un temps plein\n au Smic","Avec un temps plein \n à 1,5 Smic"))

basegr$name<-factor(basegr$name, levels=rev(c("CGNET","Ract_conj","Salaire", "PF", "PL", "RSA", "PA", "RDISP_NET_CG")))

graphC2_15S <- ggplot(basegr %>% filter(name !='RDISP_NET_CG'), aes(fill = name,
                                                                  y = value, x = typrev)) + 
  scale_fill_manual(
    values = c("CGNET"="darkred",'Ract_conj' = '#440154FF',"Salaire"= "#482677FF", "PF" = "#39568CFF", "PL" = "#238A8DFF", 
               "RSA" = "#55C667FF", "PA" = "#95D840FF"),
    labels = c("CGNET"="Coût net de la garde",
               "PA" = "Prime d'activité",
               "RSA" = "Revenu de solidarité active",
               "PF" = "Prestations familiales",
               "PL" = "Prestations logement",
               "Salaire" = "Salaire" ,
               "Ract_conj"="Salaire conjoint"
    ),
    # guide = guide_legend(reverse = TRUE)  
  ) +
  geom_bar(
    position = "stack", 
    stat = "identity", 
    aes()
  ) +
  geom_hline(
    yintercept = seuilpm, 
    linetype = "dashed", 
    color = "lightgrey", 
    linewidth = 1
  ) +
  geom_text(
    aes(x = 0.5, y = seuilpm + 100, label = "Seuil de pauvreté (à 60%)"),
    size = 3, 
    hjust = 0, # Left-justified
    colour = "lightgrey"
  ) +
  geom_hline(
    yintercept = (4/6)*seuilpm, 
    linetype = "dashed", 
    color = "lightgrey", 
    linewidth = 1
  ) +
  geom_text(
    aes(x = 0.5, y = (4/6)*seuilpm + 100, label = "Seuil de grande \npauvreté (40%)"),
    size = 3, 
    hjust = 0, # Left-justified
    colour = "lightgrey"
  ) +
  geom_point(
    data = basegr %>% filter(name == 'RDISP_NET_CG'), 
    aes(
      x = typrev, y = round(value, 2), 
      colour = "Revenu disponible net du coût de la garde", 
    ), 
    size = 3
  ) +
  theme(
    panel.background = element_rect(fill = "white"), 
    plot.background = element_rect(fill = "white"),
    legend.position =  "right",
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    legend.text = element_text(size = rel(0.7)),
    legend.title = element_blank(),
    plot.title = element_text(size = rel(0.8), hjust = 0.7),
    plot.caption = element_text(size = rel(0.5)),
    plot.margin =  margin(t = 20, r = 10, b = 20, l = 10),
    axis.ticks.y = element_blank(),
    axis.line = element_line(color = "black", linewidth = 0.2, linetype = "solid"),
    axis.text.x = element_text(size = rel(0.9), angle = 40, hjust = 1,vjust = 1)
  ) +
  labs(title="Couple 2 enfants - conjoint à 1,5 smic",size=5)+
  # scale_y_continuous(expand = c(0, 0), limits = c(0, 3000), breaks = seq(0, 3000, by = 500)) + 
  theme(panel.border = element_blank())

graphC2_15S







# -------------------------------------------------------------------
# -------------------------------------------------------------------
# -------------------------------------------------------------------


castyp<-read.xlsx(file="cas types smic.xlsx",sheetName = "synthese") 

smic=1398.7

base<-castyp %>% 
  filter(isocou=="isolé" & nbenf==0) %>% 
  mutate(typrev=case_when(
    activite==0 ~ "Sans revenu",
    activite==0.5 ~ "Avec un mi-temps \n au Smic",
    activite==1 & Salaire==smic ~ "Avec un temps plein\n au Smic",
    TRUE ~ "Avec un temps plein \n à 1,5 Smic")
  ) %>% 
  select(typrev,Salaire,PF,PL,RSA,PA,	IR,	RDISP,	NVIE)	

basegr<-base %>% 
  pivot_longer(cols=c(Salaire,PF,PL,RSA,PA,RDISP)) 


seuilpm=1309


basegr$typrev <- factor(basegr$typrev)
basegr$typrev<-factor(basegr$typrev,levels=c("Sans revenu","Avec un mi-temps \n au Smic","Avec un temps plein\n au Smic","Avec un temps plein \n à 1,5 Smic"))

basegr$name <- factor(basegr$name)
basegr$name<-factor(basegr$name,
                    levels=c("PA","RSA","PL","PF","Salaire","RDISP"))

# Stacked

couleurs <- c('Salaire' = 'grey', 'IR'='red','PF' = 'aquamarine', 
              'PL' = 'darkcyan', 'RSA' = 'darkolivegreen',
              'PA' = 'darkgreen')

graph<-ggplot(basegr %>% filter(name !='RDISP'), aes(fill=name, y=value, x=typrev)) + 
  scale_fill_manual(values = couleurs) +
  geom_bar(position="stack", stat="identity")+
  geom_hline(yintercept=seuilpm,linetype="dashed",color="grey",linewidth=1)+
  geom_text(aes(x=1, y=seuilpm+50,label="Seuil de pauvreté monétaire"),size=4,colour="grey")+
  geom_point(data=basegr %>% filter(name=='RDISP'), 
             aes(x=typrev,y=value, 
                 colour="Revenu disponible"),size=3)+
  theme_minimal()+
  theme(panel.background = element_rect(fill = "white"), 
        plot.background = element_rect(fill = "white"),
        legend.position =  "right",
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.text=element_text(size = rel(0.8)),
        legend.title=element_blank(),
        plot.title = (element_text(size = rel(1.2),hjust=0.5)),
        plot.caption = element_text(size = rel(0.4)),
        axis.ticks.y = element_blank(),
        axis.line = element_line(color = "black", linewidth = 0.2, linetype = "solid"))+
  scale_y_continuous(expand = c(0, 0),limits=c(0,2250),breaks=seq(0, 2250, by = 250)) +
labs(title="Revenu disponible d'une personne seule selon son revenu professionnel" )


ggsave(graph,filename="graph/Cas type isolé sans enfant.png",width = 33,height = 14,units = "cm",dpi=300)


# --------------------------------------------------------------------------

base<-castyp %>% 
  filter(isocou=="couple" & nbenf==2 & actconj=="Inactif") %>% 
  mutate(typrev=case_when(
    activite==0 ~ "Sans revenu",in
    activite==0.5 ~ "Avec un mi-temps \n au Smic",
    activite==1 & Salaire==smic ~ "Avec un temps plein\n au Smic",
    TRUE ~ "Avec un temps plein \n à 1,5 Smic")
  ) %>% 
  select(typrev,Salaire,PF,PL,RSA,PA,	IR,	RDISP)	

basegr<-base %>% 
  pivot_longer(cols=c(Salaire,PF,PL,RSA,PA,RDISP)) 

nbuc=1+0.5+0.3+0.3
seuilpm=1309*nbuc


basegr$typrev <- factor(basegr$typrev)
basegr$typrev<-factor(basegr$typrev,levels=c("Sans revenu","Avec un mi-temps \n au Smic","Avec un temps plein\n au Smic","Avec un temps plein \n à 1,5 Smic"))

basegr$name <- factor(basegr$name)
basegr$name<-factor(basegr$name,
                    levels=c("PA","RSA","PL","PF","Salaire","RDISP"))

# Stacked

couleurs <- c('Salaire' = 'grey', 'IR'='red','PF' = 'darkgreen', 
              'PL' = 'forestgreen', 'RSA' = '',
              'PA' = '')

graph<-ggplot(basegr %>% filter(name !='RDISP'), aes(fill=name, y=value, x=typrev)) + 
  scale_fill_manual(values = couleurs) +
  geom_bar(position="stack", stat="identity")+
  geom_hline(yintercept=seuilpm,linetype="dashed",color="grey",linewidth=1)+
  geom_text(aes(x=1, y=seuilpm+50,label="Seuil de pauvreté monétaire"),size=4,colour="grey")+
  geom_point(data=basegr %>% filter(name=='RDISP'), 
             aes(x=typrev,y=value, 
                 colour="Revenu disponible"),size=3)+
  theme_minimal()+
  theme(panel.background = element_rect(fill = "white"), 
        plot.background = element_rect(fill = "white"),
        legend.position =  "right",
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.text=element_text(size = rel(0.8)),
        legend.title=element_blank(),
        plot.title = (element_text(size = rel(1.2),hjust=0.5)),
        plot.caption = element_text(size = rel(0.4)),
        axis.ticks.y = element_blank(),
        axis.line = element_line(color = "black", linewidth = 0.2, linetype = "solid"))+
  scale_y_continuous(expand = c(0, 0),limits=c(0,2250*nbuc),breaks=seq(0, 2250*nbuc, by = 250)) +
  labs(title="Revenu disponible d'une personne en couple avec 2 enfants selon son revenu professionnel\n(Conjoint inactif)" )


ggsave(graph,filename="graph/Cas type couple monoactif 2 enfants.png",width = 33,height = 14,units = "cm",dpi=300)

