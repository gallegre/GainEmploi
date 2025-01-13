# library(haven)
# library(glue)
library(tidyverse)
# # library(readr)
# library(dplyr)
# library(reshape2)
# library(MetricsWeighted)
# library(questionr)
library(xlsx)
library(patchwork)
library(ggplot2)
# library(ggrepel)
# library(blorr)


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

couleurs <- c('Salaire' = 'grey', 'IR'='red','PF' = 'gold', 
              'PL' = 'forestgreen', 'RSA' = 'lightblue',
              'PA' = 'darkblue')

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
    activite==0 ~ "Sans revenu",
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

couleurs <- c('Salaire' = 'grey', 'IR'='red','PF' = 'gold', 
              'PL' = 'forestgreen', 'RSA' = 'lightblue',
              'PA' = 'darkblue')

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

