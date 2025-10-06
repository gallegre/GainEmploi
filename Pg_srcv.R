
#################################
# SRCV
#################################

library(haven)
library(glue)
library(tidyverse)
library(MetricsWeighted)
library(questionr)
library(xlsx)
library(patchwork)
library(ggplot2)
library(ggrepel)


#  dom
dir<-"C:/Users/mpucci/OneDrive/Documents/DONNEES/SRCV/SRCV"

#  fac
 dir<-"D:/Utilisateurs/mpucci/OneDrive/Documents/DONNEES/SRCV/SRCV"

annee=2022

indiv <- read_sas(paste0(dir,annee,"/SAS/INDIVIDUS",substr(annee, 3, 4),".sas7bdat") |> glue())%>% 
    rename_with(.cols = everything(), .fn =\(x) toupper(x)) 
men <- read_sas(paste0(dir,annee,"/SAS/MENAGES",substr(annee, 3, 4),".sas7bdat") |> glue())%>% 
   rename_with(.cols = everything(), .fn =\(x) toupper(x)) 


indiv<-indiv %>%
    rename(IDENT=RB040,MD5HH3=MSD) %>% 
    select(IDENT,SEXE,VETA,CHAUSA,INVITA,LOISIRA,
           BESDENTR,BESMEDR,BESDENTR_DRAP,BESMEDR_DRAP,
           DEPENS,INTERNA,AGE,SEXE,COUPLE,NBENF,LIENPREF,
           PL032,PL040A,TYPEMPLOI,
           SITUADACT,DIM,PCS1,RB050,MD5HH3,
           PY090N,PY010N,PY050N,
           MALGRAV,SANETA,PCS2,
           PREST_PRECARITE_VIEIL,PREST_PRECARITE_HAND,PREST_PRECARITE_INVALIDITE,TEMPTRAV,DURCHOM) %>%
    mutate(PCS=PCS1) %>% 
    rename(CS_ACT=PCS1,STAT=PL032) %>% 
    mutate(NBENF=as.numeric(NBENF),
           AGE=as.numeric(AGE),
           ISO=if_else(COUPLE=="1",0,1),
           COU=if_else(COUPLE=="1",1,0),
           STATR=case_when(STAT=="1" & SITUADACT %in% c("1","2") ~ "1a",
                           STAT=="1" & SITUADACT %in% c("3","4") ~ "1b",
                           STAT=="2" ~ "2",
                           STAT=="3" ~ "3",
                            TRUE ~ "4"),
           P_MATSOC=case_when(MD5HH3=="1" ~ 1,TRUE ~0),
           RECH="3",MREC="3",DISPOCC="3",
           POIDS=as.numeric(RB050),
           ENF=case_when(LIENPREF=="02" & COUPLE %in% c("3","") & NBENF %in% c(0,NA)~1,TRUE ~ 0),
           ENFMIN=case_when(ENF == 1 & AGE < 18 ~ 1,TRUE ~ 0),
           MINEUR=case_when(AGE < 18 ~ 1,TRUE ~ 0),
           SALAIRES_I=case_when(!is.na(PY010N) ~ as.numeric(PY010N), TRUE ~0)
    ) %>%
    group_by(IDENT) %>%
    mutate(NBENFTOT=sum(ENF),NBENFMIN=sum(ENFMIN),NBMINEUR=sum(MINEUR)) %>% 
    ungroup()%>% 
    select(IDENT,ISO,COU,SEXE,AGE,COUPLE,NBENF,LIENPREF,
           VETA,CHAUSA,INVITA,LOISIRA,
           DEPENS,INTERNA,
           STATR,POIDS,
           SALAIRES_I,
           P_MATSOC,
           NBENFTOT,NBENFMIN,NBMINEUR
    )

  indiv <- indiv[order(indiv$IDENT),]


  men <- men %>%
    # filter(!ZEAT == "0") %>%
    rename(IDENT=DB030) %>% 
    select(IDENT,VAC,VIAND,NONPRE,TEMP,MEUB,VOIT,VOITB,IPLOG,IPLOY,IPELEC,IPCRED,NIVACTB,
           HY020,HX080,HX090,TUU2017,STOC,PROPRI,TYPMEN5,SITUAPR,HY022,
           HY060N,HH070,CHARGR,CHARGL,
           PREST_SOLIDARITE) %>%
    rename(TUU10=TUU2017) %>%
    mutate( AIDSOC=as.numeric(HY060N),
            HY022=as.numeric(HY022),
            HH070=as.numeric(HH070),
            RDISP=as.numeric(HY020),
            NVIEM=as.numeric(HX090),
            DIF1=case_when(NIVACTB == "1" ~ 1,TRUE ~ 0),
            DIF2=case_when(NIVACTB == "2" ~ 1,TRUE ~ 0),
            DIF3=case_when(NIVACTB == "3" ~ 1,TRUE ~ 0),
            PAUVRE=case_when(HX080=="TRUE" ~ 1,TRUE ~ 0),
            BEN_SOLIDARITE=case_when(PREST_SOLIDARITE>0 ~ 1 ,TRUE ~0)
    ) %>% select(-PREST_SOLIDARITE,-HX080,-HX090)



base <- inner_join(indiv,  men, by="IDENT") %>% mutate(year=annee-1)


# création des variables européennes
base <- base %>% 
  # création des indicatrices au niveau individu
  mutate(PR_VET=case_when(VETA == "2" ~ 1,
                          (VETA == "1" | VETA == "3") ~ 0,
                          TRUE~NA),
         PR_CHAUS=case_when(CHAUSA == "2" ~ 1,
                            (CHAUSA == "1" | CHAUSA == "3") ~ 0,
                            TRUE ~ NA),
         PR_INVIT=case_when(INVITA == "2" ~ 1,
                            (INVITA == "1" | INVITA == "3") ~ 0,
                            TRUE ~ NA),
         PR_LOISIR=case_when(LOISIRA == "2" ~ 1,
                             (LOISIRA == "1" | LOISIRA == "3") ~ 0,
                             TRUE ~ NA),
         PR_DEP_SOI=case_when(DEPENS == "2" ~ 1,
                              (DEPENS == "1" | DEPENS == "3") ~ 0,
                              TRUE ~ NA),
         PR_INTERNET=case_when(INTERNA == "2" ~ 1,
                               (INTERNA == "1" | INTERNA == "3") ~ 0,
                               TRUE ~ NA)) %>% 
  # création des indicatrices au niveau ménage
  mutate(PRIV_VAC=case_when(VAC == "2" ~ 1,VAC == "1" ~ 0, TRUE ~ NA),
         RENS_VAC=case_when((VAC=="1" | VAC=="2")~1,TRUE~0),
         PRIV_VIAND=case_when(VIAND == "2" ~ 1,VIAND == "1" ~ 0,TRUE ~ NA),
         RENS_VIAND=case_when((VIAND=="1" | VIAND=="2")~1, TRUE~0),
         PRIV_NONPRE=case_when(NONPRE == "2" ~ 1, NONPRE == "1" ~ 0,TRUE ~ NA),
         RENS_NONPRE=case_when((NONPRE=="1" | NONPRE=="2")~1, TRUE~0),
         PRIV_TEMP=case_when(TEMP == "2" ~ 1, TEMP == "1" ~ 0, TRUE ~ NA),
         RENS_TEMP=case_when((TEMP=="1" | TEMP=="2")~1, TRUE~0),
         PRIV_MEUB=case_when(MEUB == "2" ~ 1, MEUB == "1" ~ 0, TRUE ~ NA),
         RENS_MEUB=case_when((MEUB=="1" | MEUB=="2")~1, TRUE~0),
         PRIV_VOIT=case_when(VOIT=="1"  ~ 0, (VOIT=="2" & VOITB=="1") ~ 1, (VOIT=="2" & VOITB=="2") ~ 0, TRUE ~ NA),
         RENS_VOIT=case_when((PRIV_VOIT==1 | PRIV_VOIT==0)~1, TRUE~0),
         PRIV_LOG=case_when(((IPLOG=="1" | IPLOG=="2") | (IPLOY=="1" | IPLOY=="2"))~ 1, (IPLOG=="3" | IPLOY=="3") ~ 0, TRUE ~ NA),
         RENS_LOG=case_when((PRIV_LOG==1 | PRIV_LOG==0)~1, TRUE~0),
         PRIV_ELEC=case_when((IPELEC=="1" | IPELEC=="2") ~ 1, IPELEC=="3" ~ 0, TRUE ~ NA),
         RENS_ELEC=case_when((PRIV_ELEC==1 | PRIV_ELEC==0)~1, TRUE~0),
         PRIV_CRED=case_when((IPCRED=="1" | IPCRED=="2") ~ 1, IPCRED=="3" ~ 0, TRUE ~ NA),
         RENS_CRED=case_when((PRIV_CRED==1 | PRIV_CRED==0)~1, TRUE~0),
         PRIV_PAIEMENT=case_when((PRIV_LOG==1 | PRIV_ELEC==1 | PRIV_CRED==1) ~ 1, TRUE ~ 0)
  ) %>% 
  group_by(IDENT) %>%
  # on calcule la moyenne par ménage des privations individuelles
  mutate(PRM_VET = mean(PR_VET, na.rm = TRUE),
         PRM_CHAUS = mean(PR_CHAUS, na.rm = TRUE),
         PRM_INVIT = mean(PR_INVIT, na.rm = TRUE),         
         PRM_LOISIR = mean(PR_LOISIR, na.rm = TRUE),
         PRM_DEP_SOI = mean(PR_DEP_SOI, na.rm = TRUE),
         PRM_INTERNET = mean(PR_INTERNET, na.rm = TRUE)
  ) %>% 
  ungroup() %>% 
  # Si la moyenne est >=0.5, tout le ménage a 1
  mutate(PRMEN_VET = case_when(PRM_VET>=0.5 ~ 1,PRM_VET<0.5 ~ 0),
         PRMEN_CHAUS = case_when(PRM_CHAUS>=0.5 ~ 1,PRM_CHAUS<0.5 ~ 0),
         PRMEN_INVIT = case_when(PRM_INVIT>=0.5 ~ 1,PRM_INVIT<0.5 ~ 0),         
         PRMEN_LOISIR = case_when(PRM_LOISIR>=0.5 ~ 1,PRM_LOISIR<0.5 ~ 0) ,
         PRMEN_DEP_SOI = case_when(PRM_DEP_SOI>=0.5 ~ 1,PRM_DEP_SOI<0.5 ~ 0),
         PRMEN_INTERNET = case_when(PRM_INTERNET>=0.5 ~ 1,PRM_INTERNET<0.5 ~ 0)
  ) %>% 
  # Les moins de 16 ans ou les sans réponse indiv se voient attribuer la privation du ménage (pour les pribvations indiv)
  mutate(PRIV_VET = case_when(AGE<16 ~ PRMEN_VET,is.na(PR_VET) ~ PRMEN_VET, TRUE ~ PR_VET),
         PRIV_CHAUS = case_when(AGE<16 ~ PRMEN_CHAUS,is.na(PR_CHAUS) ~ PRMEN_CHAUS, TRUE ~ PR_CHAUS),
         PRIV_INVIT = case_when(AGE<16 ~ PRMEN_INVIT,is.na(PR_INVIT) ~ PRMEN_INVIT, TRUE ~ PR_INVIT),        
         PRIV_LOISIR = case_when(AGE<16 ~ PRMEN_LOISIR,is.na(PR_LOISIR) ~ PRMEN_LOISIR, TRUE ~ PR_LOISIR),
         PRIV_DEP_SOI = case_when(AGE<16 ~ PRMEN_DEP_SOI,is.na(PR_DEP_SOI) ~ PRMEN_DEP_SOI, TRUE ~ PR_DEP_SOI),
         PRIV_INTERNET = case_when(AGE<16 ~ PRMEN_INTERNET,is.na(PR_INTERNET) ~ PRMEN_INTERNET, TRUE ~ PR_INTERNET)
  ) %>% 
  select(year,IDENT,POIDS,c(starts_with("PRIV_")),c(starts_with("NB")),
         AGE,SEXE,COUPLE,-NBENF,LIENPREF,ISO,COU,STATR,RDISP,
         NIVACTB,PAUVRE,NVIEM,TYPMEN5,NBENFTOT,NBENFMIN,NBMINEUR,SITUAPR,HY022,
         P_MATSOC,DIF1,DIF2,DIF3,
         SALAIRES_I)

base<-base %>% 
  # transformation des NA en 0
  mutate(PRIV_VET = case_when(PRIV_VET==1 ~ 1, TRUE ~ 0),
         PRIV_CHAUS = case_when(PRIV_CHAUS==1 ~ 1, TRUE ~ 0),
         PRIV_INVIT = case_when(PRIV_INVIT==1 ~ 1, TRUE ~ 0),        
         PRIV_LOISIR = case_when(PRIV_LOISIR==1 ~ 1, TRUE ~ 0),
         PRIV_DEP_SOI = case_when(PRIV_DEP_SOI==1 ~ 1, TRUE ~ 0),
         PRIV_INTERNET = case_when(PRIV_INTERNET==1 ~ 1, TRUE ~ 0),
         PRIV_VAC = case_when(PRIV_VAC==1 ~ 1, TRUE ~ 0),
         PRIV_VIAND = case_when(PRIV_VIAND==1 ~ 1, TRUE ~ 0),
         PRIV_NONPRE = case_when(PRIV_NONPRE==1 ~ 1, TRUE ~ 0),
         PRIV_TEMP = case_when(PRIV_TEMP==1 ~ 1, TRUE ~ 0),
         PRIV_MEUB = case_when(PRIV_MEUB==1 ~ 1, TRUE ~ 0),
         PRIV_VOIT = case_when(PRIV_VOIT==1 ~ 1, TRUE ~ 0),
         PRIV_PAIEMENT = case_when(PRIV_PAIEMENT==1 ~ 1, TRUE ~ 0),
         NB_PRMEN=PRIV_VAC+PRIV_VIAND+PRIV_NONPRE+PRIV_TEMP+PRIV_MEUB+PRIV_VOIT+PRIV_PAIEMENT,
         NB_PRIV=NB_PRMEN+PRIV_VET+PRIV_CHAUS+PRIV_INVIT+PRIV_LOISIR+PRIV_DEP_SOI+PRIV_INTERNET,
         pms=case_when(is.na(NB_PRIV) ~ NA,
                              AGE>=16 & NB_PRIV>4 ~ 1,
                              AGE<16 & NB_PRIV>4 & NB_PRMEN>2 ~ 1,
                              TRUE ~ 0)
  ) %>% 
  # Variables de catégories
  mutate(ZZ="Ensemble",
         INDIV=1,
         ANNEE=annee,
         AGE=as.numeric(AGE),
         HOMME=case_when(SEXE=="1" ~ 1,TRUE ~ 0),
         FEMME=case_when(SEXE=="2" ~ 1,TRUE ~ 0),
         CHAMP=case_when(SITUAPR=="3" | HY022<0 ~ 0,TRUE ~ 1),
         TYPFAMR=case_when(TYPMEN5 == "1" ~ "1. Personne seule",
                           TYPMEN5 == "2" & NBENFTOT>0 ~ "2. Famille monoparentale",
                           TYPMEN5 == "3" ~ "3. Couple sans enfant",
                           TYPMEN5 == "4" & NBENFTOT <=2 ~ "4 Couple avec 1 ou 2 enfants",
                           TYPMEN5 == "4" & NBENFTOT>2 ~ "5. Couple avec 3 enfants ou plus",
                           TRUE ~ "6. Ménage complexe"),
         SAL=case_when(STATR =="1a" ~ 1, TRUE ~ 0)
         ) 

smic_dec<-12*1329*(1+0.9825*(0.005+0.024)/(1-0.2262))

base_sal<-base %>% filter(SAL==1 & SALAIRES_I>0) %>% 
  mutate(tranche_sal=case_when(SALAIRES_I<0.3*smic_dec ~ "TR1 - moins de 0,3 smic",
                               SALAIRES_I<0.6*smic_dec ~ "TR2 - de 0,3 à moins de 0,6 smic",
                               SALAIRES_I<0.9*smic_dec ~ "TR3 - de 0,6 à moins de 0,9 smic",
                               SALAIRES_I<1.2*smic_dec ~ "TR4 - de 0,9 à moins de 1,2 smic",
                               SALAIRES_I<1.5*smic_dec ~ "TR5 - de 1,2 à moins de 1,5 smic",
                               SALAIRES_I<1.8*smic_dec ~ "TR6 - de 1,5 à moins de 1,8 smic",
                               TRUE ~ "TR7 - 1,8 smic et plus"),
         tranche_sal2=case_when(SALAIRES_I<0.55*smic_dec ~ "TR01 - moins de 0,55 smic",
                                SALAIRES_I<0.65*smic_dec ~ "TR02 - de 0,55 à moins de 0,65 smic",
                                SALAIRES_I<0.75*smic_dec ~ "TR03 - de 0,65 à moins de 0,75 smic",
                                SALAIRES_I<0.85*smic_dec ~ "TR04 - de 0,75 à moins de 0,85 smic",
                               SALAIRES_I<0.95*smic_dec ~ "TR05 - de 0,85 à moins de 0,95 smic",
                               SALAIRES_I<=1.05*smic_dec ~ "TR06 - de 0,95 à 1,05 smic",
                               SALAIRES_I<1.15*smic_dec ~ "TR07 - de 1,05 à moins de 1,15 smic",
                               SALAIRES_I<1.25*smic_dec ~ "TR08 - de 1,15 à moins de 1,25 smic",
                               SALAIRES_I<1.35*smic_dec ~ "TR09 - de 1,25 à moins de 1,35 smic",
                               SALAIRES_I<1.45*smic_dec ~ "TR10 - de 1,35 à moins de 1,45 smic",
                               SALAIRES_I<1.55*smic_dec ~ "TR11 - de 1,45 à moins de 1,55 smic",
                               SALAIRES_I<1.65*smic_dec ~ "TR12 - de 1,55 à moins de 1,65 smic",
                               SALAIRES_I<1.75*smic_dec ~ "TR13 - de 1,65 à moins de 1,75 smic",
                               TRUE ~ "TR14 - 1,75 smic et plus"),
         tranche_sal3=case_when(SALAIRES_I<0.5*smic_dec ~ "TR01 - moins de 0,5 smic",
                               SALAIRES_I<0.7*smic_dec ~ "TR02 - de 0,5 à moins de 0,7 smic",
                               SALAIRES_I<0.9*smic_dec ~ "TR03 - de 0,7 à moins de 0,9 smic",
                               SALAIRES_I<1.1*smic_dec ~ "TR04 - de 0,9 à moins de 1,1 smic",
                               SALAIRES_I<1.3*smic_dec ~ "TR05 - de 1,1 à moins de 1,3 smic",
                               SALAIRES_I<1.5*smic_dec ~ "TR06 - de 1,3 à moins de 1,5 smic",
                               SALAIRES_I<1.7*smic_dec ~ "TR07 - de 1,5 à moins de 1,7 smic",
                               SALAIRES_I<1.9*smic_dec ~ "TR08 - de 1,7 à moins de 1,9 smic",
                               TRUE ~ "TR09 - 1,9 smic et plus"),
         inf_smic=if_else(SALAIRES_I<smic_dec,1,0)
         )

stat <- base_sal %>% 
  select(ZZ,POIDS,INDIV,ISO,COU,tranche_sal,tranche_sal2,tranche_sal3,inf_smic,pms,P_MATSOC,DIF1,DIF2,DIF3,PAUVRE,NB_PRIV) %>%
  gather(variable, modalite, -c(INDIV,ISO,COU,POIDS,pms,P_MATSOC,DIF1,DIF2,DIF3,PAUVRE,NB_PRIV)) %>%
  group_by(variable, modalite) %>%
  summarise(EFF=sum(INDIV),
            EFF_ISO=sum(ISO),
            EFF_COU=sum(COU),
            NTOT=sum(INDIV*POIDS),
            NISO=sum(ISO*POIDS),
            NCOU=sum(COU*POIDS),
            NB_PRIV_MOY=weighted.mean(NB_PRIV,w=POIDS),
            NB_PMS=sum(P_MATSOC*POIDS),
            NB_TRES_DIF=sum(DIF1*POIDS),
            NB_PAUVRE=sum(PAUVRE*POIDS),
            NISO_PRIV_MOY=weighted.mean(ISO*NB_PRIV,w=POIDS),
            NISO_PMS=sum(ISO*P_MATSOC*POIDS),
            NISO_TRES_DIF=sum(ISO*DIF1*POIDS),
            NISO_PAUVRE=sum(ISO*PAUVRE*POIDS),
            NCOU_PRIV_MOY=weighted.mean(COU*NB_PRIV,w=POIDS),
            NCOU_PMS=sum(COU*P_MATSOC*POIDS),
            NCOU_TRES_DIF=sum(COU*DIF1*POIDS),
            NCOU_PAUVRE=sum(COU*PAUVRE*POIDS)
  ) %>% 
  mutate(TX_PMS=NB_PMS/NTOT,
         TX_ISO_PMS=NISO_PMS/NISO,
         TX_COU_PMS=NCOU_PMS/NCOU,
         TX_PAUVRETE=NB_PAUVRE/NTOT,
         TX_ISO_PAUVRETE=NISO_PAUVRE/NISO,
         TX_COU_PAUVRETE=NCOU_PAUVRE/NCOU,
         TX_FORTES_DIF_FIN=NB_TRES_DIF/NTOT,
         TX_ISO_FORTES_DIF_FIN=NISO_TRES_DIF/NISO,
         TX_COU_FORTES_DIF_FIN=NCOU_TRES_DIF/NCOU
         ) %>% 
  select(-c(NB_PMS,NB_TRES_DIF,NB_PAUVRE,
            NISO_PMS,NISO_TRES_DIF,NISO_PAUVRE,
            NCOU_PMS,NCOU_TRES_DIF,NCOU_PAUVRE))




library(openxlsx)
wb<-createWorkbook()
addWorksheet(wb,"SALARIES")


writeData(wb,sheet="SALARIES",x=as.data.frame(stat))

saveWorkbook(wb,file="pauvreté et privations selon le salaire annuel.xlsx",overwrite = TRUE)
