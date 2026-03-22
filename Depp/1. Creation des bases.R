
############################################################################################################################################################################
############################################################################################################################################################################

                                                                            # 1. Creation des bases



# Ce programme vise à préparer les données qui serviront pour les cartes. 
# Il se décompose comme suit : 
  
  # A) Installation des packages et import des couches.
  # B) Import et préparation des données Cyclades.
  # C) Import et préparation des données Agri.
  # D) Fusion des données CYCLADES et Agri.
  # E) Création des tables finales pour les cartes.


############################################################################################################################################################################
############################################################################################################################################################################




#################################################################
#################################################################

# A) Installation des packages et import des couches.

#################################################################
#################################################################

# Installation des packages non presents #
install.packages("BAMMtools")
install.packages("sf")
install.packages("tmap")
install.packages("tmaptools")
install.packages("showtext")
install.packages("xlsx")

# Ouverture des packages 
library(sf)
library(tidyverse)
library(tmap)
library(tmaptools)
library(BAMMtools)
library(xlsx)
library(showtext)
library(Rcpp)
library(data.table)

font_paths("M:/prj-depp-cartographie/Documentation/Consignes aux auteurs de cartes/Cartes R/MARIANNE V2")
font_add(family = "Marianne", regular = "Marianne-Regular.otf")
showtext_auto()
options(bitmapType='cairo')


#### Import des couches ####

# Données à l'académie avec habillage région
CPCP <- st_read("M:/prj-depp-cartographie/Couches SIG/Zonages Admin/Académies/academies_FRMetroDrom_zoomParis.shp", quiet=T)
CHAB <- st_read("C:/Users/tberut/Desktop/Journée du BAC/2022/Couches/regions_FrMDrom.shp", quiet=T)

#### Eléments complémentaires ####

# Systématique
LIPA <- st_read("M:/prj-depp-cartographie/Documentation/Consignes aux auteurs de cartes/Cartes R/ligne_Paris_R.shp", quiet=T)
LIBL <- st_read("M:/prj-depp-cartographie/Documentation/Consignes aux auteurs de cartes/Cartes R/ligne_R.shp", quiet=T)

# Optionnel dans le cas où les données de Mayotte soient manquantes
DONMQ <- st_read("M:/prj-depp-cartographie/Documentation/Consignes aux auteurs de cartes/Cartes R/polygon_R6.shp", quiet=T)
MAYN <- st_read("M:/prj-depp-cartographie/Documentation/Consignes aux auteurs de cartes/Cartes R/mayotte.shp", quiet=T)



#################################################################
#################################################################

# B) Import et préparation des données Cyclades

#################################################################
#################################################################

#### Import et dedoublement des données education ####

# Import des données Educ
DON <- read.csv2("C:/Users/tberut/Desktop/Journée du BAC/2022/Journée du 09.07/extraction.csv") 
#DON <- read.xlsx2("C:/Users/tberut/Desktop/Journée du BAC/2022/Cartes/Exemple 2021/test.xlsx", 1)


# On met des 0 apparents aux académie et on recode la Normandie 
DON$Code.académie <- sprintf("%02d",DON$acam) # pour rajouter 0 apparents
DON$Code.académie <- fct_recode(DON$Code.académie,
                                "70" = "05",
                                "70" = "21")

table(DON$Code.académie)


# Deboublement des lignes : une ligne = un candidat

DONbis=as.data.frame(DON)
DONb<-DONbis[rep(row.names(DONbis), DONbis$effectif),]
DON<-DONb

DON$effectif<-NULL


# Creation des variables INSCRITS, PRESENTS, ADMIS

DON$INSCRIT=1
DON$PRESENT <- ifelse(DON$decision_1 %in% c("A","B","C","D","Y","R","E", "U", "V", "W", "P", "L"), 1, 0)

DON$ADMIS_G1 <- ifelse(DON$decision_1 %in% c("A","B","C","D", "Y"), 1, 0)
DON$ADMIS_G2 <- ifelse(DON$decision_1 %in% c("P","L") & DON$decision_11 %in% c("D"), 1, 0) # P ou L au premier groupe => admis a se presenter au second groupe, on prend resultat decision_11


DON$ADMIS <- ifelse(DON$ADMIS_G1==1 | DON$ADMIS_G2==1, 1, 0)

DON$TUTELLE = "education"

# Division de la table par série

BCG=filter(DON, codeqp=="BCG")
BCP=filter(DON, domaine=="BCP")
BCT=filter(DON, domaine=="BGT" & codeqp!="BCG")



#################################################################
#################################################################

# C) Import et préparation des données Agri

#################################################################
#################################################################

#### Import des données agri ####

Agri1 <- read.csv2("C:/Users/tberut/Desktop/Journée du BAC/2022/Journée du 09.07/Agri/resultat_p.csv", header=FALSE) 
Agri2 <- read.csv2("C:/Users/tberut/Desktop/Journée du BAC/2022/Journée du 09.07/Agri/resultat_s.csv", header=FALSE) 
Agri3 <- read.csv2("C:/Users/tberut/Desktop/Journée du BAC/2022/Journée du 09.07/Agri/resultat_t.csv", header=FALSE) 

colnames(Agri1) <-c("Académie", "presents", "admis", "ajournés")
colnames(Agri2) <-c("Académie", "presents", "admis", "ajournés")
colnames(Agri3) <-c("Académie", "presents", "admis", "ajournés")


## BCPA

#Production
Agri_p<-setDT(Agri1)[, list(ADMIS=rep(c(1,0), c(admis, presents-admis))) , "Académie"]
Agri_p$PRESENT=1
Agri_p$domaine="BCP"
Agri_p$sessionnat=as.integer(2022)
Agri_p$Code.académie <- sprintf("%02d",Agri_p$Académie) # pour rajouter 0 apparents
Agri_p<-Agri_p %>% select(-"Académie")

#Service
Agri_s<-setDT(Agri2)[, list(ADMIS=rep(c(1,0), c(admis, presents-admis))) , "Académie"]
Agri_s$PRESENT=1
Agri_s$domaine="BCP"
Agri_s$sessionnat=as.integer(2022)
Agri_s$Code.académie <- sprintf("%02d",Agri_s$Académie) # pour rajouter 0 apparents
Agri_s<-Agri_s %>% select(-"Académie")


#Fusion des deux bases. !!! Avec rbind seules les variables communes sont conservées
BCPA<-rbind(Agri_p, Agri_s)
BCPA$TUTELLE="agri"
BCPA$Code.académie <- fct_recode(BCPA$Code.académie,
                                "70" = "05",
                                "70" = "21")

table(BCPA$Code.académie)



## BCTA
BCTA<-setDT(Agri3)[, list(ADMIS=rep(c(1,0), c(admis, presents-admis))) , "Académie"]
BCTA$PRESENT=1
BCTA$domaine="BCT"
BCTA$sessionnat=as.integer(2022)
BCTA$Code.académie <- sprintf("%02d",BCTA$Académie) # pour rajouter 0 apparents
BCTA<-BCTA %>% select(-"Académie")
BCTA$TUTELLE="agri"
BCTA$Code.académie <- fct_recode(BCTA$Code.académie,
                                 "70" = "05",
                                 "70" = "21")

table(BCTA$Code.académie)


#################################################################
#################################################################

# D) Fusion des données CYCLADES et Agri

#################################################################
#################################################################

#### Merge des données éduc et agri ####

# BCG : pas de donnée agri pour la série générale

# BCP
BCP<-BCP %>% select ("sessionnat", "domaine", "Code.académie", "PRESENT", "ADMIS", "TUTELLE")
BCP<-rbind(BCP, BCPA)

BCP$Code.académie<-as.character(BCP$Code.académie)
BCP <- BCP %>% arrange(Code.académie)


# BCT
BCT<-BCT %>% select ("sessionnat", "domaine", "Code.académie", "PRESENT", "ADMIS", "TUTELLE")
BCT<-rbind(BCT, BCTA)

BCT$Code.académie<-as.character(BCT$Code.académie)
BCT <- BCT %>% arrange(Code.académie)

#sum(CPCP_BCP$PRESENTS)
#sum(CPCP_BCP$ADMIS)


#################################################################
#################################################################

# E) Création des tables finales pour les cartes.

#################################################################
#################################################################

### Creer les statistiques de taux de reussite par academie et par serie

# Calcul des taux de reussite par academie

Tab_BCG=summarise(group_by(BCG,Code.académie),PRESENTS=sum(PRESENT), ADMIS=sum(ADMIS), Taux_Reussite=sum(ADMIS)/sum(PRESENT)*100)
Tab_BCP=summarise(group_by(BCP,Code.académie),PRESENTS=sum(PRESENT), ADMIS=sum(ADMIS), Taux_Reussite=sum(ADMIS)/sum(PRESENT)*100)
Tab_BCT=summarise(group_by(BCT,Code.académie),PRESENTS=sum(PRESENT), ADMIS=sum(ADMIS), Taux_Reussite=sum(ADMIS)/sum(PRESENT)*100)


# Concaténation des données avec la couche principale pour rajouter le libelle
CPCP_BCG <- left_join(Tab_BCG, CPCP, by = c("Code.académie" = "CODE_ACA")) # Si données académiques, vérifier que dans les données 
# la variable des codes académiques est bien Code.académie
# Penser à vérifier que les codes à un chiffre commencent
# bien par 0, exemple : "01"
CPCP_BCP <- left_join(Tab_BCP, CPCP, by = c("Code.académie" = "CODE_ACA"))
CPCP_BCT <- left_join(Tab_BCT, CPCP, by = c("Code.académie" = "CODE_ACA"))


# Modification du format des variables d'interet

CPCP_BCG$VAR_G <- as.numeric(CPCP_BCG$Taux_Reussite) 
CPCP_BCT$VAR_T <- as.numeric(CPCP_BCT$Taux_Reussite) 
CPCP_BCP$VAR_P <- as.numeric(CPCP_BCP$Taux_Reussite)

