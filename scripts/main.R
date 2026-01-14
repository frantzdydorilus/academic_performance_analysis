# ===========================================================================================================
# SCRIPT MAITRE : Exécution séquentielle de toutes les étapes du projet
# ===========================================================================================================

# Chargement des librairies essentielles

library(tidyverse)
library(ggplot2)
library(dplyr)
library(tidyr)
library(moments)

#========================= CRÉATION DE LA STRUCTURE =========================================================

dir.create("data/raw", recursive = TRUE, showWarnings = FALSE)
dir.create("data/processed", recursive = TRUE, showWarnings = FALSE)
dir.create("scripts", showWarnings = FALSE)
dir.create("output/figures", recursive = TRUE, showWarnings = FALSE)
dir.create("output/tables", recursive = TRUE, showWarnings = FALSE)

#============================================================================================================
#                           ETAPE 0 : FONCTIONS
#============================================================================================================


cat("\n*** ÉTAPE 0 : Exécution du script 00_fonctions.R (Nettoyage) ***\n")
source("scripts/00_fonctions.R")

#============================================================================================================
#                           ÉTAPE 1 : IMPORTATION ET NETTOYAGE DES DONNÉES (01) 
#============================================================================================================

cat("\n*** ÉTAPE 1 : Exécution du script 01_import_nettoyage.R (Nettoyage) ***\n")
source("scripts/01_import_nettoyage.R")

# CHARGEMENT DES DONNÉES NETTOYÉES
data_projet <- read_csv("data/processed/donnees_nettoyees.csv")


#===========================================================================================================
#                           ÉTAPE 2 : ANALYSES DESCRIPTIVES (02)
#===========================================================================================================

#    ANALYSE 1 : Statistiques Descriptives (02) 

cat("\n*** ANALYSE 1 : Exécution du script 02_analyses_descriptives.R ***\n")
source("scripts/02_analyses_descriptives.R")

#===========================================================================================================
#                           ÉTAPE 3 : ANALYSES COMPARATIVES (03)
#===========================================================================================================

#    ANALYSE 2 : Analyses Comparatives (03) 

# Regroupe l'Évolution S1->S2, Performance par Groupes, et Facteurs de Réussite
cat("\n*** ANALYSE 2 : Exécution du script 03_analyses_comparatives.R ***\n")
source("scripts/03_analyses_comparatives.R")

#===========================================================================================================
#                           ÉTAPE 4 : ANALYSES PERFORMANCES (04)
#===========================================================================================================

#     ANALYSE 3 : Analyse Performances (04)

cat("\n*** ANALYSE 3  : Exécution du script 04_analyses_performances.R ***\n")
source("scripts/04_analyses_performances.R")

#===========================================================================================================
#                           ÉTAPE 5 : ANALYSES FACTEURS (05)
#===========================================================================================================

#     ANALYSE 4 : Analyses Facteurs (05) 

cat("\n*** ANALYSE 4  : Exécution du script 05_analyses_facteurs.R ***\n")
source("scripts/05_analyses_facteurs.R")

#===========================================================================================================
#                           ÉTAPE 6 : VISUALISATIONS (06)
#===========================================================================================================

#    ANALYSE 5 : Visualisations (06) 

cat("\n*** ANALYSE 5 : Exécution du script 04_visualisations.R ***\n")
source("scripts/06_visualisations.R") 

#===========================================================================================================
#                                 FIN D'EXECUTION
#===========================================================================================================

# --- Fin du Script Maitre ---
cat("\n*** Projet exécuté. ***\n")

