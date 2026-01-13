Projet : Analyse de Performance Étudiante

Description :
Ce projet vise à analyser la performance académique des étudiants selon différents groupes 
(filière, genre, âge, heures d'étude, absences) et à identifier les facteurs influençant 
la réussite.

Structure des fichiers :
- 01_import_nettoyage.R        : Importation et nettoyage des données brutes
- 02_analyses_descriptives.R   : Statistiques descriptives globales et par groupes
- 03_analyses_comparatives.R   : Comparaisons entre groupes (filière, genre, âge)
- 04_analyses_performances.R   : Analyse détaillée des performances par matière et semestre
- 05_analyses_facteurs.R       : Analyse des facteurs de réussite (heures d'étude, absences)
- 06_visualisations.R          : Graphiques et visualisations finales
- main.R                       : Script principal qui exécute tout le projet dans l'ordre
- output/                      : Dossier contenant tous les résultats
    - tables/                  : Fichiers CSV des analyses
    - figures/                 : Graphiques PNG générés

Instructions d'exécution :
1. Ouvrir le projet RStudio : mon_projet_1.Rproj
2. Installer les packages nécessaires :
   tidyverse, ggplot2, readr, dplyr, tidyr, moments.
   (Exemple : install.packages(c("tidyverse", "ggplot2", "readr", "dplyr", "tidyr", "moments")))
3. Exécuter le script principal `main.R` pour lancer toutes les analyses.
4. Vérifier les résultats dans le dossier `output` :
   - Tables CSV : output/tables/
   - Graphiques PNG : output/figures/

Auteur : Frantzdy DORILUS
Date   : 2025-12-10
