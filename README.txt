# Analyse de la Performance Étudiante

**Cours :** BSC-ADD-201 | BDC-ADD-2  
**Auteur :** Frantzdy DORILUS  
**Date :** Décembre 2025  

---

##  Description du projet

Ce projet analyse les données de performance académique de **200 étudiants** sur deux semestres (**S1** et **S2**).  
L’objectif est d’identifier les **facteurs influençant la réussite académique**, notamment :

- la filière  
- le genre  
- l’âge  
- les heures d’études  
- le nombre d’absences  

Les résultats visent à formuler des **recommandations pédagogiques fondées sur l’analyse statistique**.

---

##  Structure du projet

```text
PROJET_PERFORMANCE/
-├── data/
-│   ├── raw/
-│   │   └── etudiants_performance.csv
-│   └── processed/
-├── scripts/
-├   ├── 00_fonctions.R
-│   ├── 01_import_nettoyage.R
-│   ├── 02_analyses_descriptives.R
-│   ├── 03_analyses_comparatives.R
-│   ├── 04_analyses_performances.R
-│   ├── 05_analyses_facteurs.R
-│   └── 06_visualisations.R
-├── output/
-│   ├── figures/
-│   └── tables/
-├── main.R
-├── fonctions.R
-└── README.md
```

---

## ️ Technologies utilisées

- **Langage** : R  
- **IDE recommandé** : RStudio  
- **Packages** : tidyverse, dplyr, ggplot2, tidyr, moments.  

---

##  Méthodologie

1. Importation et nettoyage des données  
2. Analyses descriptives  
3. Comparaisons statistiques entre S1 et S2  
4. Analyse des performances par groupe  
5. Étude des facteurs explicatifs  
6. Visualisation des résultats  

---

##  Description des scripts

### `01_import_nettoyage.R`

- Importation du fichier CSV  
- Gestion des types (facteurs : genre, filière)  
- Détection des doublons  
- Imputation des valeurs manquantes (médiane ou mode)  
- Calcul des moyennes générales S1 et S2  

### `02_analyses_descriptives.R`

- Statistiques démographiques  
- Calcul du taux de réussite global (seuil : 10/20)  
- Identification des matières les plus difficiles  

### `03_analyses_comparatives.R`

- Comparaison des notes entre S1 et S2  
- Tests *t* appariés pour évaluer les différences significatives  

### `04_analyses_performances.R`

- Analyse des performances par filière et par genre  
- Tests ANOVA pour comparer les moyennes entre groupes  

### `05_analyses_facteurs.R`

- Analyse de l’impact des facteurs comportementaux  
- Corrélations entre heures d’étude, absences et moyenne finale  
- Construction de classes d’heures d’étude (méthode de Sturges)  

### `06_visualisations.R`

- Production de graphiques haute résolution (300 DPI)  
- Histogrammes, boxplots et nuages de points  

---


##  Instructions d’exécution

1. Ouvrir le projet dans **RStudio**  
2. Vérifier que le répertoire de travail est correctement défini  
3. Exécuter le script principal :

```r
source("main.R")
```

L’ensemble des résultats sera généré automatiquement dans le dossier `output/`.

---

##  Résultats attendus

- Tableaux statistiques exportés au format CSV  
- Graphiques explicatifs en format PNG (300 DPI)  
- Indicateurs clés de performance académique  
- Éléments d’aide à la décision pédagogique  

---

## Auteur

**Frantzdy DORILUS**  

