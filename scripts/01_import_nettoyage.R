#======================================================================================================
#                             IMPORT ET NETTOYAGE DE DONNEES
#======================================================================================================

# Packages nécessaires
library(tidyverse)
library(moments) 

#======================================================================================================
#                          1. IMPORTATION DES DONNÉES BRUTES
#======================================================================================================
chemin_raw <- "data/raw/etudiants_performance.csv"
data_raw <- read_csv(chemin_raw) 

#======================================================================================================
#                          2. NETTOYAGE ET HARMONISATION DES TYPES 
#======================================================================================================
data_clean <- data_raw %>%
  mutate(genre = as.factor(genre),
         filiere = as.factor(filiere))

cat("\n--- 2. VÉRIFICATION DE LA STRUCTURE DES DONNÉES ---\n")
print(str(data_clean))
#=======================================================================================================
#                          3. SUPPRESSION DES DOUBLONS 
#=======================================================================================================

n_avant_doublons <- nrow(data_clean)
data_clean <- data_clean %>% distinct()
doublons_supprimes <- n_avant_doublons - nrow(data_clean)
cat(paste("\n--- 3. GESTION DES DOUBLONS ---\n"))
cat(paste("Nombre de doublons trouvés et supprimés:", doublons_supprimes, "\n"))

#=======================================================================================================
#       4. GESTION DES VALEURS MANQUANTES (NA) : DÉTECTION, COEFFICIENT D'ASYMETRiE ET IMPUTATION 
#=======================================================================================================

# 4.1. Détection automatique des colonnes numériques avec au moins un NA
numeric_vars <- names(data_clean)[sapply(data_clean, is.numeric)]
vars_with_na <- numeric_vars[sapply(data_clean[numeric_vars], function(x) any(is.na(x)))]

if(length(vars_with_na) > 0) {
  
  cat("\nVariables numériques contenant des NA détectées :\n")
  print(vars_with_na)
  
  
  # UTILISATION DU COEFFICIENT D'ASYMETRIE
  
  # On sait que si la valuer absolue du coefficent d'asymetrique est inferieure
  # 1, la distribution est symetrique.
  # Par consequent la moyenne est representative de la serie(distribution)
  # Sinon elle est asymétrique, la mediane est representative de la distribution
  
  
  cat("\n--- Règle du coefficient d'asymétrie pour l'imputation ---\n")
  cat("Skewness ∈ [-1, 1] : distribution approximativement symétrique → utilisation de la MOYENNE.\n")
  cat("Skewness < -1 ou > 1 : distribution fortement asymétrique → utilisation de la MEDIANE.\n")
  
  # 4.3. Boucle sur chaque variable pour calculer le coefficient d'asymétrie et décider l’imputation
  for(var in vars_with_na) {
    
    # Calcul du coefficient d'asymetrie.
    skew_val <- moments::skewness(data_clean[[var]], na.rm = TRUE)
    
    cat(paste0("\nVariable : ", var, "\n"))
    cat(paste0("Skewness : ", round(skew_val,3), " → "))
    
    # Décision pour l’imputation selon la règle
    if(abs(skew_val) <= 1) {
      cat("Distribution approximativement symétrique → utilisation de la MOYENNE pour imputation.\n")
      impute_val <- mean(data_clean[[var]], na.rm = TRUE)
    } else {
      cat("Distribution fortement asymétrique → utilisation de la MEDIANE pour imputation.\n")
      impute_val <- median(data_clean[[var]], na.rm = TRUE)
    }
    
    # Imputation
    data_clean[[var]] <- ifelse(is.na(data_clean[[var]]), impute_val, data_clean[[var]])
  }
  
  cat("\n--- Imputation des valeurs manquantes terminée ---\n")
  
} else {
  cat("Aucune valeur manquante (NA) trouvée dans le jeu de données.\n")
}
#========================================================================================================
#                      5. CRÉATION DES VARIABLES CALCULÉES (Moyennes S1 et S2)
#========================================================================================================

data_clean <- data_clean %>%
  mutate(
    moyenne_s1 = (note_math_s1 + note_info_s1 + note_physique_s1 + 
                    note_economie_s1 + note_anglais_s1) / 5,
    moyenne_s2 = (note_math_s2 + note_info_s2 + note_physique_s2 + 
                    note_economie_s2 + note_anglais_s2) / 5
  )

#=========================================================================================================
#                      6. EXPORTATION DES DONNÉES NETTOYÉES 
#=========================================================================================================

chemin_export <- "data/processed/donnees_nettoyees.csv"
write_csv(data_clean, chemin_export)

cat(paste("\nNettoyage terminé. Données exportées vers :", chemin_export, "\n"))







