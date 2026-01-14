#===================================================================================================
#                       DÉBUT DE L'ANALYSE 1 : STATISTIQUES DESCRIPTIVES 
#===================================================================================================


#====================== 3.1.1. Démographie (Âge, Genre, Filière, Ville) =============================

#====================== 1. Analyse de l'Âge (a) =====================================================

cat("### 1.1. Distribution de l'Âge (Moyenne, Médiane, Écart-type)\n")

age_stats <- data_projet %>%
  summarise(
    N = n(),
    Moyenne = mean(age),
    Mediane = median(age),
    Ecart_Type = sd(age),
    Min = min(age),
    Max = max(age)
  )

print(round(age_stats, 2))
write_csv(age_stats, "output/tables/age_descriptif.csv")


#====================== 2. Répartition par Genre (b) ===============================================

cat("\n### 1.2. Répartition par Genre\n")

genre_stats <- data_projet %>%
  group_by(genre) %>%
  summarise(
    Effectif = n()
  ) %>%
  ungroup() %>%
  mutate(
    Pourcentage = round(Effectif / sum(Effectif) * 100, 2) 
  )

print(genre_stats) # Correction : 
write_csv(genre_stats, "output/tables/genre_repartition.csv")


#===================== 3. Répartition par Filière (c) =============================================

cat("\n### 1.3. Répartition par Filière\n")

filiere_stats <- data_projet %>%
  group_by(filiere) %>%
  summarise(
    Effectif = n()
  ) %>%
  ungroup() %>%
  mutate(
    Pourcentage = round(Effectif / sum(Effectif) * 100, 2) 
  )

print(filiere_stats) 
write_csv(filiere_stats, "output/tables/filiere_repartition.csv")


#==================== 4. Distribution Géographique (Villes principales) (d) ========================

cat("\n### 1.4. Distribution Géographique (Villes principales)\n")

ville_stats <- data_projet %>%
  group_by(ville_origine) %>%
  summarise(
    Effectif = n()
  ) %>%
  ungroup() %>%
  mutate(
    Pourcentage = round(Effectif / sum(Effectif) * 100, 2) 
  ) %>%
  arrange(desc(Effectif)) %>% 
  slice(1:5) 

print(ville_stats) 
write_csv(ville_stats, "output/tables/ville_repartition_top5.csv")


cat("\n Sous-section 3.1.1 Démographie terminée. 

    Résultats exportés vers output/tables/.\n")

#========================================================================================================


#====================== 3.1.2. Comportement Académique (Heures d'étude et Absences) =====================


#====================== 1. Statistiques sur les Heures d'Étude par Semaine (a) ==========================

cat("### 2.1. Statistiques Descriptives des Heures d'Étude\n")

heures_etude_stats <- data_projet %>%
  summarise(
    N = n(),
    Moyenne = mean(heures_etude_semaine),
    Mediane = median(heures_etude_semaine),
    Ecart_Type = sd(heures_etude_semaine),
    Min = min(heures_etude_semaine),
    Max = max(heures_etude_semaine)
  )

print(round(heures_etude_stats, 2))
write_csv(heures_etude_stats, "output/tables/heures_etude_descriptif.csv")


#====================== 2. Statistiques sur le Nombre d'Absences (S1 et S2) (b) ========================

cat("\n### 2.2. Statistiques Descriptives des Absences (S1 et S2)\n")

absences_stats <- data_projet %>%
  summarise(
    # Semestre 1
    
    Moyenne_Abs_S1 = mean(nb_absences_s1),
    Mediane_Abs_S1 = median(nb_absences_s1),
    Ecart_Type_Abs_S1 = sd(nb_absences_s1),
    Min_Abs_S1 = min(nb_absences_s1),
    Max_Abs_S1 = max(nb_absences_s1),
    
    # Semestre 2
    Moyenne_Abs_S2 = mean(nb_absences_s2),
    Mediane_Abs_S2 = median(nb_absences_s2),
    Ecart_Type_Abs_S2 = sd(nb_absences_s2),
    Min_Abs_S2 = min(nb_absences_s2),
    Max_Abs_S2 = max(nb_absences_s2)
  ) %>%
  
  # Transformation en format long pour une meilleure présentation
  
  pivot_longer(everything(), names_to = "Statistique", values_to = "Valeur")

# Correction: Arrondir la colonne 'Valeur' avant l'affichage et l'exportation

absences_stats <- absences_stats %>%
  mutate(Valeur = round(Valeur, 2))

print(absences_stats) # On imprime sans essayer d'arrondir la colonne 'Statistique'
write_csv(absences_stats, "output/tables/absences_descriptif.csv")


#====================== 3. Relation Heures d'Étude / Absences (Corrélation de Pearson) (c) ==============

cat("\n### 2.3. Relation Heures d'Étude / Absences (Corrélation de Pearson)\n")

# Corrélation entre heures d'étude et absences S1

cor_h_abs_s1 <- cor(data_projet$heures_etude_semaine, data_projet$nb_absences_s1)

# Corrélation entre heures d'étude et absences S2

cor_h_abs_s2 <- cor(data_projet$heures_etude_semaine, data_projet$nb_absences_s2)

#=============== Fonction d'interprétation de la corrélation ===========================
interpreter_correlation <- function(r) {
  abs_r <- abs(r)
  direction <- if (r > 0) "positive" else if (r < 0) "négative" else "nulle"
  
  if (abs_r >= 0.7) {
    force <- "Forte"
  } else if (abs_r >= 0.5) {
    force <- "Modérée"
  } else if (abs_r >= 0.3) {
    force <- "Faible"
  } else {
    force <- "Très Faible/Nulle"
  }
  
  return(paste0(force, " (", direction, ")"))
}


correlations <- tibble(
  Relation = c("Heures d'étude vs. Absences S1", "Heures d'étude vs. Absences S2"),
  Coefficient_Pearson = round(c(cor_h_abs_s1, cor_h_abs_s2), 3)
) %>%
  
  # Ajout de la colonne Nature_Relation
  
  rowwise() %>%
  mutate(
    Nature_Relation = interpreter_correlation(Coefficient_Pearson)
  ) %>%
  ungroup()

print(correlations)
write_csv(correlations, "output/tables/correlations_comportement.csv")

cat("\n Sous-section 3.1.2 Comportement Académique terminée. 
    Résultats exportés vers output/tables/.\n")
#==========================================================================================================



#=============== 3.1.3. Performance Académique (Moyennes, Taux de Succès, Matières Difficiles) ===========

# Définition des colonnes de notes

notes_cols <- c("note_math_s1", "note_math_s2", "note_info_s1", "note_info_s2", 
                "note_physique_s1", "note_physique_s2", "note_economie_s1", 
                "note_economie_s2", "note_anglais_s1", "note_anglais_s2")
moyennes_cols <- c("moyenne_s1", "moyenne_s2")

#================== 1. Statistiques Complètes pour Chaque Matière (a) ====================================
cat("### 3.1. Statistiques Complètes par Matière (S1 et S2)\n")

# Calcul des statistiques 
stats_par_matiere <- data_projet %>%
  select(all_of(notes_cols)) %>%
  summarise(across(everything(), list(
    Moyenne = ~ mean(.),
    Mediane = ~ median(.),
    EcartType = ~ sd(.), 
    Min = ~ min(.),
    Max = ~ max(.)
  ))) %>%
  pivot_longer(everything(), names_to = "Statistique", values_to = "Valeur") %>%
  # Séparer en 4 parties : (Prefix: note), Matiere, Semestre, Stat
  separate(Statistique, 
           into = c(NA, "Matiere", "Semestre", "Stat"), 
           sep = "_", 
           remove = TRUE) %>% 
  pivot_wider(names_from = Stat, values_from = Valeur) %>%
  # Correction ici : Le nom Ecart_Type est remplacé par EcartType
  mutate(across(c(Moyenne, Mediane, EcartType, Min, Max), ~round(., 2))) 

print(stats_par_matiere)
write_csv(stats_par_matiere, "output/tables/stats_completes_par_matiere.csv")


#======================= 2. Distribution des Moyennes Générales (S1 et S2) (b)=================

cat("\n### 3.2. Distribution des Moyennes Générales (S1 et S2)\n")
moyenne_generale_stats <- data_projet %>%
  select(all_of(moyennes_cols)) %>%
  summarise(across(everything(), list(
    Moyenne = ~ mean(.),
    Mediane = ~ median(.),
    EcartType = ~ sd(.), 
    Min = ~ min(.),
    Max = ~ max(.)
  ))) %>%
  pivot_longer(everything(), names_to = "Statistique", values_to = "Valeur") %>%
  # Séparer en 3 parties : (Prefix: moyenne), Semestre, Stat
  separate(Statistique, 
           into = c(NA, "Semestre", "Stat"), 
           sep = "_",
           remove = TRUE) %>% 
  pivot_wider(names_from = Stat, values_from = Valeur) %>%
  # Correction ici : Le nom Ecart_Type est remplacé par EcartType
  mutate(across(c(Moyenne, Mediane, EcartType, Min, Max), ~round(., 2))) 

print(moyenne_generale_stats)
write_csv(moyenne_generale_stats, "output/tables/distribution_moyennes_generales.csv")


#===================== 3. Taux de Réussite Global (Moyenne >= 10/20) (c) ====================

cat("\n### 3.3. Taux de Réussite Global\n")

taux_reussite_global <- data_projet %>%
  summarise(
    Reussite_S1 = sum(moyenne_s1 >= 10) / n() * 100,
    Reussite_S2 = sum(moyenne_s2 >= 10) / n() * 100
  ) %>%
  pivot_longer(everything(), names_to = "Semestre", values_to = "Taux_Reussite_Pourcentage") %>%
  mutate(Taux_Reussite_Pourcentage = round(Taux_Reussite_Pourcentage, 2))

print(taux_reussite_global)
write_csv(taux_reussite_global, "output/tables/taux_reussite_global.csv")


#================== 4. Identification des Matières les Plus Difficiles ===========================

cat("\n### 3.4. Identification des Matières les Plus Difficiles (Note Moyenne < 10)\n")

matieres_difficiles <- stats_par_matiere %>% # Utilise la table corrigée stats_par_matiere
  filter(Moyenne < 10) %>% 
  arrange(Moyenne) %>%
  rename(Note_Moyenne = Moyenne) %>%
  select(Matiere, Semestre, Note_Moyenne, Min, Max)

if (nrow(matieres_difficiles) > 0) {
  cat(" Matières avec une Note Moyenne < 10 (les plus difficiles) :\n")
  print(matieres_difficiles)
} else {
  cat(" Aucune matière n'a une note moyenne inférieure à 10. (C'est un bon résultat !)\n")
}

write_csv(matieres_difficiles, "output/tables/matieres_difficiles.csv")

cat("\n Sous-section 3.1.3 Performance Académique terminée.\n")
cat("\n--- FIN DE L'ANALYSE 1 : STATISTIQUES DESCRIPTIVES (Sections 3.1.1 à 3.1.3) ---\n")
























