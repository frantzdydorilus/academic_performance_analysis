# ===========================================================================
#                ANALYSE 3 : PERFORMANCE PAR GROUPES 
# ===========================================================================

cat("\n\n--- ANALYSE 3 : PERFORMANCE PAR GROUPES ---\n")

# Fonction pour calculer le Mode
get_mode <- function(v) {
  uniqv <- unique(v)
  # Utiliser which.max sur les fréquences
  uniqv[which.max(tabulate(match(v, uniqv)))] 
}

# --- 3.1 Par Filière (a, b, c, d) : Analyses S1 et S2 ---
cat("\n### 3.3.1 Performance par Filière (S1 et S2)\n")

# Définition des variables de semestre à analyser
semestres <- c("S1", "S2")

for (semestre in semestres) {
  
  col_moyenne <- paste0("moyenne_", tolower(semestre))
  
  cat(paste0("\n--- Analyse pour le Semestre ", semestre, " ---\n"))
  
  # --- 1. Statistiques Descriptives (a, c) ---
  stats_filiere <- data_projet %>%
    group_by(filiere) %>%
    summarise(
      N = n(),
      Moyenne = mean(get(col_moyenne)), 
      Mediane = median(get(col_moyenne)),
      Mode = get_mode(get(col_moyenne)),
      Ecart_Type = sd(get(col_moyenne)),
      Min = min(get(col_moyenne)),
      Max = max(get(col_moyenne))
    ) %>%
    mutate(across(where(is.numeric), ~round(., 2))) %>%
    arrange(desc(Moyenne))
  
  cat(paste0("1.1. Statistiques Descriptives (a) : ", semestre, "\n"))
  print(stats_filiere)
  write_csv(stats_filiere, paste0("output/tables/analyse3_stats_filiere_", tolower(semestre), ".csv"))
  
  filiere_plus_performante <- stats_filiere %>% slice(1)
  cat(paste("1.2. Conclusion (c): Filière la plus performante au", semestre, "est", 
            filiere_plus_performante$filiere, 
            "avec une moyenne de", filiere_plus_performante$Moyenne, "/20.\n"))
  
  # --- 2. Comparaison des Moyennes entre Filières (ANOVA) (b) ---
  
  # ---------------------------------------------------------
  # TEST ANOVA : Comparaison des moyennes par filière
  # H0 : Les moyennes sont égales entre toutes les filières
  # H1 : Au moins une filière a une moyenne différente
  # Statistique : F = variance inter-groupes / variance intra-groupes
  # Décision : si p-value < 0.05 → Rejet de H0 → Différence significative
  # ---------------------------------------------------------
  
  formule_anova <- as.formula(paste(col_moyenne, "~ filiere"))
  modele_anova_filiere <- aov(formule_anova, data = data_projet)
  summary_anova_filiere <- summary(modele_anova_filiere)
  p_value_anova_filiere <- summary_anova_filiere[[1]]$`Pr(>F)`[1]
  
  anova_df <- tibble(
    Semestre = semestre,
    Test = "ANOVA (Moyenne vs Filière)",
    P_Valeur = round(p_value_anova_filiere, 4),
    Conclusion = if (p_value_anova_filiere < 0.05) {
      "Différence significative de performance selon la filière (Rejet de H0)."
    } else {
      "Pas de différence significative de performance selon la filière (Acceptation de H0)."
    }
  )
  
  cat("\n1.3. Test de Comparaison : ANOVA (b)\n")
  print(anova_df)
  
  # Interprétation automatique
  if(p_value_anova_filiere < 0.05){
    cat("→ Interprétation : Les performances diffèrent significativement selon la filière.\n")
  } else {
    cat("→ Interprétation : Aucune différence significative entre les filières.\n")
  }
  
  write_csv(anova_df, paste0("output/tables/analyse3_anova_filiere_", tolower(semestre), ".csv"))
  
  # --- 3. Taux de Réussite par Filière (d) ---
  taux_reussite_filiere <- data_projet %>%
    group_by(filiere) %>%
    summarise(
      Total_Etudiants = n(),
      Nb_Reussite = sum(get(col_moyenne) >= 10), 
      Taux_Reussite_Pct = (Nb_Reussite / Total_Etudiants) * 100
    ) %>%
    mutate(Taux_Reussite_Pct = round(Taux_Reussite_Pct, 2)) %>%
    arrange(desc(Taux_Reussite_Pct))
  
  cat("\n1.4. Taux de Réussite (d)\n")
  print(taux_reussite_filiere)
  write_csv(taux_reussite_filiere, paste0("output/tables/analyse3_taux_reussite_filiere_", tolower(semestre), ".csv"))
}

# ===============================================================================================================
# ANALYSE 3 : PERFORMANCE PAR GENRE
# ===============================================================================================================

get_mean_by_genre_final <- function(data, col, g) {
  data %>% 
    filter(genre == g) %>% 
    summarise(Moyenne = mean(get(col))) %>% 
    pull(Moyenne)
}

GENRE_1 <- "Femme"
GENRE_2 <- "Homme" 

cat("\n\n### 3.3.2 Analyse de la Performance par Genre (", GENRE_1, "/", GENRE_2, ")\n")

# --- a) Comparaison des performances H/F (S1 et S2) ---
cat("\n--- 3.2.1 Comparaison des Moyennes Générales par Genre (Test T indépendant) ---\n")

resultats_genre_global <- tibble()
semestres_global <- c("S1", "S2")

for (semestre in semestres_global) {
  col_moyenne <- paste0("moyenne_", tolower(semestre))
  
  # ---------------------------------------------------------
  # TEST T STUDENT (indépendant)
  # H0 : Moyenne_Femme = Moyenne_Homme
  # H1 : Moyenne_Femme ≠ Moyenne_Homme
  # Statistique : t = (m1 - m2) / erreur standard
  # Décision : si p-value < 0.05 → Rejet de H0
  # ---------------------------------------------------------
  
  test_t_genre <- t.test(data_projet[[col_moyenne]] ~ data_projet$genre, var.equal = TRUE)
  
  Moyenne_F_val <- get_mean_by_genre_final(data_projet, col_moyenne, GENRE_1)
  Moyenne_H_val <- get_mean_by_genre_final(data_projet, col_moyenne, GENRE_2)
  
  df_resultat <- tibble(
    Semestre = semestre,
    !!paste0("Moyenne_", GENRE_1) := round(Moyenne_F_val, 2),
    !!paste0("Moyenne_", GENRE_2) := round(Moyenne_H_val, 2),
    P_Valeur_TestT = round(test_t_genre$p.value, 4),
    Significatif = test_t_genre$p.value < 0.05
  )
  resultats_genre_global <- bind_rows(resultats_genre_global, df_resultat)
  
  # Interprétation automatique
  if(test_t_genre$p.value < 0.05){
    cat(paste0("→ Interprétation : Différence significative entre ", GENRE_1, " et ", GENRE_2, " pour ", semestre, "\n"))
  } else {
    cat(paste0("→ Interprétation : Aucune différence significative entre ", GENRE_1, " et ", GENRE_2, " pour ", semestre, "\n"))
  }
}

print(resultats_genre_global)
write_csv(resultats_genre_global, "output/tables/analyse3_performance_genre_global.csv")

# --- b) Différences par matière (Test T indépendant pour chaque matière) ---
cat("\n--- 3.2.2 Différences de Performance par Matière (S2) entre Genres ---\n")

notes_s2_cols <- c("note_math_s2", "note_info_s2", "note_physique_s2", "note_economie_s2", "note_anglais_s2")
matieres <- c("Mathématiques", "Informatique", "Physique", "Économie", "Anglais")

resultats_genre_matiere <- tibble()

for (i in 1:length(matieres)) {
  matiere_col <- notes_s2_cols[i]
  
  # TEST T STUDENT (indépendant) par matière
  test_t_matiere_genre <- t.test(data_projet[[matiere_col]] ~ data_projet$genre, var.equal = TRUE)
  
  Moyenne_F_mat <- get_mean_by_genre_final(data_projet, matiere_col, GENRE_1)
  Moyenne_H_mat <- get_mean_by_genre_final(data_projet, matiere_col, GENRE_2)
  
  df_resultat_matiere <- tibble(
    Matiere = matieres[i],
    !!paste0("Moyenne_", GENRE_1) := round(Moyenne_F_mat, 2),
    !!paste0("Moyenne_", GENRE_2) := round(Moyenne_H_mat, 2),
    P_Valeur_TestT = round(test_t_matiere_genre$p.value, 4),
    Significatif = test_t_matiere_genre$p.value < 0.05
  )
  resultats_genre_matiere <- bind_rows(resultats_genre_matiere, df_resultat_matiere)
  
  # Interprétation automatique
  if(test_t_matiere_genre$p.value < 0.05){
    cat(paste0("→ Interprétation : Différence significative entre genres pour ", matieres[i], "\n"))
  } else {
    cat(paste0("→ Interprétation : Aucune différence significative pour ", matieres[i], "\n"))
  }
}

print(resultats_genre_matiere)
write_csv(resultats_genre_matiere, "output/tables/analyse3_performance_genre_matiere.csv")

# --- c) Évolution S1 → S2 par genre (Test T apparié) ---
cat("\n--- 3.2.3 Évolution S1 → S2 par Genre (Test T apparié) ---\n")

resultats_evolution_genre <- tibble()
groupes_genre_evolution <- c(GENRE_1, GENRE_2) 

for (g in groupes_genre_evolution) {
  data_genre <- data_projet %>% filter(genre == g)
  
  # TEST T STUDENT apparié
  test_t_evolution <- t.test(data_genre$moyenne_s2, data_genre$moyenne_s1, paired = TRUE)
  
  difference_moyenne <- mean(data_genre$moyenne_s2) - mean(data_genre$moyenne_s1)
  
  df_evolution <- tibble(
    Genre = g,
    Difference_S2_S1 = round(difference_moyenne, 2),
    P_Valeur_TestT = round(test_t_evolution$p.value, 4),
    Statut = if (test_t_evolution$p.value < 0.05) {
      if (difference_moyenne > 0) "Progression Significative" else "Régression Significative"
    } else "Stabilité (Non Significatif)"
  )
  resultats_evolution_genre <- bind_rows(resultats_evolution_genre, df_evolution)
  
  # Interprétation automatique
  if(test_t_evolution$p.value < 0.05){
    if(difference_moyenne > 0){
      cat(paste0("→ Interprétation : ", g, " a progressé significativement entre S1 et S2.\n"))
    } else {
      cat(paste0("→ Interprétation : ", g, " a régressé significativement entre S1 et S2.\n"))
    }
  } else {
    cat(paste0("→ Interprétation : ", g, " reste stable (non significatif).\n"))
  }
}

print(resultats_evolution_genre)
write_csv(resultats_evolution_genre, "output/tables/analyse3_evolution_par_genre.csv")

# ===========================================================================
# PERFORMANCE PAR CATÉGORIE D'ÂGE
# ===========================================================================

cat("\n\n### 3.3.3 Analyse de la Performance par Catégorie d'Âge\n")

data_projet <- data_projet %>%
  mutate(
    categorie_age = cut(age, 
                        breaks = c(18, 20.99, 23.99, 26), 
                        labels = c("18-20 ans", "21-23 ans", "24-26 ans"),
                        right = FALSE, 
                        include.lowest = TRUE)
  ) %>%
  filter(!is.na(categorie_age))

cat("Catégories d'âge créées (18-20, 21-23, 24-26).\n")

# --- Statistiques Descriptives et ANOVA sur les Moyennes Générales ---
cat("\n--- Statistiques Descriptives et ANOVA par Groupe d'Âge (S1 et S2) ---\n")

resultats_anova_global <- tibble()
semestres_global <- c("S1", "S2")

for (semestre in semestres_global) {
  col_moyenne <- paste0("moyenne_", tolower(semestre))
  
  stats_age <- data_projet %>%
    group_by(categorie_age) %>%
    summarise(
      N = n(),
      Moyenne = mean(get(col_moyenne)),
      Mediane = median(get(col_moyenne)),
      Ecart_Type = sd(get(col_moyenne))
    ) %>%
    mutate(across(where(is.numeric), ~round(., 2)))
  
  cat(paste0("\n--- Statistiques Descriptives Moyenne ", semestre, " par Groupe d'Âge ---\n"))
  print(stats_age)
  write_csv(stats_age, paste0("output/tables/analyse3_stats_age_", tolower(semestre), ".csv"))
  
  # ---------------------------------------------------------
  # TEST ANOVA par catégorie d'âge
  # H0 : Les moyennes sont égales entre les catégories d'âge
  # H1 : Au moins une catégorie diffère
  # Statistique : F = variance inter-groupes / variance intra-groupes
  # Décision : p-value < 0.05 → Rejet de H0
  # ---------------------------------------------------------
  
  formule_anova <- as.formula(paste(col_moyenne, "~ categorie_age"))
  modele_anova_age <- aov(formule_anova, data = data_projet)
  summary_anova_age <- summary(modele_anova_age)
  p_value_anova_age <- summary_anova_age[[1]]$`Pr(>F)`[1]
  
  df_anova <- tibble(
    Semestre = semestre,
    Test = "ANOVA (Moyenne générale vs Catégorie d'Âge)",
    P_Valeur_ANOVA = round(p_value_anova_age, 4),
    Significatif = p_value_anova_age < 0.05
  )
  resultats_anova_global <- bind_rows(resultats_anova_global, df_anova)
  
  # Interprétation automatique
  if(p_value_anova_age < 0.05){
    cat(paste0("→ Interprétation : Différence significative de performance entre catégories d'âge pour ", semestre, "\n"))
  } else {
    cat(paste0("→ Interprétation : Aucune différence significative entre catégories d'âge pour ", semestre, "\n"))
  }
}

cat("\n--- Résumé ANOVA Moyennes Globales par Catégorie d'Âge ---\n")
print(resultats_anova_global)
write_csv(resultats_anova_global, "output/tables/analyse3_anova_age_global.csv")

# --- Comparaison des performances par Matière (S2) ---
cat("\n--- ANOVA et Moyennes par Matière (S2) entre Catégories d'Âge ---\n")

notes_s2_cols <- c("note_math_s2", "note_info_s2", "note_physique_s2", "note_economie_s2", "note_anglais_s2")
matieres <- c("Mathématiques", "Informatique", "Physique", "Économie", "Anglais")

resultats_age_matiere <- tibble()

for (i in 1:length(matieres)) {
  matiere_col <- notes_s2_cols[i]
  
  formule_anova_matiere <- as.formula(paste(matiere_col, "~ categorie_age"))
  modele_anova_matiere <- aov(formule_anova_matiere, data = data_projet)
  summary_anova_matiere <- summary(modele_anova_matiere)
  p_value_anova_matiere <- summary_anova_matiere[[1]]$`Pr(>F)`[1]
  
  moyennes_matiere <- data_projet %>%
    group_by(categorie_age) %>%
    summarise(Moyenne = mean(get(matiere_col))) %>%
    ungroup() %>%
    mutate(Moyenne = round(Moyenne, 2))
  
  df_resultat_matiere <- tibble(
    Matiere = matieres[i],
    P_Valeur_ANOVA = round(p_value_anova_matiere, 4),
    Significatif = p_value_anova_matiere < 0.05
  )
  
  moyennes_transposees <- moyennes_matiere %>% 
    pivot_wider(names_from = categorie_age, values_from = Moyenne, names_prefix = "Moyenne_")
  
  df_resultat_matiere <- bind_cols(df_resultat_matiere, moyennes_transposees)
  # Ajouter la ligne de résultats pour cette matière
  resultats_age_matiere <- bind_rows(resultats_age_matiere, df_resultat_matiere)
  
  # Interprétation automatique
  if(p_value_anova_matiere < 0.05){
    cat(paste0("→ Interprétation : Différence significative entre catégories d'âge pour ", matieres[i], "\n"))
  } else {
    cat(paste0("→ Interprétation : Aucune différence significative pour ", matieres[i], "\n"))
  }
}

# Affichage et export final des résultats par matière
print(resultats_age_matiere)
write_csv(resultats_age_matiere, "output/tables/analyse3_performance_age_matiere.csv")

cat("\n--- Analyse 3 : Performance par Groupes est maintenant terminée ---\n")
cat("→ Tous les résultats (statistiques, ANOVA, taux de réussite, comparaisons par genre et par âge) ont été sauvegardés dans le dossier output/tables.\n")

                                     