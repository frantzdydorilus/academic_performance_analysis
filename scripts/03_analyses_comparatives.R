#=====================================================================================================
#                                 ANALYSE 2. EVOLUTION S1 → S2
#=====================================================================================================

#================================ EVOLUTION GLOBALE ==================================================

# Création de la variable de variation (Delta)
data_analyse_evolution <- data_projet %>%
  mutate(
    Difference_S2_S1 = moyenne_s2 - moyenne_s1
  )

#==================== 1. Comparaison des moyennes S1 vs S2 (a) : Test T Apparié =====================

cat("### 2.1. Comparaison Statistique S1 vs S2 (Test T Apparié)\n")

# Hypothèses :
# H0 : Moyenne S2 = Moyenne S1 (pas d'évolution)
# H1 : Moyenne S2 != Moyenne S1 (évolution significative)

test_t_resultat <- t.test(
  data_analyse_evolution$moyenne_s2, 
  data_analyse_evolution$moyenne_s1, 
  paired = TRUE
)

test_t_df <- tibble(
  Moyenne_Diff = round(test_t_resultat$estimate, 3),
  Statistique_T = round(test_t_resultat$statistic, 3),
  P_Valeur = round(test_t_resultat$p.value, 4),
  Conclusion = if (test_t_resultat$p.value < 0.05) "Différence significative (Évolution)" else "Pas de différence significative (Stabilité)"
)

print(test_t_df)
write_csv(test_t_df, "output/tables/test_t_s1_vs_s2.csv")

# Interprétation automatique

if (test_t_resultat$p.value < 0.05) {
  cat("→ Interprétation : L'évolution globale des moyennes S1 → S2 est significative.\n")
} else {
  cat("→ Interprétation : Pas d'évolution significative des moyennes S1 → S2.\n")
}

#================ 2. Statistiques descriptives de la variation (b) =================================

cat("\n### 2.2. Statistiques Descriptives de la Variation (Delta S2 - S1)\n")

variation_stats <- data_analyse_evolution %>%
  summarise(
    N = n(),
    Moyenne_Variation = mean(Difference_S2_S1),
    Mediane_Variation = median(Difference_S2_S1),
    Ecart_Type = sd(Difference_S2_S1),
    Min = min(Difference_S2_S1), # Pire régression
    Max = max(Difference_S2_S1)  # Meilleure progression
  ) %>%
  mutate(across(where(is.numeric), ~round(., 3)))

print(variation_stats)
write_csv(variation_stats, "output/tables/variation_stats_descriptives.csv")

#================ 3. Identification des progressions et régressions (c, d, e) =====================

cat("\n### 2.3. Identification des Top Progressions et Régressions\n")

top_progression <- data_analyse_evolution %>%
  filter(Difference_S2_S1 > 0) %>%
  arrange(desc(Difference_S2_S1)) %>%
  slice(1:10) %>%
  select(id_etudiant, nom, filiere, Difference_S2_S1, moyenne_s1, moyenne_s2) %>%
  mutate(Difference_S2_S1 = round(Difference_S2_S1, 2))

cat("\n--- Top 10 des Progressions ---\n")
print(top_progression)
write_csv(top_progression, "output/tables/top_10_progressions.csv")

top_regression <- data_analyse_evolution %>%
  filter(Difference_S2_S1 < 0) %>%
  arrange(Difference_S2_S1) %>% 
  slice(1:10) %>%
  select(id_etudiant, nom, filiere, Difference_S2_S1, moyenne_s1, moyenne_s2) %>%
  mutate(Difference_S2_S1 = round(Difference_S2_S1, 2))

cat("\n--- Top 10 des Régressions ---\n")
print(top_regression)
write_csv(top_regression, "output/tables/top_10_regressions.csv")

cat("\nSous-section 2.1 Évolution Globale terminée.\n")

#================================================================================================

#============ 2.2. ÉVOLUTION PAR MATIÈRE (S1 vs S2) =============================

notes_s1_cols <- c("note_math_s1", "note_info_s1", "note_physique_s1", 
                   "note_economie_s1", "note_anglais_s1")

notes_s2_cols <- c("note_math_s2", "note_info_s2", "note_physique_s2", 
                   "note_economie_s2", "note_anglais_s2")

matieres <- c("Mathématiques", "Informatique", "Physique", "Économie", "Anglais")

resultats_evolution_matiere <- tibble()

for (i in 1:length(matieres)) {
  matiere_nom <- matieres[i]
  col_s1 <- notes_s1_cols[i]
  col_s2 <- notes_s2_cols[i]
  
  # Test T apparié
  # H0 : Moyenne S2 = Moyenne S1 pour cette matière
  # H1 : Moyenne S2 != Moyenne S1 (évolution significative)
  test_t_matiere <- t.test(data_projet[[col_s2]], data_projet[[col_s1]], paired = TRUE)
  
  moyenne_s1 <- mean(data_projet[[col_s1]])
  moyenne_s2 <- mean(data_projet[[col_s2]])
  difference <- moyenne_s2 - moyenne_s1
  
  # Interprétation automatique
  if (test_t_matiere$p.value < 0.05) {
    if (difference > 0) {
      statut <- "Progression Significative"
    } else {
      statut <- "Régression Significative"
    }
  } else {
    statut <- "Stabilité (Non Significatif)"
  }
  
  resultats_evolution_matiere <- resultats_evolution_matiere %>%
    bind_rows(tibble(
      Matiere = matiere_nom,
      Moyenne_S1 = round(moyenne_s1, 2),
      Moyenne_S2 = round(moyenne_s2, 2),
      Difference = round(difference, 2),
      P_Valeur_TestT = round(test_t_matiere$p.value, 4),
      Statut = statut
    ))
}

cat("\n### 2.3. Résultats de l'Évolution par Matière\n")
print(resultats_evolution_matiere)
write_csv(resultats_evolution_matiere, "output/tables/evolution_par_matiere.csv")

#===================== Synthèse : Matières en progression / régression =========================

cat("\n### 2.4. Synthèse : Matières en Progression / Régression\n")

progression <- resultats_evolution_matiere %>%
  filter(Difference > 0) %>%
  arrange(desc(Difference))

cat("\n--- Matières en Progression (Différence > 0) ---\n")
if (nrow(progression) > 0) {
  print(progression %>% select(Matiere, Difference, Statut, P_Valeur_TestT))
} else {
  cat("Aucune matière n'a montré de progression.\n")
}

regression <- resultats_evolution_matiere %>%
  filter(Difference < 0) %>%
  arrange(Difference)

cat("\n--- Matières en Régression (Différence < 0) ---\n")
if (nrow(regression) > 0) {
  print(regression %>% select(Matiere, Difference, Statut, P_Valeur_TestT))
} else {
  cat("Aucune matière n'a montré de régression.\n")
}

cat("\nSous-section 2.2 Évolution par Matière terminée.\n")
cat("\n--- FIN DE L'ANALYSE 2 : EVOLUTION S1 → S2 ---\n")

