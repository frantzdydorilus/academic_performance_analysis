# ===========================================================================
# ANALYSE 4 : FACTEURS DE RÉUSSITE (15 points)
# ===========================================================================

cat("\n\n--- ANALYSE 4 : FACTEURS DE RÉUSSITE ---\n")

# ========================================================
# 4.1. Impact des Heures d’Étude
# ========================================================

cat("\n### 4.1 Impact des Heures d'Étude\n")

# --- Création des classes d'heures d'étude (Sturges) ---

N <- nrow(data_projet)
k_sturges <- 1 + 3.322 * log10(N)
k_final <- round(k_sturges)

Min_Heures <- min(data_projet$heures_etude_semaine, na.rm = TRUE)
Max_Heures <- max(data_projet$heures_etude_semaine, na.rm = TRUE)
Etendue <- Max_Heures - Min_Heures
A_theorique <- Etendue / k_final

data_projet <- data_projet %>%
  mutate(
    classe_heures_etude = cut(
      heures_etude_semaine,
      breaks = c(5, 11, 17, 23, 29, 35, 41, 46),
      labels = c("5-10h","11-16h","17-22h","23-28h","29-34h","35-40h","41-46h"),
      right = FALSE, include.lowest = TRUE
    )
  )

# ---------------------------------------------------------
# CORRELATION HEURES D'ETUDE / PERFORMANCE (S1 et S2)
# ---------------------------------------------------------

cat("\n### 4.1.1 Corrélation entre Heures d'Étude et Performance\n")

resultats_correlation_heures <- tibble(
  Semestre = c("S1", "S2"),
  Coefficient_Correlation = c(
    cor(data_projet$heures_etude_semaine, data_projet$moyenne_s1, use="complete.obs"),
    cor(data_projet$heures_etude_semaine, data_projet$moyenne_s2, use="complete.obs")
  )
) %>%
  mutate(
    Coefficient_Correlation = round(Coefficient_Correlation, 3),
    Force = case_when(
      abs(Coefficient_Correlation) >= 0.7 ~ "Très Forte",
      abs(Coefficient_Correlation) >= 0.5 ~ "Forte",
      abs(Coefficient_Correlation) >= 0.3 ~ "Modérée",
      abs(Coefficient_Correlation) >= 0.1 ~ "Faible",
      TRUE ~ "Négligeable"
    ),
    Interpretation = paste0(
      "Il existe une ", "correlation ", Force," ",  
      if_else(Coefficient_Correlation>0,"positive","négative"),
      " entre heures d'étude et performance (", Semestre, ")"
    )
  )

print(resultats_correlation_heures)
write_csv(resultats_correlation_heures, "output/tables/analyse4_correlation_heures_etude.csv")


# ---------------------------------------------------------
# ANOVA par Classe d'Heures d'Étude
# ---------------------------------------------------------

cat("\n### 4.1.2 ANOVA Moyenne par Classe d'Heures d'Étude\n")

resultats_anova_heures <- tibble()

for (semestre in c("S1","S2")) {
  col_moyenne <- paste0("moyenne_", tolower(semestre))
  
  # Statistiques descriptives par classe
  stats_heures <- data_projet %>%
    group_by(classe_heures_etude) %>%
    summarise(
      N = n(),
      Moyenne = mean(get(col_moyenne)),
      Ecart_Type = sd(get(col_moyenne))
    ) %>%
    mutate(across(where(is.numeric), ~round(.,2))) %>%
    arrange(desc(Moyenne))
  
  print(stats_heures)
  write_csv(stats_heures, paste0("output/tables/analyse4_stats_heures_", tolower(semestre), ".csv"))
  
  # ---------------------------------------------------------
  # TEST ANOVA
  # H0 : Les moyennes sont égales entre les classes d'heures
  # H1 : Au moins une classe diffère
  # Statistique : F = variance inter-groupes / variance intra-groupes
  # Décision : p-value < 0.05 → Rejet de H0
  # Interprétation : Si rejet H0 → la classe avec la moyenne la plus élevée correspond au seuil optimal
  # ---------------------------------------------------------
  
  formule_anova <- as.formula(paste(col_moyenne, "~ classe_heures_etude"))
  modele_anova_heures <- aov(formule_anova, data = data_projet)
  p_value <- summary(modele_anova_heures)[[1]]$`Pr(>F)`[1]
  
  seuil_optimal <- stats_heures %>% slice(1)
  
  df_anova <- tibble(
    Semestre = semestre,
    Test = "ANOVA (Moyenne vs Classe d'Heures)",
    P_Valeur_ANOVA = round(p_value,4),
    Decision = if_else(p_value < 0.05, "Rejet H0 → Différence significative entre classes", 
                       "Non Rejet H0 → Stabilité entre classes"),
    Seuil_Optimal = paste0(seuil_optimal$classe_heures_etude," (Moyenne=",seuil_optimal$Moyenne,")")
  )
  
  resultats_anova_heures <- bind_rows(resultats_anova_heures, df_anova)
  
  cat("\nSemestre :", semestre, "\n")
  cat("P-Valeur ANOVA :", round(p_value,4), "\n")
  cat("Décision :", df_anova$Decision, "\n")
  cat("Seuil Optimal :", df_anova$Seuil_Optimal, "\n")
}

print(resultats_anova_heures)
write_csv(resultats_anova_heures, "output/tables/analyse4_anova_heures_etude.csv")


# ========================================================
# 4.2 Impact des Absences
# ========================================================

cat("\n\n### 4.2 Impact des Absences\n")

# --- Corrélation Absences / Performance (S1 et S2) ---

resultats_correlation_absences <- tibble(
  Relation = c("Absences S1 vs Perf S1", "Absences S2 vs Perf S2"), 
               
  Coefficient_Correlation = c(
    cor(data_projet$nb_absences_s1, data_projet$moyenne_s1, use="complete.obs"),
    cor(data_projet$nb_absences_s2, data_projet$moyenne_s2, use="complete.obs")
  
  )
) %>%
  mutate(
    Coefficient_Correlation = round(Coefficient_Correlation,3),
    Force = case_when(
      abs(Coefficient_Correlation) >= 0.7 ~ "Très Forte",
      abs(Coefficient_Correlation) >= 0.5 ~ "Forte",
      abs(Coefficient_Correlation) >= 0.3 ~ "Modérée",
      abs(Coefficient_Correlation) >= 0.1 ~ "Faible",
      TRUE ~ "Négligeable"
    ),
    Interpretation = paste0(
      "Il existe une ", "correlation ",Force,
      if_else(Coefficient_Correlation>0," positive", " négative" ,),
      " entre absences et performance (", Relation, ")"
    )
  )

print(resultats_correlation_absences)
write_csv(resultats_correlation_absences, "output/tables/analyse4_correlation_absences.csv")


# --- ANOVA par Classe d'Absences (S2) ---

data_projet <- data_projet %>%
  mutate(
    classe_absences_s2 = cut(nb_absences_s2, breaks=c(-Inf,4.99,8.99,Inf),
                             labels=c("Faible (0-4)","Modérée (5-9)","Forte (10+)"))
  )

resultats_anova_absences <- tibble()

for (semestre in c("S1","S2")) {
  col_moyenne <- paste0("moyenne_", tolower(semestre))
  
  stats_absences <- data_projet %>%
    group_by(classe_absences_s2) %>%
    summarise(N=n(),Moyenne=mean(get(col_moyenne)),Ecart_Type=sd(get(col_moyenne))) %>%
    mutate(across(where(is.numeric),~round(.,2)))
  
  print(stats_absences)
  write_csv(stats_absences,paste0("output/tables/analyse4_stats_absences_",tolower(semestre),".csv"))
  
  formule_anova <- as.formula(paste(col_moyenne,"~ classe_absences_s2"))
  modele_anova <- aov(formule_anova, data=data_projet)
  p_value <- summary(modele_anova)[[1]]$`Pr(>F)`[1]
  
  df_anova <- tibble(
    Semestre = semestre,
    Test = "ANOVA (Moyenne vs Classe d'Absences S2)",
    P_Valeur_ANOVA = round(p_value,4),
    Decision = if_else(p_value<0.05, "Rejet H0 → Différence significative", "Non Rejet H0 → Pas de Difference significative ")
  )
  
  resultats_anova_absences <- bind_rows(resultats_anova_absences, df_anova)
}

print(resultats_anova_absences)
write_csv(resultats_anova_absences,"output/tables/analyse4_anova_absences.csv")


# --- Évolution des absences S1 → S2 (T-test apparié) ---

test_t_absences <- t.test(data_projet$nb_absences_s2, data_projet$nb_absences_s1, paired=TRUE)
diff_absences <- mean(data_projet$nb_absences_s2) - mean(data_projet$nb_absences_s1)

# ---------------------------------------------------------
# TEST T APPARIÉ (Student)
# H0 : La moyenne des absences S1 et S2 est identique
# H1 : La moyenne des absences diffère entre S1 et S2
# Statistique : t = (moyenne_différence) / (écart-type_différence / √n)
# Décision : p-value < 0.05 → Rejet H0
# Interprétation : 
#   - Si diff > 0 → Augmentation significative des absences (régression)
#   - Si diff < 0 → Diminution significative des absences (progression)
# ---------------------------------------------------------

df_evolution_absences <- tibble(
  Difference_Moyenne = round(diff_absences,2),
  P_Valeur_TestT = round(test_t_absences$p.value,4),
  Statut = if (test_t_absences$p.value < 0.05) {
    if (diff_absences > 0) "Augmentation significative des absences (Régression)"
    else "Diminution significative des absences (Progression)"
  } else "Stabilité (Non Significatif)"
)

print(df_evolution_absences)
write_csv(df_evolution_absences, "output/tables/analyse4_evolution_absences_s1_s2.csv")

cat("\n--- ANALYSE 4 TERMINÉE ---\n")

