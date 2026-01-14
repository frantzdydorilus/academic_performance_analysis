#===========================================================================================
#                       SCRIPT 06 : VISUALISATIONS DESCRIPTIVES
#===========================================================================================

#======================   CHARGEMENT DES LIBRARIES =========================================

library(tidyverse)
library(ggplot2)
library(dplyr)
library(tidyr)

#===========================================================================================
#                           3.1.1 — Démographie
#===========================================================================================

#=========================== 1. Histogramme de l'âge =======================================

ggplot(data_projet, aes(x = age)) +
  geom_histogram(bins = 20, fill = "steelblue", color = "white") +
  labs(title = "Distribution de l’âge", x = "Âge", y = "Effectif") +
  theme_minimal()

ggsave("output/figures/hist_age.png", dpi = 300, width = 7, height = 5)

#========================== 2. Barplot Répartition par Genre ===============================

ggplot(genre_stats, aes(x = genre, y = Effectif, fill = genre)) +
  geom_col() +
  labs(title = "Répartition par Genre", x = "Genre", y = "Effectif") +
  theme_minimal() +
  guides(fill = "none")

ggsave("output/figures/bar_genre.png", dpi = 300, width = 7, height = 5)

#========================== 3. Barplot Répartition par Filière ==============================

ggplot(filiere_stats, aes(x = reorder(filiere, -Effectif), y = Effectif, fill = filiere)) +
  geom_col() +
  labs(title = "Répartition par Filière", x = "Filière", y = "Effectif") +
  theme_minimal() +
  guides(fill = "none")

ggsave("output/figures/bar_filiere.png", dpi = 300, width = 8, height = 5)

#========================== 4. Barplot Top 5 des villes ======================================

ggplot(ville_stats, aes(x = reorder(ville_origine, Effectif), y = Effectif)) +
  geom_col(fill = "darkorange") +
  coord_flip() +
  labs(title = "Top 5 des Villes d’Origine", x = "Ville", y = "Effectif") +
  theme_minimal()

ggsave("output/figures/bar_ville_top5.png", dpi = 300, width = 7, height = 5)


#===========================================================================================
#                           3.1.2 — Comportement Académique
#===========================================================================================

#==================   5. Histogramme des heures d’étude ====================================
ggplot(data_projet, aes(x = heures_etude_semaine)) +
  geom_histogram(bins = 20, fill = "seagreen", color = "white") +
  labs(title = "Distribution des Heures d’Étude / Semaine",
       x = "Heures d’étude (semaine)", y = "Effectif") +
  theme_minimal()

ggsave("output/figures/hist_heures_etude.png", dpi = 300, width = 7, height = 5)

#==================== 6. Boxplot Absences S1 vs S2 ==========================================

abs_data <- data_projet %>%
  select(nb_absences_s1, nb_absences_s2) %>%
  pivot_longer(cols = everything(), names_to = "Semestre", values_to = "Absences")

ggplot(abs_data, aes(x = Semestre, y = Absences, fill = Semestre)) +
  geom_boxplot() +
  labs(title = "Comparaison des Absences : S1 vs S2",
       x = "Semestre", y = "Nombre d’Absences") +
  theme_minimal() +
  guides(fill = "none")

ggsave("output/figures/box_absences_s1_s2.png", dpi = 300, width = 7, height = 5)

#=================== 7. Scatter : Heures d’étude vs Absences S1 ===============================

ggplot(data_projet, aes(x = heures_etude_semaine, y = nb_absences_s1)) +
  geom_point(color = "red", alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  labs(title = "Relation Heures d’Étude vs Absences (S1)",
       x = "Heures d’étude", y = "Absences S1") +
  theme_minimal()

ggsave("output/figures/scatter_heures_abs_s1.png", dpi = 300, width = 7, height = 5)


#===========================================================================================
#                    3.1.3 — Performance Académique
#===========================================================================================

### 8. Boxplot Notes par Matière 
notes_long <- data_projet %>%
  select(contains("note_")) %>%
  pivot_longer(cols = everything(),
               names_to = "Matiere",
               values_to = "Note") %>%
  separate(Matiere, into = c("note", "Matiere", "Semestre"), sep = "_") %>%
  select(-note)

ggplot(notes_long, aes(x = Matiere, y = Note, fill = Semestre)) +
  geom_boxplot() +
  labs(title = "Distribution des Notes par Matière (S1 vs S2)",
       x = "Matière", y = "Note (/20)") +
  theme_minimal()

ggsave("output/figures/box_notes_matieres.png", dpi = 300, width = 9, height = 5)

#======================== 9. Histogramme des moyennes S1 ===============================================

ggplot(data_projet, aes(x = moyenne_s1)) +
  geom_histogram(bins = 20, fill = "purple", color = "white") +
  labs(title = "Distribution de la Moyenne Générale – S1",
       x = "Moyenne S1", y = "Effectif") +
  theme_minimal()

ggsave("output/figures/hist_moyennes_s1.png", dpi = 300, width = 7, height = 5)

#======================== 10. Histogramme des moyennes S2 ================================================
ggplot(data_projet, aes(x = moyenne_s2)) +
  geom_histogram(bins = 20, fill = "darkblue", color = "white") +
  labs(title = "Distribution de la Moyenne Générale – S2",
       x = "Moyenne S2", y = "Effectif") +
  theme_minimal()

ggsave("output/figures/hist_moyennes_s2.png", dpi = 300, width = 7, height = 5)

#======================== 11. Taux de réussite (S1 vs S2) ================================================

ggplot(taux_reussite_global, aes(x = Semestre, y = Taux_Reussite_Pourcentage)) +
  geom_col(fill = "darkgreen") +
  labs(title = "Taux de Réussite Global (S1 vs S2)",
       x = "Semestre", y = "Taux de Réussite (%)") +
  theme_minimal()

ggsave("output/figures/bar_taux_reussite.png", dpi = 300, width = 7, height = 5)

#======================== 12. Matières les plus difficiles =============================================== 

if (nrow(matieres_difficiles) > 0) {
  ggplot(matieres_difficiles,
         aes(x = reorder(Matiere, Note_Moyenne), y = Note_Moyenne, fill = Semestre)) +
    geom_col() +
    coord_flip() +
    labs(title = "Matières les Plus Difficiles (Note < 10)",
         x = "Matière", y = "Note Moyenne") +
    theme_minimal()
  
  ggsave("output/figures/bar_matieres_difficiles.png", dpi = 300, width = 8, height = 5)
}

# ==========================================================================================
#                   GRAPHIQUE 1 : ÉVOLUTION INDIVIDUELLE S1 → S2 (Spaghetti Plot)
# ==========================================================================================

# Préparer les données en format long
data_long <- data_analyse_evolution %>%
  select(id_etudiant, moyenne_s1, moyenne_s2) %>%
  pivot_longer(
    cols = c(moyenne_s1, moyenne_s2),
    names_to = "Semestre",
    values_to = "Moyenne"
  ) %>%
  mutate(
    Semestre = recode(Semestre,
                      "moyenne_s1" = "Semestre 1",
                      "moyenne_s2" = "Semestre 2")
  )

# Création du graphique
plot_evolution <- ggplot(data_long, aes(x = Semestre, y = Moyenne, group = id_etudiant)) +
  geom_line(alpha = 0.4, color = "steelblue") +
  geom_point(size = 2, color = "darkred") +
  labs(
    title = "Évolution des Moyennes entre le Semestre 1 et le Semestre 2",
    x = "",
    y = "Moyenne"
  ) +
  theme_minimal(base_size = 14)

print(plot_evolution) 

ggsave("output/figures/evolution_s1_s2.png",
       plot = plot_evolution,
       width = 9, height = 6, dpi = 300)

# ==========================================================================================
#                   GRAPHIQUE 2 : COMPARAISON GLOBALE DES MOYENNES (Barplot)
# ==========================================================================================

moyennes_globales <- data_analyse_evolution %>%
  summarise(
    Moyenne_S1 = mean(moyenne_s1),
    Moyenne_S2 = mean(moyenne_s2)
  ) %>%
  pivot_longer(cols = everything(), names_to = "Semestre", values_to = "Moyenne")

plot_global <- ggplot(moyennes_globales, aes(x = Semestre, y = Moyenne, fill = Semestre)) +
  geom_bar(stat = "identity", width = 0.5) +
  scale_fill_manual(values = c("Moyenne_S1" = "skyblue", "Moyenne_S2" = "orange")) +
  labs(
    title = "Moyennes Globales : Comparaison S1 vs S2",
    x = "",
    y = "Moyenne"
  ) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "none")

ggsave("output/figures/comparaison_globale_s1_s2.png",
       plot = plot_global,
       width = 8, height = 6, dpi = 300)

print(plot_global)


#=======================================================================================================
#                        GRAPHIQUE PAR FILIERE (S1 et S2)
#=======================================================================================================

# Boucle sur S1 et S2
for (semestre in c("S1","S2")) {
  col_moyenne <- paste0("moyenne_", tolower(semestre))
  
  plot_filiere <- data_projet %>%
    group_by(filiere) %>%
    summarise(Moyenne = mean(get(col_moyenne))) %>%
    ggplot(aes(x = reorder(filiere, Moyenne), y = Moyenne, fill = filiere)) +
    geom_col() +
    geom_text(aes(label = round(Moyenne,2)), vjust = -0.5) +
    labs(title = paste("Moyenne par Filière -", semestre),
         x = "Filière", y = "Moyenne") +
    theme_minimal(base_size = 14) +
    theme(legend.position = "none")
  
  print(plot_filiere)
  ggsave(filename = paste0("output/figures/filiere_moyenne_", tolower(semestre), ".png"),
         plot = plot_filiere, width = 8, height = 5, dpi = 300)
}


#======================================================================================================
#                            GRAPHIQUE PAR GENRE
#======================================================================================================

for (semestre in c("S1","S2")) {
  col_moyenne <- paste0("moyenne_", tolower(semestre))
  
  plot_genre <- data_projet %>%
    group_by(genre) %>%
    summarise(Moyenne = mean(get(col_moyenne))) %>%
    ggplot(aes(x = genre, y = Moyenne, fill = genre)) +
    geom_col() +
    geom_text(aes(label = round(Moyenne,2)), vjust = -0.5) +
    labs(title = paste("Moyenne par Genre -", semestre),
         x = "Genre", y = "Moyenne") +
    theme_minimal(base_size = 14) +
    theme(legend.position = "none")
  
  print(plot_genre)
  ggsave(filename = paste0("output/figures/genre_moyenne_", tolower(semestre), ".png"),
         plot = plot_genre, width = 6, height = 5, dpi = 300)
}

#=========================================================================================================
#                        GRAPHIQUES PAR CATEGORIE D'AGE
#=========================================================================================================

for (semestre in c("S1","S2")) {
  col_moyenne <- paste0("moyenne_", tolower(semestre))
  
  plot_age <- data_projet %>%
    group_by(categorie_age) %>%
    summarise(Moyenne = mean(get(col_moyenne))) %>%
    ggplot(aes(x = categorie_age, y = Moyenne, fill = categorie_age)) +
    geom_col() +
    geom_text(aes(label = round(Moyenne,2)), vjust = -0.5) +
    labs(title = paste("Moyenne par Catégorie d'Âge -", semestre),
         x = "Catégorie d'Âge", y = "Moyenne") +
    theme_minimal(base_size = 14) +
    theme(legend.position = "none")
  
  print(plot_age)
  ggsave(filename = paste0("output/figures/age_moyenne_", tolower(semestre), ".png"),
         plot = plot_age, width = 6, height = 5, dpi = 300)
}


#========================================================================================================
#                      GRAPHIQUE EVOLUTION (S1 et S2) GLOBALE PAR GENRE
#========================================================================================================

data_long <- data_projet %>%
  select(id_etudiant, moyenne_s1, moyenne_s2, genre) %>%
  pivot_longer(cols = c(moyenne_s1, moyenne_s2), names_to = "Semestre", values_to = "Moyenne") %>%
  mutate(Semestre = recode(Semestre, "moyenne_s1"="S1", "moyenne_s2"="S2"))

plot_evolution <- ggplot(data_long, aes(x = Semestre, y = Moyenne, group = id_etudiant)) +
  geom_line(alpha = 0.3, color = "steelblue") +
  geom_point(aes(color = genre), size = 2) +
  labs(title = "Évolution S1 → S2 par Étudiant et Genre",
       x = "Semestre", y = "Moyenne") +
  theme_minimal(base_size = 14)

print(plot_evolution)
ggsave("output/figures/evolution_s1_s2_par_genre.png",
       plot = plot_evolution, width = 9, height = 6, dpi = 300)

#========================================================================================================
#                         CORRELLATION HEURES D'ETUDES VS MOYENNE(S1 et S2)
#========================================================================================================

# Création du dossier "figures" s'il n'existe pas
if(!dir.exists("output/figures")){
  dir.create("output/figures", recursive = TRUE)
}

# Graphiques Moyenne vs Heures d'Étude (S1 et S2)
for (semestre in c("S1", "S2")) {
  col_moy <- paste0("moyenne_", tolower(semestre))
  
  p_hours <- ggplot(data_projet, aes_string(x = "heures_etude_semaine", y = col_moy)) +
    geom_point(color = "darkgreen", alpha = 0.6) +
    geom_smooth(method = "lm", se = TRUE, color = "red") +
    labs(title = paste("Moyenne", semestre, "en fonction des Heures d'Étude"),
         x = "Heures d'Étude par Semaine",
         y = paste("Moyenne", semestre)) +
    theme_minimal()
  
  # Affichage à l'écran
  print(p_hours)
  
  # Sauvegarde dans le dossier "figures"
  ggsave(filename = paste0("output/figures/moyenne_vs_heures_", tolower(semestre), ".png"),
         plot = p_hours, width = 8, height = 5, dpi = 300)
}


#========================================================================================================
#                        CORRELATION ABSENCE VS MOYENNE(S1 ET S2)
#========================================================================================================


# Création du dossier "figures" s'il n'existe pas
if(!dir.exists("output/figures")){
  dir.create("output/figures", recursive = TRUE)
}

# Graphiques Corrélation Absences vs Moyenne (S1 et S2)
for (semestre in c("S1", "S2")) {
  col_moy <- paste0("moyenne_", tolower(semestre))
  col_abs <- paste0("nb_absences_", tolower(semestre))
  
  p <- ggplot(data_projet, aes_string(x = col_abs, y = col_moy)) +
    geom_point(color = "darkorange", alpha = 0.7) +
    geom_smooth(method = "lm", se = TRUE, color = "red") +
    labs(title = paste("Absences vs Moyenne", semestre),
         x = paste("Nombre d'Absences", semestre),
         y = paste("Moyenne", semestre)) +
    theme_minimal()
  
  # Affichage à l'écran
  print(p)
  
  # Sauvegarde dans le dossier "figures"
  ggsave(filename = paste0("output/figures/absences_vs_moyenne_", tolower(semestre), ".png"),
         plot = p, width = 7, height = 5, dpi = 300)
}

#=======================================================================================================
#                        CORRELATION MOYENNE VS TRANCHE D'HEURES
#======================================================================================================

# Création du dossier "figures" s'il n'existe pas
if(!dir.exists("output/figures")){
  dir.create("output/figures", recursive = TRUE)
}

# Graphiques Moyenne par Tranche d'Heures d'étude (S1 et S2)
for (semestre in c("S1", "S2")) {
  col_moy <- paste0("moyenne_", tolower(semestre))
  
  p_tranche <- ggplot(data_projet, aes_string(x = "classe_heures_etude", y = col_moy)) +
    geom_boxplot(fill = "lightblue", color = "darkblue", alpha = 0.7) +
    geom_jitter(width = 0.2, alpha = 0.5, color = "darkorange") +
    labs(title = paste("Moyenne", semestre, "par Tranche d'Heures d'Étude"),
         x = "Tranche d'Heures d'Étude",
         y = paste("Moyenne", semestre)) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  # Affichage à l'écran
  print(p_tranche)
  
  # Sauvegarde dans le dossier "figures"
  ggsave(filename = paste0("output/figures/moyenne_par_tranche_heures_", tolower(semestre), ".png"),
         plot = p_tranche, width = 8, height = 5, dpi = 300)
}

#===================================================================================================
#                            BOXPLOT MOYENNE PAR FILIERE
#===================================================================================================
for (semestre in c("S1", "S2")) {
  col_moy <- paste0("moyenne_", tolower(semestre))
  
  p_box <- ggplot(data_projet, aes_string(x = "filiere", y = col_moy, fill = "filiere")) +
    geom_boxplot(alpha = 0.7) +
    labs(title = paste("Boxplot des Moyennes par Filière -", semestre),
         x = "Filière",
         y = paste("Moyenne", semestre)) +
    theme_minimal() +
    theme(legend.position = "none")
  
  print(p_box)
  
  ggsave(filename = paste0("output/figures/boxplot_moyenne_filiere_", tolower(semestre), ".png"),
         plot = p_box, width = 7, height = 5, dpi = 300)
}

#=======================================================================================================
#                           2. Scatter plot : Heures d'étude vs Moyenne
#=======================================================================================================
for (semestre in c("S1", "S2")) {
  col_moy <- paste0("moyenne_", tolower(semestre))
  
  p_scatter <- ggplot(data_projet, aes_string(x = "heures_etude_semaine", y = col_moy)) +
    geom_point(color = "darkblue", alpha = 0.6) +
    geom_smooth(method = "lm", se = TRUE, color = "red") +
    labs(title = paste("Heures d'étude vs Moyenne -", semestre),
         x = "Heures d'étude par semaine",
         y = paste("Moyenne", semestre)) +
    theme_minimal()
  
  print(p_scatter)
  
  ggsave(filename = paste0("output/figures/scatter_heures_vs_moyenne_", tolower(semestre), ".png"),
         plot = p_scatter, width = 7, height = 5, dpi = 300)
}

cat("\n\n--- FIN DU SCRIPT : Tous les graphiques ont été générés avec succès ---\n")
