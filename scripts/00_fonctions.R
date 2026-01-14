#==========================================================================
# RECUEIL DES FONCTIONS PERSONNALISÉES - PROJET PERFORMANCE ÉTUDIANTE
#==========================================================================

#' Calculer le Mode
#' @description Calcule la valeur la plus fréquente dans un vecteur
#' @param v Un vecteur (numérique ou facteur)
get_mode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))] 
}

#' Moyenne par Genre
#' @description Calcule la moyenne d'une colonne spécifique pour un genre donné
#' @param data Le dataframe
#' @param col Nom de la colonne (ex: "moyenne_s1")
#' @param g Le genre (ex: "Femme")
get_mean_by_genre_final <- function(data, col, g) {
  res <- data %>% 
    filter(genre == g) %>% 
    summarise(Moyenne = mean(get(col), na.rm = TRUE)) %>% 
    pull(Moyenne)
  return(res)
}
