
calcul_percentage_change <- function(data) {
  # S'assurer que les colonnes numériques sont bien formatées
  numeric_cols <- setdiff(names(data), "EwE_Model")  # Exclure la colonne EwE_Model
  for (col in numeric_cols) {
    data[[col]] <- as.numeric(as.character(data[[col]]))  # Convertir en numérique
    if (any(is.na(data[[col]]))) {
      stop(paste("La colonne", col, "contient des valeurs non numériques ou des NA après conversion."))
    }
  }
  
  # Créer un tableau vide pour stocker les résultats
  result <- data.frame(EwE_Model_combinaison = character(), 
                       matrix(NA, nrow = 0, ncol = length(numeric_cols)),
                       stringsAsFactors = FALSE)
  colnames(result)[-1] <- numeric_cols  # Nommer les colonnes
  
  # Boucler sur toutes les combinaisons possibles de EwE_Model
  for (i in 1:(nrow(data) - 1)) {
    for (j in (i + 1):nrow(data)) {
      # Extraire les noms des modèles
      model_1 <- data$EwE_Model[i]
      model_2 <- data$EwE_Model[j]
      
      # Calculer les pourcentages de changement pour chaque indicateur
      percentage_changes <- 100 * (data[j, numeric_cols] - data[i, numeric_cols]) / data[i, numeric_cols]
      
      # Ajouter les résultats au tableau
      result <- rbind(result, 
                      c(EwE_Model_combinaison = paste(model_1, "-", model_2), 
                        as.list(percentage_changes)))
    }
  }
  
  # Convertir les colonnes de résultats en numérique et arrondir à 2 décimales
  result[, -1] <- lapply(result[, -1], function(x) round(as.numeric(x), 2))
  
  # Afficher le tableau interactif
  datatable(data = result, filter = "top", 
            rownames = FALSE, options = list(pageLength = 20, scrollX = TRUE))
}

