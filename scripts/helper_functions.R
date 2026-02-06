# ============================================================================= 
# Titel:    Hilfsfunktion zur deskriptiven Analyse und Visualisierung des 
#           Titanic-Datensatzes
# Autoren:  Leticia Mbong Kwanga
# ============================================================================= 

remove_na <- function(df, vars) {
  df[complete.cases(df[, vars]), ]
}

var_exist <- function(df, vars) {
  missing <- vars[!vars %in% names(df)]
  if (length(missing) > 0) {
    stop(paste("Folgende Variablen fehlen:", paste(missing, collapse = ", ")))
  }
}
