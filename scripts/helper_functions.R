# ============================================================================= 
# Titel:    Hilfsfunktion zur deskriptiven Analyse und Visualisierung des 
#           Titanic-Datensatzes
# Autoren:  Leticia Mbong Kwanga
# ============================================================================= 

# Diese Hilfsfunktion entfernt alle NA-Werte
remove_na <- function(df, vars) {
  df[complete.cases(df[, vars]), ]
}

# Diese Hilfsfunktion überprüft, ob alle Variaben im Dataframe vorhanden sind
var_exist <- function(df, vars) {
  missing <- vars[!vars %in% names(df)]
  if (length(missing) > 0) {
    stop(paste("Folgende Variablen fehlen:", paste(missing, collapse = ", ")))
  }
}

# Diese Hilfsfunktion prüft ob die Variable numerisch ist
is_numeric <- function(x, varname = "") {
  if (!is.numeric(x)) {
    stop(paste("Variable", varname, "ist nicht numerisch."))
  }
}

# Diese Hilfsfunktion prüft ob die Variable dichotom ist
is_dichotom <- function(x, varname = "") {
  x <- x[!is.na(x)]
  if (length(levels(factor(x))) != 2) {
    stop(paste("Variable", varname, "ist nicht dichotom."))
  }
}

# Diese Hilfsfunktion prüft, ob die Variable ein Faktor ist 
is_factor <- function(x, varname = "") {
  if (!is.factor(x)) {
    stop(paste("Variable", varname, "ist kein Faktor"))
  }
}
