
# Titel:    Hilfsfunktion zur deskriptiven Analyse und Visualisierung des 
#           Titanic-Datensatzes
# Autoren:  Leticia Mbong Kwanga


remove_na <- function(df, vars) {
  df[complete.cases(df[, vars]), ]
}

var_exist <- function(df, vars) {
  missing <- vars[!vars %in% names(df)]
  if (length(missing) > 0) {
    stop(paste("Folgende Variablen fehlen:", paste(missing, collapse = ", ")))
  }
}

is_numeric <- function(x, varname = "") {
  if (!is.numeric(x)) {
    stop(paste("Variable", varname, "ist nicht numerisch."))
  }
}

is_dichotom <- function(x, varname = "") {
  x <- x[!is.na(x)]
  if (length(levels(factor(x))) != 2) {
    stop(paste("Variable", varname, "ist nicht dichotom."))
  }
}

is_factor <- function(x, varname = "") {
  if (!is.factor(x)) {
    stop(paste("Variable", varname, "ist nicht kategorial (factor)."))
  }
}
