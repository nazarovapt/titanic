# ============================================================================= 
# Titel:    Funktionen zur deskriptiven Analyse und Visualisierung
#           des Titanic-Datensatzes
# Autoren:  (i), (iii) Nazarov Sergei
#           (ii), (iv) Heetae Kim
#           (v) Meico Bastian Heil, Saskia Lapkowski
# ============================================================================= 

# Hilfsfunktionen laden
source("helper_functions.R")

# =============================================================================
# (i)
# numSummary berechnet einfache deskriptive Statistiken für
#                            metrische Variablen eines Dataframes
#
# Input:  df        - Dataframe mit den Variablen
#         variablen - Vektor mit Spaltennamen der numerischen Variablen
# Output: data.frame mit Spalten:
#           variable - Name der Variable
#           mean     - Mittelwert
#           median   - Median
#           sd       - Standardabweichung
#           min      - Minimum
#           max      - Maximum

numSummary <- function(df, variablen) {
  result <- data.frame()
  
  for (var in variablen) {
    x <- df[[var]]
    if (is.numeric(x)) {
      result <- rbind(result, data.frame(
        variable = var,
        mean   = mean(x, na.rm = TRUE),
        median = median(x, na.rm = TRUE),
        sd     = sd(x, na.rm = TRUE),
        min    = min(x, na.rm = TRUE),
        max    = max(x, na.rm = TRUE)
      ))
    }
  }
  
  result
}
# =============================================================================

# =============================================================================
# (ii)
# catSummary berechnet deskriptive Statistiken für eine kategoriale Variable
#
# Input:  df         - data.frame mit den Variablen
#         var        - Name der kategorialen Variable
# Output: data.frame mit Spalten:
#           level    - Kategorie
#           n        - absolute Häufigkeit
#           percent  - relative Häufigkeit in %
catSummary <- function(df, var) {
  x <- df[[var]]
  counts <- table(x)
  percent <- round(100 * counts / sum(counts), 1)
  
  data.frame(
    level = names(counts),
    n = as.integer(counts),
    percent = as.numeric(percent),
    row.names = NULL,
    stringsAsFactors = FALSE
  )
}
# =============================================================================

# =============================================================================
# (iii)
# catBivSummary berechnet bivariate deskriptive Statistiken für zwei 
#               kategoriale Variablen
#
# Input:  df   - data.frame mit den Variablen
#         var1 - erste kategoriale Variable (Zeilen)
#         var2 - zweite kategoriale Variable (Spalten)
# Output: Liste mit:
#           counts  - absolute Häufigkeiten (Kreuztabelle)
#           percent - relative Häufigkeiten (Zeilenprozente)

catBivSummary <- function(df, var1, var2) {
  
  # Kreuztabelle mit absoluten Häufigkeiten
  counts <- table(df[[var1]], df[[var2]])
  
  # Berechnung der Zeilensummen
  row_totals <- rowSums(counts)
  
  # Relative Häufigkeiten pro Zeile
  percent <- counts / row_totals
  percent <- round(percent, 3)
  
  list(
    counts = counts,
    percent = percent
  )
}
# =============================================================================

# =============================================================================
# (iv)
# metricDichoSummary berechnet bivariate deskriptive Statistiken für den
#                    Zusammenhang zwischen einer metrischen und einer
#                    dichotomen Variable
#
# Input:  df          - data.frame mit den Variablen
#         metric_var  - Name der metrischen Variable
#         group_var   - Name der dichotomen Variable
# Output: data.frame mit Spalten:
#           group - Ausprägungen der dichotomen Variable
#           n     - Anzahl der Beobachtungen pro Gruppe
#           mean  - Arithmetisches Mittel der metrischen Variable
#           sd    - Standardabweichung der metrischen Variable
metricDichoSummary <- function(df, metric_var, group_var) {
  d <- df[, c(metric_var, group_var)]
  d <- d[complete.cases(d), ]
  names(d) <- c("metric", "group")
  d$group <- factor(d$group)
  
  if (length(levels(d$group)) != 2) stop("group_var muss dichotom sein.")
  
  out <- by(d$metric, d$group, function(x)
    c(n = length(x), mean = mean(x), sd = sd(x))
  )
  
  data.frame(
    group = names(out),
    n = sapply(out, `[[`, "n"),
    mean = round(sapply(out, `[[`, "mean"), 2),
    sd = round(sapply(out, `[[`, "sd"), 2),
    row.names = NULL
  )
}
# =============================================================================

# =============================================================================
# (v) 
# Eine Funktion, die eine geeignete Visualisierung von drei oder vier
# kategorialen Variablen erstellt
  
library(ggplot2)

# Jetzt sind alle kategorialen Variablen Faktoren
titanic_new$Anrede <- as.factor(titanic_new$Anrede)

plot_categorical_faceted <- function(data, var_x, var_fill, var_facet1,
                                     var_facet2 = NULL) {
  
  vars <- c(var_x, var_fill, var_facet1, var_facet2)
  # Prüfen ob vars leer ist
  vars <- vars[!is.null(vars)]
  
  # Prüft anhand von Faktor ob Variable kategorial ist
  for (v in vars) {
    if (!is.factor(data[[v]])) {
      stop(paste("Variable", v, "ist nicht kategorial."))
    }
  }
  
# Eigentliche Funktion um facettiertes Balkendiagramm zu plotten
  p <- ggplot(
    data = data,
    aes_string(x = var_x, fill = var_fill)
  ) +
    geom_bar(position = "fill") +
    labs(
      x = var_x,
      y = "Relative Häufigkeit",
      fill = var_fill
    ) +
    theme_minimal()
  
  # Facettierung
  if (is.null(var_facet2)) {
    # 3 Variablen -> eine Facette
    p <- p + facet_wrap(as.formula(paste("~", var_facet1)))
  } else {
    # 4 Variablen -> Grid-Facettierung
    p <- p + facet_grid(
      as.formula(paste(var_facet2, "~", var_facet1))
    )
  }
  
  return(p)
}
# =============================================================================
