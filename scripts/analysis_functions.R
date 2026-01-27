# ============================================================================= 
# Titel:    Funktionen zur deskriptiven Analyse und Visualisierung
#           des Titanic-Datensatzes
# Autoren:  (i), (iii) Nazarov Sergei
#           (ii), (iv) Heetae Kim
#           (v) Meico Bastian Heil, Saskia Lapkowski
# ============================================================================= 

# Hilfsfunktionen laden
source("helper_functions.R")

# (i)

# numSummary berechnet einfache deskriptive Statistiken für
#                            metrische Variablen eines Dataframes
#
# Input:  df        - Datenframe mit den Variablen
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
        mean = mean(x, na.rm = TRUE),
        median = median(x, na.rm = TRUE),
        sd = sd(x, na.rm = TRUE),
        min = min(x, na.rm = TRUE),
        max = max(x, na.rm = TRUE)
      ))
    }
  }
  
  result
}

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




