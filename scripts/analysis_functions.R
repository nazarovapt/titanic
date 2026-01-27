# ============================================================================= 
# Titel:    Analyse der Verkaufsdaten
# Datei:    analyse_verkauf.R
# Autoren: (i), (iii) Nazarov Sergei
#          (ii), (iv) Heetae Kim
#          (v) Meico Bastian Heil, Saskia Lapkowski
# Datum:    2026-01-27
# Zweck:    Datenaufbereitung und Auswertung
# ============================================================================= 

# Hilfsfunktionen laden
source("helper_functions.R")

# (i)

# num_summary - Berechnet zentrale deskriptive Statistiken für
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

num_summary <- function(df, variablen) {
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





