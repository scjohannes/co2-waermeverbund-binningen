# Anteils-Regler, die zusammen 100 % ergeben (Planteile, Herkunft des Holzes).
#
# Verstellt man einen Regler, teilen sich die anderen den Rest im bisherigen Verhältnis. Kein
# Regler überschreitet sein Maximum; was nicht Platz hat, geht an die übrigen. Reicht der Platz
# aller anderen nicht, bleibt der verstellte Regler so nahe wie möglich am gewünschten Wert.
# Gerundet wird auf die Schrittweite, so dass die Summe genau stimmt.
umverteilen <- function(werte, geaendert, maximum, schritt = 1, summe = 100) {
  andere <- setdiff(names(werte), geaendert)
  grenze <- setNames(maximum, names(werte))[andere]
  werte[[geaendert]] <- max(werte[[geaendert]], summe - sum(grenze))
  rest <- summe - werte[[geaendert]]

  gewicht <- werte[andere]
  if (sum(gewicht) <= 0) gewicht[] <- 1
  ziel <- setNames(numeric(length(andere)), andere)
  frei <- andere
  while (length(frei) > 0) {
    anteil <- rest * gewicht[frei] / sum(gewicht[frei])
    voll <- anteil >= grenze[frei]
    if (!any(voll)) {
      ziel[frei] <- anteil
      break
    }
    ziel[frei[voll]] <- grenze[frei[voll]]
    rest <- rest - sum(grenze[frei[voll]])
    frei <- frei[!voll]
    if (length(frei) > 0 && sum(gewicht[frei]) <= 0) gewicht[frei] <- 1
  }

  # Auf Schritte runden; fehlende Schritte an die grössten Rundungsreste, solange Platz ist
  einheiten <- ziel / schritt
  ganz <- floor(einheiten + 1e-9)
  fehlend <- round((summe - werte[[geaendert]]) / schritt) - sum(ganz)
  for (id in names(sort(einheiten - ganz, decreasing = TRUE))) {
    if (fehlend <= 0) break
    if ((ganz[[id]] + 1) * schritt <= grenze[[id]] + 1e-9) {
      ganz[[id]] <- ganz[[id]] + 1
      fehlend <- fehlend - 1
    }
  }
  werte[andere] <- ganz * schritt
  werte
}
