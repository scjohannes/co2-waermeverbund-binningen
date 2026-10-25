# Literaturverweise. In parameter.csv stehen Verweise als [@schluessel] oder [@a; @b];
# literatur.csv enthält die vollständigen Angaben. Nummeriert wird in der Reihenfolge des
# ersten Vorkommens in der Parameterliste.

VERWEIS <- "\\[@[^]]+\\]"

lies_literatur <- function(pfad) {
  lit <- read.csv(pfad, fileEncoding = "UTF-8", stringsAsFactors = FALSE, na.strings = "")
  lit$url[is.na(lit$url)] <- ""
  lit
}

verweis_schluessel <- function(verweis) {
  innen <- gsub("^\\[|\\]$", "", verweis)
  sub("^@", "", trimws(strsplit(innen, ";", fixed = TRUE)[[1]]))
}

# Ersetzt die Verweise durch Nummern und liefert das Verzeichnis in derselben Reihenfolge.
zitate_nummerieren <- function(texte, literatur) {
  verweise <- regmatches(texte, gregexpr(VERWEIS, texte))
  schluessel <- unique(unlist(lapply(unlist(verweise), verweis_schluessel)))
  unbekannt <- setdiff(schluessel, literatur$key)
  if (length(unbekannt) > 0) {
    stop("Unbekannte Literaturschlüssel: ", paste(unbekannt, collapse = ", "))
  }
  nummer <- setNames(seq_along(schluessel), schluessel)
  nummeriert <- texte
  regmatches(nummeriert, gregexpr(VERWEIS, nummeriert)) <- lapply(verweise, function(v) {
    vapply(v, function(x) paste0("[", paste(sort(nummer[verweis_schluessel(x)]), collapse = ", "), "]"),
           character(1), USE.NAMES = FALSE)
  })
  verzeichnis <- literatur[match(schluessel, literatur$key), c("key", "zitat", "url")]
  verzeichnis$nr <- seq_along(schluessel)
  list(texte = nummeriert, verzeichnis = verzeichnis)
}
