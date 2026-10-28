# Erzeugt docs/js/daten.js für die Website aus parameter.csv, priors.csv, literatur.csv, wald.html,
# methode.html und formeln.html. Die Literaturnummern und die Stärken der Verteilungen kommen
# aus denselben R-Funktionen wie in der Shiny-App.
#
# Aufruf aus dem App-Ordner: Rscript web/bauen.R

if (!file.exists("parameter.csv")) stop("Bitte aus dem App-Ordner aufrufen: Rscript web/bauen.R")

source("R/model.R", encoding = "UTF-8")
source("R/literatur.R", encoding = "UTF-8")
source("R/unsicherheit.R", encoding = "UTF-8")

lies_text <- function(pfad) paste(readLines(pfad, encoding = "UTF-8", warn = FALSE), collapse = "\n")

register <- lies_register("parameter.csv")
priors <- priors_vorbereiten(lies_priors("priors.csv"), register)
zitate <- zitate_nummerieren(c(register$quelle, priors$begruendung, lies_text("wald.html")),
                             lies_literatur("literatur.csv"))
register$quelle <- zitate$texte[seq_len(nrow(register))]
priors$begruendung <- zitate$texte[nrow(register) + seq_len(nrow(priors))]
wald <- zitate$texte[nrow(register) + nrow(priors) + 1]

auswahl <- function(text) {
  if (!nzchar(text)) return(list())
  lapply(strsplit(strsplit(text, "|", fixed = TRUE)[[1]], "=", fixed = TRUE),
         function(paar) list(text = paar[1], wert = as.numeric(paar[2])))
}

daten <- list(
  parameter = lapply(seq_len(nrow(register)), function(i) {
    z <- register[i, ]
    list(id = z$id, gruppe = z$gruppe, typ = z$typ, label = z$label, einheit = z$einheit,
         wert = z$wert, min = z$min, max = z$max, schritt = z$schritt,
         auswahl = auswahl(z$auswahl), quelle = z$quelle)
  }),
  priors = lapply(seq_len(nrow(priors)), function(i) {
    pr <- priors[i, ]
    list(id = pr$id, familie = pr$familie, a = pr$a, b = pr$b, skala = pr$skala,
         staerke = if (is.na(pr$staerke)) NULL else pr$staerke, begruendung = pr$begruendung)
  }),
  literatur = lapply(seq_len(nrow(zitate$verzeichnis)), function(i) {
    z <- zitate$verzeichnis[i, ]
    list(nr = z$nr, zitat = z$zitat, url = z$url)
  }),
  methode = lies_text("methode.html"),
  formeln = lies_text("formeln.html"),
  wald = wald
)

json <- jsonlite::toJSON(daten, auto_unbox = TRUE, digits = NA, pretty = FALSE, null = "null")
inhalt <- c(
  "// Erzeugt von web/bauen.R aus parameter.csv, priors.csv, literatur.csv, wald.html, methode.html",
  "// und formeln.html. Nicht von Hand ändern.",
  "(function (g) {",
  paste0("  var DATEN = ", json, ";"),
  "  if (typeof module !== 'undefined' && module.exports) module.exports = DATEN; else g.DATEN = DATEN;",
  "})(typeof self !== 'undefined' ? self : this);"
)
dir.create(file.path("docs", "js"), recursive = TRUE, showWarnings = FALSE)
datei <- file(file.path("docs", "js", "daten.js"), open = "w", encoding = "UTF-8")
writeLines(enc2utf8(inhalt), datei)
close(datei)
cat("docs/js/daten.js geschrieben:", nrow(register), "Annahmen,", nrow(zitate$verzeichnis), "Quellen\n")
