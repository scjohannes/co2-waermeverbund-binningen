library(testthat)

# Aufruf aus dem App-Ordner: testthat::test_file("tests/test-literatur.R")
source(file.path("..", "R", "model.R"), encoding = "UTF-8")
source(file.path("..", "R", "literatur.R"), encoding = "UTF-8")
source(file.path("..", "R", "unsicherheit.R"), encoding = "UTF-8")
register <- lies_register(file.path("..", "parameter.csv"))
priors <- lies_priors(file.path("..", "priors.csv"))
literatur <- lies_literatur(file.path("..", "literatur.csv"))
# Die App nummeriert Quellen der Regler und Begründungen der Verteilungen gemeinsam
texte <- c(register$quelle, priors$begruendung)

test_that("jeder Verweis in Parameterliste und Verteilungen steht im Literaturverzeichnis", {
  expect_no_error(zitate_nummerieren(texte, literatur))
})

test_that("jeder Eintrag im Literaturverzeichnis wird zitiert", {
  z <- zitate_nummerieren(texte, literatur)
  expect_setequal(z$verzeichnis$key, literatur$key)
})

test_that("jede Verteilung ausser den festen Grössen ist begründet", {
  expect_true(all(nzchar(priors$begruendung[priors$familie != "fest"])))
})

test_that("Verweise werden fortlaufend nach erstem Vorkommen nummeriert", {
  lit <- data.frame(key = c("a", "b", "c"), zitat = c("A", "B", "C"), url = "")
  z <- zitate_nummerieren(c("x [@b]", "y [@c; @b] und [@a]", "ohne Verweis"), lit)
  expect_equal(z$texte, c("x [1]", "y [1, 2] und [3]", "ohne Verweis"))
  expect_equal(z$verzeichnis$key, c("b", "c", "a"))
  expect_equal(z$verzeichnis$nr, 1:3)
})

test_that("ein unbekannter Schlüssel wird gemeldet", {
  lit <- data.frame(key = "a", zitat = "A", url = "")
  expect_error(zitate_nummerieren("x [@fehlt]", lit), "fehlt")
})

test_that("nach dem Nummerieren bleibt kein Schlüssel im Text", {
  z <- zitate_nummerieren(texte, literatur)
  expect_false(any(grepl("[@", z$texte, fixed = TRUE)))
})
