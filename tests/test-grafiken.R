library(testthat)
suppressMessages(library(ggplot2))

# Aufruf aus dem App-Ordner: testthat::test_file("tests/test-grafiken.R")
source(file.path("..", "R", "model.R"), encoding = "UTF-8")
source(file.path("..", "R", "grafiken.R"), encoding = "UTF-8")
p0 <- standardwerte(lies_register(file.path("..", "parameter.csv")))

test_that("die Antwortkurven zeigen auch Werte unter 0, statt sie wegzulassen", {
  # Maximalvorrat knapp über dem heutigen: der Wald bindet nach dem Eingriff mehr als ohne
  e <- rechne_alles(modifyList(p0, list(vorrat_max = 350)))
  expect_lt(min(e$holz$kurven$anteil), 0)
  expect_no_warning(ggplotGrob(grafik_antwort(e$holz$kurven)))
})

test_that("die Verlaufsgrafik lässt sich ohne und mit Band ohne Warnung zeichnen", {
  e <- rechne_alles(p0)
  expect_no_warning(ggplotGrob(grafik_verlauf(e$summe)))
  band <- data.frame(variante = rep(VARIANTEN, JAHRE), jahr = rep(seq_len(JAHRE), each = length(VARIANTEN)),
                     q2.5 = 1, q17 = 2, q50 = 3, q83 = 4, q97.5 = 5)
  expect_no_warning(ggplotGrob(grafik_verlauf(e$summe, band)))
})
