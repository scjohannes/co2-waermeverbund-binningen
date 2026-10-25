library(testthat)

# Aufruf aus dem App-Ordner: testthat::test_file("tests/test-regler.R")
source(file.path("..", "R", "regler.R"), encoding = "UTF-8")

herkunft <- c(frisch = 50, kronen = 25, totholz = 20, kaskade = 5)
plan <- c(holz = 53, wrg = 11, wp = 28, oel = 8)
plan_max <- c(100, 30, 100, 30)

test_that("die anderen Anteile teilen sich den Rest im bisherigen Verhältnis", {
  neu <- umverteilen(replace(herkunft, "totholz", 100), "totholz", rep(100, 4), schritt = 5)
  expect_equal(neu, c(frisch = 0, kronen = 0, totholz = 100, kaskade = 0))
  neu <- umverteilen(replace(herkunft, "frisch", 70), "frisch", rep(100, 4), schritt = 5)
  expect_equal(sum(neu), 100)
  expect_equal(neu[["frisch"]], 70)
  expect_equal(unname(neu[c("kronen", "totholz", "kaskade")]), c(15, 10, 5))
})

test_that("die Summe ergibt nach dem Runden immer genau 100", {
  for (wert in seq(0, 100, 5)) {
    neu <- umverteilen(replace(herkunft, "kronen", wert), "kronen", rep(100, 4), schritt = 5)
    expect_equal(sum(neu), 100, label = paste("kronen =", wert))
    expect_true(all(neu %% 5 == 0))
  }
  for (wert in 0:100) {
    neu <- umverteilen(replace(plan, "holz", wert), "holz", plan_max)
    expect_equal(sum(neu), 100, label = paste("holz =", wert))
    expect_true(all(neu == round(neu)))
  }
})

test_that("kein Regler überschreitet sein Maximum; der Rest geht an die anderen", {
  neu <- umverteilen(replace(plan, "holz", 0), "holz", plan_max)
  expect_true(all(neu <= plan_max))
  expect_equal(sum(neu), 100)
  # danach Wärmepumpen auf 0: Wärmerückgewinnung und Öl sind voll, den Rest nimmt der Holzkessel
  stand <- umverteilen(replace(plan, "holz", 0), "holz", plan_max)
  neu <- umverteilen(replace(stand, "wp", 0), "wp", plan_max)
  expect_equal(neu, c(holz = 40, wrg = 30, wp = 0, oel = 30))
})

test_that("haben alle anderen keinen Platz mehr, bleibt der verstellte Regler so nahe wie möglich", {
  neu <- umverteilen(c(a = 20, b = 25, c = 25), "a", c(100, 30, 30))
  expect_equal(neu, c(a = 40, b = 30, c = 30))
})

test_that("waren alle anderen auf 0, teilen sie sich den Rest gleichmässig", {
  neu <- umverteilen(c(frisch = 80, kronen = 0, totholz = 0, kaskade = 0), "frisch",
                     rep(100, 4), schritt = 5)
  expect_equal(sum(neu), 100)
  expect_true(all(neu[c("kronen", "totholz", "kaskade")] %in% c(5, 10)))
})
