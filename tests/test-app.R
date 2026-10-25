library(testthat)
library(shiny)

# Aufruf aus dem App-Ordner: testthat::test_file("tests/test-app.R")
app <- shinyAppDir(normalizePath(".."))

test_that("ein Hinweis in der Seitenleiste erscheint ohne Fehler", {
  testServer(app, {
    session$setInputs(p_vorrat_max = 350)
    session$elapse(500)   # Regler werden verzögert übernommen
    expect_match(as.character(output$hinweise$html), "Der maximale Vorrat liegt kaum über dem heutigen")
  })
})

test_that("Verstellen eines gekoppelten Anteils läuft ohne Fehler", {
  testServer(app, {
    session$setInputs(p_plan_holz = 53, p_plan_wrg = 11, p_plan_wp = 28, p_plan_oel = 8)
    expect_no_error(session$setInputs(p_plan_holz = 63))
    session$elapse(500)
    expect_null(output$hinweise)
  })
})
