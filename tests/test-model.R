library(testthat)

# Aufruf aus dem App-Ordner: testthat::test_file("tests/test-model.R")
# (testthat setzt das Arbeitsverzeichnis auf den Ordner dieser Datei)
source(file.path("..", "R", "model.R"), encoding = "UTF-8")
p0 <- standardwerte(lies_register(file.path("..", "parameter.csv")))

test_that("eine frisch verbrannte Tonne ist zu 100 % in der Luft", {
  f <- antwort_frischholz(p0)
  expect_equal(f[, 1], rep(1, JAHRE))
})

test_that("ohne Zuwachs bleibt die Schuld und wächst um die Wurzeln und den Boden", {
  p <- modifyList(p0, list(zuwachs = 0, stoerung = 0, boden_erholung = 0))
  f <- antwort_frischholz(p, n = 200)
  rho <- p$wurzel_anteil / 100
  expect_equal(f[1, 200], 1 / (1 - rho) + p$boden_verlust / 100, tolerance = 1e-3)
})

test_that("wird der Wald ohnehin genutzt, bleibt die Schuld länger als bei ungenutztem Wald", {
  bisher <- antwort_frischholz(modifyList(p0, list(anteil_ungenutzt = 0)))
  ungenutzt <- antwort_frischholz(modifyList(p0, list(anteil_ungenutzt = 100)))
  expect_gt(bisher[1, JAHRE], ungenutzt[1, JAHRE])
})

test_that("bei bisheriger Nutzung und maximalem Zuwachs bildet sich die Schuld nicht zurück", {
  # theta = 1 und Vorrat = halbes Maximum: Zuwachs ist maximal und hängt kaum vom Vorrat ab
  p <- modifyList(p0, list(anteil_ungenutzt = 0, theta = 1, vorrat_max = 2 * p0$vorrat_heute,
                           stoerung = 0, klima_zuwachs = 0, eingriff = 5, verj_minderung = 0,
                           boden_verlust = 0, wurzel_hwz = 1e6))
  f <- antwort_frischholz(p, n = 11)
  expect_equal(f[1, 11], 1, tolerance = 0.02)
})

test_that("Bodenkohlenstoff geht in 10 Jahren verloren und erholt sich danach", {
  p <- modifyList(p0, list(zuwachs = 0, stoerung = 0, wurzel_hwz = 1e6,
                           boden_verlust = 10, boden_erholung = 5))
  f <- antwort_frischholz(p, n = 50)
  expect_equal(f[1, 11] - f[1, 1], 0.10, tolerance = 1e-4)   # Alter 10: voller Verlust
  expect_equal(f[1, 31], f[1, 1], tolerance = 1e-4)          # Alter 30: wieder erholt
})

test_that("auch extreme Waldannahmen liefern endliche Ergebnisse", {
  # Ziehung, die ohne Untergrenze einen negativen Vorrat und NaN ergab
  p <- modifyList(p0, list(vorrat_heute = 321.4, vorrat_max = 592.3, zuwachs = 2.24, theta = 3.19,
                           eingriff = 46.2, stoerung = 0.442, klima_zuwachs = -15.3,
                           verj_minderung = 5.3, anteil_ungenutzt = 0))
  expect_true(all(is.finite(antwort_frischholz(p))))
  expect_true(all(is.finite(rechne_kumuliert(p))))
})

test_that("Restholz ist nach einer Halbwertszeit zur Hälfte zersetzt", {
  expect_equal(antwort_zerfall(10, 0)[11], 0.5)
  expect_equal(antwort_zerfall(10, 0.1)[11], 0.1 + 0.9 * 0.5)
})

test_that("Herkunftsanteile summieren in jedem Jahr auf 1", {
  p <- modifyList(p0, list(anteil_frisch = 70, anteil_kronen = 40, anteil_totholz = 40,
                           anteil_kaskade = 30, welle_zusatz = 50, welle_jahre = 5))
  s <- herkunft_anteile(p)
  expect_equal(rowSums(s), rep(1, JAHRE))
  expect_true(all(s >= 0))
})

test_that("Holz aus der Kaskade: nur der Produktanteil zählt, und er zerfällt mit seiner Halbwertszeit", {
  p <- modifyList(p0, list(produkt_anteil = 40, hwz_produkt = 30))
  f <- antwort_kaskade(p)
  expect_equal(f[1], 0.4)
  expect_equal(f[31], 0.2)
  # nur Kaskadenholz ohne gespeicherten Anteil: kein biogenes CO2 im Plan
  p <- modifyList(p0, list(anteil_frisch = 0, anteil_kronen = 0, anteil_totholz = 0,
                           anteil_kaskade = 100, produkt_anteil = 0))
  bio <- rechne_alles(p)$jahre
  bio <- bio[bio$variante == "Plan IWB" & bio$komponente == "Holz: CO2 netto (Wald)", ]
  expect_equal(sum(abs(bio$jaehrlich)), 0)
})

test_that("100 % Öl als Einzelheizung ohne Ersatz wächst linear", {
  e <- rechne_alles(modifyList(p0, list(fossil_dezentral = 1)))
  oel <- e$summe[e$summe$variante == "100 % Öl", ]
  oel <- oel[order(oel$jahr), ]
  expect_equal(diff(oel$kumuliert), rep(oel$kumuliert[2], JAHRE))
})

test_that("ohne Holz sind der Plan und «Öl statt Holz» gleich", {
  s <- rechne_alles(modifyList(p0, list(plan_holz = 0)))$summe
  expect_equal(s$kumuliert[s$variante == "Plan, Öl statt Holz"], s$kumuliert[s$variante == "Plan IWB"])
})

test_that("«Öl statt Holz» verbrennt Öl für Holz- und Ölanteil zusammen", {
  p <- modifyList(p0, list(fossil_bau = 0))
  e <- rechne_alles(p)$jahre
  jahr1 <- e$jaehrlich[e$variante == "Plan, Öl statt Holz" & e$jahr == 1 &
                         e$komponente == "Öl/Gas: Verbrennung"]
  anteil <- (p$plan_holz + p$plan_oel) / (p$plan_holz + p$plan_wrg + p$plan_wp + p$plan_oel)
  erwartet <- anteil * p$q_del / (1 - p$netz_verlust / 100) / (p$eta_oel / 100) * p$oel_direkt / 1000
  expect_equal(jahr1, erwartet)
})

test_that("ohne Spitzenlast sind beide Erdwärmesonden-Varianten gleich", {
  s <- rechne_alles(modifyList(p0, list(erd_oel = 0)))$summe
  oel <- s$kumuliert[s$variante == "Erdwärmesonden + Öl"]
  strom <- s$kumuliert[s$variante == "Erdwärmesonden"]
  expect_equal(strom, oel)
})

test_that("Spitzenlast in den Häusern trägt keine Netzverluste, aber den Strom-Zuschlag", {
  p <- modifyList(p0, list(erd_oel = 10, wp_bau = 0, netz_bau = 0))
  e <- rechne_alles(p)$jahre
  jahr1 <- e$jaehrlich[e$variante == "Erdwärmesonden" & e$jahr == 1 & e$komponente == "Strom"]
  netz <- 0.9 * p$q_del
  erwartet_mwh <- netz / (1 - p$netz_verlust / 100) / p$jaz_erdsonde + netz * p$netz_strom / 100 +
    0.1 * p$q_del / p$jaz_spitze * (1 + p$strom_spitze / 100)
  expect_equal(jahr1, erwartet_mwh * strom_pfad(p)[1])
})

test_that("Öl-Einzelheizung entspricht Hand-Rechnung", {
  p <- modifyList(p0, list(fossil_bau = 0, fossil_dezentral = 1))
  e <- rechne_alles(p)
  jahr1 <- e$summe$kumuliert[e$summe$variante == "100 % Öl" & e$summe$jahr == 1]
  erwartet <- p$q_del / (p$eta_oel / 100) * (p$oel_direkt + p$oel_vorkette) / 1000
  expect_equal(jahr1, erwartet)
})

test_that("Gas-Fernwärme trägt dieselben Netzverluste, Pumpstrom und Netzbau wie der Plan", {
  p <- modifyList(p0, list(fossil_bau = 0, fossil_dezentral = 0))
  e <- rechne_alles(p)
  jahr1 <- e$summe$kumuliert[e$summe$variante == "100 % Gas" & e$summe$jahr == 1]
  erzeugt <- p$q_del / (1 - p$netz_verlust / 100)
  methan <- p$gas_leck / 100 * METHAN_G_PRO_KWH * p$gwp_ch4
  erwartet <- erzeugt / (p$eta_gas / 100) * (p$gas_direkt + p$gas_vorkette + methan) / 1000 +
    p$q_del * p$netz_strom / 100 * strom_pfad(p)[1] + p$q_del * p$netz_bau / 1000
  expect_equal(jahr1, erwartet)
})

test_that("ohne Holz im Plan gibt es kein biogenes CO2", {
  p <- modifyList(p0, list(plan_holz = 0, plan_wrg = 0))
  e <- rechne_alles(p)
  bio <- e$jahre[e$jahre$variante == "Plan IWB" & e$jahre$komponente == "Holz: CO2 netto (Wald)", ]
  expect_equal(sum(abs(bio$jaehrlich)), 0)
})

test_that("Holz aus Restholz mit kurzer Halbwertszeit nähert sich dem Kamin-CO2 nicht an", {
  p <- modifyList(p0, list(anteil_frisch = 0, anteil_kronen = 100, anteil_totholz = 0,
                           hwz_kronen = 1, humus_anteil = 0))
  e <- rechne_alles(p)
  netto <- tail(e$summe$kumuliert[e$summe$variante == "Plan IWB"], 1)
  expect_lt(netto, tail(e$brutto$kumuliert, 1) * 0.5)
})

test_that("Holzwärme mit Kamin-CO2 von rund 430–500 g/kWh ohne Lagerverlust", {
  e <- rechne_alles(modifyList(p0, list(dm_verlust = 0)))
  expect_gt(e$holz$kamin_g_kwh, 430)
  expect_lt(e$holz$kamin_g_kwh, 500)
})

test_that("Lagerverlust erhöht die Erntemenge und das biogene CO2 im selben Verhältnis", {
  ohne <- rechne_alles(modifyList(p0, list(dm_verlust = 0)))$holz
  mit <- rechne_alles(modifyList(p0, list(dm_verlust = 10)))$holz
  expect_equal(mit$m3 / ohne$m3, 1 / 0.9)
  expect_equal(mit$kamin_g_kwh / ohne$kamin_g_kwh, 1 / 0.9)
  expect_equal(mit$lkw_pro_jahr, ohne$lkw_pro_jahr)   # geliefert wird gleich viel
})

test_that("1 % des Holzkohlenstoffs als Methan ergibt rund 45 g CO2e/kWh Holzwärme", {
  p <- modifyList(p0, list(dm_verlust = 0, lager_methan = 1))
  hk <- holz_kennzahlen(p)
  zusatz <- hk$kamin / (p$eta_holz / 100) * lager_methan_faktor(p) * 1000   # g/kWh Wärme
  expect_gt(zusatz, 35)
  expect_lt(zusatz, 55)
})

test_that("jeder Regler verändert die Emissionen oder die Holzkennzahlen", {
  reg <- lies_register(file.path("..", "parameter.csv"))
  # Diese Regler wirken nur, wenn ein anderer nicht auf dem Standardwert steht
  bedingung <- list(welle_jahre = list(welle_zusatz = 40), jaz_luft = list(fossil_laufzeit = 20))
  ergebnis <- function(p) {
    h <- rechne_alles(p)$holz
    c(rechne_kumuliert(p), h$m3, h$srm, h$lkw_pro_jahr)
  }
  for (i in which(reg$typ != "fest")) {
    z <- reg[i, ]
    werte <- switch(z$typ,
      auswahl = as.numeric(sub(".*=", "", strsplit(z$auswahl, "|", fixed = TRUE)[[1]])),
      switch = c(0, 1),
      c(z$min, z$max))
    p <- modifyList(p0, bedingung[[z$id]] %||% list())
    tief <- ergebnis(modifyList(p, setNames(list(min(werte)), z$id)))
    hoch <- ergebnis(modifyList(p, setNames(list(max(werte)), z$id)))
    expect_gt(max(abs(hoch - tief)), 1, label = z$id)
  }
})

test_that("Klima und Bodenerholung wirken auf die Punktschätzung, aber nur schwach", {
  # Der Klimafaktor trifft den Wald mit und ohne Heizwerk gleich; die Differenz ändert sich nur
  # über die Form der Wachstumskurve. Die Bodenerholung kann höchstens den Bodenverlust zurückholen.
  plan50 <- function(aenderung) rechne_kumuliert(modifyList(p0, aenderung))["Plan IWB", JAHRE]
  basis <- plan50(list())
  expect_gt(plan50(list(klima_zuwachs = -50)) - basis, 500)
  expect_lt(plan50(list(klima_zuwachs = 20)) - basis, -500)
  expect_gt(plan50(list(boden_erholung = 0)) - basis, 500)
  expect_equal(plan50(list(boden_erholung = 0, boden_verlust = 0)),
               plan50(list(boden_erholung = 20, boden_verlust = 0)))
})

test_that("kein Regler kehrt die Richtung des Ergebnisses um", {
  # Jeder Regler wird von links nach rechts geschoben; gemessen wird über alle Varianten und
  # Jahre, wie weit eine Reihe dabei zurückläuft. Unter 0,2 kt bleiben nur gegenläufige
  # Wirkungen des Zuwachses (schnelleres Nachwachsen, aber auch schnellerer Vergleichswald),
  # die im Diagramm nicht zu sehen sind.
  reg <- lies_register(file.path("..", "parameter.csv"))
  hin_und_her <- function(y) (sum(abs(diff(y))) - abs(y[length(y)] - y[1])) / 2
  for (i in which(reg$typ == "slider")) {
    z <- reg[i, ]
    gitter <- seq(z$min, z$max, by = z$schritt)
    if (length(gitter) > 25) gitter <- seq(z$min, z$max, length.out = 25)
    k <- vapply(gitter, function(w) as.vector(rechne_kumuliert(modifyList(p0, setNames(list(w), z$id)))),
                numeric(length(VARIANTEN) * JAHRE))
    expect_lt(max(apply(k, 1, hin_und_her)), 200, label = z$id)
  }
})

test_that("die Schadholz-Welle ersetzt nur zusätzlich gefällte Bäume", {
  ohne <- herkunft_anteile(p0)
  mit <- herkunft_anteile(modifyList(p0, list(welle_zusatz = 80)))
  expect_equal(mit[, "kronen"], ohne[, "kronen"])
  expect_equal(mit[, "kaskade"], ohne[, "kaskade"])
  expect_true(all(mit[, "frisch"] >= 0))
})
