library(testthat)

# Aufruf aus dem App-Ordner: testthat::test_file("tests/test-unsicherheit.R")
source(file.path("..", "R", "model.R"), encoding = "UTF-8")
source(file.path("..", "R", "unsicherheit.R"), encoding = "UTF-8")
register <- lies_register(file.path("..", "parameter.csv"))
priors <- priors_vorbereiten(lies_priors(file.path("..", "priors.csv")), register)
p0 <- standardwerte(register)
stetig <- which(priors$familie %in% c("beta", "gamma", "normal"))

test_that("jede Annahme hat genau eine Verteilung, und nur konjugierte Familien kommen vor", {
  expect_setequal(priors$id, register$id)
  expect_false(any(duplicated(priors$id)))
  expect_true(all(priors$familie %in% FAMILIEN))
})

test_that("der Mittelwert jeder Verteilung ist der Wert des Reglers, auch nach dem Verstellen", {
  for (i in stetig) {
    pr <- priors[i, ]
    for (faktor in c(1, 0.7, 1.2)) {
      wert <- p0[[pr$id]] * faktor
      if (pr$familie == "beta" && wert > pr$skala) next
      par <- verteilung(pr$familie, wert / pr$skala, pr$staerke)
      mittel <- switch(pr$familie,
        beta = par$alpha / (par$alpha + par$beta),
        gamma = par$form / par$rate,
        normal = par$mittel
      )
      expect_equal(mittel * pr$skala, wert, tolerance = 1e-10, label = pr$id)
    }
  }
})

test_that("die Stärke trifft die Literatur-Perzentile so gut wie möglich", {
  for (i in stetig) {
    pr <- priors[i, ]
    m <- p0[[pr$id]] / pr$skala
    abstand <- function(s) abstand_literatur(pr$familie, m, s, pr$a / pr$skala, pr$b / pr$skala)
    expect_lte(abstand(pr$staerke), abstand(pr$staerke * 0.9) + 1e-12, label = pr$id)
    expect_lte(abstand(pr$staerke), abstand(pr$staerke * 1.1) + 1e-12, label = pr$id)
  }
  # Normalverteilung: 90-%-Intervall so breit wie in der Literatur
  pr <- priors[priors$id == "vorrat_heute", ]
  q <- quantil_prior(pr, p0, c(0.05, 0.95))
  expect_equal(diff(q), pr$b - pr$a, tolerance = 1e-8)
})

test_that("Verstellen verschiebt die Verteilung, die Stärke bleibt gleich (Beispiel Wärmeverluste)", {
  pr <- priors[priors$id == "netz_verlust", ]
  vorher <- verteilung("beta", 0.08, pr$staerke)
  nachher <- verteilung("beta", 0.12, pr$staerke)
  expect_equal(vorher$alpha + vorher$beta, nachher$alpha + nachher$beta)
  z <- prior_ziehen(priors, modifyList(p0, list(netz_verlust = 12)), 4000)
  expect_equal(mean(z[, "netz_verlust"]), 12, tolerance = 0.03)
})

test_that("ein Regler am Rand ergibt eine Annahme ohne Streuung", {
  z <- prior_ziehen(priors, modifyList(p0, list(verj_minderung = 0, netz_bau = 0)), 50)
  expect_equal(unname(z[, "verj_minderung"]), rep(0, 50))
  expect_equal(unname(z[, "netz_bau"]), rep(0, 50))
})

test_that("Waldszenarien: jede Annahme einzeln am 5. und 95. Perzentil, die übrigen wie eingestellt", {
  s <- wald_szenarien(p0, priors)
  expect_equal(vapply(s, `[[`, "", "id"), rep(WALD_ANNAHMEN, each = 2))
  v <- s[vapply(s, `[[`, "", "id") == "vorrat_max"]
  pr <- priors[priors$id == "vorrat_max", ]
  expect_equal(c(v[[1]]$wert, v[[2]]$wert), quantil_prior(pr, p0, c(0.05, 0.95), priors))
  expect_equal(v[[1]]$anteil, frischholz_alter(modifyList(p0, list(vorrat_max = v[[1]]$wert))))
  # tiefere Wachstumsgrenze: der ungenutzte Wald wächst bald kaum mehr, der genutzte holt schneller auf
  expect_true(all(v[[1]]$anteil[-1] < v[[2]]$anteil[-1]))
  # höhere Störungsverluste: der dichte ungenutzte Wald verliert mehr, der genutzte holt schneller auf
  st <- s[vapply(s, `[[`, "", "id") == "stoerung"]
  expect_true(all(st[[2]]$anteil[-1] < st[[1]]$anteil[-1]))
})

test_that("Spannweite der Waldantwort: ohne Streuung überall der eingestellte Wert", {
  z <- prior_ziehen(priors, p0, 200)
  q <- wald_spannweite(p0, z)
  expect_equal(q$alter, WALD_ALTER)
  expect_true(all(q$q2.5 <= q$q17 & q$q17 <= q$q50 & q$q50 <= q$q83 & q$q83 <= q$q97.5))
  fest <- z[1:3, , drop = FALSE]
  for (id in colnames(fest)) fest[, id] <- p0[[id]]
  q <- wald_spannweite(p0, fest)
  for (spalte in c("q2.5", "q50", "q97.5")) expect_equal(q[[spalte]], frischholz_alter(p0))
})

test_that("verstellt man einen Regler, ändern sich nur die Ziehungen dieser Annahme", {
  a <- prior_ziehen(priors, p0, 100)
  b <- prior_ziehen(priors, modifyList(p0, list(netz_verlust = 12)), 100)
  andere <- setdiff(colnames(a), "netz_verlust")
  expect_equal(a[, andere], b[, andere])
  expect_true(all(b[, "netz_verlust"] > a[, "netz_verlust"]))
})

test_that("gezogene Herkunftsanteile ergeben 100 % und haben die Regler als Erwartungswert", {
  p <- modifyList(p0, list(anteil_frisch = 70, anteil_kronen = 10, anteil_totholz = 15,
                           anteil_kaskade = 5))
  z <- prior_ziehen(priors, p, 4000)
  d <- priors$id[priors$familie == "dirichlet"]
  expect_equal(unname(rowSums(z[, d])), rep(100, 4000))
  expect_equal(unname(colMeans(z[, d])), c(70, 10, 15, 5), tolerance = 0.05)
})

test_that("Ziehungen sind mit gleichem Startwert reproduzierbar und verändern den globalen Zufall nicht", {
  set.seed(1)
  vorher <- runif(1)
  set.seed(1)
  a <- prior_ziehen(priors, p0, 10, seed = 5)
  nachher <- runif(1)
  b <- prior_ziehen(priors, p0, 10, seed = 5)
  expect_equal(a, b)
  expect_equal(vorher, nachher)
})

test_that("feste Grössen kommen aus den Reglern: doppelte Wärme ergibt doppelte Emissionen", {
  m1 <- mc_rechnen(p0, priors, n = 20)
  m2 <- mc_rechnen(modifyList(p0, list(q_del = 2 * p0$q_del)), priors, n = 20)
  expect_equal(m2$kumuliert, 2 * m1$kumuliert)
})

test_that("auf mehreren Kernen ergibt sich dasselbe wie auf einem", {
  skip_if(parallel::detectCores() < 4)
  alt <- setwd("..")
  cl <- mc_cluster(2)
  setwd(alt)
  on.exit(parallel::stopCluster(cl))
  einzeln <- mc_rechnen(p0, priors, n = 30)
  parallel <- mc_rechnen(p0, priors, n = 30, cl = cl)
  expect_equal(parallel$kumuliert, einzeln$kumuliert)
})

test_that("die Übersicht zeigt jede Annahme einmal, mit dem Regler als Mittelwert", {
  p <- modifyList(p0, list(netz_verlust = 12))
  t <- prior_tabelle(priors, register, p)
  expect_equal(nrow(t), nrow(register))
  expect_equal(t$Annahme, register$label)
  expect_false(any(is.na(t$Verteilung)))
  expect_equal(t$Mittelwert[register$id == "netz_verlust"], "12")
  expect_equal(t$Mittelwert[register$id == "anteil_frisch"], "50")
})

test_that("Perzentile werden in kt je Variante und Jahr ausgewertet", {
  mc <- list(kumuliert = array(0, c(4, length(VARIANTEN), JAHRE), dimnames = list(NULL, VARIANTEN, NULL)))
  mc$kumuliert[, "Plan IWB", ] <- c(1, 2, 3, 4) * 1000
  q <- mc_quantile(mc, jahre = 10, wk = 0.5)
  expect_equal(q$q50[q$variante == "Plan IWB"], 2.5)
})

test_that("das Band der Verlaufsgrafik deckt jedes Jahr ab und schliesst die Rechnung mit Standardwerten ein", {
  mc <- mc_rechnen(p0, priors, n = 200)
  band <- mc_quantile(mc, jahre = seq_len(JAHRE))
  expect_equal(nrow(band), JAHRE * length(VARIANTEN))
  expect_named(band, c("variante", "jahr", "q2.5", "q17", "q50", "q83", "q97.5"))
  expect_true(all(band$q2.5 <= band$q17 & band$q17 <= band$q83 & band$q83 <= band$q97.5))
  standard <- rechne_kumuliert(p0) / 1000
  innerhalb <- with(band, standard[cbind(match(variante, VARIANTEN), jahr)] >= q2.5 &
                      standard[cbind(match(variante, VARIANTEN), jahr)] <= q97.5)
  expect_true(all(innerhalb))
})

test_that("der Einfluss lässt Annahmen ohne Streuung weg", {
  mc <- mc_rechnen(modifyList(p0, list(verj_minderung = 0)), priors, n = 50)
  e <- mc_einfluss(mc)
  expect_false("verj_minderung" %in% e$id)
  expect_false(any(is.na(e$rho)))
})
