# Unsicherheit: Verteilungen der Annahmen (priors.csv) und Monte-Carlo-Rechnung.
#
# Der Regler legt den Mittelwert einer Verteilung fest, die Literatur ihre Stärke:
#   beta      Anteile und Wirkungsgrade: Beta(m ν, (1 - m) ν), Konzentration ν
#   gamma     positive Grössen: Gamma(k, k / m), Form k (relative Streuung 1 / √k)
#   normal    Grössen, die auch negativ sein können oder eng um einen Messwert liegen: N(m, σ²)
#   dirichlet Herkunftsanteile, die zusammen 100 % ergeben: Dir(κ s), Konzentration κ
#   fest      Bezugsgrössen, Variantendefinitionen und physikalische Konstanten: Wert des Reglers
# Für beta, gamma und normal sind a und b das 5. und 95. Perzentil aus der Literatur (Einheit des
# Reglers); die Stärke wird so gewählt, dass die Verteilung mit dem Standardwert als Mittelwert
# diese Perzentile möglichst gut trifft. Für dirichlet ist b die Konzentration κ.
# Werte in % werden für beta und dirichlet durch 100 geteilt.

FAMILIEN <- c("beta", "gamma", "normal", "dirichlet", "fest")

lies_priors <- function(pfad) {
  pr <- read.csv(pfad, fileEncoding = "UTF-8", stringsAsFactors = FALSE, na.strings = "")
  pr$begruendung[is.na(pr$begruendung)] <- ""
  pr
}

in_prozent <- function(einheit) grepl("%", einheit, fixed = TRUE)

# Parameter der Verteilung mit Mittelwert m (auf der Skala der Verteilung) und gegebener Stärke.
verteilung <- function(familie, m, staerke) {
  switch(familie,
    normal = list(mittel = m, sd = staerke),
    gamma = list(form = staerke, rate = staerke / m),
    beta = {
      m <- min(max(m, 0), 1)
      list(alpha = m * staerke, beta = (1 - m) * staerke)
    }
  )
}

quantil_verteilung <- function(familie, par, wk) {
  switch(familie,
    normal = qnorm(wk, par$mittel, par$sd),
    gamma = if (is.finite(par$rate)) qgamma(wk, par$form, par$rate) else 0 * wk,  # Mittelwert 0
    beta = qbeta(wk, par$alpha, par$beta)
  )
}

# Abstand der Perzentile von den Literaturwerten, für die Wahl der Stärke.
abstand_literatur <- function(familie, m, staerke, a, b) {
  sum((quantil_verteilung(familie, verteilung(familie, m, staerke), c(0.05, 0.95)) - c(a, b))^2)
}

staerke_anpassen <- function(familie, m, a, b) {
  if (familie == "normal") return((b - a) / (2 * qnorm(0.95)))
  bereich <- if (familie == "beta") c(0.5, 1e6) else c(0.05, 1e6)
  ziel <- function(l) abstand_literatur(familie, m, exp(l), a, b)
  exp(optimize(ziel, log(bereich), tol = 1e-8)$minimum)
}

# Ergänzt die Prior-Tabelle um Einheit, Skalenfaktor und die Stärke aus der Literatur.
priors_vorbereiten <- function(priors, register) {
  zeile <- match(priors$id, register$id)
  priors$einheit <- register$einheit[zeile]
  priors$skala <- ifelse(priors$familie %in% c("beta", "dirichlet") & in_prozent(priors$einheit), 100, 1)
  priors$staerke <- vapply(seq_len(nrow(priors)), function(i) {
    f <- priors$familie[i]
    s <- priors$skala[i]
    if (f %in% c("beta", "gamma", "normal")) {
      staerke_anpassen(f, register$wert[zeile[i]] / s, priors$a[i] / s, priors$b[i] / s)
    } else if (f == "dirichlet") {
      priors$b[i]
    } else {
      NA_real_
    }
  }, numeric(1))
  priors
}

# Erwartungswerte der Herkunftsanteile aus den Reglern (wie im Modell auf 1 skaliert).
dirichlet_mittel <- function(priors, p) {
  ids <- priors$id[priors$familie == "dirichlet"]
  s <- vapply(ids, function(id) p[[id]], numeric(1))
  if (sum(s) > 0) s / sum(s) else replace(0 * s, 1, 1)
}

# Perzentile einer Annahme beim aktuellen Wert des Reglers, in der Einheit des Reglers.
quantil_prior <- function(pr, p, wk, priors = NULL) {
  if (pr$familie == "dirichlet") {
    m <- dirichlet_mittel(priors, p)[[pr$id]]
    return(qbeta(wk, pr$staerke * m, pr$staerke * (1 - m)) * pr$skala)
  }
  par <- verteilung(pr$familie, p[[pr$id]] / pr$skala, pr$staerke)
  quantil_verteilung(pr$familie, par, wk) * pr$skala
}

kurz <- function(x) {
  text <- trimws(formatC(signif(x, 3), format = "fg", digits = 3, big.mark = "'"))
  ifelse(x != 0 & abs(x) < 1e-4, "≈ 0", text)
}

# Übersicht für die App: Verteilung beim aktuellen Wert, 90-%-Intervall und Literaturwerte,
# in Reihenfolge der Regler.
prior_tabelle <- function(priors, register, p) {
  zeile <- function(i) {
    pr <- priors[i, ]
    wert <- p[[pr$id]]
    if (pr$familie == "fest") return(c("fest", kurz(wert), "–", "–"))
    q <- quantil_prior(pr, p, c(0.05, 0.95), priors)
    intervall <- paste(kurz(q[1]), "–", kurz(q[2]))
    if (pr$familie == "dirichlet") {
      m <- dirichlet_mittel(priors, p)[[pr$id]]
      return(c(sprintf("Dirichlet(κ = %s)", kurz(pr$staerke)), kurz(m * pr$skala), intervall, "–"))
    }
    par <- verteilung(pr$familie, wert / pr$skala, pr$staerke)
    verteilung_text <- switch(pr$familie,
      beta = sprintf("Beta(α = %s, β = %s)", kurz(par$alpha), kurz(par$beta)),
      gamma = sprintf("Gamma(k = %s, ν = %s)", kurz(par$form), kurz(par$rate)),
      normal = sprintf("Normal(μ = %s, σ = %s)", kurz(par$mittel), kurz(par$sd))
    )
    c(verteilung_text, kurz(wert), intervall, paste(kurz(pr$a), "–", kurz(pr$b)))
  }
  reihenfolge <- match(register$id, priors$id)
  werte <- t(vapply(reihenfolge, zeile, character(4)))
  data.frame(
    Annahme = register$label,
    Einheit = register$einheit,
    Verteilung = werte[, 1],
    Mittelwert = werte[, 2],
    "90-%-Intervall" = werte[, 3],
    "Literatur (5.–95. Perzentil)" = werte[, 4],
    Begründung = priors$begruendung[reihenfolge],
    check.names = FALSE
  )
}

# n Ziehungen aller unsicheren Annahmen um die Werte der Regler, in den Einheiten der Regler
# (Matrix, Spalten = Parameter-ids). Die Zufallszahlen hängen nur vom Startwert ab: Verstellt man
# einen Regler, ändert sich nur die Spalte dieser Annahme.
prior_ziehen <- function(priors, p, n, seed = 2028) {
  unsicher <- priors[priors$familie != "fest", ]
  if (exists(".Random.seed", envir = globalenv())) {
    alt <- get(".Random.seed", envir = globalenv())
    on.exit(assign(".Random.seed", alt, envir = globalenv()))
  }
  set.seed(seed)
  u <- matrix(runif(n * nrow(unsicher)), n, dimnames = list(NULL, unsicher$id))

  z <- u
  for (i in which(unsicher$familie != "dirichlet")) {
    pr <- unsicher[i, ]
    z[, pr$id] <- quantil_prior(pr, p, u[, pr$id])
  }
  d <- unsicher[unsicher$familie == "dirichlet", ]
  if (nrow(d) > 0) {
    alpha <- d$staerke[1] * dirichlet_mittel(priors, p)
    g <- vapply(seq_along(alpha), function(j) qgamma(u[, d$id[j]], alpha[j]), numeric(n))
    z[, d$id] <- g / rowSums(g) * d$skala[1]
  }
  z
}

# Rechner für die Monte-Carlo-Rechnung: mehrere R-Prozesse, falls der Rechner mehrere Kerne hat.
mc_cluster <- function(anzahl = min(12, parallel::detectCores() - 2)) {
  if (is.na(anzahl) || anzahl < 2) return(NULL)
  cl <- parallel::makePSOCKcluster(anzahl)
  parallel::clusterCall(cl, function(pfad) {
    source(pfad, encoding = "UTF-8")
    NULL
  }, normalizePath(file.path("R", "model.R")))
  cl
}

mc_ziehungen_rechnen <- function(zeilen, p, z) {
  lapply(zeilen, function(i) {
    q <- p
    q[colnames(z)] <- as.list(z[i, ])
    rechne_kumuliert(q)
  })
}

# Monte-Carlo-Rechnung: feste Grössen aus p, unsichere aus ihren Verteilungen um die Werte in p.
# Ergebnis: kumulierte t CO2e als Feld [Ziehung, Variante, Jahr].
mc_rechnen <- function(p, priors, n = 1000, seed = 2028, fortschritt = NULL, cl = NULL) {
  z <- prior_ziehen(priors, p, n, seed)
  if (is.null(cl)) {
    ergebnis <- vector("list", n)
    for (i in seq_len(n)) {
      ergebnis[[i]] <- mc_ziehungen_rechnen(i, p, z)[[1]]
      if (!is.null(fortschritt) && i %% 50 == 0) fortschritt(i / n)
    }
  } else {
    teile <- split(seq_len(n), cut(seq_len(n), 2 * length(cl), labels = FALSE))
    ergebnis <- do.call(c, parallel::parLapplyLB(cl, teile, mc_ziehungen_rechnen, p = p, z = z))
    if (!is.null(fortschritt)) fortschritt(1)
  }
  kumuliert <- array(0, c(n, length(VARIANTEN), JAHRE), dimnames = list(NULL, VARIANTEN, NULL))
  for (i in seq_len(n)) kumuliert[i, , ] <- ergebnis[[i]]
  list(ziehungen = z, kumuliert = kumuliert)
}

# Perzentile der kumulierten Emissionen (kt) je Variante und Horizont; Standard: Median sowie die
# Grenzen der mittleren 66 % und 95 %. Spalten q2.5, q17, q50, q83, q97.5.
INTERVALLE <- c(0.025, 0.17, 0.5, 0.83, 0.975)

mc_quantile <- function(mc, jahre = HORIZONTE, wk = INTERVALLE) {
  do.call(rbind, lapply(jahre, function(j) {
    x <- matrix(mc$kumuliert[, , j], ncol = length(VARIANTEN))
    q <- do.call(rbind, lapply(seq_along(VARIANTEN), function(k) quantile(x[, k], wk, names = FALSE))) / 1000
    colnames(q) <- paste0("q", 100 * wk)
    data.frame(variante = VARIANTEN, jahr = j, q, row.names = NULL)
  }))
}

# Reiter «Holz & Wald»: die Annahmen mit dem grössten Einfluss darauf, wie schnell der Wald nach
# dem Fällen aufholt. Jede wird einzeln auf das 5. und das 95. Perzentil ihrer Verteilung gesetzt,
# die übrigen bleiben beim Wert des Reglers.
WALD_ANNAHMEN <- c("stoerung", "anteil_ungenutzt", "auslichtung", "verj_minderung", "eingriff", "vorrat_max")

wald_szenarien <- function(p, priors) {
  unlist(lapply(WALD_ANNAHMEN, function(id) {
    werte <- quantil_prior(priors[priors$id == id, ], p, c(0.05, 0.95), priors)
    lapply(werte, function(w) {
      list(id = id, wert = w, anteil = frischholz_alter(modifyList(p, setNames(list(w), id))))
    })
  }), recursive = FALSE)
}

# Dasselbe für alle Ziehungen z der Monte-Carlo-Rechnung: Perzentile des Anteils je Alter.
wald_spannweite <- function(p, z, wk = INTERVALLE) {
  a <- vapply(seq_len(nrow(z)), function(i) {
    q <- p
    q[colnames(z)] <- as.list(z[i, ])
    frischholz_alter(q)
  }, numeric(length(WALD_ALTER)))
  q <- t(apply(a, 1, quantile, wk, names = FALSE))
  colnames(q) <- paste0("q", 100 * wk)
  data.frame(alter = WALD_ALTER, q)
}

# Rangkorrelation (Spearman) jeder unsicheren Annahme mit den kumulierten Emissionen einer Variante.
# Annahmen ohne Streuung (Regler am Rand) fallen weg.
mc_einfluss <- function(mc, variante = "Plan IWB", jahre = c(20, 50)) {
  z <- mc$ziehungen[, apply(mc$ziehungen, 2, function(x) diff(range(x)) > 0), drop = FALSE]
  do.call(rbind, lapply(jahre, function(j) {
    y <- mc$kumuliert[, variante, j]
    rho <- apply(z, 2, function(x) cor(x, y, method = "spearman"))
    data.frame(id = colnames(z), jahr = j, rho = unname(rho))
  }))
}
