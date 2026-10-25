# Referenzwerte aus dem R-Modell für die Tests der JavaScript-Version (web/tests/).
#
# Aufruf aus dem App-Ordner: Rscript web/referenz.R

if (!file.exists("parameter.csv")) stop("Bitte aus dem App-Ordner aufrufen: Rscript web/referenz.R")

source("R/model.R", encoding = "UTF-8")
source("R/unsicherheit.R", encoding = "UTF-8")
source("R/regler.R", encoding = "UTF-8")
source("R/grafiken.R", encoding = "UTF-8")   # zahl()

register <- lies_register("parameter.csv")
priors <- priors_vorbereiten(lies_priors("priors.csv"), register)
p0 <- standardwerte(register)

faelle <- list(
  standard = list(),
  dezentral_ersatz = list(fossil_dezentral = 1, fossil_ersatz = 12),
  welle = list(welle_zusatz = 40, welle_jahre = 8, anteil_frisch = 30, anteil_kronen = 30,
               anteil_totholz = 30, anteil_kaskade = 10),
  wald_extrem = list(vorrat_heute = 321.4, vorrat_max = 592.3, zuwachs = 2.24, theta = 3.19,
                     eingriff = 46.2, stoerung = 0.442, klima_zuwachs = -15.3, verj_minderung = 5.3,
                     anteil_ungenutzt = 0),
  vorrat_nahe = list(vorrat_max = 350),
  ohne_holz = list(plan_holz = 0),
  erd_ohne_spitze = list(erd_oel = 0),
  boden_dauerhaft = list(boden_erholung = 0, boden_verlust = 8),
  gwp20 = list(gwp_ch4 = 82.5, gas_leck = 2),
  nur_kaskade = list(anteil_frisch = 0, anteil_kronen = 0, anteil_totholz = 0, anteil_kaskade = 100)
)

als_liste <- function(x) setNames(as.list(unname(x)), names(x))

modell <- lapply(faelle, function(aenderung) {
  p <- modifyList(p0, aenderung)
  e <- rechne_alles(p)
  jaehrlich <- lapply(setNames(VARIANTEN, VARIANTEN), function(v) {
    lapply(setNames(KOMPONENTEN, KOMPONENTEN), function(k) {
      e$jahre$jaehrlich[e$jahre$variante == v & e$jahre$komponente == k]
    })
  })
  kurven <- lapply(split(e$holz$kurven, e$holz$kurven$art), function(d) d$anteil)
  list(
    parameter = p,
    kumuliert = lapply(setNames(VARIANTEN, VARIANTEN), function(v) rechne_kumuliert(p)[v, ]),
    jaehrlich = jaehrlich,
    brutto = e$brutto$kumuliert,
    holz = list(m3 = e$holz$m3, srm = e$holz$srm, lkw_pro_jahr = e$holz$lkw_pro_jahr,
                kamin_g_kwh = e$holz$kamin_g_kwh, schwellen = als_liste(e$holz$schwellen),
                ab_jahr = als_liste(e$holz$ab_jahr), schnitt = e$holz$schnitt$anteil,
                kurven = kurven)
  )
})

# Jeder Regler auf Minimum und Maximum; bedingte Regler mit eingeschalteter Bedingung
bedingung <- list(welle_jahre = list(welle_zusatz = 40), jaz_luft = list(fossil_ersatz = 20))
extreme <- list()
for (i in which(register$typ != "fest")) {
  z <- register[i, ]
  werte <- switch(z$typ,
    auswahl = as.numeric(sub(".*=", "", strsplit(z$auswahl, "|", fixed = TRUE)[[1]])),
    switch = c(0, 1),
    c(z$min, z$max))
  for (w in range(werte)) {
    aenderung <- modifyList(bedingung[[z$id]] %||% list(), setNames(list(w), z$id))
    p <- modifyList(p0, aenderung)
    h <- rechne_alles(p)$holz
    k <- rechne_kumuliert(p)
    extreme[[length(extreme) + 1]] <- list(
      id = z$id, aenderung = aenderung,
      kumuliert = lapply(setNames(VARIANTEN, VARIANTEN), function(v) unname(k[v, HORIZONTE])),
      holz = list(m3 = h$m3, srm = h$srm, lkw_pro_jahr = h$lkw_pro_jahr)
    )
  }
}

# Perzentile der Verteilungen beim Standardwert und bei verstellten Reglern
wk <- c(0.001, 0.025, 0.17, 0.5, 0.83, 0.975, 0.999)
perzentile <- list()
for (i in seq_len(nrow(priors))) {
  pr <- priors[i, ]
  if (pr$familie == "fest") next
  for (faktor in c(1, 0.6, 1.3)) {
    p <- p0
    wert <- p0[[pr$id]] * faktor
    if (pr$familie == "beta" && wert > pr$skala) next
    p[[pr$id]] <- wert
    perzentile[[length(perzentile) + 1]] <- list(id = pr$id, wert = wert, wk = wk,
                                                 q = quantil_prior(pr, p, wk, priors))
  }
}
roh <- list(
  gamma = lapply(c(0.0877, 0.5, 1, 2.5, 97.6), function(k) list(form = k, wk = wk, q = qgamma(wk, k))),
  beta = lapply(list(c(0.0872, 174), c(1505, 1505), c(5.29, 60.8), c(0.128, 2.44), c(0.5, 9.5)),
                function(ab) list(a = ab[1], b = ab[2], wk = wk, q = qbeta(wk, ab[1], ab[2]))),
  normal = list(wk = wk, q = qnorm(wk))
)

# Modell auf Ziehungen aus R: dieselben Parameter müssen dieselben Emissionen ergeben
z <- prior_ziehen(priors, p0, 25)
ziehungen <- lapply(seq_len(nrow(z)), function(i) {
  q <- p0
  q[colnames(z)] <- as.list(z[i, ])
  list(parameter = as.list(z[i, ]),
       kumuliert = lapply(setNames(VARIANTEN, VARIANTEN), function(v) rechne_kumuliert(q)[v, ]))
})

# Monte-Carlo-Zusammenfassung (andere Zufallszahlen als in JavaScript: nur statistisch vergleichbar)
cl <- mc_cluster()
mc <- mc_rechnen(p0, priors, n = 1000, cl = cl)
if (!is.null(cl)) parallel::stopCluster(cl)
mc_q <- mc_quantile(mc)
mittel <- vapply(colnames(mc$ziehungen), function(id) mean(mc$ziehungen[, id]), numeric(1))

# Umverteilen der Anteils-Regler
umverteilen_faelle <- list(
  list(werte = c(frisch = 50, kronen = 25, totholz = 20, kaskade = 5), geaendert = "totholz",
       neu = 100, maximum = rep(100, 4), schritt = 5),
  list(werte = c(frisch = 50, kronen = 25, totholz = 20, kaskade = 5), geaendert = "frisch",
       neu = 70, maximum = rep(100, 4), schritt = 5),
  list(werte = c(frisch = 80, kronen = 0, totholz = 0, kaskade = 0), geaendert = "frisch",
       neu = 80, maximum = rep(100, 4), schritt = 5),
  list(werte = c(holz = 53, wrg = 11, wp = 28, oel = 8), geaendert = "holz", neu = 0,
       maximum = c(100, 30, 100, 30), schritt = 1),
  list(werte = c(holz = 53, wrg = 11, wp = 28, oel = 8), geaendert = "wp", neu = 47,
       maximum = c(100, 30, 100, 30), schritt = 1),
  list(werte = c(a = 50, b = 25, c = 25), geaendert = "a", neu = 20, maximum = c(100, 30, 30), schritt = 1)
)
umverteilen_faelle <- lapply(umverteilen_faelle, function(f) {
  w <- f$werte
  w[[f$geaendert]] <- f$neu
  f$ergebnis <- als_liste(umverteilen(w, f$geaendert, f$maximum, f$schritt))
  f$werte <- als_liste(f$werte)
  f
})

zahlen <- c(0, 0.000012, 0.0228, 0.15, 1.5, 2.78, 9.19, 12, 81.7, 145, 346, 9500, 12345.6, -27.5)
formate <- list(
  zahlen = zahlen,
  kurz = kurz(zahlen),
  zahl = lapply(c(-2, -1, 0, 1), function(s) list(stellen = s, text = zahl(zahlen, s)))
)

p_tabelle <- modifyList(p0, list(netz_verlust = 12, anteil_frisch = 70, anteil_kronen = 15,
                                 anteil_totholz = 10, anteil_kaskade = 5, verj_minderung = 0))
tabellen <- list(
  standard = prior_tabelle(priors, register, p0),
  verstellt = list(parameter = p_tabelle, tabelle = prior_tabelle(priors, register, p_tabelle))
)

spearman <- list(x = c(1, 2, 2, 3, 5, 5, 5, 8), y = c(3, 1, 4, 1, 5, 9, 2, 6))
spearman$rho <- cor(spearman$x, spearman$y, method = "spearman")
quantil7 <- list(x = c(4.2, 1, 9, 3.3, 7, 7, 2), wk = c(0.025, 0.17, 0.5, 0.83, 0.975))
quantil7$q <- quantile(quantil7$x, quantil7$wk, names = FALSE)

referenz <- list(
  modell = modell,
  extreme = extreme,
  perzentile = perzentile,
  roh = roh,
  ziehungen = ziehungen,
  mc = list(quantile = mc_q, mittel = als_liste(mittel)),
  umverteilen = umverteilen_faelle,
  formate = formate,
  tabellen = tabellen,
  spearman = spearman,
  quantil7 = quantil7
)
dir.create(file.path("web", "tests"), recursive = TRUE, showWarnings = FALSE)
datei <- file(file.path("web", "tests", "referenz.json"), open = "w", encoding = "UTF-8")
writeLines(enc2utf8(jsonlite::toJSON(referenz, auto_unbox = TRUE, digits = NA, na = "null",
                                     dataframe = "rows")), datei)
close(datei)
cat("web/tests/referenz.json geschrieben\n")
