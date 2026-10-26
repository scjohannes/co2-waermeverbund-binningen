# CO2e-Modell für den Wärmeverbund Binningen.
# Energie in MWh, Emissionen in t CO2e, Zeit in Betriebsjahren ab Inbetriebnahme.
# Gezählt wird, wie viel CO2e jede Variante bis zu einem Zeitpunkt zusätzlich in die
# Atmosphäre gebracht hat. Beim Holz heisst das: Kamin-CO2 minus das, was der Wald
# inzwischen nachgewachsen ist bzw. was ohnehin verrottet wäre.

JAHRE <- 50L
START <- 2028L
HORIZONTE <- seq(10, 50, 10)

VARIANTEN <- c("Plan IWB", "Plan, Öl statt Holz", "Erdwärmesonden + Öl", "Erdwärmesonden",
               "100 % Gas", "100 % Öl")

KOMPONENTEN <- c(
  "Öl/Gas: Verbrennung",
  "Vorketten & Nebengase",
  "Strom",
  "Bau",
  "Holz: CO2 netto (Wald)"
)

HOLZARTEN <- c(
  frisch  = "Zusätzlich gefällte lebende Bäume",
  kronen  = "Äste und Kronen (sonst verrottet)",
  totholz = "Liegendes Schadholz (sonst verrottet)",
  kaskade = "Holz, das sonst zu Produkten würde"
)

# Methan je kWh Erdgas (Heizwert rund 50 MJ/kg): 3,6 MJ / 50 MJ/kg = 72 g
METHAN_G_PRO_KWH <- 72

# Die Wuchsminderung nach dem Eingriff (verj_minderung) gilt für einen Eingriff von 20 % des
# Vorrats; schwächere und stärkere Eingriffe mindern den Zuwachs im selben Verhältnis.
EINGRIFF_BEZUG <- 20

lies_register <- function(pfad) {
  reg <- read.csv(pfad, fileEncoding = "UTF-8", stringsAsFactors = FALSE, na.strings = "")
  for (spalte in c("einheit", "auswahl", "quelle")) {
    reg[[spalte]][is.na(reg[[spalte]])] <- ""
  }
  reg
}

standardwerte <- function(reg) as.list(setNames(reg$wert, reg$id))

# Methan aus dem Schnitzelhaufen: zusätzliche t CO2e je t biogenem CO2 des geernteten Holzes.
# Der Kohlenstoff ist im Kamin-CO2 schon als CO2 gezählt; hier kommt nur die Mehrwirkung als
# Methan dazu. Biogenes Methan hat ein um 2,8 tieferes GWP als fossiles (IPCC AR6).
lager_methan_faktor <- function(p) {
  12 / 44 * p$lager_methan / 100 * (16 / 12 * (p$gwp_ch4 - 2.8) - 44 / 12)
}

# Strom-Emissionen in t CO2e/MWh je Betriebsjahr: linear bis 2050, danach konstant.
strom_pfad <- function(p, n = JAHRE) {
  w <- pmin((seq_len(n) - 1) / (2050 - START), 1)
  (p$strom_start + (p$strom_2050 - p$strom_start) * w) / 1000
}

# Kamin-CO2 und Energieinhalt der Holzschnitzel.
holz_kennzahlen <- function(p) {
  u <- p$wassergehalt / 100
  hu <- p$hu_trocken - u / (1 - u) * 2.443                  # MJ je kg Trockenmasse, wie geliefert
  list(
    kamin = p$c_anteil / 100 * 44 / 12 / (hu / 3.6),         # t CO2 je MWh Brennstoff
    mwh_pro_m3 = p$dichte * hu / 3.6                         # MWh Brennstoff je m³ Holz
  )
}

# Zusätzlich gefällte lebende Bäume: Anteil des verbrannten CO2, der nach a Jahren noch
# zusätzlich in der Luft ist. Verglichen wird ein Dauerwald-Bestand ohne und mit Eingriff;
# der Zuwachs hängt von der Dichte ab (theta-logistisch). Ohne Heizwerk bleibt ein Teil
# der Fläche ungenutzt und wächst Richtung Maximum; der Rest wird wie bisher genutzt,
# sein Vorrat bleibt auf heutigem Stand, und die bisherige Nutzung läuft auch auf der
# Fläche mit Eingriff weiter. Zeilen: Erntejahr (Kohorte), Spalten: Jahre seit der Ernte.
antwort_frischholz <- function(p, n = JAHRE) {
  x  <- min(p$vorrat_heute / p$vorrat_max, 0.95)             # heutiger Vorrat / Maximum
  th <- p$theta
  g  <- p$zuwachs / 100
  d0 <- p$stoerung / 100
  h  <- p$eingriff / 100
  # r so gewählt, dass der Nettozuwachs nach Störungen beim heutigen Vorrat g beträgt
  r <- (g + d0) * th / (1 - x^th)
  wachstum <- function(B) r * B * (1 - (B * x)^th) / th      # B relativ zum heutigen Vorrat
  # Wuchsminderung nach dem Eingriff, im Verhältnis der Eingriffsstärke zu EINGRIFF_BEZUG
  minderung <- pmin(p$verj_minderung / 100 * p$eingriff / EINGRIFF_BEZUG, 1)

  w <- p$anteil_ungenutzt / 100
  ungenutzt <- rep(1, n)            # ohne Heizwerk, ohne Nutzung
  ungenutzt_eingriff <- rep(1 - h, n)
  bisher_eingriff <- rep(1 - h, n)  # mit Heizwerk, bisherige Nutzung läuft weiter
  differenz <- matrix(0, n, n)
  for (a in seq_len(n) - 1) {
    differenz[, a + 1] <- (w * (ungenutzt - ungenutzt_eingriff) + (1 - w) * (1 - bisher_eingriff)) / h
    klima <- 1 + p$klima_zuwachs / 100 * pmin((seq_len(n) - 1 + a) / JAHRE, 1)
    verjuengung <- if (a < p$verj_jahre) 1 - minderung else 1
    # bisherige Nutzung: entnimmt, was der Wald beim heutigen Vorrat netto zuwächst
    bisherige_nutzung <- klima * wachstum(1) - d0
    # ein Vorrat kann nicht unter null sinken
    ungenutzt <- pmax(ungenutzt + klima * wachstum(ungenutzt) - d0 * ungenutzt^3, 0)
    ungenutzt_eingriff <- pmax(ungenutzt_eingriff + klima * verjuengung * wachstum(ungenutzt_eingriff) -
                                 d0 * ungenutzt_eingriff^3, 0)
    bisher_eingriff <- pmax(bisher_eingriff + klima * verjuengung * wachstum(bisher_eingriff) -
                              d0 * bisher_eingriff^3 - bisherige_nutzung, 0)
  }

  # Verbrannt wird der oberirdische Teil; Stöcke und Wurzeln bleiben und zersetzen sich.
  rho <- p$wurzel_anteil / 100
  alter <- seq_len(n) - 1
  wurzeln_noch_im_boden <- rho / (1 - rho) * 2^(-alter / p$wurzel_hwz)
  # Bodenkohlenstoff: Verlust in den ersten 10 Jahren, danach Erholung um boden_erholung % des
  # Verlusts pro Jahr (0 = dauerhaft)
  erholung <- pmax(1 - p$boden_erholung / 100 * pmax(alter - 10, 0), 0)
  bodenverlust <- p$boden_verlust / 100 * pmin(alter / 10, 1) * erholung
  sweep(differenz / (1 - rho), 2, bodenverlust - wurzeln_noch_im_boden, "+")
}

# Restholz, das sonst im Wald verrotten würde: Anteil, der ohne Nutzung noch im Wald
# (oder dauerhaft im Boden) wäre und darum durch das Verbrennen zusätzlich in der Luft ist.
antwort_zerfall <- function(hwz, humus, n = JAHRE) {
  alter <- seq_len(n) - 1
  humus + (1 - humus) * 2^(-alter / hwz)
}

# Holz, das ohne Heizwerk zu Produkten würde: Ein Teil wäre als Bauholz oder Platten
# gespeichert geblieben und zerfällt mit deren Halbwertszeit; der Rest wäre ohnehin verbrannt.
antwort_kaskade <- function(p, n = JAHRE) {
  alter <- seq_len(n) - 1
  p$produkt_anteil / 100 * 2^(-alter / p$hwz_produkt)
}

# Herkunft der zusätzlichen Holzmenge je Betriebsjahr; jede Zeile summiert auf 1.
# Die Schadholz-Welle ersetzt nur zusätzlich gefällte Bäume, höchstens deren ganzen Anteil.
herkunft_anteile <- function(p, n = JAHRE) {
  s <- c(p$anteil_frisch, p$anteil_kronen, p$anteil_totholz, p$anteil_kaskade)
  s <- if (sum(s) > 0) s / sum(s) else c(1, 0, 0, 0)
  welle <- pmin(p$welle_zusatz / 100 * pmax(1 - (seq_len(n) - 1) / p$welle_jahre, 0), s[1])
  cbind(frisch = s[1] - welle, kronen = rep(s[2], n), totholz = s[3] + welle,
        kaskade = rep(s[4], n))
}

# Biogenes CO2, das am Ende jedes Betriebsjahrs noch zusätzlich in der Luft ist.
biogen_kumuliert <- function(kamin, anteile, wald) {
  vapply(seq_along(kamin), function(t) {
    jahrgang <- seq_len(t)
    alter <- t - jahrgang + 1
    f <- anteile[jahrgang, "frisch"] * wald$frisch[cbind(jahrgang, alter)] +
      anteile[jahrgang, "kronen"] * wald$kronen[alter] +
      anteile[jahrgang, "totholz"] * wald$totholz[alter] +
      anteile[jahrgang, "kaskade"] * wald$kaskade[alter]
    sum(kamin[jahrgang] * f)
  }, numeric(1))
}

# Anteile an der gelieferten Wärme je Betriebsjahr, und welche Teile über das Netz kommen.
# Spalten: Holzkessel, Wärmerückgewinnung, zentrale Wärmepumpe (Plan bzw. Erdwärmesonden),
# Öl- und Gaskessel, Luft-Wärmepumpen als Ersatz von Einzelheizungen, Spitzenlast-Wärmepumpen
# in den Häusern.
variante_def <- function(p, variante, n = JAHRE) {
  spalten <- c("holz", "wrg", "wp", "oel", "gas", "luft", "spitze")
  a <- matrix(0, n, length(spalten), dimnames = list(NULL, spalten))
  netz <- setNames(rep(TRUE, length(spalten)), spalten)
  jaz <- p$jaz_erdsonde
  if (variante %in% c("Plan IWB", "Plan, Öl statt Holz")) {
    s <- c(holz = p$plan_holz, wrg = p$plan_wrg, wp = p$plan_wp, oel = p$plan_oel)
    if (variante == "Plan, Öl statt Holz") s <- s + c(-s[["holz"]], 0, 0, s[["holz"]])
    a[, names(s)] <- matrix(s / max(sum(s), 1e-9), n, 4, byrow = TRUE)
    jaz <- p$jaz_plan
  } else if (variante == "Erdwärmesonden + Öl") {
    a[, "oel"] <- p$erd_oel / 100
    a[, "wp"] <- 1 - p$erd_oel / 100
  } else if (variante == "Erdwärmesonden") {
    a[, "spitze"] <- p$erd_oel / 100
    a[, "wp"] <- 1 - p$erd_oel / 100
    netz["spitze"] <- FALSE
  } else {
    ersetzt <- seq_len(n) > p$fossil_laufzeit   # danach Luft-Wärmepumpen
    a[, if (variante == "100 % Gas") "gas" else "oel"] <- !ersetzt
    a[, "luft"] <- ersetzt
    netz[] <- p$fossil_dezentral < 0.5
  }
  list(anteile = a, netz = netz, jaz = jaz)
}

# Biogenes CO2 einer Variante, das am Ende jedes Jahres noch zusätzlich in der Luft ist.
# Bei gleichbleibender Holzmenge genügt die vorberechnete Antwort auf 1 t pro Jahr.
biogen_variante <- function(kamin_holz, wald) {
  if (all(kamin_holz == 0)) return(numeric(length(kamin_holz)))
  if (all(kamin_holz == kamin_holz[1])) return(kamin_holz[1] * wald$einheit)
  biogen_kumuliert(kamin_holz, wald$anteile, wald)
}

# Jährliche Emissionen einer Variante, aufgeteilt in die KOMPONENTEN (Spalten, t CO2e).
variante_komponenten <- function(p, variante, wald, n = JAHRE) {
  d <- variante_def(p, variante, n)
  a <- d$anteile
  # erzeugte Wärme je Technik (MWh); was über das Netz geht, trägt dessen Verluste
  faktor <- p$q_del * ifelse(d$netz, 1 / (1 - p$netz_verlust / 100), 1)
  erzeugt <- sweep(a, 2, faktor, "*")
  ueber_netz <- p$q_del * rowSums(a[, d$netz, drop = FALSE])
  holz <- erzeugt[, "holz"] / (p$eta_holz / 100)             # MWh Brennstoff im Kessel
  geerntet <- holz / (1 - p$dm_verlust / 100)                # inklusive Verlust im Lager
  oel <- erzeugt[, "oel"] / (p$eta_oel / 100)
  gas <- erzeugt[, "gas"] / (p$eta_gas / 100)
  strom <- erzeugt[, "wp"] / d$jaz + erzeugt[, "luft"] / p$jaz_luft +
    erzeugt[, "wrg"] * p$wrg_strom + ueber_netz * p$netz_strom / 100   # MWh Strom
  strom_spitze <- erzeugt[, "spitze"] / p$jaz_spitze          # MWh Strom an Spitzentagen
  methan <- p$gas_leck / 100 * METHAN_G_PRO_KWH * p$gwp_ch4  # g CO2e je kWh Gas
  kamin_holz <- geerntet * wald$kamin                        # t biogenes CO2 aus Kamin und Lager

  # MWh mal g/kWh ergibt kg; geteilt durch 1000 ergibt t
  komp <- cbind(
    (oel * p$oel_direkt + gas * p$gas_direkt) / 1000,
    (oel * p$oel_vorkette + gas * (p$gas_vorkette + methan) +
       geerntet * (p$holz_vorkette + p$lager_n2o) + holz * p$holz_nonco2) / 1000 +
      kamin_holz * lager_methan_faktor(p),
    (strom + strom_spitze * (1 + p$strom_spitze / 100)) * wald$strom,
    ((erzeugt[, "wp"] + erzeugt[, "luft"] + erzeugt[, "spitze"]) * p$wp_bau +
       (erzeugt[, "holz"] + erzeugt[, "oel"] + erzeugt[, "gas"]) * p$fossil_bau +
       ueber_netz * p$netz_bau) / 1000,
    diff(c(0, biogen_variante(kamin_holz, wald)))
  )
  list(komp = komp, kamin_holz = kamin_holz, holz = holz, geerntet = geerntet)
}

rechne_variante <- function(p, variante, wald, n = JAHRE) {
  v <- variante_komponenten(p, variante, wald, n)
  komp <- v$komp
  list(
    daten = data.frame(
      variante = variante,
      jahr = rep(seq_len(n), times = length(KOMPONENTEN)),
      komponente = rep(KOMPONENTEN, each = n),
      jaehrlich = as.vector(komp),
      kumuliert = as.vector(apply(komp, 2, cumsum))
    ),
    brutto = cumsum(rowSums(komp[, 1:4, drop = FALSE]) + v$kamin_holz),
    holz_brennstoff = v$holz,
    holz_geerntet = v$geerntet
  )
}

# Kennzahlen zum Holz im Plan: Holzmenge, Kamin-CO2 und ab wann Holzwärme besser ist
# als Wärme aus Öl oder Gas (pro kWh aus dem Holzkessel, ohne Wärmerückgewinnung).
holz_diagnose <- function(p, hk, wald, plan, n = JAHRE) {
  m3_geerntet <- plan$holz_geerntet[1] / hk$mwh_pro_m3
  m3_geliefert <- plan$holz_brennstoff[1] / hk$mwh_pro_m3
  eta <- p$eta_holz / 100
  ernte <- 1 / (1 - p$dm_verlust / 100)
  kamin_w <- hk$kamin / eta * ernte                         # Kamin und Lager
  neben_w <- ((p$holz_vorkette + p$lager_n2o) * ernte + p$holz_nonco2) / 1000 / eta +
    kamin_w * lager_methan_faktor(p)
  oel_w <- (p$oel_direkt + p$oel_vorkette) / 1000 / (p$eta_oel / 100)
  gas_w <- (p$gas_direkt + p$gas_vorkette + p$gas_leck / 100 * METHAN_G_PRO_KWH * p$gwp_ch4) /
    1000 / (p$eta_gas / 100)
  schwellen <- c("100 % Öl" = (oel_w - neben_w) / kamin_w, "100 % Gas" = (gas_w - neben_w) / kamin_w)

  # Durchschnittlicher Anteil noch in der Luft bei gleichbleibendem Dauerbetrieb
  schnitt <- wald$einheit / seq_len(n)
  ab_jahr <- vapply(schwellen, function(s) which(schnitt <= s)[1], integer(1))

  einzeln <- rbind(wald$frisch[1, ], wald$kronen, wald$totholz, wald$kaskade)
  mischung <- drop(wald$anteile[1, ] %*% einzeln)
  kurven <- data.frame(
    alter = rep(seq_len(n) - 1, 5),
    art = rep(c(unname(HOLZARTEN), "Mischung im ersten Betriebsjahr"), each = n),
    anteil = c(t(einzeln), mischung)
  )

  list(
    m3 = m3_geerntet,
    srm = m3_geliefert * p$srm_faktor,
    lkw_pro_jahr = m3_geliefert * p$srm_faktor / p$lkw_srm,
    kamin_g_kwh = kamin_w * 1000,
    schwellen = schwellen,
    ab_jahr = ab_jahr,
    schnitt = data.frame(jahr = seq_len(n), anteil = schnitt),
    kurven = kurven
  )
}

# Alles, was für alle Varianten gleich ist: Holzkennzahlen, Strompfad, Waldantworten und
# das biogene CO2, das von 1 t CO2 pro Betriebsjahr am Ende jedes Jahres noch in der Luft ist.
wald_vorbereiten <- function(p, n = JAHRE) {
  hk <- holz_kennzahlen(p)
  wald <- list(
    kamin = hk$kamin,
    strom = strom_pfad(p, n),
    anteile = herkunft_anteile(p, n),
    frisch = antwort_frischholz(p, n),
    kronen = antwort_zerfall(p$hwz_kronen, p$humus_anteil / 100, n),
    totholz = antwort_zerfall(p$hwz_totholz, p$humus_anteil / 100, n),
    kaskade = antwort_kaskade(p, n)
  )
  wald$einheit <- biogen_kumuliert(rep(1, n), wald$anteile, wald)
  list(hk = hk, wald = wald)
}

# Schlanke Rechnung für viele Ziehungen: kumulierte t CO2e, Zeilen = Varianten, Spalten = Jahre.
rechne_kumuliert <- function(p, n = JAHRE) {
  wald <- wald_vorbereiten(p, n)$wald
  t(vapply(VARIANTEN, function(v) cumsum(rowSums(variante_komponenten(p, v, wald, n)$komp)),
           numeric(n)))
}

rechne_alles <- function(p, n = JAHRE) {
  vorbereitet <- wald_vorbereiten(p, n)
  hk <- vorbereitet$hk
  wald <- vorbereitet$wald
  ergebnisse <- setNames(lapply(VARIANTEN, function(v) rechne_variante(p, v, wald, n)), VARIANTEN)
  jahre <- do.call(rbind, lapply(ergebnisse, `[[`, "daten"))
  rownames(jahre) <- NULL
  summe <- aggregate(kumuliert ~ variante + jahr, data = jahre, FUN = sum)
  summe <- rbind(data.frame(variante = VARIANTEN, jahr = 0, kumuliert = 0), summe)
  plan <- ergebnisse[["Plan IWB"]]
  list(
    jahre = jahre,
    summe = summe,
    brutto = data.frame(jahr = 0:n, kumuliert = c(0, plan$brutto)),
    holz = holz_diagnose(p, hk, wald, plan, n)
  )
}
