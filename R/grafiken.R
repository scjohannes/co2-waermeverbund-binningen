# Grafiken für die App. Farben: Referenzpalette (feste Reihenfolge der Kategorien).

FARBEN_VARIANTEN <- setNames(c("#2a78d6", "#6250d6", "#eb6834", "#e87ba4", "#1baf7a", "#eda100"),
                             VARIANTEN)
FARBEN_KOMPONENTEN <- setNames(c("#2a78d6", "#eb6834", "#1baf7a", "#eda100", "#e87ba4"), KOMPONENTEN)
FARBEN_HOLZ <- setNames(c("#2a78d6", "#eb6834", "#1baf7a", "#eda100", "#0b0b0b"),
                        c(unname(HOLZARTEN), "Mischung im ersten Betriebsjahr"))

TINTE <- "#1c1c1e"
TINTE_2 <- "#52525b"
GEDAEMPFT <- "#8a8f98"
GITTER <- "#eceef2"
FLAECHE <- "#ffffff"

thema <- function() {
  theme_minimal(base_size = 14) +
    theme(
      plot.background = element_rect(fill = FLAECHE, colour = NA),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(colour = GITTER, linewidth = 0.4),
      axis.text = element_text(colour = GEDAEMPFT),
      axis.title = element_text(colour = TINTE_2, size = rel(0.85)),
      legend.position = "top",
      legend.justification = "left",
      legend.title = element_blank(),
      legend.text = element_text(colour = TINTE_2),
      strip.text = element_text(colour = TINTE, face = "bold", hjust = 0),
      plot.margin = margin(8, 12, 8, 8)
    )
}

zahl <- function(x, stellen = 0) {
  r <- round(x, stellen)
  r[r == 0] <- 0   # keine «-0»
  formatC(r, format = "f", digits = max(stellen, 0), big.mark = "'")
}

achse_betriebsjahre <- function() {
  scale_x_continuous(
    breaks = c(0, HORIZONTE),
    labels = function(x) paste0(x, " J.\n", START + x),
    expand = expansion(mult = c(0.01, 0.38))
  )
}

# Beschriftungen am Linienende so verschieben, dass sie sich nicht überlappen.
entzerren <- function(y, abstand) {
  o <- order(y)
  ys <- y[o]
  for (i in seq_along(ys)[-1]) ys[i] <- max(ys[i], ys[i - 1] + abstand)
  y[o] <- ys
  y
}

# Kumulierte Emissionen je Variante; band (optional): Perzentile in kt je Jahr (mc_quantile),
# kräftig die mittleren 66 %, blass die mittleren 95 %.
grafik_verlauf <- function(summe, band = NULL) {
  d <- summe
  d$kt <- d$kumuliert / 1000
  d$reihe <- factor(d$variante, levels = VARIANTEN)
  ende <- d[d$jahr == max(d$jahr), ]
  ende$y_label <- entzerren(ende$kt, diff(range(c(0, d$kt))) * 0.06)
  punkte <- d[d$jahr %in% HORIZONTE, ]

  g <- ggplot(d, aes(jahr, kt, colour = reihe))
  if (!is.null(band)) {
    spalten <- c("q2.5", "q17", "q83", "q97.5")
    start <- data.frame(variante = VARIANTEN, jahr = 0, matrix(0, length(VARIANTEN), 4,
                                                               dimnames = list(NULL, spalten)))
    b <- rbind(start, band[, c("variante", "jahr", spalten)])
    b$reihe <- factor(b$variante, levels = VARIANTEN)
    g <- g +
      geom_ribbon(data = b, aes(x = jahr, ymin = q2.5, ymax = q97.5, fill = reihe),
                  inherit.aes = FALSE, alpha = 0.05, show.legend = FALSE) +
      geom_ribbon(data = b, aes(x = jahr, ymin = q17, ymax = q83, fill = reihe),
                  inherit.aes = FALSE, alpha = 0.13, show.legend = FALSE) +
      scale_fill_manual(values = FARBEN_VARIANTEN)
  }
  g +
    geom_line(linewidth = 0.9) +
    geom_point(data = punkte, size = 2.4, show.legend = FALSE) +
    geom_text(data = ende, aes(x = jahr + 1, y = y_label, label = reihe), hjust = 0,
              size = 3.8, colour = TINTE_2, show.legend = FALSE) +
    scale_colour_manual(values = FARBEN_VARIANTEN) +
    achse_betriebsjahre() +
    scale_y_continuous(labels = function(x) zahl(x), expand = expansion(mult = c(0, 0.05)),
                       limits = c(0, NA)) +
    coord_cartesian(clip = "off") +
    labs(x = NULL, y = "kt CO2e (kumuliert)") +
    thema() +
    theme(legend.position = "none")
}

grafik_balken <- function(jahre) {
  d <- jahre[jahre$jahr %in% HORIZONTE, ]
  d$kt <- d$kumuliert / 1000
  d$horizont <- factor(paste0(d$jahr, " J. (", START + d$jahr, ")"),
                       levels = paste0(HORIZONTE, " J. (", START + HORIZONTE, ")"))
  d$variante <- factor(d$variante, levels = rev(VARIANTEN))
  d$komponente <- factor(d$komponente, levels = rev(KOMPONENTEN))
  summe <- aggregate(kt ~ variante + horizont, data = d, FUN = sum)
  summe$text <- vapply(summe$kt, function(x) zahl(x, if (x < 10) 1 else 0), character(1))

  ggplot(d, aes(kt, variante, fill = komponente)) +
    geom_col(width = 0.68, colour = FLAECHE, linewidth = 0.4) +
    geom_text(data = summe, aes(kt, variante, label = text), inherit.aes = FALSE,
              hjust = -0.15, size = 3.3, colour = TINTE_2) +
    facet_grid(. ~ horizont, scales = "free_x") +
    scale_fill_manual(values = FARBEN_KOMPONENTEN, breaks = KOMPONENTEN, drop = FALSE) +
    scale_x_continuous(expand = expansion(mult = c(0, 0.45)), labels = function(x) zahl(x),
                       n.breaks = 3) +
    guides(fill = guide_legend(nrow = 2)) +
    coord_cartesian(clip = "off") +
    labs(x = "kt CO2e (kumuliert)", y = NULL) +
    thema() +
    theme(panel.grid.major.y = element_blank())
}

grafik_antwort <- function(kurven) {
  d <- kurven
  d$art <- factor(d$art, levels = names(FARBEN_HOLZ))
  ggplot(d, aes(alter, anteil, colour = art, linewidth = art)) +
    geom_hline(yintercept = 0, colour = "#c3c2b7", linewidth = 0.4) +
    geom_line() +
    scale_colour_manual(values = FARBEN_HOLZ) +
    scale_linewidth_manual(values = c(0.8, 0.8, 0.8, 0.8, 1.4), guide = "none") +
    scale_y_continuous(labels = function(x) paste0(round(100 * x), " %")) +
    expand_limits(y = 0) +
    scale_x_continuous(breaks = c(0, HORIZONTE)) +
    labs(x = "Jahre seit dem Verbrennen", y = "Anteil noch zusätzlich in der Luft") +
    guides(colour = guide_legend(ncol = 1)) +
    thema()
}

grafik_schnitt <- function(schnitt, schwellen) {
  linien <- data.frame(name = names(schwellen), wert = unname(schwellen))
  linien$farbe <- FARBEN_VARIANTEN[linien$name]
  linien$text <- paste0("besser als ", linien$name, " darunter")
  obergrenze <- max(1.05, max(schnitt$anteil) * 1.05)
  ggplot(schnitt, aes(jahr, anteil)) +
    geom_hline(data = linien, aes(yintercept = wert), colour = linien$farbe,
               linewidth = 0.8, linetype = "22") +
    geom_text(data = linien, aes(x = 1, y = wert, label = text), colour = TINTE_2,
              hjust = 0, vjust = -0.5, size = 3.6) +
    geom_line(colour = TINTE, linewidth = 1.1) +
    geom_point(data = schnitt[schnitt$jahr %in% HORIZONTE, ], colour = TINTE, size = 2.4) +
    scale_y_continuous(labels = function(x) paste0(round(100 * x), " %"),
                       limits = c(0, obergrenze)) +
    scale_x_continuous(breaks = c(1, HORIZONTE)) +
    labs(x = "Betriebsjahre", y = "Durchschnittlicher Anteil noch in der Luft") +
    thema()
}

# Spannweite aus der Monte-Carlo-Rechnung: Median, mittlere 66 % und 95 % je Variante und Horizont.
grafik_unsicherheit <- function(q) {
  d <- q
  d$horizont <- factor(paste0(d$jahr, " J. (", START + d$jahr, ")"),
                       levels = paste0(HORIZONTE, " J. (", START + HORIZONTE, ")"))
  d$variante <- factor(d$variante, levels = rev(VARIANTEN))
  d$text <- vapply(d$q50, function(x) zahl(x, if (x < 10) 1 else 0), character(1))
  ggplot(d, aes(y = variante, colour = variante)) +
    geom_linerange(aes(xmin = q2.5, xmax = q97.5), linewidth = 0.9, alpha = 0.4) +
    geom_linerange(aes(xmin = q17, xmax = q83), linewidth = 3.2) +
    geom_point(aes(x = q50), size = 2.6, shape = 21, fill = FLAECHE, stroke = 1.2) +
    geom_text(aes(x = q97.5, label = text), hjust = -0.25, size = 3.3, colour = TINTE_2) +
    facet_grid(. ~ horizont, scales = "free_x") +
    scale_colour_manual(values = FARBEN_VARIANTEN, guide = "none") +
    scale_x_continuous(expand = expansion(mult = c(0.02, 0.35)), labels = function(x) zahl(x),
                       limits = c(0, NA), n.breaks = 3) +
    coord_cartesian(clip = "off") +
    labs(x = "kt CO2e (kumuliert)", y = NULL) +
    thema() +
    theme(panel.grid.major.y = element_blank())
}

# Rangkorrelation der wichtigsten Annahmen mit den Emissionen einer Variante.
grafik_einfluss <- function(einfluss, beschriftung, anzahl = 10) {
  letzte <- einfluss[einfluss$jahr == max(einfluss$jahr) & !is.na(einfluss$rho), ]
  wichtig <- head(letzte$id[order(-abs(letzte$rho))], anzahl)
  d <- einfluss[einfluss$id %in% wichtig, ]
  d$annahme <- factor(beschriftung[d$id], levels = rev(beschriftung[wichtig]))
  d$horizont <- factor(paste0("nach ", d$jahr, " Jahren"),
                       levels = paste0("nach ", sort(unique(d$jahr)), " Jahren"))
  d$richtung <- ifelse(d$rho >= 0, "erhöht die Emissionen", "senkt die Emissionen")
  ggplot(d, aes(rho, annahme, fill = richtung)) +
    geom_vline(xintercept = 0, colour = "#c3c2b7", linewidth = 0.4) +
    geom_col(width = 0.65) +
    facet_grid(. ~ horizont) +
    scale_fill_manual(values = c("erhöht die Emissionen" = "#e34948",
                                 "senkt die Emissionen" = "#2a78d6")) +
    scale_x_continuous(limits = c(-1, 1), breaks = c(-0.5, 0, 0.5)) +
    scale_y_discrete(labels = function(x) vapply(strwrap(x, 50, simplify = FALSE),
                                                 paste, character(1), collapse = "\n")) +
    labs(x = "Rangkorrelation (Spearman)", y = NULL) +
    thema() +
    theme(panel.grid.major.y = element_blank(),
          panel.spacing.x = unit(1.5, "lines"),
          axis.text.y = element_text(size = rel(0.78), lineheight = 0.85))
}
