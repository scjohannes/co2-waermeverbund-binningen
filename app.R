library(shiny)
library(bslib)
library(ggplot2)

source("R/model.R", encoding = "UTF-8")
source("R/grafiken.R", encoding = "UTF-8")
source("R/literatur.R", encoding = "UTF-8")
source("R/unsicherheit.R", encoding = "UTF-8")
source("R/regler.R", encoding = "UTF-8")

N_ZIEHUNGEN <- 1000

# Zwischenspeicher für bereits gerechnete Monte-Carlo-Ergebnisse; ausdrücklich gesetzt, weil die
# Browser-Version (shinylive) sonst keinen hat
shinyOptions(cache = cachem::cache_mem(max_size = 200 * 1024^2))

register <- lies_register("parameter.csv")
gruppen <- unique(register$gruppe)
priors <- priors_vorbereiten(lies_priors("priors.csv"), register)

# Verweise in Quellen und Begründungen der Verteilungen gemeinsam nummerieren
zitate <- zitate_nummerieren(c(register$quelle, priors$begruendung), lies_literatur("literatur.csv"))
register$quelle <- zitate$texte[seq_len(nrow(register))]
priors$begruendung <- zitate$texte[nrow(register) + seq_len(nrow(priors))]

# Monte-Carlo-Rechnung auf mehreren Kernen; für die Standardwerte einmal beim Start
rechner <- mc_cluster()
onStop(function() if (!is.null(rechner)) parallel::stopCluster(rechner))
p_standard <- standardwerte(register)
mc_standard <- mc_rechnen(p_standard, priors, n = N_ZIEHUNGEN, cl = rechner)

literaturliste <- tags$ol(
  class = "literatur",
  lapply(seq_len(nrow(zitate$verzeichnis)), function(i) {
    z <- zitate$verzeichnis[i, ]
    tags$li(value = z$nr, z$zitat,
            if (nzchar(z$url)) tagList(" ", tags$a(href = z$url, target = "_blank", z$url)))
  })
)

# Ein Eingabefeld pro Zeile der Parameterliste; die Quelle erscheint als Tooltip.
eingabe <- function(z) {
  id <- paste0("p_", z$id)
  titel <- if (nzchar(z$einheit)) paste0(z$label, " (", z$einheit, ")") else z$label
  beschriftung <- if (nzchar(z$quelle)) {
    tooltip(span(titel, span("ⓘ", class = "text-muted", title = "Quelle")), z$quelle)
  } else {
    titel
  }
  switch(z$typ,
    slider = sliderInput(id, beschriftung, min = z$min, max = z$max, value = z$wert,
                         step = z$schritt, ticks = FALSE, sep = "'"),
    switch = input_switch(id, beschriftung, value = z$wert == 1),
    auswahl = {
      paare <- strsplit(strsplit(z$auswahl, "|", fixed = TRUE)[[1]], "=", fixed = TRUE)
      selectInput(id, beschriftung,
                  choices = setNames(vapply(paare, `[`, "", 2), vapply(paare, `[`, "", 1)),
                  selected = as.character(z$wert))
    },
    fest = div(class = "mb-3 small", strong(titel), ": ", zahl(z$wert),
               div(class = "text-muted", z$quelle))
  )
}

gruppen_panels <- lapply(gruppen, function(g) {
  zeilen <- register[register$gruppe == g, ]
  accordion_panel(g, lapply(seq_len(nrow(zeilen)), function(i) eingabe(zeilen[i, ])))
})

seitenleiste <- sidebar(
  width = 340,
  bg = "#ffffff",
  title = "Annahmen",
  p(class = "small text-muted mb-1",
    "Alle Werte lassen sich verstellen; das ⓘ zeigt die Quelle. Anteile, die zusammen 100 %",
    "ergeben, gleichen sich beim Verstellen aus."),
  actionButton("zuruecksetzen", "Standardwerte wiederherstellen",
               class = "btn-sm btn-outline-secondary w-100"),
  uiOutput("hinweise"),
  do.call(accordion, c(gruppen_panels, list(open = FALSE, multiple = TRUE, class = "accordion-flush")))
)

# Methodentext: gemeinsam mit der Website (docs/)
methode <- div(class = "text-seite", includeHTML("methode.html"))

thema <- bs_theme(
  version = 5,
  bg = "#f5f6f8",
  fg = "#1c1c1e",
  primary = "#2a78d6",
  secondary = "#6b7280",
  base_font = font_collection("system-ui", "-apple-system", "Segoe UI", "Roboto",
                              "Helvetica Neue", "Arial", "sans-serif"),
  "border-radius" = "0.6rem",
  "border-radius-lg" = "0.9rem",
  "card-bg" = "#ffffff",
  "card-cap-bg" = "#ffffff",
  "card-border-color" = "#e7e9ee",
  "card-border-radius" = "0.9rem"
) |>
  bs_add_rules("
    .navbar { border-bottom: 1px solid #e7e9ee; }
    .navbar-brand { font-weight: 600; letter-spacing: -0.01em; }
    .card { box-shadow: 0 1px 2px rgba(16, 24, 40, .04), 0 1px 3px rgba(16, 24, 40, .06); }
    .card-header { font-weight: 600; border-bottom: none; padding-top: 1rem; }
    .card-header .form-check, .card-header .shiny-input-container { margin-bottom: 0; }
    .card-footer { background: #ffffff; border-top: none; }
    .bslib-sidebar-layout > .sidebar { border-right: 1px solid #e7e9ee; }
    .accordion-button { font-weight: 500; font-size: .95rem; }
    .bslib-value-box .value-box-title { color: #6b7280; }
    .text-seite { max-width: 880px; padding: .5rem 0 2rem; }
    .text-seite h5 { margin-top: 1.5rem; }
    .literatur { font-size: .9rem; padding-left: 1.6rem; }
    .literatur li { margin-bottom: .35rem; }
    #t_annahmen table, #t_priors table { font-size: .85rem; }
    .formeln .MathJax_Display { overflow-x: auto; overflow-y: hidden; }
  ")

ui <- page_navbar(
  title = tagList("CO₂-Vergleich", span(class = "text-muted fw-normal ms-2", "Wärmeverbund Binningen")),
  theme = thema,
  navbar_options = navbar_options(bg = "#ffffff", theme = "light", underline = TRUE),
  fillable = FALSE,
  # Formeln in Reitern, die beim Laden verborgen sind, beim Öffnen setzen
  header = tags$script(HTML(
    "$(document).on('shown.bs.tab', function() {
       if (window.MathJax && MathJax.Hub) MathJax.Hub.Queue(['Typeset', MathJax.Hub]);
     });"
  )),
  sidebar = seitenleiste,
  nav_panel(
    "Vergleich",
    card(
      full_screen = TRUE,
      card_header(
        class = "d-flex justify-content-between align-items-center flex-wrap gap-2",
        "Zusätzliches CO₂e in der Atmosphäre ab 2028",
        downloadButton("herunterladen", "CSV", class = "btn-sm btn-outline-secondary")
      ),
      plotOutput("p_verlauf", height = "500px")
    ),
    card(
      full_screen = TRUE,
      card_header("Zusammensetzung nach 10 bis 50 Jahren"),
      plotOutput("p_balken", height = "420px")
    )
  ),
  nav_panel(
    "Unsicherheit",
    card(
      full_screen = TRUE,
      card_header("Spannweite der Ergebnisse nach 10 bis 50 Jahren"),
      plotOutput("p_unsicherheit", height = "440px"),
      card_footer(class = "small text-muted",
                  paste0(format(N_ZIEHUNGEN, big.mark = "'"), " Ziehungen aus den Verteilungen aller"),
                  "unsicheren Annahmen (Reiter «Annahmen»). Kreis: Median; dicker Balken: mittlere",
                  "66 %; dünner Balken: mittlere 95 % der Ergebnisse; Zahl: Median in kt. Die Verteilungen",
                  "haben die Werte der Regler als Mittelwert; Bezugsgrössen und Varianten sind fest.")
    ),
    card(
      full_screen = TRUE,
      card_header("Welche Annahmen die Emissionen des Plans bestimmen"),
      plotOutput("p_einfluss", height = "480px"),
      card_footer(class = "small text-muted",
                  "Die zehn Annahmen mit dem stärksten Zusammenhang nach 50 Jahren.")
    )
  ),
  nav_panel(
    "Holz & Wald",
    layout_column_wrap(
      width = "220px",
      fill = FALSE,
      value_box(title = "Holzbedarf im Plan", value = textOutput("vb_m3"),
                p(textOutput("vb_srm", inline = TRUE))),
      value_box(title = "Lastwagen pro Jahr", value = textOutput("vb_lkw"),
                p("Planungsbericht: rund 83 (Oktober–Mai)")),
      value_box(title = "Biogenes CO₂ aus Kamin und Lager", value = textOutput("vb_kamin"),
                p("je kWh Wärme aus dem Holzkessel")),
      value_box(title = "Holzwärme besser als Öl bzw. Gas nach", value = textOutput("vb_ab"),
                p("bei Dauerbetrieb, ohne Wärmerückgewinnung"))
    ),
    layout_columns(
      col_widths = c(6, 6),
      card(
        full_screen = TRUE,
        card_header("Eine Tonne Holz: Anteil, der noch zusätzlich in der Luft ist"),
        plotOutput("p_antwort", height = "420px")
      ),
      card(
        full_screen = TRUE,
        card_header("Dauerbetrieb: durchschnittlicher Anteil und Schwellen gegenüber Öl und Gas"),
        plotOutput("p_schnitt", height = "420px")
      )
    )
  ),
  nav_panel(
    "Annahmen",
    card(card_header("Alle Annahmen mit aktuellem Wert"), tableOutput("t_annahmen"),
         card_footer(class = "small text-muted",
                     "Zahlen in eckigen Klammern verweisen auf das Literaturverzeichnis.")),
    card(card_header("Unsicherheit der Annahmen"), tableOutput("t_priors"),
         card_footer(class = "small text-muted",
                     "Der Regler bestimmt den Mittelwert, die Literatur die Stärke der Verteilung: Sie",
                     "ist so gewählt, dass die Verteilung um den Standardwert die Perzentile aus der",
                     "Literatur möglichst gut trifft, und bleibt beim Verstellen des Reglers gleich.",
                     "Beta für Anteile und Wirkungsgrade, Gamma für positive Grössen, Normal für Werte,",
                     "die auch negativ sein können, Dirichlet für die Herkunftsanteile. Die Formeln",
                     "stehen im Reiter «Formeln».")),
    card(card_header("Literatur"), literaturliste)
  ),
  nav_panel("Methode", methode),
  nav_panel("Formeln", withMathJax(includeHTML("formeln.html")))
)

server <- function(input, output, session) {
  p_roh <- reactive({
    p <- standardwerte(register)
    for (id in register$id) {
      wert <- input[[paste0("p_", id)]]
      if (!is.null(wert)) p[[id]] <- as.numeric(wert)
    }
    p
  })
  p <- debounce(p_roh, 300)
  erg <- reactive(rechne_alles(p()))

  # Anteile, die zusammen 100 % ergeben: Verstellt man einen Regler, passen sich die anderen an.
  # Werte, die der Server selbst setzt, kommen vom Browser zurück und werden nur abgehakt.
  anteile_koppeln <- function(ids) {
    zeilen <- register[match(ids, register$id), ]
    stand <- setNames(zeilen$wert, ids)
    erwartet <- numeric(0)
    lesen <- function() vapply(ids, function(id) as.numeric(input[[paste0("p_", id)]] %||% NA), numeric(1))
    observe({
      werte <- lesen()
      if (anyNA(werte)) return()
      angekommen <- names(erwartet)[abs(werte[names(erwartet)] - erwartet) < 1e-9]
      erwartet <<- erwartet[setdiff(names(erwartet), angekommen)]
      if (length(erwartet) > 0) return()
      geaendert <- ids[abs(werte - stand) > 1e-9]
      if (length(geaendert) == 0) return()
      neu <- if (abs(sum(werte) - 100) < 1e-9) werte else
        umverteilen(werte, geaendert[1], zeilen$max, min(zeilen$schritt))
      stand <<- neu
      for (id in ids[abs(neu - werte) > 1e-9]) {
        erwartet[[id]] <<- neu[[id]]
        updateSliderInput(session, paste0("p_", id), value = neu[[id]])
      }
    })
    # für «Standardwerte wiederherstellen»: diese Werte setzt der Server gleich selbst
    function(neu) {
      werte <- isolate(lesen())
      for (id in ids[is.na(werte) | abs(neu - werte) > 1e-9]) erwartet[[id]] <<- neu[[id]]
      stand <<- neu
    }
  }
  gruppen_100 <- list(c("plan_holz", "plan_wrg", "plan_wp", "plan_oel"),
                      c("anteil_frisch", "anteil_kronen", "anteil_totholz", "anteil_kaskade"))
  gekoppelt <- lapply(gruppen_100, anteile_koppeln)

  observeEvent(input$zuruecksetzen, {
    for (k in seq_along(gruppen_100)) {
      gekoppelt[[k]](setNames(register$wert[match(gruppen_100[[k]], register$id)], gruppen_100[[k]]))
    }
    for (i in seq_len(nrow(register))) {
      z <- register[i, ]
      id <- paste0("p_", z$id)
      switch(z$typ,
        slider = updateSliderInput(session, id, value = z$wert),
        switch = update_switch(id, value = z$wert == 1, session = session),
        auswahl = updateSelectInput(session, id, selected = as.character(z$wert))
      )
    }
  })

  output$hinweise <- renderUI({
    q <- p()
    hinweise <- character()
    if (q$vorrat_max <= q$vorrat_heute * 1.05) {
      hinweise <- c(hinweise, "Der maximale Vorrat liegt kaum über dem heutigen; gerechnet wird mit 95 %.")
    }
    if (length(hinweise) == 0) return(NULL)
    div(class = "alert alert-warning small py-2 my-2", lapply(hinweise, tags$p, class = "mb-1"))
  })

  # Monte-Carlo-Rechnung um die Werte der Regler
  mc <- reactive({
    q <- p()
    if (isTRUE(all.equal(q, p_standard))) return(mc_standard)
    withProgress(message = "Unsicherheit wird berechnet", value = 0, {
      mc_rechnen(q, priors, n = N_ZIEHUNGEN, fortschritt = function(x) setProgress(x), cl = rechner)
    })
  }) |>
    bindCache(p())

  output$p_verlauf <- renderPlot(
    grafik_verlauf(erg()$summe, mc_quantile(mc(), jahre = seq_len(JAHRE))),
    res = 96)

  output$p_unsicherheit <- renderPlot(grafik_unsicherheit(mc_quantile(mc())), res = 96)

  output$p_einfluss <- renderPlot(
    grafik_einfluss(mc_einfluss(mc()), setNames(register$label, register$id)), res = 96)

  output$t_priors <- renderTable(prior_tabelle(priors, register, p()), striped = TRUE, spacing = "s")

  output$p_balken <- renderPlot(grafik_balken(erg()$jahre), res = 96)

  output$herunterladen <- downloadHandler(
    filename = function() paste0("co2-vergleich-binningen-", Sys.Date(), ".csv"),
    content = function(datei) {
      d <- erg()$jahre
      d$kalenderjahr <- START + d$jahr - 1
      write.csv(d, datei, row.names = FALSE, fileEncoding = "UTF-8")
    }
  )

  output$vb_m3 <- renderText(paste0(zahl(erg()$holz$m3, -1), " m³/Jahr"))
  output$vb_srm <- renderText(paste0("geerntet, inklusive Lagerverlust; geliefert ≈ ",
                                     zahl(erg()$holz$srm, -2), " Schüttraummeter Schnitzel"))
  output$vb_lkw <- renderText(zahl(erg()$holz$lkw_pro_jahr))
  output$vb_kamin <- renderText(paste0(zahl(erg()$holz$kamin_g_kwh), " g/kWh"))
  output$vb_ab <- renderText({
    ab <- erg()$holz$ab_jahr
    text <- ifelse(is.na(ab), "> 50 J.", paste0(ab, " J."))
    paste0("Öl ", text[["100 % Öl"]], " · Gas ", text[["100 % Gas"]])
  })

  output$p_antwort <- renderPlot(grafik_antwort(erg()$holz$kurven), res = 96)
  output$p_schnitt <- renderPlot(grafik_schnitt(erg()$holz$schnitt, erg()$holz$schwellen), res = 96)

  output$t_annahmen <- renderTable({
    q <- p()
    aktuell <- vapply(register$id, function(id) q[[id]], numeric(1))
    data.frame(
      Gruppe = register$gruppe,
      Annahme = register$label,
      Wert = ifelse(abs(aktuell - register$wert) > 1e-9,
                    paste0(aktuell, " (Standard ", register$wert, ")"),
                    as.character(aktuell)),
      Einheit = register$einheit,
      Quelle = register$quelle,
      check.names = FALSE
    )
  }, striped = TRUE, spacing = "s")
}

shinyApp(ui, server)
