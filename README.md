# CO₂-Vergleich Wärmeverbund Binningen

Wie viel CO₂e bringt der geplante Wärmeverbund Binningen (Holzschnitzel) bis 2078 zusätzlich in die
Atmosphäre, verglichen mit anderen Wegen, dieselbe Raumwärme zu liefern? Gerechnet wird die
tatsächliche Wirkung über die Zeit: Kamin-CO₂ minus das, was der Wald inzwischen nachgewachsen ist
bzw. was ohnehin verrottet wäre, plus Vorketten, Strom und Bau.

Alle Annahmen lassen sich auf der Website verstellen; jede trägt eine Quelle. Die Unsicherheit wird
mit 1000 Ziehungen aus den Verteilungen aller unsicheren Annahmen gezeigt.

## Aufbau

| Ordner / Datei | Inhalt |
|---|---|
| `docs/` | Die Website: HTML, CSS und JavaScript ohne Build-Schritt; GitHub Pages liefert den Ordner direkt aus |
| `parameter.csv` | Alle Annahmen mit Standardwert, Bereich und Quelle |
| `priors.csv` | Verteilung jeder unsicheren Annahme (Literatur-Bereich, 5.–95. Perzentil) |
| `literatur.csv` | Literaturliste |
| `methode.html`, `formeln.html` | Methode und Formeln (für Website und Shiny-App) |
| `R/`, `app.R` | Das Referenzmodell in R und eine Shiny-App mit demselben Inhalt |
| `web/` | Skripte, die die Daten der Website erzeugen, und Tests, die JavaScript gegen R prüfen |
| `tests/` | Tests des R-Modells |

R ist die Referenz. Die Website rechnet dasselbe Modell in JavaScript; die Tests vergleichen jeden
Regler auf Minimum und Maximum, zehn Sonderfälle und von R gezogene Parametersätze mit R.

## Nach Änderungen

Aus diesem Ordner, mit R und Node.js:

```bash
Rscript web/bauen.R
```

erzeugt `docs/js/daten.js` neu aus den CSV-Dateien, der Methode und den Formeln. Danach prüfen:

```bash
Rscript web/referenz.R
```

```bash
node --test "web/tests/*.test.js"
```

```bash
Rscript -e "testthat::test_dir('tests')"
```

## Lokal ansehen

Die Website braucht einen Webserver (wegen des Hintergrund-Threads für die Unsicherheit), zum Beispiel:

```bash
Rscript -e "httpuv::runStaticServer('docs', port = 8768)"
```

Die Shiny-App startet mit `Rscript -e "shiny::runApp()"` und braucht die Pakete shiny, bslib,
ggplot2 und cachem.
