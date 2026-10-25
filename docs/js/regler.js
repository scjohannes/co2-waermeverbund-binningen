// Anteils-Regler, die zusammen 100 % ergeben (Planteile, Herkunft des Holzes):
// Übertragung von R/regler.R.
//
// Verstellt man einen Regler, teilen sich die anderen den Rest im bisherigen Verhältnis. Kein
// Regler überschreitet sein Maximum; was nicht Platz hat, geht an die übrigen. Reicht der Platz
// aller anderen nicht, bleibt der verstellte Regler so nahe wie möglich am gewünschten Wert.
// Gerundet wird auf die Schrittweite, so dass die Summe genau stimmt.
(function (g) {
  'use strict';

  // werte: {id: Wert} mit dem neuen Wert des verstellten Reglers; maximum: {id: Maximum}
  function umverteilen(werte, geaendert, maximum, schritt, summe) {
    schritt = schritt || 1;
    summe = summe === undefined ? 100 : summe;
    var aus = Object.assign({}, werte);
    var andere = Object.keys(werte).filter(function (id) { return id !== geaendert; });
    var grenzeSumme = andere.reduce(function (s, id) { return s + maximum[id]; }, 0);
    aus[geaendert] = Math.max(aus[geaendert], summe - grenzeSumme);
    var rest = summe - aus[geaendert];

    var gewicht = {};
    var gewichtSumme = andere.reduce(function (s, id) { return s + werte[id]; }, 0);
    andere.forEach(function (id) { gewicht[id] = gewichtSumme > 0 ? werte[id] : 1; });
    var ziel = {};
    andere.forEach(function (id) { ziel[id] = 0; });
    var frei = andere.slice();
    while (frei.length > 0) {
      var gs = frei.reduce(function (s, id) { return s + gewicht[id]; }, 0);
      var anteil = {};
      frei.forEach(function (id) { anteil[id] = rest * gewicht[id] / gs; });
      var voll = frei.filter(function (id) { return anteil[id] >= maximum[id]; });
      if (voll.length === 0) {
        frei.forEach(function (id) { ziel[id] = anteil[id]; });
        break;
      }
      voll.forEach(function (id) { ziel[id] = maximum[id]; rest -= maximum[id]; });
      frei = frei.filter(function (id) { return voll.indexOf(id) < 0; });
      if (frei.length > 0 && frei.reduce(function (s, id) { return s + gewicht[id]; }, 0) <= 0) {
        frei.forEach(function (id) { gewicht[id] = 1; });
      }
    }

    // Auf Schritte runden; fehlende Schritte an die grössten Rundungsreste, solange Platz ist
    var einheiten = {}, ganz = {};
    andere.forEach(function (id) {
      einheiten[id] = ziel[id] / schritt;
      ganz[id] = Math.floor(einheiten[id] + 1e-9);
    });
    var fehlend = Math.round((summe - aus[geaendert]) / schritt) -
      andere.reduce(function (s, id) { return s + ganz[id]; }, 0);
    var reihenfolge = andere.slice().sort(function (a, b) {
      return (einheiten[b] - ganz[b]) - (einheiten[a] - ganz[a]);
    });
    reihenfolge.forEach(function (id) {
      if (fehlend <= 0) return;
      if ((ganz[id] + 1) * schritt <= maximum[id] + 1e-9) { ganz[id] += 1; fehlend -= 1; }
    });
    andere.forEach(function (id) { aus[id] = ganz[id] * schritt; });
    return aus;
  }

  var Regler = { umverteilen: umverteilen };
  if (typeof module !== 'undefined' && module.exports) module.exports = Regler; else g.Regler = Regler;
})(typeof self !== 'undefined' ? self : this);
