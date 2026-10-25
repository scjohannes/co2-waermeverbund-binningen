// Zahlenformate wie in der Shiny-App: Tausender mit Apostroph, Dezimalpunkt.
(function (g) {
  'use strict';

  function tausender(text) {
    var teile = text.split('.');
    var vorzeichen = teile[0].charAt(0) === '-' ? '-' : '';
    var ganz = teile[0].replace('-', '').replace(/\B(?=(\d{3})+(?!\d))/g, "'");
    return vorzeichen + ganz + (teile.length > 1 ? '.' + teile[1] : '');
  }

  // Runden wie round() in R: nach dem exakten Wert der Gleitkommazahl (0.15 ist etwas kleiner
  // als 0.15 und wird zu 0.1), bei genau ,5 auf die gerade Ziffer
  function runden(x, stellen) {
    if (stellen < 0) {
      var f = Math.pow(10, -stellen);
      return runden(x / f, 0) * f;
    }
    var exakt = Math.abs(x).toFixed(Math.min(stellen + 30, 100));
    var punkt = exakt.indexOf('.');
    var kopf = exakt.slice(0, stellen > 0 ? punkt + 1 + stellen : punkt);
    var rest = exakt.slice(punkt + 1 + stellen);
    var abgeschnitten = Number(kopf);
    var schritt = Math.pow(10, -stellen);
    var r;
    if (/^50*$/.test(rest)) {
      var letzte = Number(kopf.replace('.', '').slice(-1));
      r = letzte % 2 === 0 ? abgeschnitten : abgeschnitten + schritt;
    } else {
      r = rest.charAt(0) >= '5' ? abgeschnitten + schritt : abgeschnitten;
    }
    return (x < 0 ? -1 : 1) * Number(r.toFixed(stellen));
  }

  // zahl() in R/grafiken.R: auf `stellen` runden (negativ: auf Zehner, Hunderter …)
  function zahl(x, stellen) {
    stellen = stellen || 0;
    var r = runden(x, stellen);
    if (r === 0) r = 0;   // keine «-0»
    return tausender(r.toFixed(Math.max(stellen, 0)));
  }

  // kurz() in R/unsicherheit.R: drei signifikante Stellen, sehr kleine Werte als «≈ 0»
  function kurz(x) {
    if (x !== 0 && Math.abs(x) < 1e-4) return '≈ 0';
    var s = Number(x.toPrecision(3));
    return tausender(String(s));
  }

  var Format = { zahl: zahl, kurz: kurz, runden: runden };
  if (typeof module !== 'undefined' && module.exports) module.exports = Format; else g.Format = Format;
})(typeof self !== 'undefined' ? self : this);
