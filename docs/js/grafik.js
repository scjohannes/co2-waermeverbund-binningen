// Grafiken als SVG, im Stil von R/grafiken.R. Jede Funktion zeichnet in einen Behälter und
// passt sich seiner Breite an; Legenden sind HTML über der Grafik.
(function (g) {
  'use strict';

  var M = g.Modell, F = g.Format;
  var NS = 'http://www.w3.org/2000/svg';

  var FARBEN_VARIANTEN = {};
  ['#2a78d6', '#6250d6', '#eb6834', '#e87ba4', '#1baf7a', '#eda100'].forEach(function (f, i) {
    FARBEN_VARIANTEN[M.VARIANTEN[i]] = f;
  });
  var FARBEN_KOMPONENTEN = {};
  ['#2a78d6', '#eb6834', '#1baf7a', '#eda100', '#e87ba4'].forEach(function (f, i) {
    FARBEN_KOMPONENTEN[M.KOMPONENTEN[i]] = f;
  });
  var FARBEN_HOLZ = {};
  FARBEN_HOLZ[M.HOLZARTEN.frisch] = '#2a78d6';
  FARBEN_HOLZ[M.HOLZARTEN.kronen] = '#eb6834';
  FARBEN_HOLZ[M.HOLZARTEN.totholz] = '#1baf7a';
  FARBEN_HOLZ[M.HOLZARTEN.kaskade] = '#eda100';
  FARBEN_HOLZ[M.MISCHUNG] = '#0b0b0b';
  var ROT = '#e34948', BLAU = '#2a78d6';

  var TINTE = '#1c1c1e', TINTE_2 = '#52525b', GEDAEMPFT = '#8a8f98', GITTER = '#eceef2', NULLLINIE = '#c3c2b7';
  var SCHRIFT = 'system-ui, -apple-system, "Segoe UI", Roboto, "Helvetica Neue", Arial, sans-serif';

  // ---- Bausteine ----

  function el(tag, attribute, eltern) {
    var e = document.createElementNS(NS, tag);
    for (var k in attribute) if (attribute[k] !== undefined && attribute[k] !== null) e.setAttribute(k, attribute[k]);
    if (eltern) eltern.appendChild(e);
    return e;
  }

  function text(eltern, x, y, inhalt, attribute) {
    var t = el('text', Object.assign({ x: x, y: y, fill: GEDAEMPFT, 'font-size': 12 }, attribute || {}), eltern);
    t.textContent = inhalt;
    return t;
  }

  function titel(eltern, inhalt) { el('title', {}, eltern).textContent = inhalt; }

  var messer = null;
  function textbreite(inhalt, groesse, gewicht) {
    if (!messer) messer = document.createElement('canvas').getContext('2d');
    messer.font = (gewicht || 400) + ' ' + groesse + 'px ' + SCHRIFT;
    return messer.measureText(inhalt).width;
  }

  // Text in Zeilen umbrechen, die höchstens `breite` Pixel breit sind
  function umbrechen(inhalt, breite, groesse) {
    var woerter = inhalt.split(' '), zeilen = [], zeile = '';
    woerter.forEach(function (w) {
      var probe = zeile ? zeile + ' ' + w : w;
      if (zeile && textbreite(probe, groesse) > breite) { zeilen.push(zeile); zeile = w; } else zeile = probe;
    });
    if (zeile) zeilen.push(zeile);
    return zeilen;
  }

  function skala(d0, d1, r0, r1) {
    var f = function (v) { return r0 + (v - d0) / (d1 - d0) * (r1 - r0); };
    f.d0 = d0; f.d1 = d1; f.r0 = r0; f.r1 = r1;
    return f;
  }

  // Runde Achsenwerte wie in d3/ggplot
  function ticks(start, stop, anzahl) {
    if (!(stop > start)) return [start];
    var schritt = (stop - start) / Math.max(1, anzahl);
    var potenz = Math.pow(10, Math.floor(Math.log10(schritt)));
    var fehler = schritt / potenz;
    schritt = potenz * (fehler >= Math.sqrt(50) ? 10 : fehler >= Math.sqrt(10) ? 5 : fehler >= Math.sqrt(2) ? 2 : 1);
    var aus = [];
    for (var i = Math.ceil(start / schritt); i * schritt <= stop + schritt * 1e-9; i++) {
      aus.push(Number((i * schritt).toPrecision(12)));
    }
    return aus;
  }

  function stellenFuer(werte) {
    var s = 0;
    werte.forEach(function (v) {
      var t = String(v), p = t.indexOf('.');
      if (p >= 0) s = Math.max(s, t.length - p - 1);
    });
    return s;
  }

  function leeren(behaelter) {
    while (behaelter.firstChild) behaelter.removeChild(behaelter.firstChild);
  }

  function svgFlaeche(behaelter, breite, hoehe, beschreibung) {
    var svg = el('svg', {
      width: breite, height: hoehe, viewBox: '0 0 ' + breite + ' ' + hoehe, role: 'img',
      'aria-label': beschreibung, 'font-family': SCHRIFT
    });
    behaelter.appendChild(svg);
    return svg;
  }

  function legende(behaelter, eintraege, art) {
    var div = document.createElement('div');
    div.className = 'legende';
    eintraege.forEach(function (e) {
      var span = document.createElement('span');
      span.className = 'legende-eintrag';
      var zeichen = document.createElement('i');
      zeichen.className = art === 'linie' ? 'zeichen-linie' : 'zeichen-flaeche';
      zeichen.style.background = e.farbe;
      if (e.dick) zeichen.style.height = '4px';
      span.appendChild(zeichen);
      span.appendChild(document.createTextNode(e.name));
      div.appendChild(span);
    });
    behaelter.appendChild(div);
  }

  // Waagrechte Gitterlinien und Beschriftung der y-Achse
  function yAchse(svg, y, werte, x0, x1, format) {
    werte.forEach(function (v) {
      el('line', { x1: x0, x2: x1, y1: y(v), y2: y(v), stroke: GITTER }, svg);
      text(svg, x0 - 8, y(v) + 4, format(v), { 'text-anchor': 'end' });
    });
  }

  function xGitter(svg, x, werte, y0, y1) {
    werte.forEach(function (v) { el('line', { x1: x(v), x2: x(v), y1: y0, y2: y1, stroke: GITTER }, svg); });
  }

  function achsentitelY(svg, inhalt, x, ymitte) {
    text(svg, x, ymitte, inhalt, { transform: 'rotate(-90 ' + x + ' ' + ymitte + ')', 'text-anchor': 'middle',
      fill: TINTE_2, 'font-size': 12.5 });
  }

  // Facetten: Anzahl Spalten nach verfügbarer Breite
  function spalten(anzahl, breite, mindestens) {
    return Math.max(1, Math.min(anzahl, Math.floor(breite / mindestens)));
  }

  function horizontLabel(j) { return j + ' J. (' + (M.START + j) + ')'; }

  // Beschriftungen am Linienende so verschieben, dass sie sich nicht überlappen
  function entzerren(werte, abstand) {
    var idx = werte.map(function (_, i) { return i; }).sort(function (a, b) { return werte[a] - werte[b]; });
    var aus = werte.slice();
    for (var i = 1; i < idx.length; i++) aus[idx[i]] = Math.max(aus[idx[i]], aus[idx[i - 1]] + abstand);
    return aus;
  }

  function kt(v) { return F.zahl(v, Math.abs(v) < 10 ? 1 : 0); }

  // Hinweisfenster: mit der Maus beim Darüberfahren, mit dem Finger beim Antippen; ein Tippen
  // ausserhalb schliesst es wieder (Touch löst nach dem Loslassen sofort pointerleave aus)
  var offen = null;
  if (typeof document !== 'undefined') {
    document.addEventListener('pointerdown', function (e) {
      if (offen && !offen.ziel.contains(e.target)) { var o = offen; offen = null; o.verbergen(); }
    }, true);
  }
  function hinweisBinden(ziel, flaeche, zeigen, verbergen) {
    flaeche.addEventListener('pointermove', zeigen);
    flaeche.addEventListener('pointerdown', function (e) { offen = { ziel: ziel, verbergen: verbergen }; zeigen(e); });
    flaeche.addEventListener('pointerleave', function (e) { if (e.pointerType !== 'touch') verbergen(); });
  }

  function hinweisfenster(behaelter) {
    var fenster = document.createElement('div');
    fenster.className = 'hinweisfenster';
    fenster.hidden = true;
    behaelter.appendChild(fenster);
    return fenster;
  }

  // ---- Verlauf der kumulierten Emissionen mit Unsicherheit ----

  // summen: {Variante: [kt in Jahr 0..50]}; band: Zeilen aus mcQuantile über alle Jahre (oder null)
  function verlauf(behaelter, summen, band) {
    leeren(behaelter);
    var B = behaelter.clientWidth, H = behaelter.clientHeight || 480;
    // Teile jeder Variante: Zeigt man auf eine Linie oder ihren Namen, treten die anderen zurück
    var formen = {}, namen = {}, linien = {};
    M.VARIANTEN.forEach(function (v) { formen[v] = []; namen[v] = []; });
    // schmal: Legende über der Grafik statt Namen am Linienende
    var schmal = B < 560;
    if (schmal) {
      legende(behaelter, M.VARIANTEN.map(function (v) { return { name: v, farbe: FARBEN_VARIANTEN[v] }; }), 'linie');
      M.VARIANTEN.forEach(function (v, i) { namen[v].push(behaelter.firstChild.children[i]); });
      H -= behaelter.firstChild.offsetHeight + 6;
    }
    var labelBreite = Math.max.apply(null, M.VARIANTEN.map(function (v) { return textbreite(v, 13); }));
    var m = { l: schmal ? 46 : 62, r: schmal ? 10 : labelBreite + 22, t: 12, b: 44 };
    var svg = svgFlaeche(behaelter, B, H, 'Kumulierte zusätzliche Emissionen der Varianten ab 2028');

    var baender = {};
    if (band) {
      M.VARIANTEN.forEach(function (v) { baender[v] = [{ jahr: 0, 'q2.5': 0, q17: 0, q83: 0, 'q97.5': 0 }]; });
      band.forEach(function (z) { baender[z.variante].push(z); });
    }
    var hoechst = 0;
    M.VARIANTEN.forEach(function (v) {
      summen[v].forEach(function (s) { hoechst = Math.max(hoechst, s); });
      if (band) baender[v].forEach(function (z) { hoechst = Math.max(hoechst, z['q97.5']); });
    });
    hoechst *= 1.05;
    var x = skala(0, M.JAHRE, m.l, B - m.r);
    var y = skala(0, hoechst, H - m.b, m.t);
    var yWerte = ticks(0, hoechst, schmal ? 4 : 5);
    var xWerte = [0].concat(M.HORIZONTE);

    xGitter(svg, x, xWerte, m.t, H - m.b);
    yAchse(svg, y, yWerte, m.l, B - m.r, function (v) { return F.zahl(v); });
    achsentitelY(svg, 'kt CO₂e (kumuliert)', 14, (m.t + H - m.b) / 2);
    xWerte.forEach(function (j) {
      text(svg, x(j), H - m.b + 18, j + ' J.', { 'text-anchor': 'middle' });
      text(svg, x(j), H - m.b + 33, String(M.START + j), { 'text-anchor': 'middle' });
    });

    function flaeche(punkte, unten, oben) {
      var d = punkte.map(function (z, i) { return (i ? 'L' : 'M') + x(z.jahr) + ',' + y(z[oben]); }).join('');
      for (var i = punkte.length - 1; i >= 0; i--) d += 'L' + x(punkte[i].jahr) + ',' + y(punkte[i][unten]);
      return d + 'Z';
    }
    if (band) {
      M.VARIANTEN.forEach(function (v) {
        formen[v].push(el('path', { d: flaeche(baender[v], 'q2.5', 'q97.5'), fill: FARBEN_VARIANTEN[v],
          'fill-opacity': 0.05 }, svg));
      });
      M.VARIANTEN.forEach(function (v) {
        formen[v].push(el('path', { d: flaeche(baender[v], 'q17', 'q83'), fill: FARBEN_VARIANTEN[v],
          'fill-opacity': 0.13 }, svg));
      });
    }
    M.VARIANTEN.forEach(function (v) {
      var gruppe = linien[v] = el('g', {}, svg);
      var d = summen[v].map(function (s, j) { return (j ? 'L' : 'M') + x(j) + ',' + y(s); }).join('');
      el('path', { d: d, fill: 'none', stroke: FARBEN_VARIANTEN[v], 'stroke-width': 2.2, 'stroke-linejoin': 'round' }, gruppe);
      M.HORIZONTE.forEach(function (j) {
        el('circle', { cx: x(j), cy: y(summen[v][j]), r: 3.6, fill: FARBEN_VARIANTEN[v] }, gruppe);
      });
      formen[v].push(gruppe);
    });

    var namenFlaechen = [];
    if (!schmal) {
      var enden = M.VARIANTEN.map(function (v) { return summen[v][M.JAHRE]; });
      var lage = entzerren(enden, hoechst * 0.06);
      M.VARIANTEN.forEach(function (v, i) {
        namen[v].push(text(svg, x(M.JAHRE) + 12, y(lage[i]) + 4.5, v, { fill: TINTE_2, 'font-size': 13 }));
        namenFlaechen.push(el('rect', { x: x(M.JAHRE) + 6, y: y(lage[i]) - 9, width: textbreite(v, 13) + 12,
          height: 18, fill: 'transparent' }, svg));
      });
    } else {
      namenFlaechen = M.VARIANTEN.map(function (v) { return namen[v][0]; });
    }
    M.VARIANTEN.forEach(function (v) {
      formen[v].concat(namen[v]).forEach(function (e) { e.classList.add('verlauf-teil'); });
    });

    var hervorgehoben = null;
    function hervorheben(v) {
      if (v === hervorgehoben) return;
      hervorgehoben = v;
      M.VARIANTEN.forEach(function (w) {
        var zurueck = v !== null && w !== v;
        formen[w].forEach(function (e) { e.style.opacity = zurueck ? 0.15 : ''; });
        namen[w].forEach(function (e) { e.style.opacity = zurueck ? 0.35 : ''; });
      });
      // die hervorgehobene Linie liegt über den anderen
      M.VARIANTEN.forEach(function (w) { svg.insertBefore(linien[w], fuehrung); });
      if (v !== null) svg.insertBefore(linien[v], fuehrung);
    }

    // Hover: Werte aller Varianten im nächsten Jahr
    var fuehrung = el('line', { y1: m.t, y2: H - m.b, stroke: TINTE_2, 'stroke-dasharray': '3 3', visibility: 'hidden' }, svg);
    var fenster = hinweisfenster(behaelter);
    var flaecheHover = el('rect', { x: m.l, y: m.t, width: Math.max(0, B - m.r - m.l), height: H - m.t - m.b,
      fill: 'transparent' }, svg);
    function zeigen(ereignis) {
      var r = svg.getBoundingClientRect();
      var px = ereignis.clientX - r.left, py = ereignis.clientY - r.top;
      var genau = Math.max(0, Math.min(M.JAHRE, (px - m.l) / (B - m.r - m.l) * M.JAHRE));
      var j = Math.round(genau);
      // die Linie nahe beim Zeiger hervorheben
      var j0 = Math.floor(genau), j1 = Math.min(M.JAHRE, j0 + 1), naechste = null;
      var abstand = ereignis.pointerType === 'touch' ? 24 : 12;
      M.VARIANTEN.forEach(function (v) {
        var d = Math.abs(y(summen[v][j0] + (summen[v][j1] - summen[v][j0]) * (genau - j0)) - py);
        if (d < abstand) { abstand = d; naechste = v; }
      });
      hervorheben(naechste);
      fuehrung.setAttribute('x1', x(j)); fuehrung.setAttribute('x2', x(j));
      fuehrung.setAttribute('visibility', 'visible');
      var reihen = M.VARIANTEN.map(function (v) {
        var z = band && j > 0 ? baender[v][j] : null;
        return { v: v, wert: summen[v][j], z: z };
      }).sort(function (a, b) { return b.wert - a.wert; });
      fenster.innerHTML = '<div class="hinweis-titel">' + j + ' J. (' + (M.START + j) + ')</div>' +
        reihen.map(function (r2) {
          var spanne = r2.z ? '<span class="hinweis-spanne">' + kt(r2.z.q17) + '–' + kt(r2.z.q83) + '</span>' : '';
          var klasse = naechste !== null && r2.v !== naechste ? 'hinweis-zeile zurueck' : 'hinweis-zeile';
          return '<div class="' + klasse + '"><i style="background:' + FARBEN_VARIANTEN[r2.v] + '"></i><span>' + r2.v +
            '</span><b>' + kt(r2.wert) + ' kt</b>' + spanne + '</div>';
        }).join('') + (band && j > 0 ? '<div class="hinweis-fuss">Spanne: mittlere 66 % der Ergebnisse</div>' : '');
      fenster.hidden = false;
      var links = x(j) + 14;
      if (links + fenster.offsetWidth > B) links = x(j) - fenster.offsetWidth - 14;
      fenster.style.left = Math.max(0, links) + 'px';
      fenster.style.top = m.t + 'px';
    }
    function verbergen() { fuehrung.setAttribute('visibility', 'hidden'); fenster.hidden = true; hervorheben(null); }
    hinweisBinden(svg, flaecheHover, zeigen, verbergen);
    namenFlaechen.forEach(function (flaecheName, i) {
      var v = M.VARIANTEN[i];
      flaecheName.addEventListener('pointerenter', function () { hervorheben(v); });
      flaecheName.addEventListener('pointerleave', function (e) { if (e.pointerType !== 'touch') hervorheben(null); });
      flaecheName.addEventListener('pointerdown', function () {
        offen = { ziel: behaelter, verbergen: verbergen };
        hervorheben(v);
      });
    });
  }

  // ---- Zusammensetzung nach 10 bis 50 Jahren ----

  // werte[Horizont][Variante][Komponente]: kumulierte kt
  function balken(behaelter, werte) {
    leeren(behaelter);
    legende(behaelter, M.KOMPONENTEN.map(function (k) {
      return { name: k.replace('CO2', 'CO₂'), farbe: FARBEN_KOMPONENTEN[k] };
    }), 'flaeche');
    var B = behaelter.clientWidth;
    var labelBreite = Math.max.apply(null, M.VARIANTEN.map(function (v) { return textbreite(v, 12.5); })) + 12;
    var nSpalten = spalten(M.HORIZONTE.length, B - labelBreite, 165);
    var nZeilen = Math.ceil(M.HORIZONTE.length / nSpalten);
    var luecke = 18, streifen = 24, achse = 22, zeilenHoehe = 30;
    var panelB = (B - labelBreite - (nSpalten - 1) * luecke) / nSpalten;
    var panelH = M.VARIANTEN.length * zeilenHoehe;
    var H = nZeilen * (streifen + panelH + achse + 10) + 26;
    var svg = svgFlaeche(behaelter, B, H, 'Zusammensetzung der kumulierten Emissionen nach 10 bis 50 Jahren');

    M.HORIZONTE.forEach(function (j, h) {
      var sp = h % nSpalten, ze = Math.floor(h / nSpalten);
      var x0 = labelBreite + sp * (panelB + luecke), y0 = ze * (streifen + panelH + achse + 10);
      text(svg, x0, y0 + 16, horizontLabel(j), { fill: TINTE, 'font-weight': 600, 'font-size': 13 });
      var top = y0 + streifen;
      var summen = M.VARIANTEN.map(function (v) {
        return M.KOMPONENTEN.reduce(function (s, k) { return s + werte[j][v][k]; }, 0);
      });
      var minimum = Math.min(0, Math.min.apply(null, M.VARIANTEN.map(function (v) {
        return M.KOMPONENTEN.reduce(function (s, k) { return s + Math.min(0, werte[j][v][k]); }, 0);
      })));
      var maximum = Math.max.apply(null, M.VARIANTEN.map(function (v) {
        return M.KOMPONENTEN.reduce(function (s, k) { return s + Math.max(0, werte[j][v][k]); }, 0);
      }));
      var xWerte = ticks(minimum, maximum, 3);
      var x = skala(minimum, maximum * 1.45 - minimum * 0.05, x0, x0 + panelB);
      xGitter(svg, x, xWerte, top, top + panelH);
      xWerte.forEach(function (v) {
        text(svg, x(v), top + panelH + 15, F.zahl(v, stellenFuer(xWerte)), { 'text-anchor': 'middle' });
      });
      M.VARIANTEN.forEach(function (v, i) {
        var yb = top + i * zeilenHoehe + zeilenHoehe * 0.16, hb = zeilenHoehe * 0.68;
        if (sp === 0) text(svg, labelBreite - 10, yb + hb / 2 + 4, v, { 'text-anchor': 'end', 'font-size': 12.5 });
        var plus = 0, minus = 0;
        M.KOMPONENTEN.forEach(function (k) {
          var w = werte[j][v][k];
          if (w === 0) return;
          var a = w > 0 ? plus : minus + w, b = w > 0 ? plus + w : minus;
          if (w > 0) plus += w; else minus += w;
          var r = el('rect', { x: x(a), y: yb, width: Math.max(0, x(b) - x(a)), height: hb,
            fill: FARBEN_KOMPONENTEN[k], stroke: '#fff', 'stroke-width': 1 }, svg);
          titel(r, v + ', nach ' + j + ' Jahren – ' + k.replace('CO2', 'CO₂') + ': ' + kt(w) + ' kt');
        });
        text(svg, x(Math.max(plus, 0)) + 5, yb + hb / 2 + 4, kt(summen[i]), { fill: TINTE_2, 'font-size': 12 });
      });
    });
    text(svg, labelBreite + (B - labelBreite) / 2, H - 6, 'kt CO₂e (kumuliert)',
      { 'text-anchor': 'middle', fill: TINTE_2, 'font-size': 12.5 });
  }

  // ---- Spannweite der Ergebnisse ----

  function unsicherheit(behaelter, zeilen) {
    leeren(behaelter);
    var B = behaelter.clientWidth;
    var labelBreite = Math.max.apply(null, M.VARIANTEN.map(function (v) { return textbreite(v, 12.5); })) + 12;
    var nSpalten = spalten(M.HORIZONTE.length, B - labelBreite, 150);
    var nZeilen = Math.ceil(M.HORIZONTE.length / nSpalten);
    var luecke = 18, streifen = 24, achse = 22, zeilenHoehe = nSpalten === 1 ? 30 : 44;
    var panelB = (B - labelBreite - (nSpalten - 1) * luecke) / nSpalten;
    var panelH = M.VARIANTEN.length * zeilenHoehe;
    var H = nZeilen * (streifen + panelH + achse + 10) + 26;
    var svg = svgFlaeche(behaelter, B, H, 'Spannweite der Ergebnisse nach 10 bis 50 Jahren');
    var fenster = hinweisfenster(behaelter);
    fenster.classList.add('hinweis-schmal');
    var markierung = null;

    function zeigen(v, j, z, ym, links, rechts, kreis) {
      if (markierung && markierung !== kreis) markierung.setAttribute('r', 4.6);
      markierung = kreis;
      kreis.setAttribute('r', 6);
      fenster.innerHTML = '<div class="hinweis-titel"><i style="background:' + FARBEN_VARIANTEN[v] + '"></i>' + v +
        ', nach ' + j + ' Jahren</div><div class="hinweis-werte">' +
        '<span>Median</span><b>' + kt(z.q50) + ' kt</b>' +
        '<span>mittlere 66 %</span><b>' + kt(z.q17) + '–' + kt(z.q83) + ' kt</b>' +
        '<span>mittlere 95 %</span><b>' + kt(z['q2.5']) + '–' + kt(z['q97.5']) + ' kt</b></div>';
      fenster.hidden = false;
      var w = fenster.offsetWidth, h = fenster.offsetHeight;
      var l = rechts + 12;
      if (l + w > B) l = links - w - 12;
      if (l < 0) l = Math.max(0, Math.min(B - w, rechts - w / 2));
      fenster.style.left = l + 'px';
      fenster.style.top = Math.max(0, Math.min(H - h, ym - h / 2)) + 'px';
    }
    function verbergen() {
      if (markierung) markierung.setAttribute('r', 4.6);
      markierung = null;
      fenster.hidden = true;
    }

    M.HORIZONTE.forEach(function (j, h) {
      var sp = h % nSpalten, ze = Math.floor(h / nSpalten);
      var x0 = labelBreite + sp * (panelB + luecke), y0 = ze * (streifen + panelH + achse + 10);
      text(svg, x0, y0 + 16, horizontLabel(j), { fill: TINTE, 'font-weight': 600, 'font-size': 13 });
      var top = y0 + streifen;
      var teil = zeilen.filter(function (z) { return z.jahr === j; });
      var maximum = Math.max.apply(null, teil.map(function (z) { return z['q97.5']; }));
      var xWerte = ticks(0, maximum, 3);
      var x = skala(0, maximum * 1.08, x0, x0 + panelB);
      xGitter(svg, x, xWerte, top, top + panelH);
      xWerte.forEach(function (v) {
        text(svg, x(v), top + panelH + 15, F.zahl(v, stellenFuer(xWerte)), { 'text-anchor': 'middle' });
      });
      M.VARIANTEN.forEach(function (v, i) {
        var z = teil.filter(function (r) { return r.variante === v; })[0];
        var ym = top + i * zeilenHoehe + zeilenHoehe / 2, farbe = FARBEN_VARIANTEN[v];
        if (sp === 0) text(svg, labelBreite - 10, ym + 4, v, { 'text-anchor': 'end', 'font-size': 12.5 });
        el('line', { x1: x(z['q2.5']), x2: x(z['q97.5']), y1: ym, y2: ym, stroke: farbe, 'stroke-width': 2,
          'stroke-opacity': 0.4 }, svg);
        el('line', { x1: x(z.q17), x2: x(z.q83), y1: ym, y2: ym, stroke: farbe, 'stroke-width': 7 }, svg);
        var kreis = el('circle', { cx: x(z.q50), cy: ym, r: 4.6, fill: '#fff', stroke: farbe, 'stroke-width': 2.4 }, svg);
        // Trefferfläche: die ganze Zeile des Panels, grösser als die Marke
        var flaeche = el('rect', { x: x0, y: ym - zeilenHoehe / 2, width: panelB, height: zeilenHoehe, fill: 'transparent',
          'aria-label': v + ', nach ' + j + ' Jahren: Median ' + kt(z.q50) + ' kt; mittlere 66 %: ' + kt(z.q17) + '–' +
            kt(z.q83) + ' kt; mittlere 95 %: ' + kt(z['q2.5']) + '–' + kt(z['q97.5']) + ' kt' }, svg);
        hinweisBinden(svg, flaeche, function () {
          zeigen(v, j, z, ym, x(z['q2.5']), x(z['q97.5']), kreis);
        }, verbergen);
      });
    });
    text(svg, labelBreite + (B - labelBreite) / 2, H - 6, 'kt CO₂e (kumuliert)',
      { 'text-anchor': 'middle', fill: TINTE_2, 'font-size': 12.5 });
  }

  // ---- Einfluss der Annahmen ----

  function einfluss(behaelter, eintraege, beschriftung, anzahl) {
    leeren(behaelter);
    anzahl = anzahl || 10;
    legende(behaelter, [{ name: 'erhöht die Emissionen', farbe: ROT }, { name: 'senkt die Emissionen', farbe: BLAU }], 'flaeche');
    var jahre = Array.from(new Set(eintraege.map(function (e) { return e.jahr; }))).sort(function (a, b) { return a - b; });
    var letzte = eintraege.filter(function (e) { return e.jahr === jahre[jahre.length - 1]; })
      .sort(function (a, b) { return Math.abs(b.rho) - Math.abs(a.rho); });
    var wichtig = letzte.slice(0, anzahl).map(function (e) { return e.id; });

    var B = behaelter.clientWidth;
    var wert = function (j, id) { return eintraege.filter(function (r) { return r.jahr === j && r.id === id; })[0]; };
    var balken = function (svg, x, e, id, j, yb, hb) {
      if (!e) return;
      var r = el('rect', { x: Math.min(x(0), x(e.rho)), y: yb, width: Math.abs(x(e.rho) - x(0)), height: hb,
        fill: e.rho >= 0 ? ROT : BLAU }, svg);
      titel(r, beschriftung[id] + ', nach ' + j + ' Jahren: ' + F.zahl(e.rho, 2));
    };
    var skalaBeschriften = function (svg, x, y) {
      [-0.5, 0, 0.5].forEach(function (v) { text(svg, x(v), y, F.zahl(v, 1), { 'text-anchor': 'middle' }); });
    };
    var streifen = 24;

    if (B < 560) {
      // schmal: Zeitpunkte untereinander, Beschriftung über dem Balken
      var zeilenS = wichtig.map(function (id) { return umbrechen(beschriftung[id], B - 8, 12); });
      var balkenH = 14, zeileAbstand = 10;
      var hoehen = zeilenS.map(function (z) { return z.length * 15 + balkenH + zeileAbstand; });
      var panelHs = hoehen.reduce(function (a, b) { return a + b; }, 0);
      var blockS = streifen + panelHs + 22;
      var HS = jahre.length * blockS + 20;
      var svgS = svgFlaeche(behaelter, B, HS, 'Rangkorrelation der wichtigsten Annahmen mit den Emissionen des Plans');
      jahre.forEach(function (j, s) {
        var y0 = s * blockS;
        text(svgS, 0, y0 + 16, 'nach ' + j + ' Jahren', { fill: TINTE, 'font-weight': 600, 'font-size': 13 });
        var x = skala(-1, 1, 4, B - 4);
        var yc = y0 + streifen;
        wichtig.forEach(function (id, i) {
          zeilenS[i].forEach(function (t, k) { text(svgS, 0, yc + 12 + k * 15, t, { 'font-size': 12 }); });
          var yb = yc + zeilenS[i].length * 15 + 2;
          el('line', { x1: x(0), x2: x(0), y1: yb - 2, y2: yb + balkenH + 2, stroke: NULLLINIE }, svgS);
          balken(svgS, x, wert(j, id), id, j, yb, balkenH);
          yc += hoehen[i];
        });
        skalaBeschriften(svgS, x, yc + 12);
      });
      text(svgS, B / 2, HS - 4, 'Rangkorrelation (Spearman)', { 'text-anchor': 'middle', fill: TINTE_2, 'font-size': 12.5 });
      return;
    }

    // breit: beide Zeitpunkte nebeneinander, Beschriftung links
    var labelMax = Math.min(300, Math.max(140, B * 0.4));
    var zeilen = wichtig.map(function (id) { return umbrechen(beschriftung[id], labelMax - 12, 12); });
    var labelBreite = Math.max.apply(null, zeilen.map(function (z) {
      return Math.max.apply(null, z.map(function (t) { return textbreite(t, 12); }));
    })) + 14;
    var zeilenHoehe = Math.max(30, Math.max.apply(null, zeilen.map(function (z) { return z.length; })) * 15 + 8);
    var luecke = 22, achse = 36;
    var panelB = (B - labelBreite - luecke) / 2;
    var panelH = wichtig.length * zeilenHoehe;
    var H = streifen + panelH + achse;
    var svg = svgFlaeche(behaelter, B, H, 'Rangkorrelation der wichtigsten Annahmen mit den Emissionen des Plans');

    jahre.forEach(function (j, s) {
      var x0 = labelBreite + s * (panelB + luecke);
      text(svg, x0, 16, 'nach ' + j + ' Jahren', { fill: TINTE, 'font-weight': 600, 'font-size': 13 });
      var x = skala(-1, 1, x0, x0 + panelB);
      xGitter(svg, x, [-0.5, 0.5], streifen, streifen + panelH);
      el('line', { x1: x(0), x2: x(0), y1: streifen, y2: streifen + panelH, stroke: NULLLINIE }, svg);
      skalaBeschriften(svg, x, streifen + panelH + 15);
      wichtig.forEach(function (id, i) {
        var yb = streifen + i * zeilenHoehe + zeilenHoehe * 0.175, hb = zeilenHoehe * 0.65;
        if (s === 0) {
          var start = yb + hb / 2 - (zeilen[i].length - 1) * 7.5 + 4;
          zeilen[i].forEach(function (t, k) {
            text(svg, labelBreite - 10, start + k * 15, t, { 'text-anchor': 'end', 'font-size': 12 });
          });
        }
        balken(svg, x, wert(j, id), id, j, yb, hb);
      });
    });
    text(svg, labelBreite + (B - labelBreite) / 2, H - 4, 'Rangkorrelation (Spearman)',
      { 'text-anchor': 'middle', fill: TINTE_2, 'font-size': 12.5 });
  }

  // ---- Holz: Anteil einer Tonne, der noch in der Luft ist ----

  function prozent(v) { return F.zahl(100 * v) + ' %'; }

  function antwort(behaelter, kurven) {
    leeren(behaelter);
    var arten = [M.HOLZARTEN.frisch, M.HOLZARTEN.kronen, M.HOLZARTEN.totholz, M.HOLZARTEN.kaskade, M.MISCHUNG];
    legende(behaelter, arten.map(function (a) { return { name: a, farbe: FARBEN_HOLZ[a], dick: a === M.MISCHUNG }; }), 'linie');
    var B = behaelter.clientWidth, H = Math.max(260, (behaelter.clientHeight || 420) - behaelter.firstChild.offsetHeight - 6);
    var m = { l: 70, r: 14, t: 10, b: 40 };
    var svg = svgFlaeche(behaelter, B, H, 'Anteil einer verbrannten Tonne Holz, der noch zusätzlich in der Luft ist');
    var alle = [];
    arten.forEach(function (a) { alle = alle.concat(kurven[a]); });
    var unten = Math.min(0, Math.min.apply(null, alle)), oben = Math.max.apply(null, alle);
    var spanne = oben - unten;
    unten -= spanne * 0.05 * (unten < 0 ? 1 : 0); oben += spanne * 0.05;
    var x = skala(0, M.JAHRE - 1, m.l, B - m.r);
    var y = skala(unten, oben, H - m.b, m.t);
    var yWerte = ticks(unten, oben, 5);
    xGitter(svg, x, [0].concat(M.HORIZONTE.filter(function (j) { return j < M.JAHRE; })), m.t, H - m.b);
    yAchse(svg, y, yWerte, m.l, B - m.r, prozent);
    el('line', { x1: m.l, x2: B - m.r, y1: y(0), y2: y(0), stroke: NULLLINIE }, svg);
    [0].concat(M.HORIZONTE).forEach(function (j) {
      if (j <= M.JAHRE - 1) text(svg, x(j), H - m.b + 16, String(j), { 'text-anchor': 'middle' });
    });
    text(svg, (m.l + B - m.r) / 2, H - 6, 'Jahre seit dem Verbrennen', { 'text-anchor': 'middle', fill: TINTE_2, 'font-size': 12.5 });
    achsentitelY(svg, 'Anteil noch zusätzlich in der Luft', 14, (m.t + H - m.b) / 2);
    arten.forEach(function (a) {
      var d = kurven[a].map(function (v, i) { return (i ? 'L' : 'M') + x(i) + ',' + y(v); }).join('');
      el('path', { d: d, fill: 'none', stroke: FARBEN_HOLZ[a], 'stroke-width': a === M.MISCHUNG ? 3.4 : 2,
        'stroke-linejoin': 'round' }, svg);
    });
  }

  // ---- Dauerbetrieb: durchschnittlicher Anteil und Schwellen ----

  function schnitt(behaelter, werte, schwellen) {
    leeren(behaelter);
    var B = behaelter.clientWidth, H = behaelter.clientHeight || 420;
    var m = { l: 70, r: 14, t: 14, b: 40 };
    var svg = svgFlaeche(behaelter, B, H, 'Durchschnittlicher Anteil noch in der Luft bei Dauerbetrieb, mit Schwellen gegenüber Öl und Gas');
    var oben = Math.max(1.05, Math.max.apply(null, werte) * 1.05, Math.max.apply(null, Object.values(schwellen)) * 1.05);
    var x = skala(1, M.JAHRE, m.l, B - m.r);
    var y = skala(0, oben, H - m.b, m.t);
    var xWerte = [1].concat(M.HORIZONTE);
    xGitter(svg, x, xWerte, m.t, H - m.b);
    yAchse(svg, y, ticks(0, oben, 5), m.l, B - m.r, prozent);
    xWerte.forEach(function (j) { text(svg, x(j), H - m.b + 16, String(j), { 'text-anchor': 'middle' }); });
    text(svg, (m.l + B - m.r) / 2, H - 6, 'Betriebsjahre', { 'text-anchor': 'middle', fill: TINTE_2, 'font-size': 12.5 });
    achsentitelY(svg, 'Durchschnittlicher Anteil noch in der Luft', 14, (m.t + H - m.b) / 2);
    Object.keys(schwellen).forEach(function (name) {
      var ys = y(schwellen[name]);
      if (ys < m.t || ys > H - m.b) return;
      el('line', { x1: m.l, x2: B - m.r, y1: ys, y2: ys, stroke: FARBEN_VARIANTEN[name], 'stroke-width': 2,
        'stroke-dasharray': '6 6' }, svg);
      text(svg, x(1) + 4, ys - 6, 'besser als ' + name + ' darunter', { fill: TINTE_2, 'font-size': 12.5 });
    });
    var d = werte.map(function (v, i) { return (i ? 'L' : 'M') + x(i + 1) + ',' + y(v); }).join('');
    el('path', { d: d, fill: 'none', stroke: TINTE, 'stroke-width': 2.6, 'stroke-linejoin': 'round' }, svg);
    M.HORIZONTE.forEach(function (j) { el('circle', { cx: x(j), cy: y(werte[j - 1]), r: 3.8, fill: TINTE }, svg); });
  }

  var Grafik = {
    verlauf: verlauf, balken: balken, unsicherheit: unsicherheit, einfluss: einfluss,
    antwort: antwort, schnitt: schnitt, FARBEN_VARIANTEN: FARBEN_VARIANTEN, ticks: ticks
  };
  g.Grafik = Grafik;
})(typeof self !== 'undefined' ? self : this);
