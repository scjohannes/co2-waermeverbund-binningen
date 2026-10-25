// Steuerung der Website: Regler, Reiter, Tabellen und das Zusammenspiel mit der Rechnung.
(function () {
  'use strict';

  var M = window.Modell, V = window.Verteilung, F = window.Format, R = window.Regler, G = window.Grafik;
  var D = window.DATEN;
  var N_ZIEHUNGEN = 1000;
  var MATHJAX = 'https://cdn.jsdelivr.net/npm/mathjax@3.2.2/es5/tex-chtml.js';

  var standard = M.standardwerte(D.parameter);
  var p = Object.assign({}, standard);
  var nachId = {};
  D.parameter.forEach(function (z) { nachId[z.id] = z; });
  var beschriftung = {};
  D.parameter.forEach(function (z) { beschriftung[z.id] = z.label; });

  // Anteile, die zusammen 100 % ergeben: Verstellt man einen Regler, passen sich die anderen an.
  var GRUPPEN_100 = [['plan_holz', 'plan_wrg', 'plan_wp', 'plan_oel'],
    ['anteil_frisch', 'anteil_kronen', 'anteil_totholz', 'anteil_kaskade']];

  var erg = null;          // deterministische Rechnung mit den Werten der Regler
  var mc = null;           // Zusammenfassung der Monte-Carlo-Rechnung
  var aktiv = 'vergleich';

  function $(id) { return document.getElementById(id); }
  function stellen(schritt) { var t = String(schritt), i = t.indexOf('.'); return i < 0 ? 0 : t.length - i - 1; }
  function sauber(v) { return Number(Number(v).toPrecision(12)); }

  // ---- Seitenleiste mit Reglern ----

  var eingaben = {};

  function anzeigen(id) {
    var z = nachId[id], e = eingaben[id];
    if (!e) return;
    if (z.typ === 'slider') {
      e.feld.value = p[id];
      e.wert.textContent = F.zahl(p[id], stellen(z.schritt));
    } else if (z.typ === 'switch') {
      e.feld.checked = p[id] === 1;
    } else if (z.typ === 'auswahl') {
      e.feld.value = String(p[id]);
    }
  }

  function quelleKnopf(kopf, z, behaelter) {
    if (!z.quelle) return;
    var knopf = document.createElement('button');
    knopf.type = 'button';
    knopf.className = 'info';
    knopf.textContent = 'ⓘ';
    knopf.setAttribute('aria-label', 'Quelle zu «' + z.label + '» anzeigen');
    knopf.setAttribute('aria-expanded', 'false');
    var quelle = document.createElement('p');
    quelle.className = 'quelle';
    quelle.hidden = true;
    quelle.textContent = z.quelle;
    knopf.addEventListener('click', function () {
      quelle.hidden = !quelle.hidden;
      knopf.setAttribute('aria-expanded', String(!quelle.hidden));
    });
    kopf.appendChild(knopf);
    behaelter.appendChild(quelle);
  }

  function reglerBauen() {
    var gruppen = [];
    D.parameter.forEach(function (z) { if (gruppen.indexOf(z.gruppe) < 0) gruppen.push(z.gruppe); });
    var ziel = $('regler');
    gruppen.forEach(function (gruppe) {
      var details = document.createElement('details');
      details.className = 'gruppe';
      var summary = document.createElement('summary');
      summary.textContent = gruppe;
      details.appendChild(summary);
      D.parameter.filter(function (z) { return z.gruppe === gruppe; }).forEach(function (z) {
        var block = document.createElement('div');
        block.className = 'regler regler-' + z.typ;
        var kopf = document.createElement('div');
        kopf.className = 'regler-kopf';
        var titel = z.einheit ? z.label + ' (' + z.einheit + ')' : z.label;
        var id = 'p_' + z.id;
        if (z.typ === 'slider') {
          var label = document.createElement('label');
          label.htmlFor = id;
          label.textContent = titel;
          kopf.appendChild(label);
          quelleKnopf(kopf, z, block);
          var wert = document.createElement('output');
          wert.htmlFor = id;
          kopf.appendChild(wert);
          var feld = document.createElement('input');
          feld.type = 'range'; feld.id = id;
          feld.min = z.min; feld.max = z.max; feld.step = z.schritt; feld.value = z.wert;
          feld.addEventListener('input', function () { verstellen(z.id, sauber(feld.value)); });
          block.appendChild(kopf);
          block.appendChild(feld);
          eingaben[z.id] = { feld: feld, wert: wert };
        } else if (z.typ === 'switch') {
          var schalter = document.createElement('label');
          schalter.className = 'schalter';
          var box = document.createElement('input');
          box.type = 'checkbox'; box.id = id; box.setAttribute('role', 'switch');
          box.addEventListener('change', function () { verstellen(z.id, box.checked ? 1 : 0); });
          schalter.appendChild(box);
          schalter.appendChild(document.createTextNode(titel));
          kopf.appendChild(schalter);
          quelleKnopf(kopf, z, block);
          block.appendChild(kopf);
          eingaben[z.id] = { feld: box };
        } else if (z.typ === 'auswahl') {
          var l2 = document.createElement('label');
          l2.htmlFor = id;
          l2.textContent = titel;
          kopf.appendChild(l2);
          quelleKnopf(kopf, z, block);
          var select = document.createElement('select');
          select.id = id;
          z.auswahl.forEach(function (o) {
            var option = document.createElement('option');
            option.value = String(o.wert);
            option.textContent = o.text;
            select.appendChild(option);
          });
          select.addEventListener('change', function () { verstellen(z.id, Number(select.value)); });
          block.appendChild(kopf);
          block.appendChild(select);
          eingaben[z.id] = { feld: select };
        } else {
          var fest = document.createElement('p');
          fest.className = 'fest';
          var stark = document.createElement('strong');
          stark.textContent = titel;
          fest.appendChild(stark);
          fest.appendChild(document.createTextNode(': ' + F.zahl(z.wert, stellen(z.wert))));
          block.appendChild(fest);
          if (z.quelle) {
            var q = document.createElement('p');
            q.className = 'quelle';
            q.textContent = z.quelle;
            block.appendChild(q);
          }
        }
        details.appendChild(block);
      });
      ziel.appendChild(details);
    });
    D.parameter.forEach(function (z) { anzeigen(z.id); });
  }

  function verstellen(id, wert) {
    p[id] = wert;
    GRUPPEN_100.forEach(function (gruppe) {
      if (gruppe.indexOf(id) < 0) return;
      var werte = {}, maximum = {}, schritt = Infinity;
      gruppe.forEach(function (k) {
        werte[k] = p[k];
        maximum[k] = nachId[k].max;
        schritt = Math.min(schritt, nachId[k].schritt);
      });
      var neu = R.umverteilen(werte, id, maximum, schritt);
      gruppe.forEach(function (k) { p[k] = sauber(neu[k]); });
      gruppe.forEach(anzeigen);
    });
    anzeigen(id);
    neuRechnen();
  }

  // ---- Rechnen ----

  // Zeichnen im nächsten Bild; der Timer sorgt dafür, dass auch gedrosselte Fenster zeichnen
  var geplant = false;
  function neuRechnen() {
    hinweise();
    unsicherheitAnfordern();
    if (geplant) return;
    geplant = true;
    var los = function () {
      if (!geplant) return;
      geplant = false;
      erg = M.rechneAlles(p);
      zeichnen();
    };
    requestAnimationFrame(los);
    setTimeout(los, 50);
  }

  // Monte-Carlo-Rechnung im Hintergrund; während sie läuft, wartet höchstens ein neuer Auftrag
  var rechner = null, laufend = false, wartet = false, zaehler = 0, verzoegerung = null;
  try {
    rechner = new Worker('js/rechner.js');
    rechner.onmessage = function (e) { mcErhalten(e.data); };
    rechner.onerror = function (e) {
      e.preventDefault();
      rechner = null;
      laufend = false;
      unsicherheitAnfordern();
    };
  } catch (fehler) {
    rechner = null;
  }

  function rechnetAnzeigen(ja) {
    Array.prototype.forEach.call(document.querySelectorAll('.rechnet'), function (e) { e.hidden = !ja; });
  }

  function unsicherheitAnfordern() {
    rechnetAnzeigen(true);
    clearTimeout(verzoegerung);
    verzoegerung = setTimeout(function () {
      if (laufend) { wartet = true; return; }
      laufend = true;
      var auftrag = { p: Object.assign({}, p), n: N_ZIEHUNGEN, id: ++zaehler };
      if (rechner) {
        rechner.postMessage(auftrag);
      } else {
        setTimeout(function () { mcErhalten(V.mcZusammenfassen(auftrag.p, D.priors, auftrag.n, auftrag.id)); }, 0);
      }
    }, 120);
  }

  function mcErhalten(ergebnis) {
    laufend = false;
    if (wartet) {
      wartet = false;
      unsicherheitAnfordern();
    } else {
      rechnetAnzeigen(false);
    }
    mc = ergebnis;
    zeichnen();
  }

  function hinweise() {
    var texte = [];
    if (p.vorrat_max <= p.vorrat_heute * 1.05) {
      texte.push('Der maximale Vorrat liegt kaum über dem heutigen; gerechnet wird mit 95 %.');
    }
    var ziel = $('hinweise');
    ziel.innerHTML = '';
    if (texte.length === 0) return;
    var box = document.createElement('div');
    box.className = 'warnung';
    texte.forEach(function (t) { var e = document.createElement('p'); e.textContent = t; box.appendChild(e); });
    ziel.appendChild(box);
  }

  // ---- Zeichnen ----

  function summenKt() {
    var aus = {};
    M.VARIANTEN.forEach(function (v) { aus[v] = erg.varianten[v].summe.map(function (s) { return s / 1000; }); });
    return aus;
  }

  function zeichnen() {
    if (!erg) return;
    if (aktiv === 'vergleich') {
      G.verlauf($('g-verlauf'), summenKt(), mc ? mc.band : null);
      var werte = {};
      M.HORIZONTE.forEach(function (j) {
        werte[j] = {};
        M.VARIANTEN.forEach(function (v) {
          werte[j][v] = {};
          M.KOMPONENTEN.forEach(function (k, i) { werte[j][v][k] = erg.varianten[v].kumuliert[i][j - 1] / 1000; });
        });
      });
      G.balken($('g-balken'), werte);
    } else if (aktiv === 'unsicherheit' && mc) {
      G.unsicherheit($('g-unsicherheit'), mc.band.filter(function (z) { return M.HORIZONTE.indexOf(z.jahr) >= 0; }));
      G.einfluss($('g-einfluss'), mc.einfluss, beschriftung);
    } else if (aktiv === 'holz') {
      var h = erg.holz;
      $('k-m3').textContent = F.zahl(h.m3, -1) + ' m³/Jahr';
      $('k-srm').textContent = 'geerntet, inklusive Lagerverlust; geliefert ≈ ' + F.zahl(h.srm, -2) +
        ' Schüttraummeter Schnitzel';
      $('k-lkw').textContent = F.zahl(h.lkw_pro_jahr);
      $('k-kamin').textContent = F.zahl(h.kamin_g_kwh) + ' g/kWh';
      var ab = function (name) { return h.ab_jahr[name] === null ? '> 50 J.' : h.ab_jahr[name] + ' J.'; };
      $('k-ab').textContent = 'Öl ' + ab('100 % Öl') + ' · Gas ' + ab('100 % Gas');
      G.antwort($('g-antwort'), h.kurven);
      G.schnitt($('g-schnitt'), h.schnitt, h.schwellen);
    } else if (aktiv === 'annahmen') {
      tabellen();
    }
  }

  function tabelle(ziel, spalten, zeilen) {
    var t = document.createElement('table');
    var kopf = t.createTHead().insertRow();
    spalten.forEach(function (s) { var th = document.createElement('th'); th.textContent = s; kopf.appendChild(th); });
    var koerper = t.createTBody();
    zeilen.forEach(function (z) {
      var r = koerper.insertRow();
      z.forEach(function (w) { r.insertCell().textContent = w; });
    });
    ziel.innerHTML = '';
    ziel.appendChild(t);
  }

  function tabellen() {
    tabelle($('t-annahmen'), ['Gruppe', 'Annahme', 'Wert', 'Einheit', 'Quelle'], D.parameter.map(function (z) {
      var wert = String(sauber(p[z.id]));
      if (Math.abs(p[z.id] - z.wert) > 1e-9) wert += ' (Standard ' + sauber(z.wert) + ')';
      return [z.gruppe, z.label, wert, z.einheit, z.quelle];
    }));
    tabelle($('t-priors'), ['Annahme', 'Einheit', 'Verteilung', 'Mittelwert', '90-%-Intervall',
      'Literatur (5.–95. Perzentil)', 'Begründung'], V.priorTabelle(D.priors, D.parameter, p).map(function (z) {
      return [z.Annahme, z.Einheit, z.Verteilung, z.Mittelwert, z.Intervall, z.Literatur, z.Begruendung];
    }));
  }

  function literatur() {
    var liste = $('literatur');
    D.literatur.forEach(function (eintrag) {
      var li = document.createElement('li');
      li.value = eintrag.nr;
      li.appendChild(document.createTextNode(eintrag.zitat));
      if (eintrag.url) {
        li.appendChild(document.createTextNode(' '));
        var a = document.createElement('a');
        a.href = eintrag.url; a.target = '_blank'; a.rel = 'noopener';
        a.textContent = eintrag.url;
        li.appendChild(a);
      }
      liste.appendChild(li);
    });
  }

  // ---- Reiter ----

  var mathjaxGeladen = false;
  function formelnSetzen() {
    if (mathjaxGeladen) {
      if (window.MathJax && MathJax.typesetPromise) MathJax.typesetPromise([$('formeln-text')]);
      return;
    }
    mathjaxGeladen = true;
    window.MathJax = {
      tex: { inlineMath: [['\\(', '\\)']], displayMath: [['$$', '$$']] },
      options: { ignoreHtmlClass: 'tex2jax_ignore', processHtmlClass: 'tex2jax_process' }
    };
    var s = document.createElement('script');
    s.src = MATHJAX;
    s.async = true;
    document.head.appendChild(s);
  }

  function reiterWaehlen(name, fokus) {
    aktiv = name;
    Array.prototype.forEach.call(document.querySelectorAll('[role=tab]'), function (tab) {
      var ja = tab.id === 'tab-' + name;
      tab.setAttribute('aria-selected', String(ja));
      tab.tabIndex = ja ? 0 : -1;
      if (ja && fokus) tab.focus();
      $(tab.getAttribute('aria-controls')).hidden = !ja;
    });
    if (name === 'formeln') formelnSetzen();
    if (history.replaceState) history.replaceState(null, '', '#' + name);
    zeichnen();
  }

  function reiterBauen() {
    var tabs = Array.prototype.slice.call(document.querySelectorAll('[role=tab]'));
    tabs.forEach(function (tab, i) {
      var name = tab.id.replace('tab-', '');
      tab.addEventListener('click', function () { reiterWaehlen(name); });
      tab.addEventListener('keydown', function (e) {
        var neu = e.key === 'ArrowRight' ? i + 1 : e.key === 'ArrowLeft' ? i - 1 : null;
        if (neu === null) return;
        e.preventDefault();
        reiterWaehlen(tabs[(neu + tabs.length) % tabs.length].id.replace('tab-', ''), true);
      });
    });
  }

  // ---- Weiteres ----

  function csvHerunterladen() {
    var zeilen = ['"variante","jahr","komponente","jaehrlich","kumuliert","kalenderjahr"'];
    M.VARIANTEN.forEach(function (v) {
      M.KOMPONENTEN.forEach(function (k, i) {
        for (var t = 0; t < M.JAHRE; t++) {
          zeilen.push(['"' + v + '"', t + 1, '"' + k + '"', erg.varianten[v].jaehrlich[i][t],
            erg.varianten[v].kumuliert[i][t], M.START + t].join(','));
        }
      });
    });
    var blob = new Blob(['﻿' + zeilen.join('\n') + '\n'], { type: 'text/csv;charset=utf-8' });
    var a = document.createElement('a');
    a.href = URL.createObjectURL(blob);
    a.download = 'co2-vergleich-binningen-' + new Date().toISOString().slice(0, 10) + '.csv';
    document.body.appendChild(a);
    a.click();
    setTimeout(function () { URL.revokeObjectURL(a.href); a.remove(); }, 0);
  }

  function seitenleiste(offen) {
    $('seitenleiste').classList.toggle('offen', offen);
    $('schleier').hidden = !offen;
    $('seitenleiste-auf').setAttribute('aria-expanded', String(offen));
  }

  function start() {
    reglerBauen();
    reiterBauen();
    literatur();
    $('methode-text').innerHTML = D.methode;
    $('formeln-text').innerHTML = D.formeln;
    $('zuruecksetzen').addEventListener('click', function () {
      p = Object.assign({}, standard);
      D.parameter.forEach(function (z) { anzeigen(z.id); });
      neuRechnen();
    });
    $('csv').addEventListener('click', csvHerunterladen);
    $('seitenleiste-auf').addEventListener('click', function () { seitenleiste(true); });
    $('seitenleiste-zu').addEventListener('click', function () { seitenleiste(false); });
    $('schleier').addEventListener('click', function () { seitenleiste(false); });
    document.addEventListener('keydown', function (e) { if (e.key === 'Escape') seitenleiste(false); });
    var breite = window.innerWidth, groesse = null;
    window.addEventListener('resize', function () {
      if (window.innerWidth === breite) return;
      breite = window.innerWidth;
      clearTimeout(groesse);
      groesse = setTimeout(zeichnen, 150);
    });
    var anfang = location.hash.replace('#', '');
    if (document.getElementById('tab-' + anfang)) reiterWaehlen(anfang);
    neuRechnen();
  }

  start();
})();
