// Das JavaScript-Modell rechnet dasselbe wie R/model.R.
'use strict';
const test = require('node:test');
const assert = require('node:assert/strict');
const { js, referenz, DATEN, nahe, reiheNahe } = require('./hilfe.js');

const Modell = js('modell.js');
const p0 = Modell.standardwerte(DATEN.parameter);

test('die Standardwerte stimmen mit den Parametern der R-Referenz überein', () => {
  const r = referenz.modell.standard.parameter;
  for (const id of Object.keys(r)) nahe(p0[id], r[id], 0, 0, id);
});

for (const [fall, r] of Object.entries(referenz.modell)) {
  test(`Fall «${fall}»: kumulierte Emissionen aller Varianten wie in R`, () => {
    const ist = Modell.rechneKumuliert(r.parameter);
    Modell.VARIANTEN.forEach((v, i) => reiheNahe(ist[i], r.kumuliert[v], 1e-9, 1e-6, `${fall} ${v}`));
  });

  test(`Fall «${fall}»: jährliche Komponenten, brutto und Holzkennzahlen wie in R`, () => {
    const e = Modell.rechneAlles(r.parameter);
    for (const v of Modell.VARIANTEN) {
      Modell.KOMPONENTEN.forEach((k, i) => {
        reiheNahe(e.varianten[v].jaehrlich[i], r.jaehrlich[v][k], 1e-9, 1e-6, `${fall} ${v} ${k}`);
      });
    }
    reiheNahe(e.varianten['Plan IWB'].brutto, r.brutto, 1e-9, 1e-6, `${fall} brutto`);
    const h = e.holz;
    for (const name of ['m3', 'srm', 'lkw_pro_jahr', 'kamin_g_kwh']) nahe(h[name], r.holz[name], 1e-9, 1e-9, name);
    for (const name of Object.keys(r.holz.schwellen)) {
      nahe(h.schwellen[name], r.holz.schwellen[name], 1e-9, 1e-12, `Schwelle ${name}`);
      assert.equal(h.ab_jahr[name], r.holz.ab_jahr[name], `ab Jahr ${name}`);
    }
    reiheNahe(h.schnitt, r.holz.schnitt, 1e-9, 1e-12, 'Schnitt');
    for (const art of Object.keys(r.holz.kurven)) reiheNahe(h.kurven[art], r.holz.kurven[art], 1e-9, 1e-12, art);
  });
}

test('jeder Regler auf Minimum und Maximum rechnet wie in R', () => {
  assert.ok(referenz.extreme.length >= 2 * 60, 'Referenz deckt alle Regler ab');
  for (const f of referenz.extreme) {
    const e = Modell.rechneAlles(Object.assign({}, p0, f.aenderung));
    const text = `${f.id} = ${f.aenderung[f.id]}`;
    for (const v of Modell.VARIANTEN) {
      reiheNahe(Modell.HORIZONTE.map((j) => e.varianten[v].summe[j]), f.kumuliert[v], 1e-9, 1e-6, `${text}, ${v}`);
    }
    for (const name of ['m3', 'srm', 'lkw_pro_jahr']) nahe(e.holz[name], f.holz[name], 1e-9, 1e-9, `${text}, ${name}`);
  }
});

test('kein Regler kehrt die Richtung des Ergebnisses um', () => {
  // wie in tests/test-model.R: Rückschritte unter 0,2 kt sind gegenläufige Wirkungen des Zuwachses
  const hinUndHer = (y) => {
    let s = 0;
    for (let i = 1; i < y.length; i++) s += Math.abs(y[i] - y[i - 1]);
    return (s - Math.abs(y[y.length - 1] - y[0])) / 2;
  };
  for (const z of DATEN.parameter.filter((r) => r.typ === 'slider')) {
    const schritte = Math.round((z.max - z.min) / z.schritt);
    const gitter = schritte + 1 > 25
      ? Array.from({ length: 25 }, (_, k) => z.min + k * (z.max - z.min) / 24)
      : Array.from({ length: schritte + 1 }, (_, k) => z.min + k * z.schritt);
    const reihen = gitter.map((w) => Modell.rechneKumuliert(Object.assign({}, p0, { [z.id]: w })));
    let groesste = 0;
    Modell.VARIANTEN.forEach((v, i) => {
      for (let j = 0; j < Modell.JAHRE; j++) groesste = Math.max(groesste, hinUndHer(reihen.map((r) => r[i][j])));
    });
    assert.ok(groesste < 200, `${z.id}: läuft um ${Math.round(groesste)} t zurück`);
  }
});

test('auf Parametern, die R gezogen hat, rechnet JavaScript dieselben Emissionen', () => {
  for (const [i, z] of referenz.ziehungen.entries()) {
    const p = Object.assign({}, p0, z.parameter);
    const ist = Modell.rechneKumuliert(p);
    Modell.VARIANTEN.forEach((v, k) => reiheNahe(ist[k], z.kumuliert[v], 1e-9, 1e-6, `Ziehung ${i + 1} ${v}`));
  }
});
