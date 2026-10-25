// Verteilungen und Monte-Carlo-Rechnung wie in R/unsicherheit.R.
'use strict';
const test = require('node:test');
const assert = require('node:assert/strict');
const { js, referenz, DATEN, nahe, reiheNahe } = require('./hilfe.js');

const Modell = js('modell.js');
const V = js('verteilung.js');
const priors = DATEN.priors;
const p0 = Modell.standardwerte(DATEN.parameter);
const nachId = Object.fromEntries(priors.map((pr) => [pr.id, pr]));

test('Perzentile der Normal-, Gamma- und Betaverteilung wie in R', () => {
  reiheNahe(referenz.roh.normal.wk.map(V.qnorm), referenz.roh.normal.q, 1e-12, 1e-12, 'qnorm');
  for (const g of referenz.roh.gamma) {
    reiheNahe(g.wk.map((u) => V.qgammaEins(u, g.form)), g.q, 1e-8, 1e-300, `qgamma k = ${g.form}`);
  }
  for (const b of referenz.roh.beta) {
    reiheNahe(b.wk.map((u) => V.qbeta(u, b.a, b.b)), b.q, 1e-8, 1e-300, `qbeta(${b.a}, ${b.b})`);
  }
});

test('jede Annahme hat beim Standardwert und verstellt dieselben Perzentile wie in R', () => {
  for (const f of referenz.perzentile) {
    const p = Object.assign({}, p0, { [f.id]: f.wert });
    const ist = V.quantilPrior(nachId[f.id], p, f.wk, priors);
    reiheNahe(ist, f.q, 1e-7, 1e-12, `${f.id} bei ${f.wert}`);
  }
});

test('die Übersicht der Verteilungen zeigt dieselben Texte wie in R', () => {
  const vergleiche = (p, soll) => {
    const ist = V.priorTabelle(priors, DATEN.parameter, p);
    assert.equal(ist.length, soll.length);
    soll.forEach((zeile, i) => {
      assert.equal(ist[i].Annahme, zeile.Annahme);
      assert.equal(ist[i].Verteilung, zeile.Verteilung, `${zeile.Annahme}: Verteilung`);
      assert.equal(ist[i].Mittelwert, zeile.Mittelwert, `${zeile.Annahme}: Mittelwert`);
      assert.equal(ist[i].Intervall, zeile['90-%-Intervall'], `${zeile.Annahme}: Intervall`);
      assert.equal(ist[i].Literatur, zeile['Literatur (5.–95. Perzentil)'], `${zeile.Annahme}: Literatur`);
    });
  };
  vergleiche(p0, referenz.tabellen.standard);
  vergleiche(referenz.tabellen.verstellt.parameter, referenz.tabellen.verstellt.tabelle);
});

test('die Ziehungen haben die Werte der Regler als Mittelwert', () => {
  const n = 4000;
  const p = Object.assign({}, p0, { netz_verlust: 12, anteil_frisch: 70, anteil_kronen: 15, anteil_totholz: 10, anteil_kaskade: 5 });
  const z = V.priorZiehen(priors, p, n);
  for (const [id, x] of Object.entries(z)) {
    const mittel = x.reduce((s, v) => s + v, 0) / n;
    const sd = Math.sqrt(x.reduce((s, v) => s + (v - mittel) ** 2, 0) / (n - 1));
    const soll = nachId[id].familie === 'dirichlet' ? V.dirichletMittel(priors, p)[id] * 100 : p[id];
    assert.ok(Math.abs(mittel - soll) <= 4 * sd / Math.sqrt(n) + 1e-9, `${id}: Mittel ${mittel}, Regler ${soll}`);
  }
});

test('die Herkunftsanteile ergeben in jeder Ziehung 100 %', () => {
  const z = V.priorZiehen(priors, p0, 500);
  const ids = priors.filter((pr) => pr.familie === 'dirichlet').map((pr) => pr.id);
  for (let i = 0; i < 500; i++) nahe(ids.reduce((s, id) => s + z[id][i], 0), 100, 1e-12, 1e-9, `Ziehung ${i}`);
});

test('verstellt man einen Regler, ändern sich nur die Ziehungen dieser Annahme', () => {
  const a = V.priorZiehen(priors, p0, 200);
  const b = V.priorZiehen(priors, Object.assign({}, p0, { netz_verlust: 12 }), 200);
  for (const id of Object.keys(a)) {
    if (id === 'netz_verlust') {
      for (let i = 0; i < 200; i++) assert.ok(b[id][i] > a[id][i]);
    } else {
      assert.deepEqual(Array.from(b[id]), Array.from(a[id]), id);
    }
  }
});

test('ein Regler am Rand ergibt eine Annahme ohne Streuung', () => {
  const z = V.priorZiehen(priors, Object.assign({}, p0, { verj_minderung: 0, netz_bau: 0 }), 50);
  assert.ok(z.verj_minderung.every((x) => x === 0));
  assert.ok(z.netz_bau.every((x) => x === 0));
});

test('Monte-Carlo-Ergebnisse stimmen statistisch mit R überein', () => {
  const start = process.hrtime.bigint();
  const mc = V.mcRechnen(p0, priors, 1000);
  const sekunden = Number(process.hrtime.bigint() - start) / 1e9;
  console.log(`  1000 Ziehungen in ${sekunden.toFixed(2)} s`);
  const ist = V.mcQuantile(mc);
  for (const soll of referenz.mc.quantile) {
    const zeile = ist.find((z) => z.variante === soll.variante && z.jahr === soll.jahr);
    const breite = soll['q97.5'] - soll['q2.5'];
    for (const spalte of ['q2.5', 'q17', 'q50', 'q83', 'q97.5']) {
      assert.ok(Math.abs(zeile[spalte] - soll[spalte]) <= 0.1 * breite + 0.3,
        `${soll.variante} ${soll.jahr} J. ${spalte}: JS ${zeile[spalte].toFixed(1)}, R ${soll[spalte].toFixed(1)}`);
    }
  }
});

test('Perzentile und Rangkorrelation werden wie in R berechnet', () => {
  const q = referenz.quantil7;
  const sortiert = Float64Array.from(q.x).sort();
  reiheNahe(q.wk.map((w) => V.quantilSortiert(sortiert, w)), q.q, 1e-12, 1e-12, 'quantile type 7');
  nahe(V.spearman(Float64Array.from(referenz.spearman.x), Float64Array.from(referenz.spearman.y)),
    referenz.spearman.rho, 1e-12, 1e-12, 'Spearman');
});

test('der Einfluss lässt Annahmen ohne Streuung weg', () => {
  const mc = V.mcRechnen(Object.assign({}, p0, { verj_minderung: 0 }), priors, 60);
  const e = V.mcEinfluss(mc);
  assert.ok(!e.some((z) => z.id === 'verj_minderung'));
  assert.ok(e.every((z) => Number.isFinite(z.rho)));
});
