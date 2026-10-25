// Anteils-Regler und Zahlenformate wie in R/regler.R und R/grafiken.R bzw. R/unsicherheit.R.
'use strict';
const test = require('node:test');
const assert = require('node:assert/strict');
const { js, referenz } = require('./hilfe.js');

const Regler = js('regler.js');
const Format = js('format.js');

test('die Anteils-Regler werden wie in R umverteilt', () => {
  for (const f of referenz.umverteilen) {
    const werte = Object.assign({}, f.werte, { [f.geaendert]: f.neu });
    const ids = Object.keys(f.werte);
    const maximum = Object.fromEntries(ids.map((id, i) => [id, f.maximum[i]]));
    const ist = Regler.umverteilen(werte, f.geaendert, maximum, f.schritt);
    assert.deepEqual(ist, f.ergebnis, `${f.geaendert} = ${f.neu}`);
    assert.equal(Object.values(ist).reduce((s, v) => s + v, 0), 100);
  }
});

test('Zahlen werden wie in der Shiny-App formatiert', () => {
  const { zahlen, kurz, zahl } = referenz.formate;
  assert.deepEqual(zahlen.map(Format.kurz), kurz);
  for (const z of zahl) assert.deepEqual(zahlen.map((x) => Format.zahl(x, z.stellen)), z.text, `stellen = ${z.stellen}`);
});
