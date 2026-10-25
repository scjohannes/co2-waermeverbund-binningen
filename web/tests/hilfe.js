// Gemeinsame Hilfen für die Tests: Referenzwerte aus R (web/referenz.R) und die Website-Skripte.
'use strict';
const path = require('node:path');
const fs = require('node:fs');
const assert = require('node:assert/strict');

const js = (datei) => require(path.join(__dirname, '..', '..', 'docs', 'js', datei));

const referenz = JSON.parse(fs.readFileSync(path.join(__dirname, 'referenz.json'), 'utf8'));
const DATEN = js('daten.js');

// Zahlen gleich bis auf Rundungsfehler der Gleitkommarechnung
function nahe(ist, soll, relativ = 1e-9, absolut = 1e-9, text = '') {
  if (soll === null || soll === undefined) {
    assert.ok(ist === null || ist === undefined || Number.isNaN(ist), `${text}: erwartet leer, ist ${ist}`);
    return;
  }
  const grenze = absolut + relativ * Math.abs(soll);
  assert.ok(Math.abs(ist - soll) <= grenze, `${text}: ist ${ist}, soll ${soll}`);
}

function reiheNahe(ist, soll, relativ, absolut, text) {
  assert.equal(ist.length, soll.length, `${text}: Länge`);
  for (let i = 0; i < soll.length; i++) nahe(ist[i], soll[i], relativ, absolut, `${text}[${i}]`);
}

module.exports = { js, referenz, DATEN, nahe, reiheNahe };
