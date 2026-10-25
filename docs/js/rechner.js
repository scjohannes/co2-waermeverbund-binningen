// Monte-Carlo-Rechnung in einem eigenen Thread, damit die Regler nicht hängen.
importScripts('daten.js', 'format.js', 'modell.js', 'verteilung.js');

self.onmessage = function (ereignis) {
  var auftrag = ereignis.data;
  self.postMessage(Verteilung.mcZusammenfassen(auftrag.p, DATEN.priors, auftrag.n, auftrag.id));
};
