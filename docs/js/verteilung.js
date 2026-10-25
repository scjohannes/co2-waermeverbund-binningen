// Unsicherheit: Übertragung von R/unsicherheit.R.
// Der Regler legt den Mittelwert einer Verteilung fest, die Literatur ihre Stärke (in R
// berechnet und in daten.js mitgeliefert):
//   beta      Beta(m ν, (1 - m) ν), Konzentration ν
//   gamma     Gamma(k, k / m), Form k
//   normal    N(m, σ²)
//   dirichlet Dir(κ s), Konzentration κ
//   fest      Wert des Reglers
// Perzentile: Numerical Recipes (3. Aufl.) für Gamma und Beta, Wichura (AS 241) für Normal.
(function (g) {
  'use strict';

  var Modell = typeof require !== 'undefined' ? require('./modell.js') : g.Modell;
  var Format = typeof require !== 'undefined' ? require('./format.js') : g.Format;

  var EPS = 2.220446049250313e-16;
  var FPMIN = Number.MIN_VALUE / EPS;

  // ---- Gammafunktion und unvollständige Gamma- und Betafunktion ----

  var LANCZOS = [57.1562356658629235, -59.5979603554754912, 14.1360979747417471,
    -0.491913816097620199, 0.339946499848118887e-4, 0.465236289270485756e-4,
    -0.983744753048795646e-4, 0.158088703224912494e-3, -0.210264441724104883e-3,
    0.217439618115212643e-3, -0.164318106536763890e-3, 0.844182239838527433e-4,
    -0.261908384015814087e-4, 0.368991826595316234e-5];

  function gammln(xx) {
    var x = xx, y = xx, tmp = x + 5.24218750000000000;
    tmp = (x + 0.5) * Math.log(tmp) - tmp;
    var ser = 0.999999999999997092;
    for (var j = 0; j < 14; j++) ser += LANCZOS[j] / ++y;
    return tmp + Math.log(2.5066282746310005 * ser / x);
  }

  // Regularisierte untere unvollständige Gammafunktion P(a, x)
  function gammp(a, x) {
    if (x <= 0) return 0;
    var gln = gammln(a);
    if (x < a + 1) {
      var ap = a, del = 1 / a, sum = del;
      for (var i = 0; i < 100000; i++) {
        ++ap;
        del *= x / ap;
        sum += del;
        if (Math.abs(del) < Math.abs(sum) * EPS) break;
      }
      return sum * Math.exp(-x + a * Math.log(x) - gln);
    }
    var b = x + 1 - a, c = 1 / FPMIN, d = 1 / b, h = d;
    for (var k = 1; k < 100000; k++) {
      var an = -k * (k - a);
      b += 2;
      d = an * d + b;
      if (Math.abs(d) < FPMIN) d = FPMIN;
      c = b + an / c;
      if (Math.abs(c) < FPMIN) c = FPMIN;
      d = 1 / d;
      var dl = d * c;
      h *= dl;
      if (Math.abs(dl - 1) <= EPS) break;
    }
    return 1 - Math.exp(-x + a * Math.log(x) - gln) * h;
  }

  function betacf(a, b, x) {
    var qab = a + b, qap = a + 1, qam = a - 1, c = 1, d = 1 - qab * x / qap;
    if (Math.abs(d) < FPMIN) d = FPMIN;
    d = 1 / d;
    var h = d;
    for (var m = 1; m < 100000; m++) {
      var m2 = 2 * m;
      var aa = m * (b - m) * x / ((qam + m2) * (a + m2));
      d = 1 + aa * d; if (Math.abs(d) < FPMIN) d = FPMIN;
      c = 1 + aa / c; if (Math.abs(c) < FPMIN) c = FPMIN;
      d = 1 / d;
      h *= d * c;
      aa = -(a + m) * (qab + m) * x / ((a + m2) * (qap + m2));
      d = 1 + aa * d; if (Math.abs(d) < FPMIN) d = FPMIN;
      c = 1 + aa / c; if (Math.abs(c) < FPMIN) c = FPMIN;
      d = 1 / d;
      var del = d * c;
      h *= del;
      if (Math.abs(del - 1) <= EPS) break;
    }
    return h;
  }

  // Regularisierte unvollständige Betafunktion I_x(a, b)
  function betai(a, b, x) {
    if (x <= 0) return 0;
    if (x >= 1) return 1;
    var bt = Math.exp(gammln(a + b) - gammln(a) - gammln(b) + a * Math.log(x) + b * Math.log(1 - x));
    if (x < (a + 1) / (a + b + 2)) return bt * betacf(a, b, x) / a;
    return 1 - bt * betacf(b, a, 1 - x) / b;
  }

  // ---- Perzentile ----

  // Normalverteilung, Wichura (1988), Algorithmus AS 241 (PPND16)
  function qnorm(p) {
    var q = p - 0.5, r, x;
    if (Math.abs(q) <= 0.425) {
      r = 0.180625 - q * q;
      return q * (((((((r * 2509.0809287301226727 + 33430.575583588128105) * r + 67265.770927008700853) * r +
        45921.953931549871457) * r + 13731.693765509461125) * r + 1971.5909503065514427) * r +
        133.14166789178437745) * r + 3.387132872796366608) /
        (((((((r * 5226.495278852545925 + 28729.085735721942674) * r + 39307.89580009271061) * r +
          21213.794301586595867) * r + 5394.1960214247511077) * r + 687.1870074920579083) * r +
          42.313330701600911252) * r + 1);
    }
    r = q < 0 ? p : 1 - p;
    if (r <= 0) return q < 0 ? -Infinity : Infinity;
    r = Math.sqrt(-Math.log(r));
    if (r <= 5) {
      r -= 1.6;
      x = (((((((r * 7.7454501427834140764e-4 + 0.0227238449892691845833) * r + 0.24178072517745061177) * r +
        1.27045825245236838258) * r + 3.64784832476320460504) * r + 5.7694972214606914055) * r +
        4.6303378461565452959) * r + 1.42343711074968357734) /
        (((((((r * 1.05075007164441684324e-9 + 5.475938084995344946e-4) * r + 0.0151986665636164571966) * r +
          0.14810397642748007459) * r + 0.68976733498510000455) * r + 1.6763848301838038494) * r +
          2.05319162663775882187) * r + 1);
    } else {
      r -= 5;
      x = (((((((r * 2.01033439929228813265e-7 + 2.71155556874348757815e-5) * r + 0.0012426609473880784386) * r +
        0.026532189526576123093) * r + 0.29656057182850489123) * r + 1.7848265399172913358) * r +
        5.4637849111641143699) * r + 6.6579046435011037772) /
        (((((((r * 2.04426310338993978564e-15 + 1.4215117583164458887e-7) * r + 1.8463183175100546818e-5) * r +
          7.868691311456132591e-4) * r + 0.0148753612908506148525) * r + 0.13692988092273580531) * r +
          0.59983220655588793769) * r + 1);
    }
    return q < 0 ? -x : x;
  }

  // Gamma(a, 1): Numerical Recipes invgammp, Newton-Halley-Schritte
  function qgammaEins(p, a) {
    if (p <= 0 || a <= 0) return 0;
    if (p >= 1) return Infinity;
    var a1 = a - 1, gln = gammln(a), lna1 = 0, afac = 0, x, t, u;
    if (a > 1) {
      lna1 = Math.log(a1);
      afac = Math.exp(a1 * (lna1 - 1) - gln);
      var pp = p < 0.5 ? p : 1 - p;
      t = Math.sqrt(-2 * Math.log(pp));
      x = (2.30753 + t * 0.27061) / (1 + t * (0.99229 + t * 0.04481)) - t;
      if (p < 0.5) x = -x;
      x = Math.max(1e-3, a * Math.pow(1 - 1 / (9 * a) - x / (3 * Math.sqrt(a)), 3));
    } else {
      t = 1 - a * (0.253 + a * 0.12);
      x = p < t ? Math.pow(p / t, 1 / a) : 1 - Math.log(1 - (p - t) / (1 - t));
    }
    for (var j = 0; j < 100; j++) {
      if (x <= 0) return 0;
      var err = gammp(a, x) - p;
      t = a > 1 ? afac * Math.exp(-(x - a1) + a1 * (Math.log(x) - lna1)) : Math.exp(-x + a1 * Math.log(x) - gln);
      u = err / t;
      t = u / (1 - 0.5 * Math.min(1, u * ((a - 1) / x - 1)));
      x -= t;
      if (x <= 0) x = 0.5 * (x + t);
      if (Math.abs(t) < 1e-12 * x) break;
    }
    return x;
  }

  // Beta(a, b): Numerical Recipes invbetai; a = 0 bzw. b = 0 ergibt eine Punktmasse bei 0 bzw. 1
  function qbeta(p, a, b) {
    if (a === 0) return 0;
    if (b === 0) return 1;
    if (p <= 0) return 0;
    if (p >= 1) return 1;
    var a1 = a - 1, b1 = b - 1, x, t, u, w;
    if (a >= 1 && b >= 1) {
      var pp = p < 0.5 ? p : 1 - p;
      t = Math.sqrt(-2 * Math.log(pp));
      x = (2.30753 + t * 0.27061) / (1 + t * (0.99229 + t * 0.04481)) - t;
      if (p < 0.5) x = -x;
      var al = (x * x - 3) / 6;
      var h = 2 / (1 / (2 * a - 1) + 1 / (2 * b - 1));
      w = (x * Math.sqrt(al + h) / h) - (1 / (2 * b - 1) - 1 / (2 * a - 1)) * (al + 5 / 6 - 2 / (3 * h));
      x = a / (a + b * Math.exp(2 * w));
    } else {
      var lna = Math.log(a / (a + b)), lnb = Math.log(b / (a + b));
      t = Math.exp(a * lna) / a;
      u = Math.exp(b * lnb) / b;
      w = t + u;
      x = p < t / w ? Math.pow(a * w * p, 1 / a) : 1 - Math.pow(b * w * (1 - p), 1 / b);
    }
    var afac = -gammln(a) - gammln(b) + gammln(a + b);
    for (var j = 0; j < 100; j++) {
      if (x === 0 || x === 1) return x;
      var err = betai(a, b, x) - p;
      t = Math.exp(a1 * Math.log(x) + b1 * Math.log(1 - x) + afac);
      u = err / t;
      t = u / (1 - 0.5 * Math.min(1, u * (a1 / x - b1 / (1 - x))));
      x -= t;
      if (x <= 0) x = 0.5 * (x + t);
      if (x >= 1) x = 0.5 * (x + t + 1);
      if (Math.abs(t) < 1e-12 * x && j > 0) break;
    }
    return x;
  }

  // ---- Verteilungen der Annahmen ----

  // Parameter der Verteilung mit Mittelwert m (auf der Skala der Verteilung) und gegebener Stärke.
  function verteilung(familie, m, staerke) {
    if (familie === 'normal') return { mittel: m, sd: staerke };
    if (familie === 'gamma') return { form: staerke, rate: staerke / m };
    m = Math.min(Math.max(m, 0), 1);
    return { alpha: m * staerke, beta: (1 - m) * staerke };
  }

  function quantilVerteilung(familie, par, u) {
    if (familie === 'normal') return par.mittel + par.sd * qnorm(u);
    if (familie === 'gamma') return isFinite(par.rate) ? qgammaEins(u, par.form) / par.rate : 0;  // Mittelwert 0
    return qbeta(u, par.alpha, par.beta);
  }

  // Erwartungswerte der Herkunftsanteile aus den Reglern (wie im Modell auf 1 skaliert).
  function dirichletMittel(priors, p) {
    var ids = priors.filter(function (pr) { return pr.familie === 'dirichlet'; }).map(function (pr) { return pr.id; });
    var s = ids.map(function (id) { return p[id]; });
    var summe = s.reduce(function (x, y) { return x + y; }, 0);
    var aus = {};
    ids.forEach(function (id, j) { aus[id] = summe > 0 ? s[j] / summe : (j === 0 ? 1 : 0); });
    return aus;
  }

  // Perzentile einer Annahme beim aktuellen Wert des Reglers, in der Einheit des Reglers.
  function quantilPrior(pr, p, wk, priors) {
    if (pr.familie === 'dirichlet') {
      var m = dirichletMittel(priors, p)[pr.id];
      return wk.map(function (u) { return qbeta(u, pr.staerke * m, pr.staerke * (1 - m)) * pr.skala; });
    }
    var par = verteilung(pr.familie, p[pr.id] / pr.skala, pr.staerke);
    return wk.map(function (u) { return quantilVerteilung(pr.familie, par, u) * pr.skala; });
  }

  // ---- Zufallszahlen: ein eigener Strom je Annahme ----

  function streuwert(text) {   // cyrb53
    var h1 = 0xdeadbeef, h2 = 0x41c6ce57;
    for (var i = 0; i < text.length; i++) {
      var ch = text.charCodeAt(i);
      h1 = Math.imul(h1 ^ ch, 2654435761);
      h2 = Math.imul(h2 ^ ch, 1597334677);
    }
    h1 = Math.imul(h1 ^ (h1 >>> 16), 2246822507) ^ Math.imul(h2 ^ (h2 >>> 13), 3266489909);
    h2 = Math.imul(h2 ^ (h2 >>> 16), 2246822507) ^ Math.imul(h1 ^ (h1 >>> 13), 3266489909);
    return [h1 >>> 0, h2 >>> 0];
  }

  // n gleichverteilte Zahlen in (0, 1) aus sfc32
  function gleichverteilt(text, n) {
    var s = streuwert(text);
    var a = s[0], b = s[1], c = s[0] ^ 0x9e3779b9, d = 1;
    var aus = new Float64Array(n);
    for (var i = -12; i < n; i++) {
      a >>>= 0; b >>>= 0; c >>>= 0; d >>>= 0;
      var t = (a + b) | 0;
      a = b ^ (b >>> 9);
      b = (c + (c << 3)) | 0;
      c = (c << 21) | (c >>> 11);
      d = (d + 1) | 0;
      t = (t + d) | 0;
      c = (c + t) | 0;
      if (i >= 0) aus[i] = ((t >>> 0) + 0.5) / 4294967296;
    }
    return aus;
  }

  // n Ziehungen aller unsicheren Annahmen um die Werte der Regler, in den Einheiten der Regler.
  // Verstellt man einen Regler, ändern sich nur die Ziehungen dieser Annahme.
  function priorZiehen(priors, p, n, seed) {
    seed = seed === undefined ? 2028 : seed;
    var z = {};
    var dirichlet = [];
    priors.forEach(function (pr) {
      if (pr.familie === 'fest') return;
      var u = gleichverteilt(seed + ':' + pr.id, n);
      if (pr.familie === 'dirichlet') { dirichlet.push({ pr: pr, u: u }); return; }
      var par = verteilung(pr.familie, p[pr.id] / pr.skala, pr.staerke);
      var x = new Float64Array(n);
      for (var i = 0; i < n; i++) x[i] = quantilVerteilung(pr.familie, par, u[i]) * pr.skala;
      z[pr.id] = x;
    });
    if (dirichlet.length > 0) {
      var mittel = dirichletMittel(priors, p);
      var gammas = dirichlet.map(function (d) {
        var alpha = d.pr.staerke * mittel[d.pr.id];
        return Array.from(d.u, function (u) { return alpha > 0 ? qgammaEins(u, alpha) : 0; });
      });
      dirichlet.forEach(function (d, j) {
        var x = new Float64Array(n);
        for (var i = 0; i < n; i++) {
          var summe = 0;
          for (var k = 0; k < gammas.length; k++) summe += gammas[k][i];
          x[i] = gammas[j][i] / summe * d.pr.skala;
        }
        z[d.pr.id] = x;
      });
    }
    return z;
  }

  // ---- Monte-Carlo-Rechnung ----

  // Feste Grössen aus p, unsichere aus ihren Verteilungen um die Werte in p.
  // kumuliert[(i * Varianten + v) * Jahre + t]: kumulierte t CO2e der Ziehung i.
  function mcRechnen(p, priors, n, seed) {
    var z = priorZiehen(priors, p, n, seed);
    var ids = Object.keys(z);
    var V = Modell.VARIANTEN.length, J = Modell.JAHRE;
    var kumuliert = new Float64Array(n * V * J);
    var q = Object.assign({}, p);
    for (var i = 0; i < n; i++) {
      for (var k = 0; k < ids.length; k++) q[ids[k]] = z[ids[k]][i];
      var r = Modell.rechneKumuliert(q, J);
      for (var v = 0; v < V; v++) kumuliert.set(r[v], (i * V + v) * J);
    }
    return { n: n, ziehungen: z, kumuliert: kumuliert };
  }

  function spalte(mc, variante, jahr) {
    var V = Modell.VARIANTEN.length, J = Modell.JAHRE;
    var v = Modell.VARIANTEN.indexOf(variante);
    var aus = new Float64Array(mc.n);
    for (var i = 0; i < mc.n; i++) aus[i] = mc.kumuliert[(i * V + v) * J + jahr - 1];
    return aus;
  }

  // Perzentil wie quantile(type = 7) in R
  function quantilSortiert(sortiert, wk) {
    var h = (sortiert.length - 1) * wk, lo = Math.floor(h), hi = Math.min(lo + 1, sortiert.length - 1);
    return sortiert[lo] + (h - lo) * (sortiert[hi] - sortiert[lo]);
  }

  var INTERVALLE = [0.025, 0.17, 0.5, 0.83, 0.975];
  function spaltenname(wk) { return 'q' + String(Math.round(wk * 100000) / 1000); }

  // Perzentile der kumulierten Emissionen (kt) je Variante und Jahr.
  function mcQuantile(mc, jahre, wk) {
    jahre = jahre || Modell.HORIZONTE;
    wk = wk || INTERVALLE;
    var zeilen = [];
    jahre.forEach(function (j) {
      Modell.VARIANTEN.forEach(function (v) {
        var x = spalte(mc, v, j).sort();
        var zeile = { variante: v, jahr: j };
        wk.forEach(function (w) { zeile[spaltenname(w)] = quantilSortiert(x, w) / 1000; });
        zeilen.push(zeile);
      });
    });
    return zeilen;
  }

  function raenge(x) {   // mittlere Ränge bei Bindungen
    var idx = Array.from(x, function (_, i) { return i; }).sort(function (a, b) { return x[a] - x[b]; });
    var r = new Float64Array(x.length);
    for (var i = 0; i < idx.length;) {
      var j = i;
      while (j + 1 < idx.length && x[idx[j + 1]] === x[idx[i]]) j++;
      for (var k = i; k <= j; k++) r[idx[k]] = (i + j) / 2 + 1;
      i = j + 1;
    }
    return r;
  }

  function korrelation(a, b) {
    var n = a.length, ma = 0, mb = 0;
    for (var i = 0; i < n; i++) { ma += a[i]; mb += b[i]; }
    ma /= n; mb /= n;
    var sab = 0, saa = 0, sbb = 0;
    for (var k = 0; k < n; k++) {
      sab += (a[k] - ma) * (b[k] - mb);
      saa += (a[k] - ma) * (a[k] - ma);
      sbb += (b[k] - mb) * (b[k] - mb);
    }
    return sab / Math.sqrt(saa * sbb);
  }

  function spearman(x, y) { return korrelation(raenge(x), raenge(y)); }

  // Rangkorrelation jeder unsicheren Annahme mit den kumulierten Emissionen einer Variante.
  // Annahmen ohne Streuung (Regler am Rand) fallen weg.
  function mcEinfluss(mc, variante, jahre) {
    variante = variante || 'Plan IWB';
    jahre = jahre || [20, 50];
    var ids = Object.keys(mc.ziehungen).filter(function (id) {
      var x = mc.ziehungen[id];
      for (var i = 1; i < x.length; i++) if (x[i] !== x[0]) return true;
      return false;
    });
    var rangZiehung = {};
    ids.forEach(function (id) { rangZiehung[id] = raenge(mc.ziehungen[id]); });
    var aus = [];
    jahre.forEach(function (j) {
      var ry = raenge(spalte(mc, variante, j));
      ids.forEach(function (id) { aus.push({ id: id, jahr: j, rho: korrelation(rangZiehung[id], ry) }); });
    });
    return aus;
  }

  // Was die Website braucht: Perzentile für jedes Jahr und den Einfluss der Annahmen.
  function mcZusammenfassen(p, priors, n, id) {
    var mc = mcRechnen(p, priors, n);
    var jahre = [];
    for (var j = 1; j <= Modell.JAHRE; j++) jahre.push(j);
    return { id: id, n: n, band: mcQuantile(mc, jahre), einfluss: mcEinfluss(mc) };
  }

  // ---- Übersicht für den Reiter «Annahmen» ----

  function priorTabelle(priors, parameter, p) {
    var nachId = {};
    priors.forEach(function (pr) { nachId[pr.id] = pr; });
    var mittel = dirichletMittel(priors, p);
    var kurz = Format.kurz;
    return parameter.map(function (z) {
      var pr = nachId[z.id];
      var wert = p[z.id];
      var zeile = { Annahme: z.label, Einheit: z.einheit, Begruendung: pr.begruendung };
      if (pr.familie === 'fest') {
        zeile.Verteilung = 'fest'; zeile.Mittelwert = kurz(wert); zeile.Intervall = '–'; zeile.Literatur = '–';
        return zeile;
      }
      var q = quantilPrior(pr, p, [0.05, 0.95], priors);
      zeile.Intervall = kurz(q[0]) + ' – ' + kurz(q[1]);
      if (pr.familie === 'dirichlet') {
        zeile.Verteilung = 'Dirichlet(κ = ' + kurz(pr.staerke) + ')';
        zeile.Mittelwert = kurz(mittel[pr.id] * pr.skala);
        zeile.Literatur = '–';
        return zeile;
      }
      var par = verteilung(pr.familie, wert / pr.skala, pr.staerke);
      zeile.Verteilung = pr.familie === 'beta' ? 'Beta(α = ' + kurz(par.alpha) + ', β = ' + kurz(par.beta) + ')' :
        pr.familie === 'gamma' ? 'Gamma(k = ' + kurz(par.form) + ', ν = ' + kurz(par.rate) + ')' :
          'Normal(μ = ' + kurz(par.mittel) + ', σ = ' + kurz(par.sd) + ')';
      zeile.Mittelwert = kurz(wert);
      zeile.Literatur = kurz(pr.a) + ' – ' + kurz(pr.b);
      return zeile;
    });
  }

  var Verteilung = {
    qnorm: qnorm, qgammaEins: qgammaEins, qbeta: qbeta, gammp: gammp, betai: betai,
    verteilung: verteilung, quantilPrior: quantilPrior, dirichletMittel: dirichletMittel,
    priorZiehen: priorZiehen, mcRechnen: mcRechnen, mcQuantile: mcQuantile, mcEinfluss: mcEinfluss,
    mcZusammenfassen: mcZusammenfassen,
    quantilSortiert: quantilSortiert, spearman: spearman, priorTabelle: priorTabelle,
    INTERVALLE: INTERVALLE
  };
  if (typeof module !== 'undefined' && module.exports) module.exports = Verteilung; else g.Verteilung = Verteilung;
})(typeof self !== 'undefined' ? self : this);
