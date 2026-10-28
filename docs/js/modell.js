// CO2e-Modell für den Wärmeverbund Binningen: Übertragung von R/model.R.
// Energie in MWh, Emissionen in t CO2e, Zeit in Betriebsjahren ab Inbetriebnahme.
// Gezählt wird, wie viel CO2e jede Variante bis zu einem Zeitpunkt zusätzlich in die
// Atmosphäre gebracht hat. Beim Holz heisst das: Kamin-CO2 minus das, was der Wald
// inzwischen nachgewachsen ist bzw. was ohnehin verrottet wäre.
// Die Tests in web/tests/ vergleichen jede Funktion mit den Ergebnissen aus R.
(function (g) {
  'use strict';

  var JAHRE = 50;
  var START = 2028;
  var HORIZONTE = [10, 20, 30, 40, 50];

  var VARIANTEN = ['Plan IWB', 'Plan, Öl statt Holz', 'Erdwärmesonden + Öl', 'Erdwärmesonden',
    '100 % Holz', '100 % Gas', '100 % Öl'];

  var KOMPONENTEN = ['Öl/Gas: Verbrennung', 'Vorketten & Nebengase', 'Strom', 'Bau',
    'Holz: CO2 netto (Wald)'];

  var HOLZARTEN = {
    frisch: 'Zusätzlich gefällte lebende Bäume',
    kronen: 'Äste und Kronen (sonst verrottet)',
    totholz: 'Liegendes Schadholz (sonst verrottet)',
    kaskade: 'Holz, das sonst zu Produkten würde'
  };
  var MISCHUNG = 'Mischung im ersten Betriebsjahr';

  // Methan je kWh Erdgas (Heizwert rund 50 MJ/kg): 3,6 MJ / 50 MJ/kg = 72 g
  var METHAN_G_PRO_KWH = 72;

  // Bezugseingriff von 20 % des Vorrats. Die Wuchsminderung nach dem Eingriff (verj_minderung)
  // gilt für ihn, schwächere und stärkere Eingriffe mindern den Zuwachs im selben Verhältnis; die
  // Reaktion auf Auslichtung (auslichtung) ist der Mehr- oder Minderzuwachs nach ihm.
  var EINGRIFF_BEZUG = 20;

  // Suchbereich für die Form der Wachstumskurve theta
  var THETA_GRENZEN = [0.05, 20];

  var TECHNIKEN = ['holz', 'wrg', 'wp', 'oel', 'gas', 'luft', 'spitze'];

  function standardwerte(parameter) {
    var p = {};
    parameter.forEach(function (z) { p[z.id] = z.wert; });
    return p;
  }

  // Methan aus dem Schnitzelhaufen: zusätzliche t CO2e je t biogenem CO2 des geernteten Holzes.
  // Der Kohlenstoff ist im Kamin-CO2 schon als CO2 gezählt; hier kommt nur die Mehrwirkung als
  // Methan dazu. Biogenes Methan hat ein um 2,8 tieferes GWP als fossiles (IPCC AR6).
  function lagerMethanFaktor(p) {
    return 12 / 44 * p.lager_methan / 100 * (16 / 12 * (p.gwp_ch4 - 2.8) - 44 / 12);
  }

  // Strom-Emissionen in t CO2e/MWh je Betriebsjahr: linear bis 2050, danach konstant.
  function stromPfad(p, n) {
    var aus = new Float64Array(n);
    for (var t = 0; t < n; t++) {
      var w = Math.min(t / (2050 - START), 1);
      aus[t] = (p.strom_start + (p.strom_2050 - p.strom_start) * w) / 1000;
    }
    return aus;
  }

  // Kamin-CO2 und Energieinhalt der Holzschnitzel.
  function holzKennzahlen(p) {
    var u = p.wassergehalt / 100;
    var hu = p.hu_trocken - u / (1 - u) * 2.443;               // MJ je kg Trockenmasse, wie geliefert
    return {
      kamin: p.c_anteil / 100 * 44 / 12 / (hu / 3.6),          // t CO2 je MWh Brennstoff
      mwh_pro_m3: p.dichte * hu / 3.6                          // MWh Brennstoff je m³ Holz
    };
  }

  // Form der Wachstumskurve (theta) aus der Reaktion auf Auslichtung: Nach Entnahme von
  // EINGRIFF_BEZUG % des Vorrats wächst der Bestand um p.auslichtung % mehr oder weniger als ohne
  // Eingriff (ohne Klimawirkung und Wuchsminderung). Das Verhältnis fällt mit theta; gesucht wird
  // durch Halbieren, ausserhalb von THETA_GRENZEN gilt die nächste Grenze.
  function waldTheta(p) {
    var x = Math.min(p.vorrat_heute / p.vorrat_max, 0.95);
    var b = 1 - EINGRIFF_BEZUG / 100;
    function verhaeltnis(th) { return b * (1 - Math.pow(b * x, th)) / (1 - Math.pow(x, th)); }
    var ziel = 1 + p.auslichtung / 100;
    var unten = THETA_GRENZEN[0], oben = THETA_GRENZEN[1];
    for (var i = 0; i < 60; i++) {
      var mitte = (unten + oben) / 2;
      if (verhaeltnis(mitte) > ziel) unten = mitte; else oben = mitte;
    }
    return (unten + oben) / 2;
  }

  // Zusätzlich gefällte lebende Bäume: Anteil des verbrannten CO2, der nach a Jahren noch
  // zusätzlich in der Luft ist (Dauerwald-Bestand ohne und mit Eingriff, theta-logistisch).
  // Ergebnis: Feld der Länge n * n, Zeile = Erntejahr (Kohorte), Spalte = Jahre seit der Ernte.
  function antwortFrischholz(p, n) {
    var x = Math.min(p.vorrat_heute / p.vorrat_max, 0.95);      // heutiger Vorrat / Maximum
    var th = waldTheta(p);
    var gz = p.zuwachs / 100;
    var d0 = p.stoerung / 100;
    var h = p.eingriff / 100;
    // r so gewählt, dass der Nettozuwachs nach Störungen beim heutigen Vorrat gz beträgt
    var r = (gz + d0) * th / (1 - Math.pow(x, th));
    function wachstum(B) { return r * B * (1 - Math.pow(B * x, th)) / th; }  // B relativ zu heute
    // Wuchsminderung nach dem Eingriff, im Verhältnis der Eingriffsstärke zu EINGRIFF_BEZUG
    var minderung = Math.min(p.verj_minderung / 100 * p.eingriff / EINGRIFF_BEZUG, 1);

    var w = p.anteil_ungenutzt / 100;
    var klimaWirkung = p.klima_zuwachs / 100;
    var ungenutzt = new Float64Array(n).fill(1);              // ohne Heizwerk, ohne Nutzung
    var ungenutztEingriff = new Float64Array(n).fill(1 - h);
    var bisherEingriff = new Float64Array(n).fill(1 - h);     // mit Heizwerk, bisherige Nutzung läuft weiter
    var differenz = new Float64Array(n * n);
    var wachstumHeute = wachstum(1);
    for (var a = 0; a < n; a++) {
      var verjuengung = a < p.verj_jahre ? 1 - minderung : 1;
      for (var k = 0; k < n; k++) {
        differenz[k * n + a] = (w * (ungenutzt[k] - ungenutztEingriff[k]) + (1 - w) * (1 - bisherEingriff[k])) / h;
        var klima = 1 + klimaWirkung * Math.min((k + a) / JAHRE, 1);
        // bisherige Nutzung: entnimmt, was der Wald beim heutigen Vorrat netto zuwächst
        var bisherigeNutzung = klima * wachstumHeute - d0;
        var U = ungenutzt[k], UE = ungenutztEingriff[k], BE = bisherEingriff[k];
        // ein Vorrat kann nicht unter null sinken
        ungenutzt[k] = Math.max(U + klima * wachstum(U) - d0 * U * U * U, 0);
        ungenutztEingriff[k] = Math.max(UE + klima * verjuengung * wachstum(UE) - d0 * UE * UE * UE, 0);
        bisherEingriff[k] = Math.max(BE + klima * verjuengung * wachstum(BE) - d0 * BE * BE * BE -
          bisherigeNutzung, 0);
      }
    }

    // Verbrannt wird der oberirdische Teil; Stöcke und Wurzeln bleiben und zersetzen sich.
    // Bodenkohlenstoff: Verlust in den ersten 10 Jahren, danach Erholung um boden_erholung % des
    // Verlusts pro Jahr (0 = dauerhaft)
    var rho = p.wurzel_anteil / 100;
    var zusatz = new Float64Array(n);
    for (var a2 = 0; a2 < n; a2++) {
      var wurzeln = rho / (1 - rho) * Math.pow(2, -a2 / p.wurzel_hwz);
      var erholung = Math.max(1 - p.boden_erholung / 100 * Math.max(a2 - 10, 0), 0);
      var boden = p.boden_verlust / 100 * Math.min(a2 / 10, 1) * erholung;
      zusatz[a2] = boden - wurzeln;
    }
    var aus = new Float64Array(n * n);
    for (var i = 0; i < n * n; i++) aus[i] = differenz[i] / (1 - rho) + zusatz[i % n];
    return aus;
  }

  // Gefällte lebende Bäume, im ersten Betriebsjahr geerntet: Anteil ihres CO2, der nach
  // WALD_ALTER Jahren noch zusätzlich in der Luft ist.
  var WALD_ALTER = [10, 20, 30, 50];

  function frischholzAlter(p) {
    var f = antwortFrischholz(p, WALD_ALTER[WALD_ALTER.length - 1] + 1);
    return WALD_ALTER.map(function (alter) { return f[alter]; });
  }

  // Zur Erklärung im Reiter «Holz & Wald»: Zuwachs vor Störungen und Störungsverluste in m³/ha
  // pro Jahr beim heutigen Vorrat und gleich nach dem Eingriff (ohne Klimawirkung und
  // Wuchsminderung), der Vorrat, bei dem der Wald am meisten zuwächst, und der Vorrat, bei dem
  // ein ungenutzter Wald netto nicht mehr wächst. Wie in antwortFrischholz().
  function waldWachstum(p) {
    var x = Math.min(p.vorrat_heute / p.vorrat_max, 0.95);
    var th = waldTheta(p);
    var d0 = p.stoerung / 100;
    var r = (p.zuwachs / 100 + d0) * th / (1 - Math.pow(x, th));
    function zuwachs(B) { return p.vorrat_heute * r * B * (1 - Math.pow(B * x, th)) / th; }
    function verlust(B) { return p.vorrat_heute * d0 * B * B * B; }
    // Gleichgewicht zwischen heute und dem Maximum, durch Halbieren; ohne Störungen beim Maximum
    var unten = 1, oben = 1 / x;
    for (var i = 0; i < 60; i++) {
      var mitte = (unten + oben) / 2;
      if (zuwachs(mitte) > verlust(mitte)) unten = mitte; else oben = mitte;
    }
    var nach = 1 - p.eingriff / 100;
    return {
      theta: th,
      vorrat_spitze: p.vorrat_heute / x * Math.pow(1 + th, -1 / th),
      vorrat_gleichgewicht: p.vorrat_heute * (unten + oben) / 2,
      zuwachs_heute: zuwachs(1), zuwachs_eingriff: zuwachs(nach),
      verlust_heute: verlust(1), verlust_eingriff: verlust(nach)
    };
  }

  // Restholz, das sonst im Wald verrotten würde: Anteil, der ohne Nutzung noch im Wald
  // (oder dauerhaft im Boden) wäre und darum durch das Verbrennen zusätzlich in der Luft ist.
  function antwortZerfall(hwz, humus, n) {
    var aus = new Float64Array(n);
    for (var a = 0; a < n; a++) aus[a] = humus + (1 - humus) * Math.pow(2, -a / hwz);
    return aus;
  }

  // Holz, das ohne Heizwerk zu Produkten würde: Ein Teil wäre als Bauholz oder Platten
  // gespeichert geblieben und zerfällt mit deren Halbwertszeit; der Rest wäre ohnehin verbrannt.
  function antwortKaskade(p, n) {
    var aus = new Float64Array(n);
    for (var a = 0; a < n; a++) aus[a] = p.produkt_anteil / 100 * Math.pow(2, -a / p.hwz_produkt);
    return aus;
  }

  // Herkunft der zusätzlichen Holzmenge je Betriebsjahr; die vier Anteile summieren auf 1.
  // Die Schadholz-Welle ersetzt nur zusätzlich gefällte Bäume, höchstens deren ganzen Anteil.
  function herkunftAnteile(p, n) {
    var s = [p.anteil_frisch, p.anteil_kronen, p.anteil_totholz, p.anteil_kaskade];
    var summe = s[0] + s[1] + s[2] + s[3];
    s = summe > 0 ? s.map(function (v) { return v / summe; }) : [1, 0, 0, 0];
    var aus = { frisch: new Float64Array(n), kronen: new Float64Array(n),
      totholz: new Float64Array(n), kaskade: new Float64Array(n) };
    for (var t = 0; t < n; t++) {
      var welle = Math.min(p.welle_zusatz / 100 * Math.max(1 - t / p.welle_jahre, 0), s[0]);
      aus.frisch[t] = s[0] - welle;
      aus.kronen[t] = s[1];
      aus.totholz[t] = s[2] + welle;
      aus.kaskade[t] = s[3];
    }
    return aus;
  }

  // Biogenes CO2, das am Ende jedes Betriebsjahrs noch zusätzlich in der Luft ist.
  function biogenKumuliert(kamin, anteile, wald) {
    var n = kamin.length;
    var aus = new Float64Array(n);
    for (var t = 0; t < n; t++) {
      var summe = 0;
      for (var j = 0; j <= t; j++) {
        var a = t - j;
        var f = anteile.frisch[j] * wald.frisch[j * n + a] + anteile.kronen[j] * wald.kronen[a] +
          anteile.totholz[j] * wald.totholz[a] + anteile.kaskade[j] * wald.kaskade[a];
        summe += kamin[j] * f;
      }
      aus[t] = summe;
    }
    return aus;
  }

  // Anteile an der gelieferten Wärme je Betriebsjahr, und welche Teile über das Netz kommen.
  // Techniken: Holzkessel, Wärmerückgewinnung, zentrale Wärmepumpe (Plan bzw. Erdwärmesonden),
  // Öl- und Gaskessel, Luft-Wärmepumpen als Ersatz von Einzelheizungen, Spitzenlast-Wärmepumpen
  // in den Häusern.
  function varianteDef(p, variante, n) {
    var anteile = {};
    TECHNIKEN.forEach(function (k) { anteile[k] = new Float64Array(n); });
    var netz = {};
    TECHNIKEN.forEach(function (k) { netz[k] = true; });
    var jaz = p.jaz_erdsonde;
    var t;
    if (variante === 'Plan IWB' || variante === 'Plan, Öl statt Holz') {
      var s = { holz: p.plan_holz, wrg: p.plan_wrg, wp: p.plan_wp, oel: p.plan_oel };
      if (variante === 'Plan, Öl statt Holz') { s.oel += s.holz; s.holz = 0; }
      var summe = Math.max(s.holz + s.wrg + s.wp + s.oel, 1e-9);
      ['holz', 'wrg', 'wp', 'oel'].forEach(function (k) { anteile[k].fill(s[k] / summe); });
      jaz = p.jaz_plan;
    } else if (variante === 'Erdwärmesonden + Öl') {
      anteile.oel.fill(p.erd_oel / 100);
      anteile.wp.fill(1 - p.erd_oel / 100);
    } else if (variante === 'Erdwärmesonden') {
      anteile.spitze.fill(p.erd_oel / 100);
      anteile.wp.fill(1 - p.erd_oel / 100);
      netz.spitze = false;
    } else if (variante === '100 % Holz') {
      anteile.holz.fill(1);                       // ohne Wärmerückgewinnung
    } else {
      var kessel = variante === '100 % Gas' ? 'gas' : 'oel';
      for (t = 0; t < n; t++) {
        var ersetzt = t + 1 > p.fossil_laufzeit;   // danach Luft-Wärmepumpen
        anteile[kessel][t] = ersetzt ? 0 : 1;
        anteile.luft[t] = ersetzt ? 1 : 0;
      }
      TECHNIKEN.forEach(function (k) { netz[k] = p.fossil_dezentral < 0.5; });
    }
    return { anteile: anteile, netz: netz, jaz: jaz };
  }

  // Biogenes CO2 einer Variante, das am Ende jedes Jahres noch zusätzlich in der Luft ist.
  // Bei gleichbleibender Holzmenge genügt die vorberechnete Antwort auf 1 t pro Jahr.
  function biogenVariante(kaminHolz, wald) {
    var n = kaminHolz.length;
    var alleNull = true, gleich = true;
    for (var t = 0; t < n; t++) {
      if (kaminHolz[t] !== 0) alleNull = false;
      if (kaminHolz[t] !== kaminHolz[0]) gleich = false;
    }
    if (alleNull) return new Float64Array(n);
    if (gleich) return wald.einheit.map(function (v) { return v * kaminHolz[0]; });
    return biogenKumuliert(kaminHolz, wald.anteile, wald);
  }

  // Jährliche Emissionen einer Variante, aufgeteilt in die KOMPONENTEN (t CO2e).
  // komp[k][t]: Komponente k im Betriebsjahr t.
  function varianteKomponenten(p, variante, wald, n) {
    var d = varianteDef(p, variante, n);
    var a = d.anteile, netz = d.netz;
    // erzeugte Wärme je Technik (MWh); was über das Netz geht, trägt dessen Verluste
    var verlustFaktor = 1 / (1 - p.netz_verlust / 100);
    var faktor = {}, imNetz = {};
    for (var i = 0; i < TECHNIKEN.length; i++) {
      var k = TECHNIKEN[i];
      faktor[k] = p.q_del * (netz[k] ? verlustFaktor : 1);
      imNetz[k] = netz[k] ? 1 : 0;
    }
    var methan = p.gas_leck / 100 * METHAN_G_PRO_KWH * p.gwp_ch4;  // g CO2e je kWh Gas
    var lagerMethan = lagerMethanFaktor(p);
    var etaHolz = p.eta_holz / 100, ernte = 1 - p.dm_verlust / 100;
    var etaOel = p.eta_oel / 100, etaGas = p.eta_gas / 100;
    var spitzenZuschlag = 1 + p.strom_spitze / 100;
    var qDel = p.q_del, jaz = d.jaz, jazLuft = p.jaz_luft, jazSpitze = p.jaz_spitze;
    var wrgStrom = p.wrg_strom, netzStrom = p.netz_strom / 100;
    var oelDirekt = p.oel_direkt, gasDirekt = p.gas_direkt, oelVorkette = p.oel_vorkette;
    var gasVorkette = p.gas_vorkette + methan, holzVorkette = p.holz_vorkette + p.lager_n2o;
    var holzNonco2 = p.holz_nonco2, wpBau = p.wp_bau, fossilBau = p.fossil_bau, netzBau = p.netz_bau;
    var kamin = wald.kamin, stromFaktor = wald.strom;
    var komp = [new Float64Array(n), new Float64Array(n), new Float64Array(n), new Float64Array(n),
      new Float64Array(n)];
    var kaminHolz = new Float64Array(n), holz = new Float64Array(n), geerntet = new Float64Array(n);
    for (var t = 0; t < n; t++) {
      var eHolz = a.holz[t] * faktor.holz, eWrg = a.wrg[t] * faktor.wrg, eWp = a.wp[t] * faktor.wp;
      var eOel = a.oel[t] * faktor.oel, eGas = a.gas[t] * faktor.gas, eLuft = a.luft[t] * faktor.luft;
      var eSpitze = a.spitze[t] * faktor.spitze;
      var ueberNetz = qDel * (imNetz.holz * a.holz[t] + imNetz.wrg * a.wrg[t] + imNetz.wp * a.wp[t] +
        imNetz.oel * a.oel[t] + imNetz.gas * a.gas[t] + imNetz.luft * a.luft[t] + imNetz.spitze * a.spitze[t]);
      holz[t] = eHolz / etaHolz;                               // MWh Brennstoff im Kessel
      geerntet[t] = holz[t] / ernte;                           // inklusive Verlust im Lager
      var oel = eOel / etaOel;
      var gas = eGas / etaGas;
      var strom = eWp / jaz + eLuft / jazLuft + eWrg * wrgStrom + ueberNetz * netzStrom;  // MWh Strom
      var stromSpitze = eSpitze / jazSpitze;                   // MWh Strom an Spitzentagen
      kaminHolz[t] = geerntet[t] * kamin;                      // t biogenes CO2 aus Kamin und Lager

      // MWh mal g/kWh ergibt kg; geteilt durch 1000 ergibt t
      komp[0][t] = (oel * oelDirekt + gas * gasDirekt) / 1000;
      komp[1][t] = (oel * oelVorkette + gas * gasVorkette + geerntet[t] * holzVorkette +
        holz[t] * holzNonco2) / 1000 + kaminHolz[t] * lagerMethan;
      komp[2][t] = (strom + stromSpitze * spitzenZuschlag) * stromFaktor[t];
      komp[3][t] = ((eWp + eLuft + eSpitze) * wpBau + (eHolz + eOel + eGas) * fossilBau +
        ueberNetz * netzBau) / 1000;
    }
    var bio = biogenVariante(kaminHolz, wald);
    for (var t2 = 0; t2 < n; t2++) komp[4][t2] = bio[t2] - (t2 > 0 ? bio[t2 - 1] : 0);
    return { komp: komp, kaminHolz: kaminHolz, holz: holz, geerntet: geerntet };
  }

  // Alles, was für alle Varianten gleich ist: Holzkennzahlen, Strompfad, Waldantworten und
  // das biogene CO2, das von 1 t CO2 pro Betriebsjahr am Ende jedes Jahres noch in der Luft ist.
  function waldVorbereiten(p, n) {
    var hk = holzKennzahlen(p);
    var wald = {
      kamin: hk.kamin,
      strom: stromPfad(p, n),
      anteile: herkunftAnteile(p, n),
      frisch: antwortFrischholz(p, n),
      kronen: antwortZerfall(p.hwz_kronen, p.humus_anteil / 100, n),
      totholz: antwortZerfall(p.hwz_totholz, p.humus_anteil / 100, n),
      kaskade: antwortKaskade(p, n)
    };
    wald.einheit = biogenKumuliert(new Float64Array(n).fill(1), wald.anteile, wald);
    return { hk: hk, wald: wald };
  }

  // Schlanke Rechnung für viele Ziehungen: kumulierte t CO2e je Variante (Feld mit n Jahren).
  function rechneKumuliert(p, n) {
    n = n || JAHRE;
    var wald = waldVorbereiten(p, n).wald;
    return VARIANTEN.map(function (v) {
      var komp = varianteKomponenten(p, v, wald, n).komp;
      var aus = new Float64Array(n), summe = 0;
      for (var t = 0; t < n; t++) {
        summe += komp[0][t] + komp[1][t] + komp[2][t] + komp[3][t] + komp[4][t];
        aus[t] = summe;
      }
      return aus;
    });
  }

  // Kennzahlen zum Holz im Plan: Holzmenge, Kamin-CO2 und ab wann Holzwärme besser ist
  // als Wärme aus Öl oder Gas (pro kWh aus dem Holzkessel, ohne Wärmerückgewinnung).
  function holzDiagnose(p, hk, wald, plan, n) {
    var m3Geerntet = plan.geerntet[0] / hk.mwh_pro_m3;
    var m3Geliefert = plan.holz[0] / hk.mwh_pro_m3;
    var eta = p.eta_holz / 100;
    var ernte = 1 / (1 - p.dm_verlust / 100);
    var kaminW = hk.kamin / eta * ernte;                       // Kamin und Lager
    var nebenW = ((p.holz_vorkette + p.lager_n2o) * ernte + p.holz_nonco2) / 1000 / eta +
      kaminW * lagerMethanFaktor(p);
    var oelW = (p.oel_direkt + p.oel_vorkette) / 1000 / (p.eta_oel / 100);
    var gasW = (p.gas_direkt + p.gas_vorkette + p.gas_leck / 100 * METHAN_G_PRO_KWH * p.gwp_ch4) /
      1000 / (p.eta_gas / 100);
    var schwellen = { '100 % Öl': (oelW - nebenW) / kaminW, '100 % Gas': (gasW - nebenW) / kaminW };

    // Durchschnittlicher Anteil noch in der Luft bei gleichbleibendem Dauerbetrieb
    var schnitt = wald.einheit.map(function (v, t) { return v / (t + 1); });
    var abJahr = {};
    Object.keys(schwellen).forEach(function (name) {
      abJahr[name] = null;
      for (var t = 0; t < n; t++) {
        if (schnitt[t] <= schwellen[name]) { abJahr[name] = t + 1; break; }
      }
    });

    var einzeln = {
      frisch: wald.frisch.slice(0, n), kronen: wald.kronen, totholz: wald.totholz, kaskade: wald.kaskade
    };
    var kurven = {};
    Object.keys(HOLZARTEN).forEach(function (k) { kurven[HOLZARTEN[k]] = Array.from(einzeln[k]); });
    kurven[MISCHUNG] = Array.from({ length: n }, function (_, a) {
      return wald.anteile.frisch[0] * einzeln.frisch[a] + wald.anteile.kronen[0] * einzeln.kronen[a] +
        wald.anteile.totholz[0] * einzeln.totholz[a] + wald.anteile.kaskade[0] * einzeln.kaskade[a];
    });

    return {
      m3: m3Geerntet,
      srm: m3Geliefert * p.srm_faktor,
      lkw_pro_jahr: m3Geliefert * p.srm_faktor / p.lkw_srm,
      kamin_g_kwh: kaminW * 1000,
      schwellen: schwellen,
      ab_jahr: abJahr,
      schnitt: Array.from(schnitt),
      kurven: kurven
    };
  }

  // Alle Ergebnisse für die Grafiken: jährliche und kumulierte Werte je Komponente,
  // Summen je Variante (mit Jahr 0) und Kennzahlen zum Holz.
  function rechneAlles(p, n) {
    n = n || JAHRE;
    var vorbereitet = waldVorbereiten(p, n);
    var wald = vorbereitet.wald;
    var varianten = {};
    var plan = null;
    VARIANTEN.forEach(function (v) {
      var r = varianteKomponenten(p, v, wald, n);
      var kumuliert = r.komp.map(function (reihe) {
        var s = 0;
        return Array.from(reihe, function (x) { s += x; return s; });
      });
      var summe = [0];
      for (var t = 0; t < n; t++) {
        summe.push(kumuliert[0][t] + kumuliert[1][t] + kumuliert[2][t] + kumuliert[3][t] + kumuliert[4][t]);
      }
      var brutto = [0], b = 0;
      for (var t2 = 0; t2 < n; t2++) {
        b += r.komp[0][t2] + r.komp[1][t2] + r.komp[2][t2] + r.komp[3][t2] + r.kaminHolz[t2];
        brutto.push(b);
      }
      varianten[v] = {
        jaehrlich: r.komp.map(function (reihe) { return Array.from(reihe); }),
        kumuliert: kumuliert,
        summe: summe,
        brutto: brutto
      };
      if (v === 'Plan IWB') plan = r;
    });
    return {
      varianten: varianten,
      holz: holzDiagnose(p, vorbereitet.hk, wald, plan, n)
    };
  }

  var Modell = {
    JAHRE: JAHRE, START: START, HORIZONTE: HORIZONTE, VARIANTEN: VARIANTEN,
    KOMPONENTEN: KOMPONENTEN, HOLZARTEN: HOLZARTEN, MISCHUNG: MISCHUNG,
    standardwerte: standardwerte, lagerMethanFaktor: lagerMethanFaktor, stromPfad: stromPfad,
    holzKennzahlen: holzKennzahlen, antwortFrischholz: antwortFrischholz,
    WALD_ALTER: WALD_ALTER, frischholzAlter: frischholzAlter, waldWachstum: waldWachstum,
    waldTheta: waldTheta,
    antwortZerfall: antwortZerfall, antwortKaskade: antwortKaskade,
    herkunftAnteile: herkunftAnteile, biogenKumuliert: biogenKumuliert,
    varianteDef: varianteDef, varianteKomponenten: varianteKomponenten,
    waldVorbereiten: waldVorbereiten, rechneKumuliert: rechneKumuliert, rechneAlles: rechneAlles
  };
  if (typeof module !== 'undefined' && module.exports) module.exports = Modell; else g.Modell = Modell;
})(typeof self !== 'undefined' ? self : this);
