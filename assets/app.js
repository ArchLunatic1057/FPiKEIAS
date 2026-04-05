(() => {
  "use strict";

  const num = (id) => {
    const el = document.getElementById(id);
    if (!el) return 0;
    const v = Number(el.value);
    return Number.isFinite(v) ? v : 0;
  };

  const money = (v, currency = "RUB") => {
    const n = Number(v);
    if (!Number.isFinite(n)) return "—";
    return `${n.toLocaleString("ru-RU", { minimumFractionDigits: 2, maximumFractionDigits: 2 })} ${currency}`;
  };

  const setHtml = (id, html) => {
    const el = document.getElementById(id);
    if (el) el.innerHTML = html;
  };

  // ---------------- Top navigation ----------------
  const modules = [
    { id: "module1", name: "Модуль 1" },
    { id: "module2", name: "Модуль 2" },
    { id: "module3", name: "Модуль 3" },
    { id: "module4", name: "Модуль 4" },
    { id: "module5", name: "Модуль 5" },
    { id: "module6", name: "Модуль 6" },
    { id: "module7", name: "Модуль 7" },
    { id: "module8", name: "Модуль 8" },
  ];

  function initTopTabs() {
    const topTabsSelect = document.getElementById("topTabsSelect");
    if (!topTabsSelect) return;

    topTabsSelect.innerHTML = "";
    modules.forEach((m, i) => {
      const opt = document.createElement("option");
      opt.value = m.id;
      opt.textContent = m.name;
      if (i === 0) opt.selected = true;
      topTabsSelect.appendChild(opt);
    });

    topTabsSelect.addEventListener("change", () => {
      showModule(topTabsSelect.value);
    });
  }

  function showModule(id) {
    document
      .querySelectorAll(".module")
      .forEach((s) => s.classList.remove("active"));
    const moduleEl = document.getElementById(id);
    if (moduleEl) moduleEl.classList.add("active");

    const topTabsSelect = document.getElementById("topTabsSelect");
    if (topTabsSelect && topTabsSelect.value !== id) {
      topTabsSelect.value = id;
    }

    window.scrollTo({ top: 0, behavior: "smooth" });
  }

  // ---------------- Module 1 ----------------
  const EN1 = 0.2;
  const KP = {
    1: 1.0,
    2: 0.4762,
    3: 0.3021,
    4: 0.2155,
    5: 0.1638,
    6: 0.1296,
    7: 0.1054,
    8: 0.0874,
    9: 0.0736,
    10: 0.0627,
    11: 0.054,
    12: 0.0468,
    13: 0.0408,
    14: 0.0352,
    15: 0.0315,
  };
  const ALPHA_PREV = { 1: 1.1, 2: 1.21, 3: 1.331, 4: 1.464, 5: 1.611 };
  const ALPHA_NEXT = { 1: 0.9091, 2: 0.8264, 3: 0.7513, 4: 0.683, 5: 0.6209 };

  function initModule1Tabs() {
    document.querySelectorAll("[data-m1]").forEach((b) => {
      b.addEventListener("click", () => {
        const key = b.getAttribute("data-m1");
        document
          .querySelectorAll(".m1-panel")
          .forEach((p) => p.classList.add("hidden"));
        const panel = document.getElementById(`m1-${key}`);
        if (panel) panel.classList.remove("hidden");
      });
    });
  }

  window.calcM1Eff = () => {
    const P = num("m1eP");
    const K = num("m1eK");
    const R = num("m1eR");

    const E = P - K;
    const Ek = R !== 0 ? P / R : Infinity;
    const T = P !== 0 ? R / P : Infinity;

    setHtml(
      "m1eOut",
      `
      <div class="kpis">
        <div class="kpi"><div class="k">Э</div><div>${money(E, "тыс. руб.")}</div></div>
        <div class="kpi"><div class="k">Eк</div><div>${Number.isFinite(Ek) ? Ek.toFixed(4) : "∞"}</div></div>
        <div class="kpi"><div class="k">T (лет)</div><div>${Number.isFinite(T) ? T.toFixed(2) : "∞"}</div></div>
      </div>
    `,
    );
  };

  window.calcM1Economy = () => {
    const p1 = num("m1p1");
    const p2 = num("m1p2");
    const p3 = num("m1p3");
    const dt = num("m1dt");
    const total = (p1 + p2 + p3) * (1 + EN1 * dt);
    setHtml("m1ecoOut", `<b>П = ${money(total, "RUB/год")}</b>`);
  };

  window.calcM1Total = () => {
    const I2 = num("m1I2");
    const P = num("m1Ps");
    const method = (
      document.getElementById("m1TotalMethod")?.value || "table"
    ).toLowerCase();

    let kp = 0;
    let tslInfo = "";

    if (method === "formula") {
      const tslF = num("m1TslF");
      const denom = Math.pow(1 + EN1, tslF) - 1;
      kp = denom !== 0 ? EN1 / denom : Infinity;
      tslInfo = `Tсл (формула): ${tslF}`;
    } else {
      const tsl = Math.max(1, Math.min(15, Math.round(num("m1Tsl"))));
      kp = KP[tsl] ?? 0;
      tslInfo = `Tсл (таблица): ${tsl}`;
    }

    const K = I2 + ((Number.isFinite(kp) ? kp : 0) + EN1) * P;

    setHtml(
      "m1totOut",
      `
      <div class="kpis">
        <div class="kpi"><div class="k">${tslInfo}</div><div>${method === "formula" ? "Формула (6)" : "Таблица 1"}</div></div>
        <div class="kpi"><div class="k">kр</div><div>${Number.isFinite(kp) ? kp.toFixed(4) : "∞"}</div></div>
        <div class="kpi"><div class="k">K</div><div>${money(K)}</div></div>
      </div>
    `,
    );
  };

  window.calcM1Period = () => {
    const txt = document.getElementById("m1periodTxt");
    const lines = (txt?.value || "").trim().split("\n").filter(Boolean);

    let P0 = 0;
    let K0 = 0;
    const rows = [];

    lines.forEach((ln) => {
      const [yS, pS, kS] = ln.split(",").map((s) => s.trim());
      const y = Number(yS);
      const p = Number(pS);
      const k = Number(kS);
      if (!Number.isFinite(y) || !Number.isFinite(p) || !Number.isFinite(k))
        return;

      const a =
        y < 0
          ? (ALPHA_PREV[Math.abs(y)] ?? 1)
          : y > 0
            ? (ALPHA_NEXT[y] ?? 1)
            : 1;
      P0 += p * a;
      K0 += k * a;
      rows.push(
        `<tr><td>${y}</td><td>${money(p)}</td><td>${money(k)}</td><td>${a.toFixed(4)}</td></tr>`,
      );
    });

    const omega = P0 - K0;
    setHtml(
      "m1perOut",
      `
      <table>
        <thead><tr><th>Год</th><th>Пt</th><th>Кt</th><th>αt</th></tr></thead>
        <tbody>${rows.join("")}</tbody>
      </table>
      <p><b>П0:</b> ${money(P0)} · <b>К0:</b> ${money(K0)} · <b>Ω:</b> ${money(omega)}</p>
    `,
    );
  };

  window.calcM1Preprod = () => {
    const method = (
      document.getElementById("m1PreMethod")?.value || "components"
    ).toLowerCase();

    if (method === "estimate") {
      const tpr = num("m1tpr");
      const cd = num("m1cd");
      const v = tpr * cd;
      setHtml(
        "m1preOut",
        `
        <div class="kpis">
          <div class="kpi"><div class="k">Метод</div><div>По смете (формула 12)</div></div>
          <div class="kpi"><div class="k">tпр × Cд</div><div>${tpr.toFixed(2)} × ${money(cd)}</div></div>
          <div class="kpi"><div class="k">Пии</div><div>${money(v)}</div></div>
        </div>
      `,
      );
      return;
    }

    const pip = num("m1pip");
    const ppo = num("m1ppo");
    const pio = num("m1pio");
    const pbb = num("m1pbb");
    const v = pip + ppo + pio + pbb;

    setHtml(
      "m1preOut",
      `
      <div class="kpis">
        <div class="kpi"><div class="k">Метод</div><div>По составляющим (формула 11)</div></div>
        <div class="kpi"><div class="k">Пип + Ппо + Пио + Пвв</div><div>${money(pip)} + ${money(ppo)} + ${money(pio)} + ${money(pbb)}</div></div>
        <div class="kpi"><div class="k">Пии</div><div>${money(v)}</div></div>
      </div>
    `,
    );
  };

  window.calcM1Capital = () => {
    const baseSum =
      num("m1r1") +
      num("m1r2") +
      num("m1r3") +
      num("m1r4") +
      num("m1r5") +
      num("m1r6") +
      num("m1r7");

    const useVysv = !!document.getElementById("m1usev")?.checked;
    let Pvysv = 0;

    if (useVysv) {
      const pFirst = num("m1vfirst");
      const a = num("m1anorm");
      const t = num("m1ttehn");
      Pvysv = pFirst * (1 - a * t);
    }

    const Pk = baseSum - Pvysv;

    setHtml(
      "m1capOut",
      `
      <div class="kpis">
        <div class="kpi"><div class="k">Сумма составляющих</div><div>${money(baseSum)}</div></div>
        <div class="kpi"><div class="k">Pвысв</div><div>${money(Pvysv)}</div></div>
        <div class="kpi"><div class="k">Рк</div><div>${money(Pk)}</div></div>
      </div>
    `,
    );
  };

  window.calcM1Soft = () => {
    const n = num("m1n");
    const t = 3.6 * Math.pow(n, 1.2);
    const T = 2.5 * Math.pow(t, 0.32);
    const Pr = t ? (1000 * n) / t : 0;
    const Ch = T ? t / T : 0;

    setHtml(
      "m1softOut",
      `
      <div class="kpis">
        <div class="kpi"><div class="k">t</div><div>${t.toFixed(2)} чел.-мес.</div></div>
        <div class="kpi"><div class="k">T</div><div>${T.toFixed(2)} мес.</div></div>
        <div class="kpi"><div class="k">Пр</div><div>${Pr.toFixed(2)}</div></div>
        <div class="kpi"><div class="k">Чп</div><div>${Ch.toFixed(2)}</div></div>
      </div>
    `,
    );
  };

  window.calcM1Current = () => {
    const method = (
      document.getElementById("m1CurMethod")?.value || "first"
    ).toLowerCase();

    if (method === "second") {
      const txt = document.getElementById("m1tasks")?.value || "";
      const lines = txt
        .split("\n")
        .map((s) => s.trim())
        .filter(Boolean);

      let sumTasks = 0;
      const details = [];

      lines.forEach((line) => {
        const parts = line.split(",").map((s) => s.trim());
        const value = Number(parts.length > 1 ? parts[1] : parts[0]);
        if (Number.isFinite(value)) {
          sumTasks += value;
          details.push(
            `<tr><td>${parts.length > 1 ? parts[0] : "Задача"}</td><td>${money(value)}</td></tr>`,
          );
        }
      });

      const Isist = num("m1isist");
      const Ig = sumTasks + Isist;

      setHtml(
        "m1curOut",
        `
        <table>
          <thead><tr><th>Позиция</th><th>Затраты</th></tr></thead>
          <tbody>
            ${details.join("")}
            <tr><td>Исист</td><td>${money(Isist)}</td></tr>
          </tbody>
        </table>
        <p><b>Иг (второй метод) = ${money(Ig, "RUB/год")}</b></p>
      `,
      );
      return;
    }

    const Ig =
      num("m1ikts") + num("m1ison") + num("m1in") + num("m1iz1") + num("m1iz2");
    setHtml("m1curOut", `<b>Иг (первый метод) = ${money(Ig, "RUB/год")}</b>`);
  };

  // ---------------- Module 2 ----------------
  window.calcM2 = () => {
    const Nz = num("m2Nz"),
      tzb = num("m2tzb"),
      tzp = num("m2tzp");
    const Nd = num("m2Nd"),
      tdb = num("m2tdb"),
      tdp = num("m2tdp");
    const Cm = num("m2Cm"),
      Ik = num("m2Ikts"),
      Iz = num("m2Iz");
    const Cmm = num("m2Cmm"),
      Cs = num("m2Cs");
    const tbd = num("m2tbd"),
      tvv = num("m2tvv"),
      sec = num("m2sec");
    const n = num("m2n"),
      Cpr = num("m2Cpr"),
      dT = num("m2dT"),
      En = num("m2En");
    const years = Math.max(1, Math.round(num("m2years")));

    const t = 3.6 * Math.pow(n, 1.2);
    const Tn = 2.5 * Math.pow(t, 0.32);
    const Pno = Cpr * Tn * 24;
    const Pio = Cm * tbd;
    const Pve = Cm * tvv;
    const P = Pno + Pio + Pve;

    const Ig = Ik + Iz * 12;
    let alpha = 1;
    for (let i = 1; i < years; i++) alpha += ALPHA_NEXT[i] || 0;

    const It = Ig * alpha;
    const K2 = P + Ig;
    const Ktot = P + It;

    const P1 = sec * 12;
    const P2 = Cs * tzb * Nz - Cm * tzp * Nz;
    const P3 = Cmm * tdb * Nd - Cm * tdp * Nd;

    const Py = (P1 + P2 + P3) * (1 + En * dT);
    const P0 = Py * alpha;

    const eY = Py - K2;
    const eT = P0 - Ktot;

    const den = Py - Ig;
    const Ek = den ? den / P : Infinity;
    const Tok = den ? P / den : Infinity;

    setHtml(
      "m2Out",
      `
      <div class="kpis">
        <div class="kpi"><div class="k">P (единовременные)</div><div>${money(P)}</div></div>
        <div class="kpi"><div class="k">Иг</div><div>${money(Ig, "RUB/год")}</div></div>
        <div class="kpi"><div class="k">Пгод</div><div>${money(Py, "RUB/год")}</div></div>
        <div class="kpi"><div class="k">Эгод</div><div>${money(eY)}</div></div>
        <div class="kpi"><div class="k">Эпериод</div><div>${money(eT)}</div></div>
        <div class="kpi"><div class="k">Eк / Tок</div><div>${Number.isFinite(Ek) ? Ek.toFixed(4) : "∞"} / ${Number.isFinite(Tok) ? Tok.toFixed(2) : "∞"} лет</div></div>
      </div>
    `,
    );
  };

  // ---------------- Module 3 ----------------
  window.calcM3 = () => {
    const N = num("m3N"),
      En = num("m3En");
    const Wb = num("m3Wb"),
      Wp = num("m3Wp");
    const Kb = num("m3Kb"),
      Kp = num("m3Kp");
    const Zb = num("m3Zb"),
      Zp = num("m3Zp");
    const Esop = num("m3Esop");

    const Es = N * (Zb - Zp) + (Wb - Wp);
    const dK = Kp - Kb;
    const Eg = Es + Esop - En * dK;
    const ncrit = (Wp - Wb + En * (Kp - Kb)) / (Zb - Zp || 1e-9);

    setHtml(
      "m3Out",
      `
      <div class="kpis">
        <div class="kpi"><div class="k">Эс</div><div>${money(Es)}</div></div>
        <div class="kpi"><div class="k">ΔK</div><div>${money(dK)}</div></div>
        <div class="kpi"><div class="k">Eгод</div><div>${money(Eg)}</div></div>
        <div class="kpi"><div class="k">Nкр</div><div>${Number.isFinite(ncrit) ? ncrit.toFixed(2) : "∞"}</div></div>
      </div>
      <p class="${Eg >= 0 ? "ok" : "bad"}">${Eg >= 0 ? "Проект экономически целесообразен" : "Проект не проходит по экономическому эффекту"}</p>
    `,
    );
  };

  // ---------------- Module 4 ----------------
  window.calcM4 = () => {
    const H = num("m4H"),
      Tb = num("m4Tb"),
      Ti = num("m4Ti"),
      Fpv = num("m4Fpv"),
      Kvn = num("m4Kvn");
    const Ch = num("m4Ch"),
      Kp = num("m4Kp"),
      Kr = num("m4Kr"),
      Kd = num("m4Kd"),
      Ks = num("m4Ks");
    const Zsr = num("m4Zsr"),
      Chs = num("m4Chs"),
      Na = num("m4Na"),
      Cp = num("m4Cp");
    const Kvt = num("m4Kvt"),
      Kstr = num("m4Kstr"),
      Fg = num("m4Fg"),
      Ce = num("m4Ce"),
      Pel = num("m4Pel");
    const Kz = num("m4Kz"),
      Kpr = num("m4Kpr"),
      Tts = num("m4Tts"),
      d = num("m4d"),
      En = num("m4En");

    const g = Ti / (Tb || 1);
    const Q1 = H * Tb;
    const Q2 = Q1 * g;
    const dQ = Q1 - Q2;
    const Nv = dQ / ((Fpv || 1) * (Kvn || 1));

    const Zpl = Zsr * Chs * 12 * Kd * Ks;
    const Zam = Na * (Cp + Kvt + Kstr);
    const Zel = Fg * Ce * Pel;
    const Zmat = 0.01 * Cp;
    const Zrem = 0.035 * Cp;
    const Kn = num("m4Kn");
    const Znak = Zsr * 12 * Kn;
    const Zpr = 0.005 * Cp;
    const W = Zpl + Zam + Zel + Zmat + Zrem + Znak + Zpr;

    const dC = dQ * Ch * Kp * Kr * Kd * Ks - W;
    const dK = (Kvt + Kstr) * Kz + Kpr;
    const Sm = En * dC * Tts * d;
    const Eg = dC - dK * En + Sm;

    const Tok = dC + Sm ? dK / (dC + Sm) : Infinity;
    const Epr = Cp + Kvt + Kstr ? Eg / (Cp + Kvt + Kstr) : 0;

    setHtml(
      "m4Out",
      `
      <div class="kpis">
        <div class="kpi"><div class="k">g</div><div>${g.toFixed(4)}</div></div>
        <div class="kpi"><div class="k">ΔQ</div><div>${dQ.toFixed(2)} чел.-ч</div></div>
        <div class="kpi"><div class="k">Nвысв</div><div>${Nv.toFixed(2)} чел.</div></div>
        <div class="kpi"><div class="k">W</div><div>${money(W)}</div></div>
        <div class="kpi"><div class="k">ΔCпр</div><div>${money(dC)}</div></div>
        <div class="kpi"><div class="k">ΔKпр</div><div>${money(dK)}</div></div>
        <div class="kpi"><div class="k">Σμ</div><div>${money(Sm)}</div></div>
        <div class="kpi"><div class="k">Σгод</div><div>${money(Eg)}</div></div>
        <div class="kpi"><div class="k">Tок</div><div>${Number.isFinite(Tok) ? Tok.toFixed(2) : "∞"} лет</div></div>
        <div class="kpi"><div class="k">Eпр</div><div>${Epr.toFixed(4)} (Eн=${En})</div></div>
      </div>
      <p class="${Epr >= En ? "ok" : "bad"}">${Epr >= En ? "Проект эффективен" : "Проект неэффективен по Eпр"}</p>
    `,
    );
  };

  // ---------------- Module 5 ----------------
  window.calcM5 = () => {
    const mat = num("m5mat"),
      base = num("m5base"),
      addp = num("m5addp"),
      socp = num("m5socp");
    const ch = num("m5ch"),
      pch = num("m5pch"),
      ov = num("m5ov"),
      oth = num("m5oth");

    const labor = base * (1 + addp / 100) * (1 + socp / 100);
    const pc = ch * pch;
    const total = mat + labor + pc + ov + oth;

    setHtml(
      "m5Out",
      `
      <div class="kpis">
        <div class="kpi"><div class="k">Материалы</div><div>${money(mat)}</div></div>
        <div class="kpi"><div class="k">Труд</div><div>${money(labor)}</div></div>
        <div class="kpi"><div class="k">ПЭВМ</div><div>${money(pc)}</div></div>
        <div class="kpi"><div class="k">Накладные</div><div>${money(ov)}</div></div>
        <div class="kpi"><div class="k">Прочие</div><div>${money(oth)}</div></div>
        <div class="kpi"><div class="k">ИТОГО</div><div>${money(total)}</div></div>
      </div>
    `,
    );
  };

  // ---------------- Module 6 ----------------
  window.calcM6 = () => {
    const q = num("m6q"),
      p = num("m6p"),
      c = num("m6c"),
      B = num("m6B"),
      k = num("m6k");
    const sal = num("m6sal"),
      dpm = num("m6dpm"),
      hpd = num("m6hpd");
    const soc = num("m6soc"),
      ov = num("m6ov"),
      pc = num("m6pc"),
      mp = num("m6matp");

    const Q = q * c * (1 + p);
    const tpp = 40;
    const tu = (Q * B) / Math.pow(160, k || 1);
    const ta = Q / Math.pow(45, k || 1);
    const tn = ta;
    const to = Q / Math.pow(9, k || 1);
    const td = 1.75 * ta;
    const tz = tpp + tu + ta + tn + to + td;

    const zbase = (tz / ((dpm || 1) * (hpd || 1))) * sal;
    const zadd = 0.2 * zbase;
    const fzp = zbase + zadd;
    const social = fzp * (soc / 100);
    const overhead = fzp * (ov / 100);
    const materials = pc * (mp / 100);
    const total = fzp + social + overhead + materials;

    setHtml(
      "m6Out",
      `
      <div class="kpis">
        <div class="kpi"><div class="k">Q</div><div>${Q.toFixed(2)}</div></div>
        <div class="kpi"><div class="k">tz</div><div>${tz.toFixed(2)} ч</div></div>
        <div class="kpi"><div class="k">ФЗП</div><div>${money(fzp)}</div></div>
        <div class="kpi"><div class="k">Соц.</div><div>${money(social)}</div></div>
        <div class="kpi"><div class="k">Накладные</div><div>${money(overhead)}</div></div>
        <div class="kpi"><div class="k">Материалы</div><div>${money(materials)}</div></div>
        <div class="kpi"><div class="k">ИТОГО</div><div>${money(total)}</div></div>
      </div>
    `,
    );
  };

  // ---------------- Module 7 ----------------
  window.calcM7 = () => {
    const seats = Math.max(1, Math.round(num("m7seats")));
    const ibm = document.getElementById("m7ibm")?.value || "basic";

    const CMM_LICENSE_10 = 44000;
    const CMM_SEAT = 4400;
    const IBM_B = 1500;
    const IBM_A = 11500;
    const ARIS = 2600;
    const ARIS_SUP = 0.22;

    const blocks = Math.ceil(seats / 10);
    const cmmTotal = blocks * CMM_LICENSE_10;
    const ibmSeat = ibm === "basic" ? IBM_B : IBM_A;
    const ibmTotal = ibmSeat * seats;
    const arisLic = ARIS * seats;
    const arisSup = arisLic * ARIS_SUP;
    const arisTot = arisLic + arisSup;

    const pairs = [
      ["Cognos Metrics Manager", cmmTotal],
      [`IBM WebSphere (${ibm})`, ibmTotal],
      ["ARIS BSC", arisTot],
    ].sort((a, b) => a[1] - b[1]);

    setHtml(
      "m7Out",
      `
      <table>
        <thead><tr><th>Продукт</th><th>Итого</th></tr></thead>
        <tbody>
          <tr><td>Cognos</td><td>${money(cmmTotal, "USD")} (по ${CMM_SEAT} USD/место, блоки по 10)</td></tr>
          <tr><td>IBM ${ibm}</td><td>${money(ibmTotal, "USD")}</td></tr>
          <tr><td>ARIS</td><td>${money(arisTot, "EUR")}</td></tr>
        </tbody>
      </table>
      <p><b>Минимум:</b> ${pairs[0][0]}</p>
    `,
    );
  };

  // ---------------- Module 8 ----------------
  window.calcM8 = () => {
    const seats = Math.max(1, Math.round(num("m8seats")));
    const sap = num("m8sap");
    const or = num("m8or");
    const usd = num("m8usd");
    const bsV = document.getElementById("m8bs")?.value || "basic";
    const cock = (document.getElementById("m8cock")?.value || "no") === "yes";

    const HYP = 700,
      HSUP = 154,
      BS_B = 44000,
      BS_A = 63000,
      BS_C = 9800;

    const sapUsd = sap * seats;
    const orUsd = or * seats;
    const hypUsd = (HYP + HSUP) * seats;

    const bsSeat = bsV === "basic" ? BS_B : BS_A;
    const bsLic = bsSeat * seats;
    const bsCock = cock ? Math.ceil(seats / 10) * BS_C : 0;
    const bsRub = bsLic + bsCock;

    const cmp = [
      ["SAP SEM", sapUsd * usd],
      ["Oracle BSC", orUsd * usd],
      ["Hyperion", hypUsd * usd],
      ["Business Studio", bsRub],
    ].sort((a, b) => a[1] - b[1]);

    setHtml(
      "m8Out",
      `
      <table>
        <thead><tr><th>Продукт</th><th>Итого USD</th><th>Итого RUB</th></tr></thead>
        <tbody>
          <tr><td>SAP</td><td>${money(sapUsd, "USD")}</td><td>${money(sapUsd * usd, "RUB")}</td></tr>
          <tr><td>Oracle</td><td>${money(orUsd, "USD")}</td><td>${money(orUsd * usd, "RUB")}</td></tr>
          <tr><td>Hyperion</td><td>${money(hypUsd, "USD")}</td><td>${money(hypUsd * usd, "RUB")}</td></tr>
          <tr><td>Business Studio (${bsV}${cock ? ", cockpit" : ""})</td><td>${usd ? money(bsRub / usd, "USD") : "—"}</td><td>${money(bsRub, "RUB")}</td></tr>
        </tbody>
      </table>
      <p class="ok"><b>Минимальная стоимость в RUB:</b> ${cmp[0][0]} — ${money(cmp[0][1], "RUB")}</p>
    `,
    );
  };

  // ---------------- Init ----------------
  document.addEventListener("DOMContentLoaded", () => {
    initTopTabs();
    initModule1Tabs();

    const totalMethod = document.getElementById("m1TotalMethod");
    const tslBlock = document.getElementById("m1TslBlock");
    const tslFBlock = document.getElementById("m1TslFBlock");
    const preMethod = document.getElementById("m1PreMethod");
    const preComp = document.getElementById("m1PreComponents");
    const preEst = document.getElementById("m1PreEstimate");
    const useV = document.getElementById("m1usev");
    const vBlock = document.getElementById("m1vBlock");
    const curMethod = document.getElementById("m1CurMethod");
    const curFirst = document.getElementById("m1CurFirst");
    const curSecond = document.getElementById("m1CurSecond");

    const syncM1Ui = () => {
      const tm = (totalMethod?.value || "table").toLowerCase();
      if (tslBlock) tslBlock.classList.toggle("hidden", tm !== "table");
      if (tslFBlock) tslFBlock.classList.toggle("hidden", tm !== "formula");

      const pm = (preMethod?.value || "components").toLowerCase();
      if (preComp) preComp.classList.toggle("hidden", pm !== "components");
      if (preEst) preEst.classList.toggle("hidden", pm !== "estimate");

      if (vBlock) vBlock.classList.toggle("hidden", !useV?.checked);

      const cm = (curMethod?.value || "first").toLowerCase();
      if (curFirst) curFirst.classList.toggle("hidden", cm !== "first");
      if (curSecond) curSecond.classList.toggle("hidden", cm !== "second");
    };

    totalMethod?.addEventListener("change", syncM1Ui);
    preMethod?.addEventListener("change", syncM1Ui);
    useV?.addEventListener("change", syncM1Ui);
    curMethod?.addEventListener("change", syncM1Ui);

    syncM1Ui();

    // initial calculations
    window.calcM1Eff();
    window.calcM2();
    window.calcM3();
    window.calcM4();
    window.calcM5();
    window.calcM6();
    window.calcM7();
    window.calcM8();
  });
})();
