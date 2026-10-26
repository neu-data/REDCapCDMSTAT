"use strict";

const $ = (s) => document.querySelector(s);
const STATUSES = ["open", "answered", "resolved", "closed"];
const NEXT = { open: ["answered", "resolved", "closed"], answered: ["open", "resolved", "closed"], resolved: ["open", "closed"], closed: ["open"] };
let KEY = null;
let ME = null;
let SUMMARY = null;

function store(k, v) { try { v === undefined ? sessionStorage.removeItem(k) : sessionStorage.setItem(k, v); } catch (e) { /* storage unavailable */ } }
function load(k) { try { return sessionStorage.getItem(k); } catch (e) { return null; } }

function esc(v) {
  if (v === null || v === undefined) return "";
  return String(v).replace(/[&<>"']/g, (c) => ({ "&": "&amp;", "<": "&lt;", ">": "&gt;", '"': "&quot;", "'": "&#39;" }[c]));
}

function toast(msg, err = false) {
  const t = document.createElement("div");
  t.className = "toast" + (err ? " err" : "");
  t.textContent = msg;
  $("#toasts").appendChild(t);
  setTimeout(() => t.remove(), 5000);
}

async function api(path, opts = {}) {
  const res = await fetch("/api/" + path, {
    method: opts.method || "GET",
    headers: Object.assign({ Authorization: "Bearer " + KEY }, opts.body ? { "Content-Type": "application/json" } : {}),
    body: opts.body ? JSON.stringify(opts.body) : undefined,
  });
  const text = await res.text();
  let data;
  try { data = JSON.parse(text); } catch (e) { data = text; }
  if (!res.ok) {
    const msg = (data && (data.error || data.message || data.detail)) || res.statusText;
    throw new Error(Array.isArray(msg) ? msg.join(" ") : msg);
  }
  return data;
}

function table(rows, cols, opts = {}) {
  if (!rows || !rows.length) return '<p class="muted">Nothing to show.</p>';
  cols = cols || Object.keys(rows[0]);
  const head = cols.map((c) => `<th>${esc(c)}</th>`).join("");
  const body = rows.map((r, i) => "<tr" + (opts.rowAttr ? opts.rowAttr(r, i) : "") + ">" + cols.map((c) => {
    const v = r[c];
    const cls = typeof v === "number" ? "num" : (c === "severity" ? "sev-" + v : "");
    const shown = typeof v === "number" && !Number.isInteger(v) ? Number(v.toPrecision(4)) : v;
    return `<td class="${cls}">${opts.html && opts.html[c] ? opts.html[c](r) : esc(shown)}</td>`;
  }).join("") + "</tr>").join("");
  return `<table><thead><tr>${head}</tr></thead><tbody>${body}</tbody></table>`;
}

function plotTheme() {
  const css = getComputedStyle(document.documentElement);
  return {
    paper_bgcolor: "rgba(0,0,0,0)", plot_bgcolor: "rgba(0,0,0,0)",
    font: { color: css.getPropertyValue("--text").trim(), family: "Inter, Segoe UI, sans-serif" },
    margin: { l: 50, r: 10, t: 10, b: 40 },
    xaxis: { gridcolor: css.getPropertyValue("--border").trim() },
    yaxis: { gridcolor: css.getPropertyValue("--border").trim() },
    legend: { orientation: "h", y: -0.2 },
  };
}

function modal(title, bodyHtml, okLabel = "OK") {
  return new Promise((resolve) => {
    const dlg = $("#modal");
    $("#modal-title").textContent = title;
    $("#modal-body").innerHTML = bodyHtml;
    $("#modal-ok").textContent = okLabel;
    dlg.onclose = () => {
      if (dlg.returnValue !== "ok") return resolve(null);
      const vals = {};
      dlg.querySelectorAll("#modal-body [name]").forEach((el) => { vals[el.name] = el.value; });
      resolve(vals);
    };
    dlg.showModal();
  });
}

function can(action) {
  const acl = {
    dm: ["query_write", "query_raise", "cut_write", "rule_write", "randomize", "rand_setup", "lock", "coding", "deviation", "sdv_plan"],
    stats: ["query_raise"], monitor: ["query_raise", "sdv", "deviation"], pi: ["esign", "randomize"], viewer: [],
  };
  return (acl[ME.role] || []).includes(action);
}

// ---------------------------------------------------------------- views
const views = {
  async dashboard() {
    SUMMARY = await api("summary");
    $("#project").textContent = SUMMARY.project;
    const q = SUMMARY.queries || {};
    const open = (q.open || 0) + (q.answered || 0);
    $("#tiles").innerHTML = [
      ["Records", SUMMARY.records, ""], ["Open queries", open, "warn"], ["Rule violations", SUMMARY.issues, "danger"],
      ["Events", SUMMARY.events.length || 0, ""], ["Dictionary", SUMMARY.dict_hash, ""],
      ["Analysing", SUMMARY.active_cut || "Live data", ""],
    ].map(([k, v, c]) => `<div class="tile ${c}"><div class="k">${esc(k)}</div><div class="v">${esc(v)}</div></div>`).join("");
    Plotly.react("q-chart", [{ type: "bar", x: STATUSES, y: STATUSES.map((s) => q[s] || 0),
      marker: { color: STATUSES.map((s) => getComputedStyle(document.documentElement).getPropertyValue("--s-" + s).trim()) } }],
      plotTheme(), { displayModeBar: false, responsive: true });
    const sch = await api("schema/history");
    $("#schema").innerHTML = table(sch.map((r) => ({ at: r.at, change: r.summary, hash: (r.new_hash || "").slice(0, 10) })));
    $("#synclog").innerHTML = table(await api("sync/history"), ["at", "mode", "n_changed", "n_deleted", "metadata_changed", "seconds"]);
  },

  async quality() {
    const issues = await api("issues");
    $("#issues").innerHTML = table(issues, ["severity", "rule", "record_id", "event", "instance", "field", "value", "message"]);
    $("#btn-raise").disabled = !can("query_write");
  },

  async queries() {
    const qs = await api("queries");
    const drw = await api("drw/status");
    const links = drw.enabled ? await api("drw/links") : [];
    const sid = Object.fromEntries(links.filter((l) => l.status_id !== null).map((l) => [l.query_id, l.status_id]));
    $("#drw-state").textContent = drw.enabled ? `on: ${drw.linked} linked` : "off";
    $("#drw-enable").classList.toggle("hidden", !!drw.enabled || !can("query_write"));
    $("#drw-sync").classList.toggle("hidden", !drw.enabled || !can("query_write"));
    $("#board").innerHTML = STATUSES.map((s) => {
      const items = qs.filter((q) => q.status === s);
      return `<div class="col"><h3>${s}<span>${items.length}</span></h3>` + items.slice(0, 200).map((q) => `
        <div class="qcard" data-status="${s}" data-id="${esc(q.query_id)}">
          <div><b>Record ${esc(q.record_id)}</b> &middot; ${esc(q.field)} = ${esc(q.value)}</div>
          <div>${esc(q.message)}</div>
          <div class="meta">${esc(q.event || "")} &middot; ${esc(q.source)} &middot; ${esc(q.created_by)} &middot; ${esc(q.updated_at)}${sid[q.query_id] ? ` &middot; DRW #${esc(sid[q.query_id])}` : ""}</div>
        </div>`).join("") + "</div>";
    }).join("");
    document.querySelectorAll(".qcard").forEach((el) => el.addEventListener("click", () => openQuery(qs.find((q) => q.query_id === el.dataset.id))));
  },

  async explore() {
    await fillFieldSelects();
    await drawScatter();
  },

  async wrangle() {
    const v = await api(`variables?longitudinal=${$("#wr-long").value}&repeating=${$("#wr-rep").value}`);
    $("#wr-dim").textContent = `${v.n_cols} columns x ${v.n_rows} rows`;
    $("#wr-vars").innerHTML = table(v.variables, ["name", "type", "label", "event"]);
    $("#wr-preview").innerHTML = table(v.preview);
  },

  async stats() {
    await fillFieldSelects();
    await drawTable1();
    await drawTrend();
  },

  async cuts() {
    const cuts = await api("cuts");
    $("#cuts").innerHTML = table(cuts.map((c) => Object.assign({}, c, { sha256: (c.sha256 || "").slice(0, 16) })),
      ["cut_id", "label", "created_by", "created_at", "n_records", "n_rows", "sha256", "signed_off_by", "signed_off_at", "action"],
      { html: { action: (c) => c.signed_off_by ? "&#10003; signed" : (can("cut_write") ? `<button class="btn small" data-sign="${esc(c.cut_id)}">Sign off</button>` : "") } });
    document.querySelectorAll("[data-sign]").forEach((b) => b.addEventListener("click", async () => {
      const v = await modal("Sign off " + b.dataset.sign, '<label>Sign-off statement<textarea name="comment" required>Data reviewed and clean for analysis</textarea></label>', "Sign off");
      if (!v) return;
      try { await api(`cuts/${b.dataset.sign}/signoff`, { method: "POST", body: v }); toast("Cut signed off"); views.cuts(); } catch (e) { toast(e.message, true); }
    }));
  },

  async random() {
    const r = await api("randomization");
    if (!r.configured) { $("#rz-balance").innerHTML = '<p class="muted">Randomisation is not set up yet.</p>'; $("#rz-alloc").innerHTML = ""; return; }
    $("#rz-balance").innerHTML = table(r.balance);
    $("#rz-alloc").innerHTML = r.allocations ? table(r.allocations) : '<p class="muted">Concealed for your role.</p>';
  },

  async sdv() {
    const rows = await api("sdv?required=1");
    const done = rows.filter((r) => r.status === "verified").length;
    $("#sdv-progress").textContent = `${done} / ${rows.length} required units verified`;
    $("#sdv-tbl").innerHTML = table(rows, ["pick", "site", "record_id", "event", "form", "reason", "status", "verified_by", "verified_at"],
      { html: { pick: (r) => r.status === "verified" ? "" : `<input type="checkbox" data-unit="${esc(r.unit_key)}">` } });
  },

  async locks() {
    $("#locks-tbl").innerHTML = table(await api("locks"), ["lock_id", "record_id", "event", "form", "reason", "locked_by", "locked_at", "integrity"]);
    $("#sigs-tbl").innerHTML = table(await api("signatures"), ["sig_id", "record_id", "signer", "meaning", "signed_at", "status"]);
  },

  async coding() {
    const rows = await api("coding");
    $("#cd-tbl").innerHTML = table(rows, ["status", "verbatim", "term", "code", "suggestion_term", "score", "record_id", "event", "action"],
      { html: { action: (r) => (r.status === "pending" && can("coding")) ? `<button class="btn small" data-approve="${esc(r.coding_key)}">Approve suggestion</button>` : "" } });
    document.querySelectorAll("[data-approve]").forEach((b) => b.addEventListener("click", async () => {
      try { await api("coding/approve", { method: "POST", body: { coding_key: b.dataset.approve } }); views.coding(); } catch (e) { toast(e.message, true); }
    }));
  },

  async deviations() {
    const rows = await api("deviations");
    $("#dv-tbl").innerHTML = table(rows, ["dev_id", "status", "severity", "category", "record_id", "event", "description", "source", "reported_by", "action"],
      { html: { action: (r) => (r.status === "open" && can("deviation")) ? `<button class="btn small" data-close="${esc(r.dev_id)}">Close</button>` : "" } });
    document.querySelectorAll("[data-close]").forEach((b) => b.addEventListener("click", async () => {
      const v = await modal("Close " + b.dataset.close, '<label>Closing comment<textarea name="comment" required></textarea></label>', "Close");
      if (!v) return;
      try { await api(`deviations/${b.dataset.close}/close`, { method: "POST", body: v }); views.deviations(); } catch (e) { toast(e.message, true); }
    }));
  },

  async sites() {
    $("#sites-tbl").innerHTML = table(await api("metrics/sites"));
    const e = await api("metrics/enrollment");
    const sites = [...new Set(e.map((r) => r.site))];
    Plotly.react("enrol-plot", sites.map((s) => {
      const d = e.filter((r) => r.site === s);
      return { type: "scatter", mode: "lines", line: { shape: "hv" }, name: s, x: d.map((r) => r.date), y: d.map((r) => r.cumulative) };
    }), plotTheme(), { displayModeBar: false, responsive: true });
  },

  async cdisc() {
    SDTM = await api("sdtm/" + $("#sd-domain").value);
    $("#sd-tbl").innerHTML = table(SDTM.slice(0, 300));
  },

  async script() {
    $("#script").textContent = await api("script");
  },
};

async function openQuery(q) {
  const audit = await api(`queries/${q.query_id}/audit`);
  const opts = NEXT[q.status].map((s) => `<option>${s}</option>`).join("");
  const body = `
    <div><b>${esc(q.query_id)}</b> &middot; record ${esc(q.record_id)} &middot; ${esc(q.event || "")} &middot; ${esc(q.field)} = ${esc(q.value)}</div>
    <div>${esc(q.message)}</div>
    ${can("query_write") ? `<label>Move to <select name="status">${opts}<option value="">(comment only)</option></select></label>` : '<input type="hidden" name="status" value="">'}
    <label>Reason for change / comment (required)<textarea name="comment" required></textarea></label>
    <div class="table-wrap">${table(audit, ["at", "actor", "action", "old_status", "new_status", "comment"])}</div>`;
  const v = await modal("Query", body, "Save");
  if (!v) return;
  try {
    if (v.status) await api(`queries/${q.query_id}/transition`, { method: "POST", body: { status: v.status, comment: v.comment } });
    else await api(`queries/${q.query_id}/comment`, { method: "POST", body: { comment: v.comment } });
    toast("Query updated");
    views.queries();
  } catch (e) { toast(e.message, true); }
}

let SDTM = [];
const list = (v) => String(v || "").split(/[,\s]+/).filter(Boolean);
function download(name, text, type) {
  const a = Object.assign(document.createElement("a"), { href: URL.createObjectURL(new Blob([text], { type })), download: name });
  a.click();
}

let FIELDS = null;
async function fillFieldSelects() {
  if (FIELDS) return;
  FIELDS = await api("fields/numeric");
  SUMMARY = SUMMARY || await api("summary");
  const opt = (f, sel) => `<option value="${esc(f.field)}"${f.field === sel ? " selected" : ""}>${esc(f.field)} - ${esc(f.label)}</option>`;
  $("#ex-x").innerHTML = FIELDS.map((f) => opt(f, "height")).join("");
  $("#ex-y").innerHTML = FIELDS.map((f) => opt(f, "weight")).join("");
  $("#tr-field").innerHTML = FIELDS.map((f) => opt(f, "sbp")).join("");
  const evs = SUMMARY.events || [];
  $("#ex-event").innerHTML = '<option value="">All events</option>' + evs.map((e) => `<option value="${esc(e.unique_event_name)}">${esc(e.event_name)}</option>`).join("");
  $("#t1-event").innerHTML = evs.map((e) => `<option value="${esc(e.unique_event_name)}">${esc(e.event_name)}</option>`).join("");
  const md = await api("metadata");
  const cats = md.filter((m) => ["radio", "dropdown", "yesno"].includes(m.field_type));
  $("#t1-by").innerHTML = '<option value="">No grouping</option>' + cats.map((m) =>
    `<option value="${esc(m.field_name)}"${m.field_name === SUMMARY.arm ? " selected" : ""}>${esc(m.field_name)} - ${esc(m.field_label)}</option>`).join("");
}

let SCATTER = [];
async function drawScatter() {
  const p = new URLSearchParams({ x: $("#ex-x").value, y: $("#ex-y").value, event: $("#ex-event").value, method: $("#ex-method").value, threshold: $("#ex-thr").value });
  SCATTER = await api("scatter?" + p);
  const trace = (flag, name, color) => {
    const d = SCATTER.filter((r) => r.outlier === flag);
    return { type: "scatter", mode: "markers", name, x: d.map((r) => r.x), y: d.map((r) => r.y),
      customdata: d.map((r) => [r.record_id, r.event, r.score]), marker: { size: 9, opacity: 0.8, color },
      hovertemplate: "Record %{customdata[0]}<br>%{customdata[1]}<br>score %{customdata[2]}<extra></extra>" };
  };
  const layout = Object.assign(plotTheme(), { xaxis: Object.assign(plotTheme().xaxis, { title: $("#ex-x").value }), yaxis: Object.assign(plotTheme().yaxis, { title: $("#ex-y").value }) });
  await Plotly.react("ex-plot", [trace(false, "Within range", "#2c5d8f"), trace(true, "Outlier", "#d1495b")], layout, { responsive: true, displaylogo: false });
  const el = $("#ex-plot");
  el.removeAllListeners && el.removeAllListeners("plotly_click");
  el.on("plotly_click", async (ev) => {
    const [record, event, score] = ev.points[0].customdata;
    if (!can("query_raise")) return toast("Your role cannot raise queries", true);
    const y = $("#ex-y").value, x = $("#ex-x").value;
    const v = await modal(`Raise query: record ${record} (${event})`, `
      <label>Field <select name="field"><option>${esc(y)}</option><option>${esc(x)}</option></select></label>
      <label>Query text <textarea name="message">Please verify ${esc(y)} = ${esc(ev.points[0].y)} (outlier score ${esc(score)} in ${esc(event)}).</textarea></label>`, "Raise query");
    if (!v) return;
    const value = v.field === y ? ev.points[0].y : ev.points[0].x;
    try {
      const r = await api("queries", { method: "POST", body: { record_id: record, event, field: v.field, value, message: v.message, source: "analysis" } });
      toast("Query " + r.query_id + " raised");
    } catch (e) { toast(e.message, true); }
  });
}

async function drawTable1() {
  const by = $("#t1-by").value, ev = $("#t1-event").value;
  try {
    const t = await api(`table1?by=${encodeURIComponent(by)}&event=${encodeURIComponent(ev)}`);
    const cols = Object.keys(t[0] || {}).filter((c) => c !== "variable");
    $("#t1").innerHTML = table(t, cols);
  } catch (e) { $("#t1").innerHTML = `<p class="error">${esc(e.message)}</p>`; }
}

async function drawTrend() {
  const d = await api("trend?field=" + encodeURIComponent($("#tr-field").value));
  const groups = [...new Set(d.map((r) => r.group))];
  const traces = groups.map((g) => {
    const s = d.filter((r) => r.group === g);
    return { type: "scatter", mode: "lines+markers", name: g, x: s.map((r) => r.event), y: s.map((r) => r.mean),
      error_y: { type: "data", array: s.map((r) => 1.96 * (r.se || 0)) } };
  });
  Plotly.react("tr-plot", traces, plotTheme(), { displayModeBar: false, responsive: true });
}

// ---------------------------------------------------------------- wiring
function route() {
  const name = (location.hash || "#dashboard").slice(1);
  const v = views[name] ? name : "dashboard";
  document.querySelectorAll(".view").forEach((s) => s.classList.toggle("active", s.id === "view-" + v));
  document.querySelectorAll("#nav a").forEach((a) => a.classList.toggle("active", a.getAttribute("href") === "#" + v));
  views[v]().catch((e) => toast(e.message, true));
}

function connectSocket() {
  const proto = location.protocol === "https:" ? "wss" : "ws";
  const ws = new WebSocket(`${proto}://${location.host}/ws?key=${encodeURIComponent(KEY)}`);
  ws.onopen = () => $("#live").classList.add("on");
  ws.onclose = () => { $("#live").classList.remove("on"); if (KEY) setTimeout(connectSocket, 3000); };
  ws.onmessage = (m) => {
    const ev = JSON.parse(m.data);
    if (ev.type === "presence") return;
    if (ev.by && ev.by !== ME.name) toast(ev.type === "data" ? `REDCap saved record ${ev.record} (${ev.instrument})` : `${ev.by} updated ${ev.path}`);
    route();
  };
}

async function signIn(key) {
  KEY = key;
  const r = await fetch("/auth/me", { headers: { Authorization: "Bearer " + key } });
  if (!r.ok) throw new Error("Invalid API key");
  ME = await r.json();
  store("cdmstat_key", key);
  $("#whoami").textContent = `${ME.name} (${ME.role})`;
  $("#login").classList.add("hidden");
  $("#app").classList.remove("hidden");
  connectSocket();
  route();
}

$("#login-form").addEventListener("submit", async (e) => {
  e.preventDefault();
  try { await signIn($("#login-key").value.trim()); } catch (err) { $("#login-error").textContent = err.message; }
});
$("#logout").addEventListener("click", () => { store("cdmstat_key"); KEY = null; location.reload(); });
window.addEventListener("hashchange", route);
$("#btn-sync").addEventListener("click", async () => {
  try { const r = await api("sync", { method: "POST", body: {} }); toast(`Sync: ${r.changed} patched${r.metadata_changed ? ", DICTIONARY CHANGED" : ""}`); } catch (e) { toast(e.message, true); }
});
$("#btn-raise").addEventListener("click", async () => {
  try { const r = await api("queries/raise-from-rules", { method: "POST", body: {} }); toast(`${r.created} new queries, ${r.auto_resolved} auto-resolved`); } catch (e) { toast(e.message, true); }
});
$("#rule-form").addEventListener("submit", async (e) => {
  e.preventDefault();
  try { await api("rules", { method: "POST", body: Object.fromEntries(new FormData(e.target)) }); toast("Rule added"); } catch (err) { toast(err.message, true); }
});
$("#cut-form").addEventListener("submit", async (e) => {
  e.preventDefault();
  try { const r = await api("cuts", { method: "POST", body: Object.fromEntries(new FormData(e.target)) }); toast("Frozen " + r.cut_id); e.target.reset(); } catch (err) { toast(err.message, true); }
});
$("#fit-form").addEventListener("submit", async (e) => {
  e.preventDefault();
  try {
    const r = await api("fit", { method: "POST", body: Object.fromEntries(new FormData(e.target)) });
    $("#fit-out").innerHTML = `<p class="muted">${esc(r.type)} model, n = ${esc(r.n)}</p>` + table(r.coefficients);
  } catch (err) { $("#fit-out").innerHTML = `<p class="error">${esc(err.message)}</p>`; }
});
["#ex-x", "#ex-y", "#ex-event", "#ex-method", "#ex-thr"].forEach((s) => $(s).addEventListener("change", () => drawScatter().catch((e) => toast(e.message, true))));
["#wr-long", "#wr-rep"].forEach((s) => $(s).addEventListener("change", () => views.wrangle()));
["#t1-by", "#t1-event"].forEach((s) => $(s).addEventListener("change", drawTable1));
$("#tr-field").addEventListener("change", drawTrend);
$("#btn-dl-script").addEventListener("click", () => {
  const blob = new Blob([$("#script").textContent], { type: "text/plain" });
  const a = Object.assign(document.createElement("a"), { href: URL.createObjectURL(blob), download: "cdmstat_analysis.R" });
  a.click();
});

$("#drw-enable").addEventListener("click", async () => {
  try { const s = await api("drw/enable", { method: "POST", body: {} }); toast(`DRW write-back enabled (token user ${s.user})`); views.queries(); } catch (e) { toast(e.message, true); }
});
$("#drw-sync").addEventListener("click", async () => {
  try {
    const r = await api("drw/sync", { method: "POST", body: {} });
    toast(`DRW: ${r.opened} opened, ${r.updated} updated, ${r.pulled} site updates, ${r.imported} imported${r.errors.length ? ", " + r.errors.length + " errors" : ""}`, r.errors.length > 0);
    views.queries();
  } catch (e) { toast(e.message, true); }
});

// ---- CDMS wiring
$("#rz-form").addEventListener("submit", async (e) => {
  e.preventDefault();
  try {
    const r = await api("randomization/assign", { method: "POST", body: Object.fromEntries(new FormData(e.target)) });
    $("#rz-result").innerHTML = `Record <b>${esc(r.record_id)}</b> allocated to <b>${esc(r.arm_label)}</b> (stratum ${esc(r.stratum)}, slot ${esc(r.seq)})${r.written_to_edc ? ", written to the EDC" : ""}.`;
    views.random();
  } catch (err) { toast(err.message, true); }
});
$("#rz-setup").addEventListener("submit", async (e) => {
  e.preventDefault();
  const f = Object.fromEntries(new FormData(e.target));
  try {
    await api("randomization/setup", { method: "POST", body: { strata: list(f.strata), block_sizes: list(f.block_sizes).map(Number), per_stratum: Number(f.per_stratum) } });
    toast("Schedule generated"); views.random();
  } catch (err) { toast(err.message, true); }
});
$("#sdv-plan").addEventListener("submit", async (e) => {
  e.preventDefault();
  const f = Object.fromEntries(new FormData(e.target));
  try {
    const r = await api("sdv/plan", { method: "POST", body: { critical_fields: list(f.critical_fields), first_n: Number(f.first_n), sample_rate: Number(f.sample_rate) } });
    toast(`${r.required} of ${r.units} units require SDV`); views.sdv();
  } catch (err) { toast(err.message, true); }
});
$("#sdv-verify").addEventListener("click", async () => {
  const keys = [...document.querySelectorAll("[data-unit]:checked")].map((c) => c.dataset.unit);
  if (!keys.length) return toast("Tick the units you verified", true);
  try { await api("sdv/verify", { method: "POST", body: { unit_keys: keys } }); toast(`${keys.length} unit(s) verified`); views.sdv(); } catch (err) { toast(err.message, true); }
});
$("#lock-form").addEventListener("submit", async (e) => {
  e.preventDefault();
  try { await api("locks", { method: "POST", body: Object.fromEntries(new FormData(e.target)) }); e.target.reset(); views.locks(); } catch (err) { toast(err.message, true); }
});
$("#sign-form").addEventListener("submit", async (e) => {
  e.preventDefault();
  try { await api("esign", { method: "POST", body: Object.fromEntries(new FormData(e.target)) }); toast("Signed"); e.target.credential.value = ""; views.locks(); }
  catch (err) { toast(err.message, true); }
});
$("#cd-run").addEventListener("click", async () => {
  try { const r = await api("coding/run", { method: "POST", body: { field: $("#cd-field").value } }); toast(`${r.auto_coded} auto-coded, ${r.for_review} for review`); views.coding(); }
  catch (err) { toast(err.message, true); }
});
$("#dv-form").addEventListener("submit", async (e) => {
  e.preventDefault();
  try { await api("deviations", { method: "POST", body: Object.fromEntries(new FormData(e.target)) }); e.target.reset(); views.deviations(); } catch (err) { toast(err.message, true); }
});
$("#dv-check").addEventListener("click", async () => {
  try { const r = await api("deviations/check-windows", { method: "POST", body: { window: 7 } }); toast(`${r.new} new deviation(s)`); views.deviations(); } catch (err) { toast(err.message, true); }
});
$("#sd-domain").addEventListener("change", () => views.cdisc());
$("#sd-csv").addEventListener("click", () => {
  if (!SDTM.length) return;
  const cols = Object.keys(SDTM[0]);
  const q = (v) => '"' + String(v ?? "").replace(/"/g, '""') + '"';
  download(`${$("#sd-domain").value.toLowerCase()}.csv`, [cols.join(","), ...SDTM.map((r) => cols.map((c) => q(r[c])).join(","))].join("\n"), "text/csv");
});
$("#odm-dl").addEventListener("click", async () => {
  try { download("study.odm.xml", await api("export/odm"), "application/xml"); } catch (err) { toast(err.message, true); }
});

const saved = load("cdmstat_key");
if (saved) signIn(saved).catch(() => $("#login").classList.remove("hidden"));
else $("#login").classList.remove("hidden");
