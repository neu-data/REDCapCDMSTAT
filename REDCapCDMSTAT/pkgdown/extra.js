// Run the guide code in the browser with webR (https://docs.r-wasm.org/webr/).
// Pinned to webR v0.5.9 (R 4.5): RSQLite, which the ledger needs, loads there
// but not yet in v0.6.0. Keep in step with the R version of the webr-package
// job in .github/workflows/pkgdown.yaml.
//
// Executed chunks (wrapped in div.webr-chunk by a knitr hook) get Run and Edit
// buttons. R runs on the reader's device; REDCapCDMSTAT is installed from the
// WebAssembly repository published with this site (/webr/), its dependencies
// from repo.r-wasm.org. The guides use the simulated REDCap project, so no
// REDCap server or token is involved.
(function () {
  "use strict";

  var WEBR_URL = "https://webr.r-wasm.org/v0.5.9/webr.mjs";
  var CRAN_WASM = "https://repo.r-wasm.org/";

  function siteRoot() {
    var brand = document.querySelector("a.navbar-brand");
    var href = brand ? brand.getAttribute("href") : "index.html";
    var url = new URL(href, window.location.href);
    return url.href.replace(/index\.html$/, "");
  }

  var REPOS = 'c("' + siteRoot() + 'webr/", "' + CRAN_WASM + '")';
  var webR = null;
  var ready = null;
  var queue = Promise.resolve();
  var installed = {};

  function codeOf(pre) {
    var code = pre.querySelector("code") || pre;
    return code.innerText.replace(/ /g, " ");
  }

  function setStatus(text) {
    document.querySelectorAll(".webr-status").forEach(function (el) { el.textContent = text; });
  }

  function start() {
    if (ready) return ready;
    ready = (async function () {
      setStatus("Starting R in your browser…");
      var mod = await import(WEBR_URL);
      webR = new mod.WebR();
      await webR.init();
      setStatus("Installing REDCapCDMSTAT and its dependencies (first run only)…");
      await webR.evalRVoid("webr::install('REDCapCDMSTAT', repos = " + REPOS + ", quiet = TRUE)");
      await webR.evalRVoid("suppressPackageStartupMessages(library(REDCapCDMSTAT)); options(width = 90)");
      installed.REDCapCDMSTAT = true;
      setStatus("R is running in your browser (webR " + (await webR.evalRString("as.character(getRversion())")) + ").");
    })();
    ready.catch(function (e) { setStatus("Could not start webR: " + e.message); ready = null; });
    return ready;
  }

  // Optional packages, installed only when a chunk needs them.
  async function ensurePackages(code) {
    var need = [];
    if (/Surv\(|"cox"/.test(code)) need.push("survival");
    if (/\(1 \||"mixed"/.test(code)) need.push("lme4");
    if (/odm|ODM/.test(code)) need.push("xml2");
    for (var i = 0; i < need.length; i++) {
      var p = need[i];
      if (installed[p]) continue;
      setStatus("Installing " + p + "…");
      await webR.evalRVoid("if (!requireNamespace('" + p + "', quietly = TRUE)) webr::install('" + p + "', repos = " + REPOS + ", quiet = TRUE)");
      installed[p] = true;
    }
  }

  function outputBox(block) {
    var box = block.bar.nextElementSibling;
    if (!box || !box.classList.contains("webr-result")) {
      box = document.createElement("div");
      box.className = "webr-result";
      block.wrapper.parentNode.insertBefore(box, block.bar.nextSibling);
    }
    box.innerHTML = "";
    var pre = document.createElement("pre");
    pre.className = "webr-output";
    box.appendChild(pre);
    return { box: box, pre: pre };
  }

  async function execute(block) {
    var code = codeOf(block.pre);
    await ensurePackages(code);
    var out = outputBox(block);
    out.pre.textContent = "Running…";
    var shelter = await new webR.Shelter();
    try {
      var res = await shelter.captureR(code, {
        withAutoprint: true,
        captureStreams: true,
        captureConditions: false,
        captureGraphics: { width: 900, height: 480 }
      });
      out.pre.textContent = "";
      res.output.forEach(function (o) {
        var line = document.createElement("span");
        if (o.type === "stderr") line.className = "webr-err";
        line.textContent = o.data + "\n";
        out.pre.appendChild(line);
      });
      if (!res.output.length && !res.images.length) out.pre.textContent = "(no printed output)";
      res.images.forEach(function (img) {
        var c = document.createElement("canvas");
        c.width = img.width;
        c.height = img.height;
        c.getContext("2d").drawImage(img, 0, 0);
        out.box.appendChild(c);
      });
      block.done = true;
    } catch (e) {
      out.pre.innerHTML = "";
      var err = document.createElement("span");
      err.className = "webr-err";
      err.textContent = "Error: " + (e && e.message ? e.message : String(e));
      out.pre.appendChild(err);
    } finally {
      shelter.purge();
    }
  }

  function run(blocks, index) {
    queue = queue.then(async function () {
      blocks.forEach(function (b) { b.button.disabled = true; });
      try {
        await start();
        // Earlier chunks first, so the session matches the guide.
        for (var i = 0; i <= index; i++) {
          if (!blocks[i].done || i === index) {
            setStatus("Running chunk " + (i + 1) + " of " + blocks.length + "…");
            await execute(blocks[i]);
          }
        }
        setStatus("Done. Edit any chunk and run it again.");
      } catch (e) {
        setStatus("Error: " + e.message);
      } finally {
        blocks.forEach(function (b) { b.button.disabled = false; });
      }
    });
  }

  function setup() {
    var pres = Array.prototype.slice.call(document.querySelectorAll("div.webr-chunk pre.sourceCode"));
    if (!pres.length) return;
    var blocks = pres.map(function (pre) {
      var wrapper = pre.closest("div.sourceCode") || pre;
      return { pre: pre, wrapper: wrapper, done: false };
    });

    var panel = document.createElement("div");
    panel.className = "webr-panel";
    panel.innerHTML = "<strong>Run this guide in your browser.</strong> " +
      "R runs on your own device with <a href='https://docs.r-wasm.org/webr/'>webR</a>, " +
      "against the simulated REDCap project, so no REDCap server or token is needed. " +
      "Press <em>Run</em> on any chunk; earlier chunks run first. " +
      "<div class='webr-bar'><button type='button' class='webr-start'>Start R</button>" +
      "<span class='webr-status'>R is not started.</span></div>";
    blocks[0].wrapper.parentNode.insertBefore(panel, blocks[0].wrapper);
    panel.querySelector(".webr-start").addEventListener("click", function () { start(); });

    blocks.forEach(function (b, i) {
      var bar = document.createElement("div");
      bar.className = "webr-bar";
      var btn = document.createElement("button");
      btn.type = "button";
      btn.textContent = "Run";
      var edit = document.createElement("button");
      edit.type = "button";
      edit.textContent = "Edit";
      edit.addEventListener("click", function () {
        var code = b.pre.querySelector("code") || b.pre;
        var on = code.getAttribute("contenteditable") === "true";
        code.setAttribute("contenteditable", on ? "false" : "true");
        code.classList.toggle("webr-editing", !on);
        code.setAttribute("spellcheck", "false");
        edit.textContent = on ? "Edit" : "Stop editing";
        if (!on) code.focus();
      });
      btn.addEventListener("click", function () { run(blocks, i); });
      bar.appendChild(btn);
      bar.appendChild(edit);
      b.wrapper.parentNode.insertBefore(bar, b.wrapper.nextSibling);
      b.bar = bar;
      b.button = btn;
    });
  }

  if (document.readyState === "loading") document.addEventListener("DOMContentLoaded", setup);
  else setup();
})();
