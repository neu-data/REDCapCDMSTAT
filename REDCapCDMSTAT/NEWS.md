# REDCapCDMSTAT 0.1.0

First release.

* **Engine:** incremental sync from the REDCap log and Data Entry Trigger,
  data dictionary drift detection, and branching logic and calculations
  compiled from the live dictionary.
* **Data quality:** dictionary-driven and custom validation rules, and an
  audited query ledger with auto-resolution and write-back.
* **REDCap DRW:** two-way sync with the Data Resolution Workflow through the
  CDMSTAT DRW External Module, including REDCap form locks and SDV
  verification.
* **CDMS modules:** stratified block randomisation, risk-based SDV, locks
  and hash-bound e-signatures, medical coding, protocol deviations, site
  metrics and portfolio views.
* **Statistics:** long/wide wrangling, Table 1 with guarded or SAP-specified
  tests, outlier-to-query, trends, and linear, logistic, Cox and mixed
  models.
* **Reproducibility and standards:** hashed and signed data cuts, a
  replayable script log, a Quarto report, SDTM (DM, VS, LB, AE, SV, DS)
  with define.xml, and CDISC ODM import and export.
* **Sources:** REDCap, REDCap Cloud (REST) and any EDC through CDISC ODM;
  `cdmstat_setup()` and `redcap_check()` connect and diagnose a project.
* **Interfaces:** Shiny app, portfolio dashboard, plumber REST API, FastAPI
  gateway with roles, and a web frontend.
* **Validation and deployment:** validation pack (IQ/OQ runner,
  requirements traceability, PQ protocol) and an HTTPS deployment kit.
