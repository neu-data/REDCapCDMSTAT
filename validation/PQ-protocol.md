# Performance qualification (PQ) protocol: REDCap CDMSTAT

PQ shows the system works for **your** study, on **your** infrastructure,
with **your** users. Run it after a passing IQ/OQ report from
`Rscript validation/run_validation.R`, on a development or UAT copy of
the production REDCap project. Never run it on live trial data.

Record the actual result of every step. Attach screenshots and exports as
evidence, and log any deviation from expected results with its impact
assessment.

## 1. Scope and environment

| Item | Value |
|---|---|
| Study / protocol | |
| REDCap version and URL (UAT) | |
| CDMSTAT version / IQ-OQ report file | |
| DRW External Module version | |
| Server, OS, R version | |
| Gateway URL (HTTPS) | |
| Test users and roles (dm, stats, monitor, pi, viewer) | |

## 2. Test cases

| # | Requirement | Procedure | Expected result | Actual / evidence | Pass? |
|---|---|---|---|---|---|
| PQ-01 | URS-01 | Connect to the UAT project with a keyring-stored token; run a full sync | Record and field counts match REDCap's Record Status Dashboard | | |
| PQ-02 | URS-01, URS-02 | Edit a value in REDCap; save the form | Within seconds (DET) or at the next sync, CDMSTAT shows the new value; the sync log lists 1 changed record | | |
| PQ-03 | URS-05 | Change a validation max in the Online Designer; sync | Dictionary drift is logged with the changed column; new range applied | | |
| PQ-04 | URS-06, URS-07 | Enter 5 known errors (range, required, hidden field, calc, custom rule) | Each error appears once under Data quality | | |
| PQ-05 | URS-08, URS-10 | Raise queries; open the REDCap DRW pop-up for one field | The query shows in REDCap with the CDMSTAT text and attribution | | |
| PQ-06 | URS-10 | As a site user, respond in the REDCap DRW; sync in CDMSTAT | The CDMSTAT query becomes "answered", attributed to `redcap:<user>` | | |
| PQ-07 | URS-09 | Correct the value in REDCap; sync and raise queries | The query auto-resolves; the DRW thread is CLOSED | | |
| PQ-08 | URS-11 | Use a token whose user has only "View" DRW rights | Write actions return 403; nothing changes in REDCap | | |
| PQ-09 | URS-16 | Lock a record in CDMSTAT; try to edit it in REDCap | REDCap refuses the edit; unlock needs a reason and is logged | | |
| PQ-10 | URS-16 | E-sign a record in the web app with a wrong key, then with the right key | Wrong key: refused (403). Right key: signature valid; after an edit: INVALID | | |
| PQ-11 | URS-14 | Set up stratified randomisation; randomise 8 test subjects in one stratum | Balance per block; a second attempt for a subject is refused; the arm is written to REDCap | | |
| PQ-12 | URS-15 | Apply an SDV plan; verify one form as a monitor; edit it in REDCap | The unit becomes "reverify"; DRW shows the values as verified before the edit | | |
| PQ-13 | URS-17 | Load the licensed MedDRA LLT file; auto-code AE terms | Exact and synonym matches are coded; others are queued; approvals are logged | | |
| PQ-14 | URS-13 | Freeze and sign off a cut; render the report with `require_signoff = TRUE` | The report shows the cut hash and sign-off; tampering with the cut file blocks rendering | | |
| PQ-15 | URS-20, URS-21 | Compare Table 1 and the primary model with an independent SAS/R program | Estimates and p-values agree to the stated precision | | |
| PQ-16 | URS-22 | Export SDTM; run the sponsor's conformance checker (e.g. Pinnacle 21) on the XPT files and define.xml | Findings are reviewed and either resolved or justified | | |
| PQ-17 | Security | Check that the API is unreachable without the gateway, that the gateway uses HTTPS only, and that keys are not in logs | All confirmed | | |
| PQ-18 | Backup | Restore the CDMSTAT data directory from backup to a clean server | Ledger, cuts and audit trail are intact and their hashes verify | | |

## 3. Acceptance

The system is accepted for use on this study when every PQ case passes, or
when each failure has a documented, approved justification.

| Role | Name | Signature | Date |
|---|---|---|---|
| Executed by | | | |
| Reviewed by (QA) | | | |
| Approved by (study / system owner) | | | |
