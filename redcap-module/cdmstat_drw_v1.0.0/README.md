# CDMSTAT Data Resolution Workflow API (REDCap External Module)

REDCap's Data Resolution Workflow (DRW) is the query pop-up next to every
field, but the standard REDCap API has no way to read or write it. This
module adds Module API actions for it, so REDCap CDMSTAT, or any client
holding a project API token, can:

- open, respond to, close and reopen field-level queries,
- add comments,
- mark values verified or de-verified, and
- read query threads back, incrementally.

Site staff keep working in the familiar DRW pop-up and resolve dashboard.

## Requirements

- A REDCap version whose External Module framework supports **Module API
  actions** (`api-actions` in `config.json`; REDCap 14.x and later). The
  module declares `framework-version: 15`.
- PHP 7.4 or later.
- The project must use the **Data Resolution Workflow** (Project Setup →
  Additional customizations → "Enable the Field Comment Log or Data
  Resolution Workflow" → *Data Resolution Workflow*).

## Install

1. Copy the folder `cdmstat_drw_v1.0.0` into `<redcap root>/modules/`.
2. In **Control Center → External Modules → Manage**, enable *CDMSTAT Data
   Resolution Workflow API*.
3. In the project, go to **External Modules → Manage** and enable it. The
   optional settings are:
   - *Restrict write actions to these API users*: for example `svc_cdmstat`.
   - *Prefix comments with the CDMSTAT user*: recommended when a shared
     service account holds the token. Comments then read
     `[CDMSTAT: dana] ...`.
4. Create, or choose, the API user. Give it **API Import/Export** and the
   DRW right **"Open, close, and respond to queries"**, then generate a
   project API token.
5. Check the connection:

   ```bash
   curl -F token=$TOKEN -F content=externalModule -F prefix=cdmstat_drw -F action=drw-status -F returnFormat=json https://<redcap>/api/
   ```

   You should see `"drw_enabled": true, "schema_ok": true, "rights_level": 3`.

6. In CDMSTAT:

   ```r
   nx <- cdmstat_connect("https://<redcap>/api/", token_env = "REDCAP_API_TOKEN")
   nx$drw_enable()      # checks the module, the DRW and the rights
   nx$drw_sync()        # also runs automatically after every nx$sync()
   ```

   You can do the same from the Shiny app's Queries tab, or the web app's
   Queries board.

## Actions

All actions need a project token and `returnFormat=json`.

| Action | Parameters | Needs DRW right |
|---|---|---|
| `drw-status` | none | any |
| `list-queries` | `since_res_id`, `record` | view |
| `open-query` | `record`, `field`, `comment`, plus optional `event`, `instance`, `repeat_instrument`, `assign_to`, `actor` | open |
| `respond` | `status_id`, `comment`, plus optional `response` (`DATA_MISSING`, `TYPOGRAPHICAL_ERROR`, `CONFIRMED_CORRECT`, `WRONG_SOURCE`, `OTHER`) and `actor` | respond |
| `close-query` | `status_id`, `comment`, `actor` | open, close and respond |
| `reopen-query` | `status_id`, `comment`, `actor` | open |
| `add-comment` | `status_id`, `comment`, `actor` | respond |
| `verify-field` / `deverify-field` | `record`, `field`, plus optional `event`, `instance`, `repeat_instrument`, `comment` (required to de-verify) | open, close and respond |

`open-query` is idempotent: if the field already has an open query, it
returns that query's `status_id`.

## How CDMSTAT maps statuses

| CDMSTAT ledger | REDCap DRW |
|---|---|
| open | OPEN, response requested |
| answered | OPEN with a response |
| resolved / closed | CLOSED |
| reopened | OPEN, response requested again |

Sync works both ways:

- **Push:** new and changed CDMSTAT queries are written to the DRW.
- **Pull:** site responses, closures and reopenings made in REDCap update
  the ledger, attributed to `redcap:<username>`. Queries that users opened
  directly in REDCap are imported with source `redcap-drw`.

Each link stores the last DRW resolution it has seen, so nothing is applied
twice. Queries that aren't about a project field (for example "record changed
after lock") stay CDMSTAT-only.

## Safety

- **Schema guard.** REDCap does not publish the DRW schema. Before every
  write, the module checks that `redcap_data_quality_status` and
  `redcap_data_quality_resolutions` have the expected columns. If a REDCap
  upgrade changes them, it returns HTTP 501 and refuses to write.
  `drw-status` lists anything missing.
- **Rights.** Each action is checked against the token user's
  `data_quality_resolution` right; a role assignment takes precedence over
  individual rights. Superusers are allowed.
- **Validation.** Field, record and event are checked before writing.
  Changes run in a transaction.
- **Audit.** Every write is recorded in REDCap's project Logging
  (`CDMSTAT DRW API: ...`) and in REDCap's API log.

## Tests

```bash
php tests/run_tests.php
```

The 34 checks run against a stub framework and an SQLite copy of the DRW
tables. They cover the lifecycle, idempotency, rights levels, role overrides,
the allowed-users setting, the schema guard, verification rules and
attribution. Before production use, test on a development REDCap copy of
your project. The module writes the same tables as the DRW pop-up, so
confirm in the pop-up that threads display as expected on your REDCap
version.

## Not covered

- Data Quality rule discrepancies (`rule_id` / `pd_rule_id` rows): only
  field-level queries (`non_rule = 1`) are handled.
- File attachments on DRW comments.
- The Field Comment Log mode.
- DRW email notifications: REDCap sends these from its own UI actions, so
  check whether your version also sends them for writes made by this module.
