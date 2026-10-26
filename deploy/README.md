# Deploying REDCap CDMSTAT

The production layout is:

```
browser ──HTTPS──> nginx or Caddy :443 ──> gateway 127.0.0.1:8080 ──> R engine 127.0.0.1:8000 ──> REDCap API
REDCap DET ──HTTPS──> https://cdmstat.example.org/det?token=…
```

Only the reverse proxy is reachable from the network. The R engine accepts
requests only when they carry the gateway secret.

## Linux (Ubuntu / RHEL)

1. Create the service user and folders:

   ```bash
   sudo useradd --system --home /var/lib/cdmstat cdmstat
   sudo mkdir -p /opt/cdmstat /var/lib/cdmstat /etc/cdmstat
   sudo chown cdmstat:cdmstat /var/lib/cdmstat
   sudo chmod 700 /var/lib/cdmstat
   ```

2. Install R and the package (`R CMD INSTALL REDCapCDMSTAT`) plus its
   suggested packages (shiny, bslib, plotly, DT, plumber, survival, lme4,
   xml2, haven).
3. Copy `web/` to `/opt/cdmstat/web`, then create the Python environment:

   ```bash
   python3 -m venv /opt/cdmstat/venv
   /opt/cdmstat/venv/bin/pip install -r /opt/cdmstat/web/gateway/requirements.txt
   ```

4. Copy `cdmstat.env.example` to `/etc/cdmstat/cdmstat.env` and fill it in
   (owner `root:cdmstat`, mode `0640`).
5. Create users. Each key is printed once:

   ```bash
   cd /opt/cdmstat/web/gateway
   sudo -u cdmstat CDMSTAT_USERS_FILE=/var/lib/cdmstat/users.json /opt/cdmstat/venv/bin/python manage_users.py add dana dm
   ```

6. Install `cdmstat-api.service` and `cdmstat-gateway.service` into
   `/etc/systemd/system/`, then run `systemctl enable --now cdmstat-api cdmstat-gateway`.
7. Configure the reverse proxy with `nginx-cdmstat.conf` (plus certbot) or
   `Caddyfile` (automatic TLS).
8. In REDCap, set the Data Entry Trigger URL to
   `https://cdmstat.example.org/det?token=<CDMSTAT_DET_TOKEN>`.

## Windows Server

- Run the two processes as services with [NSSM](https://nssm.cc/) or Task
  Scheduler ("run whether user is logged on"), using the same commands as the
  `ExecStart` lines in the unit files. Set the environment variables at the
  service level.
- Put IIS (with URL Rewrite, ARR and WebSocket support enabled) or Caddy in
  front for HTTPS.
- Keep `CDMSTAT_DIR` on a BitLocker-encrypted volume.

## Security checklist

- [ ] HTTPS only, with HSTS; ports 8000 and 8080 are firewalled from the network
- [ ] `CDMSTAT_GATEWAY_SECRET` and `CDMSTAT_DET_TOKEN` are random (32+ bytes) and stored only in the service environment
- [ ] The REDCap token belongs to a dedicated service account with the minimum rights: export, plus import if write-back is used, plus the DRW and record-locking rights if the External Module is used
- [ ] `CDMSTAT_DIR` (cache, ledger, cuts) is on encrypted storage (LUKS or BitLocker), with mode 0700, owned by the service user
- [ ] The data directory is backed up daily and a restore has been tested (PQ-18)
- [ ] Access logs strip the `Authorization` header and the `key` query parameter (the provided configs do this)
- [ ] One personal key per person (`manage_users.py`); keys are removed when people leave
- [ ] OS, R, Python and REDCap patching is on a schedule
- [ ] IQ/OQ (`Rscript validation/run_validation.R`) is re-run and archived after every upgrade
