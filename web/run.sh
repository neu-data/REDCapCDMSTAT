#!/usr/bin/env bash
# Start the R engine (plumber, localhost only) and the FastAPI gateway.
set -euo pipefail
here="$(cd "$(dirname "$0")" && pwd)"
API_PORT="${API_PORT:-8000}"
WEB_PORT="${WEB_PORT:-8080}"

export CDMSTAT_GATEWAY_SECRET="${CDMSTAT_GATEWAY_SECRET:-$(python3 -c 'import secrets;print(secrets.token_hex(16))')}"
if [ -z "${CDMSTAT_DET_TOKEN:-}" ]; then
  export CDMSTAT_DET_TOKEN="$(python3 -c 'import secrets;print(secrets.token_hex(16))')"
  echo "DET URL for REDCap: http://<this-host>:${WEB_PORT}/det?token=${CDMSTAT_DET_TOKEN}"
fi

cd "$here/gateway"
if [ ! -f users.json ]; then
  echo "Created users (keys are shown once):"
  echo "  data_manager (dm):    $(python3 manage_users.py add data_manager dm)"
  echo "  statistician (stats): $(python3 manage_users.py add statistician stats)"
fi

Rscript -e "REDCapCDMSTAT::run_api(port=${API_PORT})" &
api_pid=$!
trap 'kill $api_pid 2>/dev/null' EXIT
CDMSTAT_R_API="http://127.0.0.1:${API_PORT}" python3 -m uvicorn main:app --host 127.0.0.1 --port "$WEB_PORT"
