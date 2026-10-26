# Start the R engine (plumber, localhost only) and the FastAPI gateway.
#   .\run.ps1                      demo project
#   $env:CDMSTAT_URL = "https://redcap.example.org/api/"; $env:REDCAP_API_TOKEN = "..."; .\run.ps1
param([int]$ApiPort = 8000, [int]$WebPort = 8080)

$ErrorActionPreference = "Stop"
$here = Split-Path -Parent $MyInvocation.MyCommand.Path
$rscript = (Get-ChildItem "C:\Program Files\R\*\bin\Rscript.exe" | Sort-Object FullName | Select-Object -Last 1).FullName

if (-not $env:CDMSTAT_GATEWAY_SECRET) { $env:CDMSTAT_GATEWAY_SECRET = [guid]::NewGuid().ToString("N") }
if (-not $env:CDMSTAT_DET_TOKEN) {
  $env:CDMSTAT_DET_TOKEN = [guid]::NewGuid().ToString("N")
  Write-Host "DET URL for REDCap: http://<this-host>:$WebPort/det?token=$($env:CDMSTAT_DET_TOKEN)"
}

$users = Join-Path $here "gateway\users.json"
if (-not (Test-Path $users)) {
  Push-Location (Join-Path $here "gateway")
  $dm = python manage_users.py add data_manager dm
  $st = python manage_users.py add statistician stats
  Pop-Location
  Write-Host "Created users (keys are shown once):"
  Write-Host "  data_manager (dm):    $dm"
  Write-Host "  statistician (stats): $st"
}

$api = Start-Process -PassThru -NoNewWindow $rscript -ArgumentList "-e", "REDCapCDMSTAT::run_api(port=$ApiPort)"
try {
  $env:CDMSTAT_R_API = "http://127.0.0.1:$ApiPort"
  Push-Location (Join-Path $here "gateway")
  python -m uvicorn main:app --host 127.0.0.1 --port $WebPort
} finally {
  Pop-Location
  Stop-Process -Id $api.Id -ErrorAction SilentlyContinue
}
