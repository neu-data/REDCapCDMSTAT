"""REDCap CDMSTAT gateway.

Sits in front of the R plumber API (which must listen on localhost only) and
adds what R is not good at: per-user API-key authentication with roles,
WebSocket push notifications, a public Data Entry Trigger endpoint protected
by its own token, and serving the web frontend.

Environment:
  CDMSTAT_R_API            URL of the plumber API (default http://127.0.0.1:8000)
  CDMSTAT_GATEWAY_SECRET   shared secret sent to plumber in X-CDMSTAT-Secret (required)
  CDMSTAT_DET_TOKEN        token REDCap must append to the DET URL (?token=...)
  CDMSTAT_USERS_FILE       users file created by manage_users.py (default ./users.json)
"""

from __future__ import annotations

import hmac
import json
import os
from pathlib import Path

import httpx
from fastapi import FastAPI, HTTPException, Request, WebSocket, WebSocketDisconnect
from fastapi.responses import JSONResponse, Response
from fastapi.staticfiles import StaticFiles

from main_hash import hash_key

HERE = Path(__file__).resolve().parent
R_API = os.environ.get("CDMSTAT_R_API", "http://127.0.0.1:8000").rstrip("/")
SECRET = os.environ.get("CDMSTAT_GATEWAY_SECRET", "")
DET_TOKEN = os.environ.get("CDMSTAT_DET_TOKEN", "")
USERS_FILE = Path(os.environ.get("CDMSTAT_USERS_FILE", HERE / "users.json"))
STATIC = HERE.parent / "static"

if not SECRET:
    raise SystemExit("CDMSTAT_GATEWAY_SECRET must be set (and given to the R API too).")

app = FastAPI(title="REDCap CDMSTAT gateway", docs_url=None, redoc_url=None)
client = httpx.AsyncClient(timeout=120)


def load_users() -> dict:
    if not USERS_FILE.exists():
        return {}
    return json.loads(USERS_FILE.read_text())


def authenticate(key: str | None) -> dict:
    if not key:
        raise HTTPException(401, "Missing API key")
    user = load_users().get(hash_key(key))
    if not user:
        raise HTTPException(401, "Invalid API key")
    return user


def key_from_request(request: Request) -> str | None:
    auth = request.headers.get("authorization", "")
    return auth[7:] if auth.lower().startswith("bearer ") else None


class Hub:
    """Fan-out of change notifications to every connected browser."""

    def __init__(self) -> None:
        self.sockets: set[WebSocket] = set()

    async def broadcast(self, message: dict) -> None:
        dead = []
        for ws in self.sockets:
            try:
                await ws.send_json(message)
            except Exception:
                dead.append(ws)
        for ws in dead:
            self.sockets.discard(ws)


hub = Hub()


async def call_r(method: str, path: str, user: dict, *, params=None, content=None, headers=None):
    hdrs = {
        "X-CDMSTAT-Secret": SECRET,
        "X-CDMSTAT-User": user["name"],
        "X-CDMSTAT-Role": user["role"],
    }
    if headers:
        hdrs.update(headers)
    try:
        return await client.request(method, f"{R_API}/{path}", params=params, content=content, headers=hdrs)
    except httpx.ConnectError:
        raise HTTPException(503, "The R engine (plumber API) is not reachable")


@app.get("/auth/me")
async def me(request: Request):
    user = authenticate(key_from_request(request))
    return {"name": user["name"], "role": user["role"]}


@app.api_route("/api/{path:path}", methods=["GET", "POST"])
async def proxy(path: str, request: Request):
    key = key_from_request(request)
    user = authenticate(key)
    body = await request.body()
    ctype = request.headers.get("content-type")
    headers = {"content-type": ctype} if ctype else {}
    if path.strip("/") == "esign":
        # Electronic signatures need the signer to re-enter their credential
        # (21 CFR 11.200); it is checked here and never forwarded to R.
        try:
            payload = json.loads(body or b"{}")
        except ValueError:
            raise HTTPException(400, "JSON body required")
        credential = str(payload.pop("credential", ""))
        if not credential or not hmac.compare_digest(hash_key(credential), hash_key(key)):
            raise HTTPException(403, "Re-authentication failed: credential does not match the signed-in user")
        body = json.dumps(payload).encode()
        headers = {"content-type": "application/json", "X-CDMSTAT-Reauth": "1"}
    resp = await call_r(request.method, path, user, params=dict(request.query_params), content=body,
                        headers=headers or None)
    if request.method == "POST" and resp.status_code < 300:
        await hub.broadcast({"type": "changed", "path": path, "by": user["name"]})
    return Response(resp.content, status_code=resp.status_code,
                    media_type=resp.headers.get("content-type", "application/json"))


@app.post("/det")
async def data_entry_trigger(request: Request, token: str = ""):
    """Point REDCap's Data Entry Trigger at https://<host>/det?token=<CDMSTAT_DET_TOKEN>."""
    if not DET_TOKEN or not hmac.compare_digest(token, DET_TOKEN):
        raise HTTPException(403, "Bad DET token")
    body = await request.body()
    system = {"name": "redcap-det", "role": "dm"}
    resp = await call_r("POST", "det", system, content=body,
                        headers={"content-type": request.headers.get("content-type",
                                                                     "application/x-www-form-urlencoded")})
    form = dict(x.split("=", 1) for x in body.decode().split("&") if "=" in x)
    await hub.broadcast({"type": "data", "record": form.get("record"),
                         "instrument": form.get("instrument"), "by": "REDCap"})
    return JSONResponse(resp.json() if resp.status_code < 300 else {"error": resp.text},
                        status_code=resp.status_code)


@app.websocket("/ws")
async def ws(websocket: WebSocket, key: str = ""):
    try:
        user = authenticate(key)
    except HTTPException:
        await websocket.close(code=4401)
        return
    await websocket.accept()
    hub.sockets.add(websocket)
    await hub.broadcast({"type": "presence", "user": user["name"], "online": len(hub.sockets)})
    try:
        while True:
            await websocket.receive_text()
    except WebSocketDisconnect:
        hub.sockets.discard(websocket)


@app.on_event("shutdown")
async def shutdown() -> None:
    await client.aclose()


app.mount("/", StaticFiles(directory=STATIC, html=True), name="static")
