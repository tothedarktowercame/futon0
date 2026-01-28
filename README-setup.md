# Futon Stack Setup

Prerequisites and configuration for running the full futon stack.

## System Dependencies

### nginx (for WSS/TLS)

The MUSN activity WebSocket requires TLS termination via nginx.

```bash
sudo apt install nginx
```

Create `/etc/nginx/sites-available/musn-wss`:

```nginx
server {
    listen 6066 ssl;
    server_name YOUR_DOMAIN;

    ssl_certificate /etc/letsencrypt/live/YOUR_DOMAIN/fullchain.pem;
    ssl_certificate_key /etc/letsencrypt/live/YOUR_DOMAIN/privkey.pem;

    location / {
        proxy_pass http://127.0.0.1:6065;
        proxy_http_version 1.1;
        proxy_set_header Upgrade $http_upgrade;
        proxy_set_header Connection "upgrade";
        proxy_set_header Host $host;
        proxy_set_header X-Real-IP $remote_addr;
    }
}
```

Enable and reload:

```bash
sudo ln -s /etc/nginx/sites-available/musn-wss /etc/nginx/sites-enabled/
sudo nginx -t && sudo systemctl reload nginx
```

### certbot (for Let's Encrypt certificates)

```bash
sudo apt install certbot
sudo certbot certonly --standalone -d YOUR_DOMAIN
```

Certificates renew automatically via systemd timer.

## Environment Variables

Key variables for futon3/MUSN:

| Variable | Description | Example |
|----------|-------------|---------|
| `MUSN_PORT` | HTTP port for MUSN | `6065` |
| `MUSN_SSL_PORT` | (unused, nginx handles TLS) | - |
| `MUSN_SSL_DOMAIN` | (unused, nginx handles TLS) | - |

## Ports

| Port | Service | Protocol |
|------|---------|----------|
| 5050 | Transport (HUD) | HTTP |
| 6060 | UI | HTTP |
| 6065 | MUSN HTTP + WS | HTTP |
| 6066 | MUSN WSS (via nginx) | HTTPS |
| 6667 | IRC bridge | IRC |
| 8080 | Futon1 API | HTTP |

## Quick Start

```bash
cd futon3
make dev
```

For WSS, ensure nginx is running with the config above.

---

*Last updated: 2026-01-28*
