# Prolog Playground

A simple HTML-based playground UI that talks to the Prolog HTTP server via a REST API.

## Start the server (Docker)

1. Start the dev container:

```bash
./scripts/dev.sh start
```

2. Enter the container:

```bash
./scripts/dev.sh shell
```

3. Start the Prolog HTTP server:

```bash
cd /workspace
swipl -s prolog/http_server.pl -g "game_http_server:start_server(8080)"
```

## Access from the host

The server binds to `0.0.0.0:8080` inside the container.

Depending on your compose port mapping, you may access it via:

- Container: `http://localhost:8080`
- Host: `http://localhost:8081` (commonly mapped in `docker-compose.dev.yml`)

## API endpoints

- `GET /api/status` - get game status
- `POST /api/init` - initialize game
- `POST /api/command` - execute a command
- `GET /api/map` - get map data
- `GET /` - open the playground UI

## Examples

```bash
curl -X POST http://localhost:8081/api/init
curl http://localhost:8081/api/status
curl -X POST -H "Content-Type: application/json" -d '{"command":"look"}' http://localhost:8081/api/command
```



