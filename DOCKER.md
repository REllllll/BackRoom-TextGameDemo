# Docker Guide

This project uses Docker Compose to provide a ready-to-run environment with **SWI-Prolog** and **PDDL4J** tooling.

## Prerequisites

- Docker
- Docker Compose (either `docker compose` or `docker-compose`)

## Helper script

Use `./scripts/dev.sh` for common workflows.

### Sudo auto-detection

If your user cannot access Docker directly, the script will auto-use `sudo`. You can also force behavior:

```bash
# Force using sudo
USE_SUDO=sudo ./scripts/dev.sh start

# Force not using sudo (e.g. user is in docker group)
USE_SUDO="" ./scripts/dev.sh start
```

## Dev mode (recommended)

Dev mode keeps an interactive container running for iteration and debugging.

### Start

```bash
./scripts/dev.sh start
```

### Enter shell

```bash
./scripts/dev.sh shell
```

### Run commands inside the container

```bash
./scripts/dev.sh run swipl --version
./scripts/dev.sh run swipl -s prolog/liminal_logic_game.pl -g start
./scripts/dev.sh run ff --help
```

### Logs / status / stop

```bash
./scripts/dev.sh logs
./scripts/dev.sh status
./scripts/dev.sh stop
```

## "Production" compose

The default `docker-compose.yml` starts the game directly:

```bash
docker-compose up --build
```

Stop and clean:

```bash
docker-compose down
docker-compose down -v
```

## Notes

- The project directory is mounted into the container at `/workspace`.
- Gradle cache is stored in the `pddl4j-cache` volume to speed up rebuilds.



