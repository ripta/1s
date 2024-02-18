build-debug:
	cargo build --bin 1s --features readline

build-release:
	cargo build --release --bin 1s --features readline

fix:
	cargo clippy --fix --bin "1s" --allow-dirty --allow-staged -- -A clippy::needless_return

lint:
	cargo clippy -- -A clippy::needless_return

repl:
	cargo run --features readline --bin 1s -- -l -i base repl

repl-opt:
	cargo run --features readline --release --bin 1s -- -l -i base repl

test:
	cargo test
	./scripts/gauntlet.sh

update-golden:
	./scripts/gauntlet.sh -u

wasm:
	wasm-pack build -t web
