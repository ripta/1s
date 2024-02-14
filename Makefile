build-debug:
	cargo build

build-release:
	cargo build --release

fix:
	cargo clippy --fix --bin "1s" --allow-dirty --allow-staged -- -A clippy::needless_return

lint:
	cargo clippy -- -A clippy::needless_return

repl:
	cargo run --bin 1s -- -l -i prelude repl math seq logic shuffle

repl-opt:
	cargo run --release --bin 1s -- -l -i prelude repl math seq logic shuffle

test:
	cargo test
	./scripts/gauntlet.sh

update-golden:
	./scripts/gauntlet.sh -u
