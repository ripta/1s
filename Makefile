build-debug:
	cargo build

build-release:
	cargo build --release

fix:
	cargo clippy --fix --bin "1s" --allow-dirty --allow-staged -- -A clippy::needless_return

lint:
	cargo clippy -- -A clippy::needless_return

repl:
	cargo run --bin 1s -- -i lib/prelude.1s lib/repl.1s lib/math.1s lib/seq.1s lib/logic.1s

test:
	cargo test
	./scripts/gauntlet.sh

update-golden:
	./scripts/gauntlet.sh -u
