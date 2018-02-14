.PHONY: all
all:
	@cargo build --release

.PHONY: debug
release:
	@cargo build

.PHONY: clean
clean:
	@cargo clean

.PHONY: run
run:
	@cargo run --release

.PHONY: debugrun
debugrun:
	@cargo run