all:
	rustc lib.rs --opt-level 3

test:
	rustc lib.rs --test -Z extra-debug-info

debug:
	rustc lib.rs -Z extra-debug-info

clean:
	rm liblz4* lz4