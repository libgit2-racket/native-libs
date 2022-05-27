.PHONY: all
all: README.md
	$(warning Run `make update` to update Nix and Guix lock files.)
	$(warning Run `make show` to see Nix commands.)

.PHONY: update
update: channels.scm flake.lock
	$(MAKE) --always-make $^
	$(MAKE) all

GUIX := guix
GUIX_TIME_MACHINE := $(GUIX) time-machine -C channels.scm
GUIX_SHELL := $(GUIX_TIME_MACHINE) -- shell --pure --manifest=manifest.scm
NIX := nix
NIX_FEATURES := --extra-experimental-features "flakes nix-command"
GUIX_SHELL_FOR_NIX := $(GUIX_SHELL) --
RUN_NIX := $(GUIX_SHELL_FOR_NIX) $(NIX) $(NIX_FEATURES)

.PHONY: show
show:
	$(RUN_NIX) flake show
	$(RUN_NIX) flake metadata

# real targets

RACKET_RELEASES := https://download.racket-lang.org/releases
RACKET_VERSION_CMD := racket -e "(display (version))"
README.md: channels.scm manifest.scm flake.lock guix/scripts/*
	$(GUIX_SHELL) -- \
	    scribble --markdown --link-section \
	    ++main-xref-in \
	    --redirect-main $(RACKET_RELEASES)/`$(RACKET_VERSION_CMD)`/doc/ \
	    --dest-name \#tmp-$@ \
	    guix/scripts/self-readme.scrbl
	mv \#tmp-$@ $@

flake.lock:
	$(RUN_NIX) flake update

channels.scm:
	echo ";; -*- mode: scheme -*-" > \#tmp-$@
	echo ';; To update this file, run `make update` in the' >> \#tmp-$@
	echo ';; `build-scripts` branch of the source repository.' >> \#tmp-$@
	$(GUIX) describe -f channels >> \#tmp-$@
	mv \#tmp-$@ $@
