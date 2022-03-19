.PHONY: all
all: README.md
	$(warning No default target (yet)!)

GUIX := guix
# nix via guix shell ? @2.5.1 (I'm at 2.6.1)
NIX := nix --extra-experimental-features "flakes nix-command"

README.md: channels.scm manifest.scm flake.lock guix/scripts/*
	$(GUIX) time-machine -C channels.scm -- \
	  shell --pure --manifest=manifest.scm -- \
	  scribble --markdown --link-section \
	  --dest-name \#tmp-$@ \
	  guix/scripts/self-readme.scrbl
	mv \#tmp-$@ $@

flake.lock:
	$(error To do!)

channels.scm:
	echo ";; -*- mode: scheme -*-" > \#tmp-$@
	echo ';; to update this file, run `make $@` in the' >> \#tmp-$@
	echo ';; `build-scripts` branch of the source repository' >> \#tmp-$@
	$(GUIX) describe -f channels >> \#tmp-$@
	mv \#tmp-$@ $@
