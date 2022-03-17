.PHONY: all
all:
	$(warning No default target (yet)!)

NIX := nix --extra-experimental-features "flakes nix-command"
GUIX := guix

channels.scm:
	echo ";; -*- mode: scheme -*-" > \#tmp-$@
	echo ';; to update this file, run `make $@`' >> \#tmp-$@
	$(GUIX) describe -f channels >> \#tmp-$@
	mv \#tmp-$@ $@
