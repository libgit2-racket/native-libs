libgit2-x86_64-macosx/libgit2.28.dylib: build/libgit2.28.dylib
	cp build/libgit2.28.dylib libgit2-x86_64-macosx/libgit2.28.dylib

build/libgit2.28.dylib: | build
	cmake -S src/ -B build/
	cmake --build build/
	cd build && ctest -V

build:
	mkdir build


.PHONY: clean
clean:
	rm -rf build

