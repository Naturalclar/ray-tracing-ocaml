.DEFAULT_GOAL := build

build:
	dune build

clean:
	rm -rf _build

run:
	dune exec Raytrace
	open image.ppm
