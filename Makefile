all: build

build:
	sbt assembly
	chmod +x play_icfp2015
