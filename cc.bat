@echo off

IF "%2" == "-build-compiler" (
	odin build . -debug
)

crest.exe %1