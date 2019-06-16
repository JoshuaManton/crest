@echo off

IF "%2" == "-build-compiler" (
	odin build . -debug
)

crest.exe %1

rem odin run output
