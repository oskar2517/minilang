@echo off
cd src
ghc --make Main.hs -outputdir../build -o ../interpreter.exe