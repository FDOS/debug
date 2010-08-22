@echo off
rem assemble the NASM samples
nasm dpmicl16.nsm -O 2 -f bin -o dpmicl16.com
nasm dpmicl32.nsm -O 2 -f bin -o dpmicl32.com
