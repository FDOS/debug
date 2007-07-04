@echo off
nasmw dpmicl32.asm -O 2 -f obj -o dpmicl32.obj
valx /32 /MAP dpmicl32.obj;
