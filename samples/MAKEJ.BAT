@echo off
rem assemble the samples which are written in MASM syntax
rem JWasm and Open Watcom's WLINK are used.
jwasm -nologo -bin -Fo DPMICL16.COM DPMICL16.ASM
jwasm -nologo DPMICL32.ASM
wlink sys dos file DPMICL32.OBJ op quiet
