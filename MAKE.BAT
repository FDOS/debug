@echo off
echo creating debug.com
jwasm -nologo -DPM=0 -bin -Fo DEBUG.COM -Fl debug.asm 
echo creating debugx.com
jwasm -nologo -DPM=1 -bin -Fo DEBUGX.COM -Fl=debugx.lst -Sa debug.asm
