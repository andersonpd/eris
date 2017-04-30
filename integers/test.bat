@echo off
title unittest
prompt [$T]$_
dub build --force --build=unittest
rem --force --build=unittest
integer > zzz.txt
pause
