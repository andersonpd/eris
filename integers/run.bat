@echo off
title unittest
prompt [$T]$_
dub build --build=unittest
pause
