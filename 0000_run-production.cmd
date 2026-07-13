@echo off
setlocal
powershell.exe -NoProfile -ExecutionPolicy Bypass -File "%~dp0CCMM_2026_wp--production.ps1"
exit /b %ERRORLEVEL%
