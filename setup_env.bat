@echo off
REM ==============================================================================
REM SETUP ENV - QuantumLLAMA ETF Pro (Windows)
REM ==============================================================================

echo 🔧 Configurando el entorno de desarrollo...

REM 1. Instalar dependencias de Node.js
if exist resources\app (
    echo 📦 Instalando dependencias de Node.js...
    cd resources\app
    call npm install
    cd ..\..
) else (
    echo ❌ Error: No se encontró la carpeta resources\app
)

REM 2. Instalar dependencias de R
echo 📊 Instalando dependencias de R...
Rscript -e "if (!require('pacman')) install.packages('pacman', repos = 'https://cloud.r-project.org')"
Rscript -e "pacman::p_load(quantmod, tidyquant, PerformanceAnalytics, tidyverse, xts, lubridate, caret, ranger, quadprog, tseries, TTR, zoo, gridExtra, shiny, shinydashboard, DT, openxlsx, doParallel, foreach)"

echo ✅ Configuración completada con éxito.
pause
