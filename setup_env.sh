#!/bin/bash

# ==============================================================================
# SETUP ENV - QuantumLLAMA ETF Pro (Linux/macOS)
# ==============================================================================

echo "🔧 Configurando el entorno de desarrollo..."

# 1. Instalar dependencias de Node.js
if [ -d "resources/app" ]; then
    echo "📦 Instalando dependencias de Node.js..."
    cd resources/app && npm install
    cd ../..
else
    echo "❌ Error: No se encontró la carpeta resources/app"
fi

# 2. Instalar dependencias de R
echo "📊 Instalando dependencias de R..."
Rscript -e "if (!require('pacman')) install.packages('pacman', repos = 'https://cloud.r-project.org')"
Rscript -e "pacman::p_load(quantmod, tidyquant, PerformanceAnalytics, tidyverse, xts, lubridate, caret, ranger, quadprog, tseries, TTR, zoo, gridExtra, shiny, shinydashboard, DT, openxlsx, doParallel, foreach)"

echo "✅ Configuración completada con éxito."
