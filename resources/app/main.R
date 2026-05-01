# ==================================================================================
# MAIN - Sistema Cuantitativo ETF Pro v6.0 (Arquitectura Modular)
# ==================================================================================

# Cargar librerías necesarias
suppressWarnings(suppressMessages({
    if (!require("pacman")) install.packages("pacman", repos = "https://cloud.r-project.org")
    pacman::p_load(
        quantmod, tidyquant, PerformanceAnalytics, tidyverse, xts, lubridate,
        caret, ranger, quadprog, tseries, TTR, zoo, gridExtra,
        shiny, shinydashboard, DT, openxlsx, doParallel, foreach
    )
}))

options(warn = -1, scipen = 999, digits = 6)
Sys.setenv(TZ = "UTC")
set.seed(2025)

# ==================== CARGAR MÓDULOS ============================================

# Módulos Core (Backend - Lógica de negocio)
source("core/config.R", encoding = "UTF-8")
source("core/utils.R", encoding = "UTF-8")

# Por ahora, cargar el resto del código desde el archivo original
# TODO: Refactorizar gradualmente estos módulos
source("Estrategia-ETF-Pro.R", encoding = "UTF-8")

# ==================== FUNCIÓN PRINCIPAL =========================================

# La función ejecutar_pro_v6() ya está definida en Estrategia-ETF-Pro.R
# y está lista para usar

# ==================== CONFIGURAR RECURSOS ESTÁTICOS =============================

# Configurar ruta de imágenes para Shiny
if (dir.exists("imagenes")) {
    shiny::addResourcePath("imagenes", file.path(getwd(), "imagenes"))
}

# ==================== MENSAJE DE INICIO =========================================

cat("\n")
cat("═══════════════════════════════════════════════════════════\n")
cat("  SISTEMA CUANTITATIVO ETF PRO v6.0\n")
cat("  Arquitectura Modular - LLAMA IA Trading System\n")
cat("═══════════════════════════════════════════════════════════\n\n")
cat("✓ Módulos cargados correctamente\n")
cat("✓ Configuración lista\n")
cat("✓ Sistema listo para ejecutar\n\n")

# ==================== AUTO-EJECUCIÓN EN PRODUCCIÓN ==============================

# Detectar si estamos en entorno de producción (Render)
is_production <- !interactive() || Sys.getenv("RENDER") != ""

if (is_production) {
    cat("🚀 Entorno de PRODUCCIÓN detectado\n")
    cat("   Shiny se iniciará desde app.R\n\n")
} else {
    # Entorno de desarrollo local
    cat("Para ejecutar:\n")
    cat("  ejecutar_pro_v6(run_shiny = TRUE)   # Con interfaz GUI\n")
    cat("  ejecutar_pro_v6(run_shiny = FALSE)  # Solo backtesting\n\n")
    cat("═══════════════════════════════════════════════════════════\n\n")
}
