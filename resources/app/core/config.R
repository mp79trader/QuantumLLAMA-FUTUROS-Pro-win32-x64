# ==================================================================================
# CONFIGURACIÓN GLOBAL - Sistema Cuantitativo ETF Pro v6.0
# ==================================================================================

# Configuración principal del sistema
CFG <- list(
    # Activos a operar
    symbols = c("VTI", "VIG", "VNQ", "BND", "GLD", "SPY", "IEF", "VCIT", "XLK", "TLT"),

    # Parámetros temporales
    fecha_inicio = as.Date("2019-01-01"),

    # Capital y costos
    capital_inicial = 100000,
    costo_transaccion = 0.001,
    slippage = 0.0005,
    market_impact_factor = 0.0002,

    # Ventanas de entrenamiento y testing
    ventana_train = 252 * 1.5, # 1.5 años
    ventana_test = 63, # 3 meses
    min_obs_ml = 120,

    # Parámetros de modelos ML
    n_arboles = 100,
    min_node_size = 10,

    # Gestión de riesgo
    umbral_volatilidad = 0.30,
    max_position_pct = 0.25,
    stop_loss_pct = 0.02,
    take_profit_pct = 0.04,
    trailing_stop_pct = 0.03,

    # Umbrales de predicción
    pred_threshold = 0.005,
    confidence_threshold = 0.6,

    # Métricas de riesgo
    tasa_libre_riesgo = 0.02,
    max_dd_tolerance = 0.20,
    min_sharpe = 0.5,

    # Configuración de sistema
    outdir = "Resultados_PRO_v6",
    verbose = TRUE,
    use_parallel = FALSE,
    n_cores = max(1, parallel::detectCores() - 1)
)

# Crear directorio de salida
dir.create(CFG$outdir, showWarnings = FALSE, recursive = TRUE)
