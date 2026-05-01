# ==================================================================================
# FUNCIONES UTILIT ARIAS - Sistema Cuantitativo ETF Pro v6.0
# ==================================================================================

# Sistema de logging
LOG_FILE <- file.path(CFG$outdir, sprintf("log_%s.txt", format(Sys.Date(), "%Y%m%d")))

log_msg <- function(..., level = "INFO") {
    msg <- sprintf("[%s][%s] %s", Sys.time(), level, paste(..., collapse = " "))
    if (CFG$verbose) message(msg)
    tryCatch(cat(msg, "\n", file = LOG_FILE, append = TRUE), error = function(e) NULL)
}

# Predicción segura para diferentes tipos de modelos
safe_predict_to_vector <- function(model_obj, newdata) {
    raw <- tryCatch(
        {
            if (inherits(model_obj, "ranger")) {
                predict(model_obj, data = newdata)$predictions
            } else {
                predict(model_obj, newdata = newdata)
            }
        },
        error = function(e) {
            log_msg("Predict error:", e$message, level = "WARN")
            return(NULL)
        }
    )

    if (is.null(raw)) {
        return(NULL)
    }
    if (is.list(raw) && !is.null(raw$predictions)) {
        return(as.numeric(raw$predictions))
    }
    if (is.numeric(raw)) {
        return(as.numeric(raw))
    }
    if (is.data.frame(raw) && ncol(raw) == 1) {
        return(as.numeric(raw[[1]]))
    }
    return(as.numeric(raw))
}

# Cálculo de métricas de performance
calcular_metricas <- function(returns_xts, capital_inicial = 100000) {
    if (is.null(returns_xts) || length(returns_xts) == 0) {
        return(NULL)
    }

    tryCatch(
        {
            list(
                retorno_anualizado = Return.annualized(returns_xts, scale = 252),
                volatilidad_anualizada = StdDev.annualized(returns_xts, scale = 252),
                sharpe = SharpeRatio.annualized(returns_xts, Rf = CFG$tasa_libre_riesgo, scale = 252),
                sortino = SortinoRatio(returns_xts, MAR = 0),
                max_drawdown = maxDrawdown(returns_xts),
                calmar = Return.annualized(returns_xts, scale = 252) / abs(maxDrawdown(returns_xts)),
                retorno_acumulado = Return.cumulative(returns_xts),
                num_trades = length(returns_xts[returns_xts != 0]),
                win_rate = sum(returns_xts > 0) / sum(returns_xts != 0)
            )
        },
        error = function(e) {
            log_msg("Error calculando métricas:", e$message, level = "ERROR")
            return(NULL)
        }
    )
}
