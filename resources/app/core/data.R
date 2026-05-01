# ==================================================================================
# DESCARGA Y PROCESAMIENTO DE DATOS - Sistema Cuantitativo ETF Pro v6.0
# ==================================================================================

# Helper para estandarizar precios y calcular retornos
procesar_precios <- function(datos_raw, source_name) {
    tryCatch(
        {
            # Estandarizar nombres de columnas
            col_precio <- "adjusted"
            if (source_name == "tiingo") col_precio <- "adjClose"
            if (source_name == "alphavantage") col_precio <- "adjusted_close"

            nombres <- colnames(datos_raw)
            if (!col_precio %in% nombres) {
                # Fallback a columnas comunes
                if ("close" %in% nombres) {
                    col_precio <- "close"
                } else if ("Close" %in% nombres) {
                    col_precio <- "Close"
                } else {
                    stop(paste("Columna de precio no encontrada en", source_name))
                }
            }

            # Calcular retornos logarítmicos
            r <- datos_raw %>%
                tq_transmute(
                    select = all_of(col_precio), mutate_fun = periodReturn,
                    period = "daily", type = "log", col_rename = "retornos"
                )

            return(xts(r$retornos, order.by = r$date))
        },
        error = function(e) {
            log_msg(paste("Error procesando datos de", source_name, ":", e$message), level = "ERROR")
            return(NULL)
        }
    )
}

# --- Motores de Descarga ---

download_yahoo <- function(symbol, from) {
    tryCatch(
        {
            # Pausa aleatoria pequeña para evitar detección de bot
            Sys.sleep(runif(1, 0.5, 1.5))
            tmp <- suppressWarnings(tq_get(symbol, from = from, to = Sys.Date(), get = "stock.prices"))
            if (!is.null(tmp) && nrow(tmp) >= 60) {
                return(procesar_precios(tmp, "yahoo"))
            }
        },
        error = function(e) {
            return(NULL)
        }
    )
    return(NULL)
}

download_tiingo <- function(symbol, from) {
    key <- Sys.getenv("TIINGO_API_KEY")
    if (key == "") {
        return(NULL)
    }

    tryCatch(
        {
            tmp <- suppressWarnings(tq_get(symbol, from = from, to = Sys.Date(), get = "stock.prices", source = "tiingo", api_key = key))
            if (!is.null(tmp) && nrow(tmp) >= 60) {
                return(procesar_precios(tmp, "tiingo"))
            }
        },
        error = function(e) {
            return(NULL)
        }
    )
    return(NULL)
}

download_alphavantage <- function(symbol, from) {
    key <- Sys.getenv("ALPHAVANTAGE_API_KEY")
    if (key == "") {
        return(NULL)
    }

    tryCatch(
        {
            tmp <- suppressWarnings(tq_get(symbol, from = from, to = Sys.Date(), get = "stock.prices", source = "alphavantage", api_key = key))
            if (!is.null(tmp) && nrow(tmp) >= 60) {
                return(procesar_precios(tmp, "alphavantage"))
            }
        },
        error = function(e) {
            return(NULL)
        }
    )
    return(NULL)
}

# --- Función Principal ---

descargar_retornos <- function(symbols, from = CFG$fecha_inicio, verbose = TRUE) {
    if (verbose) log_msg("Iniciando descarga de datos Multi-Fuente...", level = "INFO")

    datos <- list()
    pb <- txtProgressBar(min = 0, max = length(symbols), style = 3)

    for (i in seq_along(symbols)) {
        s <- symbols[i]
        ok <- FALSE
        result <- NULL

        # 1. Yahoo Finance (Default Local)
        if (!ok) {
            result <- download_yahoo(s, from)
            if (!is.null(result)) {
                ok <- TRUE
                if (verbose) log_msg(sprintf("✓ %s descargado vía Yahoo", s))
            }
        }

        # 2. Tiingo (Backup Cloud)
        if (!ok && Sys.getenv("TIINGO_API_KEY") != "") {
            log_msg(sprintf("ℹ Probando Tiingo para %s...", s))
            result <- download_tiingo(s, from)
            if (!is.null(result)) {
                ok <- TRUE
                log_msg(sprintf("✓ %s descargado vía Tiingo", s))
            }
        }

        # 3. Alpha Vantage (Backup alternativo)
        if (!ok && Sys.getenv("ALPHAVANTAGE_API_KEY") != "") {
            log_msg(sprintf("ℹ Probando Alpha Vantage para %s...", s))
            result <- download_alphavantage(s, from)
            if (!is.null(result)) {
                ok <- TRUE
                log_msg(sprintf("✓ %s descargado vía AlphaVantage", s))
            }
        }

        # Guardar resultado
        if (ok) {
            datos[[s]] <- result
        } else {
            log_msg(sprintf("✗ %s falló en todas las fuentes online", s), level = "ERROR")
        }

        setTxtProgressBar(pb, i)
    }
    close(pb)

    datos_validos <- datos[!sapply(datos, is.null)]
    num_validos <- length(datos_validos)

    # --- FALLBACK OFFLINE (ÚLTIMO RECURSO) ---
    datos_validos <- datos[!sapply(datos, is.null)]
    num_validos <- length(datos_validos)

    if (num_validos < length(symbols)) {
        log_msg(sprintf("⚠ Faltan datos (%d/%d). Activando protocolo de emergencia offline...", num_validos, length(symbols)), level = "WARN")

        posibles_rutas <- c(
            "data/datos_historicos_fallback.rds",
            "datos_historicos_fallback.rds",
            "../data/datos_historicos_fallback.rds",
            "/app/data/datos_historicos_fallback.rds"
        )

        fallback_data <- NULL
        for (ruta in posibles_rutas) {
            if (file.exists(ruta)) {
                try(
                    {
                        fallback_data <- readRDS(ruta)
                        log_msg(sprintf("DEBUG: Respaldo encontrado en: %s (%d filas)", ruta, nrow(fallback_data)))
                        break
                    },
                    silent = TRUE
                )
            }
        }

        if (!is.null(fallback_data)) {
            try(
                {
                    fallback_data <- fallback_data[index(fallback_data) >= from]
                },
                silent = TRUE
            )

            # Recuperar activos faltantes
            faltantes <- setdiff(symbols, names(datos_validos))
            log_msg(sprintf("✓ MODO HÍBRIDO: Recuperando %d activos (%s) desde respaldo local", length(faltantes), paste(faltantes, collapse = ", ")), level = "WARN")

            for (f in faltantes) {
                col_idx <- which(colnames(fallback_data) == f)
                if (length(col_idx) > 0) {
                    # Convertir a xts explícitamente si es necesario
                    datos[[f]] <- fallback_data[, col_idx]
                    log_msg(sprintf("  + %s recuperado de backup local", f))
                } else {
                    log_msg(sprintf("  MISSING: %s no está en el backup local", f), level = "ERROR")
                }
            }
            # Recalcular válidos con los recuperados
            datos_validos <- datos[!sapply(datos, is.null)]
        } else {
            log_msg("ERROR CRÍTICO: No se encontró archivo de respaldo .rds. Verifique rutas.", level = "ERROR")
            log_msg(sprintf("Directorio actual: %s", getwd()))
            log_msg(sprintf("Archivos en data/: %s", paste(list.files("data"), collapse = ", ")))
        }
    }

    if (length(datos_validos) == 0) stop("FATAL: No hay datos disponibles (Online ni Offline).")

    retornos_xts <- do.call(merge, datos_validos)
    colnames(retornos_xts) <- names(datos_validos)
    retornos_xts <- na.omit(retornos_xts)

    if (verbose) log_msg("✓ Dataset final:", nrow(retornos_xts), "filas,", ncol(retornos_xts), "activos")
    return(retornos_xts)
}

# Constructor de características (sin cambios)
crear_features <- function(ret_xts, lookback = 20) {
    n <- nrow(ret_xts)
    df <- data.frame(
        ret = as.numeric(ret_xts),
        ret_lag1 = c(NA, as.numeric(ret_xts[-n])),
        ret_lag2 = c(NA, NA, as.numeric(ret_xts[-c((n - 1):n)])),
        vol_20 = rollapply(ret_xts, width = lookback, FUN = sd, fill = NA, align = "right"),
        mean_20 = rollapply(ret_xts, width = lookback, FUN = mean, fill = NA, align = "right"),
        max_20 = rollapply(ret_xts, width = lookback, FUN = max, fill = NA, align = "right"),
        min_20 = rollapply(ret_xts, width = lookback, FUN = min, fill = NA, align = "right")
    )

    df$target <- ifelse(df$ret > CFG$pred_threshold, 1, 0)
    df <- df %>% drop_na()

    return(df)
}
