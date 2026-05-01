# ==================================================================================
# MT5 BRIDGE - Conexión R <-> MetaTrader 5
# ==================================================================================

# Cargar reticulate para interactuar con Python
if (!require("reticulate")) install.packages("reticulate")
library(reticulate)

# Variable global para el módulo MT5
mt5 <- NULL

# Inicializar conexión con MT5
init_mt5 <- function() {
    tryCatch(
        {
            # Log info de Python para diagnóstico
            try(
                {
                    py_info <- py_config()
                    log_msg(sprintf("Python en uso: %s", py_info$python))
                },
                silent = TRUE
            )

            # Intentar cargar la librería MetaTrader5 de Python
            mt5 <<- import("MetaTrader5")

            # Inicializar conexión
            if (!mt5$initialize()) {
                log_msg("Error inicializando MT5:", mt5$last_error(), level = "ERROR")
                return(FALSE)
            }

            info <- mt5$terminal_info()
            log_msg(sprintf("✓ Conectado a MT5: %s (%s)", info$name, info$company))
            return(TRUE)
        },
        error = function(e) {
            log_msg("Error cargando librería MetaTrader5 (Python):", e$message, level = "ERROR")

            # Intento de auto-reparación
            log_msg("Intentando instalar MetaTrader5 automáticamente...", level = "WARN")
            tryCatch(
                {
                    py_install("MetaTrader5", pip = TRUE)
                    mt5 <<- import("MetaTrader5")

                    if (!mt5$initialize()) {
                        log_msg("Error inicializando MT5 post-instalación:", mt5$last_error(), level = "ERROR")
                        return(FALSE)
                    }
                    log_msg("✓ MetaTrader5 instalado y cargado correctamente.")
                    return(TRUE)
                },
                error = function(e2) {
                    log_msg("Fallo auto-instalación:", e2$message, level = "ERROR")
                    log_msg("Asegúrese de tener Python instalado y ejecutar en terminal: pip install MetaTrader5", level = "WARN")
                    return(FALSE)
                }
            )
        }
    )
}

# Obtener precio actual de un símbolo
get_current_price <- function(symbol) {
    if (is.null(mt5)) {
        return(NULL)
    }

    tick <- mt5$symbol_info_tick(symbol)
    if (is.null(tick)) {
        # Intentar habilitar el símbolo si no está visible
        if (mt5$symbol_select(symbol, TRUE)) {
            tick <- mt5$symbol_info_tick(symbol)
        }
    }

    if (is.null(tick)) {
        log_msg(sprintf("No se pudo obtener precio para %s", symbol), level = "WARN")
        return(NULL)
    }

    return(list(bid = tick$bid, ask = tick$ask))
}

# Obtener datos históricos desde MT5
get_historical_data <- function(symbol, n_days = 500, symbol_map = NULL) {
    # log_msg(sprintf("DEBUG: Iniciando descarga para %s", symbol))

    if (is.null(mt5)) {
        log_msg("DEBUG: mt5 es NULL, intentando inicializar...")
        if (!init_mt5()) {
            log_msg("Error: No se pudo inicializar MT5 para descarga de datos", level = "ERROR")
            return(NULL)
        }
    }

    # Traducir símbolo si hay mapa
    mt5_symbol <- symbol
    if (!is.null(symbol_map)) {
        match_idx <- which(symbol_map$SystemSymbol == symbol)
        if (length(match_idx) > 0) {
            mt5_symbol <- symbol_map$MT5Symbol[match_idx[1]]
            # log_msg(sprintf("Traduciendo %s -> %s", symbol, mt5_symbol))
        }
    }

    # Verificar símbolo con manejo de errores
    sym_info <- tryCatch(
        {
            mt5$symbol_info(mt5_symbol)
        },
        error = function(e) {
            log_msg(sprintf("Error en symbol_info(%s): %s", mt5_symbol, e$message), level = "ERROR")
            return(NULL)
        }
    )

    if (is.null(sym_info)) {
        log_msg(sprintf("Símbolo %s no encontrado en MT5 (symbol_info es NULL)", mt5_symbol), level = "WARN")
        # Intento de debug: ver error de MT5
        err <- mt5$last_error()
        log_msg(sprintf("MT5 Last Error: %s", paste(err, collapse = ", ")), level = "WARN")
        return(NULL)
    }

    # Obtener tasas (D1 timeframe)
    rates <- tryCatch(
        {
            # Forzar tipos explícitos para evitar SystemError en Python
            mt5$copy_rates_from_pos(
                as.character(mt5_symbol),
                as.integer(mt5$TIMEFRAME_D1),
                as.integer(0),
                as.integer(n_days)
            )
        },
        error = function(e) {
            log_msg(sprintf("Error en copy_rates(%s): %s", mt5_symbol, e$message), level = "ERROR")
            return(NULL)
        }
    )

    if (is.null(rates) || length(rates) == 0) {
        log_msg(sprintf("No se pudieron obtener datos históricos para %s (rates vacío)", symbol), level = "WARN")
        err <- mt5$last_error()
        log_msg(sprintf("MT5 Last Error: %s", paste(err, collapse = ", ")), level = "WARN")
        return(NULL)
    }

    # Convertir a data frame
    df <- tryCatch(
        {
            # Intentar importar pandas si no está cargado
            if (!exists("pd") || is.null(pd)) {
                tryCatch(
                    {
                        pd <<- import("pandas")
                    },
                    error = function(e) {
                        log_msg("Instalando pandas para conversión de datos...", level = "WARN")
                        py_install("pandas", pip = TRUE)
                        pd <<- import("pandas")
                    }
                )
            }

            # Convertir numpy array estructurado a DataFrame de Pandas
            # Reticulate convierte automáticamente Pandas DF a R DF
            pd_df <- pd$DataFrame(rates)

            # Asegurar conversión a R data.frame
            r_df <- as.data.frame(pd_df)
            r_df
        },
        error = function(e) {
            log_msg(sprintf("Error convirtiendo rates a DF para %s: %s", symbol, e$message), level = "ERROR")
            # Fallback: intentar conversión manual si pandas falla
            tryCatch(
                {
                    log_msg("Intentando conversión manual de numpy array...", level = "WARN")
                    list(
                        time = as.numeric(rates$time),
                        open = as.numeric(rates$open),
                        high = as.numeric(rates$high),
                        low = as.numeric(rates$low),
                        close = as.numeric(rates$close),
                        tick_volume = as.numeric(rates$tick_volume)
                    ) |> as.data.frame()
                },
                error = function(e2) {
                    log_msg(sprintf("Fallo conversión manual: %s", e2$message), level = "ERROR")
                    NULL
                }
            )
        }
    )

    if (is.null(df)) {
        return(NULL)
    }

    # Convertir timestamp a Date
    df$date <- as.Date(as.POSIXct(df$time, origin = "1970-01-01", tz = "UTC"))

    # Seleccionar y renombrar columnas para compatibilidad con tidyquant
    # MT5 devuelve: time, open, high, low, close, tick_volume, spread, real_volume
    # Necesitamos: date, open, high, low, close, volume, adjusted (usaremos close como adjusted)

    df_clean <- df[, c("date", "open", "high", "low", "close", "tick_volume")]
    colnames(df_clean) <- c("date", "open", "high", "low", "close", "volume")
    df_clean$adjusted <- df_clean$close

    return(df_clean)
}

# Ejecutar orden en MT5
place_trade_mt5 <- function(symbol, action, volume, sl_pct = 0.02, tp_pct = 0.04, comment = "QuantumLLAMA AI") {
    if (is.null(mt5)) {
        return(FALSE)
    }

    # Verificar símbolo
    symbol_info <- mt5$symbol_info(symbol)
    if (is.null(symbol_info)) {
        log_msg(sprintf("Símbolo %s no encontrado en MT5", symbol), level = "ERROR")
        return(FALSE)
    }

    if (!symbol_info$visible) {
        if (!mt5$symbol_select(symbol, TRUE)) {
            log_msg(sprintf("No se pudo seleccionar %s", symbol), level = "ERROR")
            return(FALSE)
        }
    }

    price_info <- get_current_price(symbol)
    if (is.null(price_info)) {
        return(FALSE)
    }

    # Definir tipo de orden y precios
    order_type <- if (action == "BUY") mt5$ORDER_TYPE_BUY else mt5$ORDER_TYPE_SELL
    price <- if (action == "BUY") price_info$ask else price_info$bid

    # Calcular SL y TP
    sl <- 0
    tp <- 0

    if (action == "BUY") {
        sl <- price * (1 - sl_pct)
        tp <- price * (1 + tp_pct)
    } else {
        sl <- price * (1 + sl_pct)
        tp <- price * (1 - tp_pct)
    }

    # Preparar request
    request <- list(
        "action" = mt5$TRADE_ACTION_DEAL,
        "symbol" = symbol,
        "volume" = as.numeric(volume),
        "type" = order_type,
        "price" = price,
        "sl" = sl,
        "tp" = tp,
        "deviation" = 20,
        "magic" = 234000,
        "comment" = comment,
        "type_time" = mt5$ORDER_TIME_GTC,
        "type_filling" = mt5$ORDER_FILLING_IOC
    )

    # Enviar orden
    result <- mt5$order_send(request)

    if (result$retcode != mt5$TRADE_RETCODE_DONE) {
        log_msg(sprintf("Error orden %s %s: %s", action, symbol, result$comment), level = "ERROR")
        return(FALSE)
    }

    log_msg(sprintf("✓ Orden ejecutada: %s %s Vol: %.2f @ %.5f", action, symbol, volume, price))
    return(TRUE)
}

# Función principal de ejecución automática
execute_auto_trading <- function(signals_xts, confidence_xts, capital, cfg, symbol_map = NULL) {
    log_msg("Iniciando ejecución automática en MT5...")

    if (!init_mt5()) {
        return(list(success = FALSE, message = "No se pudo conectar con MT5"))
    }

    # Obtener últimas señales (asumiendo que son para el día siguiente/actual)
    last_idx <- nrow(signals_xts)
    last_signals <- as.numeric(signals_xts[last_idx, ])
    last_conf <- as.numeric(confidence_xts[last_idx, ])
    assets <- colnames(signals_xts)

    trades_count <- 0
    errors_count <- 0

    # Calcular pesos (replicando lógica de simulate_portfolio simplificada para live)
    # Nota: En live, usamos pesos iguales o lógica simple si no tenemos covarianza futura
    # Aquí usaremos una asignación basada en confianza

    active_indices <- which(last_signals != 0 & last_conf >= cfg$confidence_threshold)

    if (length(active_indices) == 0) {
        log_msg("No hay señales activas con suficiente confianza para ejecutar.")
        return(list(success = TRUE, message = "Sin señales activas"))
    }

    # Calcular asignación de capital por activo
    # Usamos max_position_pct como límite
    capital_per_asset <- capital * cfg$max_position_pct

    # Ajustar por confianza (opcional, aquí simplificamos para asegurar ejecución)
    # Si hay muchos activos, normalizamos para no exceder capital total
    if (length(active_indices) * capital_per_asset > capital) {
        capital_per_asset <- capital / length(active_indices)
    }

    for (i in active_indices) {
        symbol <- assets[i]

        # Traducir símbolo si hay mapa
        mt5_symbol <- symbol
        if (!is.null(symbol_map)) {
            match_idx <- which(symbol_map$SystemSymbol == symbol)
            if (length(match_idx) > 0) {
                mt5_symbol <- symbol_map$MT5Symbol[match_idx[1]]
            }
        }

        signal <- last_signals[i] # 1 o -1
        conf <- last_conf[i]

        action <- if (signal == 1) "BUY" else "SELL"

        # Obtener precio actual para calcular volumen
        price_info <- get_current_price(mt5_symbol)

        if (!is.null(price_info)) {
            price <- if (action == "BUY") price_info$ask else price_info$bid

            # Calcular volumen (lotes)
            # Asumiendo 1 lote = 1 unidad para ETFs/Stocks en MT5 (verificar especificación del broker)
            # En Forex 1 lote = 100,000 unidades. En Stocks suele ser 1 o 100.
            # Consultaremos symbol_info para contract_size
            sym_info <- mt5$symbol_info(mt5_symbol)
            contract_size <- sym_info$trade_contract_size

            if (is.null(contract_size) || contract_size == 0) contract_size <- 1

            # Volumen = (Capital Asignado) / (Precio * Contract Size)
            raw_volume <- capital_per_asset / (price * contract_size)

            # Ajustar a paso de volumen (lot step)
            vol_step <- sym_info$volume_step
            volume <- floor(raw_volume / vol_step) * vol_step

            # Verificar volumen mínimo
            min_vol <- sym_info$volume_min
            if (volume < min_vol) {
                log_msg(sprintf("Volumen calculado (%.2f) menor al mínimo (%.2f) para %s", raw_volume, min_vol, mt5_symbol), level = "WARN")
                next
            }

            # Ejecutar trade
            if (place_trade_mt5(mt5_symbol, action, volume, cfg$stop_loss_pct, cfg$take_profit_pct)) {
                trades_count <- trades_count + 1
            } else {
                errors_count <- errors_count + 1
            }
        } else {
            errors_count <- errors_count + 1
        }
    }

    mt5$shutdown()

    msg <- sprintf("Ejecución finalizada. Trades: %d, Errores: %d", trades_count, errors_count)
    log_msg(msg)

    return(list(success = TRUE, message = msg))
}
