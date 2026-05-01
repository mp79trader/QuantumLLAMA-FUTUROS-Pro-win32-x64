# ==================================================================================
# SISTEMA CUANTITATIVO ETFs - v6.0 PRO FULL - Quantum LLAMA IA
# Autor: Pablo Ez. Moscardo (Punto&Coma Trading - NBM-Systemas)
# Fecha: Enero 2026
# ==================================================================================

suppressWarnings(suppressMessages({
  if (!require("pacman")) install.packages("pacman", repos = "https://cloud.r-project.org")
  pacman::p_load(
    quantmod, tidyquant, PerformanceAnalytics, tidyverse, xts, lubridate,
    caret, ranger, quadprog, tseries, TTR, zoo, gridExtra,
    shiny, shinydashboard, DT, openxlsx, doParallel, foreach,
    reticulate, shinyWidgets
  )
  # Asegurar que shinydashboard esté cargado
  library(shinydashboard)
}))

# Cargar módulo MT5
# Cargar módulo MT5 de forma segura
MT5_AVAILABLE <- FALSE
tryCatch(
  {
    source("core/mt5_bridge.R", encoding = "UTF-8")
    MT5_AVAILABLE <- TRUE
    message("✓ Módulo MT5 cargado correctamente")
  },
  error = function(e) {
    message("ADVERTENCIA CRÍTICA: No se pudo cargar el módulo MT5.")
    message("Detalle del error: ", e$message)
    # Intentar escribir en un log de emergencia
    try(cat(sprintf("[%s] Error cargando MT5: %s\n", Sys.time(), e$message), file = "mt5_error.log", append = TRUE))
  }
)

options(warn = -1, scipen = 999, digits = 6)
Sys.setenv(TZ = "UTC")
set.seed(2025)

# ==================== CONFIGURACIÓN ===============================================
CFG <- list(
  symbols = c("VTI", "VIG", "VNQ", "BND", "GLD", "SPY", "IEF", "VCIT", "XLK", "TLT"),
  fecha_inicio = as.Date("2019-01-01"),
  capital_inicial = 100000,
  costo_transaccion = 0.001,
  slippage = 0.0005,
  market_impact_factor = 0.0002,
  ventana_train = 252 * 1.5,
  ventana_test = 63,
  min_obs_ml = 120,
  n_arboles = 100,
  min_node_size = 10,
  umbral_volatilidad = 0.30,
  max_position_pct = 0.25,
  stop_loss_pct = 0.02,
  take_profit_pct = 0.04,
  trailing_stop_pct = 0.03,
  pred_threshold = 0.005,
  confidence_threshold = 0.6,
  outdir = "Resultados_PRO_v6",
  verbose = TRUE,
  tasa_libre_riesgo = 0.02,
  max_dd_tolerance = 0.20,
  min_sharpe = 0.5,
  use_parallel = FALSE,
  n_cores = max(1, parallel::detectCores() - 1)
)

dir.create(CFG$outdir, showWarnings = FALSE, recursive = TRUE)

# ==================== LOGGING =====================================================
LOG_FILE <- file.path(CFG$outdir, sprintf("log_%s.txt", format(Sys.Date(), "%Y%m%d")))

log_msg <- function(..., level = "INFO") {
  msg <- sprintf("[%s][%s] %s", Sys.time(), level, paste(..., collapse = " "))
  if (CFG$verbose) message(msg)
  tryCatch(cat(msg, "\n", file = LOG_FILE, append = TRUE), error = function(e) NULL)
}

# ==================== UTILIDADES ==================================================
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

save_rds_safe <- function(obj, path) {
  tryCatch(
    {
      saveRDS(obj, file = path)
      log_msg("Guardado:", basename(path))
      TRUE
    },
    error = function(e) {
      log_msg("Error guardando", path, level = "ERROR")
      FALSE
    }
  )
}

save_csv_safe <- function(df, path) {
  tryCatch(
    {
      write.csv(df, path, row.names = FALSE)
      log_msg("CSV guardado:", basename(path))
      TRUE
    },
    error = function(e) {
      log_msg("Error CSV:", e$message, level = "ERROR")
      FALSE
    }
  )
}

calc_robust_stats <- function(returns_xts) {
  if (is.null(returns_xts) || nrow(returns_xts) < 10) {
    return(list(mean = NA, sd = NA, sharpe = NA, maxdd = NA))
  }
  tryCatch(
    {
      list(
        mean_ret = mean(returns_xts, na.rm = TRUE),
        annual_ret = Return.annualized(returns_xts),
        volatility = StdDev.annualized(returns_xts),
        sharpe = SharpeRatio.annualized(returns_xts, Rf = CFG$tasa_libre_riesgo),
        sortino = SortinoRatio(returns_xts),
        maxdd = maxDrawdown(returns_xts),
        calmar = Return.annualized(returns_xts) / (maxDrawdown(returns_xts) + 1e-10),
        win_rate = sum(returns_xts > 0, na.rm = TRUE) / sum(!is.na(returns_xts))
      )
    },
    error = function(e) list(mean = NA, sd = NA, sharpe = NA, maxdd = NA)
  )
}

# ==================== DESCARGA DE DATOS ===========================================
descargar_retornos <- function(symbols, from = CFG$fecha_inicio, verbose = CFG$verbose, source = "yahoo", symbol_map = NULL) {
  log_msg(sprintf("Descargando %d símbolos desde %s...", length(symbols), source))
  datos <- list()
  pb <- txtProgressBar(min = 0, max = length(symbols), style = 3)

  for (i in seq_along(symbols)) {
    s <- symbols[i]
    tries <- 0
    ok <- FALSE

    while (tries < 4 && !ok) {
      tries <- tries + 1
      try(
        {
          if (source == "mt5") {
            # Descarga desde MT5
            if (exists("get_historical_data")) {
              # Calcular días aproximados desde 'from'
              n_days <- as.numeric(Sys.Date() - as.Date(from)) + 100
              tmp_df <- get_historical_data(s, n_days, symbol_map = symbol_map)

              if (!is.null(tmp_df) && nrow(tmp_df) >= 60) {
                # Filtrar por fecha
                tmp_df <- tmp_df[tmp_df$date >= as.Date(from), ]

                # Calcular retornos logarítmicos
                r <- tmp_df %>% tq_transmute(
                  select = adjusted, mutate_fun = periodReturn,
                  period = "daily", type = "log", col_rename = "retornos"
                )
                datos[[s]] <- xts(r$retornos, order.by = r$date)
                ok <- TRUE
                log_msg(sprintf("✓ %s (MT5): %d obs", s, nrow(r)))
              } else {
                stop("Datos insuficientes o nulos desde MT5")
              }
            } else {
              stop("Función get_historical_data no disponible (módulo MT5 no cargado)")
            }
          } else {
            # Descarga desde Yahoo Finance (Default)
            tmp <- tq_get(s, from = from, to = Sys.Date(), get = "stock.prices")
            if (is.null(tmp) || nrow(tmp) < 60) stop("Datos insuficientes")
            r <- tmp %>% tq_transmute(
              select = adjusted, mutate_fun = periodReturn,
              period = "daily", type = "log", col_rename = "retornos"
            )
            datos[[s]] <- xts(r$retornos, order.by = r$date)
            ok <- TRUE
            log_msg(sprintf("✓ %s (Yahoo): %d obs", s, nrow(r)))
          }
        },
        silent = TRUE
      )
      if (!ok) Sys.sleep(1)
    }
    if (!ok) log_msg(sprintf("✗ %s no descargado", s), level = "ERROR")
    setTxtProgressBar(pb, i)
  }
  close(pb)

  datos_validos <- datos[!sapply(datos, is.null)]
  if (length(datos_validos) == 0) stop("No se descargaron datos válidos")
  if (length(datos_validos) < 0.5 * length(symbols)) {
    log_msg("ADVERTENCIA: Solo", length(datos_validos), "de", length(symbols), level = "WARN")
  }

  merged <- tryCatch(na.omit(do.call(merge, datos_validos)),
    error = function(e) stop("Error merge: ", e$message)
  )
  if (nrow(merged) < 100) stop("Datos insuficientes: ", nrow(merged), " filas")

  colnames(merged) <- names(datos_validos)
  log_msg("Datos combinados:", nrow(merged), "filas,", ncol(merged), "activos")
  return(merged)
}

# ==================== FEATURES ====================================================
crear_features_single <- function(ret_vec, dates) {
  n <- length(ret_vec)
  price_cum <- cumprod(1 + ret_vec)

  df <- tibble(
    date = dates, ret = ret_vec,
    lag1 = dplyr::lag(ret_vec, 1), lag2 = dplyr::lag(ret_vec, 2),
    lag3 = dplyr::lag(ret_vec, 3), lag5 = dplyr::lag(ret_vec, 5),
    ma5_p = TTR::SMA(price_cum, n = 5), ma10_p = TTR::SMA(price_cum, n = 10),
    ma20_p = TTR::SMA(price_cum, n = 20), ma50_p = TTR::SMA(price_cum, n = 50),
    rsi = TTR::RSI(price_cum, n = 14),
    rsi_ma = TTR::SMA(TTR::RSI(price_cum, n = 14), n = 5),
    vol10 = TTR::runSD(ret_vec, n = 10), vol20 = TTR::runSD(ret_vec, n = 20),
    vol_ratio = TTR::runSD(ret_vec, n = 10) / (TTR::runSD(ret_vec, n = 20) + 1e-6),
    mom5 = zoo::rollapply(ret_vec, 5, sum, fill = NA, align = "right"),
    mom10 = zoo::rollapply(ret_vec, 10, sum, fill = NA, align = "right"),
    mom21 = zoo::rollapply(ret_vec, 21, sum, fill = NA, align = "right"),
    trend_5 = (price_cum - dplyr::lag(price_cum, 5)) / (dplyr::lag(price_cum, 5) + 1e-6),
    trend_20 = (price_cum - dplyr::lag(price_cum, 20)) / (dplyr::lag(price_cum, 20) + 1e-6),
    target = dplyr::lead(ret_vec, 1)
  ) %>% drop_na()

  return(df)
}

crear_features_matrix <- function(retornos_xts) {
  dates <- index(retornos_xts)
  features <- list()
  log_msg("Creando features para", ncol(retornos_xts), "activos...")
  for (j in seq_len(ncol(retornos_xts))) {
    vec <- as.numeric(retornos_xts[, j])
    features[[colnames(retornos_xts)[j]]] <- crear_features_single(vec, dates)
  }
  log_msg("Features creadas")
  return(features)
}

# ==================== WALK-FORWARD ================================================
entrenar_walkforward <- function(retornos_xts, cfg = CFG, verbose = CFG$verbose) {
  fechas <- index(retornos_xts)
  n <- nrow(retornos_xts)
  assets <- colnames(retornos_xts)
  log_msg("Walk-Forward: Train =", cfg$ventana_train, "| Test =", cfg$ventana_test)

  feats <- crear_features_matrix(retornos_xts)
  senales_mat <- matrix(NA_real_, nrow = n, ncol = length(assets))
  colnames(senales_mat) <- assets
  confidence_mat <- matrix(NA_real_, nrow = n, ncol = length(assets))
  colnames(confidence_mat) <- assets
  modelos_list <- list()

  if (cfg$use_parallel) {
    cl <- makeCluster(cfg$n_cores)
    registerDoParallel(cl)
    log_msg("Paralelización:", cfg$n_cores, "cores")
  }

  for (a in seq_along(assets)) {
    activo <- assets[a]
    log_msg(sprintf("Entrenando %s (%d/%d)", activo, a, length(assets)))
    df_feat <- feats[[activo]]

    if (is.null(df_feat) || nrow(df_feat) < cfg$min_obs_ml) {
      log_msg(sprintf("%s: insuficiente", activo), level = "WARN")
      next
    }

    dates_avail <- df_feat$date
    i_start <- 1
    n_windows <- 0

    while (TRUE) {
      train_end_idx <- i_start + cfg$ventana_train - 1
      if (train_end_idx > length(dates_avail)) break

      test_start_idx <- train_end_idx + 1
      test_end_idx <- min(length(dates_avail), test_start_idx + cfg$ventana_test - 1)
      if (test_start_idx > length(dates_avail)) break

      train_df <- df_feat[i_start:train_end_idx, ] %>% filter(abs(target) < cfg$umbral_volatilidad)
      test_df <- df_feat[test_start_idx:test_end_idx, ]

      if (nrow(train_df) < cfg$min_obs_ml) {
        i_start <- i_start + cfg$ventana_test
        next
      }

      train_data <- train_df %>% select(-date)
      cols_to_keep <- sapply(train_data, function(x) {
        !any(is.na(x)) && !any(is.infinite(x)) && var(x, na.rm = TRUE) > 1e-10
      })

      if (sum(cols_to_keep) < 3) {
        log_msg(sprintf("%s: pocas features válidas en ventana", activo), level = "WARN")
        i_start <- i_start + cfg$ventana_test
        next
      }

      train_data <- train_data[, cols_to_keep, drop = FALSE]
      test_data <- test_df %>%
        select(-date) %>%
        select(all_of(names(train_data)[names(train_data) != "target"]))

      if (var(train_data$target, na.rm = TRUE) < 1e-10) {
        log_msg(sprintf("%s: target sin varianza", activo), level = "WARN")
        i_start <- i_start + cfg$ventana_test
        next
      }

      ctrl <- trainControl(
        method = "cv", number = 3, verboseIter = FALSE,
        allowParallel = FALSE, savePredictions = "final"
      )

      tune_grid <- expand.grid(
        mtry = floor(sqrt(ncol(train_data) - 1)),
        splitrule = "variance",
        min.node.size = cfg$min_node_size
      )

      model <- tryCatch(
        {
          train(target ~ .,
            data = train_data, method = "ranger",
            trControl = ctrl, tuneGrid = tune_grid, num.trees = 100,
            importance = "impurity", verbose = FALSE
          )
        },
        error = function(e) {
          log_msg(sprintf("%s: caret falló, usando ranger directo", activo), level = "WARN")
          tryCatch(
            {
              library(ranger)
              ranger(target ~ .,
                data = train_data, num.trees = 100,
                min.node.size = cfg$min_node_size, importance = "impurity",
                verbose = FALSE
              )
            },
            error = function(e2) {
              log_msg(sprintf("%s: ranger también falló - %s", activo, e2$message), level = "ERROR")
              NULL
            }
          )
        }
      )

      if (is.null(model)) {
        i_start <- i_start + cfg$ventana_test
        next
      }

      modelos_list[[activo]] <- model
      n_windows <- n_windows + 1
      predvec <- safe_predict_to_vector(model, test_data)

      if (!is.null(predvec) && length(predvec) > 0) {
        test_dates <- as.character(test_df$date)
        for (k in seq_along(test_dates)) {
          if (k > length(predvec)) break
          idx_global <- which(as.character(fechas) == test_dates[k])
          if (length(idx_global) == 1) {
            val <- predvec[k]
            conf <- abs(val) / (max(abs(predvec), na.rm = TRUE) + 1e-6)
            sgn <- if (conf > cfg$confidence_threshold) {
              if (val > cfg$pred_threshold) 1 else if (val < -cfg$pred_threshold) -1 else 0
            } else {
              0
            }
            senales_mat[idx_global, a] <- sgn
            confidence_mat[idx_global, a] <- conf
          }
        }
      }
      i_start <- i_start + cfg$ventana_test
    }
    if (n_windows > 0) log_msg(sprintf("  ✓ %s: %d ventanas", activo, n_windows))
  }

  if (cfg$use_parallel) {
    stopCluster(cl)
    registerDoSEQ()
  }

  senales_mat[is.na(senales_mat)] <- 0
  confidence_mat[is.na(confidence_mat)] <- 0
  senales_xts <- xts(senales_mat, order.by = fechas)
  colnames(senales_xts) <- assets
  confidence_xts <- xts(confidence_mat, order.by = fechas)
  colnames(confidence_xts) <- assets

  log_msg("Walk-Forward completado:", length(modelos_list), "modelos")
  log_msg("✓ Confidence generada:", nrow(confidence_xts), "filas x", ncol(confidence_xts), "activos")

  return(list(signals = senales_xts, confidence = confidence_xts, models = modelos_list))
}

# ==================== PORTFOLIO ALLOCATION ========================================
alloc_mean_variance <- function(expected_returns, covmat, cfg = CFG) {
  p <- length(expected_returns)
  if (p == 0) {
    return(numeric(0))
  }

  # Caso trivial: 1 solo activo
  if (p == 1) {
    return(min(1, cfg$max_position_pct))
  }

  if (any(is.nan(covmat)) || any(is.infinite(covmat)) || det(covmat) < 1e-10) {
    log_msg("Covmat inválida, regularizando", level = "WARN")
    covmat <- diag(diag(covmat) + 0.01)
  }

  # Ajustar target de exposición para evitar restricciones inconsistentes
  # Si tenemos pocos activos, no podemos invertir el 100% si cada uno está limitado
  target_exposure <- min(1, p * cfg$max_position_pct)

  Dmat <- 2 * covmat
  dvec <- expected_returns
  Amat <- cbind(rep(1, p), diag(p), -diag(p))

  # bvec[1] es la suma de pesos (target_exposure)
  # bvec[2:(p+1)] son min weights (0)
  # bvec[(p+2):(2p+1)] son -max weights
  bvec <- c(target_exposure, rep(0, p), rep(-cfg$max_position_pct, p))

  res <- tryCatch(solve.QP(Dmat, dvec, Amat, bvec, meq = 1),
    error = function(e) {
      log_msg("QP error:", e$message, level = "WARN")
      NULL
    }
  )

  if (is.null(res)) {
    # Fallback seguro: respetar max_position_pct
    safe_weight <- min(1 / p, cfg$max_position_pct)
    w <- rep(safe_weight, p)
  } else {
    w <- res$solution
    w[w < 1e-8] <- 0
    # No normalizamos a 1, respetamos el target_exposure optimizado
  }
  return(w)
}

# ==================== SIMULACIÓN ==================================================
simulate_portfolio <- function(retornos_xts, signals_xts, confidence_xts = NULL, cfg = CFG) {
  if (!all(index(retornos_xts) == index(signals_xts))) stop("Fechas no alineadas")

  n <- nrow(retornos_xts)
  assets <- colnames(retornos_xts)
  dates <- index(retornos_xts)
  log_msg("Simulando portafolio:", n, "períodos")

  if (!is.null(confidence_xts)) {
    log_msg("✓ Confidence recibida en simulate_portfolio:", nrow(confidence_xts), "x", ncol(confidence_xts))
  } else {
    log_msg("⚠️ Confidence NO recibida (NULL)", level = "WARN")
  }

  capital <- cfg$capital_inicial
  positions <- matrix(0, nrow = n, ncol = length(assets))
  colnames(positions) <- assets
  cash <- numeric(n)
  nav <- numeric(n)
  cash[1] <- capital
  nav[1] <- capital
  trans_cost_tot <- 0
  max_nav_reached <- capital
  n_trades <- 0

  for (t in 2:n) {
    sigs_prev <- as.numeric(signals_xts[t - 1, ])

    for (j in seq_along(assets)) {
      if (positions[t - 1, j] != 0) {
        ret_today <- as.numeric(retornos_xts[t, j])
        if (ret_today < -cfg$stop_loss_pct) {
          sigs_prev[j] <- 0
          log_msg(sprintf("Stop loss: %s (%.2f%%)", assets[j], ret_today * 100), level = "WARN")
        }
        if (ret_today > cfg$take_profit_pct) {
          sigs_prev[j] <- 0
          log_msg(sprintf("Take profit: %s (%.2f%%)", assets[j], ret_today * 100))
        }
      }
    }

    long_idx <- which(sigs_prev == 1)
    short_idx <- which(sigs_prev == -1)
    active_idx <- c(long_idx, short_idx)
    m <- length(active_idx)

    if (m == 0) {
      positions[t, ] <- 0
      cash[t] <- cash[t - 1]
      nav[t] <- cash[t]
      next
    }

    exp_ret <- sapply(active_idx, function(i) mean(as.numeric(retornos_xts[max(1, t - 21):(t - 1), i]), na.rm = TRUE))
    covmat <- cov(as.matrix(retornos_xts[max(1, t - 63):(t - 1), active_idx]), use = "pairwise.complete.obs")

    w_rel <- tryCatch(alloc_mean_variance(exp_ret, covmat, cfg),
      error = function(e) rep(1 / m, m)
    )

    if (!is.null(confidence_xts)) {
      conf_vec <- as.numeric(confidence_xts[t - 1, active_idx])
      w_rel <- w_rel * conf_vec
      w_rel <- w_rel / sum(w_rel)
    }

    alloc_amounts <- cash[t - 1] * w_rel
    pos_fracs <- rep(0, length(assets))
    pos_fracs[active_idx] <- alloc_amounts / cash[t - 1]
    trans_vol <- sum(abs(pos_fracs - positions[t - 1, ]))

    if (trans_vol > 0) {
      trans_cost <- trans_vol * cfg$costo_transaccion * cash[t - 1] +
        cfg$slippage * cash[t - 1] * trans_vol +
        cfg$market_impact_factor * (trans_vol^1.5) * cash[t - 1]
      n_trades <- n_trades + 1
    } else {
      trans_cost <- 0
    }

    positions[t, ] <- pos_fracs
    cash[t] <- cash[t - 1] - trans_cost
    trans_cost_tot <- trans_cost_tot + trans_cost

    day_ret_vec <- as.numeric(retornos_xts[t, ])
    holdings_value <- sum(positions[t, ] * cash[t] * (1 + day_ret_vec))
    nav[t] <- cash[t] + holdings_value

    max_nav_reached <- max(max_nav_reached, nav[t])
    current_dd <- (nav[t] - max_nav_reached) / max_nav_reached

    if (current_dd < -cfg$max_dd_tolerance) {
      log_msg(sprintf("CIRCUIT BREAKER: DD=%.2f%%", current_dd * 100), level = "ERROR")
      positions[t, ] <- 0
      cash[t] <- nav[t]
    }

    if (nav[t] <= 0) {
      log_msg("NAV = 0 en", as.character(dates[t]), level = "ERROR")
      break
    }
  }

  nav_xts <- xts(nav, order.by = dates)
  colnames(nav_xts) <- "NAV"
  ret_nav <- na.omit(diff(nav_xts) / lag(nav_xts))
  colnames(ret_nav) <- "ret"

  if (!is.null(confidence_xts)) {
    log_msg("✓ Confidence preparada para retorno:", nrow(confidence_xts), "x", ncol(confidence_xts))
  } else {
    log_msg("⚠️ Confidence es NULL antes de retornar", level = "WARN")
  }

  log_msg("Simulación OK | NAV final:", round(nav[n], 2), "| Trades:", n_trades)

  return(list(
    NAV = nav_xts,
    returns = ret_nav,
    positions = xts(positions, order.by = dates),
    confidence = confidence_xts,
    trans_cost = trans_cost_tot,
    n_trades = n_trades,
    final_nav = nav[n]
  ))
}

# ==================== REPORTES ====================================================
generar_reportes <- function(models_list, signals_xts, metrics, retornos_xts, outdir = CFG$outdir) {
  dir.create(outdir, showWarnings = FALSE, recursive = TRUE)
  log_msg("Generando reportes...")

  save_rds_safe(models_list, file.path(outdir, "models_all.rds"))
  save_rds_safe(signals_xts, file.path(outdir, "signals.rds"))
  save_rds_safe(metrics, file.path(outdir, "metrics.rds"))

  if (!is.null(metrics$confidence)) {
    save_rds_safe(metrics$confidence, file.path(outdir, "confidence.rds"))
    log_msg("✓ Datos de confianza guardados")
  } else {
    log_msg("⚠️ Sin datos de confianza en metrics", level = "WARN")
  }

  if (!is.null(metrics$returns) && nrow(metrics$returns) > 10) {
    stats <- calc_robust_stats(metrics$returns)

    perf_df <- data.frame(
      Metric = c(
        "Retorno Acumulado", "Retorno Anualizado", "Volatilidad Anual",
        "Sharpe Ratio", "Sortino Ratio", "Max Drawdown", "Calmar Ratio",
        "Win Rate", "Trades Totales", "Costos Totales"
      ),
      Value = c(
        as.numeric(Return.cumulative(metrics$returns)), as.numeric(stats$annual_ret),
        as.numeric(stats$volatility), as.numeric(stats$sharpe), as.numeric(stats$sortino),
        as.numeric(stats$maxdd), as.numeric(stats$calmar), as.numeric(stats$win_rate),
        metrics$n_trades, metrics$trans_cost
      )
    )
    save_csv_safe(perf_df, file.path(outdir, "performance_summary.csv"))

    tryCatch(
      {
        png(file.path(outdir, "nav_evolution.png"), width = 1200, height = 600)
        par(mfrow = c(2, 2), mar = c(4, 4, 3, 2))

        nav_vec <- as.numeric(metrics$NAV)
        nav_dates <- index(metrics$NAV)
        plot(nav_dates, nav_vec,
          type = "l", main = "Evolución del NAV",
          col = "blue", lwd = 2, ylab = "NAV ($)", xlab = "Fecha"
        )
        grid()

        ret_vec <- as.numeric(metrics$returns)
        cum_ret <- cumprod(1 + ret_vec)
        running_max <- cummax(cum_ret)
        dd <- (cum_ret - running_max) / running_max
        plot(index(metrics$returns), dd,
          type = "l", main = "Drawdown",
          col = "red", lwd = 2, ylab = "Drawdown", xlab = "Fecha"
        )
        abline(h = -CFG$max_dd_tolerance, col = "darkred", lty = 2, lwd = 1.5)
        text(index(metrics$returns)[length(dd) %/% 2], -CFG$max_dd_tolerance,
          sprintf("Circuit Breaker: %.0f%%", CFG$max_dd_tolerance * 100),
          pos = 3, col = "darkred", cex = 0.8
        )
        grid()

        plot(index(metrics$returns), cum_ret,
          type = "l",
          main = "Retornos Acumulados (Wealth Index)",
          col = "darkgreen", lwd = 2, ylab = "Wealth Index", xlab = "Fecha"
        )
        grid()

        hist(ret_vec,
          breaks = 50, main = "Distribución de Retornos Diarios",
          xlab = "Retorno", col = "lightblue", border = "white", freq = FALSE
        )
        lines(density(ret_vec, na.rm = TRUE), col = "darkblue", lwd = 2)
        abline(v = 0, col = "red", lty = 2)

        dev.off()
        log_msg("✓ Gráfico NAV guardado")
      },
      error = function(e) {
        log_msg("Error gráficos:", e$message, level = "ERROR")
        if (dev.cur() > 1) try(dev.off(), silent = TRUE)
      }
    )

    if (!is.null(retornos_xts) && "SPY" %in% colnames(retornos_xts)) {
      tryCatch(
        {
          spy_ret <- retornos_xts[, "SPY"]
          dates_common <- intersect(index(metrics$returns), index(spy_ret))

          if (length(dates_common) > 10) {
            strat_ret <- as.numeric(metrics$returns[dates_common])
            spy_ret_vec <- as.numeric(spy_ret[dates_common])

            cum_strat <- cumprod(1 + strat_ret)
            cum_spy <- cumprod(1 + spy_ret_vec)

            png(file.path(outdir, "benchmark_comparison.png"), width = 1000, height = 600)

            y_range <- range(c(cum_strat, cum_spy), na.rm = TRUE)
            plot(dates_common, cum_strat,
              type = "l", main = "Estrategia vs SPY (Benchmark)",
              col = "blue", lwd = 2.5, ylab = "Wealth Index ($1 inicial)", xlab = "Fecha",
              ylim = y_range
            )
            lines(dates_common, cum_spy, col = "red", lwd = 2.5)

            legend("topleft",
              legend = c(
                sprintf("Estrategia (%.1f%%)", (tail(cum_strat, 1) - 1) * 100),
                sprintf("SPY (%.1f%%)", (tail(cum_spy, 1) - 1) * 100)
              ),
              col = c("blue", "red"), lwd = 2.5, bg = "white", box.lwd = 1
            )
            grid()

            dev.off()
            log_msg("✓ Gráfico benchmark guardado")
          }
        },
        error = function(e) {
          log_msg("Error gráfico benchmark:", e$message, level = "WARN")
          if (dev.cur() > 1) try(dev.off(), silent = TRUE)
        }
      )
    }
  }

  wb <- createWorkbook()
  addWorksheet(wb, "Performance")
  if (exists("perf_df")) writeData(wb, "Performance", perf_df)
  addWorksheet(wb, "Señales")
  sig_df <- as.data.frame(tail(signals_xts, 100))
  sig_df$Date <- rownames(sig_df)
  writeData(wb, "Señales", sig_df)
  if (!is.null(metrics$positions)) {
    addWorksheet(wb, "Posiciones")
    pos_df <- as.data.frame(tail(metrics$positions, 100))
    pos_df$Date <- rownames(pos_df)
    writeData(wb, "Posiciones", pos_df)
  }
  saveWorkbook(wb, file.path(outdir, "report_completo.xlsx"), overwrite = TRUE)

  log_msg("✓ Reportes en:", outdir)
  return(TRUE)
}

# ==================== SHINY UI MEJORADA (LLAMA IA DESIGN) =========================
ui_shiny_v6 <- function() {
  dashboardPage(
    title = "Quantum Llama - ETF Trading System",
    skin = "black",
    dashboardHeader(
      title = tags$div(
        style = "display: flex; align-items: center; gap: 10px; font-weight: bold;",
        tags$img(
          src = "imagenes/llama_logo_footer_1768075300145.png",
          style = "width: 40px; height: 40px; border-radius: 50%; filter: drop-shadow(0 0 8px rgba(251, 189, 36, 0.5));"
        ),
        "ETF Trading System"
      ),
      titleWidth = 350
    ),
    dashboardSidebar(
      width = 300,
      tags$head(
        tags$title("Quantum Llama - ETF Trading System"),
        tags$meta(name = "description", content = "Sistema Cuantitativo ETF Pro v6.0 - Inteligencia Artificial para Trading"),
        tags$meta(charset = "UTF-8"),
        tags$style(HTML("
          /* Estilos globales */
          body {
            background: #0a0a0f !important;
            font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, 'Helvetica Neue', Arial, sans-serif !important;
          }

          /* Header con gradiente púrpura-dorado LLAMA IA */
          .skin-black .main-header .logo {
            background: linear-gradient(135deg, #667eea 0%, #764ba2 50%, #fbbd24 100%) !important;
            font-weight: 700 !important;
          }
          .skin-black .main-header .navbar {
            background: linear-gradient(135deg, #667eea 0%, #764ba2 50%, #fbbd24 100%) !important;
          }

          /* Sidebar con fondo oscuro profundo */
          .skin-black .main-sidebar {
            background: linear-gradient(180deg, #0a0a0f 0%, #1e1e24 100%) !important;
            box-shadow: 2px 0 10px rgba(0, 0, 0, 0.5) !important;
          }

          /* Items del menú con efectos dorados */
          .sidebar-menu > li.active > a,
          .sidebar-menu > li:hover > a {
            background: linear-gradient(135deg, #667eea 0%, #764ba2 50%, #fbbd24 100%) !important;
            border-left: 4px solid #fbbd24 !important;
            box-shadow: 0 4px 20px rgba(251, 189, 36, 0.4) !important;
            transform: translateX(5px) !important;
          }
          .sidebar-menu > li > a {
            color: #e0e0e0 !important;
            transition: all 0.4s cubic-bezier(0.4, 0, 0.2, 1) !important;
            border-left: 4px solid transparent !important;
            padding: 15px 20px !important;
          }

          /* Value boxes mejoradas */
          .small-box {
            border-radius: 20px !important;
            box-shadow: 0 10px 40px rgba(0, 0, 0, 0.4) !important;
            transition: all 0.4s cubic-bezier(0.4, 0, 0.2, 1) !important;
            overflow: hidden !important;
          }
          .small-box:hover {
            transform: translateY(-8px) scale(1.02) !important;
            box-shadow: 0 15px 50px rgba(0, 0, 0, 0.5) !important;
          }
          .small-box.bg-aqua {
            background: linear-gradient(135deg, #667eea 0%, #764ba2 100%) !important;
          }
          .small-box.bg-green {
            background: linear-gradient(135deg, #11998e 0%, #38ef7d 100%) !important;
          }
          .small-box.bg-yellow {
            background: linear-gradient(135deg, #fbbd24 0%, #f5576c 100%) !important;
          }
          .small-box.bg-red {
            background: linear-gradient(135deg, #fa709a 0%, #fee140 100%) !important;
          }

          /* Cajas de contenido mejoradas */
          .box {
            border-radius: 20px !important;
            box-shadow: 0 8px 30px rgba(0, 0, 0, 0.3) !important;
            border-top: none !important;
            background: #1e1e24 !important;
            transition: all 0.3s ease !important;
          }
          .box:hover {
            box-shadow: 0 12px 40px rgba(0, 0, 0, 0.4) !important;
          }
          .box-header {
            background: linear-gradient(135deg, #667eea 0%, #764ba2 50%, #fbbd24 100%) !important;
            color: white !important;
            border-radius: 20px 20px 0 0 !important;
            padding: 15px 20px !important;
          }
          .box-title {
            font-weight: 700 !important;
            font-size: 18px !important;
          }
          .box-body {
            background: #1e1e24 !important;
            color: #e0e0e0 !important;
          }

          /* Botones con efectos dorados */
          .btn-primary, .btn-success {
            background: linear-gradient(135deg, #667eea 0%, #764ba2 50%, #fbbd24 100%) !important;
            border: none !important;
            border-radius: 30px !important;
            padding: 14px 35px !important;
            font-weight: 700 !important;
            font-size: 16px !important;
            box-shadow: 0 6px 20px rgba(251, 189, 36, 0.4) !important;
            transition: all 0.4s cubic-bezier(0.4, 0, 0.2, 1) !important;
            text-transform: uppercase !important;
            letter-spacing: 0.5px !important;
          }
          .btn-primary:hover, .btn-success:hover {
            transform: translateY(-3px) scale(1.05) !important;
            box-shadow: 0 10px 30px rgba(251, 189, 36, 0.6) !important;
            background: linear-gradient(135deg, #fbbd24 0%, #764ba2 50%, #667eea 100%) !important;
          }
          .btn-default {
            border-radius: 30px !important;
            padding: 12px 28px !important;
            transition: all 0.3s ease !important;
            border: 2px solid #667eea !important;
            background: transparent !important;
            color: #667eea !important;
            font-weight: 600 !important;
          }
          .btn-default:hover {
            background: rgba(102, 126, 234, 0.1) !important;
            border-color: #fbbd24 !important;
            color: #fbbd24 !important;
            transform: translateY(-2px) !important;
          }

          /* Estilos para pickerInput (bootstrap-select) */
          /* Estilos para pickerInput (bootstrap-select) */
          .bootstrap-select > .dropdown-toggle,
          .bootstrap-select > .dropdown-toggle:hover,
          .bootstrap-select > .dropdown-toggle:focus,
          .bootstrap-select > .dropdown-toggle:active {
            background-color: #1e1e24 !important;
            background: #1e1e24 !important;
            color: #e0e0e0 !important;
            border: 1px solid #667eea !important;
            box-shadow: none !important;
            outline: none !important;
          }
          .bootstrap-select .dropdown-menu {
            background-color: #1e1e24 !important;
            background: #1e1e24 !important;
            border: 1px solid #667eea !important;
          }
          .bootstrap-select .dropdown-menu li a {
            color: #e0e0e0 !important;
            background: transparent !important;
          }
          .bootstrap-select .dropdown-menu li a:hover,
          .bootstrap-select .dropdown-menu li a:focus,
          .bootstrap-select .dropdown-menu .active a,
          .bootstrap-select .dropdown-menu .selected a {
            background-color: #667eea !important;
            background: #667eea !important;
            color: #ffffff !important;
          }
          .bootstrap-select .filter-option {
            color: #e0e0e0 !important;
          }

          /* Clase personalizada para el botón del picker */
          .btn-dark-custom {
            background-color: #1e1e24 !important;
            background: #1e1e24 !important;
            color: #e0e0e0 !important;
            border: 1px solid #667eea !important;
            box-shadow: none !important;
          }
          .btn-dark-custom:hover, .btn-dark-custom:focus, .btn-dark-custom:active, .btn-dark-custom.active {
            background-color: #2d2d35 !important;
            color: #ffffff !important;
            border-color: #fbbd24 !important;
          }

          /* Footer FIJO al final - SOLUCIÓN DEFINITIVA */
          html, body {
            height: 100% !important;
            margin: 0 !important;
            padding: 0 !important;
          }
          .wrapper {
            min-height: 100vh !important;
            display: flex !important;
            flex-direction: column !important;
            position: relative !important;
            padding-bottom: 50px !important;
          }
          .content-wrapper {
            flex: 1 !important;
            background: #0a0a0f !important;
            padding-bottom: 55px !important;
          }
          .main-footer {
            position: fixed !important;
            bottom: 0 !important;
            left: 0 !important;
            right: 0 !important;
            width: 100% !important;
            background: linear-gradient(135deg, #0a0a0f 0%, #1e1e24 100%) !important;
            color: #e0e0e0 !important;
            border-top: 2px solid transparent !important;
            border-image: linear-gradient(90deg, #667eea 0%, #764ba2 50%, #fbbd24 100%) 1 !important;
            padding: 3px 10px !important;
            text-align: center !important;
            box-shadow: 0 -2px 8px rgba(0, 0, 0, 0.3) !important;
            z-index: 9999 !important;
            min-height: 35px !important;
          }
          .footer-logo {
            width: 40px !important;
            height: 40px !important;
            margin: 0 auto 10px !important;
            display: block !important;
            filter: drop-shadow(0 0 10px rgba(251, 189, 36, 0.5)) !important;
          }
          .footer-brand {
            font-size: 12px !important;
            font-weight: 700 !important;
            background: linear-gradient(135deg, #667eea 0%, #fbbd24 100%) !important;
            -webkit-background-clip: text !important;
            -webkit-text-fill-color: transparent !important;
            background-clip: text !important;
            margin-bottom: 8px !important;
            display: inline-block !important;
          }
          .footer-tagline {
            font-size: 10px !important;
            color: #a0a0a0 !important;
            margin-top: 1px !important;
            font-weight: 400 !important;
          }

          /* Tablas mejoradas */
          table.dataTable {
            background: #1e1e24 !important;
            color: #e0e0e0 !important;
          }
          table.dataTable thead {
            background: linear-gradient(135deg, #667eea 0%, #764ba2 50%, #fbbd24 100%) !important;
            color: white !important;
          }
          table.dataTable thead th {
            font-weight: 700 !important;
            padding: 15px !important;
          }
          table.dataTable tbody tr {
            transition: all 0.3s ease !important;
          }
          table.dataTable tbody tr:hover {
            background-color: rgba(251, 189, 36, 0.1) !important;
            transform: scale(1.01) !important;
          }
          table.dataTable tbody td {
            padding: 12px !important;
          }

          /* Secciones de ayuda */
          .help-section {
            padding: 25px !important;
            margin: 20px 0 !important;
            border-left: 5px solid #fbbd24 !important;
            background: linear-gradient(135deg, rgba(102, 126, 234, 0.05) 0%, rgba(251, 189, 36, 0.05) 100%) !important;
            border-radius: 12px !important;
            transition: all 0.3s ease !important;
          }
          .help-section:hover {
            box-shadow: 0 5px 20px rgba(251, 189, 36, 0.2) !important;
            transform: translateX(5px) !important;
          }
          .help-title {
            color: #fbbd24 !important;
            font-weight: 700 !important;
            font-size: 20px !important;
            margin-bottom: 15px !important;
          }

          /* Indicadores de señales */
          .signal-indicator {
            display: inline-block !important;
            padding: 8px 20px !important;
            border-radius: 25px !important;
            font-weight: 700 !important;
            font-size: 14px !important;
            box-shadow: 0 4px 15px rgba(0, 0, 0, 0.3) !important;
            transition: all 0.3s ease !important;
          }
          .signal-indicator:hover {
            transform: scale(1.1) !important;
          }
          .signal-buy {
            background: linear-gradient(135deg, #11998e 0%, #38ef7d 100%) !important;
            color: white !important;
          }
          .signal-sell {
            background: linear-gradient(135deg, #fa709a 0%, #fee140 100%) !important;
            color: white !important;
          }
          .signal-hold {
            background: linear-gradient(135deg, #6c757d 0%, #495057 100%) !important;
            color: white !important;
          }

          /* Scrollbar personalizada */
          ::-webkit-scrollbar {
            width: 10px !important;
          }
          ::-webkit-scrollbar-track {
            background: #0a0a0f !important;
          }
          ::-webkit-scrollbar-thumb {
            background: linear-gradient(135deg, #667eea 0%, #fbbd24 100%) !important;
            border-radius: 5px !important;
          }
          ::-webkit-scrollbar-thumb:hover {
            background: linear-gradient(135deg, #fbbd24 0%, #667eea 100%) !important;
          }

          /* ==================== RESPONSIVE DESIGN ==================== */
          @media (max-width: 768px) {
            .content-wrapper { padding: 10px !important; }
            .box { margin-bottom: 15px !important; }
          }

          /* ==================== DATATABLES MODO OSCURO ==================== */
          .dataTables_wrapper {
            color: #e0e0e0 !important;
          }
          .dataTables_wrapper .dataTables_filter input,
          .dataTables_wrapper .dataTables_length select {
            background: #2a2a3e !important;
            color: #e0e0e0 !important;
            border: 1px solid #667eea !important;
            padding: 5px 10px !important;
            border-radius: 5px !important;
          }
          .dataTables_wrapper .dataTables_paginate .paginate_button {
            background: #2a2a3e !important;
            color: #e0e0e0 !important;
            border: 1px solid #667eea !important;
          }
          .dataTables_wrapper .dataTables_paginate .paginate_button:hover {
            background: linear-gradient(135deg, #667eea 0%, #fbbd24 100%) !important;
            color: white !important;
          }
          table.dataTable {
            background: #1e1e24 !important;
            color: #e0e0e0 !important;
          }
          table.dataTable thead th {
            background: linear-gradient(135deg, #667eea 0%, #764ba2 100%) !important;
            color: white !important;
          }
          table.dataTable tbody tr {
            background: #1e1e24 !important;
          }
          table.dataTable tbody tr:hover {
            background: rgba(102, 126, 234, 0.1) !important;
          }
          table.dataTable tbody td {
            color: #e0e0e0 !important;
          }

          /* ==================== SEÑALES CON COLORES ==================== */
          .signal-buy {
            background: linear-gradient(135deg, #10b981 0%, #059669 100%) !important;
            color: white !important;
            padding: 5px 12px !important;
            border-radius: 15px !important;
            font-weight: 600 !important;
          }
          .signal-sell {
            background: linear-gradient(135deg, #ef4444 0%, #dc2626 100%) !important;
            color: white !important;
            padding: 5px 12px !important;
            border-radius: 15px !important;
            font-weight: 600 !important;
          }
          .signal-hold {
            background: linear-gradient(135deg, #6b7280 0%, #4b5563 100%) !important;
            color: white !important;
            padding: 5px 12px !important;
            border-radius: 15px !important;
            font-weight: 600 !important;
          }
        ")),
      ),
      sidebarMenu(
        id = "sidebar_menu",
        menuItem("Panel de Control", tabName = "control", icon = icon("dashboard")),
        menuItem("Performance", tabName = "performance", icon = icon("chart-line")),
        menuItem("Señales de Trading", tabName = "signals", icon = icon("broadcast-tower")),
        menuItem("Posiciones", tabName = "positions", icon = icon("briefcase")),
        menuItem("Modelos IA", tabName = "models", icon = icon("brain")),
        menuItem("Ayuda del Sistema", tabName = "help", icon = icon("question-circle")),
        menuItem("Configuración", tabName = "config", icon = icon("cogs"))
      ),
      tags$div(
        style = "padding: 20px; margin-top: 30px; background: rgba(102, 126, 234, 0.1);
                 border-radius: 10px; margin-left: 10px; margin-right: 10px;",
        tags$h4("Sistema Activo", style = "color: #667eea; margin: 0; font-size: 14px;"),
        tags$p(
          id = "status_indicator",
          style = "color: #11998e; margin: 5px 0 0 0; font-weight: bold; font-size: 12px;",
          "✓ Operacional"
        )
      )
    ),
    dashboardBody(
      tabItems(
        tabItem(
          tabName = "control",
          fluidRow(
            valueBoxOutput("nav_box", width = 3),
            valueBoxOutput("return_box", width = 3),
            valueBoxOutput("sharpe_box", width = 3),
            valueBoxOutput("dd_box", width = 3)
          ),
          fluidRow(
            box(
              title = "Centro de Comando",
              width = 6,
              status = "primary",
              solidHeader = TRUE,
              tags$div(
                style = "margin-bottom: 15px; padding: 10px; background: rgba(102, 126, 234, 0.1); border-radius: 8px; border: 1px solid rgba(102, 126, 234, 0.3);",
                pickerInput(
                  inputId = "data_source",
                  label = "Fuente de Datos:",
                  choices = c("Yahoo Finance (Default)" = "yahoo", "MetaTrader 5" = "mt5"),
                  selected = "yahoo",
                  options = list(style = "btn-dark-custom")
                ),
                tags$hr(style = "border-color: rgba(102, 126, 234, 0.3); margin: 10px 0;"),
                if (exists("MT5_AVAILABLE") && MT5_AVAILABLE) {
                  tagList(
                    materialSwitch(
                      inputId = "auto_trading",
                      label = tags$span(style = "color: #fbbd24; font-weight: bold;", "🚀 Ejecución Automática MT5"),
                      value = FALSE,
                      status = "warning",
                      right = TRUE
                    ),
                    tags$div(
                      style = "font-size: 11px; color: #a0a0a0; margin-top: 5px;",
                      "⚠️ Las órdenes se enviarán a MT5 inmediatamente tras el análisis."
                    )
                  )
                } else {
                  tags$div(
                    style = "color: #ef4444; font-size: 12px; font-weight: bold;",
                    "⚠️ Módulo MT5 no disponible (verificar logs)"
                  )
                }
              ),
              actionButton(
                "run_analysis",
                "Ejecutar Análisis Completo",
                icon = icon("play"),
                class = "btn-success btn-lg",
                style = "width: 100%; margin-bottom: 15px;"
              ),
              hr(style = "border-color: rgba(102, 126, 234, 0.3);"),
              fluidRow(
                column(
                  6,
                  actionButton(
                    "refresh_data",
                    "Actualizar Datos",
                    icon = icon("refresh"),
                    class = "btn-default",
                    style = "width: 100%;"
                  )
                ),
                column(
                  6,
                  actionButton(
                    "export_report",
                    "Exportar Reporte",
                    icon = icon("file-export"),
                    class = "btn-default",
                    style = "width: 100%;"
                  )
                )
              )
            ),
            box(
              title = "Estado del Sistema",
              width = 6,
              status = "info",
              solidHeader = TRUE,
              htmlOutput("system_status"),
              hr(style = "border-color: rgba(102, 126, 234, 0.3);"),
              tags$div(
                style = "background: #1e1e24; padding: 10px; border-radius: 10px; margin-top: 10px;",
                tags$h5("Evolución NAV (últimos 100 días)", style = "color: #fbbd24; margin-bottom: 10px;"),
                plotOutput("mini_nav_plot", height = 300)
              )
            )
          )
        ),
        tabItem(
          tabName = "performance",
          fluidRow(
            box(
              title = "Evolución del NAV (Valor Neto de Activos)",
              width = 12,
              status = "primary",
              solidHeader = TRUE,
              plotOutput("nav_plot", height = 400)
            )
          ),
          fluidRow(
            box(
              title = "Drawdown del Portafolio",
              width = 6,
              status = "danger",
              solidHeader = TRUE,
              plotOutput("dd_plot", height = 300)
            ),
            box(
              title = "Retornos Rolling (21 días)",
              width = 6,
              status = "success",
              solidHeader = TRUE,
              plotOutput("rolling_ret_plot", height = 300)
            )
          ),
          fluidRow(
            box(
              title = "Métricas de Rendimiento",
              width = 12,
              status = "warning",
              solidHeader = TRUE,
              DTOutput("performance_table")
            )
          )
        ),
        tabItem(
          tabName = "signals",
          fluidRow(
            box(
              title = "Señales Activas de Trading",
              width = 12,
              status = "warning",
              solidHeader = TRUE,
              DTOutput("signals_table")
            )
          ),
          fluidRow(
            box(
              title = "Distribución de Señales por Activo",
              width = 6,
              status = "info",
              solidHeader = TRUE,
              plotOutput("signals_dist", height = 350)
            ),
            box(
              title = "Nivel de Confianza por Activo",
              width = 6,
              status = "success",
              solidHeader = TRUE,
              plotOutput("confidence_plot", height = 350)
            )
          )
        ),
        tabItem(
          tabName = "positions",
          fluidRow(
            box(
              title = "Posiciones Actuales del Portafolio",
              width = 12,
              status = "primary",
              solidHeader = TRUE,
              DTOutput("positions_table")
            )
          ),
          fluidRow(
            box(
              title = "Exposición por Activo",
              width = 12,
              status = "info",
              solidHeader = TRUE,
              plotOutput("exposure_plot", height = 400)
            )
          )
        ),
        tabItem(
          tabName = "models",
          fluidRow(
            box(
              title = "Modelos de Inteligencia Artificial",
              width = 12,
              status = "primary",
              solidHeader = TRUE,

              # Contexto explicativo
              tags$div(
                style = "background: linear-gradient(135deg, rgba(102, 126, 234, 0.1) 0%, rgba(251, 189, 36, 0.1) 100%);
                         padding: 20px; border-radius: 10px; margin-bottom: 20px; border-left: 4px solid #667eea;",
                tags$div(
                  style = "display: flex; align-items: center; margin-bottom: 10px;",
                  tags$span(style = "font-size: 28px; margin-right: 12px;", "🤖"),
                  tags$h4("¿Qué son los Modelos de IA?", style = "margin: 0; color: #fbbd24;")
                ),
                tags$p(
                  style = "color: #e0e0e0; line-height: 1.6; margin: 10px 0;",
                  "Los modelos de Machine Learning son algoritmos entrenados con datos históricos del mercado para predecir movimientos futuros de precios. Este sistema utiliza ",
                  tags$strong("Random Forest"), " y otros algoritmos avanzados para generar señales de trading con alta precisión."
                ),
                tags$div(
                  style = "display: grid; grid-template-columns: repeat(auto-fit, minmax(200px, 1fr)); gap: 10px; margin-top: 15px;",
                  tags$div(
                    style = "background: #2a2a3e; padding: 10px; border-radius: 8px;",
                    tags$div(style = "color: #fbbd24; font-weight: 600;", "📊 Features"),
                    tags$div(style = "color: #a0a0a0; font-size: 13px;", "Volatilidad, medias móviles, momentum")
                  ),
                  tags$div(
                    style = "background: #2a2a3e; padding: 10px; border-radius: 8px;",
                    tags$div(style = "color: #10b981; font-weight: 600;", "🎯 Objetivo"),
                    tags$div(style = "color: #a0a0a0; font-size: 13px;", "Predecir movimientos alcistas/bajistas")
                  ),
                  tags$div(
                    style = "background: #2a2a3e; padding: 10px; border-radius: 8px;",
                    tags$div(style = "color: #667eea; font-weight: 600;", "🔄 Actualización"),
                    tags$div(style = "color: #a0a0a0; font-size: 13px;", "Walk-Forward automático")
                  ),
                  tags$div(
                    style = "background: #2a2a3e; padding: 10px; border-radius: 8px;",
                    tags$div(style = "color: #ef4444; font-weight: 600;", "✅ Validación"),
                    tags$div(style = "color: #a0a0a0; font-size: 13px;", "Confianza > 60% requerida")
                  )
                )
              ),

              # Información de modelos
              htmlOutput("models_summary")
            )
          )
        ),
        tabItem(
          tabName = "help",
          fluidRow(
            box(
              title = "Guía Completa del Sistema LLAMA IA",
              width = 12,
              status = "info",
              solidHeader = TRUE,
              tags$div(
                class = "help-section",
                tags$h3("¿Qué es el Sistema Cuantitativo ETF Pro?", class = "help-title"),
                tags$p(
                  "El Sistema Cuantitativo ETF Pro v6.0 de LLAMA IA es una plataforma avanzada de trading algorítmico
                  que utiliza Inteligencia Artificial (Machine Learning) para analizar mercados financieros y generar
                  señales de trading automatizadas en ETFs (Exchange-Traded Funds)."
                ),
                tags$ul(
                  tags$li(tags$strong("Tecnología:"), " Random Forest y algoritmos de Machine Learning"),
                  tags$li(tags$strong("Activos:"), " ETFs de renta variable, bonos, oro y sectores tecnológicos"),
                  tags$li(tags$strong("Metodología:"), " Walk-Forward Analysis con validación continua"),
                  tags$li(tags$strong("Gestión de Riesgo:"), " Stop-loss, take-profit y circuit breakers automáticos")
                )
              ),
              tags$div(
                class = "help-section",
                tags$h3("Frecuencia de Ejecución", class = "help-title"),
                tags$p(tags$strong("¿Cada cuánto debe ejecutarse el sistema?")),
                tags$ul(
                  tags$li(tags$strong("Análisis Completo:"), " Se recomienda ejecutar 1 vez al día, preferiblemente después del cierre del mercado (después de las 4:00 PM EST)"),
                  tags$li(tags$strong("Actualización de Datos:"), " El sistema descarga automáticamente los datos más recientes al ejecutarse"),
                  tags$li(tags$strong("Reentrenamiento de Modelos:"), " Los modelos se actualizan automáticamente usando la metodología Walk-Forward"),
                  tags$li(tags$strong("Generación de Señales:"), " Las señales se generan diariamente basadas en los modelos actualizados")
                ),
                tags$div(
                  style = "background: rgba(17, 153, 142, 0.1); padding: 15px; border-radius: 8px; margin-top: 10px;",
                  tags$p(
                    style = "margin: 0; color: #11998e; font-weight: 600;",
                    "✓ Recomendación: Ejecutar el análisis cada noche entre las 6:00 PM y 8:00 PM después del cierre de mercado."
                  )
                )
              ),
              tags$div(
                class = "help-section",
                tags$h3("Interpretación de Señales", class = "help-title"),
                tags$p(tags$strong("¿Cómo interpretar las señales del sistema?")),
                tags$p("El sistema genera tres tipos de señales para cada activo, mostradas con colores distintivos:"),
                tags$div(
                  style = "margin: 15px 0;",
                  tags$div(
                    style = "margin: 15px 0; padding: 15px; background: rgba(16, 185, 129, 0.1); border-radius: 8px; border-left: 4px solid #10b981;",
                    tags$div(
                      style = "display: flex; align-items: center; margin-bottom: 8px;",
                      tags$span(style = "font-size: 20px; margin-right: 10px;", "📈"),
                      tags$span("BUY", style = "background: linear-gradient(135deg, #10b981 0%, #059669 100%);
                                                  color: white; padding: 5px 15px; border-radius: 15px;
                                                  font-weight: 600; margin-right: 10px;"),
                      tags$span("SEÑAL DE COMPRA", style = "color: #10b981; font-weight: 600;")
                    ),
                    tags$p(
                      style = "margin: 5px 0 0 0; color: #e0e0e0;",
                      "El modelo predice movimientos alcistas. Se recomienda tomar posición LARGA en el activo. ",
                      "Aparece en ", tags$strong("verde"), " en la tabla de señales."
                    )
                  ),
                  tags$div(
                    style = "margin: 15px 0; padding: 15px; background: rgba(239, 68, 68, 0.1); border-radius: 8px; border-left: 4px solid #ef4444;",
                    tags$div(
                      style = "display: flex; align-items: center; margin-bottom: 8px;",
                      tags$span(style = "font-size: 20px; margin-right: 10px;", "📉"),
                      tags$span("SELL", style = "background: linear-gradient(135deg, #ef4444 0%, #dc2626 100%);
                                                   color: white; padding: 5px 15px; border-radius: 15px;
                                                   font-weight: 600; margin-right: 10px;"),
                      tags$span("SEÑAL DE VENTA", style = "color: #ef4444; font-weight: 600;")
                    ),
                    tags$p(
                      style = "margin: 5px 0 0 0; color: #e0e0e0;",
                      "El modelo predice movimientos bajistas. Se recomienda cerrar posiciones o mantenerse fuera. ",
                      "Aparece en ", tags$strong("rojo"), " en la tabla de señales."
                    )
                  ),
                  tags$div(
                    style = "margin: 15px 0; padding: 15px; background: rgba(107, 114, 128, 0.1); border-radius: 8px; border-left: 4px solid #6b7280;",
                    tags$div(
                      style = "display: flex; align-items: center; margin-bottom: 8px;",
                      tags$span(style = "font-size: 20px; margin-right: 10px;", "⏸️"),
                      tags$span("HOLD", style = "background: linear-gradient(135deg, #6b7280 0%, #4b5563 100%);
                                                  color: white; padding: 5px 15px; border-radius: 15px;
                                                  font-weight: 600; margin-right: 10px;"),
                      tags$span("MANTENER POSICIÓN", style = "color: #6b7280; font-weight: 600;")
                    ),
                    tags$p(
                      style = "margin: 5px 0 0 0; color: #e0e0e0;",
                      "El modelo no tiene confianza suficiente (< 60%). Mantener posiciones actuales o permanecer en efectivo. ",
                      "Aparece en ", tags$strong("gris"), " en la tabla de señales."
                    )
                  )
                )
              ),
              tags$div(
                class = "help-section",
                tags$h3("¿Cómo Ingresar al Mercado?", class = "help-title"),
                tags$p(tags$strong("Protocolo de Ejecución de Operaciones:")),
                tags$ol(
                  tags$li(
                    tags$strong("Al día siguiente del análisis:"),
                    " Ejecutar las operaciones en el mercado ANTES de la apertura (pre-market) o en los primeros 30 minutos de trading"
                  ),
                  tags$li(
                    tags$strong("Revisar el nivel de confianza:"),
                    " Solo ejecutar señales con nivel de confianza ≥ 60% (mostrado en gráfico de confianza)"
                  ),
                  tags$li(
                    tags$strong("Asignación de capital:"),
                    " El sistema calcula automáticamente el % óptimo de capital para cada activo basado en:",
                    tags$ul(
                      tags$li("Optimización media-varianza"),
                      tags$li("Nivel de confianza del modelo"),
                      tags$li("Límite máximo por posición: 25% del capital")
                    )
                  ),
                  tags$li(
                    tags$strong("Protección automática:"),
                    tags$ul(
                      tags$li(tags$strong("Stop Loss:"), " 2% - Cierre automático si la pérdida alcanza este nivel"),
                      tags$li(tags$strong("Take Profit:"), " 4% - Cierre automático al alcanzar ganancia objetivo"),
                      tags$li(tags$strong("Circuit Breaker:"), " 20% - Cierre de todas las posiciones si el drawdown total supera este umbral")
                    )
                  )
                ),
                tags$div(
                  style = "background: rgba(250, 112, 154, 0.1); padding: 15px; border-radius: 8px; margin-top: 15px;",
                  tags$p(
                    style = "margin: 0; color: #fa709a; font-weight: 600;",
                    "⚠️ IMPORTANTE: Las señales son válidas para el día de trading siguiente. No ejecutar señales con más de 24 horas de antigüedad."
                  )
                )
              ),
              tags$div(
                class = "help-section",
                tags$h3("🚀 Ejecución Automática con MT5", class = "help-title"),
                tags$p(
                  "El sistema ahora incluye un módulo de conexión directa con MetaTrader 5 para automatizar la ejecución de órdenes.
                  Esta funcionalidad elimina la necesidad de ingresar operaciones manualmente."
                ),
                tags$div(
                  style = "display: flex; gap: 20px; margin-top: 15px;",
                  tags$div(
                    style = "flex: 1; background: rgba(102, 126, 234, 0.1); padding: 15px; border-radius: 8px; border-left: 4px solid #667eea;",
                    tags$h4("¿Cómo funciona?", style = "color: #667eea; margin-top: 0;"),
                    tags$ol(
                      tags$li("Active el interruptor 'Ejecución Automática MT5' en el Panel de Control."),
                      tags$li("Asegúrese de tener su terminal MT5 abierta en el escritorio."),
                      tags$li("Haga clic en 'Ejecutar Análisis Completo'."),
                      tags$li("Al finalizar el análisis, el sistema calculará el lotaje exacto y enviará las órdenes a MT5.")
                    )
                  ),
                  tags$div(
                    style = "flex: 1; background: rgba(251, 189, 36, 0.1); padding: 15px; border-radius: 8px; border-left: 4px solid #fbbd24;",
                    tags$h4("Requisitos Técnicos", style = "color: #fbbd24; margin-top: 0;"),
                    tags$ul(
                      tags$li("Terminal MetaTrader 5 instalada y abierta."),
                      tags$li("Librería Python instalada: ", tags$code("pip install MetaTrader5")),
                      tags$li("Cuenta de trading activa y conectada."),
                      tags$li("Permisos de Auto-Trading habilitados en MT5 (opcional, pero recomendado).")
                    )
                  )
                ),
                tags$div(
                  style = "margin-top: 15px; font-size: 13px; color: #a0a0a0;",
                  tags$strong("Nota de Seguridad:"), " El sistema verifica el precio actual, calcula el volumen basado en su gestión de riesgo y coloca Stop Loss y Take Profit automáticamente en cada orden."
                )
              ),
              tags$div(
                class = "help-section",
                tags$h3("🔧 Configuración de Símbolos (Brokers)", class = "help-title"),
                tags$p(
                  "Diferentes brokers de MT5 utilizan diferentes nombres para los mismos activos (ej. SPY vs SPY.US vs #SPY).
                  El sistema permite configurar estas equivalencias para asegurar la correcta descarga de datos y ejecución."
                ),
                tags$div(
                  style = "background: rgba(139, 92, 246, 0.1); padding: 15px; border-radius: 8px; border-left: 4px solid #8b5cf6; margin-top: 15px;",
                  tags$h4("¿Cómo configurar el mapeo?", style = "color: #8b5cf6; margin-top: 0;"),
                  tags$ol(
                    tags$li("Vaya a la pestaña ", tags$strong("Configuración"), " en el menú lateral."),
                    tags$li("Verá una tabla con dos columnas: ", tags$strong("SystemSymbol"), " (interno) y ", tags$strong("MT5Symbol"), " (broker)."),
                    tags$li("Identifique cómo su broker nombra los ETFs (ej. busque 'SPY' en su Observación de Mercado)."),
                    tags$li("Haga doble clic en la celda de la columna ", tags$strong("MT5Symbol"), " correspondiente."),
                    tags$li("Escriba el nombre exacto que usa su broker (ej. 'SPY.US') y presione Enter."),
                    tags$li("Haga clic en ", tags$strong("Guardar Cambios"), ".")
                  )
                ),
                tags$div(
                  style = "margin-top: 15px;",
                  tags$p(tags$strong("Ejemplos comunes:")),
                  tags$ul(
                    tags$li("Standard: ", tags$code("SPY"), " -> ", tags$code("SPY")),
                    tags$li("Con sufijo de país: ", tags$code("SPY"), " -> ", tags$code("SPY.US")),
                    tags$li("Con prefijo: ", tags$code("SPY"), " -> ", tags$code("#SPY")),
                    tags$li("CFDs: ", tags$code("SPY"), " -> ", tags$code("SPY_CFD"))
                  )
                )
              ),
              tags$div(
                class = "help-section",
                tags$h3("Métricas Clave", class = "help-title"),
                tags$ul(
                  tags$li(tags$strong("NAV (Net Asset Value):"), " Valor total del portafolio incluyendo ganancias/pérdidas"),
                  tags$li(tags$strong("Sharpe Ratio:"), " Medida de retorno ajustado por riesgo. >1.0 es excelente, >0.5 es bueno"),
                  tags$li(tags$strong("Max Drawdown:"), " Pérdida máxima desde un pico. Límite del sistema: 20%"),
                  tags$li(tags$strong("Win Rate:"), " Porcentaje de operaciones ganadoras"),
                  tags$li(tags$strong("Sortino Ratio:"), " Retorno ajustado por riesgo a la baja")
                )
              ),
              tags$div(
                class = "help-section",
                tags$h3("Gestión de Riesgo Integrada", class = "help-title"),
                tags$ul(
                  tags$li("Límite de pérdida por operación: Stop-loss al 2%"),
                  tags$li("Límite de ganancia por operación: Take-profit al 4%"),
                  tags$li("Límite de exposición por activo: Máximo 25% del capital"),
                  tags$li("Circuit breaker total: Cierre automático al 20% de drawdown"),
                  tags$li("Filtro de volatilidad: Exclusión de periodos con volatilidad >30%"),
                  tags$li("Validación de confianza: Solo señales con >60% de confianza del modelo")
                )
              ),
              tags$div(
                class = "help-section",
                tags$h3("Flujo de Trabajo Recomendado", class = "help-title"),
                tags$div(
                  style = "background: linear-gradient(135deg, rgba(102, 126, 234, 0.1) 0%, rgba(251, 189, 36, 0.1) 100%);
                           padding: 20px; border-radius: 10px; border-left: 4px solid #fbbd24;",
                  tags$ol(
                    tags$li(tags$strong("Lunes a Viernes - 6:00 PM:"), " Ejecutar análisis completo"),
                    tags$li(tags$strong("Revisar señales:"), " Ir a pestaña 'Señales de Trading' y verificar señales activas"),
                    tags$li(tags$strong("Verificar confianza:"), " Revisar gráfico de confianza - solo señales >60%"),
                    tags$li(tags$strong("Día siguiente - Pre-Market:"), " Ejecutar órdenes en broker basadas en señales"),
                    tags$li(tags$strong("Monitorear:"), " Revisar pestaña 'Posiciones' durante el día de trading"),
                    tags$li(tags$strong("Al cierre:"), " Verificar métricas de performance")
                  )
                )
              )
            )
          )
        ),
        # Pestaña Configuración
        tabItem(
          tabName = "config",
          fluidRow(
            box(
              title = "Mapeo de Símbolos MT5",
              width = 12,
              status = "primary",
              solidHeader = TRUE,
              tags$p("Configure la correspondencia entre los símbolos del sistema y los símbolos de su broker en MT5."),
              tags$p(style = "color: #a0a0a0; font-size: 12px;", "Edite la columna 'MT5Symbol' haciendo doble clic en la celda. Los cambios se guardan automáticamente."),
              DTOutput("symbol_map_table"),
              tags$hr(),
              actionButton("save_map", "Guardar Cambios", icon = icon("save"), class = "btn-success")
            )
          )
        )
      ), # Cierre de tabItems

      tags$footer(
        class = "main-footer",
        tags$div(
          # Logo de la llama
          tags$div(
            style = "margin-bottom: 1px;",
            tags$span(
              style = "font-size: 16px; display: inline-block;
                       background: linear-gradient(135deg, #667eea 0%, #fbbd24 100%);
                       -webkit-background-clip: text; -webkit-text-fill-color: transparent;
                       filter: drop-shadow(0 0 2px rgba(251, 189, 36, 0.2));",
              "🦙"
            )
          ),
          tags$div(class = "footer-brand", "LLAMA IA © 2024"),
          tags$div(class = "footer-tagline", "Inteligencia Artificial para Trading"),
          tags$div(
            style = "margin-top: 2px; font-size: 8px; color: #909090;",
            "Sistema Cuantitativo ETF Pro v6.0 | ",
            tags$a(
              href = "https://llamaia.nbmsystemas.com/",
              target = "_blank",
              style = "color: #fbbd24; text-decoration: none; font-weight: 600;
                       transition: all 0.3s ease;",
              onmouseover = "this.style.color='#667eea'",
              onmouseout = "this.style.color='#fbbd24'",
              "llamaia.nbmsystemas.com"
            )
          )
        )
      )
    )
  )
}

# ==================== SHINY SERVER (CON DATOS REALES) =============================
server_shiny_v6 <- function(input, output, session) {
  rv <- reactiveValues(core_data = NULL, last_update = NULL, is_running = FALSE)

  # Sistema de caché con verificación de timestamps
  last_file_times <- reactiveVal(list())

  observe({
    # Solo verificar archivos cada 2 segundos para no sobrecargar
    invalidateLater(2000)

    metrics_file <- file.path(CFG$outdir, "metrics.rds")

    if (file.exists(metrics_file)) {
      # Verificar si los archivos han cambiado
      current_time <- file.info(metrics_file)$mtime
      last_times <- last_file_times()

      # Solo recargar si es la primera vez o si el archivo cambió
      if (is.null(last_times$metrics) || current_time > last_times$metrics) {
        tryCatch(
          {
            # Mostrar indicador de carga
            withProgress(message = "Cargando datos...", value = 0, {
              incProgress(0.2, detail = "Métricas")
              rv$core_data <- list(
                metrics = readRDS(metrics_file),
                signals = readRDS(file.path(CFG$outdir, "signals.rds")),
                models = readRDS(file.path(CFG$outdir, "models_all.rds"))
              )

              incProgress(0.5, detail = "Confidence")
              conf_file <- file.path(CFG$outdir, "confidence.rds")
              if (file.exists(conf_file)) {
                rv$core_data$confidence <- readRDS(conf_file)
                log_msg("✓ Confidence cargada desde caché")
              } else if (!is.null(rv$core_data$metrics$confidence)) {
                rv$core_data$confidence <- rv$core_data$metrics$confidence
                log_msg("✓ Confidence cargada desde metrics")
              }

              incProgress(1.0, detail = "Completado")
              rv$last_update <- Sys.time()

              # Actualizar timestamp del caché
              last_file_times(list(metrics = current_time))
            })
          },
          error = function(e) {
            log_msg("Error cargando datos:", e$message, level = "WARN")
          }
        )
      }
    }
  })

  output$nav_box <- renderValueBox({
    if (is.null(rv$core_data)) {
      valueBox("N/A", "NAV Actual", icon = icon("dollar-sign"), color = "aqua")
    } else {
      nav <- as.numeric(tail(rv$core_data$metrics$NAV, 1))
      valueBox(format(nav, big.mark = ",", digits = 2), "NAV Actual",
        icon = icon("dollar-sign"), color = "aqua"
      )
    }
  })

  output$return_box <- renderValueBox({
    if (is.null(rv$core_data) || is.null(rv$core_data$metrics$returns)) {
      valueBox("N/A", "Retorno Total", icon = icon("chart-line"), color = "green")
    } else {
      ret <- Return.cumulative(rv$core_data$metrics$returns) * 100
      valueBox(paste0(round(ret, 2), "%"), "Retorno Total",
        icon = icon("chart-line"),
        color = if (ret > 0) "green" else "red"
      )
    }
  })

  output$sharpe_box <- renderValueBox({
    if (is.null(rv$core_data) || is.null(rv$core_data$metrics$returns)) {
      valueBox("N/A", "Sharpe Ratio", icon = icon("trophy"), color = "yellow")
    } else {
      sr <- SharpeRatio.annualized(rv$core_data$metrics$returns, Rf = CFG$tasa_libre_riesgo)
      valueBox(round(sr, 2), "Sharpe Ratio",
        icon = icon("trophy"),
        color = if (sr > 1) "green" else if (sr > 0.5) "yellow" else "red"
      )
    }
  })

  output$dd_box <- renderValueBox({
    if (is.null(rv$core_data) || is.null(rv$core_data$metrics$returns)) {
      valueBox("N/A", "Max Drawdown", icon = icon("arrow-down"), color = "red")
    } else {
      dd <- maxDrawdown(rv$core_data$metrics$returns) * 100
      valueBox(paste0(round(dd, 2), "%"), "Max Drawdown", icon = icon("arrow-down"), color = "red")
    }
  })

  output$system_status <- renderUI({
    tags$div(
      style = "background: linear-gradient(135deg, rgba(102, 126, 234, 0.1) 0%, rgba(251, 189, 36, 0.1) 100%);
               padding: 20px; border-radius: 10px; color: #e0e0e0;",
      tags$div(
        style = "display: flex; align-items: center; margin-bottom: 15px; border-bottom: 2px solid #667eea; padding-bottom: 10px;",
        tags$span(style = "font-size: 24px; margin-right: 10px;", "🚀"),
        tags$h4("Sistema ETF Pro v6.0", style = "margin: 0; color: #fbbd24; font-weight: bold;")
      ),
      tags$div(
        style = "display: grid; grid-template-columns: 1fr 1fr; gap: 15px; margin-top: 15px;",

        # Estado
        tags$div(
          style = "background: #2a2a3e; padding: 12px; border-radius: 8px; border-left: 4px solid #10b981;",
          tags$div(
            style = "display: flex; align-items: center;",
            tags$span(style = "font-size: 20px; margin-right: 8px;", if (rv$is_running) "⚡" else "✅"),
            tags$div(
              tags$div("Estado", style = "font-size: 11px; color: #a0a0a0;"),
              tags$div(
                if (rv$is_running) "EJECUTANDO" else "LISTO",
                style = paste0("font-size: 16px; font-weight: 600; color: ", if (rv$is_running) "#fbbd24" else "#10b981", ";")
              )
            )
          )
        ),

        # Última actualización
        tags$div(
          style = "background: #2a2a3e; padding: 12px; border-radius: 8px; border-left: 4px solid #667eea;",
          tags$div(
            style = "display: flex; align-items: center;",
            tags$span(style = "font-size: 20px; margin-right: 8px;", "🕒"),
            tags$div(
              tags$div("Última Actualización", style = "font-size: 11px; color: #a0a0a0;"),
              tags$div(
                if (!is.null(rv$last_update)) format(rv$last_update, "%H:%M:%S") else "N/A",
                style = "font-size: 16px; font-weight: 600; color: #e0e0e0;"
              )
            )
          )
        ),

        # Activos
        tags$div(
          style = "background: #2a2a3e; padding: 12px; border-radius: 8px; border-left: 4px solid #fbbd24;",
          tags$div(
            style = "display: flex; align-items: center;",
            tags$span(style = "font-size: 20px; margin-right: 8px;", "📊"),
            tags$div(
              tags$div("Activos", style = "font-size: 11px; color: #a0a0a0;"),
              tags$div(
                length(CFG$symbols),
                style = "font-size: 16px; font-weight: 600; color: #fbbd24;"
              )
            )
          )
        ),

        # Capital
        tags$div(
          style = "background: #2a2a3e; padding: 12px; border-radius: 8px; border-left: 4px solid #10b981;",
          tags$div(
            style = "display: flex; align-items: center;",
            tags$span(style = "font-size: 20px; margin-right: 8px;", "💰"),
            tags$div(
              tags$div("Capital Inicial", style = "font-size: 11px; color: #a0a0a0;"),
              tags$div(
                paste0("$", format(CFG$capital_inicial, big.mark = ",")),
                style = "font-size: 16px; font-weight: 600; color: #10b981;"
              )
            )
          )
        )
      ),

      # Información adicional si hay datos
      if (!is.null(rv$core_data)) {
        tags$div(
          style = "margin-top: 15px; padding-top: 15px; border-top: 1px solid rgba(102, 126, 234, 0.3);",
          tags$div(
            style = "display: grid; grid-template-columns: 1fr 1fr; gap: 10px;",
            tags$div(
              tags$span(style = "color: #a0a0a0;", "🤖 Modelos: "),
              tags$span(style = "color: #fbbd24; font-weight: 600;", length(rv$core_data$models))
            ),
            tags$div(
              tags$span(style = "color: #a0a0a0;", "📈 Trades: "),
              tags$span(style = "color: #10b981; font-weight: 600;", rv$core_data$metrics$n_trades)
            )
          )
        )
      }
    )
  })

  output$mini_nav_plot <- renderPlot(
    {
      if (!is.null(rv$core_data) && !is.null(rv$core_data$metrics$NAV)) {
        # Convertir xts a vector numérico correctamente
        nav_xts <- rv$core_data$metrics$NAV
        nav_data <- as.numeric(tail(nav_xts, 100))

        # Configurar fondo oscuro
        par(
          bg = "#1e1e24", col.axis = "#e0e0e0", col.lab = "#e0e0e0",
          col.main = "#fbbd24", fg = "#e0e0e0"
        )

        plot(nav_data,
          main = "",
          ylab = "NAV ($)",
          xlab = "Días",
          col = "#10b981",
          lwd = 3,
          type = "l",
          las = 1,
          cex.axis = 0.9
        )

        # Añadir área bajo la curva
        polygon(c(1:length(nav_data), length(nav_data):1),
          c(nav_data, rep(min(nav_data, na.rm = TRUE), length(nav_data))),
          col = rgb(16, 185, 129, alpha = 50, maxColorValue = 255),
          border = NA
        )

        # Grid
        grid(col = "#2a2a3e", lty = 1, lwd = 1)

        # Línea de tendencia
        abline(lm(nav_data ~ seq_along(nav_data)),
          col = "#fbbd24", lwd = 2, lty = 2
        )
      } else {
        # Mensaje si no hay datos
        par(bg = "#1e1e24", col = "#e0e0e0")
        plot(1,
          type = "n", axes = FALSE, xlab = "", ylab = "",
          xlim = c(0, 1), ylim = c(0, 1)
        )
        text(0.5, 0.5, "Ejecuta un análisis para ver el gráfico NAV",
          cex = 1.2, col = "#a0a0a0"
        )
        box(col = "#2a2a3e")
      }
    },
    bg = "#1e1e24"
  )

  output$nav_plot <- renderPlot(
    {
      req(rv$core_data)
      req(rv$core_data$metrics$NAV)

      nav_vec <- as.numeric(rv$core_data$metrics$NAV)
      nav_dates <- index(rv$core_data$metrics$NAV)

      # Configurar fondo oscuro
      par(
        bg = "#1e1e24", col.axis = "#e0e0e0", col.lab = "#e0e0e0",
        col.main = "#fbbd24", fg = "#e0e0e0"
      )

      plot(nav_dates, nav_vec,
        type = "l", main = "Evolución del NAV",
        col = "#10b981", lwd = 3, ylab = "NAV ($)", xlab = "Fecha",
        las = 1
      )

      abline(h = CFG$capital_inicial, col = "#667eea", lty = 2, lwd = 2)
      text(nav_dates[length(nav_dates) %/% 2], CFG$capital_inicial,
        sprintf("Capital Inicial: $%s", format(CFG$capital_inicial, big.mark = ",")),
        pos = 3, col = "#a0a0a0", cex = 0.9
      )

      grid(col = "#2a2a3e", lty = 1)
    },
    bg = "#1e1e24"
  )

  output$dd_plot <- renderPlot(
    {
      req(rv$core_data)
      ret_vec <- as.numeric(rv$core_data$metrics$returns)
      cum_ret <- cumprod(1 + ret_vec)
      running_max <- cummax(cum_ret)
      dd <- (cum_ret - running_max) / running_max

      # Configurar fondo oscuro
      par(
        bg = "#1e1e24", col.axis = "#e0e0e0", col.lab = "#e0e0e0",
        col.main = "#fbbd24", fg = "#e0e0e0"
      )

      plot(index(rv$core_data$metrics$returns), dd,
        type = "l",
        main = "Drawdown del Portafolio", col = "#ef4444", lwd = 3,
        ylab = "Drawdown", xlab = "Fecha", las = 1
      )
      abline(h = -CFG$max_dd_tolerance, col = "#dc2626", lty = 2, lwd = 2)
      text(index(rv$core_data$metrics$returns)[length(dd) %/% 2], -CFG$max_dd_tolerance,
        sprintf("Circuit Breaker: %.0f%%", CFG$max_dd_tolerance * 100),
        pos = 3, col = "#fca5a5", cex = 0.9
      )
      grid(col = "#2a2a3e", lty = 1)
    },
    bg = "#1e1e24"
  )

  output$rolling_ret_plot <- renderPlot(
    {
      req(rv$core_data)
      ret_vec <- as.numeric(rv$core_data$metrics$returns)

      roll_ret <- zoo::rollapply(ret_vec,
        width = 21, FUN = function(x) prod(1 + x) - 1,
        fill = NA, align = "right"
      )

      # Configurar fondo oscuro
      par(
        bg = "#1e1e24", col.axis = "#e0e0e0", col.lab = "#e0e0e0",
        col.main = "#fbbd24", fg = "#e0e0e0"
      )

      plot(index(rv$core_data$metrics$returns), roll_ret * 100,
        type = "l",
        main = "Retorno Rolling 21 días", col = "#10b981", lwd = 3,
        ylab = "Retorno (%)", xlab = "Fecha", las = 1
      )
      abline(h = 0, col = "#ef4444", lty = 2, lwd = 2)
      grid(col = "#2a2a3e", lty = 1)
    },
    bg = "#1e1e24"
  )

  output$performance_table <- renderDT({
    req(rv$core_data)
    stats <- calc_robust_stats(rv$core_data$metrics$returns)
    df <- data.frame(
      Métrica = c(
        "Retorno Acumulado", "Retorno Anualizado", "Volatilidad Anual",
        "Sharpe Ratio", "Sortino Ratio", "Max Drawdown", "Calmar Ratio", "Win Rate"
      ),
      Valor = c(
        sprintf("%.2f%%", Return.cumulative(rv$core_data$metrics$returns) * 100),
        sprintf("%.2f%%", as.numeric(stats$annual_ret) * 100),
        sprintf("%.2f%%", as.numeric(stats$volatility) * 100),
        sprintf("%.3f", as.numeric(stats$sharpe)),
        sprintf("%.3f", as.numeric(stats$sortino)),
        sprintf("%.2f%%", as.numeric(stats$maxdd) * 100),
        sprintf("%.3f", as.numeric(stats$calmar)),
        sprintf("%.2f%%", as.numeric(stats$win_rate) * 100)
      )
    )
    datatable(df, options = list(dom = "t", pageLength = 20), rownames = FALSE)
  })

  output$signals_table <- renderDT({
    req(rv$core_data)
    sig_df <- as.data.frame(tail(rv$core_data$signals, 50))
    sig_df$Fecha <- rownames(sig_df)
    sig_df <- sig_df[, c("Fecha", setdiff(names(sig_df), "Fecha"))]

    # Convertir valores numéricos a texto
    for (col in names(sig_df)[-1]) {
      sig_df[[col]] <- sapply(sig_df[[col]], function(x) {
        if (is.na(x) || x == 0) {
          return("HOLD")
        } else if (x > 0) {
          return("BUY")
        } else {
          return("SELL")
        }
      })
    }

    # Crear tabla con estilos
    dt <- datatable(sig_df,
      options = list(pageLength = 25, scrollX = TRUE),
      class = "cell-border stripe"
    )

    # Aplicar colores a cada columna
    for (col in names(sig_df)[-1]) {
      dt <- dt %>% formatStyle(
        col,
        backgroundColor = styleEqual(
          c("BUY", "SELL", "HOLD"),
          c("#10b981", "#ef4444", "#6b7280")
        ),
        color = "white",
        fontWeight = "600",
        textAlign = "center"
      )
    }

    return(dt)
  })

  output$signals_dist <- renderPlot(
    {
      req(rv$core_data)
      sig_counts <- colSums(abs(rv$core_data$signals) > 0)
      if (sum(sig_counts) > 0) {
        # Configurar fondo oscuro
        par(
          bg = "#1e1e24", col.axis = "#e0e0e0", col.lab = "#e0e0e0",
          col.main = "#fbbd24", fg = "#e0e0e0"
        )

        barplot(sig_counts,
          main = "Número de Señales por Activo",
          col = "#667eea", las = 2, ylab = "Señales Totales",
          border = "#fbbd24"
        )
        grid(col = "#2a2a3e", lty = 1)
      }
    },
    bg = "#1e1e24"
  )

  output$confidence_plot <- renderPlot(
    {
      req(rv$core_data)

      conf_data <- NULL

      if (!is.null(rv$core_data$confidence)) {
        conf_data <- rv$core_data$confidence
      } else if (!is.null(rv$core_data$metrics$confidence)) {
        conf_data <- rv$core_data$metrics$confidence
      }

      if (!is.null(conf_data) && nrow(conf_data) > 0) {
        conf_recent <- tail(conf_data, 100)
        conf_avg <- colMeans(abs(conf_recent), na.rm = TRUE)

        conf_avg <- conf_avg[!is.na(conf_avg)]

        if (length(conf_avg) > 0) {
          # Configurar fondo oscuro
          par(
            bg = "#1e1e24", col.axis = "#e0e0e0", col.lab = "#e0e0e0",
            col.main = "#fbbd24", fg = "#e0e0e0"
          )

          colors <- ifelse(conf_avg >= CFG$confidence_threshold, "#10b981",
            ifelse(conf_avg >= 0.4, "#fbbd24", "#ef4444")
          )

          barplot(conf_avg,
            main = "Confianza Promedio por Activo (últimos 100 días)",
            col = colors, las = 2, ylab = "Confianza Promedio",
            ylim = c(0, max(1, max(conf_avg, na.rm = TRUE) * 1.1)),
            border = "#2a2a3e"
          )

          abline(h = CFG$confidence_threshold, col = "#dc2626", lty = 2, lwd = 2)
          text(length(conf_avg) / 2, CFG$confidence_threshold,
            sprintf("Umbral: %.0f%%", CFG$confidence_threshold * 100),
            pos = 3, col = "#fca5a5", cex = 0.9
          )
          grid(col = "#2a2a3e", lty = 1)
          return()
        }
      }

      # Mensaje si no hay datos
      par(bg = "#1e1e24", col = "#e0e0e0")
      plot(1,
        type = "n", xlab = "", ylab = "", xlim = c(0, 1), ylim = c(0, 1),
        main = "Datos de Confianza No Disponibles", axes = FALSE
      )
      text(0.5, 0.5, "Los datos de confianza no están disponibles.\nEjecuta un análisis completo para generarlos.",
        cex = 1.2, col = "#a0a0a0"
      )
      box(col = "#2a2a3e")
    },
    bg = "#1e1e24"
  )

  output$positions_table <- renderDT({
    req(rv$core_data)
    if (!is.null(rv$core_data$metrics$positions)) {
      pos_df <- as.data.frame(tail(rv$core_data$metrics$positions, 20))
      pos_df$Fecha <- rownames(pos_df)
      pos_df <- pos_df[, c("Fecha", setdiff(names(pos_df), "Fecha"))]

      dt <- datatable(pos_df, options = list(pageLength = 20, scrollX = TRUE)) %>%
        formatPercentage(2:ncol(pos_df), 2)

      # Aplicar colores según valor positivo/negativo
      for (col in names(pos_df)[-1]) {
        dt <- dt %>% formatStyle(
          col,
          backgroundColor = styleInterval(
            c(-0.001, 0.001),
            c("#ef4444", "#6b7280", "#10b981")
          ),
          color = "white"
        )
      }

      return(dt)
    }
  })

  output$exposure_plot <- renderPlot(
    {
      req(rv$core_data)
      if (!is.null(rv$core_data$metrics$positions)) {
        pos_latest <- as.numeric(tail(rv$core_data$metrics$positions, 1))
        names(pos_latest) <- colnames(rv$core_data$metrics$positions)
        pos_latest <- pos_latest[pos_latest != 0]
        if (length(pos_latest) > 0) {
          # Configurar fondo oscuro
          par(
            bg = "#1e1e24", col.axis = "#e0e0e0", col.lab = "#e0e0e0",
            col.main = "#fbbd24", fg = "#e0e0e0"
          )

          barplot(pos_latest * 100,
            main = "Exposición Actual (%)",
            col = ifelse(pos_latest > 0, "#10b981", "#ef4444"),
            las = 2, ylab = "%", border = "#2a2a3e"
          )
          abline(h = 0, lty = 2, col = "#667eea", lwd = 2)
          grid(col = "#2a2a3e", lty = 1)
        }
      }
    },
    bg = "#1e1e24"
  )

  # Cargar mapa de símbolos
  symbol_map <- reactiveVal({
    if (file.exists("symbol_map.csv")) {
      read.csv("symbol_map.csv", stringsAsFactors = FALSE)
    } else {
      data.frame(SystemSymbol = CFG$symbols, MT5Symbol = CFG$symbols, stringsAsFactors = FALSE)
    }
  })

  output$symbol_map_table <- renderDT({
    datatable(symbol_map(),
      editable = list(target = "cell", disable = list(columns = c(0))),
      options = list(pageLength = 15, dom = "t"),
      rownames = FALSE
    )
  })

  observeEvent(input$symbol_map_table_cell_edit, {
    info <- input$symbol_map_table_cell_edit
    temp_map <- symbol_map()
    temp_map[info$row, info$col + 1] <- info$value # +1 porque DT es 0-indexed en col
    symbol_map(temp_map)
  })

  observeEvent(input$save_map, {
    write.csv(symbol_map(), "symbol_map.csv", row.names = FALSE)
    showNotification("Mapa de símbolos guardado correctamente", type = "message")
  })

  observeEvent(input$run_analysis, {
    rv$is_running <- TRUE
    showModal(modalDialog("Ejecutando análisis... esto puede tardar varios minutos",
      footer = NULL, easyClose = FALSE
    ))

    tryCatch(
      {
        # Pasar mapa de símbolos a descargar_retornos si es necesario (si implementamos lógica de mapeo allí también)
        # Por ahora, descargar_retornos usa get_historical_data que ya soporta el mapa
        # Necesitamos pasar el mapa actual
        current_map <- symbol_map()

        # Modificar descargar_retornos para aceptar symbol_map si no lo hace (lo haremos dinámicamente o pasando el argumento)
        # Nota: descargar_retornos llama a get_historical_data. Debemos actualizar descargar_retornos para pasar el mapa.
        # Como descargar_retornos está fuera del server, le pasaremos el mapa como argumento.

        retornos <- descargar_retornos(CFG$symbols, CFG$fecha_inicio, verbose = TRUE, source = input$data_source, symbol_map = current_map)
        wf <- entrenar_walkforward(retornos, cfg = CFG, verbose = TRUE)
        metrics <- simulate_portfolio(retornos, wf$signals, wf$confidence, cfg = CFG)
        generar_reportes(wf$models, wf$signals, metrics, retornos, outdir = CFG$outdir)

        # ==================== EJECUCIÓN AUTOMÁTICA MT5 ====================
        if (isTRUE(input$auto_trading)) {
          showNotification("Conectando con MT5 para ejecución automática...", type = "warning", duration = 5)

          # Ejecutar en un bloque try-catch separado para no detener el flujo si falla
          tryCatch(
            {
              res_mt5 <- execute_auto_trading(wf$signals, wf$confidence, CFG$capital_inicial, CFG, symbol_map = current_map)
              if (res_mt5$success) {
                showNotification(paste("MT5:", res_mt5$message), type = "message", duration = 10)
                log_msg(paste("AUTO-TRADING MT5:", res_mt5$message))
              } else {
                showNotification(paste("Error MT5:", res_mt5$message), type = "error", duration = 10)
                log_msg(paste("ERROR AUTO-TRADING MT5:", res_mt5$message), level = "ERROR")
              }
            },
            error = function(e) {
              showNotification(paste("Fallo crítico en módulo MT5:", e$message), type = "error", duration = 10)
              log_msg(paste("CRITICAL MT5 ERROR:", e$message), level = "ERROR")
            }
          )
        }
        # ==================================================================

        rv$core_data <- list(
          models = wf$models,
          signals = wf$signals,
          metrics = metrics,
          confidence = metrics$confidence
        )
        rv$last_update <- Sys.time()
        rv$is_running <- FALSE

        removeModal()
        showModal(modalDialog(
          title = "✓ Análisis Completado",
          sprintf(
            "NAV Final: $%s\nRetorno: %.2f%%\nSharpe: %.2f",
            format(metrics$final_nav, big.mark = ","),
            (metrics$final_nav / CFG$capital_inicial - 1) * 100,
            as.numeric(SharpeRatio.annualized(metrics$returns))
          ),
          easyClose = TRUE, footer = modalButton("Cerrar")
        ))
      },
      error = function(e) {
        rv$is_running <- FALSE
        removeModal()
        showModal(modalDialog(
          title = "Error", paste("Error:", e$message),
          easyClose = TRUE, footer = modalButton("Cerrar")
        ))
      }
    )
  })

  observeEvent(input$refresh_data, {
    showNotification("Actualizando datos...", type = "message")
    tryCatch(
      {
        current_map <- symbol_map()
        retornos <- descargar_retornos(CFG$symbols, CFG$fecha_inicio, verbose = FALSE, source = input$data_source, symbol_map = current_map)
        showNotification("✓ Datos actualizados", type = "message")
      },
      error = function(e) {
        showNotification(paste("Error:", e$message), type = "error")
      }
    )
  })
}

# ==================== FUNCIÓN PRINCIPAL ===========================================
ejecutar_pro_v6 <- function(run_shiny = FALSE, verbose = CFG$verbose) {
  CFG$verbose <<- verbose
  outdir <- CFG$outdir
  dir.create(outdir, showWarnings = FALSE, recursive = TRUE)

  log_msg(paste(rep("=", 70), collapse = ""))
  log_msg("SISTEMA ETF PRO v6.0 - LLAMA IA")
  log_msg(paste(rep("=", 70), collapse = ""))
  log_msg("Fecha:", Sys.time())
  log_msg("Activos:", paste(CFG$symbols, collapse = ", "))
  log_msg("Capital: $", format(CFG$capital_inicial, big.mark = ","))

  if (run_shiny) {
    log_msg("Iniciando dashboard Shiny...")
    # Configurar ruta de recursos estáticos para imágenes
    shiny::addResourcePath("imagenes", file.path(getwd(), "imagenes"))
    shiny::runApp(list(ui = ui_shiny_v6(), server = server_shiny_v6), launch.browser = TRUE)
    return(invisible(NULL))
  }

  tryCatch(
    {
      log_msg("\n[1/4] Descargando datos...")
      retornos_xts <- descargar_retornos(CFG$symbols, CFG$fecha_inicio, verbose)

      log_msg("\n[2/4] Entrenando modelos Walk-Forward...")
      wf <- entrenar_walkforward(retornos_xts, cfg = CFG, verbose = verbose)

      log_msg("\n[3/4] Simulando estrategia...")
      metrics <- simulate_portfolio(retornos_xts, wf$signals, wf$confidence, cfg = CFG)

      log_msg("\n[4/4] Generando reportes...")
      generar_reportes(wf$models, wf$signals, metrics, retornos_xts, outdir = outdir)

      log_msg("\n", paste(rep("=", 70), collapse = ""))
      log_msg("EJECUCIÓN COMPLETADA")
      log_msg(paste(rep("=", 70), collapse = ""))
      log_msg("NAV Final:", format(metrics$final_nav, big.mark = ","))
      log_msg("Retorno Total:", sprintf(
        "%.2f%%",
        (metrics$final_nav / CFG$capital_inicial - 1) * 100
      ))

      stats <- calc_robust_stats(metrics$returns)
      log_msg("Sharpe Ratio:", round(as.numeric(stats$sharpe), 3))
      log_msg("Max Drawdown:", sprintf("%.2f%%", as.numeric(stats$maxdd) * 100))
      log_msg("Trades:", metrics$n_trades)
      log_msg("\nReportes en:", outdir)

      if (as.numeric(stats$sharpe) < CFG$min_sharpe) {
        log_msg("\n⚠️ Sharpe bajo del umbral mínimo", level = "WARN")
      }

      if (abs(as.numeric(stats$maxdd)) > CFG$max_dd_tolerance) {
        log_msg("\n⚠️ Drawdown excede tolerancia", level = "WARN")
      }

      core_res <- list(
        models = wf$models, signals = wf$signals,
        confidence = wf$confidence, metrics = metrics, stats = stats
      )
      return(core_res)
    },
    error = function(e) {
      log_msg("\n", paste(rep("=", 70), collapse = ""), level = "ERROR")
      log_msg("ERROR CRÍTICO", level = "ERROR")
      log_msg(paste(rep("=", 70), collapse = ""), level = "ERROR")
      log_msg(e$message, level = "ERROR")
      print(e)
      stop(e)
    }
  )
}

# ==================== CLI ENTRY POINT =============================================
args <- commandArgs(trailingOnly = TRUE)

if (length(args) > 0 && "--run" %in% args) {
  cat("\n╔═══════════════════════════════════════════════════════╗\n")
  cat("║     SISTEMA CUANTITATIVO ETF PRO v6.0 LLAMA IA       ║\n")
  cat("║     Ejecución CLI                                    ║\n")
  cat("╚═══════════════════════════════════════════════════════╝\n\n")

  result <- tryCatch(
    {
      ejecutar_pro_v6(run_shiny = FALSE, verbose = TRUE)
    },
    error = function(e) {
      cat("\n❌ ERROR FATAL:\n", e$message, "\n")
      quit(status = 1)
    }
  )

  cat("\n✓ Ejecución finalizada\n")
  quit(status = 0)
}

if (!interactive() && length(args) == 0) {
  cat("\nUso:\n")
  cat("  Rscript sistema_etf_v6_pro.R --run\n\n")
  cat("Modo R interactivo:\n")
  cat("  source('sistema_etf_v6_pro.R')\n")
  cat("  ejecutar_pro_v6(run_shiny = FALSE)  # Sin UI\n")
  cat("  ejecutar_pro_v6(run_shiny = TRUE)   # Dashboard\n\n")
}

log_msg("Script v6.0 LLAMA IA cargado y listo")
cat("\n✓ Sistema ETF Pro v6.0 LLAMA IA cargado\n")
cat("  Ejecutar: ejecutar_pro_v6(run_shiny = TRUE/FALSE)\n\n")
