# Script auxiliar para descargar y guardar datos iniciales
# Modificado para funcionar standalone en CI/CD

# Crear directorio data si no existe
if (!dir.exists("data")) dir.create("data")
if (!dir.exists("core")) dir.create("core")

# Definir símbolos directamente para evitar dependencia de config.R complejo en CI
symbols <- c("VTI", "VIG", "VNQ", "BND", "GLD", "SPY", "IEF", "VCIT", "XLK", "TLT")
fecha_inicio <- as.Date("2015-01-01")

message("=== Iniciando Descarga Automática de Datos ===")
message("Fecha: ", Sys.Date())
message("Activos: ", paste(symbols, collapse = ", "))

library(quantmod)
library(tidyquant)
library(xts)

datos <- list()

for (s in symbols) {
    tryCatch(
        {
            message(paste("Descargando", s, "..."))
            # Intentar con getSymbols (Yahoo)
            # GitHub Actions IPs suelen funcionar bien con Yahoo
            tmp <- getSymbols(s, src = "yahoo", from = fecha_inicio, auto.assign = FALSE, warnings = FALSE)

            # IMPORTANTE: Calcular retornos LOGARÍTMICOS como espera el sistema
            ret_log <- periodReturn(Ad(tmp), period = "daily", type = "log")
            colnames(ret_log) <- "retornos"

            datos[[s]] <- ret_log
            message(paste("✓ OK:", s, nrow(ret_log), "filas, Último:", end(ret_log)))
        },
        error = function(e) {
            message(paste("✗ ERROR descargando", s, ":", e$message))
        }
    )
}

if (length(datos) == length(symbols)) {
    # Unir todos
    retornos_xts <- do.call(merge, datos)
    colnames(retornos_xts) <- names(datos)
    retornos_xts <- na.omit(retornos_xts)

    # Guardar en la ruta esperada por la app
    saveRDS(retornos_xts, "data/datos_historicos_fallback.rds")
    message("✓ ACTUALIZACIÓN EXITOSA: Archivo guardado con ", nrow(retornos_xts), " filas.")
} else {
    message("⚠ ADVERTENCIA: No se descargaron todos los activos. Se descargaron: ", length(datos), " de ", length(symbols))
    if (length(datos) > 0) {
        # Guardar lo que haya, mejor que nada, pero idealmente queremos todo
        retornos_xts <- do.call(merge, datos)
        colnames(retornos_xts) <- names(datos)
        saveRDS(retornos_xts, "data/datos_historicos_fallback.rds")
        message("✓ GUARDADO PARCIAL: Se guardaron datos parciales.")
    } else {
        stop("FALLO TOTAL: No se descargó ningún dato.")
    }
}
