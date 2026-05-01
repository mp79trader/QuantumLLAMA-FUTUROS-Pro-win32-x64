# ==================================================================================
# APP.R - Punto de entrada para Shiny en Render
# ==================================================================================
# Shiny requiere que exista app.R o server.R en el directorio raíz

# Cargar el sistema completo
source("main.R", encoding = "UTF-8")

# Shiny buscará automáticamente ui_shiny_v6() y server_shiny_v6()
# que ya están definidos en Estrategia-ETF-Pro.R

# Crear la aplicación Shiny
shinyApp(
    ui = ui_shiny_v6(),
    server = server_shiny_v6
)
