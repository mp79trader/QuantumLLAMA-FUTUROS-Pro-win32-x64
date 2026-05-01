# QuantumLLAMA-FUTUROS-Pro

![Version](https://img.shields.io/badge/version-6.0.0-blue.svg)
![License](https://img.shields.io/badge/license-Proprietary-red.svg)
![Platform](https://img.shields.io/badge/platform-win32--x64-lightgrey.svg)

## 🚀 Descripción del Proyecto
**QuantumLLAMA ETF Pro** es un sistema cuantitativo avanzado de trading diseñado para operar ETFs mediante el uso de Inteligencia Artificial y modelos de Machine Learning. El sistema combina una interfaz robusta desarrollada en **Electron** con un núcleo analítico de alto rendimiento programado en **R**.

Este repositorio contiene la arquitectura modular del sistema, permitiendo la ejecución de estrategias complejas, backtesting exhaustivo y visualización de datos en tiempo real.

## 🛠️ Tecnologías Utilizadas
- **Frontend/Shell:** Electron (Node.js)
- **Análisis Cuantitativo:** R Language
- **Modelos de ML:** Random Forest (Ranger), Gradient Boosting (Caret)
- **Visualización:** Shiny, Shinydashboard, DT
- **Conectividad:** Bridge con MetaTrader 5 (MT5)

## 📁 Estructura del Repositorio
```text
.
├── resources/app/          # Código fuente principal
│   ├── core/               # Lógica de negocio y configuración
│   ├── data/               # Almacenamiento de datos históricos
│   ├── imagenes/           # Recursos visuales
│   ├── app.R               # Punto de entrada de la interfaz Shiny
│   ├── main.js             # Proceso principal de Electron
│   ├── main.R              # Script de arranque del motor R
│   └── package.json        # Dependencias de Node.js
├── setup_env.sh            # Script de configuración (Linux/macOS)
├── setup_env.bat           # Script de configuración (Windows)
└── .gitignore              # Archivos ignorados por Git
```

## ⚙️ Instalación y Configuración

### Requisitos Previos
1. **Node.js** (v18 o superior recomendado)
2. **R** (v4.0 o superior)
3. **MetaTrader 5** (para trading en vivo)

### Configuración Automática
Para configurar el entorno de desarrollo, ejecute el script correspondiente a su sistema operativo:

**Windows:**
```cmd
setup_env.bat
```

**Linux/macOS:**
```bash
chmod +x setup_env.sh
./setup_env.sh
```

### Scripts Disponibles
En el directorio `resources/app/`:
- `npm start`: Inicia la aplicación en modo desarrollo.
- `npm run build`: Empaqueta la aplicación para distribución.

## 📊 Estrategia y Modelos
El sistema utiliza una combinación de indicadores técnicos y modelos de aprendizaje supervisado para predecir movimientos en una cesta de ETFs seleccionados:
- **Activos:** VTI, VIG, VNQ, BND, GLD, SPY, IEF, VCIT, XLK, TLT.
- **Gestión de Riesgo:** Umbrales de volatilidad, Stop Loss dinámico y Trailing Stop.

## ⚖️ Licencia
Este software es propiedad de **NBM Sistemas**. Consulte el archivo `LICENSE.txt` en `resources/app/` para más detalles sobre los términos de uso.

---
Desarrollado con ❤️ por [NBM Sistemas](https://github.com/nbmsistemas).
