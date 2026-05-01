const { app, BrowserWindow } = require('electron');
const { spawn } = require('child_process');
const path = require('path');
const http = require('http');
const fs = require('fs');

let mainWindow;
let rProcess;
const SHINY_PORT = 3838;

// Habilitar logging
console.log('=== QuantumLLAMA ETF Pro - Electron ===');
console.log('App Path:', app.getAppPath());
console.log('User Data:', app.getPath('userData'));

// Función para verificar si el puerto está disponible
function checkPort(port, callback) {
  const server = http.createServer();
  server.once('error', (err) => {
    if (err.code === 'EADDRINUSE') {
      callback(false);
    }
  });
  server.once('listening', () => {
    server.close();
    callback(true);
  });
  server.listen(port);
}

// Función mejorada para encontrar R en el sistema
function findRExecutable() {
  console.log('Buscando instalación de R...');

  // Posibles ubicaciones de R
  const possiblePaths = [
    'C:\\Program Files\\R\\R-4.5.0\\bin\\x64\\Rscript.exe',
    'C:\\Program Files\\R\\R-4.5.0\\bin\\Rscript.exe',
    'C:\\Program Files\\R\\R-4.4.0\\bin\\x64\\Rscript.exe',
    'C:\\Program Files\\R\\R-4.4.0\\bin\\Rscript.exe',
    'C:\\Program Files\\R\\R-4.3.0\\bin\\x64\\Rscript.exe',
    'C:\\Program Files\\R\\R-4.3.0\\bin\\Rscript.exe',
  ];

  // Buscar en todas las versiones de R instaladas
  const rBaseDir = 'C:\\Program Files\\R';
  if (fs.existsSync(rBaseDir)) {
    const rVersions = fs.readdirSync(rBaseDir);
    console.log('Versiones de R encontradas:', rVersions);

    for (const version of rVersions) {
      const rscriptPath64 = path.join(rBaseDir, version, 'bin', 'x64', 'Rscript.exe');
      const rscriptPath32 = path.join(rBaseDir, version, 'bin', 'Rscript.exe');

      if (fs.existsSync(rscriptPath64)) {
        console.log('✓ R encontrado:', rscriptPath64);
        return rscriptPath64;
      }
      if (fs.existsSync(rscriptPath32)) {
        console.log('✓ R encontrado:', rscriptPath32);
        return rscriptPath32;
      }
    }
  }

  // Verificar paths predefinidos
  for (const rPath of possiblePaths) {
    if (fs.existsSync(rPath)) {
      console.log('✓ R encontrado:', rPath);
      return rPath;
    }
  }

  // Intentar usar Rscript del PATH
  console.log('⚠ R no encontrado en ubicaciones estándar, intentando PATH...');
  return 'Rscript.exe';
}

// Iniciar el servidor Shiny
function startShinyServer() {
  return new Promise((resolve, reject) => {
    const rScript = findRExecutable();
    const appDir = app.getAppPath();

    console.log('=== Iniciando Servidor Shiny ===');
    console.log('R executable:', rScript);
    console.log('App directory:', appDir);

    // Verificar que main.R existe
    const mainRPath = path.join(appDir, 'main.R');
    if (!fs.existsSync(mainRPath)) {
      const error = `ERROR: No se encontró main.R en ${mainRPath}`;
      console.error(error);
      reject(new Error(error));
      return;
    }

    console.log('✓ main.R encontrado');

    // Script R para lanzar la app
    const launchScript = `
      # Configurar para Electron (sin abrir navegador)
      options(shiny.launch.browser = FALSE)
      options(shiny.port = ${SHINY_PORT})
      options(shiny.host = "127.0.0.1")
      
      setwd("${appDir.replace(/\\/g, '/')}")
      cat("Working directory:", getwd(), "\\n")
      
      # Verificar que main.R existe
      if (!file.exists("main.R")) {
        stop("ERROR: main.R no encontrado")
      }
      
      cat("Cargando main.R...\\n")
      source("main.R", encoding = "UTF-8")
      
      cat("Configurando Shiny para Electron...\\n")
      
      # Modificar temporalmente la función ejecutar_pro_v6 para NO abrir navegador
      ejecutar_pro_v6_electron <- function() {
        # Configurar ruta de recursos estáticos para imágenes
        shiny::addResourcePath("imagenes", file.path(getwd(), "imagenes"))
        
        # Lanzar Shiny SIN abrir navegador
        cat("Iniciando servidor Shiny en puerto ${SHINY_PORT}...\\n")
        shiny::runApp(
          list(ui = ui_shiny_v6(), server = server_shiny_v6),
          port = ${SHINY_PORT},
          host = "127.0.0.1",
          launch.browser = FALSE,  # CRÍTICO: No abrir navegador
          quiet = FALSE
        )
      }
      
      cat("Lanzando aplicación Shiny...\\n")
      ejecutar_pro_v6_electron()
    `;

    console.log('Ejecutando script R...');

    rProcess = spawn(rScript, ['-e', launchScript], {
      cwd: appDir,
      stdio: ['pipe', 'pipe', 'pipe']
    });

    let hasResolved = false;

    rProcess.stdout.on('data', (data) => {
      const output = data.toString();
      console.log('[R stdout]', output);

      if (output.includes('Listening on') && !hasResolved) {
        console.log('✓ Servidor Shiny iniciado correctamente');
        hasResolved = true;
        resolve();
      }
    });

    rProcess.stderr.on('data', (data) => {
      const error = data.toString();
      console.error('[R stderr]', error);

      // Algunos mensajes de stderr son solo warnings, no errores fatales
      if (error.includes('Error') && !hasResolved) {
        hasResolved = true;
        reject(new Error(error));
      }
    });

    rProcess.on('error', (error) => {
      console.error('Error al iniciar proceso R:', error);
      if (!hasResolved) {
        hasResolved = true;
        reject(error);
      }
    });

    rProcess.on('exit', (code) => {
      console.log('Proceso R terminó con código:', code);
      if (code !== 0 && !hasResolved) {
        hasResolved = true;
        reject(new Error(`R process exited with code ${code}`));
      }
    });

    // Timeout de 20 segundos
    setTimeout(() => {
      if (!hasResolved) {
        console.log('⚠ Timeout alcanzado, asumiendo que Shiny está listo...');
        hasResolved = true;
        resolve();
      }
    }, 20000);
  });
}

// Crear ventana principal
function createWindow() {
  console.log('Creando ventana principal...');

  mainWindow = new BrowserWindow({
    width: 1400,
    height: 900,
    backgroundColor: '#1e3c72', // Color de fondo para evitar pantalla blanca
    show: false, // No mostrar hasta que esté listo
    icon: path.join(__dirname, 'build', 'icon.png'),
    webPreferences: {
      nodeIntegration: false,
      contextIsolation: true,
      devTools: false // Deshabilitar DevTools en producción
    },
    title: 'QuantumLLAMA ETF Pro v6.0'
  });

  // NO abrir DevTools automáticamente
  // mainWindow.webContents.openDevTools();

  // Pantalla de carga
  mainWindow.loadURL(`data:text/html;charset=utf-8,
    <html>
      <head>
        <style>
          body {
            margin: 0;
            padding: 0;
            display: flex;
            justify-content: center;
            align-items: center;
            height: 100vh;
            background: linear-gradient(135deg, #1e3c72 0%, #2a5298 100%);
            font-family: 'Segoe UI', Tahoma, Geneva, Verdana, sans-serif;
            color: white;
          }
          .loader {
            text-align: center;
          }
          .spinner {
            border: 8px solid rgba(255, 255, 255, 0.3);
            border-top: 8px solid #00d4ff;
            border-radius: 50%;
            width: 80px;
            height: 80px;
            animation: spin 1s linear infinite;
            margin: 0 auto 30px;
          }
          @keyframes spin {
            0% { transform: rotate(0deg); }
            100% { transform: rotate(360deg); }
          }
          h1 {
            font-size: 32px;
            margin-bottom: 10px;
            background: linear-gradient(135deg, #00d4ff 0%, #fbbd24 100%);
            -webkit-background-clip: text;
            -webkit-text-fill-color: transparent;
            font-weight: bold;
          }
          p {
            font-size: 18px;
            opacity: 0.9;
          }
          .status {
            margin-top: 20px;
            font-size: 16px;
            opacity: 0.9;
            animation: pulse 2s ease-in-out infinite;
          }
          @keyframes pulse {
            0%, 100% { opacity: 0.6; }
            50% { opacity: 1; }
          }
          .progress-bar {
            width: 300px;
            height: 6px;
            background: rgba(255, 255, 255, 0.2);
            border-radius: 10px;
            margin: 25px auto 15px;
            overflow: hidden;
          }
          .progress-fill {
            height: 100%;
            background: linear-gradient(90deg, #00d4ff 0%, #fbbd24 100%);
            border-radius: 10px;
            animation: loading 2s ease-in-out infinite;
            width: 70%;
          }
          @keyframes loading {
            0% { transform: translateX(-100%); }
            100% { transform: translateX(200%); }
          }
        </style>
      </head>
      <body>
        <div class="loader">
          <div class="spinner"></div>
          <h1>QuantumLLAMA ETF Pro</h1>
          <p style="font-size: 18px; margin: 10px 0;">Sistema Cuantitativo de Trading</p>
          
          <div class="progress-bar">
            <div class="progress-fill"></div>
          </div>
          
          <p class="status">Iniciando sistema, por favor espera...</p>
          <p style="margin-top: 30px; font-size: 12px; opacity: 0.5;">
            Esto puede tomar 15-30 segundos
          </p>
        </div>
      </body>
    </html>
  `);

  console.log('Pantalla de carga mostrada');

  // Mostrar ventana cuando el contenido esté listo
  mainWindow.once('ready-to-show', () => {
    mainWindow.show();
    console.log('Ventana mostrada');
  });

  // Iniciar Shiny y luego cargar la app
  startShinyServer()
    .then(() => {
      console.log('Esperando 2 segundos antes de cargar la app...');
      setTimeout(() => {
        const shinyUrl = `http://127.0.0.1:${SHINY_PORT}`;
        console.log('Cargando app Shiny desde:', shinyUrl);
        mainWindow.loadURL(shinyUrl);
      }, 2000);
    })
    .catch((error) => {
      console.error('ERROR FATAL al iniciar Shiny:', error);
      mainWindow.loadURL(`data:text/html;charset=utf-8,
        <html>
          <body style="font-family: Arial; padding: 50px; background: #f44336; color: white;">
            <h1>Error al iniciar la aplicación</h1>
            <p>No se pudo iniciar el servidor R Shiny.</p>
            <h3>Posibles causas:</h3>
            <ul>
              <li>R no está instalado en tu sistema</li>
              <li>Los paquetes de R necesarios no están instalados</li>
              <li>El archivo main.R no se encuentra</li>
            </ul>
            <h3>Detalles del error:</h3>
            <pre style="background: rgba(0,0,0,0.3); padding: 15px; border-radius: 5px; overflow: auto;">
${error.message}
            </pre>
            <p style="margin-top: 30px;">
              <strong>Solución:</strong> Asegúrate de tener R instalado y ejecuta la app desde RStudio primero para verificar que funciona.
            </p>
          </body>
        </html>
      `);
    });

  mainWindow.on('closed', () => {
    mainWindow = null;
  });
}

// Cuando Electron esté listo
app.whenReady().then(() => {
  console.log('Electron listo, creando ventana...');
  createWindow();
});

// Cerrar cuando todas las ventanas se cierren
app.on('window-all-closed', () => {
  console.log('Todas las ventanas cerradas');
  if (rProcess) {
    console.log('Terminando proceso R...');
    rProcess.kill();
  }
  app.quit();
});

app.on('activate', () => {
  if (mainWindow === null) {
    createWindow();
  }
});

// Manejar cierre de la app
app.on('before-quit', () => {
  console.log('Aplicación cerrándose...');
  if (rProcess) {
    rProcess.kill();
  }
});
