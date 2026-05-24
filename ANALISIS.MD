# Análisis Detallado — BlueBird Flexibility Manager

## 1. Visión General

El proyecto **BlueBird Flexibility Manager** es un sistema de **demand-response** para el control de activos energéticos locales en sitios de socios europeos. Cada tipo de activo se denomina **piloto** (cámaras frigoríficas EWH, HVAC de edificios Montcada, obras hidráulicas Enlion, calefacción urbana). El sistema ejecuta **Control Predictivo por Modelo (MPC)** económico con cadencia configurable, escribe los puntos de consigna óptimos en una base de datos compartida y los publica al Building Management System (BMS) a través de un Knowledge Engine.

---

## 2. Arquitectura General

```
┌─────────────────┐   dynamics JSON    ┌──────────────────────┐
│  Digital Twin   │ ─────────────────► │                      │
│  (R/ctsmTMB)    │   shared volume    │  Flexibility         │
└─────────────────┘                    │  Controller          │
                                       │  (Julia/FlexOPTi)    │
┌─────────────────┐   DB forecasts     │                      │
│  Disturbances   │ ─────────────────► │  · lee dinámica      │
│  Forecast       │                    │  · lee previsiones    │
└─────────────────┘                    │  · obtiene precios    │
                                       │  · escribe setpoints  │
┌─────────────────┐   HTTP prices      │                      │
│  Trading        │ ─────────────────► │                      │
│  Manager (ext.) │                    └──────────────────────┘
└─────────────────┘                             │
                                                │ setpoints
┌─────────────────┐   shared DB        ┌────────▼─────────────┐
│  Control Room   │ ◄────────────────► │  Database (SQLite)   │
│  (dashboard)    │                    └──────────────────────┘
└─────────────────┘
```

### 2.1 Servicios Docker

| Servicio | Lenguaje | Puerto | Perfil | Rol |
|----------|----------|--------|--------|-----|
| `dummy-forecast` | Python 3.12 | 5000 | _(siempre)_ | Inicializa esquema BD; escribe previsiones sintéticas cada 15 min |
| `fc` | Julia 1.12 (FlexOPTi) | 8080 | _(siempre)_ | Bucle MPC — lee dinámica + previsiones + precios, escribe setpoints |
| `control-room` | Python 3.12 (Flask) | 9000 | _(siempre)_ | Dashboard de operador — monitoriza el sistema y KPIs |
| `dt-ewh` | R 4.4.2 (ctsmTMB) | — | `retrain` | Estimación de parámetros del Digital Twin (ejecución única) |
| `fc-sim` | Julia 1.12 | — | `sim` | Simulación MPC en bucle cerrado (demo) |
| Trading Manager | Externo | 9090 | — | Precios day-ahead de electricidad (ENTSOE) |

### 2.2 Redes y Volúmenes

- **Redes Docker:**
  - `flexibility-manager_default` — red interna entre servicios
  - `local_tm-net` — red externa para comunicación con el Trading Manager
- **Volúmenes compartidos:**
  - `db-data` → `/data/bb.db` (base de datos SQLite compartida)
  - `dt-volume` → intercambio de modelos JSON entre Digital Twin y FC

### 2.3 Flujo de Datos

1. **`dummy-forecast` → BD:** Escribe previsiones de perturbaciones (temperatura ambiente, carga de puertas) cada 15 minutos.
2. **`fc` ← BD:** Lee mediciones (condiciones iniciales) y previsiones de perturbaciones.
3. **`fc` → BD:** Escribe setpoints óptimos de refrigeración.
4. **`dt-ewh` → `dt-volume`:** Escribe `cold_room_model_continuous.json` con parámetros estimados.
5. **`fc` ← `dt-volume`:** Lee el modelo dinámico del Digital Twin en cada iteración.
6. **`fc` → Trading Manager:** Obtiene precios day-ahead de electricidad vía HTTP.
7. **`control-room` ← BD / fc:** Lee estado del sistema para visualización.

---

## 3. Configuración del Sistema

### 3.1 Variables de Entorno (`.env`)

| Variable | Valor por defecto | Descripción |
|----------|-------------------|-------------|
| `DB_PATH` | `/data/bb.db` | Ruta de la base de datos SQLite |
| `PILOT` | `Ewh` | Piloto activo (`Ewh`, `Montcada`) |
| `SOLVER` | `HiGHS` | Solver de optimización MILP |
| `HU` | `48` | Horizonte de control (pasos) |
| `DELTA_T` | `900.0` | Paso temporal (segundos) → 15 min |
| `RETRAIN_EVERY_H` | `168` | Re-entrenamiento semanal del DT (horas) |
| `MIP_GAP` | `0.01` | Tolerancia de gap del solver (1%) |
| `HTTP_PORT` | `8080` | Puerto del servidor de estado del FC |
| `TM_BASE_URL` | `http://tm-service:9090` | URL del Trading Manager |
| `MARKET_COUNTRY` | `Germany` | País para consulta de precios |
| `CADENCE_S` | `900` | Intervalo de escritura de previsiones (s) |
| `HORIZON_STEPS` | `48` | Número de pasos por previsión |
| `T_AMBIENT` | `18.0` | Temperatura ambiente constante (°C) |
| `N_ROOMS` | `5` | Número de cámaras frigoríficas |

### 3.2 Settings del FC (`FlexOPTi/settings.json`)

```json
{
  "pilot":           "Ewh",
  "solver":          "HiGHS",
  "Hu":              48,
  "delta_t":         900.0,
  "retrain_every_h": 168,
  "market_country":  "Germany",
  "tm_base_url":     "http://tm-service:9090",
  "http_port":       8080,
  "mip_gap":         0.01,
  "milp_horizon":    1
}
```

- **`Hu=48`**: 48 pasos × 900s = **12 horas** de horizonte de optimización.
- **`milp_horizon=1`**: Solo el primer paso es binario (el resto se relaja a [0,1]).
- **`mip_gap=0.01`**: El solver se detiene al alcanzar un gap de optimalidad del 1%.

---

## 4. Base de Datos (`db/init.sql`)

### 4.1 Tablas Principales

#### `measurements` — Mediciones de sensores del BMS

| Columna | Tipo | Descripción |
|---------|------|-------------|
| `id` | INTEGER PK | Identificador |
| `timestamp` | TEXT | Marca temporal |
| `room_id` | INTEGER | ID de la cámara (1-5) |
| `T_air` | REAL | Temperatura del aire [°C] |
| `T_product` | REAL | Temperatura del producto [°C] |

#### `disturbance_forecasts` — Previsiones de perturbaciones

| Columna | Tipo | Descripción |
|---------|------|-------------|
| `id` | INTEGER PK | Identificador |
| `timestamp` | TEXT | Marca temporal |
| `room_id` | INTEGER | ID de la cámara (1-5) |
| `T_ambient` | REAL | Temperatura ambiente [°C] |
| `door_load` | REAL | Carga térmica de puerta [W] |

#### `setpoints` — Puntos de consigna del controlador

| Columna | Tipo | Descripción |
|---------|------|-------------|
| `id` | INTEGER PK | Identificador |
| `timestamp` | TEXT | Marca temporal |
| `room_id` | INTEGER | ID de la cámara (1-5) |
| `u` | REAL | Potencia de refrigeración [W] |
| `p3` | REAL | Potencia del congelador [W] |

#### Tablas adicionales de mercado

| Tabla | Propósito |
|-------|-----------|
| `assets` | Activos energéticos (con ubicación y tipo) |
| `price_tag` | Etiquetas de precio por país y tipo de mercado |
| `price` | Precios de electricidad [EUR/kWh] por timestamp |
| `weather` | Variables meteorológicas por activo |
| `forecasts` | Previsiones genéricas con intervalos de confianza (`yhat`, `yhat_lower`, `yhat_upper`) |

### 4.2 Datos Iniciales (Semilla)

Se pre-cargan 5 cámaras frigoríficas:

| Cámara | T_air (°C) | T_product (°C) | Uso |
|--------|------------|-----------------|-----|
| 1 | 4.5 | 4.8 | Lácteos |
| 2 | 2.1 | 2.3 | Productos terminados |
| 3 | 3.8 | 4.0 | Carne |
| 4 | 5.2 | 5.5 | Verduras |
| 5 | -18.3 | -18.0 | Congelador |

---

## 5. FlexOPTi — Controlador de Flexibilidad (Julia)

### 5.1 Estructura del Módulo

```
FlexOPTi/
├── Project.toml              # Dependencias Julia
├── Manifest.toml
├── settings.json             # Configuración operativa
├── Dockerfile
├── src/
│   ├── FlexOPTi.jl           # Módulo principal
│   ├── logging.jl            # Configuración de logs
│   ├── print/
│   │   └── print_output.jl   # Formateo de salida
│   ├── core/
│   │   ├── types.jl           # Definiciones de tipos
│   │   ├── parse_digital_twin.jl  # Parser del JSON del DT
│   │   ├── parse_sensors.jl       # Condiciones iniciales
│   │   ├── parse_forecasts.jl     # Previsiones de perturbaciones
│   │   ├── fetch_prices.jl        # Precios del Trading Manager
│   │   ├── build_constraints.jl   # Restricciones del MPC
│   │   ├── mpc_update.jl          # Optimización MPC
│   │   ├── mpc_helper.jl          # Funciones auxiliares
│   │   ├── fm_optimize.jl         # API principal: optimize()
│   │   ├── utils.jl               # Utilidades
│   │   └── parse_output.jl        # Parseo de resultados
│   └── pilots/
│       ├── ewh/                   # Implementación piloto EWH
│       │   ├── Ewh.jl
│       │   ├── parse_digital_twin.jl
│       │   ├── parse_forecasts.jl
│       │   ├── constraints.jl
│       │   ├── batch_mpc.jl
│       │   ├── mpc_types.jl
│       │   └── mpc_main.jl
│       └── montcada/              # Implementación piloto Montcada
│           └── Montcada.jl
├── scripts/
│   └── ewh/
│       ├── dr_controller.jl       # Bucle MPC principal (producción)
│       ├── sim_demo.jl            # Simulación en bucle cerrado
│       ├── sim_plot.jl            # Gráficos de simulación
│       ├── mpc_base.jl            # Demo MPC standalone
│       ├── dt.jl                  # Utilidades del DT
│       ├── ewh.jl                 # Utilidades EWH
│       └── mpc_plot.jl            # Utilidades de gráficos
├── data/
│   ├── EWH/inputs/                # Datos de entrada
│   └── cache/                     # Caché de precios
└── test/                          # Tests
```

### 5.2 Dependencias Principales

| Paquete | Uso |
|---------|-----|
| `JuMP` | Modelado de optimización matemática |
| `HiGHS` | Solver MILP (Mixed-Integer Linear Programming) |
| `HTTP` | Cliente HTTP (consultas al Trading Manager) |
| `JSON3` | Parseo de JSON |
| `SQLite` | Acceso a base de datos |
| `TimeZones` | Manejo de zonas horarias |
| `JLD2` | Serialización binaria de datos JuMP |
| `LoggingExtras` | Logging avanzado |

### 5.3 Sistema de Tipos (Multiple Dispatch)

```julia
abstract type AbstractBuilding end
abstract type AbstractHeatBuilding <: AbstractBuilding end
abstract type AbstractElecBuilding <: AbstractBuilding end

struct Montcada <: AbstractElecBuilding end
struct Ewh      <: AbstractElecBuilding end
struct Karno    <: AbstractHeatBuilding end
```

El piloto se pasa como string en el API (`"Ewh"`, `"Montcada"`) y se resuelve una vez a un tipo Julia. A partir de ahí, el **multiple dispatch** de Julia gestiona automáticamente la lógica específica de cada piloto sin usar comparaciones de strings.

### 5.4 Estructuras de Datos Clave

#### `O` — Opciones de configuración (mutable)

| Campo | Tipo | Descripción |
|-------|------|-------------|
| `Hu` | Int | Horizonte de control [pasos] |
| `Δt` | Float64 | Paso temporal [segundos] |
| `pilot` | AbstractBuilding | Tipo de piloto resuelto |
| `solver` | String | `"HiGHS"` o `"Gurobi"` |
| `mip_gap` | Float64 | Gap relativo MIP |
| `warm_start` | Bool | Reusar solución anterior |
| `milp_horizon` | Int | Pasos con variables binarias |
| `market_country` | String | País para precios |
| `tm_base_url` | String | URL del Trading Manager |

#### `OX` — Datos de entrada del MPC

| Campo | Tipo | Descripción |
|-------|------|-------------|
| `digital_twin` | Dict | Estructura del modelo + dinámica |
| `sensors` | Vector{Dict} | Condiciones iniciales (mediciones) |
| `forecast` | Dict | Predicciones de perturbaciones |
| `constraints` | Dict | Restricciones del sistema |
| `dynamics` | BatchDynamics | Espacio de estados discretizado |
| `prices` | Vector{Float64} | Precios day-ahead [EUR/Wh] |

#### `BatchDynamics` — Formulación batch del MPC

```
X = M·ξ₁ + Ξ·U + Ψ·Δ
```

| Campo | Descripción |
|-------|-------------|
| `M` | Matriz de mapeo de estados |
| `Ξ` | Matriz de mapeo de control |
| `Ψ` | Matriz de mapeo de perturbaciones |
| `ξ₁` | Vector de condiciones iniciales |
| `Δ` | Vector de perturbaciones apilado |

### 5.5 Función Principal: `optimize()`

```julia
function optimize(
    digital_twin_file::String,
    sensors_file::String,
    forecasts_file::String;
    pilot::String,
    Hu::Int = 1,
    solver::String = "HiGHS",
    ...
)::Dict{Symbol, Any}
```

**Flujo de ejecución:**

1. Carga configuración desde `settings.json`
2. Sobrescribe valores con los argumentos recibidos
3. Parsea JSON del Digital Twin (estructura del modelo + dinámica identificada)
4. Parsea JSON de sensores (mediciones actuales → condiciones iniciales `x₀`)
5. Parsea JSON de previsiones (temperatura ambiente, aperturas de puerta para `Hu` pasos)
6. Obtiene precios day-ahead del Trading Manager (o usa caché)
7. Construye dinámica batch mediante discretización (Zero-Order Hold a `Δt`)
8. Despacha a `mpc_update()` específico del piloto vía multiple dispatch
9. Retorna diccionario con: valor del objetivo [EUR], setpoints óptimos `u[1..Hu]`, estados predichos `x[1..Hu]`, estado del solver, metadatos computacionales

### 5.6 Obtención de Precios (`fetch_prices.jl`)

El sistema consulta precios de electricidad del Trading Manager con un mecanismo de fallback en cascada:

| Nivel de calidad | Fuente | Descripción |
|------------------|--------|-------------|
| `:live` | Trading Manager | Precios frescos obtenidos en tiempo real |
| `:cached` | Archivo local | TM inalcanzable; se usa caché en disco |
| `:fallback` | Caché anterior | Se usan precios del día anterior |
| `:unavailable` | Ceros | Sin datos; optimización no fiable |

**Funciones clave:**

- `_get_market_id(tm_base, country)` — Busca ID de mercado por país
- `_query_tm_prices(tm_base, market_id)` — Consulta precios vía REST
- `_snap_to_slot(dt)` — Cuantiza a slot de 15 minutos
- `_align_prices(prices_dict, t0, Hu, Δt)` — Construye vector de precios del horizonte
- Caché local en `FlexOPTi/data/cache/prices_<country>.json` (máximo 36 horas)

### 5.7 Piloto EWH — Cámaras Frigoríficas

**Descripción del sistema físico:**

- **5 cámaras:** lácteos, productos terminados, carne, verduras, congelador
- **2 compresores:** P1 (modulado, cámaras 1-4), P2 (on/off, apoyo)
- **1 compresor de congelador:** P3 (on/off, cámara 5)
- **Vector de estados:** 10 estados (T_air, T_product por cámara)
- **Vector de control:** `u ∈ [0, P1_max]` para cámaras 1-4; `u[5] ∈ {0, P3_max}` para congelador
- **Restricciones:** límites de setpoint (±ΔT del nominal), límites de potencia, tasas de rampa

### 5.8 Bucle Principal de Producción (`dr_controller.jl`)

```
INICIO
  │
  ├── Arrancar servidor HTTP de estado (:8080)
  ├── Conectar a BD SQLite
  ├── Verificar existencia del archivo del DT
  │
  └── BUCLE MPC (cada Δt_s segundos):
        │
        ├── 1. Leer sensores de BD → x₀
        ├── 2. Leer previsiones de BD → perturbaciones
        ├── 3. Obtener precios del Trading Manager (o caché)
        ├── 4. Llamar FlexOPTi.optimize(dt_file, sensors, forecasts, ...)
        ├── 5. Extraer u_first [1..N_ROOMS] del resultado
        ├── 6. Escribir setpoints en BD
        ├── 7. Comprobar temporizador de re-entrenamiento del DT
        │     └── Si toca: lanzar Rscript en subproceso
        └── 8. Actualizar estado del servidor HTTP
```

**Funciones de BD:**

| Función | Descripción |
|---------|-------------|
| `db_read_sensors(db, tmp_dir)` | SELECT último timestamp → vector `x₀` intercalado `[T_air_1, T_prod_1, ...]` |
| `db_read_forecasts(db, tmp_dir)` | SELECT `timestamp >= NOW()` → próximos `Hu` pasos de T_ambient y door_openings |
| `db_write_setpoints(db, u, t)` | INSERT setpoints por cámara; cámaras 1-4 usan `u`, cámara 5 usa `p3` |
| `dt_trigger_retrain()` | Lanza `Rscript` en background; copia output al volumen compartido |

---

## 6. Digital Twin (`dt/`)

### 6.1 EWH_coldrooms_CSTM — Estimación de Parámetros (R/ctsmTMB)

**Tecnología:** R 4.4.2 con librería `ctsmTMB` (Continuous-Time Stochastic Modelling using TMB).

**Modelo físico (por cámara):**

$$\frac{dT_{a,i}}{dt} = k_{1,i}(T_{amb} - T_{a,i}) + k_{2,i}(T_{p,i} - T_{a,i}) - k_3 \cdot u_i + k_{4,i} \cdot d_i \cdot 1000 + \sigma_a \cdot w_i$$

$$\frac{dT_{p,i}}{dt} = k_{5,i}(T_{a,i} - T_{p,i}) + \sigma_p \cdot w_{p,i}$$

Donde:

| Parámetro | Significado |
|-----------|-------------|
| $k_{1,i} = UA_i / C_{a,i}$ | Tasa de pérdida de calor por pared |
| $k_{2,i} = UA_{ap,i} / C_{a,i}$ | Tasa de acoplamiento aire-producto |
| $k_3 = COP / C_a$ | Eficiencia de refrigeración |
| $k_{4,i} = Q_{door} / (1000 \cdot C_a)$ | Efecto de carga de puerta |
| $k_{5,i} = UA_{ap,i} / C_{p,i}$ | Recuperación de calor del producto |
| $\sigma_a, \sigma_p$ | Ruido de proceso (estocástico) |
| $w_i, w_{p,i}$ | Procesos de Wiener (movimiento browniano) |

**Datos de entrenamiento:** `simulation_data_8day_noise_real_door_opening.csv` (8 días de simulación con ruido y aperturas de puerta reales).

**Proceso:**

1. **Preparación de datos:** Mapear observaciones (temperaturas), inputs (potencias de refrigeración) y perturbaciones (aperturas de puerta).
2. **Split train/test:** Primeras 164 horas para entrenamiento, resto para test.
3. **Definición del modelo ctsmTMB:** Ecuaciones diferenciales estocásticas para las 5 cámaras.
4. **Estimación:** Máxima verosimilitud de los parámetros.
5. **Salida:** JSON con matrices de espacio de estados continuo y discretizado.

**Salida JSON (`cold_room_model_continuous.json`):**

```json
{
  "state_space": {
    "A": [...],       // Matriz A continua
    "B": [...],       // Matriz B (input de control)
    "E": [...],       // Matriz E (perturbaciones)
    "x0": [...]       // Condiciones iniciales
  },
  "discretization": {
    "delta_t": 900.0, // Tiempo de muestreo [s]
    "Ad": [...],      // A discretizada
    "Bd": [...],      // B discretizada
    "Ed": [...]       // E discretizada
  },
  "parameters": {
    "k1": [...],      // Tasas de pérdida de calor [W/K]
    "k2": [...],      // Tasas de acoplamiento aire-producto
    "k3": 2.5,        // COP
    "k4": [...],      // Tasas de carga de puerta
    "k5": [...]       // Tasas de recuperación de calor
  }
}
```

### 6.2 EWH_coldrooms — Simulación Python

Simulador del sistema físico de las 5 cámaras frigoríficas usado para generar datos de entrenamiento del Digital Twin.

**Modelo de espacio de estados discreto:**

$$x[k+1] = A_d \cdot x[k] + B_d \cdot u[k] + E_d \cdot d[k]$$

Donde:
- $x = [T_1, T_2, T_3, T_4, T_5]$ — temperaturas de las cámaras
- $u = [u_1, u_2, u_3, u_4, u_5]$ — potencias de refrigeración
- $d = [T_{amb,1}, Q_{door,1}, T_{amb,2}, Q_{door,2}, ...]$ — perturbaciones

**Discretización exacta** vía exponencial de matriz:

$$M_d = e^{M \cdot \Delta t}, \quad M = \begin{bmatrix} A & B & E \\ 0 & 0 & 0 \\ 0 & 0 & 0 \end{bmatrix}$$

**Parámetros físicos:**

| Parámetro | Valor | Descripción |
|-----------|-------|-------------|
| `m` | [1200, 1000, 1500, 1100, 800] kg | Masas térmicas |
| `cv` | [1000, 1000, 1000, 1000, 900] J/kg·K | Calores específicos |
| `UA` | [40, 35, 50, 45, 38] W/K | Conductancias de pared |
| `COP` | 2.5 | Coeficiente de rendimiento |
| `P1_max` | 2000 W | Compresor primario |
| `P2_max` | 500 W | Compresor secundario |
| `P3_max` | 2000 W | Compresor del congelador |

**Lógica de control simulada:**

- **Cámaras 1-4:** Control proporcional con asignación de potencia. Si la demanda supera `P1_max`, se activa P2 y se reparte proporcionalmente.
- **Congelador:** Control por histéresis con banda de ±0.5°C.
- **Perturbaciones:** Aperturas de puerta modeladas como llegadas de Poisson.

### 6.3 EWH_EV_charging_BC7 — Carga de Vehículos Eléctricos (RL)

Agente de **Deep Q-Network (DQN)** para programación óptima de carga de vehículos eléctricos.

| Componente | Detalle |
|------------|---------|
| **Tecnología** | PyTorch 2.7.0 |
| **Agente** | `DQNAgent` |
| **Entorno** | `EVChargingEnv` |
| **Recompensa** | $-\text{PENALTY\_PER\_KWH} \times E_{charged} - \text{FIXED\_PENALTY}$ si infactible |
| **Acción** | Tasa de carga $[0, P_{max}]$ con $P_{max} = 11$ kW |
| **Episodios** | 50 |
| **Split datos** | 70% train, 30% test (por estación, cronológico) |

### 6.4 BlueBird-multizone-modelling — Edificio Montcada

Modelos de dinámica térmica para el piloto Montcada (edificio HVAC multizona):

- `DT_dynamics_estimator.R` — Estimador de dinámica
- `DT_training.R` — Entrenamiento del modelo
- `DT_predict.R` — Predicción
- `HourlyCalendarEnergyModel.R` — Modelo energético por calendario
- `HourlyHVACEnergyModel.R` — Modelo energético HVAC
- `HourlyTemperatureModel.R` — Modelo de temperatura

### 6.5 Enlion — Obras Hidráulicas

Modelo de **regresión lineal con lag-24** para dinámica de tanques de almacenamiento de agua:

$$\text{tank\_level}(t) = \sum_i w_i \cdot \text{feature}(t - \text{lag}_i) + \text{intercept}$$

**Proceso:**
1. Generación de datos sintéticos (`waterworks-simulated-data-generator.py`)
2. Entrenamiento del modelo (`prediction_linear_regression.py`)
3. Exportación de pesos a CSV (`linear_reg_lag24_weights.csv`)

---

## 7. Sistema de Previsiones (`forecasting/`)

### 7.1 Dummy Forecast Service

Servicio de previsiones sintéticas para desarrollo y pruebas.

**Comportamiento:**

- Inicializa el esquema de BD al arrancar (CREATE TABLE si no existe + datos semilla).
- Cada 15 minutos (configurable vía `CADENCE_S`), escribe 48 pasos de previsión (12h) en la tabla `disturbance_forecasts`.
- Todas las previsiones usan temperatura ambiente constante (18°C) y carga de puerta 0.
- Expone endpoint `/health` en puerto 5000.

### 7.2 API de Previsiones (NeuralForecast / Chronos)

API REST basada en **FastAPI** para inferencia de modelos de series temporales.

| Componente | Tecnología |
|------------|------------|
| Framework | FastAPI |
| Modelos | NeuralForecast 2.0.1, Amazon Chronos |
| ML Framework | PyTorch 2.2.0 (CPU) |
| Tracking | CodeCarbon (emisiones de CO₂) |

**Endpoint principal:**

```
POST /predict/
Body: { ds: [timestamps], unique_id: [ids], y: [values] }
Response: { yhat: [...], yhat_lower: [...], yhat_upper: [...], ds: [...], unique_id: [...] }
```

### 7.3 Aplicación Streamlit

Dashboard web para visualización de datos y ejecución de inferencia:

- **Página "View Data":** Visualización de datos históricos
- **Página "Inference":** Ejecución de predicciones en tiempo real

### 7.4 Prefect Pipelines

Pipelines de datos programados (Prefect 3.4.15) para:

- Entrenamiento de modelos en cadencia
- Ejecución de inferencia programada
- Generación de datos sintéticos

### 7.5 FAST Framework

Framework modular de previsiones organizado en 4 fases:

| Fase | Nombre | Propósito |
|------|--------|-----------|
| 1 | **Forge** | Preparación de datos |
| 2 | **Augur** | Entrenamiento de modelos |
| 3 | **Surveil** | Monitorización y detección de anomalías |
| 4 | **Translate** | Transformación de salida |

---

## 8. Control Room — Dashboard de Operador

### 8.1 Endpoints REST

| Método | Ruta | Descripción |
|--------|------|-------------|
| GET | `/` | Dashboard HTML con auto-refresco cada 15s |
| GET | `/health` | Health check del servicio |
| GET | `/status` | Estado del FC + últimos setpoints |
| GET | `/services` | Ping de salud de todos los servicios |
| GET | `/metrics` | KPIs (contadores de setpoints, previsiones, mediciones) |
| GET | `/setpoints` | Últimos N setpoints de la BD (JSON) |
| GET | `/forecasts` | Últimas N previsiones de la BD |
| POST | `/clear` | Limpiar tablas de la BD (con confirmación) |

### 8.2 Dashboard HTML

El dashboard muestra en tiempo real:

1. **Estado de servicios:** Semáforo de salud de `fc`, `dummy-forecast`, `control-room`
2. **Estado del FC:** Estado de optimización (OPTIMAL/INFEASIBLE), coste [EUR], setpoints actuales `u[1..5]`
3. **Setpoints recientes:** Tabla con los últimos 5 setpoints escritos
4. **KPIs:** Total de setpoints, previsiones y mediciones en BD
5. **Gestión de BD:** Botones para limpiar tablas individuales o toda la BD

---

## 9. API Summary — Endpoints por Servicio

| Servicio | Puerto | Endpoints |
|----------|--------|-----------|
| `dummy-forecast` | 5000 | `GET /health` |
| `fc` (FlexOPTi) | 8080 | `GET /health`, `GET /status` |
| `control-room` | 9000 | `GET /`, `/health`, `/status`, `/services`, `/metrics`, `/setpoints`, `/forecasts`, `POST /clear` |
| Trading Manager (ext.) | 9090 | `GET /api/market`, `GET /api/market/{id}/offerinfo`, `GET /api/offer/{id}` |

---

## 10. Despliegue

### Comandos Docker

```bash
# Construir y arrancar servicios principales
docker compose -f docker-compose.ewh.yml up --build

# Arrancar en background
docker compose -f docker-compose.ewh.yml up --build -d

# Re-entrenamiento del Digital Twin (ejecución única)
docker compose -f docker-compose.ewh.yml --profile retrain run --rm dt-ewh

# Simulación MPC en bucle cerrado (demo)
docker compose -f docker-compose.ewh.yml stop fc
docker compose -f docker-compose.ewh.yml --profile sim run --rm fc-sim

# Ver logs
docker compose -f docker-compose.ewh.yml logs -f fc
docker compose -f docker-compose.ewh.yml logs -f control-room

# Parar todo
docker compose -f docker-compose.ewh.yml down
```

### Prerequisitos

- Docker + Docker Compose v2
- Copiar `.env.example` a `.env`
- Ejecutar el re-entrenamiento del DT antes del primer arranque del FC (para generar `cold_room_model_continuous.json` en el volumen compartido)

---

## 11. Resumen de Tecnologías

| Componente | Tecnología | Versión |
|------------|------------|---------|
| Controlador MPC | Julia + JuMP + HiGHS | Julia 1.12 |
| Digital Twin (EWH) | R + ctsmTMB | R 4.4.2 |
| Digital Twin (EV) | Python + PyTorch (DQN) | PyTorch 2.7.0 |
| Simulador físico | Python + NumPy + SciPy | Python 3.12 |
| Dashboard | Python + Flask | Flask 3.1.1 |
| Previsiones API | Python + FastAPI + NeuralForecast | NeuralForecast 2.0.1 |
| Previsiones UI | Python + Streamlit | — |
| Pipelines | Python + Prefect | Prefect 3.4.15 |
| Base de datos | SQLite | — |
| Contenedores | Docker + Docker Compose | v2 |
