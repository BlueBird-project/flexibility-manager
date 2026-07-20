# Column reference — `karno-410708.raw.k0001`

Explanation and confidence for the 765 columns of the Profondeville "Energy Center"
(K-0001) BigQuery table. Built from the as-built docs
(`K-0001 Implémentation des scenarios sur HMI.pdf`, the Rev11 P&ID), `varname.json`,
`README.md`, and standard instrument-tag conventions.

## How to read this file

**Confidence** — how sure the explanation is:

| Mark | Meaning |
|---|---|
| **High** | Confirmed by a doc, the HMI screenshots, `varname.json`, or an unambiguous tag convention. |
| **Med** | Reasoned from context / naming; plausible but not cross-checked against a source. |
| **Low** | Guess. Verify before relying on it. |

**MPC role** — relevance to the demand-response MPC (see the shortlist in the next
section for the rationale). Blank = not directly used.

| Tag | Role in the controller |
|---|---|
| 🎛️ **CTRL** | Manipulated variable — you actuate this. |
| 📈 **STATE** | Internal state the model must track (mostly the tank). |
| 🎯 **OUT** | Controlled output — the district-supply boundary. |
| 🌡️ **DIST** | Measured disturbance / forecast input. |
| ⚡ **COST** | Electrical power/energy — the DR objective. |
| ⚙️ **SP** | Existing-controller setpoint — feedforward / grey-box prior, not a free input. |

---

## System model (one paragraph)

In **winter** the Energy Center only makes hot water into a **2.5 m³ buffer tank**,
charged by a **geothermal HP (PAC P3.01, ~40 kW, on/off)** and an **air-source HP
(PAC P5.01, ~20 kW, modulating 0–100 %)**. A **heating curve** sets the district
**supply-temperature** target `TT_701` from the outdoor temperature (day/night
levels); the tank target sits ~4–6 °C above supply and the HP condenser target ~4 °C
above the tank. The HPs are staged by tank-temperature offsets, with GEO-vs-AERO
priority chosen by COP (ground vs air temperature). A **3-way mixing valve FCV_702**
blends warm district return with hot tank-top water to keep `TT_701` flat. In
**summer** the plant provides cooling and actively/passively regenerates the
boreholes. Your MPC boundary is the **supply temperature to the district**; the tank
is your flexibility reservoir.

---

## ⭐ MPC shortlist (answers task 2)

The signals that matter for system identification + demand-response control.
Confirmed actuation: **ASHP modulates 0–100 %, GSHP is on/off, FCV_702 is continuous.**

### Manipulated variables (what you command) — 🎛️ CTRL

| Column | What it does | Note |
|---|---|---|
| `PAC_501_MODULATION` | ASHP compressor modulation **0–100 %** | **primary continuous input** (air HP power) |
| `PAC_501_RUN` / `PAC_501_LIBERATION` | ASHP run / enable-permission | gate for the modulating input |
| `PAC_301_RUN` / `PAC_301_LIBERATION` | GSHP **on/off** / enable | **primary binary input** (geo HP, ~40 kW) |
| `FCV_702_POSITION` (cmd `FCV_702_CONSIGNE`) | 3-way supply mixing valve position | **continuous input**, shapes `TT_701` |
| `PAC_501_CONSIGNE_ACTUEL` | ASHP active setpoint (condenser-out temp) | alternative/secondary handle if you control by setpoint |
| `PAC_301_CONSIGNE_ACTUEL` | GSHP active setpoint | alternative/secondary handle |

### States (thermal dynamics / storage) — 📈 STATE

| Column | Meaning |
|---|---|
| `TT_601` | Tank **top** temperature — main storage state = your flexibility |
| `TT_602` | Tank **bottom** temperature (return/cold side) — stratification state |
| `PAC_501_T_OUT_EAU_ECHANGEUR` / `PAC_301_T_OUT_CONDENSEUR` | HP outlet (charging) temps |
| `PAC_501_T_IN_EAU_ECHANGEUR` / `PAC_301_T_IN_CONDENSEUR` | HP inlet temps |
| `TT_101`, `TT_102` | Borehole field temps — slow ground state; sets GSHP COP & the GEO/AERO decision |

### Controlled output (boundary = district supply) — 🎯 OUT

| Column | Meaning |
|---|---|
| `TT_701` | **Supply temperature to the district** — the variable your MPC must hold on the heating curve |
| `TT_702` | District **return** temperature (also a demand signal) |
| `EC_701_Q` | Thermal power delivered to the district (see caveat in the EC section) |
| `FQT_701_OUT` | District volumetric flow |

### Measured disturbances / forecasts — 🌡️ DIST

| Column | Meaning |
|---|---|
| `TT_Text` | Outdoor air temperature — the main forecastable disturbance (drives the heating curve & COP) |
| `EC_701_Q`, `TT_702`, `FQT_701_OUT` | District **heat demand** (your "consumption forecast" target) |
| *(external)* | **Electricity price** and **PV production** — **not in this table**; join from an external source |

### Electrical / DR objective — ⚡ COST

| Column | Meaning |
|---|---|
| `PAC_501_PUISSANCE` / `EM_PAC_501_PWR_TOT_P` | ASHP electrical power |
| `PAC_301_PUISSANCE` / `EM_PAC_301_PWR_TOT_P` | GSHP electrical power |
| `p1_meter_active_power_w` | Site grid-import instantaneous power |
| `p1_meter_total_power_import_kwh`, `ORES_index` | Cumulative grid-import energy (DR cost accounting) |

### Existing-controller setpoints (feedforward / grey-box priors) — ⚙️ SP

| Column | Meaning |
|---|---|
| `MODE_HIV_ConsigneSupplyIfTempExtInfParam` / `...SupParam` | Supply setpoint, cold / mild outdoor (night/day heating-curve levels) |
| `MODE_HIV_ConsignePACIfTempExtInfParam` / `...SupParam` | HP setpoint, cold / mild outdoor |
| `MODE_HIV_modHiv_ConsigneTempStock` | Tank-temperature setpoint |
| `MODE_HIV_ModeHivernal` | Winter mode active |
| `MODE_HIV_paramTemperatureExt` / `paramOffsetTempExt` | Outdoor day/night switch threshold + deadband |

> **COP is not stored directly** — derive it per HP as (thermal power)/(electrical
> power), e.g. `EC_201_*`/temperatures vs `PAC_301_PUISSANCE`. It depends on source
> and sink temperatures, so keep those temps as model regressors.

---

## Full column reference

### 1. Temperature transmitters (`TT_*`) — °C

| Column | Explanation | Conf. | MPC |
|---|---|---|---|
| `TT_101` | Borehole field temperature, inlet side of GSHP (ground loop) | High | 📈 STATE |
| `TT_102` | Borehole field temperature, outlet side of GSHP | High | 📈 STATE |
| `TT_601` | Buffer-tank **top** temperature (TT6.01) | High | 📈 STATE |
| `TT_602` | Buffer-tank **bottom** temperature (TT6.02) | High | 📈 STATE |
| `TT_701` | District **supply** temperature (TT7.01) | High | 🎯 OUT |
| `TT_702` | District **return** temperature (TT7.02) | High | 🎯 OUT / 🌡️ DIST |
| `TT_Text` | Outdoor air temperature (dedicated sensor; replaced dead `PAC_501_T_EXT`) | High | 🌡️ DIST |

### 2. Pressure transmitters (`PT_*`) — bar

| Column | Explanation | Conf. | MPC |
|---|---|---|---|
| `PT_101` | Pressure, ground/borehole loop | Med | |
| `PT_201` | Filter/system pressure, ground circuit | High | |
| `PT_202` | Filter/system pressure, ground circuit (2nd point) | High | |
| `PT203` | Pressure, ground circuit (legacy tag, no underscore — likely = a PT_20x) | Low | |
| `PT_401` | System pressure, HP side | High | |
| `PT_601` | Buffer-tank pressure | Med | |
| `PT_701` | Distribution pressure (pump side) | Med | |
| `PT_702` | Distribution pressure (2nd point) | Med | |
| `PT_703` | Filter pressure, distribution | High | |
| `PT_704` | Filter pressure, distribution (2nd point) | High | |
| `PT_705` | System pressure, distribution outlet (target ~2.3 bar) | High | |

### 3. Differential-pressure transmitters (`DPT*`)

`_VALEUR` = differential value; `_PT_AMONT` = upstream pressure; `_PT_AVAL` = downstream pressure.

| Column | Explanation | Conf. |
|---|---|---|
| `DPT201_VALEUR`, `DPT201_VALEUR_PT_AMONT`, `DPT201_VALEUR_PT_AVAL` | ΔP across a ground-circuit component (filter/pump) | Med |
| `DPT_701_VALEUR`, `..._PT_AMONT`, `..._PT_AVAL` | ΔP, distribution circuit (point 1) | Med |
| `DPT_702_VALEUR`, `..._PT_AMONT`, `..._PT_AVAL` | ΔP, distribution circuit (point 2) | Med |

### 4. Flow transmitters (`FQT_*`, `FQ*`) — m³/h

| Column | Explanation | Conf. | MPC |
|---|---|---|---|
| `FQT_201_OUT` | Volumetric flow, ground circuit | High | |
| `FQT_701_OUT` | Volumetric flow, district/distribution circuit | High | 🌡️ DIST |
| `FQ1201_OUT` | Legacy/duplicate of `FQT_201_OUT` | Low | |
| `FQ1701_OUT_1` | Legacy/duplicate of `FQT_701_OUT` | Low | |

### 5. Heat meters / calorimeters (`EC_201_*`, `EC_701_*`)

`EC_201` = ground heat exchanger meter; `EC_701` = district/distribution meter.
Suffixes: `E_h` heating energy, `E_c` cooling energy, `E_b` balance/other energy
register, `T_flow`/`T_return` supply/return temps, and `P`/`Q` instantaneous readings.

> ⚠️ **Naming caveat.** `varname.json` treats `EC_701_Q` as thermal power [kW] and
> `EC_701_P` as a power in kW. Standard heat-meter convention is the opposite
> (`P` = thermal power kW, `Q` = flow m³/h). **Verify which is which** against the HMI
> "Puiss. instant." before using either as the delivered-power signal.

| Column | Explanation | Conf. | MPC |
|---|---|---|---|
| `EC_201_E_h` / `EC_201_E_c` / `EC_201_E_b` | Ground meter: heating / cooling / balance energy (kWh, cumulative) | Med | |
| `EC_201_P` | Ground meter instantaneous reading (power kW *or* flow — see caveat) | Med | |
| `EC_201_Q` | Ground meter thermal power [kW] (per `varname.json`) | Med | |
| `EC_201_T_flow` / `EC_201_T_return` | Ground exchanger supply / return temp | High | 📈 STATE |
| `EC_701_E_h` / `EC_701_E_c` / `EC_701_E_b` | District meter: heating / cooling / balance energy (kWh) | Med | |
| `EC_701_P` | District meter instantaneous reading (see caveat) | Med | |
| `EC_701_Q` | District thermal power delivered [kW] (per `varname.json`) | Med | 🎯 OUT |
| `EC_701_T_flow` / `EC_701_T_return` | District supply / return temp (≈ `TT_701`/`TT_702`) | High | |

### 6. Electrical power-quality meters (`EM_*`) — pattern block

Three Modbus energy meters, one per major consumer:
`EM_EC_701_*` (distribution), `EM_PAC_301_*` (GSHP), `EM_PAC_501_*` (ASHP).
Each exposes the same register set (confidence **High** for the electrical meaning,
these are standard Modbus energy-meter registers):

| Suffix pattern | Meaning |
|---|---|
| `_PWR_TOT_P` / `_PWR_TOT_Q` / `_PWR_TOT_S` | Total active [W/kW] / reactive [var] / apparent [VA] power |
| `_PWR_TOT_PF` | Total power factor |
| `_PWR_P_L1..L3` / `_PWR_Q_L1..L3` / `_PWR_S_L1..L3` | Per-phase active / reactive / apparent power |
| `_PWR_PF_L1..L3` | Per-phase power factor |
| `_CURRENT_AVG`, `_CURR_L1..L3`, `_CURR_N` | Average / per-phase / neutral current [A] |
| `_VOLT_AVG_LL`, `_VOLT_AVG_LN`, `_VOLT_Lx_Ly`, `_VOLT_Lx_N` | Line-line / line-neutral voltages [V] |
| `_FREQ` | Frequency [Hz] |
| `_ENER_P_IMP_*` / `_ENER_P_EXP_*` | Active energy imported / exported (T1/T2 tariffs, period & phase registers) [kWh] |
| `_ENER_Q_IMP_*` / `_ENER_Q_EXP_*` | Reactive energy imported / exported [kvarh] |
| `_ENER_S_T1` / `_ENER_S_T2` | Apparent energy, tariff 1 / 2 [kVAh] |
| `_TIME_UTC` | Meter's own UTC timestamp |

**MPC:** `EM_PAC_301_PWR_TOT_P` and `EM_PAC_501_PWR_TOT_P` are the clean per-HP
electrical-power signals for the ⚡ COST term (higher-quality than `PAC_xxx_PUISSANCE`).

### 7. Geothermal heat pump — PAC P3.01 (`PAC_301_*`, on/off)

| Column | Explanation | Conf. | MPC |
|---|---|---|---|
| `PAC_301_RUN` | Running status / **on-off command** | High | 🎛️ CTRL |
| `PAC_301_LIBERATION` | Enable / release permission | High | 🎛️ CTRL |
| `PAC_301_CONSIGNE_ACTUEL` | Active setpoint (condenser-out target °C) | High | 🎛️ CTRL / ⚙️ SP |
| `PAC_301_CONSIGNE_1` / `_2` | Setpoint level 1 / 2 (day / night curve) | Med | ⚙️ SP |
| `PAC_301_CONSIGNE_CHAUD_1` / `_2` | Heating setpoint 1 / 2 | Med | ⚙️ SP |
| `PAC_301_CONSIGNE_AUTO` / `_MANU` | Auto / manual setpoint source | Med | |
| `PAC_301_CONSIGNE_SCALE_MIN` / `_MAX` | Setpoint scaling bounds | Med | |
| `PAC_301_PUISSANCE` | Electrical power [kW] | High | ⚡ COST |
| `PAC_301_P_MAX` | Max-power parameter | Med | |
| `PAC_301_COP_MAX` | Max-COP parameter (nameplate, not measured COP) | Med | |
| `PAC_301_DEFAUT` | Fault flag | High | |
| `PAC_301_FLOW_SWITCH` | Flow-switch status (safety interlock) | High | |
| `PAC_301_T_IN_CONDENSEUR` | Condenser water inlet (tank/return side) | High | 📈 STATE |
| `PAC_301_T_OUT_CONDENSEUR` | Condenser water outlet (charging temp) | High | 📈 STATE |
| `PAC_301_T_IN_EAU_ECHANGEUR` | Evaporator (ground-side) water inlet | High | 📈 STATE |
| `PAC_301_T_OUT_EAU_ECHANGEUR` | Evaporator (ground-side) water outlet | High | 📈 STATE |
| `PAC_301_T_EXT` | Outdoor temp at GSHP (not the primary air sensor) | Med | |
| `PAC_301_HEURES_FONCTIONNEMENT_MACHINE` | Running-hours counter | High | |
| `PAC_301_NB_DEMARRAGE_MACHINE` | Number of compressor starts | High | |
| `PAC_301_TIME_BEFORE_RESTART` | Anti-short-cycle timer | Med | ⚙️ SP |
| `PAC_301_DELAI_STARTUP` | Startup delay | Med | |
| `PAC_301_PT_CONTROLE_1` | Control pressure/point reference | Low | |
| `PAC_301_LIBERATION_AUTO` / `_MANU` | Enable source auto / manual | Med | |
| `PAC_301_HMI_BUTTON_AUTO`/`_MANU`/`_START`/`_STOP` | HMI push-buttons | High | |
| `PAC_301_HMI_CONSIGNE` | HMI-entered setpoint | Med | |
| `PAC_301_HMI_LIBERATION_AUTO` / `_MANU` | HMI enable toggles | Med | |

### 8. Air-source heat pump — PAC P5.01 (`PAC_501_*`, modulating 0–100 %)

Same register layout as PAC_301, plus modulation. (Circulation pump is P4.01, built-in.)

| Column | Explanation | Conf. | MPC |
|---|---|---|---|
| `PAC_501_MODULATION` | Compressor **modulation 0–100 %** | High | 🎛️ CTRL |
| `PAC_501_RUN` | Running status / run command | High | 🎛️ CTRL |
| `PAC_501_LIBERATION` | Enable / release permission | High | 🎛️ CTRL |
| `PAC_501_CONSIGNE_ACTUEL` | Active setpoint (condenser-out target °C) | High | 🎛️ CTRL / ⚙️ SP |
| `PAC_501_CONSIGNE_1` / `_2` | Setpoint level 1 / 2 (day / night) | Med | ⚙️ SP |
| `PAC_501_CONSIGNE_CHAUD_1` / `_2` | Heating setpoint 1 / 2 | Med | ⚙️ SP |
| `PAC_501_CONSIGNE_AUTO` / `_MANU` | Auto / manual setpoint source | Med | |
| `PAC_501_CONSIGNE_SCALE_MIN` / `_MAX` | Setpoint scaling bounds | Med | |
| `PAC_501_PUISSANCE` | Electrical power [kW] | High | ⚡ COST |
| `PAC_501_P_MAX` | Max-power parameter | Med | |
| `PAC_501_COP_MAX` | Max-COP parameter (nameplate) | Med | |
| `PAC_501_DEFAUT` | Fault flag | High | |
| `PAC_501_FLOW_SWITCH` | Flow-switch status | High | |
| `PAC_501_T_IN_EAU_ECHANGEUR` | Water inlet to air-HP condenser (tank/return side) | High | 📈 STATE |
| `PAC_501_T_OUT_EAU_ECHANGEUR` | Water outlet of air-HP condenser (charging temp) | High | 📈 STATE |
| `PAC_501_T_IN_CONDENSEUR` / `_T_OUT_CONDENSEUR` | Alternate condenser in/out sensors (likely same circuit as `_EAU_ECHANGEUR`) | Med | 📈 STATE |
| `PAC_501_T_EXT` | Outdoor temp at ASHP — **dead since 2025-12-05** (use `TT_Text`) | High | |
| `PAC_501_HEURES_FONCTIONNEMENT_MACHINE` | Running-hours counter | High | |
| `PAC_501_NB_DEMARRAGE_MACHINE` | Number of compressor starts | High | |
| `PAC_501_TIME_BEFORE_RESTART` | Anti-short-cycle timer | Med | |
| `PAC_501_PT_CONTROLE_1` | Control pressure/point reference | Low | |
| `PAC_501_LIBERATION_AUTO` / `_MANU` | Enable source auto / manual | Med | |
| `PAC_501_HMI_BUTTON_AUTO`/`_MANU`/`_START`/`_STOP` | HMI push-buttons | High | |
| `PAC_501_HMI_CONSIGNE` | HMI-entered setpoint | Med | |
| `PAC_501_HMI_LIBERATION_AUTO` / `_MANU` | HMI enable toggles | Med | |

### 9. Pumps (`P_*`, `PID_P701_*`)

`_VITESSE` = speed [%], `_CONSIGNE_AUTO`/`_MANU` = auto/manual speed setpoint,
`_STATUT`/`_MODE`/`_DEFAUT`/`_WARNING` = status/mode/fault/warning,
`_HMI_*` = HMI buttons & scaling. Assignments (from P&ID):

| Pump | Role | Key columns |
|---|---|---|
| **P2.01** (`P_201_*`) | Ground-loop circulator (also drives passive regeneration in summer) | `P_201_VITESSE` (speed), `P_201_DEFAUT`, `P_201_HMI_*` |
| **P3.01** (`P_301_*`) | GSHP source-side circulator | `P_301_VITESSE`, `P_301_DEFAUT`, `P_301_HMI_*` |
| **P4.01** (`P_401_*`) | ASHP water-side circulator (≤3.9 m³/h max, 2.9 nominal) | `P_401_VITESSE`, `P_401_DEFAUT`, `P_401_HMI_*` |
| **P7.01** (`P_701_*`) | District/distribution circulator (variable speed, pressure-controlled; always on) | `P_701_VITESSE`, `P_701_MODE`, `P_701_STATUT`, `P_701_CONSIGNE_AUTO/MANU` |
| `P701_VITESSE` | Legacy/duplicate of `P_701_VITESSE` | — |
| `PID_P701_*` / `PID_P_701_*` | Distribution-pump pressure PID: `ConsignePression` = pressure setpoint, `InputPression` = measured pressure (duplicate spellings) | Med |

Per-pump confidence: role assignment **High** (P&ID), individual HMI/scale sub-fields **Med**.

### 10. Motorised valves (`FCV_*`, `ELV_*`)

`FCV` = flow-control valve, `ELV` = electric (isolation/changeover) valve. Common
suffixes: `_OUVERTURE_A`/`_B` open-position A/B port [%], `_FERMETURE` closed,
`_STATUT` status, `_MODE` auto/manual, `_DEFAUT` fault, `_WARNING`, `_BUTTON_AUTO`/`_MANU`.

| Column | Explanation | Conf. | MPC |
|---|---|---|---|
| `FCV_702_POSITION` | **3-way supply mixing valve** position [%] (blends return + tank-top) | High | 🎛️ CTRL |
| `FCV_702_CONSIGNE` | 3-way valve position **command/setpoint** | High | 🎛️ CTRL |
| `FCV_702_MANU_CONSIGNE` | Manual position setpoint | Med | |
| `FCV_702_MODE`/`_STATUT`/`_DEFAUT`/`_BUTTON_AUTO`/`_BUTTON_MANU` | Valve mode/status/fault/buttons | Med | |
| `FCV702_POSITION` / `FCV702_CONSIGNE` | Legacy/duplicate spellings of the above | Low | |
| `FCV_701_*` | Distribution valve 7.01 (`_OUVERTURE_A/B`, status, mode, fault, buttons) | Med | |
| `FCV_401_*` … `FCV_404_*` | HP-side changeover/mixing valves (geo↔aero circuit routing); each with `_OUVERTURE_A/B`, `_MODE`, `_STATUT`, `_DEFAUT`, `_WARNING`, buttons | Med | |
| `ELV_401_*`, `ELV_402_*`, `ELV_403_*` | Motorised isolation valves (`_OUVERTURE`, `_FERMETURE`, `_STATUT`, `_MODE`, `_DEFAUT`, `_WARNING`, buttons) | Med | |

### 11. Filter/hydraulic stations (`FH*`)

Self-cleaning filter / hydraulic-balancing stations. `_DPT` = differential pressure,
`_ConsigneDPT` = ΔP setpoint, `_PTamont`/`_PTaval` = up/downstream pressure,
`_Encrasse` = clogged flag, `_Mode` = operating mode.

| Column group | Explanation | Conf. |
|---|---|---|
| `FH201_*`, `FH_201_*` | Ground-circuit filter station (duplicate spellings) | Med |
| `FH701_*`, `FH_701_*` | Distribution-circuit filter station (duplicate spellings) | Med |

### 12. Winter scenario / heating-curve logic (`MODE_HIV_*`)

Parameters and internal decision flags of the winter ("Hivernal") scenario tree.
Green-highlighted HMI cells = operator-settable setpoints; `set*` = internal booleans.

| Column | Explanation | Conf. | MPC |
|---|---|---|---|
| `MODE_HIV_ModeHivernal` | Winter mode active flag | High | ⚙️ SP |
| `MODE_HIV_ModeEstival` | Summer-mode flag mirrored in this block | Med | |
| `MODE_HIV_modeHiv_scenario` | Active winter scenario number (decision-tree id) | High | |
| `MODE_HIV_ConsigneSupplyIfTempExtInfParam` | Supply setpoint when outdoor < threshold (cold/night level) | High | ⚙️ SP |
| `MODE_HIV_ConsigneSupplyIfTempExtSupParam` | Supply setpoint when outdoor ≥ threshold (mild/day level) | High | ⚙️ SP |
| `MODE_HIV_ConsignePACIfTempExtInfParam` | HP setpoint when outdoor < threshold | High | ⚙️ SP |
| `MODE_HIV_ConsignePACIfTempExtSupParam` | HP setpoint when outdoor ≥ threshold | High | ⚙️ SP |
| `MODE_HIV_modHiv_ConsigneTempStock` | Tank-temperature setpoint | High | ⚙️ SP |
| `MODE_HIV_paramTemperatureExt` | Outdoor day/night switch threshold (~0 °C in doc) | High | ⚙️ SP |
| `MODE_HIV_paramOffsetTempExt` | Deadband offset on the outdoor switch | High | ⚙️ SP |
| `MODE_HIV_paramTempSoil` | Ground-temp threshold for GEO/AERO priority | High | ⚙️ SP |
| `MODE_HIV_paramOffsetTempSoil` | Offset on ground-temp threshold | High | |
| `MODE_HIV_paramTempSoilOffset3` | "Offset 3" — anti-hunting on the priority switch | High | |
| `MODE_HIV_paramStockTemperatureMax` / `Min` | Tank max/min setpoints | High | ⚙️ SP |
| `MODE_HIV_paramStockTempOffset1` | Offset that stages the priority HP on (tank < SP − offset1) | High | ⚙️ SP |
| `MODE_HIV_paramStockTempOffset2` | Offset that stages the 2nd HP on (tank < SP − offset2) | High | ⚙️ SP |
| `MODE_HIV_tempStockageBallon` | Measured tank temperature used by the logic | High | 📈 STATE |
| `MODE_HIV_temperatureSoil` | Measured soil temperature used by the logic | High | |
| `MODE_HIV_setConsigneMaxHivdegree` | Internal flag: max winter setpoint reached | Med | |
| `MODE_HIV_setSoilTempSupExtTemp` / `setSoilTempSupParam` | Internal priority-decision booleans (soil vs air / vs param) | Med | |
| `MODE_HIV_setStockTempMax` / `setStockTempMin` | Internal flags: tank above max / below min | Med | |
| `MODE_HIV_setStockTempMaxMinusOffset1` / `Offset2` | Internal flags: tank below (max − offset1/2) | Med | |
| `MODE_HIV_setStockTempMinMinusOffset1` / `Offset2` | Internal flags: tank below (min − offset1/2) | Med | |
| `MODE_HIV_alarmDemandCanNotBeMet` | Alarm: heat demand cannot be met | High | |
| `MODE_HIV_alarmTooLowGeoTemperature` | Alarm: borehole temperature too low | High | |
| `MODE_HIV_startDate_day` / `_month`, `_endDate_day` / `_month` | Winter-season start/end dates | High | |

### 13. Summer scenario / regeneration logic (`MODE_EST_*`)

Parameters and flags of the summer ("Estival") scenario: cooling + active/passive
borehole regeneration. Mostly out of scope for a winter heating MPC.

| Column | Explanation | Conf. |
|---|---|---|
| `MODE_EST_numScenario` | Active summer scenario number | High |
| `MODE_EST_startDate_day`/`_month`, `_endDate_*`, `_criticalDate_*`, `_setCriticalDate` | Summer period & "critical day" (forced regeneration) | High |
| `MODE_EST_paramExternalTemperature` / `...Offset` | Outdoor-temp threshold + offset enabling economic active regeneration | High |
| `MODE_EST_paramSoilTemperatureInjection` / `...Offset` | Soil injection-temp limit (legal max 25 °C) + offset | High |
| `MODE_EST_soilTemperatureInjection`, `_temperatureMaxInjection` | Measured / max soil injection temperature | Med |
| `MODE_EST_paramTemperatureMaxTank` / `...Offset`, `_paramTemperatureMinTank` / `...Offset` | Tank max/min setpoints (passive cooling vs active reheat) + offsets | High |
| `MODE_EST_BottomTemperatureStock`, `_setTempStock*` | Tank-bottom temp & internal tank-band decision flags | Med |
| `MODE_EST_heatExtracted` / `_heatInjected` / `_heatBalance` / `_paramBalance` / `_setBalanceThresholdHits` | Seasonal ground energy balance (extracted+injected; target ×1.3) | High |
| `MODE_EST_setPAC501ModeRegeneration` / `_setPAC501ModeSupply` | Commands ASHP into regeneration / supply mode | High |
| `MODE_EST_setP201Run` | Command ground pump P2.01 on (passive regen) | High |
| `MODE_EST_buttonForceRegeneration` / `_modeForceRegeneration` / `_setForceRegeneration` | Manual forced-regeneration button/mode | High |
| `MODE_EST_temperatureMaxNetwork` | Max allowed network temperature (summer) | Med |
| `MODE_EST_indexStartModehivSeptembre_EB`/`_EC`/`_EH` | Energy-counter snapshots at winter-mode start (Sept) | Low |
| `MODE_EST_setExteralTempSupParam`, `_setSoilTemperatureInjectionSupParam`, `_setTempStockInfMinParam`, `_setTempStockSupMaxParam`, `_setTempStockEntreMaxAndMin` | Internal decision booleans of the summer tree | Med |
| `MODE_EST_paramSoilTemperatureInjectionOffset`, `_paramExternalTemperatureOffset` | (offsets listed above) | High |

### 14. Grid / P1 smart meter and DSO index

The site electricity meter (P1/DSM-style) plus the ORES (Walloon DSO) index. Two
near-identical register sets appear: a **bare** set and a **`p1_meter_`-prefixed**
set — treat them as duplicate representations of the same meter (verify which is live).

| Column (prefixed / bare) | Explanation | Conf. | MPC |
|---|---|---|---|
| `p1_meter_total_power_import_kwh` / `total_power_import_kwh` | Cumulative grid **import** energy [kWh] | High | ⚡ COST |
| `..._total_power_import_t1_kwh` / `_t2_kwh` | Import energy, tariff 1 / 2 | High | ⚡ COST |
| `p1_meter_total_power_export_kwh` / `total_power_export_kwh` (+ t1/t2) | Cumulative grid **export** energy [kWh] | High | ⚡ COST |
| `p1_meter_active_power_w` / `active_power_w` | Instantaneous total active power [W] (import +/export −) | High | ⚡ COST |
| `p1_meter_active_power_average_w` / `active_power_average_w` | Averaged active power [W] | High | ⚡ COST |
| `..._active_power_l1_w`/`_l2_w`/`_l3_w` | Per-phase active power [W] | High | |
| `..._active_current_a`, `_l1_a`/`_l2_a`/`_l3_a` | Total / per-phase current [A] | High | |
| `..._active_voltage_l1_v`/`_l2_v`/`_l3_v` | Per-phase voltage [V] | High | |
| `p1_meter_active_tariff` / `active_tariff` | Active tariff register (T1/T2) | High | |
| `p1_meter_montly_power_peak_w` / `montly_power_peak_w` (+ `_timestamp`) | Monthly power peak [W] and its timestamp | High | ⚡ COST |
| `ORES_index` | DSO (ORES) cumulative energy index | Med | ⚡ COST |
| `p1_meter_meter_model` / `meter_model`, `_smr_version` / `smr_version`, `_unique_id` / `unique_id` | Meter model / SMR firmware / serial | High | |
| `p1_meter_wifi_ssid` / `wifi_ssid`, `_wifi_strength` / `wifi_strength` | Meter Wi-Fi link (telemetry health) | High | |
| `p1_meter_external` / `external` | External/auxiliary meter channel (e.g. gas) | Low | |
| `p1_quick_save` | Meter quick-save/housekeeping flag | Low | |
| `test`, `test_ores` | Test/debug columns — ignore | Low | |

### 15. Timestamp

| Column | Explanation | Conf. |
|---|---|---|
| `UTC_DateTime` | Sample timestamp (UTC); becomes the tz-aware index in `bq_extract.py` | High |

### 16. Per-apartment heat-interface units (`A01_*` … `D21_*`) — pattern block

20 apartment HIUs — **A01, A02, A11, A12, A21, B01, B02, B11, B12, B21, C01, C02,
C11, C12, C21, D01, D02, D11, D12, D21** — each with the same 10 fields. These are
downstream of your MPC boundary (inside the buildings) → **not control signals**, but
their aggregate is the district demand you forecast.

| Field (per apartment `XXX_`) | Explanation | Conf. |
|---|---|---|
| `XXX_heating_energy` | Cumulative heating energy delivered to the apartment [kWh] | High |
| `XXX_cooling_energy` | Cumulative cooling energy [kWh] | High |
| `XXX_volume` | Cumulative water volume through the HIU [m³] | High |
| `XXX_flow_temperature` | Supply temperature into the apartment [°C] | High |
| `XXX_return_temperature` | Return temperature from the apartment [°C] | High |
| `XXX_CONSOMME` | Instantaneous consumption (power/flow) | Med |
| `XXX_vanne_bypass` | Bypass-valve position/state | Med |
| `XXX_BP` | Bypass status/flag (`BP` = by-pass) | Low |
| `XXX_TC` | Setpoint / control temperature (`T° Consigne`) | Low |
| `XXX_DEFAUT_MODBUS` | Modbus communication fault flag for that HIU | High |

> **Demand aggregate for the "consumption forecast":** sum the 20 `XXX_heating_energy`
> (differenced to power) — cross-check against `EC_701_Q`.

---

## Data / external-source notes

- **Not in this table (must be joined externally):** day-ahead **electricity price**
  and **PV production / forecast**. The DR objective needs both.
- **COP** is not logged; derive per HP from thermal vs electrical power and keep
  source/sink temperatures as regressors.
- Cumulative counters (`*_kwh`, `*_energy`, `ORES_index`, `EM_*_ENER_*`) must be
  **differenced** to get rates — do this in the training/feature step, not the
  faithful extractor.
- Duplicate/legacy tag spellings exist (`FCV702_*` vs `FCV_702_*`, `P701_VITESSE`
  vs `P_701_VITESSE`, `FQ1201` vs `FQT_201`, bare vs `p1_meter_` grid registers).
  Confirm which member is live before wiring it into the controller.
