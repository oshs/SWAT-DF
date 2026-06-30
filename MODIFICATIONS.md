# SWAT-DF — Documentation of Code Modifications

This document records every modification made to the SWAT2012 baseline to implement the dynamic feedback (DF) coupling between SWAT and the PMAUP microeconomic model. It is intended to let a reviewer reproduce the changes by diffing `src/` against the unmodified baseline.

## 1. Baseline reference

| Item | Value |
|---|---|
| SWAT version | SWAT2012, Feb 2024 release — version string in `main.f`: `SWAT Feb 1 VER 2024/Rev 693` |
| Distribution downloaded | `Rev_692_64debug` (official SWAT2012 distribution, swat.tamu.edu) |
| Compiler / toolchain | Intel Fortran via Visual Studio (`‹compiler version›`); build **Release \| x64** for reported results (the distributed debug build disables optimisation) |
| Diff command | `diff -ruN swat2012_rev693/ SWAT-DF/src/` |

All DF-specific code is delimited by the comment banner `! ============   SWAT_DF =========================` for easy identification.

## 2. Summary of changes

| Type | Count | Files |
|---|---|---|
| New subroutines (new files) | 2 | `readsoclup.f`, `resetsoclu.f` |
| Modified subroutines | 4 | `main.f`, `modparm.f`, `readfile.f`, `simulate.f` |
| Cosmetic-only change | 1 | `res.f` (whitespace; no functional change) |
| New global variables | 5 | in module `parm` (`modparm.f`) |
| New input files read | 4 | `soclup.dat`, `LupInput25/50/100` |
| New output files written | 1 | `lup_change.log` |

## 3. Modified and added subroutines

| Source file | Subroutine | Change | Purpose |
|---|---|---|---|
| `modparm.f` | module `parm` | modified | Declare DF global variables (see §6) |
| `main.f` | `main` | modified | Open `lup_change.log`; call `readsoclup` after `readlup`; close log at end of run |
| `readfile.f` | `readfile` | modified | Open `soclup.dat` on unit 123 (alongside existing `lup.dat` on unit 122) |
| `readsoclup.f` | `readsoclup` | **added** | Read `soclup.dat`: map each scenario id to its land-use file name |
| `simulate.f` | `simulate` | modified | Annual baseline snapshot/restore; March-31 reservoir check; scenario selection; call `resetsoclu` |
| `resetsoclu.f` | `resetsoclu` | **added** | Apply the selected scenario: overwrite `hru_fr` from the scenario file and rescale all HRU-dependent geometry and storage |
| `res.f` | `res` | cosmetic | Whitespace only — no functional change (candidate for revert) |

## 4. New input files

### 4.1 `soclup.dat` — scenario registry (read by `readsoclup`, unit 123)

One line per water-restriction scenario, format `(i5,1x,a)`, terminated by a `0` id or EOF:

```
   id  filename
    1  LupInput25
    2  LupInput50
    3  LupInput100
    0
```

| Field | Type | Meaning |
|---|---|---|
| `soc_id` | integer | Scenario id: 1 = 25 % water reduction, 2 = 50 %, 3 = 100 % restriction |
| `socfnam` | char(20) | Land-use file name for that scenario, stored in `fname_soc(soc_id)` |

### 4.2 `LupInput25` / `LupInput50` / `LupInput100` — HRU land-use fractions (read by `resetsoclu`, unit 9124)

Line 1 is a header (skipped); subsequent lines list per-HRU fractions (PMAUP output):

```
<header line>
   <hru index>   <hru_fr>
   ...
```

| Field | Unit | Meaning |
|---|---|---|
| `hru` | – | HRU index |
| `hru_fr(j)` | fraction (0–1) | Fraction of subbasin area in that HRU under the scenario |

## 5. New output file

| File | Unit | Written by | Contents |
|---|---|---|---|
| `lup_change.log` | `soclup_log_unit` | `main`, `simulate`, `readsoclup`, `resetsoclu` | Run log: scenario registry read, per-year March reservoir volume, selected scenario, and pre/post `hru_fr` per HRU |

## 6. New global variables (module `parm`, `modparm.f`)

| Variable | Type | Meaning |
|---|---|---|
| `soc_id` | integer | Scenario id read from `soclup.dat` (1/2/3) |
| `current_lup_id` | integer | Scenario selected in the current year from the March reservoir volume |
| `soclup_log_unit` | integer | Fortran unit for `lup_change.log` |
| `fname_soc(100)` | char(20) | Scenario id → land-use file name map |
| `hru_fr_base(:)` | real, allocatable | Baseline HRU fractions (reserved) |

## 7. Coupling logic and decision rule (`simulate.f`)

The DF hook executes once per simulation year, after the warm-up period (`curyr > nyskip`).

**Annual baseline handling.** In the first simulated year the model snapshots the baseline state of all HRU-dependent variables (`hru_fr`, `hru_km`, pond areas/volumes, wetland areas/volumes, and monthly pond/shallow/deep water use). In every subsequent year these are restored to baseline ("clean slate") before a new scenario is applied, so restrictions do not compound across years.

**Trigger.** On 31 March (`i_mo == 3`, `iida == 91 - leapyr`) the storage of reservoir 1 (`res_vol(1)`, m³) is converted to MCM and compared against fixed thresholds:

| March-31 storage of reservoir 1 (MCM) | `current_lup_id` | Scenario | File applied |
|---|---|---|---|
| ≥ 312 | – (none) | No restriction; baseline land use retained | – |
| 246 < V < 312 | 1 | 25 % water reduction | `LupInput25` |
| 180 < V ≤ 246 | 2 | 50 % water reduction | `LupInput50` |
| 0 < V ≤ 180 | 3 | 100 % water restriction | `LupInput100` |

When a restriction is triggered, `resetsoclu` reads the corresponding file and overwrites `hru_fr`, then recomputes `hru_km`, `hru_ha`, `hru_dafr` and rescales pond, wetland, and monthly water-use variables in proportion to the new fractions.

> **Basin-specific constants.** The reservoir index (`res_vol(1)`), the storage thresholds (312 / 246 / 180 MCM), and the evaluation date (31 March) are specific to the Tormes application and are currently set in `simulate.f`. Adjust these for other basins.
