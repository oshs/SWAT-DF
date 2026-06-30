# SWAT-DF — Documentation of Code Modifications

This document records every modification made to the SWAT2012 baseline to implement the dynamic feedback (DF) coupling between SWAT and the PMAUP microeconomic model.

## 1. Baseline reference

| Item | Value |
|---|---|
| SWAT version | SWAT2012, Feb 2024 release — version string in `main.f`: `SWAT Feb 1 VER 2024/Rev 692` |
| Distribution downloaded | `Rev_692_64debug` (official SWAT2012 distribution, swat.tamu.edu) |
| Compiler | Intel Fortran via Visual Studio (`ifx 2025.2.0`)


## 2. Summary of changes

| Type | Count | Files |
|---|---|---|
| New subroutines (new files) | 2 | `readsoclup.f`, `resetsoclu.f` |
| Modified subroutines | 4 | `main.f`, `modparm.f`, `readfile.f`, `simulate.f` |
| New global variables | 5 | in module `parm` (`modparm.f`) |
| New input files read | 4 | `soclup.dat`, `LupInput25/50/100` |
| New output files written | 1 | `lup_change.log` |

## 3. Modified and added subroutines

| Source file | Subroutine | Change | Purpose |
|---|---|---|---|
| `modparm.f` | module `parm` | modified | Declare DF global variables (see §6) |
| `main.f` | `main` | modified | Open `lup_change.log`; call `readsoclup` after `readlup`; close log at end of run |
| `readfile.f` | `readfile` | modified | Open `soclup.dat` on unit 123 (alongside existing `lup.dat` on unit 122) |
| `readsoclup.f` | `readsoclup` | **added** | Read `soclup.dat`: map each scenario_id to its land-use file name |
| `simulate.f` | `simulate` | modified | Annual baseline snapshot/restore; March-31 reservoir check; scenario selection; call `resetsoclu` |
| `resetsoclu.f` | `resetsoclu` | **added** | Apply the selected scenario: overwrite `hru_fr` from the scenario file and rescale all HRU-dependent geometry and storage |

## 4. New input files

### 4.1 `soclup.dat` — scenario registry

One line per water-restriction scenario

```
   id  filename
    1  LupInput25
    2  LupInput50
    3  LupInput100
```

| Field | Type | Meaning |
|---|---|---|
| `soc_id` | integer | Scenario id: 1 = 25 % water reduction, 2 = 50 %, 3 = 100 % restriction |
| `socfnam` | char(20) | Land-use file name for that scenario, stored in `fname_soc(soc_id)` |

### 4.2 `LupInput25` / `LupInput50` / `LupInput100` — HRU land-use fractions (read by `resetsoclu`)


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



