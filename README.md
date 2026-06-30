# SWAT-DF

**Dynamic Feedback Coupling between SWAT and a Positive Multi-Attribute Utility Programming (PMAUP) Microeconomic Model**

SWAT-DF is a customized implementation of SWAT2012 that enables dynamic, interannual coupling with a microeconomic model (PMAUP). The framework introduces a two-way feedback mechanism between hydrological processes and land-use decisions: hydrological state (reservoir storage) conditions the water-restriction level imposed on agriculture, and the economic model's optimal crop portfolio is fed back into SWAT as updated HRU land-use fractions before the next simulation year.

| | |
|---|---|
| Baseline | SWAT2012 Rev. 692 (Feb 2024 release; distributed as Rev_692) |
| Companion economic model | PMAUP (see Zenodo DOI below) |
| Input/calibration data | [10.5281/zenodo.18543335](https://doi.org/10.5281/zenodo.18543335) |

---

## 1. Conceptual framework

The dynamic feedback (DF) protocol operates through an annual two-way exchange:

1. **Hydrological simulation (SWAT).** SWAT simulates basin-scale water-balance components and reservoir dynamics.
2. **Water-restriction assessment.** Reservoir storage thresholds determine the applicable water-restriction (WR) level: 25 %, 50 %, or 100 %.
3. **Economic response (PMAUP).** PMAUP adjusts crop portfolios and irrigation decisions in response to the WR condition.
4. **Land-use update (SWAT-DF).** The updated crop portfolio is translated into HRU-level fractional areas (`hru_fr`) and incorporated into SWAT before the next simulation year.

See [`MODIFICATIONS.md`](MODIFICATIONS.md) for the subroutine-level documentation of all changes made to the SWAT2012 baseline.

---

## 2. Requirements

| Component | Version / notes |
|---|---|
| Fortran compiler | Intel Fortran (`ifort`/`ifx`, oneAPI ≥ 2024) — baseline build. |
| Build environment (Windows) | Visual Studio 2022 with the Intel oneAPI HPC Toolkit integration |
| SWAT2012 source | Rev. 692 — the unmodified baseline this fork derives from |

For a general, maintainer-provided walkthrough of configuring an Intel Fortran project for SWAT, see the Intel_Fortran_dev_settings document attached by N. B. Sammons (USDA-ARS) to the SWAT-user group thread "How to compile SWAT source code on a Windows system" (Feb 2025):(https://groups.google.com/g/swatuser/c/l7BIRcwh80g/m/7mrV86IvAgAJ).

---

## 3. Build (installation)

### 3.1 Windows — Visual Studio 2022

1. Clone the repository:
   ```bash
   git clone https://github.com/oshs/SWAT-DF.git
   cd SWAT-DF
   ```
2. Open `Soc_LUP.sln` in Visual Studio 2022.
3. Select the **Release | x64** configuration.
4. **Build → Build Solution**.
5. The executable is written to `‹x64/Release/SWAT-DF.exe›`.

---

## 4. Quick start

A SWAT-DF run is a standard SWAT2012 run plus the DF land-use inputs. The coupling executes automatically once the inputs are in place.

1. **Prepare the SWAT project.** Place your calibrated SWAT `TxtInOut` directory (all `.cio`, `.hru`, `.mgt`, `.res`, … files) in the working directory.
2. **Add the scenario registry.** Place `soclup.dat` in the project directory. It maps each water-restriction scenario_id to its land-use file:

   ```
      1  LupInput25
      2  LupInput50
      3  LupInput100
   ```

3. **Add the PMAUP-derived land-use files** referenced by `soclup.dat`:

   | File | Scenario | Content |
   |---|---|---|
   | `LupInput25` | 25 % water reduction | Per-HRU `hru_fr` (header line + `hru  fraction` rows) |
   | `LupInput50` | 50 % water reduction | as above |
   | `LupInput100` | 100 % water restriction | as above |

   (The unrestricted case uses the baseline land use already in `TxtInOut`.)
4. **Hydrological Response** As an example, we used the reservoir storage volume. The decision rule (reservoir 1 storage on 31 March → scenario) and its thresholds  are defined in `simulate.f`; adjust there for another basin. See [`MODIFICATIONS.md`](MODIFICATIONS.md).
5. **Run:**
   ```bash
   # run x64\Release\SWAT-DF.exe from the project directory on Windows
   ```
6. **Outputs.** Standard SWAT outputs are written to `output.hru`, `output.rch`, `output.std`, etc. The DF activity — scenario registry read, each year's March reservoir volume, the selected scenario, and pre/post `hru_fr` — is logged to `lup_change.log`.


---

## 5. Documentation of code modifications

All deviations from the SWAT2012 baseline — new subroutines, modified subroutines, and new I/O — are documented file-by-file in [`MODIFICATIONS.md`](MODIFICATIONS.md), with the baseline revision pinned.
