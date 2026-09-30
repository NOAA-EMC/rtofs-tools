# RTOFS Restart Processing Tools

These utilities provide:
- A fast, self-contained pipeline for downloading (from NOAA HPSS) 
- Converting RTOFS operational restarts into a UFS-coupled format (MOM6 and CICE6). 
- RTOFS restarts are from HYCOM and CICE4.

The tools are designed to be completely portable and run cleanly on RDHPCS systems (WCOSS2, Ursa) 
without relying on personal `.bashrc` environments, pre-existing Python packages, or bloated template files.

---

## 1. Pulling Restarts from HPSS

**Script:** `pull_restarts_from_hpss.sh`

- This script securely connects to HPSS
- Determines the correct RTOFS version path based on the requested date
- Downloads the `n00` nowcast restarts, and prepares them for conversion.

### Usage
```bash
./pull_restarts_from_hpss.sh <YYYYMMDD> <OUTPUT_DIR>
```

Example: `./pull_restarts_from_hpss.sh 20251215 /scratch5/.../restart_in`

### Key Features
- Machine Protection: Checks hostname and automatically aborts if run on a non-supported node (prevents hanging/crashing on compute nodes without HPSS access).

- Smart Versioning: Automatically maps the requested date (e.g., 20220805 vs 20251215) to the correct HPSS archive path (v2.3, v2.4, v2.5, etc.).

- Integrity Checks: Verifies that downloaded tarballs are not 0 bytes.

- Auto-Cleanup: Automatically extracts the nested v2.5 restart tar bundles:
  1. `rtofs_glo.t00z.n00.restart.a.tgz`
  2. `rtofs_glo.t00z.n00.restart_cice.tgz`
deletes the raw .tgz files to halve the storage footprint.

---

## 2. Converting CICE4 to CICE6 Restarts

**Wrapper Script:** `run_cice_restart_conversion.sh`

**Python Engine:** `convert_cice4_to_cice6_restart.py`

Translates the unformatted sequential binary CICE4 restart into a fully UFS-compliant CICE6 NetCDF4 restart.

### Usage
```bash
./run_cice_restart_conversion.sh <IN_DIR> <OUT_DIR> <YYYYMMDD> [options]
```
Example: `./run_cice_restart_conversion.sh --rdate 20220118 --out rtofs_glo.20220118_00000.restart_cice.nc --in rtofs_glo.t00z.n-24.restart_cice --tmp iced.2025-05-08-00000.nc`

**Note**: Optional `ktherm` was retained so that one can run CICE6 in a similar fashion as CICE4 with BL ice thermodynamics.
