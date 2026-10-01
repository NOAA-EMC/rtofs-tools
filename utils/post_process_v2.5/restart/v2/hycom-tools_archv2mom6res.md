### Pseudocode of [archv2mom6res](https://github.com/HYCOM/HYCOM-tools/blob/master/archive/src/archv2mom6res.f)

1. INITIALIZATION & SETUP
   - Initialize HYCOM parallel subroutines (`xcspmd`) and array I/O (`zaiost`).
   - Read command-line positional parameters:
     - Input HYCOM archive file (`flnm_i`)
     - Output single MOM6 restart file (`flnm_o`)
     - Template NetCDF files for dimensions (`flnm_rt`, `flnm_ru`, `flnm_rv`)
     - Grid dimensions: `idm`, `jdm`, `kdm`, test points (`itest`, `jtest`), output model day (`dayout`), and grid flags (`symetr`, `arctic`).
   - Compute MOM6 grid dimensions based on flags:
     - If `larctic=True` and `lsymetr=False`:
       - `nto = idm`, `mto = jdm - 1`
       - `ntq = idm`, `mtq = jdm - 1`
   - Set missing value marker (`misval = -1.e20`).

2. READ & VALIDATE HYCOM ARCHIVE DATA
   - Load HYCOM topology and grid definitions (`regional.depth`).
   - Read binary archive file header (`getdat`) to extract `srfht`, `ubavg`, `vbavg`, `dp`, `temp`, `saln`, `u`, `v`.
   - Validate bathymetry against sea surface height:
     - Verify land/sea mask array (`ip`) matches valid non-land points in `srfht`.
     - Abort if topographic sea points contain land values in `srfht`.

3. CONVERT HYCOM FIELDS TO PHYSICAL UNITS & TOTAL VELOCITY
   - Loop over all 3D grid points `(i, j, k)`:
     - Velocity: If `iu(i,j) == 1`, set `u(i,j,k) = u(i,j,k) + ubaro(i,j)`, else set `0.0`.
     - Velocity: If `iv(i,j) == 1`, set `v(i,j,k) = v(i,j,k) + vbaro(i,j)`, else set `0.0`.
     - Layer Thickness: If `ip(i,j) == 1`, convert pressure units to meters (`dp / 9806.0`), else set `0.0`.
     - Tracers: If `ip(i,j) == 0`, explicitly zero out `temp` and `saln`.

4. APPLY SEA SURFACE HEIGHT (SSH) SQUEEZE
   - Loop over all 2D columns `(i, j)` where `ip(i,j) == 1`:
     - Calculate column stretch ratio: `q = (srfht(i,j)/9.806 + depths(i,j)) / depths(i,j)`
     - Scale layer thicknesses: `dp(i,j,k) = q * dp(i,j,k)` across all vertical layers `k`.

5. GRID INTERPOLATION / STAGGER SHIFTS TO MOM6 C-GRID
   - Allocate 4D output arrays (`u_mom`, `v_mom`, `temp_mom`, `saln_mom`, `h_mom`).
   - Map tracers & thickness (`h2m_p`):
     - Transfer `temp`, `saln`, and `dp` directly to `(nto, mto)` domain. Set missing points to `0.0`.
   - Shift u-velocity (`h2m_u`):
     - For non-symmetric grid (`lsymetr=False`), shift index horizontally: `ia = mod(i, ntq) + 1`. Map `u(ia, j, k)` to `u_mom(i, j, k)`.
   - Shift v-velocity (`h2m_v`):
     - For non-symmetric grid (`lsymetr=False`), shift index vertically: map `v(i, j+1, k)` to `v_mom(i, j, k)` for `j = 1..mtq`. Zero out row `j=1`.

6. NETCDF EXPORT
   - Open template restart files (`flnm_rt`, `flnm_ru`, `flnm_rv`) to read coordinate vectors (`lath`, `lonh`, `latq`, `lonq`, `Layer`).
   - Construct single output NetCDF file (`flnm_o`).
   - Define dimensions (`lath`, `lonh`, `latq`, `lonq`, `Layer`, `Time`) and NetCDF variables (`Temp`, `Salt`, `h`, `u`, `v`).
   - Write coordinate variables and model state arrays to disk.
   - Close file handles and deallocate memory.
