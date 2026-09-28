# HYCOM to MOM6 Restart Conversion
*Source: `HYCOM-tools/archv2mom6res`*

## 1. Initialization & Configuration
1. **Input Parsing**: Read paths for the input HYCOM archive (`.a`), output MOM6 restart directory, and target model date.
2. **Dimension Setup**: Ingest spatial dimensions (`idm`, `jdm`, `kdm`).
3. **MOM6 Grid Sizing**: Calculate MOM6 destination array sizes (`nto`, `mto`, `ntq`, `mtq`) based on HYCOM symmetry and Arctic bipolar patch flags.

## 2. Data Ingestion
1. **Parse HYCOM Binary**: Read the HYCOM `.a`/`.b` archives, bypassing 32-bit offset limits via C-wrappers, extracting 3D fields into memory:
   - Zonal Velocity (`u`).
   - Meridional Velocity (`v`).
   - Pressure Thickness (`dp`).
   - Temperature (`temp`).
   - Salinity (`saln`).
2. **Thickness Conversion**: Divide `dp` by 9806.0 to convert from pressure thickness (Pascals) to physical layer depth (meters).

## 3. Arakawa Grid Shifting
HYCOM utilizes an Arakawa C-grid with velocities on the South/West faces. MOM6 utilizes an Arakawa C-grid with velocities on the North/East faces.

1. **Tracer Shift (`h2m_p`)**: 
   - Operation: Direct 1:1 copy. Both models colocate mass/tracers at the cell center.
   - Result: `Temp`, `Saln`, and `h` map directly.
2. **Zonal Velocity Shift (`h2m_u`)**:
   - Operation: Shift index `i` by +1 to move West-face velocities to the East face of the neighboring cell.
   - Boundary: Apply periodic wrap-around (`mod(i, idm) + 1`) at the global seam.
3. **Meridional Velocity Shift (`h2m_v`)**:
   - Operation: Shift index `j` by +1 to move South-face velocities to the North face.
   - Boundary: Apply a 0.0 mask to the southernmost boundary row (`j=1`).

## 4. MOM6 Restart NetCDF File Updates
1. **Template Preparation**: Copy the entire suite of `MOM.res*.nc` base templates to the output directory, stripping date prefixes.
2. **Time Calculation**: Calculate the strict FMS Julian Calendar Day from the requested YYYYMMDD[HH] string (Epoch: 0001-01-01).
3. **Write Arrays**: Open the respective split NetCDF files (`MOM.res.nc`, `MOM.res_1.nc`, etc.), overwrite the `Time` vector, 
     and dump the converted 3D arrays (`Temp`, `Salt`, `h`, `u`, `v`) in place.
4. **Cleanup**: Close NetCDF handles and deallocate memory.
