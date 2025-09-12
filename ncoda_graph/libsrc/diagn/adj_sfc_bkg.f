      subroutine adj_sfc_bkg (out_dir, dtg, var_name, adj_tau, n_obs, 
     *                        n_data, lat, lon, typ, bkg, eob, spval)
c
c.............................START PROLOGUE............................
c
c CONFIGURATION IDENTIFICATION:
c      $HeadURL$
c      @(#)$Id$
c
c MODULE NAME:  adj_sfc_bkg
c
c DESCRIPTION:  retrieves background information for raw sfc data
c
c LIBRARIES OF RESIDENCE:   ...ops/lib/libcoda.a
c
c PARAMETERS:
c    Name            Type       Usage            Description
c   --------      ---------   -------   ------------------------------
c
c....................MAINTENANCE SECTION................................
c
c METHOD:
c
c RECORD OF CHANGES:
c   Initial Installation - April 1994 -- Cummings, J.
c
c..............................END PROLOGUE.............................
c
      implicit  none
c
c     ..set global hycom dimensions
c
      integer    n_lon
      parameter (n_lon = 4500)
      integer    n_lat
      parameter (n_lat = 3298)
c
c     ..local array dimensions
c
      integer   n_obs     
c
c     ..define work unit number
c
      integer    UNIT
      parameter (UNIT = 20)
c
      integer   adj_tau
      real      bkg (n_obs)
      real      eob (n_obs)
      character dtg * 10
      character file_dtg * 10
      integer   i
      real      lat (n_obs)
      real      lon (n_obs)
      integer   len
      integer   nest
      integer   n_data
      integer   n_sfc
      integer   n_strt
      character out_dir * (*)
      logical   rd_fail
      real      spval
      integer   status
      integer   tau, tau_hr
      integer   typ (n_obs)
      integer   upd
      character var_name * 6
c
c     ..data types
c
      include 'coda_types.h'
c
c     ..allocatable grid arrays
c
      logical,  allocatable :: fail (:)
      real,     allocatable :: fld (:)
      real,     allocatable :: grd_lat (:)
      real,     allocatable :: grd_lon (:)
c
c     ..allocatable data arrays
c
      real,     allocatable :: xi (:)
      real,     allocatable :: yj (:)
c
c...............................executable..............................
c
c     ..set grid parameters
c
      nest = 1
      n_sfc = 1
      n_strt = 1
      tau = 0
      tau_hr = adj_tau + 24
      upd = 24
c
c     ..initialize return variables
c
      bkg = spval
      eob = spval
c
c     ..allocate arrays
c
      allocate (fail (n_data))
      allocate (fld (n_lon * n_lat))
      allocate (grd_lat (n_lon * n_lat))
      allocate (grd_lon (n_lon * n_lat))
      allocate (xi (n_data))
      allocate (yj (n_data))
c
c     ..retrieve global grid arrays
c
      call rd_coda_file (out_dir, dtg, nest, n_lon, n_lat, n_sfc,
     *                   'datafld', 'grdlat', 'o', tau, 'sfc', 
     *                   grd_lat, .true., rd_fail)
      if (rd_fail) then
         write (*, '(''grdlat missing: '', a, 2x, a)') 
     *          trim (out_dir), dtg 
         stop
      endif
      call rd_coda_file (out_dir, dtg, nest, n_lon, n_lat, n_sfc,
     *                   'datafld', 'grdlon', 'o', tau, 'sfc',
     *                   grd_lon, .true., rd_fail)
      if (rd_fail) then
         write (*, '(''grdlon missing: '', a, 2x, a)') 
     *          trim (out_dir), dtg 
         stop
      endif
c
c     ..index obs onto global grid
c
      call irreg_ll2ij (n_lon, n_lat, grd_lat, grd_lon, n_data,
     *                  lat, lon, xi, yj)
c
c     ..set adjoint forecast base dtg
c
      call dtgmod (dtg, -tau_hr, file_dtg, status)
c
c     ..read forecast background
c
      call rd_coda_file (out_dir, file_dtg, nest, n_lon, n_lat, 
     *                   n_sfc, 'fcstfld', var_name, 'o', upd,
     *                   'sfc', fld, .true., rd_fail)
      if (rd_fail) then
         write (*, '(''sfc forecast missing: '', a, 2x, a, 2x, i3)') 
     *          trim (out_dir), file_dtg, adj_tau
         stop
      endif
c
c     ..interpolate to obs locations
c
      call fld2_trp (n_obs, n_strt, n_data, xi, yj, n_lon, n_lat,
     *               fld, spval, bkg, fail)
c
c     ..set observation error
c
      do i = 1, n_data
         if (.not. fail(i)) then
            eob(i) = inst_err(typ(i))
         endif
      enddo
c
c     ..clean up
c
      deallocate (fail, fld, grd_lat, grd_lon, xi, yj)
c
      return
      end
