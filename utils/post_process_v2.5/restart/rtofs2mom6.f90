program rtofs2mom6
    use netcdf
    use mod_rtofs_io
    implicit none

    ! --- HYCOM Grid Dimensions ---
    integer, parameter :: rtofs_idm = 4500
    integer, parameter :: rtofs_jdm = 3298
    integer, parameter :: rtofs_kdm = 41

    ! --- MOM6 Grid Dimensions (larctic=T, lsymetr=F) ---
    integer, parameter :: nto = rtofs_idm
    integer, parameter :: mto = rtofs_jdm - 1
    integer, parameter :: ntq = rtofs_idm
    integer, parameter :: mtq = rtofs_jdm - 1

    ! --- String & Path Buffers ---
    character(len=512) :: flnm_i, flnm_b, flnm_depth, outdir
    character(len=512) :: flnm_temp, flnm_salt, flnm_h, flnm_u, flnm_v
    character(len=512) :: arg, text_line, date_str

    integer :: num_args, i, j, k, iu_b, irec, ierr
    integer :: fid_out, varid
    integer, allocatable :: rec_u(:), rec_v(:), rec_dp(:), rec_temp(:), rec_saln(:)
    integer :: rec_ubavg, rec_vbavg, rec_srfht
    real(4) :: q

    ! --- Date & Time Calculation Variables ---
    integer :: iyyyy, imm, idd, ihh, doy, nleap
    integer :: dpm(12) = (/ 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 /)
    real(8) :: rtofs_dayout

    ! --- Dynamic Arrays (Heap Allocated) ---
    real(4), allocatable :: srfht_hycom(:,:), depths_hycom(:,:)
    real(4), allocatable :: ubavg_hycom(:,:), vbavg_hycom(:,:)
    real(4), allocatable :: rtofs_u_hycom(:,:,:), rtofs_v_hycom(:,:,:)
    real(4), allocatable :: rtofs_dp_hycom(:,:,:), rtofs_t_hycom(:,:,:), rtofs_s_hycom(:,:,:)
    
    real(4), allocatable :: rtofs_t_mom(:,:,:), rtofs_s_mom(:,:,:), rtofs_dp_mom(:,:,:)
    real(4), allocatable :: rtofs_u_mom(:,:,:), rtofs_v_mom(:,:,:)

    ! -------------------------------------------------------------------
    ! 1. Parse Command Line Arguments
    ! -------------------------------------------------------------------
    num_args = command_argument_count()
    if (num_args == 0) stop "Usage: ./rtofs2mom6 --in <restart.a> --depth <regional.depth.a> --outdir <dir> --date YYYYMMDD[HH]"

    i = 1
    do while (i <= num_args)
        call get_command_argument(i, arg)
        if (trim(arg) == "--in") then
            call get_command_argument(i+1, flnm_i); i = i + 2
        else if (trim(arg) == "--depth") then
            call get_command_argument(i+1, flnm_depth); i = i + 2
        else if (trim(arg) == "--outdir") then
            call get_command_argument(i+1, outdir); i = i + 2
        else if (trim(arg) == "--date") then
            call get_command_argument(i+1, date_str)
            read(date_str(1:4), *) iyyyy
            read(date_str(5:6), *) imm
            read(date_str(7:8), *) idd
            if (len_trim(date_str) >= 10) then
                read(date_str(9:10), *) ihh
            else
                ihh = 0
            end if
            i = i + 2
        else
            i = i + 1
        end if
    end do

    ! --- Calculate FMS Julian Time ---
    if (mod(iyyyy, 4) == 0) dpm(2) = 29
    doy = sum(dpm(1:imm-1)) + idd
    nleap = (iyyyy - 1) / 4
    rtofs_dayout = real((iyyyy - 1) * 365 + nleap + doy - 1, 8) + (real(ihh, 8) / 24.0d0)

    flnm_temp = trim(outdir) // "/MOM.res.nc"
    flnm_salt = trim(outdir) // "/MOM.res_1.nc"
    flnm_h    = trim(outdir) // "/MOM.res_2.nc"
    flnm_u    = trim(outdir) // "/MOM.res_3.nc"
    flnm_v    = trim(outdir) // "/MOM.res_4.nc"

    print *, "RTOFS to MOM6 Conversion Initialized (Pure HYCOM Depths)."

    ! -------------------------------------------------------------------
    ! 2. Allocate Arrays
    ! -------------------------------------------------------------------
    allocate(rec_u(rtofs_kdm), rec_v(rtofs_kdm), rec_dp(rtofs_kdm), rec_temp(rtofs_kdm), rec_saln(rtofs_kdm))
    
    allocate(srfht_hycom(rtofs_idm, rtofs_jdm), depths_hycom(rtofs_idm, rtofs_jdm))
    allocate(ubavg_hycom(rtofs_idm, rtofs_jdm), vbavg_hycom(rtofs_idm, rtofs_jdm))
    allocate(rtofs_u_hycom(rtofs_idm, rtofs_jdm, rtofs_kdm), rtofs_v_hycom(rtofs_idm, rtofs_jdm, rtofs_kdm))
    allocate(rtofs_dp_hycom(rtofs_idm, rtofs_jdm, rtofs_kdm))
    allocate(rtofs_t_hycom(rtofs_idm, rtofs_jdm, rtofs_kdm), rtofs_s_hycom(rtofs_idm, rtofs_jdm, rtofs_kdm))
    
    allocate(rtofs_t_mom(nto, mto, rtofs_kdm), rtofs_s_mom(nto, mto, rtofs_kdm), rtofs_dp_mom(nto, mto, rtofs_kdm))
    allocate(rtofs_u_mom(ntq, mto, rtofs_kdm), rtofs_v_mom(nto, mtq, rtofs_kdm))

    ! -------------------------------------------------------------------
    ! 3. Parse HYCOM RESTART .b File
    ! -------------------------------------------------------------------
    rec_ubavg = 0; rec_vbavg = 0; rec_srfht = 0
    flnm_b = flnm_i(1:len_trim(flnm_i)-2) // '.b'
    open(newunit=iu_b, file=trim(flnm_b), status='old', action='read')
    irec = 0; k = 1
    do
        read(iu_b, '(A)', iostat=ierr) text_line
        if (ierr /= 0) exit
        if (index(text_line, 'RESTART2:') > 0) cycle
        
        irec = irec + 1
        if (index(text_line, 'srfht   :') == 1 .and. rec_srfht == 0) rec_srfht = irec
        if (index(text_line, 'ubavg   :') == 1 .and. rec_ubavg == 0) rec_ubavg = irec
        if (index(text_line, 'vbavg   :') == 1 .and. rec_vbavg == 0) rec_vbavg = irec
        if (index(text_line, 'u       :') == 1 .and. index(text_line, '  1  ') > 0) rec_u(k) = irec
        if (index(text_line, 'v       :') == 1 .and. index(text_line, '  1  ') > 0) rec_v(k) = irec
        if (index(text_line, 'dp      :') == 1 .and. index(text_line, '  1  ') > 0) rec_dp(k) = irec
        if (index(text_line, 'temp    :') == 1 .and. index(text_line, '  1  ') > 0) rec_temp(k) = irec
        if (index(text_line, 'saln    :') == 1 .and. index(text_line, '  1  ') > 0) then
            rec_saln(k) = irec
            k = k + 1
        end if
    end do
    close(iu_b)

    ! -------------------------------------------------------------------
    ! 4. Read Binary Data via 64-bit C Wrapper
    ! -------------------------------------------------------------------
    call read_hycom_record(flnm_depth, 1, rtofs_idm, rtofs_jdm, depths_hycom)
    call read_hycom_record(flnm_i, rec_srfht, rtofs_idm, rtofs_jdm, srfht_hycom)
    call read_hycom_record(flnm_i, rec_ubavg, rtofs_idm, rtofs_jdm, ubavg_hycom)
    call read_hycom_record(flnm_i, rec_vbavg, rtofs_idm, rtofs_jdm, vbavg_hycom)
    do k = 1, rtofs_kdm
        call read_hycom_record(flnm_i, rec_dp(k), rtofs_idm, rtofs_jdm, rtofs_dp_hycom(:,:,k))
        call read_hycom_record(flnm_i, rec_temp(k), rtofs_idm, rtofs_jdm, rtofs_t_hycom(:,:,k))
        call read_hycom_record(flnm_i, rec_saln(k), rtofs_idm, rtofs_jdm, rtofs_s_hycom(:,:,k))
        call read_hycom_record(flnm_i, rec_u(k), rtofs_idm, rtofs_jdm, rtofs_u_hycom(:,:,k))
        call read_hycom_record(flnm_i, rec_v(k), rtofs_idm, rtofs_jdm, rtofs_v_hycom(:,:,k))
    end do

    ! -------------------------------------------------------------------
    ! 5. Apply Alan's Math: SSH Correction, Land Mask, & Total Velocity
    ! -------------------------------------------------------------------
    do j = 1, rtofs_jdm
        do i = 1, rtofs_idm
            ! 'ip' check: Valid depths are > 0.0 and less than missing value
            if (depths_hycom(i,j) > 0.0 .and. depths_hycom(i,j) < 1.0e20) then
                ! SSH Correction to thicknesses
                q = (srfht_hycom(i,j)/9.806 + depths_hycom(i,j)) / depths_hycom(i,j)
                do k = 1, rtofs_kdm
                    rtofs_dp_hycom(i,j,k) = (rtofs_dp_hycom(i,j,k) / 9806.0) * q
                    rtofs_u_hycom(i,j,k) = rtofs_u_hycom(i,j,k) + ubavg_hycom(i,j)
                    rtofs_v_hycom(i,j,k) = rtofs_v_hycom(i,j,k) + vbavg_hycom(i,j)
                end do
            else
                ! Land Point: Explicitly zero everything
                do k = 1, rtofs_kdm
                    rtofs_dp_hycom(i,j,k) = 0.0
                    rtofs_t_hycom(i,j,k)  = 0.0
                    rtofs_s_hycom(i,j,k)  = 0.0
                    rtofs_u_hycom(i,j,k)  = 0.0
                    rtofs_v_hycom(i,j,k)  = 0.0
                end do
            end if
        end do
    end do

    ! -------------------------------------------------------------------
    ! 6. Clip to MOM6 Domain & Apply C-Grid Shifts
    ! -------------------------------------------------------------------
    rtofs_t_mom(:,:,:)  = rtofs_t_hycom(:, 1:mto, :)
    rtofs_s_mom(:,:,:)  = rtofs_s_hycom(:, 1:mto, :)
    rtofs_dp_mom(:,:,:) = rtofs_dp_hycom(:, 1:mto, :)

    call shift_to_mom6_u(rtofs_u_hycom, rtofs_idm, rtofs_jdm, rtofs_kdm, rtofs_u_mom, ntq, mto)
    call shift_to_mom6_v(rtofs_v_hycom, rtofs_idm, rtofs_jdm, rtofs_kdm, rtofs_v_mom, nto, mtq)

    ! -------------------------------------------------------------------
    ! 7. WRITE HYCOM DATA DIRECTLY TO NETCDF
    ! -------------------------------------------------------------------
    print *, "Writing PURE HYCOM data directly to MOM6 Restart Shells..."

    ! Thickness (h)
    ierr = nf90_open(trim(flnm_h), NF90_WRITE, fid_out);    call check_nc(ierr, "Open " // trim(flnm_h))
    ierr = nf90_inq_varid(fid_out, "h", varid)
    if (ierr /= NF90_NOERR) ierr = nf90_inq_varid(fid_out, "layer_depth", varid)
    ierr = nf90_put_var(fid_out, varid, rtofs_dp_mom);      call write_time(fid_out, rtofs_dayout)
    ierr = nf90_close(fid_out)
    
    ! Temp
    ierr = nf90_open(trim(flnm_temp), NF90_WRITE, fid_out); call check_nc(ierr, "Open " // trim(flnm_temp))
    ierr = nf90_inq_varid(fid_out, "Temp", varid);          call check_nc(ierr, "Find Temp")
    ierr = nf90_put_var(fid_out, varid, rtofs_t_mom);       call write_time(fid_out, rtofs_dayout)
    ierr = nf90_close(fid_out)

    ! Salt
    ierr = nf90_open(trim(flnm_salt), NF90_WRITE, fid_out); call check_nc(ierr, "Open " // trim(flnm_salt))
    ierr = nf90_inq_varid(fid_out, "Salt", varid);          call check_nc(ierr, "Find Salt")
    ierr = nf90_put_var(fid_out, varid, rtofs_s_mom);       call write_time(fid_out, rtofs_dayout)
    ierr = nf90_close(fid_out)

    ! u-velocity
    ierr = nf90_open(trim(flnm_u), NF90_WRITE, fid_out);    call check_nc(ierr, "Open " // trim(flnm_u))
    ierr = nf90_inq_varid(fid_out, "u", varid);             call check_nc(ierr, "Find u")
    ierr = nf90_put_var(fid_out, varid, rtofs_u_mom);       call write_time(fid_out, rtofs_dayout)
    ierr = nf90_close(fid_out)

    ! v-velocity
    ierr = nf90_open(trim(flnm_v), NF90_WRITE, fid_out);    call check_nc(ierr, "Open " // trim(flnm_v))
    ierr = nf90_inq_varid(fid_out, "v", varid);             call check_nc(ierr, "Find v")
    ierr = nf90_put_var(fid_out, varid, rtofs_v_mom);       call write_time(fid_out, rtofs_dayout)
    ierr = nf90_close(fid_out)

    print *, "SUCCESS: MOM6 Restart Files Populated!"

    ! --- Cleanup ---
    deallocate(rec_u, rec_v, rec_dp, rec_temp, rec_saln)
    deallocate(srfht_hycom, depths_hycom, ubavg_hycom, vbavg_hycom)
    deallocate(rtofs_u_hycom, rtofs_v_hycom, rtofs_dp_hycom, rtofs_t_hycom, rtofs_s_hycom)
    deallocate(rtofs_t_mom, rtofs_s_mom, rtofs_dp_mom, rtofs_u_mom, rtofs_v_mom)

contains
    subroutine check_nc(status, msg)
        integer, intent(in) :: status
        character(len=*), intent(in) :: msg
        if (status /= NF90_NOERR) then
            print *, "FATAL: ", trim(msg), " - ", trim(nf90_strerror(status))
            stop 1
        end if
    end subroutine check_nc

    subroutine write_time(ncid, time_val)
        integer, intent(in) :: ncid
        real(8), intent(in) :: time_val
        integer :: varid_t, status
        status = nf90_inq_varid(ncid, "Time", varid_t)
        if (status == NF90_NOERR) status = nf90_put_var(ncid, varid_t, (/ time_val /))
    end subroutine write_time

end program rtofs2mom6
