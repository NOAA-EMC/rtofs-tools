program rtofs2mom6
    implicit none

    ! =========================================================================
    ! VARIABLE DECLARATIONS
    ! =========================================================================
    ! Filenames
    character(len=256) :: flnm_i_a, flnm_i_b   ! HYCOM archive .a and .b
    character(len=256) :: flnm_depth           ! regional.depth.a
    
    ! MOM6 restart files
    character(len=256) :: flnm_rt, & ! Temperature (MOM.res.nc)
                          flnm_rs, & ! Salinity (MOM.res_1.nc)
                          flnm_rh, & ! Layer thickness (MOM.res_2.nc)
                          flnm_ru, & ! u-velocity (MOM.res_3.nc)
                          flnm_rv    ! v-velocity (MOM.res_4.nc)

    character(len=256) :: text_line
    character(len=256) :: arg_str

    ! HYCOM Grid Dimensions
    integer, parameter :: idm = 4500
    integer, parameter :: jdm = 3298
    integer, parameter :: kdm = 41

    ! MOM6 Grid Dimensions (larctic=T, lsymetr=F)
    integer, parameter :: nto = idm
    integer, parameter :: mto = jdm - 1
    integer, parameter :: ntq = idm
    integer, parameter :: mtq = jdm - 1

    ! Missing value marker
    real, parameter :: misval = -1.e20

    ! Output day
    real :: dayout

    ! Loop and Record indices
    integer :: i, j, k, irec, ios
    integer :: rec_srfht, rec_ubavg, rec_vbavg
    integer, allocatable :: rec_u(:), rec_v(:), rec_dp(:), rec_temp(:), rec_saln(:)

    ! HYCOM data arrays
    real, allocatable :: srfht(:,:), ubavg(:,:), vbavg(:,:), depths(:,:)
    real, allocatable :: dp(:,:,:), temp(:,:,:), saln(:,:,:)
    real, allocatable :: u(:,:,:), v(:,:,:)
    integer, allocatable :: ip(:,:), iu(:,:), iv(:,:) ! Explicit C-Grid Masks

    ! =========================================================================
    ! 1. INITIALIZATION & SETUP
    ! =========================================================================

    ! Read command-line positional parameters
    ! Usage: ./rtofs2mom6 <archive.a> <archive.b> <regional.depth.a> <Temp.nc> <Salt.nc> <h.nc> <u.nc> <v.nc> <dayout>

    ! Check for correct number of arguments
    if (command_argument_count() < 9) then
        print *, "Usage: ./rtofs2mom6 <archive.a> <archive.b> <regional.depth.a> <Temp.nc> <Salt.nc> <h.nc> <u.nc> <v.nc> <dayout>"
        stop 1
    end if

    call get_command_argument(1, flnm_i_a)
    call get_command_argument(2, flnm_i_b)
    call get_command_argument(3, flnm_depth)
    call get_command_argument(4, flnm_rt)
    call get_command_argument(5, flnm_rs)
    call get_command_argument(6, flnm_rh)
    call get_command_argument(7, flnm_ru)
    call get_command_argument(8, flnm_rv)
    call get_command_argument(9, arg_str); read(arg_str, *) dayout

    ! Allocate Arrays
    allocate(rec_u(kdm), rec_v(kdm), rec_dp(kdm), rec_temp(kdm), rec_saln(kdm))
    
    allocate(srfht(idm,jdm), ubavg(idm,jdm), vbavg(idm,jdm), depths(idm,jdm))
    allocate(ip(idm,jdm), iu(idm,jdm), iv(idm,jdm))
    
    allocate(dp(idm,jdm,kdm), temp(idm,jdm,kdm), saln(idm,jdm,kdm))
    allocate(u(idm,jdm,kdm), v(idm,jdm,kdm))

    ! Initialize Masks
    ip = 0; iu = 0; iv = 0

    ! =========================================================================
    ! 2. READ & VALIDATE HYCOM ARCHIVE DATA
    ! =========================================================================
    print *, "Reading HYCOM Data..."

    ! A. Read regional.depth.a directly (Record 1)
    ! Intel '-assume byterecl' means recl is in bytes (4500 * 3298 * 4 bytes for real*4)
    open(10, file=trim(flnm_depth), access='direct', recl=idm*jdm*4, status='old')
    read(10, rec=1) depths
    close(10)

    ! B. Parse the .b file to find record numbers for variables
    rec_srfht = 0; rec_ubavg = 0; rec_vbavg = 0
    k = 1; irec = 0
    open(11, file=trim(flnm_i_b), status='old', action='read')
    do
        read(11, '(A)', iostat=ios) text_line
        if (ios /= 0) exit
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
    close(11)

    ! C. Read the .a file using direct access and the record indices
    open(12, file=trim(flnm_i_a), access='direct', recl=idm*jdm*4, status='old')
    read(12, rec=rec_srfht) srfht
    read(12, rec=rec_ubavg) ubavg
    read(12, rec=rec_vbavg) vbavg
    do k = 1, kdm
        read(12, rec=rec_dp(k)) dp(:,:,k)
        read(12, rec=rec_temp(k)) temp(:,:,k)
        read(12, rec=rec_saln(k)) saln(:,:,k)
        read(12, rec=rec_u(k)) u(:,:,k)
        read(12, rec=rec_v(k)) v(:,:,k)
    end do
    close(12)

    ! D. Validate Bathymetry & Derive C-Grid Masks
    do j = 1, jdm
        do i = 1, idm
            ! ip: Ocean cell if depth > 0 and not missing
            if (depths(i,j) > 0.0 .and. depths(i,j) < 1.0e20) then
                ip(i,j) = 1
                
                ! Abort if topographic sea point has a land value in srfht
                ! HYCOM land values are typically 2.0**100
                if (srfht(i,j) > 2.0**99) then
                    print *, "ERROR: Topo sea, srfht land at i,j = ", i, j
                    stop "Bathymetry/SSH validation failed."
                end if
            end if
        end do
    end do

    ! Derive Alan's implicit iu and iv masks exactly
    do j = 1, jdm
        do i = 1, idm
            ! iu is active if current AND western neighbor are ocean
            if (i > 1) then
                if (ip(i,j) == 1 .and. ip(i-1,j) == 1) iu(i,j) = 1
            else
                if (ip(i,j) == 1 .and. ip(idm,j) == 1) iu(i,j) = 1 ! Periodic wrap
            end if
            
            ! iv is active if current AND southern neighbor are ocean
            if (j > 1) then
                if (ip(i,j) == 1 .and. ip(i,j-1) == 1) iv(i,j) = 1
            end if
        end do
    end do

end program rtofs2mom6
