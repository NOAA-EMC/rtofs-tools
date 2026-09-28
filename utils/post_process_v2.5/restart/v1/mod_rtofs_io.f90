module mod_rtofs_io
    use iso_c_binding, only: c_char, c_int, c_float, c_null_char
    implicit none

    ! Define the C function interface
    interface
        subroutine read_hycom_record_c(filename, irec, n2drec, array) bind(C, name="read_hycom_record_c")
            import :: c_char, c_int, c_float
            character(kind=c_char), intent(in) :: filename(*)
            integer(c_int), value, intent(in)  :: irec
            integer(c_int), value, intent(in)  :: n2drec
            real(c_float), intent(out)         :: array(*)
        end subroutine read_hycom_record_c
    end interface

contains

    !-------------------------------------------------------------------
    ! Reads a 2D unformatted record utilizing the C wrapper for >2GB seeks
    !-------------------------------------------------------------------
    subroutine read_hycom_record(file_name, irec_in, idm, jdm, fld)
        character(len=*), intent(in) :: file_name
        integer, intent(in)  :: irec_in, idm, jdm
        real(4), intent(out) :: fld(idm, jdm)
        
        integer :: n2drec, i, j, idx
        real(4), allocatable :: buf(:)
        character(kind=c_char, len=len_trim(file_name)+1) :: c_file_name

        ! Calculate HYCOM padded size in words
        n2drec = ((idm * jdm + 4095) / 4096) * 4096
        allocate(buf(n2drec))

        ! Append null terminator for C string compatibility
        c_file_name = trim(file_name) // c_null_char

        ! Call the 64-bit C wrapper
        call read_hycom_record_c(c_file_name, irec_in, n2drec, buf)

        ! Map 1D buffer to 2D array, zeroing out huge missing values
        do j = 1, jdm
            do i = 1, idm
                idx = i + (j - 1) * idm
                if (buf(idx) > 1.0e20) then
                    fld(i,j) = 0.0
                else
                    fld(i,j) = buf(idx)
                end if
            end do
        end do
        deallocate(buf)
    end subroutine read_hycom_record

    !-------------------------------------------------------------------
    ! Grid Extractors (Arctic Patch Logic)
    !-------------------------------------------------------------------
    subroutine extrct_p(work, n, m, array, no, mo)
        integer, intent(in) :: n, m, no, mo
        real(4), intent(in) :: work(n,m)
        real(4), intent(out):: array(no,mo)
        integer :: i, iw, j, jw

        do j=1, mo
            jw = j
            if (j <= m) then
                do i=1, no
                    iw = mod(i-1, n) + 1
                    array(i,j) = work(iw,jw)
                end do
            else
                jw = m - 1 - (j - m)
                do i=1, no
                    iw = n - mod(i-1, n)
                    array(i,j) = work(iw,jw)
                end do
            end if
        end do
    end subroutine extrct_p

    subroutine extrct_u(work, n, m, array, no, mo)
        integer, intent(in) :: n, m, no, mo
        real(4), intent(in) :: work(n,m)
        real(4), intent(out):: array(no,mo)
        integer :: i, iw, j, jw

        do j=1, mo
            jw = j
            if (j <= m) then
                do i=1, no
                    iw = mod(i-1, n) + 1
                    array(i,j) = work(iw,jw)
                end do
            else
                jw = m - 1 - (j - m)
                do i=1, no
                    iw = mod(n - mod(i-1, n), n) + 1
                    array(i,j) = -work(iw,jw)
                end do
            end if
        end do
    end subroutine extrct_u

    subroutine extrct_v(work, n, m, array, no, mo)
        integer, intent(in) :: n, m, no, mo
        real(4), intent(in) :: work(n,m)
        real(4), intent(out):: array(no,mo)
        integer :: i, iw, j, jw

        do j=1, mo
            jw = j
            if (j <= m) then
                do i=1, no
                    iw = mod(i-1, n) + 1
                    array(i,j) = work(iw,jw)
                end do
            else
                jw = m - (j - m)
                do i=1, no
                    iw = n - mod(i-1, n)
                    array(i,j) = -work(iw,jw)
                end do
            end if
        end do
    end subroutine extrct_v

    !-------------------------------------------------------------------
    ! Arakawa Grid Shifting (HYCOM to MOM6)
    ! Logic derived from HYCOM-tools/archv2mom6res
    !-------------------------------------------------------------------
    subroutine shift_to_mom6_u(f_in, idm, jdm, kdm, f_out, ntq, mto)
        integer, intent(in) :: idm, jdm, kdm, ntq, mto
        real(4), intent(in) :: f_in(idm, jdm, kdm)
        real(4), intent(out):: f_out(ntq, mto, kdm)
        integer :: i, ia, j, k

        do k = 1, kdm
            do j = 1, mto
                do i = 1, ntq
                    ia = mod(i, ntq) + 1  ! Periodic wrap mapping
                    f_out(i,j,k) = f_in(ia,j,k)
                end do
            end do
        end do
    end subroutine shift_to_mom6_u

    subroutine shift_to_mom6_v(f_in, idm, jdm, kdm, f_out, nto, mtq)
        integer, intent(in) :: idm, jdm, kdm, nto, mtq
        real(4), intent(in) :: f_in(idm, jdm, kdm)
        real(4), intent(out):: f_out(nto, mtq, kdm)
        integer :: i, j, k

        do k = 1, kdm
            do j = 1, min(mtq, jdm-1)
                do i = 1, nto
                    f_out(i,j,k) = f_in(i,j+1,k) ! Vertical shift
                end do
            end do
            do i = 1, nto
                f_out(i,1,k) = 0.0 ! South boundary padding
            end do
        end do
    end subroutine shift_to_mom6_v

end module mod_rtofs_io
