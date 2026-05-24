!======================================================================!
! Module: mod_wrf_writer
! Author: Yan Chen
! Date:   2026-05-24
! Purpose: Write WRF intermediate format files
!======================================================================!

module mod_wrf_writer

    implicit none
    
    ! WRF intermediate format data structure
    type :: tp_wrf_data
        ! Header information
        integer :: version = 5
        character(len=8) :: hdate
        character(len=8) :: xfcst
        character(len=8) :: map_source
        character(len=8) :: field
        character(len=8) :: units
        character(len=8) :: desc
        character(len=8) :: xlvl
        integer :: nx, ny
        integer :: iproj
        real :: startlat, startlon
        real :: deltalat, deltalon
        real :: dx, dy
        real :: xlonc, truelat1, truelat2
        integer :: nlats
        real :: earth_radius
        logical :: is_wind_grid_rel
        character(len=8) :: startloc
        integer :: nxl, nyl
        real, allocatable :: slab(:,:)
    end type tp_wrf_data
    
contains

!======================================================================!
! Subroutine: wrf_write
! Purpose: Write data to WRF intermediate format binary file
!======================================================================!

subroutine wrf_write(filename, wrf_data)

    implicit none
    character(len=*), intent(in) :: filename
    type(tp_wrf_data), intent(in) :: wrf_data
    
    integer :: iunit
    integer :: i, j
    integer :: date_char(8)
    integer :: xfcst_int
    integer :: iproj_int
    character(len=1) :: hdate_str(8)
    character(len=1) :: field_str(8)
    character(len=1) :: units_str(8)
    character(len=1) :: desc_str(8)
    character(len=1) :: xlvl_str(8)
    character(len=1) :: map_source_str(8)
    character(len=1) :: startloc_str(8)
    integer :: is_wind_grid_rel_int
    
    ! Open output file as unformatted (binary)
    iunit = 50
    open(unit=iunit, file=filename, form='unformatted', status='replace', &
         action='write', convert='little_endian')
    
    ! Write header as per WRF intermediate format specification
    ! Format: Record 1
    write(iunit) wrf_data%version
    
    ! Convert strings to character arrays
    call string_to_char8(wrf_data%hdate, hdate_str)
    write(iunit) hdate_str
    
    ! xfcst as integer
    read(wrf_data%xfcst, *) xfcst_int
    write(iunit) xfcst_int
    
    call string_to_char8(wrf_data%map_source, map_source_str)
    write(iunit) map_source_str
    
    call string_to_char8(wrf_data%field, field_str)
    write(iunit) field_str
    
    call string_to_char8(wrf_data%units, units_str)
    write(iunit) units_str
    
    call string_to_char8(wrf_data%desc, desc_str)
    write(iunit) desc_str
    
    call string_to_char8(wrf_data%xlvl, xlvl_str)
    write(iunit) xlvl_str
    
    ! Record 2
    write(iunit) wrf_data%nx, wrf_data%ny
    write(iunit) wrf_data%iproj
    
    ! Record 3
    write(iunit) wrf_data%startlat, wrf_data%startlon
    write(iunit) wrf_data%deltalat, wrf_data%deltalon
    write(iunit) wrf_data%dx, wrf_data%dy
    write(iunit) wrf_data%xlonc, wrf_data%truelat1, wrf_data%truelat2
    write(iunit) wrf_data%nlats
    
    ! Record 4
    write(iunit) wrf_data%earth_radius
    
    ! Convert logical to integer
    if (wrf_data%is_wind_grid_rel) then
        is_wind_grid_rel_int = 1
    else
        is_wind_grid_rel_int = 0
    end if
    write(iunit) is_wind_grid_rel_int
    
    call string_to_char8(wrf_data%startloc, startloc_str)
    write(iunit) startloc_str
    
    write(iunit) wrf_data%nxl, wrf_data%nyl
    
    ! Write the actual data slab
    ! Note: Data is written in row-major order (latitude varies fastest)
    do j = 1, wrf_data%ny
        do i = 1, wrf_data%nx
            write(iunit) wrf_data%slab(i, j)
        end do
    end do
    
    ! Close the file
    close(iunit)
    
    print*, "  Written: ", trim(filename)
    
end subroutine wrf_write

!======================================================================!
! Subroutine: string_to_char8
! Purpose: Convert string to 8-character array for binary output
!======================================================================!

subroutine string_to_char8(str, char8)

    implicit none
    character(len=*), intent(in) :: str
    character(len=1), intent(out) :: char8(8)
    
    integer :: i
    character(len=8) :: padded_str
    
    ! Pad or truncate string to 8 characters
    padded_str = str
    do i = len_trim(padded_str)+1, 8
        padded_str(i:i) = ' '
    end do
    
    ! Convert to character array
    do i = 1, 8
        char8(i) = padded_str(i:i)
    end do
    
end subroutine string_to_char8

!======================================================================!
! Subroutine: wrf_write_simple
! Purpose: Simplified write routine (alternative implementation)
!======================================================================!

subroutine wrf_write_simple(filename, wrf_data)

    implicit none
    character(len=*), intent(in) :: filename
    type(tp_wrf_data), intent(in) :: wrf_data
    
    integer :: iunit
    integer :: i, j
    
    iunit = 51
    open(unit=iunit, file=filename, form='unformatted', status='replace', &
         action='write', convert='little_endian')
    
    ! Write all header data in one record
    write(iunit) wrf_data%version
    write(iunit) wrf_data%hdate
    write(iunit) wrf_data%xfcst
    write(iunit) wrf_data%map_source
    write(iunit) wrf_data%field
    write(iunit) wrf_data%units
    write(iunit) wrf_data%desc
    write(iunit) wrf_data%xlvl
    write(iunit) wrf_data%nx, wrf_data%ny
    write(iunit) wrf_data%iproj
    write(iunit) wrf_data%startlat, wrf_data%startlon
    write(iunit) wrf_data%deltalat, wrf_data%deltalon
    write(iunit) wrf_data%dx, wrf_data%dy
    write(iunit) wrf_data%xlonc, wrf_data%truelat1, wrf_data%truelat2
    write(iunit) wrf_data%nlats
    write(iunit) wrf_data%earth_radius
    write(iunit) wrf_data%is_wind_grid_rel
    write(iunit) wrf_data%startloc
    write(iunit) wrf_data%nxl, wrf_data%nyl
    
    ! Write data slab
    write(iunit) wrf_data%slab
    
    close(iunit)
    
    print*, "Written: ", trim(filename)
    
end subroutine wrf_write_simple

end module mod_wrf_writer
