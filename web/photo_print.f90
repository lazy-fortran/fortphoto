module photo_print
    !! The browser has no printer protocol: the print button stays disabled
    !! and exports (PNG or JPEG downloads) serve printing.
    use iso_fortran_env, only: int8
    implicit none
    private
    public :: print_photo_set, print_view, print_hold, print_supported
    logical :: print_hold = .false.
    logical, parameter :: print_supported = .false.
contains
    subroutine print_photo_set(rgb, name)
        integer(int8), intent(in) :: rgb(:, :, :)
        character(*), intent(in) :: name
        if (size(rgb) < 0 .or. len(name) < 0) continue
    end subroutine print_photo_set
    subroutine print_view(leave)
        logical, intent(inout) :: leave
        leave = .true.
    end subroutine print_view
end module photo_print
