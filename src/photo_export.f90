module photo_export
    !! Writes the edited photo next to the original as JPEG or PNG without
    !! replacing an existing file, and offers it (a download in the browser).
    use iso_fortran_env, only: int8
    use fortimg, only: fortimg_image_t => image_t, jpeg_encode, png_encode
    use fortui, only: ui_offer_file, ui_str
    use fortui_files, only: file_write, file_is_readable
    implicit none
    private
    public :: export_photo, export_name

contains

    function export_name(source, kind, taken) result(name)
        !! "photo-edited.jpg", then "photo-edited-2.jpg" and so on while a
        !! name is taken.
        character(*), intent(in) :: source, kind
        interface
            logical function taken(path)
                character(*), intent(in) :: path
            end function taken
        end interface
        character(len=:), allocatable :: name, stem
        integer :: dot, slash, k
        dot = index(source, ".", back=.true.)
        slash = index(source, "/", back=.true.)
        stem = source
        if (dot > slash + 1) stem = source(1:dot - 1)
        if (len(stem) == 0) stem = "photo"
        name = stem // "-edited." // kind
        k = 1
        do while (taken(name))
            k = k + 1
            name = stem // "-edited-" // ui_str(k) // "." // kind
        end do
    end function export_name

    subroutine export_photo(rgb, source, kind, dpi, target, message)
        integer(int8), intent(in) :: rgb(:, :, :)
        character(*), intent(in) :: source, kind
        integer, intent(in) :: dpi
        character(len=:), allocatable, intent(out) :: target, message
        type(fortimg_image_t) :: img
        integer(int8), allocatable :: bytes(:)
        img%width = size(rgb, 2)
        img%height = size(rgb, 3)
        img%channels = 3
        img%pixels = rgb
        if (kind == "png") then
            call png_encode(img, bytes)
        else
            call jpeg_encode(img, bytes, quality=92, dpi=dpi)
        end if
        target = export_name(source, kind, file_is_readable)
        call file_write(target, bytes, message)
        if (len(message) == 0) call ui_offer_file(target)
    end subroutine export_photo
end module photo_export
