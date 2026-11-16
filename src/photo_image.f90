module photo_image
    !! Photos as 8-bit sRGB arrays rgb(3, width, height), row 1 at the top.
    !!
    !! JPEG and PNG are decoded by fortimg; the EXIF orientation of a JPEG is
    !! read here and applied, so a photo taken upright is upright.
    use fortui_files, only: file_entry_t, file_list, file_join, file_read
    use iso_fortran_env, only: int8, int32
    use fortimg, only: fortimg_image_t => image_t, image_decode, image_convert, IMG_OK
    implicit none
    private

    type, public :: image_t
        integer :: width = 0, height = 0
        integer(int8), allocatable :: rgb(:, :, :)
    end type image_t

    public :: image_load, image_load_bytes, image_shrink, image_orient, exif_orientation, image_glob


contains

    subroutine image_load(path, img, message)
        !! Read a JPEG or PNG file; `message` is empty on success.
        character(*), intent(in) :: path
        type(image_t), intent(out) :: img
        character(len=:), allocatable, intent(out) :: message
        integer(int8), allocatable, target :: bytes(:)
        call file_read(path, bytes, message)
        if (len(message) > 0) return
        call image_load_bytes(bytes, img, message)
    end subroutine image_load

    subroutine image_load_bytes(bytes, img, message)
        integer(int8), intent(in) :: bytes(:)
        type(image_t), intent(out) :: img
        character(:), allocatable, intent(out) :: message
        integer :: n
        message = ""
        n = size(bytes)
        if (n < 8) then
            message = "This file is too short to be a photo."
            return
        end if
        if (byte(bytes, 1) == 255 .and. byte(bytes, 2) == 216) then
            call decode(bytes(1:n), "JPEG", img, message)
            if (len(message) == 0) call image_orient(img, exif_orientation(bytes(1:n)))
        else if (byte(bytes, 1) == 137 .and. byte(bytes, 2) == 80 .and. &
                byte(bytes, 3) == 78 .and. byte(bytes, 4) == 71) then
            call decode(bytes(1:n), "PNG", img, message)
        else
            message = "Choose a JPEG or PNG photo."
        end if
    end subroutine image_load_bytes

    subroutine decode(bytes, kind, img, message)
        integer(int8), intent(in) :: bytes(:)
        character(*), intent(in) :: kind
        type(image_t), intent(inout) :: img
        character(len=:), allocatable, intent(inout) :: message
        type(fortimg_image_t) :: f
        integer :: status

        call image_decode(bytes, f, status)
        if (status /= IMG_OK) then
            message = "not a readable " // kind
            return
        end if
        call image_convert(f, 3)
        img%width = f%width
        img%height = f%height
        call move_alloc(f%pixels, img%rgb)
    end subroutine decode

    pure integer function byte(b, k)
        integer(int8), intent(in) :: b(:)
        integer, intent(in) :: k
        byte = iand(int(b(k)), 255)
    end function byte

    pure integer function exif_orientation(b) result(o)
        !! EXIF orientation (1..8) of a JPEG, 1 when absent. Walks the markers to
        !! the APP1 "Exif" segment and reads tag 0x0112 of the first TIFF IFD.
        integer(int8), intent(in) :: b(:)
        integer :: p, seg, t, ifd, entries, k, e
        logical :: le

        o = 1
        p = 3
        do while (p + 3 <= size(b))
            if (byte(b, p) /= 255) return
            if (byte(b, p + 1) == 218 .or. byte(b, p + 1) == 217) return
            seg = 256 * byte(b, p + 2) + byte(b, p + 3)
            if (byte(b, p + 1) == 225 .and. p + 3 + seg <= size(b) .and. seg > 16) then
                if (all([(byte(b, p + 3 + k), k = 1, 6)] == [69, 120, 105, 102, 0, 0])) then
                    t = p + 10
                    le = byte(b, t) == 73
                    ifd = t + u32(b, t + 4, le)
                    if (ifd + 1 > size(b)) return
                    entries = u16(b, ifd, le)
                    do k = 0, entries - 1
                        e = ifd + 2 + 12 * k
                        if (e + 11 > size(b)) return
                        if (u16(b, e, le) == 274) then
                            o = u16(b, e + 8, le)
                            if (o < 1 .or. o > 8) o = 1
                            return
                        end if
                    end do
                    return
                end if
            end if
            p = p + 2 + seg
        end do
    end function exif_orientation

    pure integer function u16(b, p, le)
        integer(int8), intent(in) :: b(:)
        integer, intent(in) :: p
        logical, intent(in) :: le
        if (le) then
            u16 = byte(b, p) + 256 * byte(b, p + 1)
        else
            u16 = 256 * byte(b, p) + byte(b, p + 1)
        end if
    end function u16

    pure integer function u32(b, p, le)
        integer(int8), intent(in) :: b(:)
        integer, intent(in) :: p
        logical, intent(in) :: le
        if (le) then
            u32 = u16(b, p, le) + 65536 * u16(b, p + 2, le)
        else
            u32 = 65536 * u16(b, p, le) + u16(b, p + 2, le)
        end if
    end function u32

    subroutine image_orient(img, orientation)
        !! Apply an EXIF orientation: the stored pixel at column x, row y moves to
        !! where an upright view shows it.
        type(image_t), intent(inout), target :: img
        integer, intent(in) :: orientation
        integer(int8), allocatable :: out(:, :, :)
        integer :: w, h, x, y, nx, ny, ow, oh

        if (orientation <= 1 .or. orientation > 8) return
        w = img%width
        h = img%height
        if (orientation >= 5) then
            ow = h
            oh = w
        else
            ow = w
            oh = h
        end if
        allocate(out(3, ow, oh))
        !$omp parallel do private(x, nx, ny)
        do y = 1, h
            do x = 1, w
                select case (orientation)
                case (2)
                    nx = w - x + 1; ny = y
                case (3)
                    nx = w - x + 1; ny = h - y + 1
                case (4)
                    nx = x; ny = h - y + 1
                case (5)
                    nx = y; ny = x
                case (6)
                    nx = h - y + 1; ny = x
                case (7)
                    nx = h - y + 1; ny = w - x + 1
                case default
                    nx = y; ny = w - x + 1
                end select
                out(:, nx, ny) = img%rgb(:, x, y)
            end do
        end do
        !$omp end parallel do
        call move_alloc(out, img%rgb)
        img%width = ow
        img%height = oh
    end subroutine image_orient

    subroutine image_shrink(img, longest, small)
        !! Box-filtered copy whose longer side is at most `longest` pixels.
        type(image_t), intent(in) :: img
        integer, intent(in) :: longest
        type(image_t), intent(out) :: small
        integer :: f, x, y, i, j, c
        real :: acc(3)

        f = max(1, (max(img%width, img%height) + longest - 1) / longest)
        small%width = max(1, img%width / f)
        small%height = max(1, img%height / f)
        allocate(small%rgb(3, small%width, small%height))
        !$omp parallel do private(x, i, j, c, acc)
        do y = 1, small%height
            do x = 1, small%width
                acc = 0.0
                do j = (y - 1) * f + 1, y * f
                    do i = (x - 1) * f + 1, x * f
                        do c = 1, 3
                            acc(c) = acc(c) + real(iand(int(img%rgb(c, i, j)), 255))
                        end do
                    end do
                end do
                acc = acc / real(f * f)
                small%rgb(:, x, y) = int(nint(acc) - merge(256, 0, nint(acc) > 127), int8)
            end do
        end do
        !$omp end parallel do
    end subroutine image_shrink

    subroutine image_glob(dir, paths)
        !! JPEG and PNG files in `dir`, sorted by name.
        character(*), intent(in) :: dir
        character(len=:), allocatable, intent(out) :: paths(:)
        type(file_entry_t), allocatable :: entries(:)
        character(:), allocatable :: message
        integer :: k, n, longest
        call file_list(dir, entries, message, ".jpg,.jpeg,.png")
        n = 0
        longest = 1
        do k = 1, size(entries)
            if (entries(k)%directory) cycle
            n = n + 1
            longest = max(longest, len(file_join(dir, entries(k)%name)))
        end do
        allocate(character(longest) :: paths(n))
        n = 0
        do k = 1, size(entries)
            if (entries(k)%directory) cycle
            n = n + 1
            paths(n) = file_join(dir, entries(k)%name)
        end do
    end subroutine image_glob

end module photo_image
