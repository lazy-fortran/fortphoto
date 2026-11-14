program test_photo
    !! Layout arithmetic worked out by hand, the rendered page against it,
    !! quarter turns, EXIF orientation parsing and applying, and decoding of
    !! small JPEG/PNG fixtures (test/fixtures/README.md).
    use iso_fortran_env, only: int8
    use photo_layout, only: layout_t, layout_compute, layout_render, rotate_cw, &
        layout_offset_by, MODE_FILL, MODE_FIT
    use photo_image, only: image_t, image_load, image_orient, exif_orientation, &
        image_shrink, image_glob
    implicit none

    integer :: failures = 0

    call test_fill_fit()
    call test_render()
    call test_rotate()
    call test_exif()
    call test_decode()

    if (failures > 0) then
        print '(i0,a)', failures, " photo checks failed"
        error stop 1
    end if
    print '(a)', " fortphoto tests passed"

contains

    subroutine check(ok, what)
        logical, intent(in) :: ok
        character(*), intent(in) :: what
        if (.not. ok) then
            failures = failures + 1
            print '(a)', "FAIL: " // what
        end if
    end subroutine check

    logical function near(a, b)
        real, intent(in) :: a, b
        near = abs(a - b) < 1e-3 * max(1.0, abs(b))
    end function near

    subroutine test_fill_fit()
        !! A 4:3 photo (400 x 300) on a 3:2 sheet (600 x 400) without margins.
        !! Fill: scale max(1.5, 4/3) = 1.5, photo 600 x 450, 50 rows cut, 1/9 lost.
        !! Fit: scale 4/3, photo 533.3 x 400 centred, 33.3 white on each side.
        type(layout_t) :: lay
        real :: ox, oy

        lay = layout_compute(400, 300, 600.0, 400.0, [0.0, 0.0, 0.0, 0.0], MODE_FILL, &
            1.0, 0.0, 0.0)
        call check(near(lay%pw, 600.0) .and. near(lay%ph, 450.0), "fill size")
        call check(near(lay%px, 0.0) .and. near(lay%py, -25.0), "fill centred")
        call check(near(lay%kept, 8.0 / 9.0), "fill keeps 8/9 of a 4:3 photo on 3:2")
        lay = layout_compute(400, 300, 600.0, 400.0, [0.0, 0.0, 0.0, 0.0], MODE_FILL, &
            1.0, 0.0, -1.0)
        call check(near(lay%py, 0.0), "offset -1 keeps the top edge")
        lay = layout_compute(400, 300, 600.0, 400.0, [0.0, 0.0, 0.0, 0.0], MODE_FILL, &
            1.0, 0.0, 1.0)
        call check(near(lay%py, -50.0), "offset +1 keeps the bottom edge")
        lay = layout_compute(400, 300, 600.0, 400.0, [0.0, 0.0, 0.0, 0.0], MODE_FILL, &
            1.2, 0.0, 0.0)
        call check(near(lay%pw, 720.0) .and. near(lay%px, -60.0), "zoom 1.2 crops more")
        lay = layout_compute(400, 300, 600.0, 400.0, [0.0, 0.0, 0.0, 0.0], MODE_FIT, &
            1.5, 1.0, 1.0)
        call check(near(lay%pw, 1600.0 / 3.0) .and. near(lay%ph, 400.0) .and. &
            near(lay%px, 100.0 / 3.0) .and. near(lay%kept, 1.0), &
            "fit ignores zoom and offset, centres the whole photo")
        ! Margins 10, 20, 30, 40: printable area 560 x 340 at (10, 20).
        lay = layout_compute(400, 300, 600.0, 400.0, [10.0, 20.0, 30.0, 40.0], MODE_FIT, &
            1.0, 0.0, 0.0)
        call check(near(lay%ph, 340.0) .and. near(lay%py, 20.0) .and. &
            near(lay%px, 10.0 + (560.0 - 400.0 * 340.0 / 300.0) / 2.0), "fit inside margins")
        ! Dragging the photo down by 25 units from the centre shows the top edge.
        lay = layout_compute(400, 300, 600.0, 400.0, [0.0, 0.0, 0.0, 0.0], MODE_FILL, &
            1.0, 0.0, 0.0)
        ox = 0.0
        oy = 0.0
        call layout_offset_by(lay, 0.0, 25.0, ox, oy)
        call check(near(oy, -1.0) .and. near(ox, 0.0), "drag moves the crop")
    end subroutine test_fill_fit

    subroutine test_render()
        !! A 2 x 2 photo (red, green / blue, white) fitted into a 8 x 4 sheet:
        !! scale 2, photo 4 x 4 at x = 2..6; white elsewhere. Page pixel (4, 2) has
        !! its centre at source position (1.25, 1.25): weights 0.25, so red is
        !! 0.75 * (0.75 * 255) + 0.25 * (0.25 * 255) = 159, green and blue 64.
        type(layout_t) :: lay
        integer(int8) :: src(3, 2, 2)
        integer(int8), allocatable :: page(:, :, :)

        src(:, 1, 1) = b([255, 0, 0])
        src(:, 2, 1) = b([0, 255, 0])
        src(:, 1, 2) = b([0, 0, 255])
        src(:, 2, 2) = b([255, 255, 255])
        lay = layout_compute(2, 2, 8.0, 4.0, [0.0, 0.0, 0.0, 0.0], MODE_FIT, 1.0, 0.0, 0.0)
        call layout_render(lay, src, 1.0, page)
        call check(size(page, 2) == 8 .and. size(page, 3) == 4, "page size")
        call check(all(page(:, 1, :) == -1_int8) .and. all(page(:, 8, :) == -1_int8), &
            "white beside the fitted photo")
        call check(all(page(:, 3, 1) == b([255, 0, 0])) .and. &
            all(page(:, 6, 1) == b([0, 255, 0])) .and. &
            all(page(:, 3, 4) == b([0, 0, 255])), "corners keep their colours")
        call check(all(abs(int(iand(int(page(:, 4, 2)), 255)) - [159, 64, 64]) <= 1), &
            "bilinear blend between neighbours")
        call layout_render(lay, src, 0.5, page)
        call check(size(page, 2) == 4 .and. size(page, 3) == 2, "half-scale preview size")
    end subroutine test_render

    function b(v) result(p)
        integer, intent(in) :: v(:)
        integer(int8) :: p(size(v))
        p = int(v - merge(256, 0, v > 127), int8)
    end function b

    subroutine test_rotate()
        !! 3 x 2 page: its top row becomes the right column.
        integer(int8) :: page(3, 3, 2)
        integer(int8), allocatable :: out(:, :, :)
        integer :: x, y

        do y = 1, 2
            do x = 1, 3
                page(:, x, y) = int(10 * y + x, int8)
            end do
        end do
        call rotate_cw(page, out)
        call check(size(out, 2) == 2 .and. size(out, 3) == 3, "turned size")
        call check(out(1, 2, 1) == 11 .and. out(1, 2, 3) == 13 .and. out(1, 1, 1) == 21, &
            "top row is now the right column")
    end subroutine test_rotate

    subroutine test_exif()
        !! JPEG start, APP1 "Exif" with one IFD entry 0x0112 = 6 (little-endian),
        !! then the same as big-endian with value 8.
        integer(int8), allocatable :: j(:)
        type(image_t) :: img

        j = b([255, 216, 255, 225, 0, 34, 69, 120, 105, 102, 0, 0, &
            73, 73, 42, 0, 8, 0, 0, 0, 1, 0, 18, 1, 3, 0, 1, 0, 0, 0, 6, 0, 0, 0, &
            0, 0, 0, 0, 255, 217])
        call check(exif_orientation(j) == 6, "little-endian orientation")
        j = b([255, 216, 255, 225, 0, 34, 69, 120, 105, 102, 0, 0, &
            77, 77, 0, 42, 0, 0, 0, 8, 0, 1, 1, 18, 0, 3, 0, 0, 0, 1, 0, 8, 0, 0, &
            0, 0, 0, 0, 255, 217])
        call check(exif_orientation(j) == 8, "big-endian orientation")
        call check(exif_orientation(b([255, 216, 255, 217])) == 1, "no EXIF")
        ! Orientation 6 (turn clockwise to view): stored A B becomes A above B.
        img%width = 2
        img%height = 1
        allocate(img%rgb(3, 2, 1))
        img%rgb(:, 1, 1) = 1_int8
        img%rgb(:, 2, 1) = 2_int8
        call image_orient(img, 6)
        call check(img%width == 1 .and. img%height == 2 .and. img%rgb(1, 1, 1) == 1 .and. &
            img%rgb(1, 1, 2) == 2, "orientation 6 applied")
        call image_orient(img, 8)
        call check(img%width == 2 .and. img%rgb(1, 1, 1) == 1 .and. img%rgb(1, 2, 1) == 2, &
            "orientation 8 undoes 6")
    end subroutine test_exif

    subroutine test_decode()
        type(image_t) :: img, small
        character(len=:), allocatable :: msg
        character(len=:), allocatable :: names(:)

        call image_load("test/fixtures/rgb-2x2.png", img, msg)
        call check(len(msg) == 0 .and. img%width == 2 .and. img%height == 2, "PNG size")
        if (len(msg) == 0) call check(all(img%rgb(:, 1, 1) == b([255, 0, 0])) .and. &
            all(img%rgb(:, 2, 1) == b([0, 255, 0])) .and. &
            all(img%rgb(:, 1, 2) == b([0, 0, 255])), "PNG pixels")
        call image_load("test/fixtures/red-16x8.jpg", img, msg)
        call check(len(msg) == 0 .and. img%width == 16 .and. img%height == 8, "JPEG size")
        if (len(msg) == 0) call check(all(abs(iand(int(img%rgb(:, 9, 4)), 255) - &
            [200, 30, 60]) <= 3), "JPEG colour within quantisation")
        if (len(msg) == 0) then
            call image_shrink(img, 4, small)
            call check(small%width == 4 .and. small%height == 2, "shrink by a box of four")
        end if
        call image_load("test/fixtures/README.md", img, msg)
        call check(len(msg) > 0, "other files are refused")
        call image_glob("test/fixtures", names)
        call check(size(names) == 2, "two photos in the fixtures folder")
        if (size(names) == 2) call check(trim(names(1)) == &
            "test/fixtures/red-16x8.jpg" .and. trim(names(2)) == &
            "test/fixtures/rgb-2x2.png", "sorted paths of the photos")
    end subroutine test_decode

end program test_photo
