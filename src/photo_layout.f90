module photo_layout
    !! Where a photo goes on a sheet, and the page raster that puts it there.
    !!
    !! Fill scales the photo until it covers the printable area and crops what
    !! sticks out; a zoom above 1 crops more, and an offset in [-1, 1] per axis
    !! chooses which part stays (-1: left/top edge, +1: right/bottom edge). Fit
    !! scales the whole photo into the printable area and leaves the rest white.
    !! The same layout renders the preview and the printed page, so they agree.
    use iso_fortran_env, only: int8
    implicit none
    private

    integer, parameter, public :: MODE_FILL = 1, MODE_FIT = 2

    type, public :: layout_t
        !! Sheet and printable area in sheet units (pixels at the print
        !! resolution), the photo's placed rectangle, and the share of the photo
        !! that is printed.
        real :: sheet_w = 0.0, sheet_h = 0.0
        real :: ax = 0.0, ay = 0.0, aw = 0.0, ah = 0.0
        real :: px = 0.0, py = 0.0, pw = 0.0, ph = 0.0
        real :: kept = 1.0
    end type layout_t

    public :: layout_compute, layout_render, rotate_cw, layout_offset_by

contains

    pure function layout_compute(photo_w, photo_h, sheet_w, sheet_h, margins, mode, &
            zoom, ox, oy) result(lay)
        !! margins = left, top, right, bottom in sheet units.
        integer, intent(in) :: photo_w, photo_h, mode
        real, intent(in) :: sheet_w, sheet_h, margins(4), zoom, ox, oy
        type(layout_t) :: lay
        real :: s, fx, fy

        lay%sheet_w = sheet_w
        lay%sheet_h = sheet_h
        lay%ax = margins(1)
        lay%ay = margins(2)
        lay%aw = max(1.0, sheet_w - margins(1) - margins(3))
        lay%ah = max(1.0, sheet_h - margins(2) - margins(4))
        if (mode == MODE_FIT) then
            s = min(lay%aw / real(photo_w), lay%ah / real(photo_h))
            fx = 0.0
            fy = 0.0
        else
            s = max(lay%aw / real(photo_w), lay%ah / real(photo_h)) * max(1.0, zoom)
            fx = max(-1.0, min(1.0, ox))
            fy = max(-1.0, min(1.0, oy))
        end if
        lay%pw = real(photo_w) * s
        lay%ph = real(photo_h) * s
        lay%px = lay%ax - (lay%pw - lay%aw) * 0.5 * (1.0 + fx)
        lay%py = lay%ay - (lay%ph - lay%ah) * 0.5 * (1.0 + fy)
        lay%kept = min(lay%pw, lay%aw) * min(lay%ph, lay%ah) / (lay%pw * lay%ph)
    end function layout_compute

    pure subroutine layout_offset_by(lay, dx, dy, ox, oy)
        !! New offsets after the photo was dragged by (dx, dy) sheet units.
        type(layout_t), intent(in) :: lay
        real, intent(in) :: dx, dy
        real, intent(inout) :: ox, oy

        if (lay%pw - lay%aw > 0.5) ox = max(-1.0, min(1.0, ox - 2.0 * dx / (lay%pw - lay%aw)))
        if (lay%ph - lay%ah > 0.5) oy = max(-1.0, min(1.0, oy - 2.0 * dy / (lay%ph - lay%ah)))
    end subroutine layout_offset_by

    subroutine layout_render(lay, src, scale, page)
        !! Page raster of the whole sheet at `scale` output pixels per sheet unit:
        !! white outside the photo or the printable area, the photo sampled
        !! bilinearly from `src` (any resolution of the same photo) inside.
        type(layout_t), intent(in) :: lay
        integer(int8), intent(in) :: src(:, :, :)
        real, intent(in) :: scale
        integer(int8), allocatable, intent(out) :: page(:, :, :)
        integer :: w, h, sw, sh, i, j, x0, y0, c
        real :: x, y, u, v, fu, fv, a, b, cx0, cx1, cy0, cy1, k
        integer(int8), parameter :: WHITE = -1_int8

        w = max(1, nint(lay%sheet_w * scale))
        h = max(1, nint(lay%sheet_h * scale))
        sw = size(src, 2)
        sh = size(src, 3)
        allocate(page(3, w, h))
        cx0 = max(lay%ax, lay%px)
        cx1 = min(lay%ax + lay%aw, lay%px + lay%pw)
        cy0 = max(lay%ay, lay%py)
        cy1 = min(lay%ay + lay%ah, lay%py + lay%ph)
        !$omp parallel do schedule(static) private(i, x, y, u, v, x0, y0, fu, fv, a, b, c, k)
        do j = 1, h
            y = (real(j) - 0.5) / scale
            do i = 1, w
                x = (real(i) - 0.5) / scale
                if (x < cx0 .or. x >= cx1 .or. y < cy0 .or. y >= cy1) then
                    page(:, i, j) = WHITE
                    cycle
                end if
                u = (x - lay%px) / lay%pw * real(sw) + 0.5
                v = (y - lay%py) / lay%ph * real(sh) + 0.5
                x0 = max(1, min(sw - 1, int(u)))
                y0 = max(1, min(sh - 1, int(v)))
                if (sw == 1) x0 = 1
                if (sh == 1) y0 = 1
                fu = max(0.0, min(1.0, u - real(x0)))
                fv = max(0.0, min(1.0, v - real(y0)))
                do c = 1, 3
                    a = (1.0 - fu) * px(src, c, x0, y0, sw, sh) + &
                        fu * px(src, c, x0 + 1, y0, sw, sh)
                    b = (1.0 - fu) * px(src, c, x0, y0 + 1, sw, sh) + &
                        fu * px(src, c, x0 + 1, y0 + 1, sw, sh)
                    k = (1.0 - fv) * a + fv * b
                    page(c, i, j) = int(nint(k) - merge(256, 0, nint(k) > 127), int8)
                end do
            end do
        end do
        !$omp end parallel do
    end subroutine layout_render

    pure real function px(src, c, i, j, sw, sh)
        integer(int8), intent(in) :: src(:, :, :)
        integer, intent(in) :: c, i, j, sw, sh
        px = real(iand(int(src(c, min(i, sw), min(j, sh))), 255))
    end function px

    subroutine rotate_cw(page, out)
        !! Quarter turn clockwise: the top row becomes the right column.
        integer(int8), intent(in) :: page(:, :, :)
        integer(int8), allocatable, intent(out) :: out(:, :, :)
        integer :: w, h, x, y

        w = size(page, 2)
        h = size(page, 3)
        allocate(out(3, h, w))
        !$omp parallel do private(x)
        do y = 1, h
            do x = 1, w
                out(:, h - y + 1, x) = page(:, x, y)
            end do
        end do
        !$omp end parallel do
    end subroutine rotate_cw

end module photo_layout
