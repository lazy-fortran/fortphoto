module photo_edit
    !! A photo and the list of editing steps applied to it. Steps are kept and
    !! replayed on the original, so any of them can be undone at any time and
    !! the same list renders the screen proxy and the full-resolution result.
    !! Positions are fractions (0..1) of the image as it enters the step, so
    !! they mean the same at every resolution.
    use iso_fortran_env, only: int8
    use fortpix
    implicit none
    private
    public :: step_t, selection_t, shape_t, edit_t, edit_render, edit_push, edit_undo, &
        edit_redo, edit_name, selection_mask, edit_reset, edit_size, new_step, &
        STEP_CROP, STEP_TURN, STEP_FLIP, STEP_STRAIGHTEN, STEP_RESIZE, STEP_CANVAS, &
        STEP_LEVELS, STEP_ADJUST, STEP_SHARPEN, STEP_INK, &
        SHAPE_RECT, SHAPE_ELLIPSE, SHAPE_LASSO, SHAPE_WAND

    integer, parameter :: STEP_CROP = 1, STEP_TURN = 2, STEP_FLIP = 3, STEP_STRAIGHTEN = 4, &
        STEP_RESIZE = 5, STEP_CANVAS = 6, STEP_LEVELS = 7, STEP_ADJUST = 8, STEP_SHARPEN = 9, &
        STEP_INK = 10
    integer, parameter :: SHAPE_RECT = 1, SHAPE_ELLIPSE = 2, SHAPE_LASSO = 3, SHAPE_WAND = 4

    type :: shape_t
        !! One selection shape combined into the mask with a MASK_* mode.
        !! Rectangles and ellipses use the box (x0, y0, x1, y1); a lasso the
        !! polygon (px, py); the magic wand its seed (x0, y0), tolerance and
        !! `contiguous`.
        integer :: kind = 0, mode = MASK_REPLACE, tolerance = 32
        logical :: contiguous = .true.
        real :: x0 = 0.0, y0 = 0.0, x1 = 0.0, y1 = 0.0
        real, allocatable :: px(:), py(:)
    end type shape_t

    type :: selection_t
        type(shape_t), allocatable :: shapes(:)
    end type selection_t

    type :: step_t
        integer :: kind = 0
        ! Crop box, flip direction, quarter turns, angle, target size.
        real :: x0 = 0.0, y0 = 0.0, x1 = 1.0, y1 = 1.0
        logical :: horizontal = .true.
        integer :: turns = 0, width = 0, height = 0
        real :: degrees = 0.0
        type(pix_levels_t) :: levels
        type(pix_adjust_t) :: adjust
        ! Sharpening radius as a fraction of the image width.
        real :: radius = 0.0, amount = 0.0, threshold = 0.0
        ! Ink: centreline and radius as fractions of the image width, colour.
        real, allocatable :: ix(:), iy(:), ir(:)
        integer :: rgb(3) = 0
        ! Tonal steps apply inside the selection when it has shapes.
        type(selection_t) :: selection
    end type step_t

    type :: edit_t
        type(step_t), allocatable :: steps(:)
        ! Steps 1..active apply; later ones can be redone.
        integer :: count = 0, active = 0
        ! Full-resolution size before the first step.
        integer :: width = 0, height = 0
    end type edit_t

contains

    function new_step(kind) result(s)
        integer, intent(in) :: kind
        type(step_t) :: s
        s%kind = kind
    end function new_step

    subroutine edit_reset(e, width, height)
        type(edit_t), intent(out) :: e
        integer, intent(in) :: width, height
        allocate(e%steps(16))
        e%width = width
        e%height = height
    end subroutine edit_reset

    subroutine edit_push(e, s)
        !! Appends a step after the active ones; steps that were undone are dropped.
        type(edit_t), intent(inout) :: e
        type(step_t), intent(in) :: s
        type(step_t), allocatable :: grown(:)
        if (.not. allocated(e%steps)) allocate(e%steps(16))
        if (e%active == size(e%steps)) then
            allocate(grown(2 * size(e%steps)))
            grown(1:e%active) = e%steps(1:e%active)
            call move_alloc(grown, e%steps)
        end if
        e%active = e%active + 1
        e%steps(e%active) = s
        e%count = e%active
    end subroutine edit_push

    logical function edit_undo(e)
        type(edit_t), intent(inout) :: e
        edit_undo = e%active > 0
        if (edit_undo) e%active = e%active - 1
    end function edit_undo

    logical function edit_redo(e)
        type(edit_t), intent(inout) :: e
        edit_redo = e%active < e%count
        if (edit_redo) e%active = e%active + 1
    end function edit_redo

    function edit_name(s) result(t)
        type(step_t), intent(in) :: s
        character(len=:), allocatable :: t
        select case (s%kind)
        case (STEP_CROP)
            t = "Crop"
        case (STEP_TURN)
            t = merge("Rotate right", "Rotate left ", s%turns > 0)
            t = trim(t)
        case (STEP_FLIP)
            t = merge("Flip horizontally", "Flip vertically  ", s%horizontal)
            t = trim(t)
        case (STEP_STRAIGHTEN)
            t = "Straighten"
        case (STEP_RESIZE)
            t = "Image size"
        case (STEP_CANVAS)
            t = "Canvas size"
        case (STEP_LEVELS)
            t = "Levels"
        case (STEP_ADJUST)
            t = "Colour"
        case (STEP_SHARPEN)
            t = "Sharpen"
        case (STEP_INK)
            t = "Pen"
        case default
            t = "Step"
        end select
        if (allocated(s%selection%shapes)) then
            if (size(s%selection%shapes) > 0) t = t // " (selection)"
        end if
    end function edit_name

    subroutine selection_mask(sel, img, mask)
        !! The selection's coverage on `img`; unallocated when it has no shapes.
        type(selection_t), intent(in) :: sel
        integer(int8), intent(in) :: img(:, :, :)
        real, allocatable, intent(out) :: mask(:, :)
        real, allocatable :: shape(:, :)
        integer :: k, w, h
        if (.not. allocated(sel%shapes)) return
        if (size(sel%shapes) == 0) return
        w = size(img, 2)
        h = size(img, 3)
        do k = 1, size(sel%shapes)
            associate(s => sel%shapes(k))
                select case (s%kind)
                case (SHAPE_RECT)
                    call pix_mask_rect(w, h, s%x0 * w, s%y0 * h, s%x1 * w, s%y1 * h, shape)
                case (SHAPE_ELLIPSE)
                    call pix_mask_ellipse(w, h, s%x0 * w, s%y0 * h, s%x1 * w, s%y1 * h, shape)
                case (SHAPE_LASSO)
                    call pix_mask_polygon(w, h, s%px * w, s%py * h, shape)
                case (SHAPE_WAND)
                    call pix_magic_wand(img, max(1, min(w, int(s%x0 * w) + 1)), &
                        max(1, min(h, int(s%y0 * h) + 1)), s%tolerance, s%contiguous, shape)
                case default
                    cycle
                end select
                call pix_mask_combine(mask, shape, s%mode)
            end associate
        end do
    end subroutine selection_mask

    subroutine edit_size(e, w, h)
        !! Full-resolution size after the active steps, with the rounding the
        !! renderer uses.
        type(edit_t), intent(in) :: e
        integer, intent(out) :: w, h
        integer :: k, t
        real :: s
        w = e%width
        h = e%height
        do k = 1, e%active
            associate(st => e%steps(k))
                select case (st%kind)
                case (STEP_CROP)
                    w = max(1, nint(abs(st%x1 - st%x0) * w))
                    h = max(1, nint(abs(st%y1 - st%y0) * h))
                case (STEP_TURN)
                    if (mod(abs(st%turns), 2) == 1) then
                        t = w
                        w = h
                        h = t
                    end if
                case (STEP_STRAIGHTEN)
                    s = pix_straighten_scale(w, h, st%degrees)
                    w = max(1, int(w * s))
                    h = max(1, int(h * s))
                case (STEP_RESIZE, STEP_CANVAS)
                    w = st%width
                    h = st%height
                end select
            end associate
        end do
    end subroutine edit_size

    subroutine edit_render(e, src, first, last, img, scale)
        !! Applies steps first..last to `src` (the image entering step `first`)
        !! into `img`. `scale` is the ratio of `src` to the full-resolution
        !! image at that point, so pixel sizes such as resizing targets and
        !! sharpening radii follow the proxy.
        type(edit_t), intent(in) :: e
        integer(int8), intent(in) :: src(:, :, :)
        integer, intent(in) :: first, last
        integer(int8), allocatable, intent(inout) :: img(:, :, :)
        real, intent(inout) :: scale
        integer(int8), allocatable :: out(:, :, :)
        integer :: k
        img = src
        do k = first, last
            call apply(e%steps(k), img, out, scale)
            if (allocated(out)) call move_alloc(out, img)
        end do
    end subroutine edit_render

    subroutine apply(s, img, out, scale)
        type(step_t), intent(in) :: s
        integer(int8), intent(in) :: img(:, :, :)
        integer(int8), allocatable, intent(out) :: out(:, :, :)
        real, intent(inout) :: scale
        integer(int8), allocatable :: edited(:, :, :)
        real, allocatable :: mask(:, :)
        integer :: w, h, k, nw, nh, x0, y0
        w = size(img, 2)
        h = size(img, 3)
        select case (s%kind)
        case (STEP_CROP)
            x0 = nint(min(s%x0, s%x1) * w) + 1
            y0 = nint(min(s%y0, s%y1) * h) + 1
            call pix_crop(img, x0, y0, max(1, nint(abs(s%x1 - s%x0) * w)), &
                max(1, nint(abs(s%y1 - s%y0) * h)), out)
        case (STEP_TURN)
            call pix_rotate90(img, s%turns, out)
        case (STEP_FLIP)
            call pix_flip(img, s%horizontal, out)
        case (STEP_STRAIGHTEN)
            call pix_rotate(img, s%degrees, .true., [255, 255, 255], out)
        case (STEP_RESIZE)
            ! The target is the full-resolution size; the proxy keeps its ratio.
            nw = max(1, nint(s%width * scale))
            nh = max(1, nint(s%height * scale))
            call pix_resize(img, nw, nh, out)
        case (STEP_CANVAS)
            nw = max(1, nint(s%width * scale))
            nh = max(1, nint(s%height * scale))
            allocate(out(3, nw, nh))
            out = -1_int8
            x0 = (nw - w) / 2
            y0 = (nh - h) / 2
            out(:, max(1, x0 + 1):min(nw, x0 + w), max(1, y0 + 1):min(nh, y0 + h)) = &
                img(:, max(1, 1 - x0):min(w, nw - x0), max(1, 1 - y0):min(h, nh - y0))
        case (STEP_LEVELS, STEP_ADJUST, STEP_SHARPEN)
            select case (s%kind)
            case (STEP_LEVELS)
                call pix_levels(img, s%levels, edited)
            case (STEP_ADJUST)
                call pix_adjust(img, s%adjust, edited)
            case default
                call pix_unsharp(img, max(0.3, s%radius * w), s%amount, s%threshold, edited)
            end select
            call selection_mask(s%selection, img, mask)
            if (allocated(mask)) then
                call pix_blend_masked(img, edited, mask, out)
            else
                call move_alloc(edited, out)
            end if
        case (STEP_INK)
            out = img
            call pix_stroke(out, s%ix * w, s%iy * w, s%ir * w, s%rgb)
        case default
            k = 0
        end select
    end subroutine apply
end module photo_edit
