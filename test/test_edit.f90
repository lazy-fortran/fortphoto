program test_edit
    !! The edit list renders the same geometry at full resolution and on a
    !! proxy, undo and redo move through it, tonal steps stay inside their
    !! selection, and exports never replace a file.
    use iso_fortran_env, only: int8
    use fortpix, only: to_byte, u8, MASK_REPLACE
    use photo_edit
    use photo_export, only: export_name
    implicit none
    integer(int8), allocatable :: src(:, :, :), half(:, :, :), full(:, :, :), proxy(:, :, :)
    type(edit_t) :: e
    type(step_t) :: s
    type(shape_t) :: box
    integer :: x, y, w, h, failures
    real :: scale
    failures = 0

    allocate(src(3, 400, 300))
    do y = 1, 300
        do x = 1, 400
            src(:, x, y) = to_byte([x / 2, y / 2, 100])
        end do
    end do
    half = src(:, 1::2, 1::2)
    call edit_reset(e, 400, 300)

    s = new_step(STEP_CROP)
    s%x0 = 0.1
    s%y0 = 0.2
    s%x1 = 0.9
    s%y1 = 0.8
    call edit_push(e, s)
    s = new_step(STEP_TURN)
    s%turns = 1
    call edit_push(e, s)
    s = new_step(STEP_STRAIGHTEN)
    s%degrees = 3.0
    call edit_push(e, s)
    s = new_step(STEP_RESIZE)
    s%width = 150
    s%height = 200
    call edit_push(e, s)
    call edit_size(e, w, h)
    call check(w == 150 .and. h == 200, 'size after geometry steps')
    scale = 1.0
    call edit_render(e, src, 1, e%active, full, scale)
    call check(size(full, 2) == w .and. size(full, 3) == h, 'full render has the computed size')
    scale = 0.5
    call edit_render(e, half, 1, e%active, proxy, scale)
    call check(abs(size(proxy, 2) - 75) <= 1 .and. abs(size(proxy, 3) - 100) <= 1, &
        'proxy render at half the size')

    call check(edit_undo(e) .and. edit_undo(e), 'undo two steps')
    call edit_size(e, w, h)
    call check(w == 180 .and. h == 320, 'undone steps no longer apply (crop and turn)')
    call check(edit_redo(e), 'redo')
    s = new_step(STEP_FLIP)
    call edit_push(e, s)
    call check(e%count == e%active .and. .not. edit_redo(e), 'a new step drops redoable ones')

    call edit_reset(e, 400, 300)
    s = new_step(STEP_LEVELS)
    s%levels%black(0) = 200.0
    box%kind = SHAPE_RECT
    box%mode = MASK_REPLACE
    box%x0 = 0.0
    box%y0 = 0.0
    box%x1 = 0.5
    box%y1 = 1.0
    s%selection%shapes = [box]
    call edit_push(e, s)
    scale = 1.0
    call edit_render(e, src, 1, 1, full, scale)
    call check(u8(full(3, 10, 10)) == 0, 'levels inside the selection')
    call check(all(full(:, 300, 10) == src(:, 300, 10)), 'pixels outside the selection unchanged')

    s = new_step(STEP_INK)
    s%rgb = [255, 0, 0]
    s%ix = [0.25, 0.75]
    s%iy = [0.25, 0.25]
    s%ir = [0.01, 0.01]
    call edit_push(e, s)
    call edit_render(e, src, 1, 2, full, scale)
    call check(u8(full(1, 200, 100)) == 255 .and. u8(full(2, 200, 100)) == 0, 'pen in image fractions')

    call check(export_name('/p/photo.jpg', 'jpg', none) == '/p/photo-edited.jpg', 'export name')
    call check(export_name('/p/photo.jpg', 'png', first) == '/p/photo-edited-2.png', 'existing export kept')
    call check(export_name('/p.d/scan', 'jpg', none) == '/p.d/scan-edited.jpg', 'name without extension')

    if (failures > 0) error stop 'edit tests failed'
    print '(a)', 'edit tests passed'
contains
    logical function none(path)
        character(*), intent(in) :: path
        none = len(path) < 0
    end function none
    logical function first(path)
        character(*), intent(in) :: path
        first = path == '/p/photo-edited.png'
    end function first
    subroutine check(ok, what)
        logical, intent(in) :: ok
        character(*), intent(in) :: what
        if (.not. ok) then
            print '(2a)', 'FAIL: ', what
            failures = failures + 1
        end if
    end subroutine check
end program test_edit
