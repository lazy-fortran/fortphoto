module photo_app
    !! The photo editor: tools, canvas, adjustment panel, history, export and
    !! the print dialog, independent of the platform that hosts it.
    use iso_fortran_env, only: int8, int32, int64
    use iso_c_binding, only: c_double
    use fortui
    use fortui_files, only: file_parent, file_name, file_home
    use fortpix
    use fortink, only: ink_state_t, ink_reset, ink_sample
    use photo_image, only: image_t, image_load, image_load_bytes, image_shrink, image_glob
    use photo_edit
    use photo_export, only: export_photo
    use photo_print, only: print_photo_set, print_view, print_hold, print_supported
    implicit none
    private
    public :: app_setup, app_open_initial, app_frame


    integer, parameter :: PROXY = 2048
    real, parameter :: TOOLS_W = 104.0, PANEL_W = 300.0, MM_PER_PX = 25.4 / 96.0
    integer, parameter :: T_MOVE = 1, T_RECT = 2, T_ELLIPSE = 3, T_LASSO = 4, T_WAND = 5, &
        T_CROP = 6, T_PEN = 7, T_WHITE = 8
    character(len=*), parameter :: TOOL_NAMES(8) = [character(len=9) :: "Move", "Rectangle", &
        "Ellipse", "Lasso", "Wand", "Crop", "Pen", "White"]
    integer, parameter :: PALETTE(3, 6) = reshape([0, 0, 0, 26, 86, 196, 211, 47, 47, &
        46, 125, 50, 239, 108, 0, 123, 31, 162], [3, 6])
    character(len=*), parameter :: COLOUR_NAMES(6) = [character(len=6) :: "Black", "Blue", &
        "Red", "Green", "Orange", "Violet"]
    real, parameter :: RATIOS(6) = [0.0, 1.0, 1.5, 4.0 / 3.0, 16.0 / 9.0, -1.0]

    type(image_t) :: original
    type(edit_t) :: doc
    type(file_dialog_t) :: chooser
    character(len=:), allocatable :: path, status, folder
    character(len=:), allocatable :: files(:)
    ! proxy0: the original at screen resolution; base: steps applied to it;
    ! shown: base with the pending adjustments.
    integer(int8), allocatable :: proxy0(:, :, :), base(:, :, :), shown(:, :, :)
    integer(int32), allocatable :: shown_rgba(:, :), sel_rgba(:, :)
    integer(int64) :: hist(0:255, 4)
    real :: scale0 = 1.0, base_scale = 1.0
    integer :: base_steps = -1, shown_gen = 0, sel_gen = 0, stat
    logical :: base_dirty = .true., shown_dirty = .true., printing = .false., fitted = .false.
    type(canvas_view_t) :: view, hview
    integer :: tool = T_MOVE, panel_tab = 1, sel_mode_tab = 1, ratio_tab = 1, colour = 1
    ! Current selection, applied to the next tonal step.
    type(selection_t) :: sel
    real, allocatable :: sel_mask(:, :)
    ! Pending tonal adjustments, shown live until applied.
    type(pix_levels_t) :: levels
    type(pix_adjust_t) :: adjust
    real :: sharpen_amount = 0.0, sharpen_radius = 1.0
    ! Gesture in progress on the canvas (fractions of the shown image).
    logical :: gesture = .false., crop_set = .false.
    real :: gx0 = 0.0, gy0 = 0.0, gx1 = 0.0, gy1 = 0.0
    real, allocatable :: lx(:), ly(:), lr(:)
    integer :: ln = 0
    type(ink_state_t) :: ink
    real :: pen_mm = 1.0, straighten = 0.0, tolerance = 32.0
    logical :: contiguous = .true., keep_ratio = .true.
    character(len=:), allocatable :: size_w, size_h, size_dpi
    integer :: dpi = 300
    integer(int64) :: clock0

contains

    subroutine app_setup()
        !! State and command line, before the user interface starts.
        status = ""
        path = ""
        folder = file_home()
        size_w = ""
        size_h = ""
        size_dpi = "300"
        call system_clock(clock0)
        call command_line()
    end subroutine app_setup

    subroutine app_open_initial()
        if (len(path) > 0) call open_photo(path)
    end subroutine app_open_initial

    subroutine app_frame()
        call frame()
    end subroutine app_frame


    subroutine command_line()
        integer :: n, k
        character(len=4096) :: arg
        do k = 1, command_argument_count()
            call get_command_argument(k, arg, n)
            if (trim(arg) == "--hold") then
                print_hold = .true.
            else
                path = trim(arg)
            end if
        end do
    end subroutine command_line

    subroutine frame()
        logical :: leave
        call file_dialog_poll(chooser)
        if (chooser%active) then
            call file_dialog_show(chooser)
            return
        end if
        if (chooser%status == FILE_ACCEPTED) then
            if (allocated(chooser%bytes)) then
                call open_photo_bytes(chooser%bytes, chooser%name)
            else
                call open_photo(chooser%path)
            end if
            chooser%status = FILE_IDLE
        end if
        if (printing) then
            leave = .false.
            call print_view(leave)
            if (leave) printing = .false.
            return
        end if
        call shortcuts()
        call refresh()
        call top_bar()
        call split_begin("main", first=TOOLS_W, last=PANEL_W, gap=8.0)
        call tools()
        call split_next()
        call editor_canvas()
        call split_next()
        call panel()
        call split_end()
    end subroutine frame

    subroutine shortcuts()
        if (ui_shortcut("mod+o", label="Open")) &
            call file_dialog_open(chooser, "Open photo", folder, ".jpg,.jpeg,.png")
        if (ui_shortcut("mod+z", label="Undo")) call undo()
        if (ui_shortcut("mod+shift+z", label="Redo")) call redo()
        if (ui_shortcut("mod+p", label="Print")) call start_print()
        if (ui_shortcut("mod+d", label="Deselect")) call deselect()
        if (ui_key_pressed(KEY_ESCAPE)) then
            if (gesture .or. crop_set) then
                gesture = .false.
                crop_set = .false.
            else
                call deselect()
            end if
        end if
        if (ui_key_pressed(KEY_ENTER) .and. crop_set) call apply_crop()
        if (ui_key_pressed(KEY_PAGE_DOWN)) call step_photo(1)
        if (ui_key_pressed(KEY_PAGE_UP)) call step_photo(-1)
    end subroutine shortcuts

    ! --- photos -------------------------------------------------------------------

    subroutine open_photo(p)
        character(*), intent(in) :: p
        character(len=:), allocatable :: msg
        type(image_t) :: loaded
        call image_load(p, loaded, msg)
        if (len(msg) > 0) then
            status = msg
            return
        end if
        path = p
        folder = file_parent(p)
        call image_glob(folder, files)
        call adopt(loaded)
    end subroutine open_photo

    subroutine open_photo_bytes(bytes, name)
        integer(int8), intent(in) :: bytes(:)
        character(*), intent(in) :: name
        type(image_t) :: loaded
        character(:), allocatable :: message
        call image_load_bytes(bytes, loaded, message)
        if (len(message) > 0) then
            status = message
            return
        end if
        path = name
        if (allocated(files)) deallocate(files)
        allocate(character(1) :: files(0))
        call adopt(loaded)
    end subroutine open_photo_bytes

    subroutine adopt(loaded)
        type(image_t), intent(in) :: loaded
        type(image_t) :: small
        original = loaded
        call image_shrink(original, PROXY, small)
        proxy0 = small%rgb
        scale0 = real(small%width) / real(original%width)
        call edit_reset(doc, original%width, original%height)
        call deselect()
        call reset_pending()
        crop_set = .false.
        gesture = .false.
        base_dirty = .true.
        fitted = .false.
        status = ""
    end subroutine adopt

    subroutine step_photo(dir)
        integer, intent(in) :: dir
        integer :: k, n
        if (.not. allocated(files)) return
        n = size(files)
        if (n == 0) return
        do k = 1, n
            if (trim(files(k)) == trim(path) .or. trim(files(k)) == "./" // trim(path)) then
                call open_photo(trim(files(modulo(k - 1 + dir, n) + 1)))
                return
            end if
        end do
    end subroutine step_photo

    logical function have_photo()
        have_photo = allocated(proxy0)
    end function have_photo

    ! --- document -----------------------------------------------------------------

    subroutine push(s)
        type(step_t), intent(in) :: s
        integer(int8), allocatable :: next(:, :, :)
        call edit_push(doc, s)
        ! Controls composed earlier in this frame show the new step next frame.
        call ui_request_frame()
        ! The new step renders onto the current base; undo re-renders.
        if (base_steps == doc%active - 1 .and. allocated(base)) then
            call edit_render(doc, base, doc%active, doc%active, next, base_scale)
            call move_alloc(next, base)
            base_steps = doc%active
            shown_dirty = .true.
        else
            base_dirty = .true.
        end if
        status = ""
    end subroutine push

    subroutine undo()
        if (edit_undo(doc)) then
            base_dirty = .true.
            call deselect()
        end if
    end subroutine undo

    subroutine redo()
        if (edit_redo(doc)) base_dirty = .true.
    end subroutine redo

    subroutine refresh()
        !! Brings the proxy render, the live adjustments, the histogram and
        !! the selection overlay up to date.
        integer(int8), allocatable :: tmp(:, :, :)
        if (.not. have_photo()) return
        if (base_dirty .or. base_steps /= doc%active) then
            base_scale = scale0
            call edit_render(doc, proxy0, 1, doc%active, base, base_scale)
            base_steps = doc%active
            base_dirty = .false.
            shown_dirty = .true.
            call update_selection()
        end if
        if (.not. shown_dirty) return
        shown = base
        if (.not. pix_is_identity(levels)) then
            call pix_levels(shown, levels, tmp)
            call move_alloc(tmp, shown)
        end if
        if (.not. pix_is_identity(adjust)) then
            call pix_adjust(shown, adjust, tmp)
            call move_alloc(tmp, shown)
        end if
        if (sharpen_amount > 0.0) then
            call pix_unsharp(shown, max(0.3, sharpen_radius * base_scale), sharpen_amount, 2.0, tmp)
            call move_alloc(tmp, shown)
        end if
        if (allocated(sel_mask) .and. .not. pending_identity()) then
            call pix_blend_masked(base, shown, sel_mask, tmp)
            call move_alloc(tmp, shown)
        end if
        call pix_histogram(shown, hist)
        call pack_rgba(shown, shown_rgba)
        shown_gen = shown_gen + 1
        shown_dirty = .false.
        if (len(size_w) == 0 .or. .not. keep_ratio) call size_fields_from_image()
    end subroutine refresh

    logical function pending_identity()
        pending_identity = pix_is_identity(levels) .and. pix_is_identity(adjust) .and. &
            sharpen_amount <= 0.0
    end function pending_identity

    subroutine reset_pending()
        levels = pix_levels_t()
        adjust = pix_adjust_t()
        sharpen_amount = 0.0
        sharpen_radius = 1.0
        shown_dirty = .true.
    end subroutine reset_pending

    subroutine apply_pending()
        !! Commits the live adjustments as steps confined to the selection.
        type(step_t) :: s
        if (pending_identity()) return
        if (.not. pix_is_identity(levels)) then
            s = new_step(STEP_LEVELS)
            s%levels = levels
            s%selection = sel
            call push(s)
        end if
        if (.not. pix_is_identity(adjust)) then
            s = new_step(STEP_ADJUST)
            s%adjust = adjust
            s%selection = sel
            call push(s)
        end if
        if (sharpen_amount > 0.0) then
            s = new_step(STEP_SHARPEN)
            s%radius = sharpen_radius * base_scale / real(size(base, 2))
            s%amount = sharpen_amount
            s%threshold = 2.0
            s%selection = sel
            call push(s)
        end if
        call reset_pending()
    end subroutine apply_pending

    subroutine pack_rgba(img, rgba)
        integer(int8), intent(in) :: img(:, :, :)
        integer(int32), allocatable, intent(inout) :: rgba(:, :)
        integer :: x, y
        if (allocated(rgba)) then
            if (size(rgba, 1) /= size(img, 2) .or. size(rgba, 2) /= size(img, 3)) deallocate(rgba)
        end if
        if (.not. allocated(rgba)) allocate(rgba(size(img, 2), size(img, 3)))
        do y = 1, size(img, 3)
            do x = 1, size(img, 2)
                rgba(x, y) = ui_rgba(u8(img(1, x, y)), u8(img(2, x, y)), u8(img(3, x, y)))
            end do
        end do
    end subroutine pack_rgba

    ! --- selection ----------------------------------------------------------------

    subroutine deselect()
        if (allocated(sel%shapes)) deallocate(sel%shapes)
        if (allocated(sel_mask)) deallocate(sel_mask)
        sel_gen = sel_gen + 1
        shown_dirty = .true.
    end subroutine deselect

    subroutine add_shape(s)
        type(shape_t), intent(inout) :: s
        type(shape_t), allocatable :: grown(:)
        integer :: n
        select case (sel_mode_tab)
        case (2)
            s%mode = MASK_ADD
        case (3)
            s%mode = MASK_SUBTRACT
        case (4)
            s%mode = MASK_INTERSECT
        case default
            s%mode = MASK_REPLACE
        end select
        if (s%mode == MASK_REPLACE .or. .not. allocated(sel%shapes)) then
            if (allocated(sel%shapes)) deallocate(sel%shapes)
            allocate(sel%shapes(0))
        end if
        n = size(sel%shapes)
        allocate(grown(n + 1))
        grown(1:n) = sel%shapes
        grown(n + 1) = s
        call move_alloc(grown, sel%shapes)
        call update_selection()
    end subroutine add_shape

    subroutine update_selection()
        !! Mask on the current proxy, and an overlay darkening what lies outside.
        integer :: x, y, a
        call ui_request_frame()
        if (allocated(sel_mask)) deallocate(sel_mask)
        if (.not. allocated(base)) return
        call selection_mask(sel, base, sel_mask)
        if (allocated(sel_rgba)) deallocate(sel_rgba)
        if (allocated(sel_mask)) then
            allocate(sel_rgba(size(sel_mask, 1), size(sel_mask, 2)))
            do y = 1, size(sel_mask, 2)
                do x = 1, size(sel_mask, 1)
                    a = nint(120.0 * (1.0 - sel_mask(x, y)))
                    sel_rgba(x, y) = ui_rgba(10, 20, 60, a)
                end do
            end do
        end if
        sel_gen = sel_gen + 1
        shown_dirty = .true.
    end subroutine update_selection

    ! --- geometry steps -----------------------------------------------------------

    subroutine turn(turns)
        integer, intent(in) :: turns
        type(step_t) :: s
        if (.not. have_photo()) return
        call apply_pending()
        s = new_step(STEP_TURN)
        s%turns = turns
        call push(s)
        call deselect()
        fitted = .false.
    end subroutine turn

    subroutine flip(horizontal)
        logical, intent(in) :: horizontal
        type(step_t) :: s
        if (.not. have_photo()) return
        call apply_pending()
        s = new_step(STEP_FLIP)
        s%horizontal = horizontal
        call push(s)
        call deselect()
    end subroutine flip

    subroutine apply_crop()
        type(step_t) :: s
        if (.not. crop_set) return
        call apply_pending()
        s = new_step(STEP_CROP)
        s%x0 = max(0.0, min(gx0, gx1))
        s%y0 = max(0.0, min(gy0, gy1))
        s%x1 = min(1.0, max(gx0, gx1))
        s%y1 = min(1.0, max(gy0, gy1))
        crop_set = .false.
        if (s%x1 - s%x0 < 1e-3 .or. s%y1 - s%y0 < 1e-3) return
        call push(s)
        call deselect()
        fitted = .false.
    end subroutine apply_crop

    subroutine crop_to_selection()
        integer :: x0, y0, x1, y1
        if (.not. allocated(sel_mask)) return
        call pix_mask_bounds(sel_mask, x0, y0, x1, y1)
        if (x1 < x0) return
        gx0 = real(x0 - 1) / size(base, 2)
        gy0 = real(y0 - 1) / size(base, 3)
        gx1 = real(x1) / size(base, 2)
        gy1 = real(y1) / size(base, 3)
        crop_set = .true.
        call apply_crop()
    end subroutine crop_to_selection

    subroutine apply_straighten()
        type(step_t) :: s
        if (abs(straighten) < 0.05) return
        call apply_pending()
        s = new_step(STEP_STRAIGHTEN)
        s%degrees = straighten
        straighten = 0.0
        call push(s)
        call deselect()
        fitted = .false.
    end subroutine apply_straighten

    integer function full_width()
        integer :: h
        call edit_size(doc, full_width, h)
    end function full_width

    integer function full_height()
        integer :: w
        call edit_size(doc, w, full_height)
    end function full_height

    subroutine size_fields_from_image()
        if (.not. allocated(base)) return
        size_w = ui_str(full_width())
        size_h = ui_str(full_height())
    end subroutine size_fields_from_image

    integer function parse(t, fallback)
        character(*), intent(in) :: t
        integer, intent(in) :: fallback
        integer :: ios
        read(t, *, iostat=ios) parse
        if (ios /= 0 .or. parse < 1) parse = fallback
    end function parse

    subroutine apply_resize()
        type(step_t) :: s
        integer :: w, h
        w = min(30000, parse(size_w, full_width()))
        h = min(30000, parse(size_h, full_height()))
        if (w == full_width() .and. h == full_height()) return
        call apply_pending()
        s = new_step(STEP_RESIZE)
        s%width = w
        s%height = h
        call push(s)
        call deselect()
        fitted = .false.
    end subroutine apply_resize

    ! --- top bar and tools --------------------------------------------------------

    subroutine top_bar()
        call row_begin(6.0)
        call file_open_button("open", "Open...", chooser, "Open photo", folder, &
            ".jpg,.jpeg,.png")
        if (button("export-jpeg", "Export JPEG", enabled=have_photo())) call export("jpg")
        if (button("export-png", "Export PNG", enabled=have_photo())) call export("png")
        if (button("print", "Print...", enabled=have_photo() .and. print_supported)) call start_print()
        if (button("undo", "Undo", enabled=doc%active > 0)) call undo()
        if (button("redo", "Redo", enabled=doc%active < doc%count)) call redo()
        if (button("left", "Rotate left", enabled=have_photo())) call turn(-1)
        if (button("right", "Rotate right", enabled=have_photo())) call turn(1)
        if (button("fliph", "Flip H", enabled=have_photo())) call flip(.true.)
        if (button("flipv", "Flip V", enabled=have_photo())) call flip(.false.)
        if (button("fit", "Fit", enabled=have_photo())) fitted = .false.
        if (button("actual", "100%", enabled=have_photo())) call zoom_actual()
        if (len(status) > 0) then
            call text(status, tone=UI_TONE_ATTENTION, ellipsis=.true.)
        else if (len(path) > 0) then
            call text(file_name(path), style=UI_CAPTION, ellipsis=.true.)
        end if
        call row_end()
    end subroutine top_bar

    subroutine tools()
        integer :: k
        call column_begin(4.0)
        do k = 1, size(TOOL_NAMES)
            if (button("tool", trim(TOOL_NAMES(k)), key=k, width=TOOLS_W - 4.0, &
                pressed=tool == k)) then
                if (tool == T_CROP .and. k /= T_CROP) crop_set = .false.
                tool = k
            end if
        end do
        call column_end()
    end subroutine tools

    ! --- canvas -------------------------------------------------------------------

    subroutine editor_canvas()
        type(canvas_events_t) :: ev
        real :: w, h, fx, fy
        call canvas_begin("image", view, ev, fill=ui_rgba(52, 54, 60))
        if (.not. have_photo()) then
            call canvas_end(view)
            return
        end if
        w = real(size(shown, 2))
        h = real(size(shown, 3))
        if (.not. fitted .and. ev%width > 0.0) then
            view%zoom = 0.94 * min(ev%width / w, ev%height / h)
            view%pan_x = 0.5 * (ev%width - w * view%zoom)
            view%pan_y = 0.5 * (ev%height - h * view%zoom)
            fitted = .true.
        end if
        if (ev%wheel /= 0.0) then
            call canvas_zoom_at(view, 1.15**ev%wheel, ev%local_x, ev%local_y, 0.02, 64.0)
        end if
        fx = ev%x / w
        fy = ev%y / h
        if (ev%dragging .and. (tool == T_MOVE .or. ev%mods == MOD_SHIFT .and. tool == T_PEN)) then
            view%pan_x = view%pan_x + ev%drag_dx
            view%pan_y = view%pan_y + ev%drag_dy
        else
            call canvas_gesture(ev, fx, fy, w, h)
        end if
        call canvas_image("photo", shown_gen, shown_rgba, 0.0, 0.0, w, h)
        if (allocated(sel_rgba)) then
            if (size(sel_rgba, 1) == size(shown, 2)) &
                call canvas_image("selection", sel_gen, sel_rgba, 0.0, 0.0, w, h)
        end if
        call overlays(w, h)
        call canvas_end(view)
    end subroutine editor_canvas

    subroutine canvas_gesture(ev, fx, fy, w, h)
        type(canvas_events_t), intent(in) :: ev
        real, intent(in) :: fx, fy, w, h
        type(shape_t) :: s
        select case (tool)
        case (T_RECT, T_ELLIPSE, T_CROP)
            if (ev%drag_started .or. (ev%pressed .and. .not. gesture)) then
                gesture = .true.
                gx0 = clamp01(ev%press_x / w)
                gy0 = clamp01(ev%press_y / h)
                gx1 = gx0
                gy1 = gy0
            end if
            if (gesture .and. (ev%dragging .or. ev%drag_ended)) then
                gx1 = clamp01(fx)
                gy1 = clamp01(fy)
                if (tool == T_CROP) call constrain_ratio(w, h)
            end if
            if (gesture .and. (ev%released .or. ev%drag_ended)) then
                gesture = .false.
                if (abs(gx1 - gx0) * w < 2.0 .or. abs(gy1 - gy0) * h < 2.0) then
                    if (tool /= T_CROP) call deselect()
                    crop_set = .false.
                else if (tool == T_CROP) then
                    crop_set = .true.
                else
                    s%kind = merge(SHAPE_RECT, SHAPE_ELLIPSE, tool == T_RECT)
                    s%x0 = gx0
                    s%y0 = gy0
                    s%x1 = gx1
                    s%y1 = gy1
                    call add_shape(s)
                end if
            end if
        case (T_LASSO)
            if (ev%pressed) then
                gesture = .true.
                ln = 0
                call lasso_add(clamp01(fx), clamp01(fy), 0.0)
            else if (gesture .and. ev%dragging) then
                call lasso_add(clamp01(fx), clamp01(fy), 0.0)
            end if
            if (gesture .and. (ev%released .or. ev%drag_ended)) then
                gesture = .false.
                if (ln >= 3) then
                    s%kind = SHAPE_LASSO
                    s%px = lx(1:ln)
                    s%py = ly(1:ln)
                    call add_shape(s)
                end if
                ln = 0
            end if
        case (T_WAND)
            if (ev%clicked .and. fx >= 0.0 .and. fx <= 1.0 .and. fy >= 0.0 .and. fy <= 1.0) then
                s%kind = SHAPE_WAND
                s%x0 = fx
                s%y0 = fy
                s%tolerance = nint(tolerance)
                s%contiguous = contiguous
                call add_shape(s)
            end if
        case (T_WHITE)
            if (ev%clicked .and. fx >= 0.0 .and. fx < 1.0 .and. fy >= 0.0 .and. fy < 1.0) then
                call white_from(int(fx * w) + 1, int(fy * h) + 1)
            end if
        case (T_PEN)
            if (ev%pressed) then
                gesture = .true.
                ln = 0
                call ink_reset(ink)
                call pen_sample(ev%x, ev%y, .false.)
            else if (gesture .and. ev%dragging) then
                call pen_sample(ev%x, ev%y, .false.)
            end if
            if (gesture .and. (ev%released .or. ev%drag_ended)) then
                call pen_sample(ev%x, ev%y, .true.)
                gesture = .false.
                call commit_pen(w)
            end if
        end select
    end subroutine canvas_gesture

    pure real function clamp01(v)
        real, intent(in) :: v
        clamp01 = max(0.0, min(1.0, v))
    end function clamp01

    subroutine constrain_ratio(w, h)
        !! Keeps the crop box at the chosen width:height ratio, in the
        !! photo's orientation.
        real, intent(in) :: w, h
        real :: ratio, bw, bh, sx, sy
        ratio = RATIOS(ratio_tab)
        if (ratio < 0.0) ratio = w / h
        if (ratio == 0.0) return
        if (h > w .and. ratio > 1.0) ratio = 1.0 / ratio
        bw = abs(gx1 - gx0) * w
        bh = abs(gy1 - gy0) * h
        if (bw / max(bh, 1e-3) > ratio) then
            bw = bh * ratio
        else
            bh = bw / ratio
        end if
        sx = sign(1.0, gx1 - gx0)
        sy = sign(1.0, gy1 - gy0)
        gx1 = gx0 + sx * bw / w
        gy1 = gy0 + sy * bh / h
    end subroutine constrain_ratio

    subroutine lasso_add(x, y, r)
        real, intent(in) :: x, y, r
        real, allocatable :: gx(:), gy(:), gr(:)
        if (.not. allocated(lx)) allocate(lx(256), ly(256), lr(256))
        if (ln == size(lx)) then
            allocate(gx(2 * ln), gy(2 * ln), gr(2 * ln))
            gx(1:ln) = lx(1:ln)
            gy(1:ln) = ly(1:ln)
            gr(1:ln) = lr(1:ln)
            call move_alloc(gx, lx)
            call move_alloc(gy, ly)
            call move_alloc(gr, lr)
        end if
        ln = ln + 1
        lx(ln) = x
        ly(ln) = y
        lr(ln) = r
    end subroutine lasso_add

    subroutine pen_sample(x, y, last)
        !! FortInk filters the position in screen millimetres, as for the
        !! notebook, so its tuning holds at every zoom.
        real, intent(in) :: x, y
        logical, intent(in) :: last
        integer(int64) :: now, rate
        real(c_double) :: out(3), k
        call system_clock(now, rate)
        k = real(view%zoom, c_double) * MM_PER_PX
        call ink_sample(ink, x * k, y * k, real(now - clock0, c_double) / rate, 0.5_c_double, &
            real(pen_mm, c_double), out, endpoint=last)
        call lasso_add(real(out(1) / k), real(out(2) / k), real(out(3) / k))
    end subroutine pen_sample

    subroutine commit_pen(w)
        real, intent(in) :: w
        type(step_t) :: s
        if (ln < 1) return
        call apply_pending()
        s = new_step(STEP_INK)
        s%ix = lx(1:ln) / w
        s%iy = ly(1:ln) / w
        s%ir = lr(1:ln) / w
        s%rgb = PALETTE(:, colour)
        ln = 0
        call push(s)
    end subroutine commit_pen

    subroutine white_from(x, y)
        integer, intent(in) :: x, y
        type(step_t) :: s
        call apply_pending()
        s = new_step(STEP_ADJUST)
        s%adjust%gains = pix_neutral_gains(u8(base(1, x, y)), u8(base(2, x, y)), u8(base(3, x, y)))
        s%selection = sel
        call push(s)
        tool = T_MOVE
    end subroutine white_from

    subroutine overlays(w, h)
        !! Gesture feedback in image coordinates: boxes, lasso, pen, crop shade.
        real, intent(in) :: w, h
        real :: a, b, c, d
        integer :: k
        real, allocatable :: xy(:, :)
        integer, allocatable :: seg(:, :)
        integer(int32) :: shade(1, 1)
        if ((gesture .and. (tool == T_RECT .or. tool == T_ELLIPSE .or. tool == T_CROP)) .or. &
            (crop_set .and. tool == T_CROP)) then
            a = min(gx0, gx1) * w
            b = min(gy0, gy1) * h
            c = max(gx0, gx1) * w
            d = max(gy0, gy1) * h
            if (tool == T_CROP) then
                shade(1, 1) = ui_rgba(0, 0, 0, 150)
                call canvas_image("shade-t", 1, shade, 0.0, 0.0, w, b)
                call canvas_image("shade-b", 1, shade, 0.0, d, w, h - d)
                call canvas_image("shade-l", 1, shade, 0.0, b, a, d - b)
                call canvas_image("shade-r", 1, shade, c, b, w - c, d - b)
            end if
            xy = reshape([a, b, c, b, c, d, a, d], [2, 4])
            seg = reshape([1, 2, 2, 3, 3, 4, 4, 1], [2, 4])
            call canvas_lines("box", shown_gen + nint(1000 * (a + b + c + d)), xy, &
                [ui_rgba(255, 255, 255)], seg, width=1.5)
        end if
        if (ln >= 2 .and. (tool == T_LASSO .or. tool == T_PEN)) then
            allocate(xy(2, ln), seg(2, ln - 1))
            if (tool == T_LASSO) then
                xy(1, :) = lx(1:ln) * w
                xy(2, :) = ly(1:ln) * h
            else
                xy(1, :) = lx(1:ln)
                xy(2, :) = ly(1:ln)
            end if
            do k = 1, ln - 1
                seg(:, k) = [k, k + 1]
            end do
            if (tool == T_PEN) then
                call canvas_lines("ink", ln, xy, [ui_rgba(PALETTE(1, colour), PALETTE(2, colour), &
                    PALETTE(3, colour))], seg, width=max(1.0, 2.0 * sum(lr(1:ln)) / ln * view%zoom))
            else
                call canvas_lines("lasso", ln, xy, [ui_rgba(255, 255, 255)], seg, width=1.5)
            end if
        end if
    end subroutine overlays

    subroutine zoom_actual()
        if (.not. have_photo()) return
        view%zoom = base_scale
        fitted = .true.
    end subroutine zoom_actual

    ! --- panel --------------------------------------------------------------------

    subroutine panel()
        call column_begin(6.0)
        call histogram()
        if (have_photo()) call text(ui_str(full_width()) // " x " // ui_str(full_height()) // &
            " px, " // ui_str(doc%active) // merge(" step ", " steps", doc%active == 1), &
            style=UI_CAPTION)
        call tool_options()
        if (tabs("panel", [character(len=8) :: "Adjust", "Size", "History"], panel_tab)) continue
        select case (panel_tab)
        case (1)
            call adjust_panel()
        case (2)
            call size_panel()
        case default
            call history_panel()
        end select
        call column_end()
    end subroutine panel

    subroutine histogram()
        !! Luminosity as grey bars, R, G and B as curves, each scaled to the
        !! highest bin outside the clipped ends.
        real, parameter :: HW = PANEL_W - 8.0, HH = 110.0
        real :: xy(2, 1024), top
        integer(int32) :: rgba(1024)
        integer :: seg(2, 1024), k, c, n
        type(canvas_events_t) :: ev
        call canvas_begin("histogram", hview, ev, width=HW, height=HH, fill=ui_rgba(32, 33, 36))
        if (have_photo()) then
            top = real(max(1_int64, maxval(hist(1:254, :))))
            n = 0
            do k = 0, 255
                n = n + 1
                xy(:, 2 * n - 1) = [(k + 0.5) * HW / 256.0, HH]
                xy(:, 2 * n) = [(k + 0.5) * HW / 256.0, HH * (1.0 - min(1.0, hist(k, 4) / top))]
                rgba(2 * n - 1:2 * n) = ui_rgba(150, 150, 150)
                seg(:, n) = [2 * n - 1, 2 * n]
            end do
            call canvas_lines("luma", shown_gen, xy(:, 1:512), rgba(1:512), seg(:, 1:256), &
                width=HW / 256.0 + 0.5)
            do c = 1, 3
                do k = 0, 255
                    xy(:, k + 1) = [(k + 0.5) * HW / 256.0, HH * (1.0 - min(1.0, hist(k, c) / top))]
                    rgba(k + 1) = ui_rgba(merge(255, 60, c == 1), merge(220, 60, c == 2), &
                        merge(255, 60, c == 3), 200)
                end do
                do k = 1, 255
                    seg(:, k) = [k, k + 1]
                end do
                call canvas_lines(merge("r", merge("g", "b", c == 2), c == 1), shown_gen, &
                    xy(:, 1:256), rgba(1:256), seg(:, 1:255), width=1.0)
            end do
        end if
        call canvas_end(hview)
    end subroutine histogram

    subroutine tool_options()
        integer :: k
        select case (tool)
        case (T_RECT, T_ELLIPSE, T_LASSO, T_WAND)
            call text("Selection", style=UI_HEADING)
            if (tabs("selmode", [character(len=9) :: "New", "Add", "Subtract", "Intersect"], &
                sel_mode_tab)) continue
            if (tool == T_WAND) then
                call text("Tolerance " // ui_str(nint(tolerance)), style=UI_CAPTION)
                if (slider("tolerance", 0.0, 128.0, tolerance, width=PANEL_W - 8.0)) continue
                if (checkbox("contiguous", contiguous, "Contiguous")) continue
            end if
            call row_begin(6.0)
            if (button("deselect", "Deselect", enabled=allocated(sel_mask))) call deselect()
            if (button("cropsel", "Crop to selection", enabled=allocated(sel_mask))) &
                call crop_to_selection()
            call row_end()
        case (T_CROP)
            call text("Crop", style=UI_HEADING)
            if (tabs("ratio", [character(len=8) :: "Free", "1:1", "3:2", "4:3", "16:9", "Photo"], &
                ratio_tab)) continue
            call row_begin(6.0)
            if (button("cropok", "Crop", enabled=crop_set, emphasis=UI_EMPHASIS_PRIMARY)) &
                call apply_crop()
            if (button("cropno", "Cancel", enabled=crop_set)) crop_set = .false.
            call row_end()
        case (T_PEN)
            call text("Pen", style=UI_HEADING)
            do k = 1, 6
                if (mod(k, 3) == 1) call row_begin(4.0, id="colours" // ui_str(k))
                if (button("colour", trim(COLOUR_NAMES(k)), key=k, width=(PANEL_W - 16.0) / 3.0, &
                    pressed=colour == k)) colour = k
                if (mod(k, 3) == 0) call row_end()
            end do
            call text("Width " // ui_str(pen_mm, 1) // " mm on screen", style=UI_CAPTION)
            if (slider("penwidth", 0.3, 6.0, pen_mm, width=PANEL_W - 8.0)) continue
        case (T_WHITE)
            call text("Click a spot that should be neutral grey or white.", style=UI_CAPTION, &
                width=PANEL_W - 8.0)
        case default
            call text("Drag to move, wheel to zoom.", style=UI_CAPTION)
        end select
    end subroutine tool_options

    subroutine adjust_panel()
        type(pix_levels_t) :: before
        type(pix_adjust_t) :: was
        real :: amount
        before = levels
        was = adjust
        amount = sharpen_amount
        call text("Levels", style=UI_HEADING)
        call labelled_slider("black", "Black point", 0.0, 254.0, levels%black(0))
        call labelled_slider("gamma", "Midtones", 0.2, 5.0, levels%gamma(0))
        call labelled_slider("white", "White point", 1.0, 255.0, levels%white(0))
        if (button("auto", "Auto levels", enabled=have_photo())) then
            levels = pix_auto_levels(hist_of_base(), 0.001)
        end if
        call text("Colour", style=UI_HEADING)
        call labelled_slider("exposure", "Exposure", -3.0, 3.0, adjust%exposure)
        call labelled_slider("brightness", "Brightness", -1.0, 1.0, adjust%brightness)
        call labelled_slider("contrast", "Contrast", -1.0, 1.0, adjust%contrast)
        call labelled_slider("saturation", "Saturation", -1.0, 1.0, adjust%saturation)
        call labelled_slider("temperature", "Temperature", -1.0, 1.0, adjust%temperature)
        call labelled_slider("tint", "Tint", -1.0, 1.0, adjust%tint)
        call text("Sharpen", style=UI_HEADING)
        call labelled_slider("sharpen", "Amount", 0.0, 3.0, sharpen_amount)
        call labelled_slider("radius", "Radius (px)", 0.3, 5.0, sharpen_radius)
        call row_begin(6.0)
        if (button("apply", "Apply", enabled=.not. pending_identity(), &
            emphasis=UI_EMPHASIS_PRIMARY)) call apply_pending()
        if (button("reset", "Reset", enabled=.not. pending_identity())) call reset_pending()
        call row_end()
        if (allocated(sel_mask)) call text("Applies inside the selection.", style=UI_CAPTION)
        if (.not. same_levels(before, levels) .or. .not. same_adjust(was, adjust) .or. &
            amount /= sharpen_amount) shown_dirty = .true.
    end subroutine adjust_panel

    function hist_of_base() result(h)
        integer(int64) :: h(0:255, 4)
        h = 0
        if (allocated(base)) call pix_histogram(base, h)
    end function hist_of_base

    logical function same_levels(a, b)
        type(pix_levels_t), intent(in) :: a, b
        same_levels = all(a%black == b%black) .and. all(a%white == b%white) .and. &
            all(a%gamma == b%gamma)
    end function same_levels

    logical function same_adjust(a, b)
        type(pix_adjust_t), intent(in) :: a, b
        same_adjust = a%exposure == b%exposure .and. a%brightness == b%brightness .and. &
            a%contrast == b%contrast .and. a%saturation == b%saturation .and. &
            a%temperature == b%temperature .and. a%tint == b%tint .and. all(a%gains == b%gains)
    end function same_adjust

    subroutine labelled_slider(id, label, lo, hi, value)
        character(*), intent(in) :: id, label
        real, intent(in) :: lo, hi
        real, intent(inout) :: value
        call row_begin(6.0, id=id)
        call text(label, width=96.0, style=UI_CAPTION)
        if (slider("s", lo, hi, value, width=PANEL_W - 172.0)) continue
        call text(ui_str(value, 2), width=56.0, style=UI_CAPTION)
        call row_end()
    end subroutine labelled_slider

    subroutine size_panel()
        logical :: changed
        real :: inches_w, inches_h
        call text("Straighten", style=UI_HEADING)
        call labelled_slider("straighten", "Angle", -15.0, 15.0, straighten)
        if (button("straighten-ok", "Straighten and crop", enabled=abs(straighten) >= 0.05)) &
            call apply_straighten()
        call text("Image size", style=UI_HEADING)
        if (.not. have_photo()) return
        call row_begin(6.0)
        call text("Width", width=60.0, style=UI_CAPTION)
        changed = text_input("w", size_w, width=90.0)
        if (changed .and. keep_ratio) size_h = ui_str(nint(parse(size_w, full_width()) * &
            real(full_height()) / full_width()))
        call text("px", style=UI_CAPTION)
        call row_end()
        call row_begin(6.0)
        call text("Height", width=60.0, style=UI_CAPTION)
        changed = text_input("h", size_h, width=90.0)
        if (changed .and. keep_ratio) size_w = ui_str(nint(parse(size_h, full_height()) * &
            real(full_width()) / full_height()))
        call text("px", style=UI_CAPTION)
        call row_end()
        if (checkbox("ratio", keep_ratio, "Keep proportions")) continue
        call row_begin(6.0)
        call text("Resolution", width=60.0, style=UI_CAPTION)
        if (text_input("dpi", size_dpi, width=90.0)) dpi = parse(size_dpi, 300)
        call text("pixels per inch", style=UI_CAPTION)
        call row_end()
        inches_w = parse(size_w, full_width()) / real(dpi)
        inches_h = parse(size_h, full_height()) / real(dpi)
        call text("Print size " // ui_str(inches_w * 2.54, 1) // " x " // &
            ui_str(inches_h * 2.54, 1) // " cm at " // ui_str(dpi) // " ppi", style=UI_CAPTION)
        call row_begin(6.0)
        if (button("resize", "Resample", emphasis=UI_EMPHASIS_PRIMARY)) call apply_resize()
        if (button("resize-reset", "Reset")) call size_fields_from_image()
        call row_end()
    end subroutine size_panel

    subroutine history_panel()
        character(len=40), allocatable :: items(:)
        integer :: k, selected
        allocate(items(doc%count + 1))
        items(1) = "Original"
        do k = 1, doc%count
            items(k + 1) = edit_name(doc%steps(k))
            if (k > doc%active) items(k + 1) = trim(items(k + 1)) // " (undone)"
        end do
        selected = doc%active + 1
        if (list_box("history", items, selected, width=PANEL_W - 8.0, height=260.0)) then
            ! Choosing an entry undoes or redoes up to it.
            doc%active = selected - 1
            base_dirty = .true.
            call deselect()
        end if
    end subroutine history_panel

    ! --- output -------------------------------------------------------------------

    subroutine full_render(img)
        integer(int8), allocatable, intent(out) :: img(:, :, :)
        real :: s
        call apply_pending()
        s = 1.0
        call edit_render(doc, original%rgb, 1, doc%active, img, s)
    end subroutine full_render

    subroutine export(kind)
        character(*), intent(in) :: kind
        integer(int8), allocatable :: img(:, :, :)
        character(len=:), allocatable :: target, message
        call full_render(img)
        call export_photo(img, path, kind, dpi, target, message)
        if (len(message) > 0) then
            status = message
        else
            status = "Saved " // file_name(target)
        end if
    end subroutine export

    subroutine start_print()
        integer(int8), allocatable :: img(:, :, :)
        if (.not. have_photo()) return
        call full_render(img)
        call print_photo_set(img, file_name(path))
        printing = .true.
    end subroutine start_print
end module photo_app
