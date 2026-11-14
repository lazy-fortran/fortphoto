program fortphoto
    !! Open a photo, see exactly how it lands on the paper, and print it.
    !!
    !! fortphoto [--hold] [IMAGE]
    !!   --hold  queue print jobs held (release: lp -i JOB -H resume)
    !!   Left/Right: previous/next photo in the folder
    use iso_fortran_env, only: int8, int32
    use fortui
    use fortui_backends, only: ui_init
    use fortui_print, only: print_printer_t, print_caps_t, print_media_t, print_job_t, &
        print_result_t, print_printers, print_capabilities, print_page, &
        print_page_pixels, print_available, PRINT_OK, PWG_QUALITY_DRAFT, &
        PWG_QUALITY_NORMAL, PWG_QUALITY_HIGH
    use photo_image, only: image_t, image_load, image_shrink, image_glob
    use photo_layout, only: layout_t, layout_compute, layout_render, rotate_cw, &
        layout_offset_by, MODE_FILL, MODE_FIT
    implicit none

    integer, parameter :: PREVIEW_SOURCE = 1600, LOW_DPI = 200
    real, parameter :: PANEL_W = 340.0

    type :: size_t
        !! One paper size; `plain`/`edge` index its bordered/borderless media.
        integer :: plain = 0, edge = 0
    end type size_t

    type(image_t) :: photo, small, dimmed
    character(len=:), allocatable :: path, status, folder
    character(len=:), allocatable :: files(:)
    type(print_printer_t), allocatable :: printers(:)
    type(print_caps_t) :: caps
    type(size_t), allocatable :: sizes(:)
    type(layout_t) :: lay
    type(canvas_view_t) :: view
    integer(int32), allocatable :: page_rgba(:, :), dim_rgba(:, :)
    integer :: printer = 0, paper = 0, mtype = 0, source = 0, quality_tab = 3
    integer :: mode_tab = 1, copies = 1, zoom_pct = 100, gen = 0, dim_gen = 0, stat
    logical :: borderless = .true., all_sizes = .false., rotated = .false.
    logical :: dirty = .true., hold = .false.
    real :: ox = 0.0, oy = 0.0, preview_scale = 1.0, cw = 0.0, ch = 0.0
    real :: sheet_x = 0.0, sheet_y = 0.0, sheet_scale = 1.0

    status = ""
    path = ""
    folder = "."
    call command_argument_count_path()
    call load_printers()
    if (len(path) > 0) call open_photo(path)

    call ui_init("gui", 1280, 900, "fortphoto", stat=stat)
    if (stat /= 0) error stop "fortphoto needs a graphical display"
    call ui_run(frame)
    call ui_shutdown()

contains

    subroutine command_argument_count_path()
        integer :: n, k
        character(len=4096) :: arg

        do k = 1, command_argument_count()
            call get_command_argument(k, arg, n)
            if (trim(arg) == "--hold") then
                hold = .true.
            else
                path = trim(arg)
            end if
        end do
    end subroutine command_argument_count_path

    subroutine frame()
        if (ui_key_pressed(KEY_ESCAPE)) call ui_request_quit()
        if (ui_key_pressed(KEY_ARROW_RIGHT)) call step_photo(1)
        if (ui_key_pressed(KEY_ARROW_LEFT)) call step_photo(-1)
        ! The sidebar reports on the layout before the preview redraws.
        if (dirty) call relayout()
        call split_begin("main", first=PANEL_W, gap=12.0)
        call sidebar()
        call split_next()
        call preview()
        call split_end()
    end subroutine frame

    ! --- photos -------------------------------------------------------------------

    subroutine open_photo(p)
        character(*), intent(in) :: p
        character(len=:), allocatable :: msg
        integer :: k

        call image_load(p, photo, msg)
        if (len(msg) > 0) then
            status = msg
            photo%width = 0
            return
        end if
        path = p
        k = index(p, "/", back=.true.)
        folder = "."
        if (k > 1) folder = p(1:k - 1)
        call image_glob(folder, files)
        call image_shrink(photo, PREVIEW_SOURCE, small)
        call make_dimmed()
        ox = 0.0
        oy = 0.0
        zoom_pct = 100
        status = ""
        dirty = .true.
    end subroutine open_photo

    subroutine step_photo(dir)
        integer, intent(in) :: dir
        integer :: k, n

        if (.not. allocated(files)) return
        n = size(files)
        if (n == 0) return
        do k = 1, n
            if (same_file(files(k), path)) then
                call open_photo(trim(files(modulo(k - 1 + dir, n) + 1)))
                return
            end if
        end do
        call open_photo(trim(files(1)))
    end subroutine step_photo

    logical function same_file(a, b)
        character(*), intent(in) :: a, b
        same_file = trim(a) == trim(b) .or. trim(a) == "./" // trim(b)
    end function same_file

    subroutine make_dimmed()
        !! The whole photo, darkened and half transparent: shows what is cut off.
        integer :: x, y
        integer :: r, g, b

        if (allocated(dim_rgba)) deallocate(dim_rgba)
        allocate(dim_rgba(small%width, small%height))
        do y = 1, small%height
            do x = 1, small%width
                r = iand(int(small%rgb(1, x, y)), 255) / 2
                g = iand(int(small%rgb(2, x, y)), 255) / 2
                b = iand(int(small%rgb(3, x, y)), 255) / 2
                dim_rgba(x, y) = ui_rgba(r, g, b, 150)
            end do
        end do
        dim_gen = dim_gen + 1
    end subroutine make_dimmed

    ! --- printers -----------------------------------------------------------------

    subroutine load_printers()
        type(print_result_t) :: res
        integer :: k

        call print_printers(printers, res)
        if (res%status /= PRINT_OK) status = res%message
        printer = 0
        do k = 1, size(printers)
            if (printers(k)%is_default .or. printer == 0) printer = k
        end do
        if (printer > 0) call load_caps()
    end subroutine load_printers

    subroutine load_caps()
        !! Capabilities of the chosen printer; preselect loaded paper, its tray,
        !! its media type and high quality.
        type(print_result_t) :: res
        integer :: k, j, n, best, rank

        call print_capabilities(printers(printer)%name, caps, res)
        if (res%status /= PRINT_OK) status = res%message
        if (allocated(sizes)) deallocate(sizes)
        allocate(sizes(0))
        do k = 1, size(caps%media)
            n = 0
            do j = 1, size(sizes)
                if (same_size(caps%media(k), caps%media(media_of(sizes(j))))) n = j
            end do
            if (n == 0) then
                sizes = [sizes, size_t()]
                n = size(sizes)
            end if
            if (caps%media(k)%borderless) then
                sizes(n)%edge = k
            else
                sizes(n)%plain = k
            end if
        end do
        paper = 0
        best = 0
        do j = 1, size(sizes)
            k = media_of(sizes(j))
            if (.not. caps%media(k)%loaded) cycle
            ! Photo paper in a paper tray first, then plain paper; never the
            ! disc tray.
            rank = 1
            if (index(caps%media(k)%source, "disc") > 0) rank = 0
            if (rank > 0 .and. len(caps%media(k)%media_type) > 0 .and. &
                index(caps%media(k)%media_type, "stationery") == 0) rank = 2
            if (rank > best) then
                best = rank
                paper = j
            end if
        end do
        if (paper == 0 .and. size(sizes) > 0) paper = 1
        call preselect_for_paper()
        quality_tab = 3
        dirty = .true.
    end subroutine load_caps

    subroutine preselect_for_paper()
        type(print_media_t) :: m

        mtype = 0
        source = 0
        if (paper == 0) return
        m = caps%media(media_of(sizes(paper)))
        mtype = find_choice_type(m%media_type)
        if (mtype == 0) mtype = find_choice_type("photographic")
        source = find_choice_source(m%source)
        borderless = sizes(paper)%edge > 0
    end subroutine preselect_for_paper

    integer function find_choice_type(keyword) result(k)
        character(*), intent(in) :: keyword
        integer :: j
        k = 0
        if (len(keyword) == 0) return
        do j = 1, size(caps%media_types)
            if (caps%media_types(j)%keyword == keyword) k = j
        end do
    end function find_choice_type

    integer function find_choice_source(keyword) result(k)
        character(*), intent(in) :: keyword
        integer :: j
        k = 0
        if (len(keyword) == 0) return
        do j = 1, size(caps%sources)
            if (caps%sources(j)%keyword == keyword) k = j
        end do
    end function find_choice_source

    logical function same_size(a, b)
        type(print_media_t), intent(in) :: a, b
        same_size = abs(a%width - b%width) <= 50 .and. abs(a%length - b%length) <= 50
    end function same_size

    integer function media_of(s) result(k)
        !! The medium printed with: borderless when chosen and offered.
        type(size_t), intent(in) :: s
        k = s%plain
        if ((borderless .or. k == 0) .and. s%edge > 0) k = s%edge
        if (k == 0) k = s%edge
    end function media_of

    function size_label(s) result(t)
        type(size_t), intent(in) :: s
        character(len=:), allocatable :: t
        integer :: k
        k = s%plain
        if (k == 0) k = s%edge
        t = caps%media(k)%label
        if (len(t) == 0) t = caps%media(k)%name
    end function size_label

    ! --- layout -------------------------------------------------------------------

    subroutine relayout()
        !! Layout in the photo's orientation; the sheet turns with the photo.
        type(print_media_t) :: m
        integer :: w, h, l, t, r, b
        real :: mg(4)

        if (paper == 0 .or. photo%width == 0) return
        m = caps%media(media_of(sizes(paper)))
        call print_page_pixels(m, caps%dpi, w, h, l, t, r, b)
        rotated = (photo%width > photo%height) .neqv. (w > h)
        if (w == h) rotated = .false.
        if (rotated) then
            ! Turning the landscape page clockwise gives the printer's portrait
            ! sheet: its top edge becomes the right edge, and so on.
            mg = real([t, r, b, l])
            lay = layout_compute(photo%width, photo%height, real(h), real(w), mg, &
                mode_of(), real(zoom_pct) / 100.0, ox, oy)
        else
            mg = real([l, t, r, b])
            lay = layout_compute(photo%width, photo%height, real(w), real(h), mg, &
                mode_of(), real(zoom_pct) / 100.0, ox, oy)
        end if
    end subroutine relayout

    integer function mode_of()
        mode_of = MODE_FILL
        if (mode_tab == 2) mode_of = MODE_FIT
    end function mode_of

    subroutine refresh_preview(width, height)
        !! Page preview at the size it is shown, from the same layout as the print.
        real, intent(in) :: width, height
        integer(int8), allocatable :: page(:, :, :)
        integer :: x, y

        call relayout()
        if (lay%sheet_w <= 0.0) return
        sheet_scale = 0.86 * min(width / lay%sheet_w, height / lay%sheet_h)
        preview_scale = min(sheet_scale * ui_scale(), 1.0)
        call layout_render(lay, small%rgb, preview_scale, page)
        if (allocated(page_rgba)) deallocate(page_rgba)
        allocate(page_rgba(size(page, 2), size(page, 3)))
        do y = 1, size(page, 3)
            do x = 1, size(page, 2)
                page_rgba(x, y) = ui_rgba(iand(int(page(1, x, y)), 255), &
                    iand(int(page(2, x, y)), 255), iand(int(page(3, x, y)), 255))
            end do
        end do
        sheet_x = 0.5 * (width - lay%sheet_w * sheet_scale)
        sheet_y = 0.5 * (height - lay%sheet_h * sheet_scale)
        gen = gen + 1
        dirty = .false.
    end subroutine refresh_preview

    subroutine preview()
        type(canvas_events_t) :: ev
        real :: w, h

        call canvas_begin("sheet", view, ev, fill=ui_rgba(46, 48, 54))
        w = ev%width
        h = ev%height
        if (abs(w - cw) > 0.5 .or. abs(h - ch) > 0.5) dirty = .true.
        cw = w
        ch = h
        if (photo%width > 0 .and. paper > 0) then
            if (ev%dragging .and. mode_tab == 1 .and. &
                (ev%drag_dx /= 0.0 .or. ev%drag_dy /= 0.0)) then
                call layout_offset_by(lay, ev%drag_dx / sheet_scale, &
                    ev%drag_dy / sheet_scale, ox, oy)
                dirty = .true.
            end if
            if (dirty) call refresh_preview(w, h)
            if (mode_tab == 1) call canvas_image("cut", dim_gen, dim_rgba, &
                sheet_x + lay%px * sheet_scale, sheet_y + lay%py * sheet_scale, &
                lay%pw * sheet_scale, lay%ph * sheet_scale)
            call canvas_image("page", gen, page_rgba, sheet_x, sheet_y, &
                lay%sheet_w * sheet_scale, lay%sheet_h * sheet_scale)
        end if
        call canvas_end(view)
    end subroutine preview

    ! --- controls -----------------------------------------------------------------

    subroutine sidebar()
        logical :: go

        call column_begin(6.0)
        call text("fortphoto", style=UI_TITLE)
        call row_begin(6.0)
        if (text_input("path", path, placeholder="photo.jpg", submitted=go, &
            width=PANEL_W - 112.0)) continue
        if (button("open", "Open") .or. go) call open_photo(trim(path))
        call row_end()
        call row_begin(6.0)
        if (button("prev", "<", width=40.0)) call step_photo(-1)
        if (button("next", ">", width=40.0)) call step_photo(1)
        if (photo%width > 0) then
            call text(ui_str(photo%width) // " x " // ui_str(photo%height) // " px", &
                style=UI_CAPTION)
        end if
        call row_end()

        if (.not. print_available()) then
            call text("Built without printing.", tone=UI_TONE_NEGATIVE)
        else if (size(printers) == 0) then
            call text("No printer is set up.", tone=UI_TONE_NEGATIVE)
        else
            if (size(printers) > 1) then
                call text("Printer", style=UI_HEADING)
                call stepper("printer", printer, size(printers), printers(printer)%name, &
                    changed=go)
                if (go) call load_caps()
            else
                call text("Printer: " // printers(printer)%name, style=UI_CAPTION)
            end if
            call ink_levels()
            call paper_controls()
        end if

        call text("Placement", style=UI_HEADING)
        if (tabs("mode", [character(16) :: "Fill and crop", "Fit whole photo"], &
            mode_tab)) dirty = .true.
        if (mode_tab == 1) then
            call row_begin(6.0)
            call text("Zoom " // ui_str(zoom_pct) // " %, drag the photo to move it", &
                style=UI_CAPTION, width=PANEL_W - 90.0)
            if (button("centre", "Centre", width=76.0)) then
                ox = 0.0
                oy = 0.0
                dirty = .true.
            end if
            call row_end()
            if (slider("zoom", 100, 200, zoom_pct, width=PANEL_W - 8.0)) dirty = .true.
        end if
        if (photo%width > 0 .and. paper > 0) call placement_facts()

        if (print_available() .and. size(printers) > 0) then
            call text("Print quality", style=UI_HEADING)
            if (tabs("quality", [character(8) :: "Draft", "Normal", "High"], &
                quality_tab)) continue
            call row_begin(6.0)
            if (button("print", "Print", width=96.0, emphasis=UI_EMPHASIS_PRIMARY, &
                enabled=photo%width > 0 .and. paper > 0)) call print_photo()
            if (button("fewer", "-", width=32.0)) copies = max(1, copies - 1)
            if (button("more", "+", width=32.0)) copies = min(99, copies + 1)
            call text(ui_str(copies) // merge(" copy  ", " copies", copies == 1))
            call row_end()
        end if
        if (len(status) > 0) call text(status, width=PANEL_W - 8.0)
        call column_end()
    end subroutine sidebar

    subroutine ink_levels()
        integer :: k
        character(len=:), allocatable :: low

        low = ""
        do k = 1, size(caps%markers)
            if (caps%markers(k)%level >= 0 .and. caps%markers(k)%level <= 15) &
                low = low // " " // caps%markers(k)%name
        end do
        if (len(low) > 0) call text("Ink low:" // low, style=UI_CAPTION, &
            tone=UI_TONE_ATTENTION)
    end subroutine ink_levels

    subroutine paper_controls()
        type(print_media_t) :: m
        logical :: changed
        integer :: k, shown

        call text("Paper", style=UI_HEADING)
        shown = 0
        do k = 1, size(sizes)
            m = caps%media(media_of(sizes(k)))
            if (.not. m%loaded) cycle
            shown = shown + 1
            if (button("loaded", loaded_label(k, m), key=k, width=PANEL_W - 8.0, &
                ellipsis=.true.)) then
                paper = k
                call preselect_for_paper()
                dirty = .true.
            end if
        end do
        if (shown == 0 .and. .not. caps%live) call text( &
            "The printer does not say which paper is loaded.", style=UI_CAPTION)
        if (checkbox("all", all_sizes, "Other paper sizes")) continue
        if (all_sizes .and. size(sizes) > 0) then
            call stepper("size", paper, size(sizes), size_label(sizes(paper)), &
                changed=changed)
            if (changed) then
                call preselect_for_paper()
                dirty = .true.
            end if
        end if
        if (paper == 0) return
        if (sizes(paper)%edge > 0 .and. sizes(paper)%plain > 0) then
            if (checkbox("borderless", borderless, "Borderless")) dirty = .true.
        else if (sizes(paper)%edge > 0) then
            call text("Borderless only", style=UI_CAPTION)
        else
            call text("This size has no borderless mode", style=UI_CAPTION)
        end if
        if (size(caps%media_types) > 0) then
            if (mtype == 0) mtype = 1
            call stepper("type", mtype, size(caps%media_types), &
                caps%media_types(mtype)%label, changed=changed)
        end if
        if (size(caps%sources) > 0) then
            if (source == 0) source = 1
            call stepper("tray", source, size(caps%sources), &
                caps%sources(source)%label, changed=changed)
        end if
    end subroutine paper_controls

    function loaded_label(k, m) result(t)
        integer, intent(in) :: k
        type(print_media_t), intent(in) :: m
        character(len=:), allocatable :: t
        integer :: j

        t = size_label(sizes(k))
        if (len(m%source) > 0) then
            j = find_choice_source(m%source)
            if (j > 0) then
                t = t // ", " // caps%sources(j)%label
            else
                t = t // ", " // m%source
            end if
        end if
        if (len(m%media_type) > 0) then
            j = find_choice_type(m%media_type)
            if (j > 0) t = t // ", " // caps%media_types(j)%label
        end if
        if (k == paper) t = "> " // t
    end function loaded_label

    subroutine stepper(id, value, n, label, changed)
        !! Previous/next through a long list, with the current entry's name.
        character(*), intent(in) :: id, label
        integer, intent(inout) :: value
        integer, intent(in) :: n
        logical, intent(out) :: changed

        changed = .false.
        call row_begin(6.0, id=id)
        if (button("down", "<", width=32.0)) then
            value = modulo(value - 2, n) + 1
            changed = .true.
        end if
        if (button("up", ">", width=32.0)) then
            value = modulo(value, n) + 1
            changed = .true.
        end if
        call text(label, width=PANEL_W - 90.0, ellipsis=.true.)
        call row_end()
    end subroutine stepper

    subroutine placement_facts()
        real :: dpi_eff
        integer :: cut

        character(len=:), allocatable :: what

        cut = nint(100.0 * (1.0 - lay%kept))
        dpi_eff = real(caps%dpi) * real(photo%width) / lay%pw
        if (mode_tab == 1) then
            what = ui_str(cut) // " % cut off"
        else
            what = "Whole photo"
        end if
        if (nint(dpi_eff) < LOW_DPI) then
            call text(what // ", only " // ui_str(nint(dpi_eff)) // &
                " pixels per inch: may look soft", style=UI_CAPTION, tone=UI_TONE_ATTENTION)
        else
            call text(what // ", " // ui_str(nint(dpi_eff)) // " photo pixels per inch", &
                style=UI_CAPTION)
        end if
        if (borderless .and. sizes(paper)%edge > 0) call text( &
            "Borderless prints lose about 1-2 mm at each edge.", style=UI_CAPTION)
    end subroutine placement_facts

    subroutine print_photo()
        !! Compose the sheet at the printer's raster resolution and send it.
        type(print_job_t) :: job
        type(print_result_t) :: res
        integer(int8), allocatable :: page(:, :, :), turned(:, :, :)
        integer :: k

        call relayout()
        call layout_render(lay, photo%rgb, 1.0, page)
        if (rotated) then
            call rotate_cw(page, turned)
            call move_alloc(turned, page)
        end if
        job%printer = printers(printer)%name
        k = index(path, "/", back=.true.)
        job%title = path(k + 1:)
        job%media = caps%media(media_of(sizes(paper)))%name
        job%media_type = ""
        if (mtype > 0) job%media_type = caps%media_types(mtype)%keyword
        job%source = ""
        if (source > 0) job%source = caps%sources(source)%keyword
        select case (quality_tab)
        case (1)
            job%quality = PWG_QUALITY_DRAFT
        case (2)
            job%quality = PWG_QUALITY_NORMAL
        case default
            job%quality = PWG_QUALITY_HIGH
        end select
        job%copies = copies
        job%hold = hold
        call print_page(job, page, caps%dpi, res)
        if (res%status == PRINT_OK) then
            status = "Sent as job " // ui_str(res%job_id)
            if (hold) status = "Job " // ui_str(res%job_id) // " held: lp -i " // &
                ui_str(res%job_id) // " -H resume"
        else
            status = res%message
        end if
    end subroutine print_photo

end program fortphoto
