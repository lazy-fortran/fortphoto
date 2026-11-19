program fortphoto
    !! Photo editor: select, crop, rotate, straighten, levels, colour, sharpen,
    !! resize, pen annotations, export and print with an exact preview.
    !!
    !! fortphoto [--hold] [IMAGE]
    !!   --hold  queue print jobs held (release: lp -i JOB -H resume)
    use fortui, only: ui_run, ui_shutdown
    use fortui_backends, only: ui_init
    use photo_app, only: app_setup, app_open_initial, app_frame
    implicit none
    integer :: stat

    call app_setup()
    call ui_init("gui", 1440, 920, "fortphoto", stat=stat)
    if (stat /= 0) error stop "fortphoto could not initialize its UI backend"
    call app_open_initial()
    call ui_run(app_frame)
    call ui_shutdown()
end program fortphoto
