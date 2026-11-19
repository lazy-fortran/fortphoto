program fortphoto_web
    use fortui, only: ui_init, ui_run
    use fortui_web, only: web_backend_t
    use photo_app, only: app_setup, app_frame
    implicit none
    integer :: stat
    call app_setup()
    call ui_init(web_backend_t(root_id='app'), 1440, 920, 'fortphoto', stat=stat)
    if (stat /= 0) error stop 'Browser initialization failed'
    call ui_run(app_frame)
end program fortphoto_web
