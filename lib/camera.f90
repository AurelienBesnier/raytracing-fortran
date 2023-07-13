module class_camera
    use class_vec3
    use class_ray
    implicit none
    private
    public :: camera, create_camera, get_ray

    type camera
        real :: aspect_ratio = 16.0 / 9.0, viewport_height = 2.0, viewport_width, focal_length = 1.0
        type(vec3) :: origin, horizontal, vertical, lower_left_corner
    end type camera

contains
    type(camera) function create_camera()
        implicit none
        create_camera%viewport_width  = create_camera%aspect_ratio * create_camera%viewport_height
        create_camera%horizontal = vec3(create_camera%viewport_width, 0.0, 0.0)
        create_camera%vertical = vec3(0.0, create_camera%viewport_height, 0.0)
        create_camera%lower_left_corner = minus_vec(minus_vec(minus_vec(create_camera%origin, &
        div_vec(create_camera%horizontal,2.0)),div_vec(create_camera%vertical,2.0)), vec3(0.0,0.0,create_camera%focal_length))
    end function

    PURE type(ray) function get_ray(cam, u, v)
        implicit none
        type(camera), intent(in) :: cam
        real, intent(in) :: u, v
        type(vec3) :: dir_ray
        dir_ray = minus_vec(add_vec(add_vec(cam%lower_left_corner, mul_vec_d(u, cam%horizontal)), mul_vec_d(v, cam%vertical)),&
                cam%origin)
        get_ray = ray(cam%origin, dir_ray)
    end function

end module class_camera
