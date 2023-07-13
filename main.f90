module utils
    use class_vec3
    use class_ray
    use class_image
    use class_camera
    implicit none
contains
    PURE real function hit_sphere(center, radius, r)
        implicit none
        type(ray), intent(in) :: r
        type(vec3), intent(in) :: center
        real, intent(in) :: radius
        type(vec3) :: oc
        real :: a, half_b, c, discriminant
        oc = minus_vec(r%origin, center)
        a = length_squared(r%direction)
        half_b = dot(oc,r%direction)
        c = length_squared(oc) - radius*radius
        discriminant = half_b*half_b - a * c
        if (discriminant < 0.0) then
            hit_sphere = -1.0
        else
            hit_sphere = (-half_b - sqrt(discriminant)) / a
        end if

    end function hit_sphere

    PURE type(vec3) function ray_color(r)
        implicit none
        type(ray), intent(in) :: r
        type(vec3) :: unit_dir, N
        real :: t
        t = hit_sphere(vec3(0.0,0.0,-1.0), 0.5, r)
        if (t > 0.0) then
            ray_color = vec3(1.0,0.0,0.0)
            N = unit_vector(minus_vec(at(r,t), vec3(0.0,0.0,-1.0)))
            ray_color = mul_vec_d(0.5, vec3(N%x+1,N%y+1,N%z+1))
        else
            unit_dir = unit_vector(r%direction)
            t = 0.5 * (unit_dir%y + 1.0)
            ray_color%x = (1.0 - t) * 1.0 + t * 0.5
            ray_color%y = (1.0 - t) * 1.0 + t * 0.5
            ray_color%z = (1.0 - t) * 1.0 + t * 1.0
        end if
    end function ray_color

    real function random_real()
        random_real = rand()
    end function random_real

end module utils


program raytracing
    use utils
    use omp_lib

    implicit none

    integer :: i, j, s
    type(vec3) :: color
    type(ray) :: r
    real :: u, v

    !-- Image
    integer :: image_width = 800
    integer :: image_height
    integer :: samples_per_pixel = 50
    type(img) :: image_

    !-- Camera
    type(camera) :: cam
    cam = create_camera()

    image_height = floor(image_width / cam%aspect_ratio)
    image_ = create_image(image_width,image_height)
    do j = image_height-1, 0, -1
        do i = 0, image_width-1
            color = vec3(0.0, 0.0, 0.0)
            !$omp parallel do
            do s = 0, samples_per_pixel
                u = real(i + rand()) / real(image_width - 1.0)
                v = real(j+  rand()) / real(image_height - 1.0)

                r = get_ray(cam, u, v)

                color = add_vec(color, ray_color(r))
                !call vec3_print(color)
            end do
            call setPixel(j,i,image_,color, samples_per_pixel)

        end do
    end do
    call writePPM(image_)
    call delete_image(image_)
end program raytracing

