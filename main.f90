module utils
    use class_vec3
    use class_ray
    use class_image
    implicit none
contains
    real function hit_sphere(center, radius, r)
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

    type(vec3) function ray_color(r)
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
            ray_color%y = (1.0 - t) * 1.0 + t * 0.7
            ray_color%z = (1.0 - t) * 1.0 + t * 1.0
        end if

    end function ray_color
end module utils


program raytracing
    use utils

    implicit none

    integer :: i, j, ir, ig, ib
    type(vec3) :: color
    type(ray) :: r
    real :: u, v

    !-- Image
    integer :: image_width = 512
    integer :: image_height
    type(img) :: image_

    !-- Camera
    real :: aspect_ratio = 16.0 / 9.0, viewport_height = 2.0, focal_length = 1.0
    real :: viewport_width
    type(vec3) :: origin = vec3(0.0, 0.0, 0.0)
    type(vec3) :: horizontal
    type(vec3) :: vertical
    type(vec3) :: lower_left_corner
    type(vec3) :: dir_ray

    image_height = floor(image_width / aspect_ratio)
    image_ = create_image(image_width,image_height)

    viewport_width  = aspect_ratio * viewport_height
    horizontal = vec3(viewport_width, 0.0, 0.0)
    vertical = vec3(0.0, viewport_height, 0.0)
    lower_left_corner = minus_vec(minus_vec(minus_vec(origin, div_vec(horizontal,2.0)),&
     div_vec(vertical,2.0)), vec3(0.0,0.0,focal_length))

    do j = image_height-1, 0, -1
        do i = 0, image_width-1
            u = real(i) / real(image_width - 1.0)
            v = real(j) / real(image_height - 1.0)
            dir_ray = minus_vec(add_vec(add_vec(lower_left_corner, mul_vec_d(u, horizontal)), mul_vec_d(v, vertical)),&
                origin)
            r = ray(origin, dir_ray)

            color = ray_color(r)
            call setPixel(j,i,image_,color)

        end do
    end do
    call writePPM(image_)
    call delete_image(image_)
end program raytracing

