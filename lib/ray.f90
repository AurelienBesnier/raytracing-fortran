module class_ray
    use class_vec3
    implicit none
    private
    public :: ray, at

    type ray
        type(vec3) :: origin
        type(vec3) :: direction
    end type ray
contains
    PURE type(vec3) function at(r, t)
        implicit none
        real, intent(in) :: t
        type(ray), intent(in) :: r

        at%x = r%origin%x + t * r%direction%x
        at%y = r%origin%y + t * r%direction%y
        at%z = r%origin%z + t * r%direction%z
    end function at
end module class_ray
