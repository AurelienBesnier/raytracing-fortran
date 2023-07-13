module class_vec3
    implicit none
    private
    real, parameter :: PI = 3.1415926535897932385
    public :: vec3, dot, cross, length, length_squared, vec3_print, unit_vector, normalize, &
    minus_vec, div_vec, add_vec, mul_vec, mul_vec_d


    type vec3
        real :: x, y, z
    contains
    end type vec3
contains

    PURE real function dot(u, v)
        implicit none
        type(vec3), intent(in) :: u, v

        dot = u%x*v%x + u%y*v%y + u%z*v%z
    end function dot

    function cross(u, v)
        implicit none
        type(vec3), intent(in) :: u, v
        type(vec3) :: cross

        cross%x = u%y * v%z - v%y * u%z
        cross%y = -(u%x * v%z - v%x * u%z)
        cross%z = u%x * v%y - v%x * u%y
    end function cross

    PURE real function length_squared(u)
        implicit none
        type(vec3), intent(in) :: u

        length_squared = u%x**2 + u%y**2 + u%z**2
    end function length_squared


    PURE real function length(u)
        implicit none
        type(vec3), intent(in) :: u

        length = sqrt(length_squared(u))
    end function length


    function normalize(u)
        implicit none
        type(vec3), intent(in) :: u
        type(vec3) :: normalize

        normalize%x = u%x / sqrt(u%x**2)
        normalize%y = u%y / sqrt(u%y**2)
        normalize%z = u%z / sqrt(u%z**2)
    end function normalize

    PURE function unit_vector(u)
        implicit none
        type(vec3), intent(in) :: u
        type(vec3) :: unit_vector

        unit_vector%x = u%x / length(u)
        unit_vector%y = u%y / length(u)
        unit_vector%z = u%z / length(u)
    end function unit_vector

    PURE type(vec3) function minus_vec(u,v)
        implicit none
        type(vec3), intent(in) :: u, v
        minus_vec%x = u%x - v%x
        minus_vec%y = u%y - v%y
        minus_vec%z = u%z - v%z
    end function minus_vec

    PURE type(vec3) function mul_vec(u,v)
        implicit none
        type(vec3), intent(in) :: u, v
        mul_vec%x = u%x - v%x
        mul_vec%y = u%y - v%y
        mul_vec%z = u%z - v%z
    end function mul_vec

    PURE type(vec3) function mul_vec_d(t,v)
        implicit none
        real, intent(in) :: t
        type(vec3), intent(in) :: v
        mul_vec_d%x = t * v%x
        mul_vec_d%y = t * v%y
        mul_vec_d%z = t * v%z
    end function mul_vec_d

    type(vec3) function div_vec(u,f)
        implicit none
        type(vec3), intent(in) :: u
        real :: f
        div_vec%x = u%x / f
        div_vec%y = u%y / f
        div_vec%z = u%z / f

    end function div_vec

    PURE type(vec3) function add_vec(u,v)
        implicit none
        type(vec3), intent(in) :: u, v
        add_vec%x = u%x + v%x
        add_vec%y = u%y + v%y
        add_vec%z = u%z + v%z
    end function add_vec

    subroutine vec3_print(this)
        type(vec3), intent(in) :: this
        print *, 'Vec3: x = ', this%x, ' y = ', this%y, ' z = ', this%z
    end subroutine vec3_print

end module class_vec3
