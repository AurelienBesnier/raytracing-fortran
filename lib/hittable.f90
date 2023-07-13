module objects
    use class_vec3
    use class_ray
    implicit none
    public :: hit_record, hittable, sphere
    type hit_record
        type(vec3) :: p, normal
        real :: t
    end type hit_record

    type, abstract :: hittable
        contains
        procedure(ihit), deferred :: hit
    end type hittable

    abstract interface
        subroutine ihit(self, r,t_min, t_max, hit_rec)
            import :: hittable
            import :: hit_record
            import :: ray
            class(hittable), intent(inout) :: self
            type(ray) :: r
            real :: t_min, t_max
            type(hit_record), intent(inout) :: hit_rec
        end subroutine ihit
    end interface

    type, extends(hittable) :: sphere
        type(vec3) :: center
        real :: radius
    contains
        procedure :: hit
    end type sphere
contains

    subroutine hit(self,r,t_min, t_max, hit_rec)
        class(sphere), intent(inout) :: self
        type(hit_record), intent(inout) :: hit_rec
        type(ray) :: r
        type(vec3) :: oc
        real :: a, half_b, c, discriminant, t_min, t_max
        oc = minus_vec(r%origin, self%center)
        a = length_squared(r%direction)
        half_b = dot(oc,r%direction)
        c = length_squared(oc) - self%radius*self%radius
        discriminant = half_b*half_b - a * c
        if (discriminant < 0.0) then
            t_min = -1.0
        else
            t_min = (-half_b - sqrt(discriminant)) / a
        end if

    end subroutine
end module objects
