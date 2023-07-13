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
            type(hit_record) :: hit_rec
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
        type(ray) :: r
        real :: t_min, t_max
        type(hit_record) :: hit_rec
    end subroutine
end module objects
