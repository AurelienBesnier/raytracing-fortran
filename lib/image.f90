module class_image
    use class_vec3
    implicit none
    private
    public :: img, create_image, setPixel, delete_image, writePPM

    type img
        integer :: width, height
        real, allocatable :: pixels(:)
    end type img

contains
    type(img) function create_image(w,h)
        implicit none
        integer, intent(in) :: w, h
        create_image%width = w
        create_image%height = h
        allocate(create_image%pixels(w*3*h*3))
    end function create_image

    PURE integer function getIndex(j,i, imge)
        implicit none
        integer, intent(in) :: j, i
        type(img), intent(in) :: imge
        getIndex = 3 * j * imge%width + i * 3
    end function getIndex

    PURE real function clamp(x, min, max)
        implicit none
        real, intent(in) :: x, min, max
        if (x < min) then
            clamp =  min
        elseif (x > max) then
            clamp = max
        else
            clamp = x
        endif
    end function


    subroutine setPixel(j, i, imge, rgb, samples)
        implicit none
        integer, intent(in) :: j, i
        type(vec3), intent(in) :: rgb
        type(img), intent(inout) :: imge
        integer :: idx, samples
        real :: scaler
        idx = getIndex(j,i,imge)
        scaler = 1.0 / samples
        imge%pixels(idx) = rgb%x * scaler
        imge%pixels(idx+1) = rgb%y * scaler
        imge%pixels(idx+2) = rgb%z * scaler
    end subroutine setPixel

    subroutine writePPM(this)
        type(img), intent(inout) :: this
        integer :: idx, ir, ig, ib, i, j
        open(unit=10, file='image.ppm', action='write')
        write(10,'(A)') 'P3'
        write(10,'(I0, A, I0)') this%width, ' ', this%height
        write(10,'(A)') '255'

        do j = this%height-1, 0, -1
            do i = 0, this%width-1
                idx = getIndex(j,i, this)
                ir = floor(256 *  clamp(this%pixels(idx),0.0,0.9999))
                ig = floor(256 *  clamp(this%pixels(idx+1),0.0,0.9999))
                ib = floor(256 *  clamp(this%pixels(idx+2),0.0,0.9999))
                write(10, '(I3, A, I3, A, I3)') ir, ' ', ig, ' ', ib
            end do
        end do

        close(10)
    end subroutine writePPM

    subroutine delete_image(this)
        implicit none
        type(img), intent(inout) :: this
        if(allocated(this%pixels)) deallocate(this%pixels)
    end subroutine delete_image

end module class_image
