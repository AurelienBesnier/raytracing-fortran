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

    integer function getIndex(j,i, imge)
        implicit none
        integer, intent(in) :: j, i
        type(img), intent(in) :: imge
        getIndex = 3 * j * imge%width + i * 3
    end function getIndex

    subroutine setPixel(j, i, imge, rgb)
        implicit none
        integer, intent(in) :: j, i
        type(vec3), intent(in) :: rgb
        type(img), intent(inout) :: imge
        integer :: idx
        idx = getIndex(j,i,imge)
        imge%pixels(idx) = rgb%x
        imge%pixels(idx+1) = rgb%y
        imge%pixels(idx+2) = rgb%z
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
                ir = floor(255.999 *  this%pixels(idx))
                ig = floor(255.999 *  this%pixels(idx+1))
                ib = floor(255.999 *  this%pixels(idx+2))
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
