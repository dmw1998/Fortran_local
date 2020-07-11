subroutine arr_sin(angle,low,high)
    implicit none

    integer,intent(in) :: low,high
    real,dimension(low:high),intent(in) :: angle

    real,dimension(low:high) :: sines
    integer :: i 

    sines = sin(angle)

    do i=low,high
        print *,"sin(",angle(i),") =",sines(i)
    end do

end subroutine arr_sin

program PE0704
implicit none

real,parameter :: pi = 3.14159265
real,dimension(1:7) :: angles = (/ 0.0, pi/7.0, 2.0*pi/7.0, 3.0*pi/7.0, 4.0*pi/7.0, 5.0*pi/7.0, 6.0*pi/7.0 /)

call arr_sin(angles,1,7)

end program PE0704