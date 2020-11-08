module interpolation_utility
    use sgl
    implicit none

    type point
        real :: x, y
    end type point

    real, parameter :: pi = 3.14159265
    real, parameter :: xmin = 0.0, xmax = pi*3.0
    integer, parameter :: n = 10, np = 100
    type(point) :: datas(n), interpolate(np)
    
contains

subroutine GenerateData(func)

    real, external :: func 
    
    real :: r, width
    integer :: i 
    
    width = (xmax - xmin) / (n-1)
    r = 0
    do i = 1,n 
        datas(i)%x = r 
        datas(i)%y = func(r)
        r = r + width
    end do

end subroutine GenerateData

real function lagrange(x)

    real :: x 

    real :: coeff
    integer :: i, j
    
    lagrange = 0
    do i = 1,n 
        coeff = 1
        do j = 1,n 
            if (i /= j) then
                coeff = coeff*(x - datas(j)%x)/(datas(i)%x - datas(j)%x)
            end if
        end do
        lagrange = lagrange + coeff*datas(i)%y
    end do

end function lagrange

subroutine display()
    ! Graph the function

    real, parameter :: size = 0.1
    integer :: i 
    
    call sglClearBuffer()
    call sglColor3i(255,255,255)

    do i = 1,np-1
        call sglLineV(interpolate(i)%x, interpolate(i)%y, &
                    interpolate(i+1)%x, interpolate(i+1)%y)
    end do

    call sglColor3i(255,0,0)

    do i = 1,n 
        call sglLineV(datas(i)%x-size, datas(i)%y-size, &
                      datas(i)%x+size, datas(i)%y+size)
        call sglLineV(datas(i)%x+size, datas(i)%y-size, &
                      datas(i)%x-size, datas(i)%y+size)
    end do

    call sglUpdateBuffer()

end subroutine display
    
end module interpolation_utility

program main
    use interpolation_utility
    implicit none

    real :: xinc, x 
    integer :: i 

    call GenerateData(f)

    x = 0
    xinc = (xmax-xmin) / (np-1)

    do i = 1,np
        interpolate(i)%x = x 
        interpolate(i)%y = lagrange(x)
        x = x + xinc
    end do

    call sglDisplaySub(display)
    call sglSetVirtual(xmin, 2.0, xmax, -2.0)
    call sglCreateWindow(100,100,400,400,1)
    call sglMainLoop()

contains

real function f(x)

    real :: x 

    f = x**2 + sin(x)

end function f 
    
end program main