module Gauss_Legendre
    implicit none

    integer, parameter :: n = 5                 ! number of Guass points
    integer, parameter :: p = selected_real_kind(p=13)
    real(kind=p), parameter :: epsilon = 1.0E-6
    
contains

real(kind=p) function f(x)

    real(kind=p) :: x 

    integer :: i 
    real(kind=p) :: a(n)

    a(1) = x 
    a(2) = 1.5_p*(x**2) - 0.5_p
    do i = 3,n 
        a(i) = (2*i-1)*x*a(i-1)/i - (i-1)*a(i-2)/i 
    end do

    f = a(n)

end function f 

real(kind=p) function f1(x)

    real(kind=p) :: x 

    integer :: i 
    real(kind=p) :: a(n)

    a(1) = x 
    a(2) = 1.5_p*(x**2) - 0.5_p
    do i = 3,n-1
        a(i) = (2*i-1)*x*a(i-1)/i - (i-1)*a(i-2)/i 
    end do

    f1 = a(n-1)

end function f1 

real(kind=p) function g(x)

    real(kind=p) :: x 

    integer :: i 
    real(kind=p) :: a(n)

    a(1) = x 
    a(2) = 1.5_p*(x**2) - 0.5_p
    do i = 3,n-1
        a(i) = (2*i-1)*x*a(i-1)/i - (i-1)*a(i-2)/i 
    end do

    g = n*a(n-1)*x*a(i-1)/i - n*x*a(n)/(1-x**2)

end function g 

real(kind=p) function bis(a,b)

    real(kind=p) :: a,b 

    real(kind=p) :: mid

    do
        mid = (a+b)/2.0_p

        if (f(mid)*f(a) > 0) then
            a = mid
        else
            b = mid
        end if

        if ((b-a) < epsilon) exit
        
    end do

    bis = mid

end function bis 

subroutine fn_0(fn,ak)

    real(kind=p), dimension(n) :: fn, ak

    real(kind=p) :: m 
    integer :: i, j

    j = 0
    m = -1.000001

    ! Step size = 0.000001
    do i = 1,200000
        if (f(m)*f(m+0.00001) > 0) then
            j = j+1
            fn(j) = bis(m,m+0.0001)
            ak(j) = 2.0_p/(n*f1(fn(j))*g(fn(j)))
            print *, "number:", j
            print *, "Gauss point:", fn(j)
            print *, "Weight:", ak(n)
        end if
        m = m + 0.00001
    end do

end subroutine fn_0
    
end module Gauss_Legendre

program mian
    use Gauss_Legendre
    implicit none

    real(kind=p) :: fn(n), ak(n), a, b, answer
    integer :: i 

    call fn_0(fn,ak)

    a = 0.0_p
    b = 1.0_p
    answer = 0.0_p

    do i = 1,n 
        answer = answer + ak(i)*y((a+b)/2.0_p+(b-a)/2.0_p*fn(i))
    end do

    answer = answer*(b-a)/2.0_p

    print *, "The result is ", answer

contains

real(kind=p) function y(x)

    real(kind=p) :: x 

    y = x**2/3.0_p

end function y 
    
end program mian