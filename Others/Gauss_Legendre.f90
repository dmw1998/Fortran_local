module Gauss_Legendre_new
    implicit none

    integer, parameter :: n = 5                 ! number of Guass points
    integer, parameter :: p = selected_real_kind(p=16)
    real(kind=p), parameter :: epsilon = 1.0E-6

contains

real(kind=p) function Legendre_poly(n,x)

    integer :: n 
    real(kind=p) :: x 

    integer :: i 
    real(kind=p) :: a(n)

    a(1) = x 
    a(2) = 1.5_p*x*x - 0.5_p
    do i = 3,n 
        a(i) = ((i+i-1)*x*a(i-1) - (i-1)*a(i-2))/i 
    end do

    Legendre_poly = a(n)

end function Legendre_poly

real(kind=p) function Legendre_poly_prime(n,x)

    integer :: n 
    real(kind=p) :: x 

    integer :: i 
    real(kind=p) :: a(n)

    a(1) = x 
    a(2) = 1.5_p*x*x - 0.5_p 
    do i = 3,n 
        a(i) = ((i+i-1)*x*a(i-1) - (i-1)*a(i-2))/i 
    end do

    Legendre_poly_prime = (a(n-1) - x*a(n))*n / (1.0_p-x*x)

end function Legendre_poly_prime

real(kind=p) function bis(a,b)

  real(kind=p) :: a,b 

  real(kind=p) :: mid

  do
      mid = (a+b)/2.0_p

      if (Legendre_poly(n,mid)*Legendre_poly(n,a) > 0) then
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
        if (Legendre_poly(n,m)*Legendre_poly(n,m+0.00001) > 0) then
            j = j+1
            fn(j) = bis(m,m+0.0001)
            ak(j) = 2.0_p/(n*Legendre_poly(n-1,fn(j))*Legendre_poly_prime(n,fn(j)))
            print *, "number:", j
            print *, "Gauss point:", fn(j)
            print *, "Weight:", ak(n)
        end if
        m = m + 0.00001
    end do

end subroutine fn_0

end module Gauss_Legendre_new

program mian
  use Gauss_Legendre_new
  implicit none

  real(kind=p) :: fn(n), ak(n), a, b, answer
  integer :: i 

  call fn_0(fn,ak)

  a = 0.0_p
  b = 1.0_p
  answer = 0.0_p

  do i = 1,n
      answer = answer + ak(i)*f((a+b)/2.0_p+(b-a)/2.0_p*fn(i))
  end do

  answer = answer*(b-a)/2.0_p

  print *, "The result is ", answer

contains

real(kind=p) function f(x)

  real(kind=p) :: x 

  f = x**2/3.0_p

end function f 
  
end program mian