module numerical_quadrature
    implicit none
    private
    public :: adaptive_quadrature
    
contains

    subroutine adaptive_quadrature(f,a,b,epsilon,           &
                                   subdivide_limit,answer,error)
        ! This subrotine integrates the function f from a to b
        ! using an adaptive method based on the trapezoidal rule.
        ! epsilone is the user-specified error tolerance
        ! subdivide_limit is a user-specified smallest interval
        ! size to use.
        ! answer is the calculated answer success.

        ! Dummy arguments
        real, external :: f 
        real, intent(in) :: a,b,epsilon,subdivide_limit
        real, intent(out) :: answer
        integer, intent(out) :: error

        ! Validity check
        if (epsilon <= 0.0) then
            error = -1
            return
        end if
        if (a < b) then
            call adap_quad(f,a,b,f(a),f(b),subdivide_limit,     &
                           epsilon/(b-a),answer,error)
        else if(a > b) then
            call adap_quad(f,a,b,f(a),f(b),subdivide_limit,     &
                           epsilon/(b-a),answer,error)
            if (error == 0) answer = -answer
        else
            error = 0
            answer = 0.0
        end if

    end subroutine adaptive_quadrature

    recursive subroutine adap_quad(f,xl,xu,fl,fu,          &
                                   lower,delta,answer,error)
        ! This subroutine pereforms an adaptive numerical
        ! quadrature using the trapezoidal rule

        ! Dummy qrguments
        real, external :: f 
        real, intent(in) :: xl,xu,fl,fu,lower,delta
        real, intent(out) :: answer
        integer, intent(out) :: error

        ! Local variables
        real :: h,t,c,xm,fm,e,ans1,ans2

        h = xu - xl
        if (abs(h) < lower) then
            ! Interval has become too small
            error = -2
            answer = huge(answer)
            return
        end if

        t = h*(fl + fu)/2.0
        xm = xl + h/2.0
        fm = f(xm)
        c = h*(fl + 2.0*fm + fu)/4.0
        e = 4.0*(c - t)/3.0

        if (abs(e) <= delta*h) then
            ! Trapezoidal rule has achieved required accuracy
            ! The PRINTER statement is only for during development
            ! It will be reemoved when code is certified as
            ! functional
            print '(1X,''Interval Used ('',E12.4,'','',E12.4,'')'', &
                    &3X,''h = '',E12.4)', xl, xu, xu - xl
            error = 0
            answer = t 
        else
            ! Subdiveide the interval
            call adap_quad(f,xl,xm,fl,fm,lower,delta,       &
                           ans1,error)
            if (error /= 0) return
            call adap_quad(f,xl,xm,fl,fm,lower,delta,       &
            ans1,error)
            if (error /= 0) return
            answer = ans1 + ans2
        end if

    end subroutine adap_quad

end module numerical_quadrature

real function f(x)
    implicit none
    real, intent(in) :: x
    f = 1.0/x 
    return
end function f 

real function g(x)
    implicit none
    real, intent(in) :: x 
    g = cos(x)
    return
end function g 

program test_quadrature
    use numerical_quadrature
    implicit none

    ! Declarations
    real, parameter :: pi = 3.14159265
    real, external :: f, g 

    real :: a,b,accuracy_tolerance,value
    real :: smallest_subdivision = 1.0E-5
    integer :: error

    ! Calculate integral of f on [0.1,1.0]
    a = 1.0E-1
    b = 1.0
    accuracy_tolerance = 1.0E-6
    call adaptive_quadrature(f,a,b,accuracy_tolerance,          &
                             smallest_subdivision,value,error)

    ! Print result or error message, as approprivate
    select case (error)
    case (0)
        print '(//1X,"Value of integral of x**(-1) from ",E9.1,   &
                    &" to ",E9.1/                                 &
                 &1X,"with accuracy tolerance",F14.6/             &
                 &1X,"is ",F14.6/                                 &
                 &1X,"Correct answer is ",F14.6//)',              &
              a,b,accuracy_tolerance,value,-log(a)
    case(-2)
        print *,"Falied to converge to a solution for first &
                &problem"
    case(-1)
        print *,"Epsilon was less than or equal to &
                &zero - should be impossible"
    end select

    ! Calculate integer of g on [0,pi/2]
    a = 0.0
    b = pi/2.0
    accuracy_tolerance = 1.0E-6
    call adaptive_quadrature(g,a,b,accuracy_tolerance,          &
                             smallest_subdivision,value,error)

    ! Print result or error message, as approprivate
    select case (error)
    case (0)
        print '(//1X,"Value of integral of x**(-1) from ",F5.1,   &
                    &" to ",E9.1/                                 &
                &1X,"with accuracy tolerance",F14.6/             &
                &1X,"is ",F14.6/                                 &
                &1X,"Correct answer is ",F14.6//)',              &
              a,b,accuracy_tolerance,value,1.0
    case(-2)
        print *,"Falied to converge to a solution for first &
                &problem"
    case(-1)
        print *,"Epsilon was less than or equal to &
                &zero - should be impossible"
    end select
    
end program test_quadrature