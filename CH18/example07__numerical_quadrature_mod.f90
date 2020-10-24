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