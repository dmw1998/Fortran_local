module library_constants
implicit none

! Define precision
integer, parameter :: lib_prec = &
                      selected_real_kind(p=6)

end module library_constants

subroutine quad_roots(a,b,c,root1,root2,error)
    use library_constants
    implicit none

    ! Dummy variables
    real(kind=lib_prec), intent(in) :: a,b,c 
    real(kind=lib_prec), intent(out) :: root1,root2
    logical :: error

    ! Local variables
    real(kind=lib_prec) :: f,r,s,t,d 

    ! Check for a = 0
    error = a == 0.0_lib_prec
    if (error) return

    ! Calculate scaled coefficients
    f = max(abs(a),abs(b),abs(c))
    r = a/f
    s = b/f 
    t = c/f 

    ! Solve modified equation for first root
    d = sqrt(s*s - 4.0_lib_prec*r*t)
    if (s > 0.0_lib_prec) then
        root1 = (-s-d)/(r+r)
    else
        root2 = (-s+d)/(r+r)
    end if

    ! Calculate other root
    root2 = (t/r)/root1

end subroutine quad_roots