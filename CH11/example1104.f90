subroutine bisect(f,xl_start,xr_start,tol,max_iter, &
                  zero,delta,n_bsecs,error)
    implicit none

    ! Dummy arguments
    real,intent(in) :: xl_start,xr_start,tol
    integer,intent(in) :: max_iter
    real,intent(out) :: zero,delta
    integer,intent(out) :: n_bisecs,error

    ! Local variables
    real :: xl,xr
    integer :: iter_count

    ! External function
    real,external :: f 

    ! Initialize the zero-bounding interval
    if (xl_start < xr_start) then
        xl = xl_start
        xr = xr_start
    else
        xl = xr_start
        xr = xl_start
    end if 

    ! Check if a solution is possible
    if (f(xl)*f(xr)>=0.0 .or. max_iter<1) then
        ! No solution possible
        error = -1
    else
        ! Solution is possible, call divide_interval to find it
        iter_count = max_iter
        call divide_interval(f,xl,rx,tol,iter_count,zero,delta, &
                             error)
        n_bisecs = max_iter - iter_count
    endif

end subroutine bisect

recursive subroutine divide_interval(f,xl,rx,tol,iter_count, &
                                     zero,delta,error)
    implicit none

    ! Dummy arguments
    real,intent(in) :: tol
    
    real,intent(inout) :: xl,xr
    integer,intent(inout) :: iter_count

    real,intent(out) :: zero,delta
    integer,intent(out) :: error

    ! Local variables
    real :: xm

    ! External function
    real,external :: f 

    delta = 0.5*(xr-xl)
    ! Check to see if within the specified tolerance of the root
    if (delta < tol) then
        ! Yes - return result
        error = 0
        zero = xl + delta
    else
        ! No root yet - check if maximum iterations reached
        iter_count = iter_count -1
        if (iter_count<0) then
            ! Maximum iterations with no solution - return error
            error = -2
            zero = xl + delta
        else 
            ! More iterations permitted
            xm = xl + delta
            if (f(xl)*f(xm) < 0.0) then
                call divide_interval(f,xl,xm,tol,iter_count,zero, &
                                     delta,error)
            else
                call divide_interval(f,xm,xr,tol,iter_count,zero, &
                                     delta,error)
            end if
        end if
    end if
end subroutine divide_interval