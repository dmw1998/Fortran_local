module methods
    implicit none
    
contains

    recursive subroutine find_bis_root(f,left,right,tol,iter,filename)

        real, external :: f 
        real :: left, right, tol
        integer :: iter
        character(len=*) :: filename

        real :: mid

        ! Check wether the initial points is available
        if (f(left)*f(right) > 0) then
            print *,"Please set two initial points whose function values have opposite signs."
            return
        else if (left > right) then
            print *,"Please set two points in the right order next time."
            print *,"We interchanged them for you this time."
            mid = left
            left = right
            right = mid
        end if

        ! Check wether the answer is initial point
        if (abs(f(left)) <= tol) then
            call write_result(filename,left,0)
            return
        else if (abs(f(right)) <= tol) then
            call write_result(filename,right,0)
            return
        end if

        ! Calculate the midpoint
        mid = left + (right - left)/2.0

        if (abs(f(mid)) <= tol) then
            ! Midpoint is the answer
            iter = iter + 1
            call write_result(filename,mid,iter)
            return
        else if (f(left)*f(mid) > 0) then
            ! Midpoint point has the same sign as left point
            left = mid
            iter = iter + 1
            call find_root(f,left,right,tol,iter,filename)
        else
            ! Midpoint point has the same sign as right point
            right = mid
            iter = iter + 1
            call find_root(f,left,right,tol,iter,filename)
        end if

    end subroutine find_bis_root

    subroutine find_Newton_root(f,df,start,tol,iter,filename)

        real, external :: f,df
        real :: start,tol
        integer :: iter
        character(len=*) :: filename

        do iter = 0,9999

            if (abs(f(start)) < tol) then
                call write_result(filename,start,iter)
                return
            end if

            start = start - f(start)/df(start)

        end do

        print *,"Cannot find root in 10000 iterations."

    end subroutine find_Newton_root

    subroutine find_secant_root(f,x1,x2,tol,iter,filename)

        real, external :: f 
        real :: x1, x2, tol
        integer :: iter
        character(len=*) :: filename

        real :: x3 
        
        x3 = x2

        do iter = 0,9999

            if (abs(f(x3)) < tol) then
                call write_result(filename,x3,iter)
                return
            end if

            x3 = x1 - f(x1)*(x2-x1)/(f(x2) - f(x1))

            x1 = x2
            x2 = x3

        end do

        print *,"Cannot find root in 10000 iterations."

    end subroutine find_secant_root

    subroutine write_result(filename,result,iter)

        real :: result
        integer :: iter
        character(len=*) :: filename

        logical :: alive

        inquire(file=filename,exist=alive)

        if (alive) then
            open(17,file=filename,status="old",access='append')
            write(17,'("Root: ",F9.4," Iteration: ",I3)') result, iter
            close(17)
        else
            open(17,file=filename,status="new")
            write(17,'("Root: ",F9.4," Iteration: ",I3)') result, iter
            close(17)
        end if
    end subroutine write_result

end module methods

program PE1802
    use methods
    implicit none

    real :: tol=1.0E-6
    integer :: iter

    iter = 0
    call find_bis_root(fa,0.5,1.0,tol,iter,"1802_bisection_result.txt")
    iter = 0
    call find_bis_root(fb,0.0,1.2,tol,iter,"1802_bisection_result.txt")
    iter = 0
    call find_bis_root(fc,1.0,2.0,tol,iter,"1802_bisection_result.txt")
    iter = 0
    call find_bis_root(fd,0.0,2.0,tol,iter,"1802_bisection_result.txt")
    iter = 0
    call find_bis_root(fe,-1.0,-0.1,tol,iter,"1802_bisection_result.txt")
    iter = 0
    call find_bis_root(ff,1.0,4.0,tol,iter,"1802_bisection_result.txt")

    iter = 0
    call find_Newton_root(fa,dfa,-1.0,tol,iter,"1802_Newton_result.txt")
    iter = 0
    call find_Newton_root(fb,dfb,-1.0,tol,iter,"1802_Newton_result.txt")
    iter = 0
    call find_Newton_root(fc,dfc,-1.0,tol,iter,"1802_Newton_result.txt")
    iter = 0
    call find_Newton_root(fd,dfd,-1.0,tol,iter,"1802_Newton_result.txt")
    iter = 0
    call find_Newton_root(fe,dfe,-1.0,tol,iter,"1802_Newton_result.txt")
    iter = 0
    call find_Newton_root(ff,dff,1.0,tol,iter,"1802_Newton_result.txt")

    iter = 0
    call find_secant_root(fa,0.5,1.0,tol,iter,"1802_secant_result.txt")
    iter = 0
    call find_secant_root(fb,0.0,1.2,tol,iter,"1802_secant_result.txt")
    iter = 0
    call find_secant_root(fc,1.0,2.0,tol,iter,"1802_secant_result.txt")
    iter = 0
    call find_secant_root(fd,0.0,2.0,tol,iter,"1802_secant_result.txt")
    iter = 0
    call find_secant_root(fe,-1.0,-0.1,tol,iter,"1802_secant_result.txt")
    iter = 0
    call find_secant_root(ff,1.0,4.0,tol,iter,"1802_secant_result.txt")
    
contains

    real function fa(x)
        real, intent(in) :: x 
        real, parameter :: pi = 3.14159265
        fa = sin(3.0*x + pi/4.0)
    end function fa

    real function dfa(x)
        real, intent(in) :: x
        real, parameter :: pi = 3.14159265
        dfa = 3.0*cos(3.0*x + pi/4.0)
    end function dfa

    real function fb(x)
        real, intent(in) :: x 
        fb = sin(3.0*x)*cos(x)
    end function fb

    real function dfb(x)
        real, intent(in) :: x 
        dfb = 3.0*cos(x)*cos(3.0*x) - sin(x)*sin(3.0*x)
    end function dfb

    real function fc(x)
        real, intent(in) :: x 
        fc = sin(5.0*x) + 5.0*cos(x)
    end function fc

    real function dfc(x)
        real, intent(in) :: x 
        dfc = 5.0*cos(5.0*x) - 5*sin(x)
    end function dfc

    real function fd(x)
        real, intent(in) :: x 
        fd = 2 - exp(sin(x))
    end function fd

    real function dfd(x)
        real, intent(in) :: x 
        dfd = -exp(sin(x))*cos(x)
    end function dfd

    real function fe(x)
        real, intent(in) :: x 
        real, parameter :: pi = 3.14159265
        fe = tan(x + pi/6.0)
    end function fe

    real function dfe(x)
        real, intent(in) :: x 
        real, parameter :: pi = 3.14159265
        dfe = 1.0/(cos(x + pi/6.0) * cos(x + pi/6.0))
    end function dfe

    real function ff(x)
        real, intent(in) :: x 
        ff = sin(exp(x/3.0))
    end function ff

    real function dff(x)
        real, intent(in) :: x 
        dff = 1.0/3.0 * exp(x/3.0) * cos(exp(x/3.0))
    end function dff

end program PE1802