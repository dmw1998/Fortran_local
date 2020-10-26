module Newton_method
    implicit none
    
contains

    subroutine find_root(f,df,start,tol,iter,filename)
        implicit none

        real, external :: f, df 
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
    
    end subroutine find_root

    subroutine write_result(filename,result,iter)
        implicit none

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

end module Newton_method

module functions
    implicit none
    
contains

    real function fa(x)
        real,intent(in) :: x
        fa = 10.0*x**3.0 - x**2.0 - 69.0*x + 72.0
    end function fa

    real function dfa(x)
        real,intent(in) :: x 
        dfa = 30.0*x**2.0 - 2.0*x - 69.0
    end function dfa

    real function fb(x)
        real,intent(in) :: x
        fb = 20.0*x**3.0 - 52.0*x**2.0 + 17.0*x + 24.0
    end function fb

    real function dfb(x)
        real,intent(in) :: x
        dfb = 60.0*x**2.0 - 104.0*x + 17.0
    end function dfb

    real function fc(x)
        real,intent(in) :: x
        fc = 5.0*x**3.0 - x**2.0 - 80.0*x + 16.0
    end function fc

    real function dfc(x)
        real,intent(in) :: x
        dfc = 15.0*x**2.0 - 2.0*x - 80.0
    end function dfc

    real function fd(x)
        real,intent(in) :: x
        fd = 10.0*x**4.0 + 13.0*x**3.0 - 163.0*x**2.0 - 208.0*x + 48.0
    end function fd

    real function dfd(x)
        real,intent(in) :: x
        dfd = 40.0*x**3.0 + 39.0*x**2.0 - 326.0*x - 208.0
    end function dfd

    real function fe(x)
        real,intent(in) :: x
        fe = x**4.0 + 2.0*x**3.0 - 23.0*x**2.0 - 24.0*x + 144.0
    end function fe

    real function dfe(x)
        real,intent(in) :: x
        dfe = 4.0*x**3.0 + 6.0*x**2.0 - 46.0*x - 24.0
    end function dfe

    real function ff(x)
        real,intent(in) :: x
        ff = 9.0*x**4.0 - 42.0*x**3.0 - 1040.0*x**2.0 + 5082.0*x - 5929
    end function ff

    real function dff(x)
        real,intent(in) :: x
        dff = 36.0*x**3.0 - 126.0*x**2.0 - 2080.0*x + 5082.0
    end function dff
        
end module functions

program PE1801_Newton
    use functions
    use bisection_method
    implicit none

    real :: start, tol=1.0E-6
    integer :: iter

! (a)
    start = 0.0
    iter = 0
    call find_root(fa,dfa,start,tol,iter,"1801a_Newton_result.txt")

    start = 1.55
    iter = 0
    call find_root(fa,dfa,start,tol,iter,"1801a_Newton_result.txt")

    start = 1.7
    iter = 0
    call find_root(fa,dfa,start,tol,iter,"1801a_Newton_result.txt")

! (b)
    start = 0
    iter = 0
    call find_root(fb,dfb,start,tol,iter,"1801b_Newton_result.txt")

    start = 1.55
    iter = 0
    call find_root(fb,dfb,start,tol,iter,"1801b_Newton_result.txt")

    start = 2
    iter = 0
    call find_root(fb,dfb,start,tol,iter,"1801b_Newton_result.txt")

! (c)
    start = -2
    iter = 0
    call find_root(fc,dfc,start,tol,iter,"1801c_Newton_result.txt")

    start = 1.55
    iter = 0
    call find_root(fc,dfc,start,tol,iter,"1801c_Newton_result.txt")

    start = 5.5
    iter = 0
    call find_root(fc,dfc,start,tol,iter,"1801c_Newton_result.txt")

! (d)
    start = -3.0
    iter = 0
    call find_root(fd,dfd,start,tol,iter,"1801d_Newton_result.txt")

    start = -1.0
    iter = 0
    call find_root(fd,dfd,start,tol,iter,"1801d_Newton_result.txt")

    start = 6.0
    iter = 0
    call find_root(fd,dfd,start,tol,iter,"1801d_Newton_result.txt")

    start = 0.3
    iter = 0
    call find_root(fd,dfd,start,tol,iter,"1801d_Newton_result.txt")

end program PE1801_Newton