program factorial
    implicit none

    integer(2) :: nlb, nub 
    real :: t1, t2

    call cpu_time(t1)
    nlb = 1
    nub = 32767
    call find_factorial(nlb,nub)
    call cpu_time(t2)

    print *, "time: ", t2-t1

contains

subroutine find_factorial(nlb,nub)
    use, intrinsic :: iso_fortran_env, only : output_unit
    implicit none

    integer(2), intent(in) :: nlb, nub 

    integer(8), parameter :: pmax = 10**9, len = 10300
    integer(8), dimension(len) :: ires
    integer(8) :: nl, nu, i, j, m, n 
    character :: char*24

    ires = 0
    ires(1) = 1
    nl = 1
    nu = 1
    do i = nlb, nub
        ires(nl:nu) = ires(nl:nu)*i 
        n = 0
        do j = nl,nu 
            if (ires(j) /= 0) then
                m = ires(j) / pmax
                ires(j+1) = ires(j+1) + m 
                ires(j) = ires(j) - pmax*m 
                n = 1
            else if (n == 0) then
                nl = j
            end if
        end do
        if (ires(nu+1) > 0) nu = nu+1
    end do

    open(output_unit,file="result.txt")

    write(char,*) ires(nu)
    char = adjustl(char)
    m = len_trim(char)
    n = (nu-1)*13+m-1

    write(output_unit,'(A)',advance="no") char(1:1)//"."//char(2:m)//"e+"
    write(char,*) n 
    char = adjustl(char)
    write(output_unit,'(A)') trim(char)

    if (nu > 1) then
        write(output_unit,'(I10)',advance="no") ires(nu)
        write(output_unit,'(10747I13.13)') ires(nu-1:1:-1)
    else
        write(output_unit,'(I10)') ires(nu)
    end if

    close(output_unit)

end subroutine find_factorial
    
end program factorial