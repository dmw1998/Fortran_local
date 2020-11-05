module stack_utility
    implicit none
    private

    integer, parameter :: top = 50
    integer, save :: current = 0
    integer, save :: stack(top)

    public :: push, pop 
    
contains

! Put data into stack
subroutine push(value)

    integer :: value

    if (current > top) then
        print *, "Stack full."
        return
    end if

    current = current+1
    stack(current) = value

end subroutine push

! Take data from stack
integer function pop(value)

    integer :: value

    if (current <= 0) then
        pop = 1
        return
    end if

    value = stack(current)
    current = current-1
    pop = 0

end function pop
    
end module stack_utility

program main
    use stack_utility
    implicit none

    integer, parameter :: n = 5
    integer :: array(n) = (/ 1, 2, 3, 4, 5 /)
    integer :: i, stat, value

    do i = 1,n 
        call push(array(i))
    end do

    do i = 1,n 
        stat = pop(value)
        print *, value
    end do
    
end program main