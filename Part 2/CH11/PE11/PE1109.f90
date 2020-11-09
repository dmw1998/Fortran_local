module QuickSort
    implicit none

    public :: sort 
    private :: partition

contains

    recursive subroutine sort(A)
        real,dimension(:),intent(inout) :: A
        integer :: index 

        if (size(A) > 1) then
            call partition(A,index)
            call sort(A(:index-1))
            call sort(A(index:))
        end if

    end subroutine sort

    subroutine partition(A,marker)
        real,dimension(:),intent(inout) :: A
        integer,intent(out) :: marker

        integer :: i,j
        real :: temp, x

        x = A(1)
        i = 0
        j = size(A) +1

        do
            j = j-1
            do
                if (A(j) <= x) exit
                j = j-1
            end do
            i = i+1
            do
                if (A(i) >= x) exit
                i = i+1
            end do

            if (i < j) then
                temp = A(i)
                A(i) = A(j)
                A(j) = temp
            else if (i == j) then
                marker = i+1
                return
            else
                marker = i
                return
            end if 
        end do

    end subroutine partition

end module QuickSort

program PE1109
use QuickSort
implicit none

integer,parameter :: p = 9
real,dimension(p) :: array = (/ 3,4,6,8,1,2,7,5,0 /)

print *, array

call sort(array)

print *, array

end program PE1109