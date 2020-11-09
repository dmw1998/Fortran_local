program PE1108
implicit none
interface
    subroutine findpivot(array,pivot)
        implicit none
        real,dimension(:),intent(inout) :: array
        integer,intent(out) :: pivot
        integer,save :: i, j
    end subroutine findpivot

    subroutine sortarray(array,pivot)
        implicit none
        real,dimension(:),intent(inout) :: array
        integer,intent(inout) :: pivot
        real,dimension(pivot) :: subarray1
        real,dimension(size(array)-pivot) :: subarray2
        integer :: pivot1,pivot2,i
    end subroutine sortarray
end interface

real,dimension(8) :: array
integer :: pivot, k

array = [4,5,1,8,2,6,3,7]

call findpivot(array,pivot)
print *, "The pivot is ",array(pivot)
print *, array

call sortarray(array,pivot)
print *, array

end program PE1108

subroutine findpivot(array,pivot)
    implicit none

    real,dimension(:),intent(inout) :: array
    integer,intent(out) :: pivot

    integer,save:: i, j

    i = 1
    j = size(array)

    do while (i /= j)
        do while (array(i) <= array(j))
            j = j-1
            pivot = j
            if (i == j) return
        end do

        if (array(i) > array(j)) then
            call interchange(array(i),array(j))
        end if

        do while (array(i) <= array(j))
            i = i+1
            pivot = i
            if (i == j) return
        end do
        
        if (array(i) > array(j)) then
            call interchange(array(i),array(j))
        end if

    end do 

contains
    subroutine interchange(a,b)
        real,intent(inout) :: a,b

        real :: c

        c = a
        a = b
        b = c
    end subroutine interchange

end subroutine findpivot

recursive subroutine sortarray(array,pivot)
    implicit none
    interface
    subroutine findpivot(array,pivot)
        implicit none
        real,dimension(:),intent(inout) :: array
        integer,intent(out) :: pivot
        integer,save :: i, j
    end subroutine findpivot
    end interface

    real,dimension(:),intent(inout) :: array
    integer,intent(inout) :: pivot

    real,dimension(pivot) :: subarray1
    real,dimension(size(array)-pivot) :: subarray2
    integer :: pivot1,pivot2,i

    do i = 1,pivot
        subarray1(i) = array(i)
    end do

    do i = pivot+1,size(array)
        subarray2(i-pivot) = array(i)
    end do

    call findpivot(subarray1,pivot1)
    call findpivot(subarray2,pivot2)

    do i = 1,pivot
        array(i) = subarray1(i)
    end do

    do i = 1,size(subarray2)
        array(pivot+i) = subarray2(i)
    end do

    if (size(subarray1) > 1) then
        call sortarray(subarray1,pivot1)
    end if

    if (size(subarray2) > 1) then
        call sortarray(subarray2,pivot2)
    end if

end subroutine sortarray