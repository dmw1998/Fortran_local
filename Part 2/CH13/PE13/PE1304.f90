module find_intersection
implicit none
contains
    subroutine intersection(arr1,arr2,intersec,size_of_intersec,num_in_intersec)

        integer,dimension(:),intent(in) :: arr1
        integer,dimension(:),intent(in) :: arr2
        integer,dimension(:),intent(out) :: intersec        
        integer,intent(out) :: size_of_intersec
        integer,intent(out) :: num_in_intersec

        integer :: i, j, k=0

        size_of_intersec = size(intersec)
        
        do i = 1,size(arr1)
            do j = 1,size(arr2)
                if (arr1(i) == arr2(j)) then
                    k = k+1
                end if
            end do
        end do
        num_in_intersec = k

        k = 0
        do i = 1,size(arr1)
            do j = 1,size(arr2)
                if (arr1(i) == arr2(j)) then
                    k = k+1
                    intersec(k) = arr2(j)
                end if
            end do
        end do

    end subroutine intersection
end module find_intersection

program PE1304
use find_intersection
implicit none

integer :: i, j, size_of_intersec, num_in_intersec
integer,dimension(18) :: arr1 = reshape((/ (i+1, i=1,18) /),(/ 18 /))
integer,dimension(7) :: arr2 = reshape((/ (i*2, i=2,8) /),(/ 7 /))
integer,dimension(5) :: intersec

call intersection(arr1,arr2,intersec,size_of_intersec,num_in_intersec)

end program PE1304