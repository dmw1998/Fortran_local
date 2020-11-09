module find_largest_sum
implicit none

contains
    real function largestsum(array1,array2)

        real,dimension(:),intent(in) :: array1
        real,dimension(:),intent(in) :: array2

        real,dimension(size(array1)) :: arr1
        real,dimension(size(array2)) :: arr2
        real,dimension(size(array1),size(array2)) :: arr
        integer :: i,j
        real :: sum

        arr1 = reshape(array1,(/size(array1)/))
        arr2 = reshape(array2,(/size(array2)/))

        do i = 1,size(arr1)
            do j = 1,size(arr2)
                arr(i,j) = arr1(i) * arr2(j) 
            end do
        end do

        largestsum = arr(1,1) + arr(2,1) + arr(1,2) + arr(2,2)

        do i = 2,size(arr1)-1
            do j = 2,size(arr2)-1
                sum = arr(i,j) + arr(i+1,j) + arr(i,j+1) + arr(i+1,j+1)
                if (sum > largestsum) largestsum = sum
            end do
        end do

    end function largestsum
end module find_largest_sum

program PE1305
use find_largest_sum
implicit none

real,dimension(5) :: array1 = (/-1,3,-5,8,3/)
real,dimension(3:9) :: array2 = (/3,4,5,6,7,-8,9/)
real,dimension(4:8) :: array3 = (/13,0,0,5,-9/)
real :: output1, output2, output3

output1 = largestsum(array1,array2)
output2 = largestsum(array2,array3)
output3 = largestsum(array3,array3)

print *,output1
print *,output2
print *,output3

end program PE1305