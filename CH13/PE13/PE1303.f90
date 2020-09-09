module cal_norm
implicit none

contains

    real function two_norm(array)

        real,dimension(:,:),intent(in) :: array

        integer :: i, j, row, column
        real,dimension(:,:),allocatable :: arr
        real :: sum

        row = size(array,1)
        column = size(array,2)
        allocate(arr(row,column))
        arr = reshape(array, (/row,column/))

        do i = 1,row
            do j = 1,column
                sum = sum + arr(i,j)**2
            end do
        end do

        two_norm = sqrt(sum)

    end function two_norm

    real function infinity_norm(array)

        real,dimension(:,:),intent(in) :: array

        integer :: i, j, row, column
        real,dimension(:,:),allocatable :: arr
        real :: max

        row = size(array,1)
        column = size(array,2)
        allocate(arr(row,column))
        arr = reshape(array, (/row,column/))

        do i = 1,row
            do j = 1,column
                if (abs(arr(i,j)) > max) max = abs(arr(i,j))
            end do
        end do

        infinity_norm = max
    
    end function infinity_norm

end module cal_norm

program PE1303
use cal_norm
implicit none

real,dimension(3:4,9) :: array
real :: result1,result2
integer :: i, j

do i = 3,4
    do j = 1,9
        array(i,j) = (-1)**j*i*j
    end do
end do

result1 = infinity_norm(array)
result2 = two_norm(array)

print *, "The infinit-norm is ", result1
print *, "The two-norm is ", result2

end program PE1303