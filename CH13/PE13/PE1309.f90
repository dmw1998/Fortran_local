module modify
implicit none
contains
    subroutine modify_square_array(array)

        integer,dimension(:,:),intent(inout) :: array

        integer :: m, n, i, j

        m = size(array,1)
        n = size(array,2)

        print *,"The original matrix is " 
        do i = 1,m
            print *, array(i,:)
        end do 

        if (m /= n) then
            print *,"A square integer array is required."
            return
        end if

        do i = 1,m 
            do j = 1,n 
                if (array(i,j) < 0 .or. array(i,j) > 10) then
                    print *,"We want the entries betweent 0 and 10."
                    return
                end if 
            end do
        end do

        where (array == 10)
            array = 0
        end where
        where (array > 5 .and. array < 9) 
            array = array + 1
        end where
        where (array > 0 .and. array < 6)
            array = array - 1
        end where 

        print *,"The modified matrix is "
        do i = 1,m
            print *, array(i,:)
        end do 

    end subroutine modify_square_array
end module modify

program PE1309
use modify
implicit none

integer,dimension(5,5) :: array1,array2
integer,dimension(3,5) :: array3

integer :: i, j

do i = 1,5
    do j = 1,5
        array1(i,j) = (-1)**i*j
        array2(i,j) = i+j
    end do

    do j = 1,3
        array3(j,i) = i+j
    end do
end do 

call modify_square_array(array1)
call modify_square_array(array2)
call modify_square_array(array3)

end program PE1309