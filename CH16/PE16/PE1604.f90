program PE1604
implicit none

real, dimension(3) :: arr = (/ 2, 5, -8 /)

print *, find_max(arr)

contains

    real function find_max(arg)
        implicit none

        real,dimension(:), intent(in) :: arg

        real, dimension(:,:), pointer :: p 
        integer :: n, i, j

        n = size(arg)
        allocate(p(n,n))
        do i = 1,n
            do j = 1,n 
                p(i,j) = arg(i)/arg(j)
            end do
        end do

        find_max = maxval(p)
        deallocate(p)

    end function find_max

end program PE1604
