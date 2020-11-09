program PE1601
implicit none

integer :: n
integer, pointer :: p(:)

print *,"Please input a maximum integer n."
read *, n 

if (n <= 1) then
    print *,"There are no prime numbers."
end if

call find_prime(p,n)

print *, p 

contains

recursive subroutine find_prime(p,n)

    integer, pointer, intent(inout) :: p(:)
    integer, intent(in) :: n 

    integer :: i, j

    do i = 1,n
        p(i) = i
    end do

    if (j /= n) then
        do j = p(i)+1,n
            if (mod(j,p(i)) /= 0) then
                i = i+1
                p(i) = j
            end if
        end do
    end if

end subroutine find_prime

end program PE1601