function bin2dec(binary)
implicit none

integer,dimension(8),intent(in) :: binary

integer :: bin2dec, i, m=0

do i=8,1,-1
    m = m + 2**i*binary(i)
end do

bin2dec = m

end function bin2dec

function dec2bin(decimal)
implicit none

integer,intent(in) :: decimal

integer :: i, m
integer,dimension(8) :: dec2bin

m=decimal
do i=8,1,-1
    dec2bin(i) = mod(m,2)
    m = m/2
end do

end function dec2bin