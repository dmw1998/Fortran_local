logical function nand(a,b)
implicit none

logical,intent(in) :: a,b

logical :: m

m = a .and. b
nand = .not. m

end function nand

program PE0504
implicit none

logical, external :: nand

logical, parameter :: T = .true., F = .false.

print *,"A  B   A .NAND. B"
print *,"T  T      ",nand(T,T)
print *,"T  F      ",nand(T,F)
print *,"F  T      ",nand(F,T)
print *,"F  F      ",nand(F,F)

end program PE0504