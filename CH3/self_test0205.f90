program test3_2_5
implicit none

CHARACTER(LEN=16) :: a,b,c,d

a = "A kindly giant"
b = "A small man"
c = b(:8)//"step"
d = "for a"//b(8:)
b = " "//d(:4)//b(9:11)//a(3:6)
a = a(:2)//a(10:15)//"leap"

PRINT *,c(:13),d 
PRINT *,TRIM(a(:12)),b

end program test3_2_5