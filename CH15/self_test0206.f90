program test_15_2_6
implicit none

character(len=5),dimension(10) :: line1, line2
integer :: i 

line1 = (/"One  ","Two  ","Three","Four ","Five ",   &
          "Six  ","Seven","Eight","Nine ","Ten  "/)

do i = 1,10
    read (unit=line1,fmt='(A5)') line2(i)
end do 
write (unit=*,fmt='(10A6)') line2

read (unit=line1,fmt='(A5)') line2
write (unit=*,fmt='(10A6)') line2

end program test_15_2_6