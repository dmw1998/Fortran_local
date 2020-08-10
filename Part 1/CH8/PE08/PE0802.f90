program ptest
implicit none

! This program tests the effect of printer control
! characters on the display and on the printer

integer,parameter :: display=6, printer=6

integer :: n 

write (unit=display,fmt=100)
write (unit=printer,fmt=100)
100 format                                              &
  ("1","This line should be at the top of a new page"/  &
   " ","This should be on the next line"/               &
   "0","This line should be after a blank line"/        &
   " ","This line should be"/                           &
   "+","                    on the next line"/          &
   "0",23X,"after a blank line"/                        &
   "+","And this one should be")

! Wait until you have checked the output to the display
write (unit=display,fmt=*)                              &
     "Check the display and then type an integer"
read *,n 

! Repeat the output - to see what happens to the display
write (display,fmt=100)
write (printer,fmt=100)

end program ptest