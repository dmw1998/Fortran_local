program poor_printer_control
implicit none

real :: x,y

x = 3.0
y = 4.0

write (unit=*,FMT=200) x
write (unit=*,FMT=200) y
write (unit=*,FMT=200) x*y
write (unit=*,FMT=201) x/y
200 format (F5.2)
201 format (F5.3)

end program poor_printer_control