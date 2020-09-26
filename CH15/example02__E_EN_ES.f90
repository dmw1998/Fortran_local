program e_en_and_es_editing
implicit none

real,dimension(4) :: x= (/1.234, -0.5, 0.00678, 98765.4/)

print '(4E14.3/4EN14.3/4ES14.3)', x, x, x

end program e_en_and_es_editing