program PE1406
implicit none

integer :: p, q

print *, "# of dcimal digit"
print *, "specified | actual"

do p = 1,30
    q = selected_real_kind(p)

    print '(4X,I2,9X,I2)', p, q

end do

end program PE1406