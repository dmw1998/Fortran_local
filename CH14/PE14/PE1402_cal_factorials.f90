program cal_factorials
implicit none

! four byte real
real(kind = 4) :: shorter_factorial = 1

! eight byte real
real(kind = 8) :: short_factorial = 1

! sixteen byte real
real(kind = 16) :: long_factorial = 1

integer :: n2=0,n4=0,n8=0,i = 1

! four byte real factorial
    do while (shorter_factorial < 1.0E10)
        n2 = n2+1
        shorter_factorial = shorter_factorial*i 
        i = i+1
    end do 

    shorter_factorial = 1
    n2 = n2-1

    do i = 1,n2
        shorter_factorial = shorter_factorial*i
    end do

    print '("For four byte real, the largest possible factorial ",I2,"! is")', n2
    print *, shorter_factorial

! eight byte real factorial
    i = 1
    do while (short_factorial < 1.0E22)
        n4 = n4+1
        short_factorial = short_factorial*i 
        i = i+1
    end do 

    short_factorial = 1
    n4 = n4-1

    do i = 1,n4
        short_factorial = short_factorial*i
    end do

    print '("For eight byte real, the largest possible factorial ",I2,"! is")', n4
    print *, short_factorial

! sixteen byte real factorial
    i = 1
    do while (long_factorial < 1.0E38)
        n8 = n8+1
        long_factorial = long_factorial*i 
        i = i+1
    end do 

    long_factorial = 1
    n8 = n8-1

    do i = 1,n8
        long_factorial = long_factorial*i
    end do

    print '("For sixteen byte real, the largest possible factorial ",I2,"! is")', n8
    print *, long_factorial

end program cal_factorials