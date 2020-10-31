program gauss_seidle_method
    implicit none

    real,dimension(:,:),allocatable :: A
    real,dimension(:),allocatable :: b, x
    integer :: n,i,j,iter ,k
    real :: old,sum,ea,es=0.00001,s,dummy

    print *,"How many equations are there?"
    read *, n 

    allocate(A(n,n))
    allocate(b(n))
    allocate(x(n))

    ! Get coefficients
    print *,"Type coefficients for each equation in turn"
    do i = 1,n 
        read *,(A(i,j),j=1,n), b(i)
    end do

    print *,"Type an array of initial guess."
    read *, (x(i),i=1,n)
    
    do i = 1,n
        s = 0
        do j = 1,n
        if(i .ne. j) s = s + abs(a(i,j))
        enddo
        if (abs(a(i,i)) .lt. s) then
            write(*,*) "these equtions are not diagonal"
            stop
        endif
    end do

    do i=1,n
        dummy = A(i,i)
        do j=1,n
        A(i,j)=A(i,j)/dummy
        enddo ; b(i)=b(i)/dummy
    enddo

    iter = 0 ; k = 0
    do while (iter < 10 .and. k == 0)
        iter = iter+1
        k = 1
        do i = 1,n
            old = x(i)
            sum = b(i)
            do j = 1,n
                if(i .ne. j)then
                    sum = sum - A(i,j)*x(j)
                endif
            enddo
            x(i) = sum
            ! print*,x(i)
            if(x(i) .ne. 0 .and. k == 1) ea = (abs(x(i)-old)/x(i))*100
            if(ea .gt. es) k = 0
        enddo
        ! print*,"root"
    enddo
    print *, x
end