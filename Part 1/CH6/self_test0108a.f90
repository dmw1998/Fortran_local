program loop_test_1
implicit none
integer :: i=1,j=2,k=4,l=8,m=0,n=0
do i=j,k,l 
    k=i 
    do j=l,m,k
        n=j
        do k=l,n 
            do l=i,k
                m=k*l 
            end do
        end do
    end do
end do
print *, i,j,k,l,m,n
end program loop_test_1