program PE0612
implicit none

real :: L0, E, L, T
integer :: i,k

do i = 1,10
    L0 = 0.1*i
    E = (1.0/L0-1.0)/20.0
    print *,"Give L0 = ", L0
    print *,"Then E = ",E
    print *,"Under these, L and T are shown like below."
    print *,"       L               T"
    do k = 0,10         ! Take rage 0 to 20 for T, out put 11 results
        T = 2.0*k
        L = L0 + E*T*L0
        print *,L, T
    end do
    print *," "
end do

end program PE0612