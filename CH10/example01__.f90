program exponential_unstable
implicit none

real :: x=5.0, ans=0.0, term=1.0
integer :: i 

print '(T5, "i", T14, "TERMI", T29, "SUMI")'
do i = 1,25
    ans = ans + term 
    print '(I5,2X,2E15.6)', i, term, ans
    term = term*(-x)/real(i)
end do 

end program exponential_unstable