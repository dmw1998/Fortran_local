program exponential_unstable
implicit none

real :: x=5.0, r_ans=0.0, term=1.0
integer :: i 

print '(T5, "i", T14, "TERMI", T29, "SUMI")'
do i = 1,25
    r_ans = r_ans + term 
    print '(I5,2X,2E15.7)', i, term, 1.0/r_ans
    term = term*(x)/float(i)
end do 

end program exponential_unstable