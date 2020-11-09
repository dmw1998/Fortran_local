program test14_2_4
implicit none
complex :: p=(1,2), q=(3,4)
real :: x=5.0

print *,p,q,x
print *,p+q,p-q,p+x
print *,p*q,x*q,p/q
print *,p/x,x/p,conjg(x/p)

end program test14_2_4