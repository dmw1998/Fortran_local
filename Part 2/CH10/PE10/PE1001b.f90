program PE1001b
implicit none

real(kind=16) :: pi, x = -1.0
real(kind=selected_real_kind(p=6)) :: pi6
real(kind=selected_real_kind(p=10)) :: pi10
real(kind=selected_real_kind(p=14)) :: pi14
real(kind=selected_real_kind(p=18)) :: pi18

pi = acos(x)
pi6 = acos(x)
pi10 = acos(x)
pi14 = acos(x)
pi18 = acos(x)

print *, pi 
print *, pi6 
print *, pi10 
print *, pi14 
print *, pi18 

end program PE1001b