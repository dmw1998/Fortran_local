program PE0304
implicit none

real :: m_a, m_b, miu

print *,"Please input m_a and m_b"
read *, m_a, m_b

miu = m_a*m_b/(m_a+m_b)

print *,"Miu is equal to ", miu

end program PE0304