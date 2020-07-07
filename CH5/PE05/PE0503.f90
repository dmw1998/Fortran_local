program PE0503
implicit none

logical, parameter :: T = .true., F = .false.

print *,"A  B   A .AND. B"
print *,"T  T      ",T.AND.T
print *,"T  F      ",T.AND.F
print *,"F  T      ",F.AND.T
print *,"F  F      ",F.AND.F
print *," "

print *,"A  B   A .OR. B"
print *,"T  T     ",T.OR.T
print *,"T  F     ",T.OR.F
print *,"F  T     ",F.OR.T
print *,"F  F     ",F.OR.F
print *," "

print *,"A  B   A .EQV. B"
print *,"T  T      ",T.EQV.T
print *,"T  F      ",T.EQV.F
print *,"F  T      ",F.EQV.T
print *,"F  F      ",F.EQV.F
print *," "

print *,"A  B   A .NEQV. B"
print *,"T  T      ",T.NEQV.T
print *,"T  F      ",T.NEQV.F
print *,"F  T      ",F.NEQV.T
print *,"F  F      ",F.NEQV.F
print *," "

end program PE0503