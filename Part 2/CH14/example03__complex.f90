module constants_complex
implicit none
    integer, parameter :: complex_kind = &
                          selected_real_kind(12,70)
end module constants_complex

program accurate
use constants_complex
implicit none

complex(kind = complex_kind), dimension(4) :: z 

z(1) = (3.72471778E-45_complex_kind, &
        723.115798E-56_complex_kind)

end program accurate