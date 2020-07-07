program PE0601
implicit none

integer :: i, year = 1986

do i = 1,10
    !Start fron 1986, print the next 10 years Halley's coment appears
    year = year + 76
    print *,i," times of the comet's appearances is ",year
    
enddo

end program PE0601