program PE0705
implicit none

character(len=6),dimension(1:6) :: competitor = (/ "name_1","name_2","name_3","name_4","name_5","name_6" /)      ! Input the names of six competitors
character(len=6) :: name1, name2, p 
real,dimension(1:6) :: scores = (/ 0,0,0,0,0,0 /)
real :: scores1,scores2,s 
integer :: bonuses(15),i,j,k,l,m

! Read the results
do i=1,15           ! For six competitors, there are 15 games.
    print *,i,"th round:"
    print *,"Please input the name and scores of the first competitor."
    read *, name1,scores1
    print *,"Please input the name and scores of the second competitor."
    read *, name2,scores2

    ! Add the score to the same name
    do j=1,size(competitor)
        if (competitor(j) == name1) then
            scores(j) = scores(j) + scores1
        else if (competitor(j) == name2) then
            scores(j) = scores(j) + scores2
        end if
    end do

    print *," "

    ! Sort the scores
    do l=1,5
        scores1 = scores(l)
        k = l 

        do j=l+1,6
            if (scores(j) >= scores(l)) then
                scores1 = scores(j)
                k = j
            end if 
        enddo

        if (k /= l) then
            p = competitor(l)
            s = scores(l)
            competitor(l) = competitor(k)
            scores(l) = scores(k)
            competitor(k) = p 
            scores(k) = s
        end if
    end do

    ! Calculate the bonuses for winner
    bonuses(i) = scores(1)/5

    ! Print the result round by round and total
    do j=2,6
        if (scores(j) /= scores(1)) exit
    end do
    print *,"The winner(s) is/are ",competitor(1:j-1)
    print *,"The score is",scores(1)

    do m=j+1,6
        if (scores(m) /= scores(j)) exit
    end do
    print *,"The running-up(s) is/(are) ",competitor(j:m-1)
    print *,"The score is ",scores(j)

end do 

end program PE0705