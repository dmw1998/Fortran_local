program Radioactive_decay
implicit none

! This program processes experimental data relating
! to radioactive decay

! Constant declarations
! max_readings is maximum number of sets of data
! in and out are the unit numbers for reading and writing
integer,parameter :: max_readings=1000,in=5,out=6

! Variable declarations
integer :: alpha,beta,gamma,i,max_interval=0
real :: time,last_time=0.0,period, &
        av_alpha,av_beta,av_gamma,max_av_gamma=0.0

! Print headings
write (unit=out,fmt=201)

! Process max_readings sets of data in a loop
do i=1,max_readings
    read (unit=in,fmt=101) time,alpha,beta,gamma

    ! Calculate interval since last readings
    period = time-last_time
    last_time = time

    ! Calculate average rates of emission
    av_alpha = alpha/period
    av_beta = beta/period
    av_gamma = gamma/period

    ! Print statistics for this interval
    write (unit=out,fmt=202) i,period,alpha,beta,gamma, &
                             av_alpha,av_beta,av_gamma

    ! Check for maximum gamma radiation in this period
    if (av_gamma > max_av_gamma) then
        max_av_gamma = av_gamma
        max_interval = i 
    end if 
end do
! Print details of interval with maximum gamma radiation
write (unit=out,fmt=203) max_av_gamma,max_interval

! Format statements
101 format (F8.2,3(5X,I6))
201 format ("1","Interval",T11,"Time",T17,"Alpha", &
    T24,"Beta",T30,"Gamma",T37,"Average",T46,"Average", &
    T55,"Average"/ &
    T38,"Alpha",T47,"Beta",T56,"Gamma")
202 format (I6,F8.2,2I6,I7,2F9.2,F10.2)
203 format ("0",T3,"Maximum average gamma radiation was", &
    F7.2," in interval",I5)

end program Radioactive_decay