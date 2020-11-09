module constants
implicit none
    real,parameter :: pi = 3.1415926536, twopi = 2*pi
end module constants

program filter
use constants
implicit none

! This program calculates the transfer function for a simple
! electronic filter, consisting of a capacitor and an
! inductor in series, with a resistor in parallel, and then
! prints the voltage amplification and phase shift that it
! proceduces on input signals in a specified range of
! frequencies

! Declarations
integer :: f1,f2,f_inc,f 
real :: r,c,l,amplitude,phase
complex :: h
character :: answer

! Get data for next case
call input

! Print title for this circuit, and column headers
print '("1","Freuency response betweeen ",I5," Hz. &
            &and ",I5," Hz." / &
        " ","for a filter with a series capacitance &
            &of ",F7.3," microfarads", / &
        " ","in parallel with a resistance of ", &
            F7.3," kilo-ohms is:", // &
        " ","Frequency",T15," Voltage",T30, &
            "Phase" / &
        " "," (Hz.)",T15," amplification",T30, &
            "shift"//)', f1,f2,c,l,r 

! Convert capacitance to farads, inductance to henries,
! and resistance to ohms
c = c*1.0E-6
l = l*1.0E-3
r = r*1.0E3

! Loop for required frequencies
do f=f1,f2,f_inc
    ! Calculate transfer function
    h = cmplx(r,0.0)/cmplx(r,twopi*f*1-1.0/(twopi*f*c))

    ! Amplification factor is absolute value of H
    amplitude = abs(h)

    ! Phase shift is arctangent of imaginary part
    ! divided by real part
    phase = atan2(aimag(h),real(h))

    ! Convert to degrees
    phase = 180.0*phase/pi 

    ! Print results for this frequencey
    print '(" ",I6,T15,F9.3,T30,F5.1)', f,amplitude,phase 
end do

contains

subroutine input

    ! This is the input routine for the main program

    print *,"What is the value of the capacitance &
            &(microfarads)?"
    read *,c 
    print *,"What is the value of the inductance &
            &(millihenries)?"
    read *,l 
    print *,"What is the value of the resistance &
            &(kilo-ohms)?"
    read *,r 
    
    ! Read frequency data
    do
        print *,"Give initial and final frequencies, and &
                &increment (Hz)"
        read *,f1,f2,f_inc
        ! Check for validity
        if (f1<=f2 .and. f_inc>0.0) then
            exit
        else
            print *,"Data is inconsistent. Please try again"
        end if
    end do
end subroutine input

end program filter