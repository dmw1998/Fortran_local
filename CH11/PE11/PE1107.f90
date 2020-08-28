program PE1107
implicit none
interface
    subroutine calpten(r,V)
        implicit none
        real,dimension(:),intent(in) :: r
        real,dimension(:),intent(out) :: V
        integer :: i
    end subroutine calpten
end interface

real,dimension(10) :: r, V

integer :: i 

do i = 1,10
    r(i) = 0.5*i
end do

call calpten(r,V)

print '(8X,A1,15X,A1)',"r","V"
do i = 1,10
    print *, r(i),V(i)
end do

end program PE1107

subroutine calpten(r,V)
    implicit none

    real,dimension(:),intent(in) :: r
    real,dimension(:),intent(out) :: V

    integer :: i 

    do i = 1,size(r)
        V(i) = potential_energy(r(i))
    end do 
    
contains
    real function potential_energy(r)
        implicit none

        real,intent(in) :: r

        real,parameter :: D = 10000, alpha = 0.1, re = 1.0

        potential_energy = D*(1-exp(-alpha*(r-re)))**2

    end function potential_energy

end subroutine calpten