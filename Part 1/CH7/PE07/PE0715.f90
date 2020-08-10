module convertSA
implicit none
save
contains
    subroutine S2A(S,A)
    implicit none

        integer,dimension(:),intent(in) :: S 
        logical,dimension(100),intent(out) :: A 

        integer :: i,n

        A = (/ (.false.,i=1,100) /)
        do i=1,size(S)
            n=S(i)
            A(n) = .true.
        end do 

    end subroutine S2A

    subroutine A2S(A,S)
    implicit none

        logical,dimension(100),intent(in) :: A 
        integer,dimension(:),intent(out) :: S 

        integer :: i,j=1

        do i=1,100
            if (A(i)) then
                S(j) = i 
                j=j+1
            end if
        end do 

    end subroutine A2S 
end module convertSA

module set_opt
implicit none
save
contains
    subroutine union(A1,A2,U)
    implicit none 

        logical,dimension(100),intent(in) :: A1,A2
        logical,dimension(100),intent(out) :: U 

        integer :: i 

        do i=1,100
            U(i) = A1(i) .or. A2(i)
        end do 
    end subroutine union

    subroutine intersection(A1,A2,intersec)
    implicit none 

        logical,dimension(100),intent(in) :: A1,A2
        logical,dimension(100),intent(out) :: intersec

        integer :: i 

        do i=1,100
            intersec(i) = A1(i) .and. A2(i)
        end do 
    end subroutine intersection
end module set_opt 

program PE0715
use convertSA
use set_opt
implicit none

integer,dimension(:),allocatable :: S1,S2,resl_U,resl_intersec
logical,dimension(100) :: A1,A2,U,intersec
integer :: i,n1,n2,size_u,size_i

print *,"Please input the size of set S1."
read *, n1
allocate(S1(n1))
print *,"Please input the elements in the set S1."
do i=1,n1
    read *,S1(i)
end do
call S2A(S1,A1)

print *,"Please input the size of set S2."
read *, n2
allocate(S2(n2))
print *,"Please input the elements in the set S2."
do i=1,n2
    read *,S2(i)
end do
call S2A(S2,A2)

call union(A1,A2,U)
call intersection(A1,A2,intersec)

! Read the size of the union and intersection
size_u = 0
size_i = 0
do i=1,100
    if (U(i)) size_u=size_u+1
    if (intersec(i)) size_i=size_i+1
end do 

! Store datas
allocate(resl_U(size_u))
allocate(resl_intersec(size_i))

call A2S(U,resl_U)
call A2S(intersec,resl_intersec)

print *, resl_U
print *,""
print *, resl_intersec

end program PE0715