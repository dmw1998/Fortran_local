module type_sets
implicit none
save
    type sets
        integer :: num
        integer,dimension(:),allocatable :: set
    end type sets
end module type_sets

subroutine set_input(size,arr)
use type_sets
implicit none

    integer,intent(in) :: size
    integer,dimension(size),intent(out) :: arr 

    integer :: i,num_input

    print *,"Please input the number in the set."

    do i=1,size
        read *, num_input
        arr(i) = num_input
    end do 

end subroutine set_input

program PE0706
use type_sets
implicit none

type(sets) :: set1,set2,union,intersec
integer :: i,j,k,l,ele

! Read two sets
print *,"Please input the size of set 1."
read *, set1%num
allocate(set1%set(set1%num))
call set_input(set1%num,set1%set)

print *,"Please input the size of set 2."
read *, set2%num
allocate(set2%set(set2%num))
call set_input(set2%num,set2%set)

! Read the size of the union and intersection
union%num=set1%num+set2%num
intersec%num = 0
do i=1,set1%num
    do j=1,set2%num
        if (set1%set(i) == set2%set(j)) then
            union%num=union%num-1
            intersec%num=intersec%num+1
        end if
    end do 
end do 

! Store datas
allocate(union%set(union%num))
allocate(intersec%set(intersec%num))

k=set2%num
union%set(:k) = set2%set
l=1

do i=1,set1%num
    do j = 1,set2%num
        if (set1%set(i) == set2%set(j)) then
            print *, set1%set(i)
            intersec%set(l) = set1%set(i)
            l = l+1
            exit
        end if
    end do 
end do 

do i=1,set1%num
    ele = set1%set(i)
    do j = 1,intersec%num
        if (ele == intersec%set(j)) then
            exit
        end if 
        print *,ele
        union%set(k+1) = ele
        k = k+1
    end do
end do 

end program PE0706