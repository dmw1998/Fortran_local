subroutine set(size,arr)
implicit none

    integer,intent(in) :: size
    integer,dimension(size),intent(out) :: arr 

    integer :: i,num

    print *,"Please input the number in the set."

    do i=1,size
        read *, num
        arr(i) = num
    end do 

end subroutine set

program PE0706
implicit none

integer :: size1,size2,size_u,size_i,i,j,k,l,ele
integer, dimension(:), allocatable :: set1, set2, union, intersec

! Read two sets
print *,"Please input the size of set 1."
read *, size1
allocate(set1(size1))
call set(size1,set1)

print *,"Please input the size of set 2."
read *, size2
allocate(set2(size2))
call set(size2,set2)

! Read the size of the union and intersection
size_u=size1+size2
size_i = 0
do i=1,size1
    do j=1,size2
        if (set1(i) == set2(j)) then
            size_u=size_u-1
            size_i=size_i+1
        end if
    end do 
end do 

! Store datas
allocate(union(size_u))
allocate(intersec(size_i))

k=size2
union(:k) = set2
l=1

do i=1,size1
    do j = 1,size2
        if (set1(i) == set2(j)) then
            intersec(l) = set1(i)
            l = l+1
            exit
        end if
    end do 
end do 

do i=1,size1
    ele = set1(i)
    do j = 1,size_i
        if (ele == intersec(j)) then
            exit
        end if 
        union(k+1) = ele
        k = k+1
    end do
end do 

print *, union
print *, ""
print *, intersec

end program PE0706