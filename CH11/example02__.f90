subroutine new_page
    implicit none

    ! This subroutine prints a heading and the page number
    ! at the top of the next page

    ! Local variable
    integer :: page=0

    ! Update page number and print heading
    page = page + 1
    print '("1",20X,"Example Page Heading",15X,I3//)', page 

end subroutine new_page