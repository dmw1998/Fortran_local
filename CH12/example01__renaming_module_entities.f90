program rename_exxample
    use geometry, circle_def => circle, line_def => line, &
                point_def => point, line => gen_line
    implicit none

    type(point_def) :: pt1, pt2
    type(line_def) :: ln1, ln2, ln3
    type(circle_def) :: cir1


    call line(ln1,pt1,pt2)
    call line(ln2,pt2,ln1)
    call line(ln3,pt2,cir1,'xlarge')

end program rename_exxample