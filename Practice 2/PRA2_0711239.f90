    program PRA2_0711239

    implicit none

    ! Variables
    real::x1,y1,x2,y2,x3,y3,ax,ay,bx,by,delta
    ! Body of PRA2_0711239
    print * ,'輸入三頂點之x1,y1,x2,y2,x3,y3'
    read(*,*)x1,y1,x2,y2,x3,y3 !依次輸入三點x,y座標
    ax = x1-x2
    ay = y1-y2
    bx = x3-x2
    by = y3-y2
    delta = (ax*by-ay*bx)/2
    print *,'面積為',ABS(delta),'平方單位'

    end program PRA2_0711239

