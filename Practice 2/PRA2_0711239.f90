    program PRA2_0711239

    implicit none

    ! Variables
    real::x1,y1,x2,y2,x3,y3,ax,ay,bx,by,delta
    ! Body of PRA2_0711239
    print * ,'��J�T���I��x1,y1,x2,y2,x3,y3'
    read(*,*)x1,y1,x2,y2,x3,y3 !�̦���J�T�Ix,y�y��
    ax = x1-x2
    ay = y1-y2
    bx = x3-x2
    by = y3-y2
    delta = (ax*by-ay*bx)/2
    print *,'���n��',ABS(delta),'������'

    end program PRA2_0711239

