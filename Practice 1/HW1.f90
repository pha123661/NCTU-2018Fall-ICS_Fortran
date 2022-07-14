!  HW1.f90 
!
!  FUNCTIONS:
!  HW1 - Entry point of console application.
!

!****************************************************************************
!
!  PROGRAM: HW1
!
!  PURPOSE:  Entry point for the console application.
!
!****************************************************************************

    program HW1

    implicit none

    ! Variables
    integer :: hour_partA , minute_partA , second_partA , output_partA_second , ss , x , y , z 
    real :: output_partA_day
    ! Body of HW1
    print*,'請輸入時間(X小時 X分鐘 X秒)'
    read(*,*)hour_partA , minute_partA , second_partA
    output_partA_second = hour_partA * 3600 + minute_partA * 60 + second_partA
    output_partA_day = output_partA_second / 86400.0
    print* , output_partA_second , '秒' , output_partA_day , '天'
    
    print* , '請輸入總秒數'    
    read(*,*)ss
    x=ss/3600
    y=MOD(ss , 3600)/60
    z=MOD(ss , 60)
    print * , x , '小時' , y ,'分' ,z ,'秒'
    end program HW1

