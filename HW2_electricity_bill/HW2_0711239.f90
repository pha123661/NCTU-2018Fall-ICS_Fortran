    Module datas
    implicit none
    save
    
    character(len=2) :: cate        !分類R B1 或 B2用
    character(len=4) :: sub_cate    !分類EUI第1 2 3 4型用
    integer :: E12_1 , E2_3 , E4_5 , E6_7 , E8_9 , E10_11 , bill_sum , E_sum,mes   !12~1月 2~3月...之每兩月電費、電費和用電總合和邏輯判斷用(mes)
    integer :: num_R , num_B1 , num_B2 , num=0  !R B1 B2 各分類總人數
    integer :: num_R1=0,num_R2=0,num_R3=0,num_R4=0,num_B11=0,num_B12=0,num_B13=0,num_B14=0,num_B21=0,num_B22=0,num_B23=0,num_B24=0  !第一型R 第二型R...第四型B2之各分類人數
    real :: Area , EUI  !面積和EUI
 
    
    end module
    
    program HW2_0711239
    use datas
    implicit none
    
    open(10,file='electricity_bill_input.txt',status='old')         !輸入檔案並設定其io
    open(11,file='electricity_bill_output.txt',status='replace')
    call calculation
    
    end program HW2_0711239

    
    Subroutine calculation
    use datas
    implicit none
    integer,external :: R_bill_nonsum,R_bill_summer,B1_bill_nonsum,B1_bill_summer,B2_bill_nonsum,B2_bill_summer !宣告函數
    write(11,'(T2,A,2x,A,2x,A,1x,A,2x,A,2x,A,4x,A,2x,A)')'no','cate','-----electricity used-----','year_bill','year_elec','area','EUI','sub_cate'   !先輸出列表開頭(方便閱讀)
    
    do
        read(10,*,iostat=mes)cate,E12_1,E2_3,E4_5,E6_7,E8_9,E10_11,Area
        if(mes<0) exit
        num=num+1   !計算第幾筆資料
        E_sum=E12_1 + E2_3 + E4_5 + E6_7 + E8_9 + E10_11    !計算年度用電量
        EUI=E_sum/Area          !計算EUI
        
        select case(cate)
        case('R')
            bill_sum=R_bill_nonsum (E12_1)+R_bill_nonsum (E2_3)+R_bill_nonsum (E4_5)+R_bill_nonsum (E10_11)+R_bill_summer(E6_7)+R_bill_summer(E8_9)+0.5 !計算R分類年度電費
            
            if(EUI<15) then     !判斷為第一~四型之R
                sub_cate='R_1'
                num_R1=num_R1+1
            else if(15<=EUI .and. EUI<20) then
                sub_cate='R_2'
                num_R2=num_R2+1
            else if(20<=EUI .and. EUI<25) then
                sub_cate='R_3'
                num_R3=num_R3+1
            else if(25<=EUI) then
                sub_cate='R_4'
                num_R4=num_R4+1
            end if
            
        case('B1')
            bill_sum=B1_bill_nonsum (E12_1)+B1_bill_nonsum (E2_3)+B1_bill_nonsum (E4_5)+B1_bill_nonsum (E10_11)+B1_bill_summer(E6_7)+B1_bill_summer(E8_9)+0.5   !計算B1分類年度電費
        
            if(EUI<40) then     !判斷為第一~四型之B1
                sub_cate='B1_1'
                num_B11=num_B11+1
            else if(40<=EUI .and. EUI<50) then
                sub_cate='B1_2'
                num_B12=num_B12+1
            else if(50<=EUI .and. EUI<60) then
                sub_cate='B1_3'
                num_B13=num_B13+1
            else if(60<=EUI) then
                sub_cate='B1_4'
                num_B14=num_B14+1
            end if
            
        case('B2')
            bill_sum=B2_bill_nonsum (E12_1)+B2_bill_nonsum (E2_3)+B2_bill_nonsum (E4_5)+B2_bill_nonsum (E10_11)+B2_bill_summer(E6_7)+B2_bill_summer(E8_9)+0.5   !計算B2分類年度電費
        
            if(EUI<40) then     !判斷為第一~四型之B1
                sub_cate='B2_1'
                num_B21=num_B21+1
            else if(40<=EUI .and. EUI<50) then
                sub_cate='B2_2'
                num_B22=num_B22+1
            else if(50<=EUI .and. EUI<60) then
                sub_cate='B2_3'
                num_B23=num_B23+1
            else if(60<=EUI) then
                sub_cate='B2_4'
                num_B24=num_B24+1
            end if
            
        end select    
        
    write(11,'(T3,I2,2x,A,3x,6I4,4x,I6,4x,I6,3x,F7.2,2x,F5.2,3x,A)')num,cate,E12_1,E2_3,E4_5,E6_7,E8_9,E10_11,bill_sum,E_sum,Area,EUI,sub_cate  !輸出計算出之各項數值
    
    end do
    
    write(11,'(T2,A,I3)')'total number = ',num
    num_R=num_R1+num_R2+num_R3+num_R4
    num_B1=num_B11+num_B12+num_B13+num_B14
    num_B2=num_B21+num_B22+num_B23+num_B24
    write(11,'(T2,A,I3,A,I3,A,I3)')'number of Resident = ',num_R,' ; number of Business I = ',num_B1,' ; number of Business II =', num_B2   !輸出各分類之人數
    write(11,'(T2,A,I2,2x,I2,2x,I2,2x,I2,2x)')'Sub of Resident    (category 1~4) : ',num_R1,num_R2,num_R3,num_R4
    write(11,'(T2,A,I2,2x,I2,2x,I2,2x,I2,2x)')'Sub of Business I  (category 1~4) : ',num_B11,num_B12,num_B13,num_B14
    write(11,'(T2,A,I2,2x,I2,2x,I2,2x,I2,2x)')'Sub of Business II (category 1~4) : ',num_B21,num_B22,num_B23,num_B24
    
    end subroutine
    
    integer Function R_bill_nonsum (E)  !計算非夏季之R分類之電費
    implicit none
    integer :: E
    select case (E)
    case(:120)
        R_bill_nonsum=E*1.63+0.5
    case(121:330)
        R_bill_nonsum=120*1.63+(E-120)*2.1+0.5
    case(331:500)
        R_bill_nonsum=120*1.63+(330-120)*2.1+(E-330)*2.89+0.5
    case(501:700)
        R_bill_nonsum=120*1.63+(330-120)*2.1+(500-330)*2.89+(E-500)*3.79+0.5
    case(701:1000)
        R_bill_nonsum=120*1.63+(330-120)*2.1+(500-330)*2.89+(700-500)*3.79+(E-700)*4.42+0.5
    case(1001:)
        R_bill_nonsum=120*1.63+(330-120)*2.1+(500-330)*2.89+(700-500)*3.79+(1000-700)*4.42+(E-1000)*4.83+0.5
    end select
    
    end function
    
    integer function R_bill_summer (E)  !計算夏季之R分類之電費
    implicit none
    
    integer :: E
    select case (E)
    case(:120)
        R_bill_summer=E*1.63+0.5
    case(121:330)
        R_bill_summer=120*1.63+(E-120)*2.38+0.5
    case(331:500)
        R_bill_summer=120*1.63+(330-120)*2.38+(E-330)*3.52+0.5
    case(501:700)
        R_bill_summer=120*1.63+(330-120)*2.38+(500-330)*3.52+(E-500)*4.61+0.5
    case(701:1000)
        R_bill_summer=120*1.63+(330-120)*2.38+(500-330)*3.52+(700-500)*4.61+(E-700)*5.42+0.5
    case(1001:)
        R_bill_summer=120*1.63+(330-120)*2.38+(500-330)*3.52+(700-500)*4.61+(1000-700)*5.42+(E-1000)*6.13+0.5
    end select
    
    end function
    
    integer function B1_bill_nonsum(E)  !計算非夏季之B1分類之電費
    implicit none
    
    integer :: E
    select case(E)
    case(:330)
        B1_bill_nonsum=E*2.12+0.5
    case(331:700)
        B1_bill_nonsum=330*2.12+(E-330)*2.91+0.5
    case(701:1500)
        B1_bill_nonsum=330*2.12+(700-330)*2.91+(E-700)*3.44+0.5
    case(1501:)
        B1_bill_nonsum=330*2.12+(700-330)*2.91+(1500-700)*3.44+(E-1500)*4.85+0.5
    end select
    
    end function
    
    integer function B1_bill_summer(E)  !計算夏季之B1分類之電費
    implicit none
    
    integer :: E
    select case(E)
    case(:330)
        B1_bill_summer=E*2.53+0.5
    case(331:700)
        B1_bill_summer=330*2.53+(E-330)*3.55+0.5
    case(701:1500)
        B1_bill_summer=330*2.53+(700-330)*3.55+(E-700)*4.25+0.5
    case(1501:)
        B1_bill_summer=330*2.53+(700-330)*3.55+(1500-700)*4.25+(E-1500)*6.15+0.5
    end select
    
    end function
    
    integer function B2_bill_nonsum(E)  !計算非夏季之B2分類之電費
    implicit none
    
    integer :: E
    select case(E)
    case(:330)
        B2_bill_nonsum=E*2.12+0.5
    case(331:700)
        B2_bill_nonsum=330*2.12+(E-330)*2.91+0.5
    case(701:)
        B2_bill_nonsum=330*2.12+(700-330)*2.91+(E-700)*3.44+0.5
    end select
    
    end function
    
    integer function B2_bill_summer(E)  !計算夏季之B2分類之電費
    implicit none
    
    integer :: E
    select case(E)
    case(:330)
        B2_bill_summer=E*2.53+0.5
    case(331:700)
        B2_bill_summer=330*2.53+(E-330)*3.55+0.5
    case(701:)
        B2_bill_summer=330*2.53+(700-330)*3.55+(E-700)*4.25+0.5
    end select
    
    end function