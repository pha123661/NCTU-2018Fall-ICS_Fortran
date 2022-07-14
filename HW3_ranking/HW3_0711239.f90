    Module datas
    implicit none
    save
    
    real,allocatable :: score(:),grade(:)       !個別百分制分數和個別等級制分數
    integer,allocatable :: AID(:),ACH(:),AMA(:),AEN(:),AHI(:),AGE(:)    !ID和五科考科之陣列(A=Array)
    real,allocatable :: s_temp(:),r_temp(:)  !排序百分制分數時用
    real,allocatable :: g_temp(:),k_temp(:)  !用於排序等級制 (放在Module中是因為排序和輸出為兩不同副程式)
    integer :: i=0,j=0          !用來決定do執行次數所用
    integer :: S_pass,S(50:100) !分別為百分制及格人數 100~90分人數... 50分以下人數
    integer :: G_pass,G(10:43)  !分別為等級制及格人數 4.3~4.1分人數...1.0分以下人數
    integer ,parameter :: lb=1  !所有陣列第一維度之lower bound
    integer :: ub=0             !所有矩陣第一維度之upper bound(尚未決定故先設0)
    real :: S_sum,S_avg,G_sum,G_avg !前兩者為百分制總分和平均 後兩者則為等級制
    real :: S_pass_sum,S_pass_avg,G_pass_sum,G_pass_avg !前兩者為百分制及格者總分和平均 後兩者則為等級制
    
    end module
    
    program HW3_0711239
    use datas
    implicit none
    open(10,file='ranking_input.txt',status='old')
    open(11,file='ranking_output.txt',status='replace')
    call calculation
    call grading
    call ranking_hundred_mark
    call ranking_hierarchy
    call output

    end program HW3_0711239

    subroutine calculation  !用於決定ub和allocate矩陣大小
    use datas
    implicit none
    integer :: mes,temp
    do
        read(10,*,iostat=mes)temp
        if(mes<0) exit
        ub=ub+1
    end do
    rewind(10)              !使read從頭開始讀入 方便接下來的資料輸入
    allocate (AID(lb:ub),ACH(lb:ub),AMA(lb:ub),AEN(lb:ub),AHI(lb:ub),AGE(lb:ub))
    allocate (score(lb:ub),grade(lb:ub))
  
    end subroutine
 
    
    subroutine grading      !計算每個人的百分制分數和等級制分數
    use datas
    implicit none
    integer :: ID,CH,MA,EN,HI,GE
    real,external :: point
    S_sum=0                 !開始前先歸零統計用變數
    G_sum=0
    S_pass_sum=0
    G_pass_sum=0
    S=0
    G=0
    
    do i=lb,ub,1
        read(10,*)ID,CH,MA,EN,HI,GE
        score(i) = (CH*4+MA*4+EN*2+HI*2+GE)/13.0
        grade(i) = (point(CH)*4+point(MA)*4+point(EN)*2+point(HI)*2+point(GE))/13
        S_sum=S_sum+score(i)
        G_sum=G_sum+grade(i)
        
        AID(i)=ID           !資料輸入陣列
        ACH(i)=CH
        AMA(i)=MA
        AEN(i)=EN
        AHI(i)=HI
        AGE(i)=GE

        
        if(score(i)>=90.and.score(i)<=100) then     !判斷百分制各分數區間之人數及計算及格者總分
            S(100)=S(100)+1
            S_pass_sum=S_pass_sum+score(i)
        else if(score(i)>=80.and.score(i)<90) then
            S(90)=S(90)+1
            S_pass_sum=S_pass_sum+score(i)
        else if(score(i)>=70.and.score(i)<80) then
            S(80)=S(80)+1
            S_pass_sum=S_pass_sum+score(i)
        else if(score(i)>=60.and.score(i)<70) then
            S(70)=S(70)+1
            S_pass_sum=S_pass_sum+score(i)
        else if(score(i)>=50.and.score(i)<60) then
            S(60)=S(60)+1
        else if(score(i)>=0.and.score(i)<50) then
            S(50)=S(50)+1
        end if
        
        if(grade(i)>=4.1.and.grade(i)<=4.3) then    !判斷等級制各分數區間之人數及計算及格者總分
            G(43)=G(43)+1
            G_pass_sum=G_pass_sum+grade(i)
        else if(grade(i)>=3.7.and.grade(i)<4.1) then
            G(41)=G(41)+1
            G_pass_sum=G_pass_sum+grade(i)
        else if(grade(i)>=2.7.and.grade(i)<3.7) then
            G(37)=G(37)+1
            G_pass_sum=G_pass_sum+grade(i)
        else if(grade(i)>=1.7.and.grade(i)<2.7) then
            G(27)=G(27)+1
            G_pass_sum=G_pass_sum+grade(i)
        else if(grade(i)>=1.0.and.grade(i)<1.7) then
            G(17)=G(17)+1
        else if(grade(i)>=0.0.and.grade(i)<1.0) then
            G(10)=G(10)+1
        end if
        
    end do
    
    S_pass=S(100)+S(90)+S(80)+S(70)         !計算及格者人數
    G_pass=G(43)+G(41)+G(37)+G(27)
    S_pass_avg=S_pass_sum/S_pass            !計算及格者平均
    G_pass_avg=G_pass_sum/G_pass
    S_avg=S_sum/ub                          !計算全班平均
    G_avg=G_sum/ub
    
    end subroutine
    
    subroutine ranking_hundred_mark     !將人數以百分制分數由大至小排名
    use datas
    implicit none
    real :: temp        !暫存變數 數值交換時先儲存於此以免數值消失
    
    allocate(s_temp(lb:ub),r_temp(lb:ub))
    s_temp=score
    r_temp=[(i,i=1,ub)]
    
    do i=1,ub-1         !決定每一次的I項
        do j=i+1,ub
            if(s_temp(j)>s_temp(i)) then    !固定I時，I項後若有任何一項>I項則和I像互換位置，即：將較大者向前移，結果為將數值由大到小排列
                temp=s_temp(i)
                s_temp(i)=s_temp(j)
                s_temp(j)=temp
                temp=r_temp(i)
                r_temp(i)=r_temp(j)
                r_temp(j)=temp
            end if
        end do
    end do
    
    end subroutine
    
    subroutine ranking_hierarchy        !將人數以等級制分數由大至小排名(內容邏輯同ranking_hundred_mark)
    use datas
    implicit none
    real :: temp        !暫存變數 數值交換時先儲存於此以免數值消失
    
    allocate(g_temp(lb:ub),k_temp(lb:ub))
    g_temp=grade
    k_temp=[(i,i=1,ub)]
    
    do i=1,ub-1
        do j=i+1,ub
            if(g_temp(j)>g_temp(i)) then
                temp=g_temp(i)
                g_temp(i)=g_temp(j)
                g_temp(j)=temp
                temp=k_temp(i)
                k_temp(i)=k_temp(j)
                k_temp(j)=temp
            end if
        end do
    end do
    
    
    end subroutine
    
    subroutine output       !用以輸出資料
    use datas
    implicit none
    write(11,'(T2,A)')'number Chinese Math English History Geography 百分制平均 等級制平均'
    
    do j=lb,ub,1        !一次輸出所有學生之學號、各科分數和平均
        write(11,'(T2,I6,2x,I3,4x,I3,3x,I3,5x,I3,6x,I3,7x,F5.2,6x,F4.2)')AID(j),ACH(j),AMA(j),AEN(j),AHI(j),AGE(j),score(j),grade(j)
    end do
    
    write(11,'(/T2,A,I3/T2,A,I3/T2,A,I3)')'全班人數:',ub,'百分制及格人數:',S_pass,'等級制及格人數:',G_pass    !輸出總人數，兩種算分方式之及格人數
    write(11,'(/T2,A,F6.2/T2,A,F6.2)')'百分制全班平均:  ',S_avg,'百分制及格者平均:',S_pass_avg                !輸出兩種算分方式之全班平均和及格者平均
    write(11,'(/T2,A,F6.2/T2,A,F6.2)')'等級制全班平均:  ',G_avg,'等級制及格者平均:',G_pass_avg
    write(11,'(/T2,A,2x,5(I6,2x))')'百分制前五名:',AID(r_temp(1)),AID(r_temp(2)),AID(r_temp(3)),AID(r_temp(4)),AID(r_temp(5)) !輸出兩種算分方式之前五名
    write(11,'(T2,A,2x,5(I6,2x))')'等級制前五名:',AID(k_temp(1)),AID(k_temp(2)),AID(k_temp(3)),AID(k_temp(4)),AID(k_temp(5))    
    write(11,'(/T2,2(A,3x)/T2,6(A,3x))')'百分制平均','各區間人數','100~90','90~80','80~70','70~60','60~50','50~0' !輸出兩種算分方式之各區間人數
    write(11,'(T4,6(I2,6x))')S(100),S(90),S(80),S(70),S(60),S(50)
    write(11,'(/T2,2(A,3x)/T2,6(A,3x))')'等級制平均','各區間人數','4.3~4.1','4.1~3.7','3.7~2.7','2.7~1.7','1.7~1.0','1.0~0'
    write(11,'(T4,4(I2,8x),I2,7x,I2)')G(43),G(41),G(37),G(27),G(17),G(10)
    
    end subroutine
    
    real function point(G)  !用以將百分制分數轉換為等級制積分
    implicit none
    integer :: G
    select case (G)         !判斷讀入的分數(G)處於哪個區間，再得出該科獲得的積分(point)
        case(93:100)
            point=4.3
        case(85:92)
            point=4.0
        case(80:84)
            point=3.7
        case(77:79)
            point=3.3
        case(73:76)
            point=3.0
        case(70:72)
            point=2.7
        case(67:69)
            point=2.3
        case(63:66)
            point=2.0
        case(60:62)
            point=1.7
        case(50:59)
            point=1.0
        case(0:49)
            point=0.0
    end select
    
    end function