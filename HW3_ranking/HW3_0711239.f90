    Module datas
    implicit none
    save
    
    real,allocatable :: score(:),grade(:)       !κだだ计㎝单だ计
    integer,allocatable :: AID(:),ACH(:),AMA(:),AEN(:),AHI(:),AGE(:)    !ID㎝きσぇ皚(A=Array)
    real,allocatable :: s_temp(:),r_temp(:)  !逼κだだ计ノ
    real,allocatable :: g_temp(:),k_temp(:)  !ノ逼单 (Moduleい琌逼㎝块ㄢぃ捌祘Α)
    integer :: i=0,j=0          !ノㄓ∕﹚do磅︽Ω计┮ノ
    integer :: S_pass,S(50:100) !だκだの计 100~90だ计... 50だ计
    integer :: G_pass,G(10:43)  !だ单の计 4.3~4.1だ计...1.0だ计
    integer ,parameter :: lb=1  !┮Τ皚材蝴ぇlower bound
    integer :: ub=0             !┮Τ痻皚材蝴ぇupper bound(﹟ゼ∕﹚珿砞0)
    real :: S_sum,S_avg,G_sum,G_avg !玡ㄢκだ羆だ㎝キА ㄢ玥单
    real :: S_pass_sum,S_pass_avg,G_pass_sum,G_pass_avg !玡ㄢκだの羆だ㎝キА ㄢ玥单
    
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

    subroutine calculation  !ノ∕﹚ub㎝allocate痻皚
    use datas
    implicit none
    integer :: mes,temp
    do
        read(10,*,iostat=mes)temp
        if(mes<0) exit
        ub=ub+1
    end do
    rewind(10)              !ㄏread眖繷秨﹍弄 よ獽钡ㄓ戈块
    allocate (AID(lb:ub),ACH(lb:ub),AMA(lb:ub),AEN(lb:ub),AHI(lb:ub),AGE(lb:ub))
    allocate (score(lb:ub),grade(lb:ub))
  
    end subroutine
 
    
    subroutine grading      !璸衡–κだだ计㎝单だ计
    use datas
    implicit none
    integer :: ID,CH,MA,EN,HI,GE
    real,external :: point
    S_sum=0                 !秨﹍玡耴箂参璸ノ跑计
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
        
        AID(i)=ID           !戈块皚
        ACH(i)=CH
        AMA(i)=MA
        AEN(i)=EN
        AHI(i)=HI
        AGE(i)=GE

        
        if(score(i)>=90.and.score(i)<=100) then     !耞κだだ计跋丁ぇ计の璸衡の羆だ
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
        
        if(grade(i)>=4.1.and.grade(i)<=4.3) then    !耞单だ计跋丁ぇ计の璸衡の羆だ
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
    
    S_pass=S(100)+S(90)+S(80)+S(70)         !璸衡の计
    G_pass=G(43)+G(41)+G(37)+G(27)
    S_pass_avg=S_pass_sum/S_pass            !璸衡のキА
    G_pass_avg=G_pass_sum/G_pass
    S_avg=S_sum/ub                          !璸衡痁キА
    G_avg=G_sum/ub
    
    end subroutine
    
    subroutine ranking_hundred_mark     !盢计κだだ计パ逼
    use datas
    implicit none
    real :: temp        !既跑计 计ユ传纗计ア
    
    allocate(s_temp(lb:ub),r_temp(lb:ub))
    s_temp=score
    r_temp=[(i,i=1,ub)]
    
    do i=1,ub-1         !∕﹚–ΩI兜
        do j=i+1,ub
            if(s_temp(j)>s_temp(i)) then    !㏕﹚II兜璝Τヴ兜>I兜玥㎝I钩が传竚盢耕玡簿挡狦盢计パ逼
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
    
    subroutine ranking_hierarchy        !盢计单だ计パ逼(ず甧呸胯ranking_hundred_mark)
    use datas
    implicit none
    real :: temp        !既跑计 计ユ传纗计ア
    
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
    
    subroutine output       !ノ块戈
    use datas
    implicit none
    write(11,'(T2,A)')'number Chinese Math English History Geography κだキА 单キА'
    
    do j=lb,ub,1        !Ω块┮Τ厩ネぇ厩腹だ计㎝キА
        write(11,'(T2,I6,2x,I3,4x,I3,3x,I3,5x,I3,6x,I3,7x,F5.2,6x,F4.2)')AID(j),ACH(j),AMA(j),AEN(j),AHI(j),AGE(j),score(j),grade(j)
    end do
    
    write(11,'(/T2,A,I3/T2,A,I3/T2,A,I3)')'痁计:',ub,'κだの计:',S_pass,'单の计:',G_pass    !块羆计ㄢ贺衡だよΑぇの计
    write(11,'(/T2,A,F6.2/T2,A,F6.2)')'κだ痁キА:  ',S_avg,'κだのキА:',S_pass_avg                !块ㄢ贺衡だよΑぇ痁キА㎝のキА
    write(11,'(/T2,A,F6.2/T2,A,F6.2)')'单痁キА:  ',G_avg,'单のキА:',G_pass_avg
    write(11,'(/T2,A,2x,5(I6,2x))')'κだ玡き:',AID(r_temp(1)),AID(r_temp(2)),AID(r_temp(3)),AID(r_temp(4)),AID(r_temp(5)) !块ㄢ贺衡だよΑぇ玡き
    write(11,'(T2,A,2x,5(I6,2x))')'单玡き:',AID(k_temp(1)),AID(k_temp(2)),AID(k_temp(3)),AID(k_temp(4)),AID(k_temp(5))    
    write(11,'(/T2,2(A,3x)/T2,6(A,3x))')'κだキА','跋丁计','100~90','90~80','80~70','70~60','60~50','50~0' !块ㄢ贺衡だよΑぇ跋丁计
    write(11,'(T4,6(I2,6x))')S(100),S(90),S(80),S(70),S(60),S(50)
    write(11,'(/T2,2(A,3x)/T2,6(A,3x))')'单キА','跋丁计','4.3~4.1','4.1~3.7','3.7~2.7','2.7~1.7','1.7~1.0','1.0~0'
    write(11,'(T4,4(I2,8x),I2,7x,I2)')G(43),G(41),G(37),G(27),G(17),G(10)
    
    end subroutine
    
    real function point(G)  !ノ盢κだだ计锣传单縩だ
    implicit none
    integer :: G
    select case (G)         !耞弄だ计(G)矪跋丁眔赣莉眔縩だ(point)
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