    module datas
    implicit none
    save
    
    integer,allocatable :: id(:),sa(:),over16(:),lower12(:),S_rank(:) !學號、個別學生出席次數、超過16、低於12次出席之學生和學生出席率排名(由大到小)
    integer :: wa(1:18),w_over90(1:18),w_lower60(1:18),W_rank(1:18)   !1~18週之出席人數、超過9成、低於6成出席率之周次和周次出席率排名(由大到小)
    integer,parameter :: lb=1    !第一維度之lower bound
    integer :: ub,l,m,n,o        !第一維度之 upper bound，l表出席次數超過16次之學生數量，m表低於12次之學生數量，n表出席人數超過9成之周次數量，o表低於6成之周次數量
    real,allocatable :: S_att(:) !每個學生之出席率
    real :: W_att(1:18)          !各週之出席率

    
    end module
    
    program HW4_0711239
    use datas
    implicit none
    
    open(10,file='attendance_input.txt',status='old')
    open(11,file='attendance_output.txt',status='replace')
    call ub_determine
    call calculation
    call ranking_student
    call ranking_week
    call output
    
    end program HW4_0711239

    subroutine ub_determine !用於判斷總學生人數，並另ub=總學生人數
    use datas
    implicit none
    integer :: mes,temp
    ub=0
    do  
        read(10,*,iostat=mes)temp
        if(mes<0) exit 
        ub=ub+1
    end do
    
    rewind(10)  !使對i/o number=10的讀寫位置回到開頭
    
    end subroutine
    
    subroutine calculation
    use datas
    implicit none
    integer :: att(1:18)    !每個週次是否出席
    integer :: i,j,k
    allocate (sa(lb:ub),id(lb:ub),S_att(lb:ub),over16(lb:ub),lower12(lb:ub))    !設定大小和學生總數相同的陣列大小
    wa=0    !每週的出席率從0開始往上加
    l=0     !l m n o皆是記數用，從1開始數故先歸零
    m=0
    n=0
    o=0

    do  i=lb,ub
        read(10,*)id(i),att(1:18)   !讀入一行資料
        sa(i)=0
        do j=1,18
            sa(i)=sa(i)+att(j)      !計算每一個學生之出席次數(=每一週是否出席相加)
        end do
        
        do k=1,18                   
            wa(k)=wa(k)+att(k)      !每讀入一行(1人18週)的出席次數便將其分別加入每一週出席的總次數
        end do   
        
        S_att(i)=sa(i)/18.0*100     !計算每個學生之出席率
        
        if(sa(i)>=16) then          !判斷這個學生出席次數是否>=16或<=12
            l=l+1
            over16(l)=id(i)            
        else if(sa(i)<=12) then
            m=m+1
            lower12(m)=id(i)            
        end if
                   
    end do
    
    do i=1,18                       !計算各週出席率並判斷各週出席率是否超過9成或低於6成
        W_att(i)=wa(i)/(ub*1.0)*100
        if(W_att(i)>=90.and.W_att(i)<=100) then
            n=n+1
            w_over90(n)=i
        else if(W_att(i)<=60.and.W_att(i)>=0) then
            o=o+1
            w_lower60(o)=i
        end if
    end do
    
    end subroutine
    
    subroutine ranking_student      !排名學生出席率用
    use datas
    implicit none
    integer,allocatable :: s_temp(:)    !我們需要移動資料位置來排名，但移動原資料不太好，故設定一暫存陣列來當作原資料進行移動
    integer :: i,j,temp                 !temp為暫存變數 數值交換時先儲存於此以免數值消失
    allocate(s_temp(lb:ub),S_rank(lb:ub))   !設定這兩個陣列大小和學生總數相同
    s_temp=S_att
    S_rank=[(i,i=1,ub)]
    do i=1,ub-1           !先決定每一次比較時的I值
        do j=i+1,ub
            if(s_temp(j)>s_temp(i)) then    !固定I值，若I值後任何一項(J)比I值還大便交換位置，結果為將出席率由大到小排名
                temp=s_temp(i)
                s_temp(i)=s_temp(j)
                s_temp(j)=temp
                
                temp=S_rank(i)
                S_rank(i)=S_rank(j)
                S_rank(j)=temp
            end if
        end do
    end do
     
    
    end subroutine
    
    subroutine ranking_week !排名各週出席率用 原理同ranking_student
    use datas
    implicit none
    integer :: i,j,temp     !temp為暫存變數 數值交換時先儲存於此以免數值消失
    integer :: w_temp(1:18) !我們需要移動資料位置來排名，但移動原資料不太好，故設定一暫存陣列來當作原資料進行移動
    w_temp=w_att
    W_rank=[(i,i=1,18)]
    do i=1,17               !先決定每一次比較時的I值
        do j=i+1,18 
            if(w_temp(j)>w_temp(i)) then    !固定I值，若I值後任何一項(J)比I值還大便交換位置，結果為將出席率由大到小排名
                temp=w_temp(i)
                w_temp(i)=w_temp(j)
                w_temp(j)=temp
                
                temp=W_rank(i)
                W_rank(i)=W_rank(j)
                W_rank(j)=temp
            end if
        end do
    end do
        
    end subroutine
    
    subroutine output
    use datas
    implicit none
    integer :: i
    !開始輸出第一頁
    write(11,'(/T2,A)')'學號    出席次數   出席率'    
    do i=lb,ub      !使用do迴圈來一一輸出陣列中的資料
        write(11,'(T2,I6,5x,I2,5x,F6.2,A)')id(i),sa(i),S_att(i),'%'
    end do
    write(11,'(/T2,A,2x,(T28,5(I6,2x)))')'出席率不低於16週之學生:',over16(lb:l)
    write(11,'(T2,A,2x,(T28,5(I6,2x)))')'出席率不超過12週之學生:',lower12(lb:m)
    write(11,'(/T2,A,I9)')'出席率最高之學生:',id(S_rank(1))
    write(11,'(T2,A,I9)')'出席率最低之學生:',id(S_rank(ub))
    !開始輸出第二頁
    write(11,'(///T2,A,I3)')'學生人數:',ub
    write(11,'(/T2,A)')'周次  出席人數  出席率'
    do i=1,18       !使用do迴圈來一一輸出陣列中的資料
        write(11,'(T2,I2,6x,I2,5x,F6.2,A)')i,wa(i),W_att(i),'%'
    end do
    
    write(11,'(/T2,A,(T24,5(I2,2x)))')'出席率不小於9成之周次:',w_over90(1:n)
    write(11,'(T2,A,(T24,5(I2,2x)))')'出席率不超過6成之周次:',w_lower60(1:o)
    write(11,'(/T2,A,2x,I3)')'出席率最高之周次:',W_rank(1)
    write(11,'(T2,A,2x,I3)')'出席率最低之周次:',W_rank(18)
    
    end subroutine