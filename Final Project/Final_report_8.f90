    module data
    implicit none
    save
    real,allocatable :: BS(:),FS(:),height(:),fixed_height(:)   !後視、前視、測量高程、修正高程
    real,allocatable ::sum_fixed_height(:),avg_fixed_height(:)  !往返測高程和、往返測高程平均
    real :: ABS_height,sigma_BS,sigma_FS,fix,closed             !已知高程、ΣB、ΣF、閉合差
    integer :: n=0
    end module

    program Final_report_8
    use data
    implicit none
    open(10,file='input_forward.txt',status='old')
    open(12,file='input_backward.txt',status='old')
    open(11,file='output.txt',status='replace')
    call allocate_forward   !計算資料筆數
    call input_forward      !計算往測資料
    call output_forward     !輸出往測資料
    call allocate_backward  !計算資料筆數
    call input_backward     !計算返測資料
    call output_backward    !輸出返測資料
    call output_final       !輸出站點最終高程

    end program Final_report_8

    subroutine allocate_forward !為可變陣列設定大小
    use data
    implicit none
    integer :: temp,mes
    do
        read(10,*,iostat=mes)temp
        if(mes<0) exit
        n=n+1
    end do
    n=n-2
    allocate(BS(1:n),FS(1:n),height(1:n+1),fixed_height(1:n+1))
    allocate(sum_fixed_height(1:n+1),avg_fixed_height(1:n+1))
    rewind 10       !使接下來可以從頭讀入input
    
    end subroutine
    
    subroutine input_forward
    use data
    implicit none
    integer :: i
    character :: temp1,temp2,temp3

    read(10,*)temp1,temp2,temp3,ABS_height  !忽略前面文字並讀入已知高程
    read(10,*)BS(1)
    sigma_BS=BS(1)
    sigma_FS=0
    do i=2,n
        read(10,*)BS(i),FS(i-1)             !讀入每一筆的前視、後視並加總
        sigma_BS=sigma_BS+BS(i)
        sigma_FS=sigma_FS+FS(i-1)
    end do
    read(10,*)FS(n)
    sigma_FS=sigma_FS+FS(n)
    closed=sigma_FS-sigma_BS        !計算閉合差
    fix=-(sigma_BS-sigma_FS)/n      !計算配賦值
    
    height(1)=ABS_height
    fixed_height(1)=ABS_height  
    do i=2,n+1
        height(i)=height(i-1)+(BS(i-1)-FS(i-1)) !計算修正高程
        fixed_height(i)=height(i)+fix*(i-1)   
    end do
    sum_fixed_height=fixed_height       
    !↑先將往測的資料加入總和中，因返測用的是同一陣列，避免資料被覆蓋消失
    end subroutine
    
    subroutine output_forward
    use data
    implicit none
    integer :: i
    write(11,'(T2,A)')'單位皆為公尺(m)'
    write(11,'(T2,A)')'---------------------開始往測--------------------------'
    write(11,'(T2,A)')'站點   後視B.S.  前視F.S.  測量高程   配賦值   修正高程'
    write(11,'(T2,A,1x,F8.3,15x,F6.3,4x,A,5x,F6.3)')'EA02',BS(1),ABS_height,'0.000',height(1)
    do i=2,n
        write(11,'(T2,I3,2x,F8.3,2x,F8.3,2x,F9.3,F9.3,2x,F9.3)')i,BS(i),FS(i-1),height(i),fix*(i-1),fixed_height(i)
    end do
    write(11,'(T2,A,11x,F8.3,5x,F6.3,F9.3,5x,F6.3)')'EA02',FS(n),height(n+1),fix*n,fixed_height(n+1)
    write(11,'(/T2,A,F6.3,2x,A,F6.3,5x/T2,A,F6.3,A,A,F6.3)')'ΣB=',sigma_BS,'ΣF=',sigma_FS,'改正數 =(ΣF-ΣB)=',closed,' , ','配賦值= 改正數/站數=',fix
    
    end subroutine
    
    subroutine allocate_backward    !判斷返視之資料筆數
    use data
    implicit none
    integer :: temp,mes
    n=0
    do
        read(12,*,iostat=mes)temp
        if(mes<0) exit
        n=n+1
    end do
    n=n-1
    rewind 12
    end subroutine
    
    subroutine input_backward
    use data
    implicit none
    integer :: i

    read(12,*)BS(1)
    sigma_BS=BS(1)
    sigma_FS=0
    do i=2,n
        read(12,*)BS(i),FS(i-1)     !讀入每一筆的前視、後視並加總
        sigma_BS=sigma_BS+BS(i)
        sigma_FS=sigma_FS+FS(i-1)
    end do
    read(12,*)FS(n)
    sigma_FS=sigma_FS+FS(n)
    closed=sigma_FS-sigma_BS        !計算閉合差
    fix=-(sigma_BS-sigma_FS)/n      !計算配賦值
    
    height(1)=ABS_height
    fixed_height(1)=ABS_height
    do i=2,n+1          !計算修正高程
        height(i)=height(i-1)+(BS(i-1)-FS(i-1))
        fixed_height(i)=height(i)+fix*(i-1)   
    end do
    
    do i=2,n    !將返測之資料加總，要注意返測之I代表第(N+2-I)站點
        sum_fixed_height(i)=sum_fixed_height(i)+fixed_height(n+2-i)        
    end do
    avg_fixed_height=sum_fixed_height/2 !計算平均高程
    
    end subroutine
    
    subroutine output_backward
    use data
    implicit none
    integer :: i
    write(11,'(/T2,A)')'---------------------開始返測--------------------------'
    write(11,'(T2,A)')'站點   後視B.S.  前視F.S.  測量高程   配賦值   修正高程'
    write(11,'(T2,A,1x,F8.3,15x,F6.3,14x,F6.3)')'EA02',BS(1),ABS_height,height(1)
    
    do i=2,n
        write(11,'(T2,I3,2x,F8.3,2x,F8.3,2x,F9.3,F9.3,2x,F9.3)')n+2-i,BS(i),FS(i-1),height(i),fix*(i-1),fixed_height(i)
    end do    
    write(11,'(T2,A,11x,F8.3,5x,F6.3,F9.3,5x,F6.3)')'EA02',FS(n),height(n+1),fix*n,fixed_height(n+1)
    write(11,'(/T2,A,F6.3,2x,A,F6.3,5x/T2,A,F6.3,A,A,F6.4)')'ΣB=',sigma_BS,'ΣF=',sigma_FS,'改正數 =(ΣF-ΣB)=',closed,' , ','配賦值= 改正數/站數=',fix
    
    end subroutine
    
    subroutine output_final !輸出兩筆資料整合後之平均高程
    use data
    implicit none
    integer :: i
    write(11,'(/T2,A)')'-------------------輸出平均結果------------------------'
    write(11,*)'站點     平均高程'
    write(11,'(T2,A,5x,F7.3)')'EA02',ABS_height
    do i=2,n
        write(11,'(T2,I3,6x,F7.3)')i,avg_fixed_height(i)
    end do
       
    
    end subroutine
    
    