    module data
    implicit none
    save
    real,allocatable :: BS(:),FS(:),height(:),fixed_height(:)   !����B�e���B���q���{�B�ץ����{
    real,allocatable ::sum_fixed_height(:),avg_fixed_height(:)  !��������{�M�B��������{����
    real :: ABS_height,sigma_BS,sigma_FS,fix,closed             !�w�����{�B�UB�B�UF�B���X�t
    integer :: n=0
    end module

    program Final_report_8
    use data
    implicit none
    open(10,file='input_forward.txt',status='old')
    open(12,file='input_backward.txt',status='old')
    open(11,file='output.txt',status='replace')
    call allocate_forward   !�p���Ƶ���
    call input_forward      !�p�⩹�����
    call output_forward     !��X�������
    call allocate_backward  !�p���Ƶ���
    call input_backward     !�p�������
    call output_backward    !��X������
    call output_final       !��X���I�̲װ��{

    end program Final_report_8

    subroutine allocate_forward !���i�ܰ}�C�]�w�j�p
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
    rewind 10       !�ϱ��U�ӥi�H�q�YŪ�Jinput
    
    end subroutine
    
    subroutine input_forward
    use data
    implicit none
    integer :: i
    character :: temp1,temp2,temp3

    read(10,*)temp1,temp2,temp3,ABS_height  !�����e����r��Ū�J�w�����{
    read(10,*)BS(1)
    sigma_BS=BS(1)
    sigma_FS=0
    do i=2,n
        read(10,*)BS(i),FS(i-1)             !Ū�J�C�@�����e���B����å[�`
        sigma_BS=sigma_BS+BS(i)
        sigma_FS=sigma_FS+FS(i-1)
    end do
    read(10,*)FS(n)
    sigma_FS=sigma_FS+FS(n)
    closed=sigma_FS-sigma_BS        !�p�ⳬ�X�t
    fix=-(sigma_BS-sigma_FS)/n      !�p��t���
    
    height(1)=ABS_height
    fixed_height(1)=ABS_height  
    do i=2,n+1
        height(i)=height(i-1)+(BS(i-1)-FS(i-1)) !�p��ץ����{
        fixed_height(i)=height(i)+fix*(i-1)   
    end do
    sum_fixed_height=fixed_height       
    !�����N��������ƥ[�J�`�M���A�]����Ϊ��O�P�@�}�C�A�קK��ƳQ�л\����
    end subroutine
    
    subroutine output_forward
    use data
    implicit none
    integer :: i
    write(11,'(T2,A)')'���Ҭ�����(m)'
    write(11,'(T2,A)')'---------------------�}�l����--------------------------'
    write(11,'(T2,A)')'���I   ���B.S.  �e��F.S.  ���q���{   �t���   �ץ����{'
    write(11,'(T2,A,1x,F8.3,15x,F6.3,4x,A,5x,F6.3)')'EA02',BS(1),ABS_height,'0.000',height(1)
    do i=2,n
        write(11,'(T2,I3,2x,F8.3,2x,F8.3,2x,F9.3,F9.3,2x,F9.3)')i,BS(i),FS(i-1),height(i),fix*(i-1),fixed_height(i)
    end do
    write(11,'(T2,A,11x,F8.3,5x,F6.3,F9.3,5x,F6.3)')'EA02',FS(n),height(n+1),fix*n,fixed_height(n+1)
    write(11,'(/T2,A,F6.3,2x,A,F6.3,5x/T2,A,F6.3,A,A,F6.3)')'�UB=',sigma_BS,'�UF=',sigma_FS,'�勵�� =(�UF-�UB)=',closed,' , ','�t���= �勵��/����=',fix
    
    end subroutine
    
    subroutine allocate_backward    !�P�_�������Ƶ���
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
        read(12,*)BS(i),FS(i-1)     !Ū�J�C�@�����e���B����å[�`
        sigma_BS=sigma_BS+BS(i)
        sigma_FS=sigma_FS+FS(i-1)
    end do
    read(12,*)FS(n)
    sigma_FS=sigma_FS+FS(n)
    closed=sigma_FS-sigma_BS        !�p�ⳬ�X�t
    fix=-(sigma_BS-sigma_FS)/n      !�p��t���
    
    height(1)=ABS_height
    fixed_height(1)=ABS_height
    do i=2,n+1          !�p��ץ����{
        height(i)=height(i-1)+(BS(i-1)-FS(i-1))
        fixed_height(i)=height(i)+fix*(i-1)   
    end do
    
    do i=2,n    !�N�������ƥ[�`�A�n�`�N�����I�N���(N+2-I)���I
        sum_fixed_height(i)=sum_fixed_height(i)+fixed_height(n+2-i)        
    end do
    avg_fixed_height=sum_fixed_height/2 !�p�⥭�����{
    
    end subroutine
    
    subroutine output_backward
    use data
    implicit none
    integer :: i
    write(11,'(/T2,A)')'---------------------�}�l���--------------------------'
    write(11,'(T2,A)')'���I   ���B.S.  �e��F.S.  ���q���{   �t���   �ץ����{'
    write(11,'(T2,A,1x,F8.3,15x,F6.3,14x,F6.3)')'EA02',BS(1),ABS_height,height(1)
    
    do i=2,n
        write(11,'(T2,I3,2x,F8.3,2x,F8.3,2x,F9.3,F9.3,2x,F9.3)')n+2-i,BS(i),FS(i-1),height(i),fix*(i-1),fixed_height(i)
    end do    
    write(11,'(T2,A,11x,F8.3,5x,F6.3,F9.3,5x,F6.3)')'EA02',FS(n),height(n+1),fix*n,fixed_height(n+1)
    write(11,'(/T2,A,F6.3,2x,A,F6.3,5x/T2,A,F6.3,A,A,F6.4)')'�UB=',sigma_BS,'�UF=',sigma_FS,'�勵�� =(�UF-�UB)=',closed,' , ','�t���= �勵��/����=',fix
    
    end subroutine
    
    subroutine output_final !��X�ⵧ��ƾ�X�ᤧ�������{
    use data
    implicit none
    integer :: i
    write(11,'(/T2,A)')'-------------------��X�������G------------------------'
    write(11,*)'���I     �������{'
    write(11,'(T2,A,5x,F7.3)')'EA02',ABS_height
    do i=2,n
        write(11,'(T2,I3,6x,F7.3)')i,avg_fixed_height(i)
    end do
       
    
    end subroutine
    
    