    module datas
    implicit none
    save
    
    integer,allocatable :: id(:),sa(:),over16(:),lower12(:),S_rank(:) !�Ǹ��B�ӧO�ǥͥX�u���ơB�W�L16�B�C��12���X�u���ǥͩM�ǥͥX�u�v�ƦW(�Ѥj��p)
    integer :: wa(1:18),w_over90(1:18),w_lower60(1:18),W_rank(1:18)   !1~18�g���X�u�H�ơB�W�L9���B�C��6���X�u�v���P���M�P���X�u�v�ƦW(�Ѥj��p)
    integer,parameter :: lb=1    !�Ĥ@���פ�lower bound
    integer :: ub,l,m,n,o        !�Ĥ@���פ� upper bound�Al��X�u���ƶW�L16�����ǥͼƶq�Am��C��12�����ǥͼƶq�An��X�u�H�ƶW�L9�����P���ƶq�Ao��C��6�����P���ƶq
    real,allocatable :: S_att(:) !�C�Ӿǥͤ��X�u�v
    real :: W_att(1:18)          !�U�g���X�u�v

    
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

    subroutine ub_determine !�Ω�P�_�`�ǥͤH�ơA�åtub=�`�ǥͤH��
    use datas
    implicit none
    integer :: mes,temp
    ub=0
    do  
        read(10,*,iostat=mes)temp
        if(mes<0) exit 
        ub=ub+1
    end do
    
    rewind(10)  !�Ϲ�i/o number=10��Ū�g��m�^��}�Y
    
    end subroutine
    
    subroutine calculation
    use datas
    implicit none
    integer :: att(1:18)    !�C�Ӷg���O�_�X�u
    integer :: i,j,k
    allocate (sa(lb:ub),id(lb:ub),S_att(lb:ub),over16(lb:ub),lower12(lb:ub))    !�]�w�j�p�M�ǥ��`�ƬۦP���}�C�j�p
    wa=0    !�C�g���X�u�v�q0�}�l���W�[
    l=0     !l m n o�ҬO�O�ƥΡA�q1�}�l�ƬG���k�s
    m=0
    n=0
    o=0

    do  i=lb,ub
        read(10,*)id(i),att(1:18)   !Ū�J�@����
        sa(i)=0
        do j=1,18
            sa(i)=sa(i)+att(j)      !�p��C�@�Ӿǥͤ��X�u����(=�C�@�g�O�_�X�u�ۥ[)
        end do
        
        do k=1,18                   
            wa(k)=wa(k)+att(k)      !�CŪ�J�@��(1�H18�g)���X�u���ƫK�N����O�[�J�C�@�g�X�u���`����
        end do   
        
        S_att(i)=sa(i)/18.0*100     !�p��C�Ӿǥͤ��X�u�v
        
        if(sa(i)>=16) then          !�P�_�o�ӾǥͥX�u���ƬO�_>=16��<=12
            l=l+1
            over16(l)=id(i)            
        else if(sa(i)<=12) then
            m=m+1
            lower12(m)=id(i)            
        end if
                   
    end do
    
    do i=1,18                       !�p��U�g�X�u�v�çP�_�U�g�X�u�v�O�_�W�L9���ΧC��6��
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
    
    subroutine ranking_student      !�ƦW�ǥͥX�u�v��
    use datas
    implicit none
    integer,allocatable :: s_temp(:)    !�ڭ̻ݭn���ʸ�Ʀ�m�ӱƦW�A�����ʭ��Ƥ��Ӧn�A�G�]�w�@�Ȧs�}�C�ӷ�@���ƶi�沾��
    integer :: i,j,temp                 !temp���Ȧs�ܼ� �ƭȥ洫�ɥ��x�s�󦹥H�K�ƭȮ���
    allocate(s_temp(lb:ub),S_rank(lb:ub))   !�]�w�o��Ӱ}�C�j�p�M�ǥ��`�ƬۦP
    s_temp=S_att
    S_rank=[(i,i=1,ub)]
    do i=1,ub-1           !���M�w�C�@������ɪ�I��
        do j=i+1,ub
            if(s_temp(j)>s_temp(i)) then    !�T�wI�ȡA�YI�ȫ����@��(J)��I���٤j�K�洫��m�A���G���N�X�u�v�Ѥj��p�ƦW
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
    
    subroutine ranking_week !�ƦW�U�g�X�u�v�� ��z�Pranking_student
    use datas
    implicit none
    integer :: i,j,temp     !temp���Ȧs�ܼ� �ƭȥ洫�ɥ��x�s�󦹥H�K�ƭȮ���
    integer :: w_temp(1:18) !�ڭ̻ݭn���ʸ�Ʀ�m�ӱƦW�A�����ʭ��Ƥ��Ӧn�A�G�]�w�@�Ȧs�}�C�ӷ�@���ƶi�沾��
    w_temp=w_att
    W_rank=[(i,i=1,18)]
    do i=1,17               !���M�w�C�@������ɪ�I��
        do j=i+1,18 
            if(w_temp(j)>w_temp(i)) then    !�T�wI�ȡA�YI�ȫ����@��(J)��I���٤j�K�洫��m�A���G���N�X�u�v�Ѥj��p�ƦW
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
    !�}�l��X�Ĥ@��
    write(11,'(/T2,A)')'�Ǹ�    �X�u����   �X�u�v'    
    do i=lb,ub      !�ϥ�do�j��Ӥ@�@��X�}�C�������
        write(11,'(T2,I6,5x,I2,5x,F6.2,A)')id(i),sa(i),S_att(i),'%'
    end do
    write(11,'(/T2,A,2x,(T28,5(I6,2x)))')'�X�u�v���C��16�g���ǥ�:',over16(lb:l)
    write(11,'(T2,A,2x,(T28,5(I6,2x)))')'�X�u�v���W�L12�g���ǥ�:',lower12(lb:m)
    write(11,'(/T2,A,I9)')'�X�u�v�̰����ǥ�:',id(S_rank(1))
    write(11,'(T2,A,I9)')'�X�u�v�̧C���ǥ�:',id(S_rank(ub))
    !�}�l��X�ĤG��
    write(11,'(///T2,A,I3)')'�ǥͤH��:',ub
    write(11,'(/T2,A)')'�P��  �X�u�H��  �X�u�v'
    do i=1,18       !�ϥ�do�j��Ӥ@�@��X�}�C�������
        write(11,'(T2,I2,6x,I2,5x,F6.2,A)')i,wa(i),W_att(i),'%'
    end do
    
    write(11,'(/T2,A,(T24,5(I2,2x)))')'�X�u�v���p��9�����P��:',w_over90(1:n)
    write(11,'(T2,A,(T24,5(I2,2x)))')'�X�u�v���W�L6�����P��:',w_lower60(1:o)
    write(11,'(/T2,A,2x,I3)')'�X�u�v�̰����P��:',W_rank(1)
    write(11,'(T2,A,2x,I3)')'�X�u�v�̧C���P��:',W_rank(18)
    
    end subroutine