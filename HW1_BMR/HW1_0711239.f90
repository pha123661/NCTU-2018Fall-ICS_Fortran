    Module datas
    implicit none
    save
     
    character(len=1) :: gen         !�ʧO(gender)
    integer :: age , AI , BMR       !�~�֡B���ʫY�ƥN���ΰ�¦�N�²v(Basal metabolic rate)
    integer :: error,mes            !�޿�P�_���ܼ�
    real :: W , H ,aii              !�魫(weight)�B����(height)�B���ʫY��(activity index)
    integer :: num=0,num_m=0,num_f=0,error_g,error_ai !�p���`�H�ơA�`�k�k�H�ƩM��ؿ��~�H�ƥ�
    integer :: m1=0,m2=0,m3=0,m4=0,m5=0,m6=0   !�p��k��AI=1~6�ӧO�H�ƥ�
    integer :: f1=0,f2=0,f3=0,f4=0,f5=0,f6=0   !�p��k��AI=1~6�ӧO�H�ƥ�

    
    End module
    
    program HW1_0711239
    use datas
    implicit none
    
    open(10,file='BMR_input.txt',status='old' )       !�N���Ū�J�O����
    open(11,file='BMR_output.txt',status= 'replace' ) !�w�ƿ�X
    call calculation
    call sum

    end program HW1_0711239

    Subroutine calculation
    use datas
    implicit none
    write(11,'(T2,A,2X,A,2X,A,X,A,2X,A,2X,A,5X,A)')'no.','index','gender','age','weight','height','BMR'
    do
    
        read(10,*,iostat=mes) AI,gen,age,W,H !��J���
        if(mes<0) exit
        error=0                 !��error�k�s�H�K�P�_�O�_���~
        num=num+1               !�p��O�ĴX�����
        aii=0
        select case(gen)        !�P�_�ʧO
        case('M')
            select case (age)   !�A�ӧP�_�k�ʦ~�ּh
            case(:30)
                select case(AI) !�P�_�~�֫�@�@�P�_���ʫY��
                case(:0)
                    error_ai=error_ai+1
                    error=2
                case(1)
                    aii=1
                    m1=m1+1
                case(2)
                    aii=1.2
                    m2=m2+1
                case(3)
                    aii=1.375
                    m3=m3+1
                case(4)
                    aii=1.55
                    m4=m4+1
                case(5)
                    aii=1.725
                    m5=m5+1
                case(6)
                    aii=2
                    m6=m6+1
                case(7:)
                    error_ai=error_ai+1
                    error=2
                end select
                BMR=(66+13.7*W+5*H-6.8*age)*aii
            case(31:55)
                select case(AI) !�P�_�~�֫�@�@�P�_���ʫY��
                case(:0)
                    error_ai=error_ai+1
                    error=2
                case(1)
                    aii=1
                    m1=m1+1
                case(2)
                    aii=1.2
                    m2=m2+1
                case(3)
                    aii=1.375
                    m3=m3+1
                case(4)
                    aii=1.55
                    m4=m4+1
                case(5)
                    aii=1.725
                    m5=m5+1
                case(6)
                    aii=2
                    m6=m6+1
                case(7:)
                    error_ai=error_ai+1
                    error=2
                end select
                BMR=(63.5+12.7*W+4.2*H-6.9*age)*aii
            case(56:)
                select case(AI) !�P�_�~�֫�@�@�P�_���ʫY��
                case(:0)
                    error_ai=error_ai+1
                    error=2
                case(1)
                    aii=1
                    m1=m1+1
                case(2)
                    aii=1.2
                    m2=m2+1
                case(3)
                    aii=1.375
                    m3=m3+1
                case(4)
                    aii=1.55
                    m4=m4+1
                case(5)
                    aii=1.725
                    m5=m5+1
                case(6)
                    aii=2
                    m6=m6+1
                case(7:)
                    error_ai=error_ai+1
                    error=2
                end select
                
                BMR=(60.5+11.4*W+3.6*H-7.1*age)*aii
            end select
            
        case('F')
            num_f=num_f+1
            select case (age)   !�A�ӧP�_�k�ʦ~�ּh
                
            case(:30)
                
                select case(AI) !�P�_�~�֫�@�@�P�_���ʫY��
                case(:0)
                    error_ai=error_ai+1
                    error=2
                case(1)
                    aii=1
                    f1=f1+1
                case(2)
                    aii=1.2
                    f2=f2+1
                case(3)
                    aii=1.375
                    f3=f3+1
                case(4)
                    aii=1.55
                    f4=f4+1
                case(5)
                    aii=1.725
                    f5=f5+1
                case(6)
                    aii=2
                    f6=f6+1
                case(7:)
                    error_ai=error_ai+1
                    error=2
                end select
                
            BMR=(655+9.6*W+1.7*H-4.7*age)*aii
                
            case(31:55)
                
                select case(AI) !�P�_�~�֫�@�@�P�_���ʫY��
                case(:0)
                    error_ai=error_ai+1
                    error=2
                case(1)
                    aii=1
                    f1=f1+1
                case(2)
                    aii=1.2
                    f2=f2+1
                case(3)
                    aii=1.375
                    f3=f3+1
                case(4)
                    aii=1.55
                    f4=f4+1
                case(5)
                    aii=1.725
                    f5=f5+1
                case(6)
                    aii=2
                    f6=f6+1
                case(7:)
                    error_ai=error_ai+1
                    error=2
                end select
                
            BMR=(635+8.6*W+1.5*H-4.8*age)*aii
                
            case(56:)
                
                select case(AI) !�P�_�~�֫�@�@�P�_���ʫY��
                case(:0)
                    error_ai=error_ai+1
                    error=2
                case(1)
                    aii=1
                    f1=f1+1
                case(2)
                    aii=1.2
                    f2=f2+1
                case(3)
                    aii=1.375
                    f3=f3+1
                case(4)
                    aii=1.55
                    f4=f4+1
                case(5)
                    aii=1.725
                    f5=f5+1
                case(6)
                    aii=2
                    f6=f6+1
                case(7:)
                    error_ai=error_ai+1
                    error=2
                end select
                
            BMR=(615+7.2*W+1.1*H-4.9*age)*aii
            
            end select
            
        case default
             error_g=error_g+1
             error = 1           !�����ʧO����M�]����F�ɡAerror=1 ���ʧO���~
             
        end select
        
        
        select case(error)      !�P�_��ƥ��T�άO�_��������~�ÿ�X
        case(0)
            write(11,'(T2,I2,4x,I2,2x,A5,2x,I5,2x,F5.1,3x,F5.1,3x,I7)')num,AI,gen,age,W,H,BMR
        case(1)
            write(11,'(T2,I2,4x,I2,2x,A5,2x,I5,2x,F5.1,3x,F5.1,2x,A)')num,AI,gen,age,W,H,'Wrong gender!'
        case(2)
            write(11,'(T2,I2,4x,I2,2x,A5,2x,I5,2x,F5.1,3x,F5.1,2x,A)')num,AI,gen,age,W,H,'Wrong index!'
        end select
        
    end do

    End subroutine
    
    Subroutine sum      !�p��U���`�M�ÿ�X
    use datas
    implicit none
    
    num_m=m1+m2+m3+m4+m5+m6
    num_f=f1+f2+f3+f4+f5+f6      
    write(11,'(T2,A,I3)')'Total = ',num
    write(11,'(T2,A,I3,A,2x,A,I3,A,2x,A,I2,A,2x,A,I2)')'Male = ',num_m,',','Female = ',num_f,',','Gender error = ',error_g , ',','Index error = ',error_ai
    write(11,'(T2,A,6I3)')'Male (index 1-6)  :',m1,m2,m3,m4,m5,m6
    write(11,'(T2,A,6I3)')'Female(index=1-6) :',f1,f2,f3,f4,f5,f6
    
    end subroutine
