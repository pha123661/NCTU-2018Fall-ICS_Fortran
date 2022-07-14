    Module datas
    implicit none
    save
    character(len=2) :: country
    real :: tax_rate
    integer :: sal,tax
    integer :: n,na,nb,nc
    integer :: a1=0,a2=0,a3=0,a4=0
    integer :: b1=0,b2=0,b3=0,b4=0
    integer :: c1=0,c2=0,c3=0,c4=0
    integer :: mes,ip,ec=0,es=0

    
    End module
    
    program PRA6_0711239
    use datas
    implicit none
    
    open(10,file='salary_tax_input.txt',status='old' )
    open(11,file='output.txt',status= 'replace' )
    call cal

    end program PRA6_0711239
    
    Subroutine cal
    use datas
    implicit none
    write(11,'(T2,A,2x,A,2x,A,2x,A,5x,A)')'no.','country','salary','tax_rate','tax'
    do
        
        read(10,*,iostat=mes)country,sal
        if(mes<0) exit
        n=n+1
        ip=0
        
        select case (country)
            
        case('A1','A3')
            select case(sal)
            case(0:1000)
                tax_rate=0.01
                a1=a1+1
            case(1001:2000)
                tax_rate=0.02
                a2=a2+1
            case(2001:5000)
                tax_rate=0.03
                a3=a3+1
            case(5001:)
                tax_rate=0.04
                a4=a4+1
            case default
                ip=2
                
            end select
            
        case('B6')
            select case(sal)
            case(0:1500)
                tax_rate=0.02
                b1=b1+1
            case(1501:2500)
                tax_rate=0.03
                b2=b2+1
            case(2501:5500)
                tax_rate=0.04
                b3=b3+1
            case(5501:)
                tax_rate=0.05
                b4=b4+1
            case default
                ip=2
                
            end select
            
        case('C2','C4','C5')
            select case(sal)
            case(0:2000)
                tax_rate=0.05
                c1=c1+1
            case(2001:3000)
                tax_rate=0.06
                c2=c2+1
            case(3001:6000)
                tax_rate=0.07
                c3=c3+1
            case(6001:)
                tax_rate=0.08
                c4=c4+1
            case default
                ip=2
        end select
        case default
            ip=1
            
        end select
        
        select case(ip)
        case(0)
            tax=tax_rate*sal
            write(11,'(I3,5x,A,4x,I6,3x,F6.2,4x,I6)')n,country,sal,tax_rate,tax
        case(1)
            ec=ec+1
            write(11,'(I3,5x,A,4x,I6,3x,A)')n,country,sal,'  ----  invalid country'
        case(2)
            es=es+1
            write(11,'(I3,5x,A,4x,I6,3x,A)')n,country,sal,'  ----  invalid salary'
            
        end select
    end do
    na=a1+a2+a4+a3
    nb=b1+b2+b3+b4
    nc=c1+c2+c3+c4
    write(11,'(T2,A,I3)')'Total number =',n
    write(11,'(T2,A,I3)')'Country A =',na
    write(11,'(T2,A,I3)')'Country B =',nb
    write(11,'(T2,A,I3)')'Country C =',nc
    write(11,'(T2,A,I3)')'Invalid Country =',ec
    write(11,'(T2,A,I3)')'Invalid Salary  =',es
    write(11,*)'        '
    write(11,*)'Country salary catogory(I~IV) table:'
    write(11,*)'        '
    write(11,'(T10,A,2x,A,2x,A,2x,A,2x)')'I','II','III','IV'
    write(11,'(T7,A,1x,I2,2x,I2,2x,I2,2x,I2,2x)')'A',a1,a2,a3,a4
    write(11,'(T7,A,1x,I2,2x,I2,2x,I2,2x,I2,2x)')'B',b1,b2,b3,b4
    write(11,'(T7,A,1x,I2,2x,I2,2x,I2,2x,I2,2x)')'C',c1,c2,c3,c4
    End subroutine