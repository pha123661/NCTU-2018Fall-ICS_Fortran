    Module datas
    implicit none
    save
    
    integer :: a1,b1,c1,d1,e1,i,ta,tb,tc,td,te,n1
    integer :: mes
    character(len=100) :: string

    
    End module
    
    program PRA5_0711239
    use datas
    implicit none
    
    open(10,file='input.txt',status='old' )
    open(11,file='output.txt',status= 'replace' )
    call calculate_string
    
    end program PRA5_0711239
    

    Subroutine calculate_string
    use datas
    implicit none
    write(11,'(T2,A)')'Line    a    b    c    d    e'
    do
        read(10,'(A)',iostat=mes)string
        if(mes<0) exit
        n1=n1+1
        a1=0
        b1=0
        c1=0
        d1=0
        e1=0
        do i=1,100,1
            select case (string(i:i))
            case('a')
                a1 = a1+1
            case('b')        
                b1=b1+1
            case('c')
                c1=c1+1
            case('d')
                d1=d1+1
            case('e')
                e1=e1+1
            end select
        end do
    write(11,'(6I5)')n1,a1,b1,c1,d1,e1
    ta=ta+a1
    tb=tb+b1
    tc=tc+c1
    td=td+d1
    te=te+e1
    end do
    write(11,'(T3,A,5I5)')'sum',ta,tb,tc,td,te
    End subroutine

    