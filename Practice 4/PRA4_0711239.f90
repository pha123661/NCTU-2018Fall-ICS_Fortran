    Module datas
    implicit none
    save
    integer :: four,six
    real :: a,b
    real :: gans
    real :: f11ans,f12ans
    real :: f21ans,f22ans
    
    End module
    
    program PRA4_0711239
    use datas
    implicit none
    
    open(10,file='input.txt',status='old' )
    open(11,file='output.txt',status= 'replace' )
    call imput
    call exact
    call nforfour
    call nforsix
    call output
    
    end program PRA4_0711239
    
    Subroutine imput
    use datas
    implicit none
    
    read(10,*)a,b,four,six
    
    End subroutine
    
    Subroutine exact
    use datas
    implicit none
    
    real :: ga,gb
    ga=(a**3/3)+a**2
    gb=(b**3/3)+b**2
    gans=gb-ga
    
    End subroutine
    
    
    Subroutine nforfour
    use datas
    implicit none
    
    real:: f11,f12,f13,f14,f15,h14
    h14=(b-a)/four    
    f11=(a**2)+2*a
    f12=((a+h14)**2)+2*(a+h14)
    f13=((a+2*h14)**2)+2*(a+2*h14)
    f14=((a+3*h14)**2)+2*(a+3*h14)
    f15=(b**2)+2*b
    !Trapezoidal Rule
    f11ans=h14/2*(f11+2*(f12+f13+f14)+f15)
    !Simpson・s Rule
    f21ans=h14/3*(f11+4*f12+2*f13+4*f14+f15)
    
    End subroutine

    Subroutine nforsix
    use datas
    implicit none
    
    real::f21,f22,f23,f24,f25,f26,f27,h16
    h16=(b-a)/six
    f21=(a**2)+2*a
    f22=((a+h16)**2)+2*(a+h16)
    f23=((a+2*h16)**2)+2*(a+2*h16)
    f24=((a+3*h16)**2)+2*(a+3*h16)
    f25=((a+4*h16)**2)+2*(a+4*h16)
    f26=((a+5*h16)**2)+2*(a+5*h16)
    f27=(b**2)+2*b
    !Trapezoidal Rule
    f12ans=h16/2*(f21+2*(f22+f23+f24+f25+f26)+f27)
    !Simpson・s Rule
    f22ans=h16/3*(f21+4*f22+2*f23+4*f24+2*f25+4*f26+f27)
    
    End subroutine
    
    Subroutine output
    use datas 
    implicit none
    
    print *,'f(x)=x^2+2x'
    write(11,*)'f(x)=x^2+2x'
    print *,'Integration range=(',a,',',b,')'
    write(11,*)'Integration range=(',a,',',b,')'
    print *,'Exact value of integration=',gans
    write(11,*)'Exact value of integration=',gans
    
    print *,'Trapezoidal Rule'
    write(11,*)'Trapezoidal Rule'
    print *,'The approximate value of',four,'division is',f11ans
    write(11,*)'The approximate value of',four,'division is',f11ans
    print *,'The approximate value of',six,'division is',f12ans
    write(11,*)'The approximate value of',six,'division is',f12ans
    
    
    print *,'Simpson・s Rule'
    write(11,*)'Simpson・s Rule'
    print *,'The approximate value of',four,'division is',f21ans
    write(11,*)'The approximate value of',four,'division is',f21ans
    print *,'The approximate value of',six,'division is',f22ans
    write(11,*)'The approximate value of',six,'division is',f22ans    

    End subroutine