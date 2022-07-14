    program PRA3_0711239

    implicit none

    ! Variables
    
    real :: a,b
    real :: ga,gb,gans
    real :: f11,f12,f13,f14,f15,f21,f22,f23,f24,f25,f26,f27,f11ans,f12ans,h14,h16
    real :: f21ans,f22ans
    ! Body of PRA3_0711239
    open(10,file='input.txt',status='old' )
    open(11,file='output.txt',status= 'replace' )
    read(10,*)a,b
    print *,'f(x)=x^2+2x'
    print *,'Integration range=(',a,',',b,')'
    write(11,*)'f(x)=x^2+2x'
    write(11,*)'Integration range=(',a,',',b,')'
    
    !exact form
    ga=(a**3/3)+a**2
    gb=(b**3/3)+b**2
    gans=gb-ga
    print *,'Exact value of integration=',gans
    write(11,*)'Exact value of integration=',gans
    !Trapezoidal Rule
    !n=4
    h14=(b-a)/4    
    f11=(a**2)+2*a
    f12=((a+h14)**2)+2*(a+h14)
    f13=((a+2*h14)**2)+2*(a+2*h14)
    f14=((a+3*h14)**2)+2*(a+3*h14)
    f15=(b**2)+2*b
    f11ans=h14/2*(f11+2*(f12+f13+f14)+f15)
    print *,'Trapezoidal Rule'
    write(11,*)'Trapezoidal Rule'
    print *,'The approximate value of 4 division is',f11ans
    write(11,*)'The approximate value of 4 division is',f11ans
    !n=6
    h16=(b-a)/6
    f21=(a**2)+2*a
    f22=((a+h16)**2)+2*(a+h16)
    f23=((a+2*h16)**2)+2*(a+2*h16)
    f24=((a+3*h16)**2)+2*(a+3*h16)
    f25=((a+4*h16)**2)+2*(a+4*h16)
    f26=((a+5*h16)**2)+2*(a+5*h16)
    f27=(b**2)+2*b
    f12ans=h16/2*(f21+2*(f22+f23+f24+f25+f26)+f27)
    print *,'The approximate value of 6 division is',f12ans
    write(11,*)'The approximate value of 6 division is',f12ans
    !Simpson¡¦s Rule
    !n=4
    print *,'Simpson¡¦s Rule'
    write(11,*)'Simpson¡¦s Rule'
    f21ans=h14/3*(f11+4*f12+2*f13+4*f14+f15)
    print *,'The approximate value of 4 division is',f21ans
    write(11,*)'The approximate value of 4 division is',f21ans
    !n=6
    f22ans=h16/3*(f21+4*f22+2*f23+4*f24+2*f25+4*f26+f27)
    print *,'The approximate value of 6 division is',f22ans
    write(11,*)'The approximate value of 6 division is',f22ans
    end program PRA3_0711239

