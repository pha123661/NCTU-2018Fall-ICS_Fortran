    module datas
    implicit none
    save
    
    real::p1sum=0,p2sum=0,p3sum=0,r1sum=0,r2sum=0,r3sum=0,p1avg=0,p2avg=0,p3avg=0,r1avg=0,r2avg=0,r3avg=0 !p1~r3 總和 p1~r1平均
    integer :: score,site,q1,q2,q3,q4,q5,q6,q7,q8,q9,num=0,mes,error  !error=1為site_error error=2為cate_error
    integer :: p1=0,p2=0,p3=0,r1=0,r2=0,r3=0,error_site=0,error_cate=0 !p1~r3和兩種錯誤之和
    integer :: p1Aa=0,p1a=0,p1b=0,p1c=0,p2Aa=0,p2a=0,p2b=0,p2c=0,p3Aa=0,p3a=0,p3b=0,p3c=0,r1Aa=0,r1a=0,r1b=0,r1c=0,r2Aa=0,r2a=0,r2b=0,r2c=0,r3Aa=0,r3a=0,r3b=0,r3c=0 !p1A+~r3c知各等級人數
    integer :: p1num,p2num,p3num,r1num,r2num,r3num
    character(len=1) :: cate,grade       !分類和等級
    character(len=2) :: p1grade,p2grade,p3grade,r1grade,r2grade,r3grade
    
    end module

    program mid_0711239
    use datas
    implicit none


    open(10,file='Tourist_Attractions_input.txt',status='old')          !開啟input並設定其i/o為10
    open(11,file='Tourist_Attractions_output.txt',status='replace')     !創造或刪除舊檔並創一新的output並設定其i/o為11
    call sum
    
    end program mid_0711239

    Subroutine sum
    use datas
    implicit none
    
    write(11,'(T2,A)')'no cate site   --scores of questionnaire--   totel score'
    
    !開始一行一行讀入資料並判斷P或R 判斷分類並計算單向分數 加總 和計算平均 最後再輸出
    do
        read(10,*,iostat=mes) cate,site,q1,q2,q3,q4,q5,q6,q7,q8,q9
        if(mes<0) exit      !邏輯判斷式 當獨入空白資料時停止迴圈
        num=num+1           !計數
        error=0             !歸零error以判斷下一筆資料的錯誤
        category:select case (cate)     !判斷為P或R
        case('P')                       
            Site_num:select case(site)      !判斷為P1 P2 P3或error
            case(1)         
                p1=p1+1
                score=(17.8*q1+8.1*q2+5.9*q3+16.2*q4+7.5*q5+22.7*q6+10.1*q7+6.5*q8+5.2*q9)+0.5
                P1grading:select case(score)    !判斷其grade
                    case(0:299)
                        p1c=p1c+1
                    case(300:549)
                        p1b=p1b+1
                    case(550:749)
                        p1a=p1a+1
                    case(750:1000)
                        p1Aa=p1Aa+1
                    end select P1grading 
                p1sum=p1sum+score
            case(2)
                p2=p2+1
                score=(17.8*q1+8.1*q2+5.9*q3+16.2*q4+7.5*q5+22.7*q6+10.1*q7+6.5*q8+5.2*q9)+0.5
                P2grading:select case(score)
                    case(0:299)
                        p2c=p2c+1
                    case(300:549)
                        p2b=p2b+1
                    case(550:749)
                        p2a=p2a+1
                    case(750:1000)
                        p2Aa=p2Aa+1
                    end select P2grading
                p2sum=p2sum+score
            case(3)
                p3=p3+1
                score=(17.8*q1+8.1*q2+5.9*q3+16.2*q4+7.5*q5+22.7*q6+10.1*q7+6.5*q8+5.2*q9)+0.5
                P3grading:select case(score)
                    case(0:299)
                        p3c=p3c+1
                    case(300:549)
                        p3b=p3b+1
                    case(550:749)
                        p3a=p3a+1
                    case(750:1000)
                        p3Aa=p3Aa+1
                    end select P3grading
                p3sum=p3sum+score
            case default
                error_site=error_site+1
                error=1
            end select Site_num
            
        case('R')
            Site_num2: select case(site)        !判斷為R1 R2 R3或error
            case(1)
                r1=r1+1
                score=(14.5*q1+6.9*q2+10.3*q3+23.8*q4+12.9*q5+6.7*q6+9.2*q7+8.5*q8+7.2*q9)+0.5
                R1grading:select case(score)    !判斷其grade
                    case(0:399)
                        r1c=r1c+1
                    case(400:599)
                        r1b=r1b+1
                    case(600:799)
                        r1a=r1a+1
                    case(800:1000)
                        r1Aa=r1Aa+1                    
                    end select R1grading
                r1sum=r1sum+score
            case(2)
                r2=r2+1
                score=(14.5*q1+6.9*q2+10.3*q3+23.8*q4+12.9*q5+6.7*q6+9.2*q7+8.5*q8+7.2*q9)+0.5
                R2grading:select case(score)
                    case(0:399)
                        r2c=r2c+1
                    case(400:599)
                        r2b=r2b+1
                    case(600:799)
                        r2a=r2a+1
                    case(800:1000)
                        r2Aa=r2Aa+1                    
                    end select R2grading
                r2sum=r2sum+score
            case(3)
                r3=r3+1
                score=(14.5*q1+6.9*q2+10.3*q3+23.8*q4+12.9*q5+6.7*q6+9.2*q7+8.5*q8+7.2*q9)+0.5
                R3grading:select case(score)
                    case(0:399)
                        r3c=r3c+1
                    case(400:599)
                        r3b=r3b+1
                    case(600:799)
                        r3a=r3a+1
                    case(800:1000)
                        r3Aa=r3Aa+1                    
                    end select R3grading
                r3sum=r3sum+score
            case default
                error_site=error_site+1
                error=1
            end select Site_num2
            
        case default
            error_cate=error_cate+1
            error=2            
            
        end select category
        
        type_of_error:select case(error)        !輸出並判斷是否有任何錯誤
            case(0)
                write(11,'(T2,I2,3x,A,3x,I1,4x,9I3,7x,I3)')num,cate,site,q1,q2,q3,q4,q5,q6,q7,q8,q9,score
            case(1)
                write(11,'(T2,I2,3x,A,3x,I1,4x,9I3,5x,A)')num,cate,site,q1,q2,q3,q4,q5,q6,q7,q8,q9,'error_site'
            case(2)
                write(11,'(T2,I2,3x,A,3x,I1,4x,9I3,5x,A)')num,cate,site,q1,q2,q3,q4,q5,q6,q7,q8,q9,'error_cate'
            end select type_of_error
            
    end do
    
    write(11,*)' '
    write(11,'(T2,A,I3)')'total_quest = ',num
    write(11,'(T2,A,I2,2x,A,I2)')'err_cate = ',error_cate,', err_site = ',error_site
    write(11,*)' '
    p1num=p1Aa+p1a+p1b+p1c      !此部分為運算各分項(P1 P2...)有效資料之總和
    p2num=p2Aa+p2a+p2b+p2c
    p3num=p3Aa+p3a+p3b+p3c
    r1num=r1Aa+r1a+r1b+r1c
    r2num=r2Aa+r2a+r2b+r2c
    r3num=r3Aa+r3a+r3b+r3c
    p1avg=p1sum/p1num           !此部分為計算每分項之分數平均值
    p2avg=p2sum/p2num
    p3avg=p3sum/p3num
    r1avg=r1sum/r1num
    r2avg=r2sum/r2num
    r3avg=r3sum/r3num
    
    if(0<=p1avg.and.p1avg<300) then !依分項平均值來判斷其grade
        p1grade='C'
    else if(300<=p1avg.and.p1avg<550) then
        p1grade='B'
    else if(550<=p1avg.and.p1avg<750) then
        p1grade='A'
    else if(750<=p1avg.and.p1avg<1000) then
        p1grade='A+'
    end if
    
    if(0<=p2avg.and.p2avg<300) then
        p2grade='C'
    else if(300<=p2avg.and.p2avg<550) then
        p2grade='B'
    else if(550<=p2avg.and.p2avg<750) then
        p2grade='A'
    else if(750<=p2avg.and.p2avg<1000) then
        p2grade='A+'
    end if

    if(0<=p3avg.and.p3avg<300) then
        p3grade='C'
    else if(300<=p3avg.and.p3avg<550) then
        p3grade='B'
    else if(550<=p3avg.and.p3avg<750) then
        p3grade='A'
    else if(750<=p3avg.and.p3avg<1000) then
        p3grade='A+'
    end if
        
        
    if(0<=r1avg.and.r1avg<400) then
        r1grade='C'
    else if(400<=r1avg.and.r1avg<600) then
        r1grade='B'
    else if(600<=r1avg.and.r1avg<800) then
        r1grade='A'
    else if(800<=r1avg.and.r1avg<=1000) then
        r1grade='A+'
    end if
    
    if(0<=r2avg.and.r2avg<400) then
        r2grade='C'
    else if(400<=r2avg.and.r2avg<600) then
        r2grade='B'
    else if(600<=r2avg.and.r2avg<800) then
        r2grade='A'
    else if(800<=r2avg.and.r2avg<=1000) then
        r2grade='A+'
    end if
    
    if(0<=r3avg.and.r3avg<400) then
        r3grade='C'
    else if(400<=r3avg.and.r3avg<600) then
        r3grade='B'
    else if(600<=r3avg.and.r3avg<800) then
        r3grade='A'
    else if(800<=r3avg.and.r3avg<=1000) then
        r3grade='A+'
    end if
    
    write(11,'(T2,A)')'cate-site  number_of_grades(C,B,A,A+) number_of_ques ave_of_score grade_of_site' !輸出各項總和資料
    write(11,'(T5,A,7x,4I5,11x,I3,10x,F6.2,10x,A)')'P1',p1c,p1b,p1a,p1Aa,p1num,p1avg,p1grade
    write(11,'(T5,A,7x,4I5,11x,I3,10x,F6.2,10x,A)')'P2',p2c,p2b,p2a,p2Aa,p2num,p2avg,p2grade
    write(11,'(T5,A,7x,4I5,11x,I3,10x,F6.2,10x,A)')'P3',p3c,p3b,p3a,p3Aa,p3num,p3avg,p3grade
    write(11,'(T5,A,7x,4I5,11x,I3,10x,F6.2,10x,A)')'R1',r1c,r1b,r1a,r1Aa,r1num,r1avg,r1grade
    write(11,'(T5,A,7x,4I5,11x,I3,10x,F6.2,10x,A)')'R2',r2c,r2b,r2a,r2Aa,r2num,r2avg,r2grade
    write(11,'(T5,A,7x,4I5,11x,I3,10x,F6.2,10x,A)')'R3',r3c,r3b,r3a,r3Aa,r3num,r3avg,r3grade
    
    end subroutine
