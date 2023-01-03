
program interpolation
implicit none
integer::i,j
real::x(7),y(7),P3,P6,diff
! x=time, y=temperature
open(100,file='in_airtemp.dat')
do i=1,7
read(100,*)x(i),y(i) 
end do
call Pol (4,P3)
call Pol (7,P6)
write(*,*)"Degree   Value of polynomial at 13:00hrs"
write(*,*)"3           ",P3
write(*,*)"6           ",P6
diff=P6-P3
write(*,*)"  "
write(*,*)"The difference between P(6) and P(3)=P6-P3=",diff

contains 
subroutine Pol(n,Pn)
implicit none
integer::n
real::Pn,Lx,r
r=13.0;Pn=0
do j=1,n
Lx=1.0
do i=1,n
IF (j.NE.i) THEN
Lx=Lx*((r-x(i))/(x(j)-x(i))) 
END IF
end do
Pn=Pn+(Lx*y(j))
end do
end subroutine Pol
end program interpolation
