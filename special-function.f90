!Accosiated lagandre's polynomial input is (integer,integer,real)
function P(m,l,x) result(sol)
implicit none
real:: x,sol,y1,y2,s,z,binomial,fact,a,b
integer:: k,l,m,test,oddeven
s=0
do k=m,l
	z=real((l+k-1))/2
	y1=(real((fact(k)))/real((fact(k-m))))*(x**(k-m))
	y2=(binomial(real(l),k))*(binomial(z,l))
	s=s+(y1*y2)
end do
test=oddeven(m)
if ((1-(x**2))<0 .and. test==1) then
	sol=((-1)**m)*(2**l)*(((x**2)-1)**(real(m)/2))*s
	Print*, 'It is complex number'
else
	sol=((-1)**m)*(2**l)*((1-(x**2))**(real(m)/2))*s
end if
end function P
 

!to find odd or even odd=>1; even=>0 input is integer
function oddeven(n) result(m)
implicit none
integer::n,m,x
x=(-1)**n
if (x==-1) then
	m=1
else
	m=0
end if
end function oddeven
 
!binomial coefficient input is (real,integer)
function binomial(a,b) result(sol)
implicit none
real:: a,alpha,temp,sol,fact
integer::i,b
alpha=1
do i=0,b-1
	temp=a-real(i)
	alpha=temp*alpha
end do
sol=alpha/real((fact(b)))
end function binomial


!factorial input is integer
function fact(n) result(factorial)
implicit none
real :: factorial
integer:: i,n
factorial=1
if (n==0) then
	factorial=1
else
	do i=1,n
	factorial=factorial*i
	end do
end if
end function fact


! Hermite polynomial input is (integer,real)
function H(n,x) result(sol)
implicit none
real:: x,temp,sol,s,a,b,d,e,fact
integer:: oddeven,test,m,n
test=oddeven(n)
temp=0

if( test==0 ) then
	do m=0,n/2
		a=(-1)**((n/2)-m)
		b=(2*x)**(2*m)
		d=fact(2*m)
		e=fact((n/2)-m)
		s=(a*b)/(d*e)
		temp=temp+s
	end do
else
	do m=0,(n-1)/2
		a=(-1)**(((n-1)/2)-m)
		b=(2*x)**((2*m)+1)
		d=fact((2*m)+1)
		e=fact(((n-1)/2)-m)
		s=(a*b)/(d*e)
		temp=temp+s
	end do
end if

sol=(fact(n))*temp
end function H


! Bessel's function input is (integer,real)
function J(p,x) result(temp)
implicit none
real:: x,a,b,d,s,temp
integer::n,p
temp=0
do n=0,20
	a=((-1)**n)*((x/2)**((2*n)+p)
	b=fact(m)
	d=gamma(n+1+p)
	s=a/(b*d)
	temp=temp+s
end do
end function J





