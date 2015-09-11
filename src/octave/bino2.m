function Y=bino2(n)
n=n;
T=10;
tao=T/n;


mu=1.5;
sigma=1;

x=rand(1,n)
h=sqrt((mu*tao)^2+tao*sigma^2);
p=0.5*(mu*tao/h+1)

Y=rand(1,n);
Y1=max(0,2*(p-Y)./abs(Y-p))-1;
Y1
Y2=h*Y1;
Y3=cumsum(Y2)
t=tao*[0:n];

plot(t,[0 Y3],'.r-');

