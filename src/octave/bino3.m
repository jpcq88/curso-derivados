function Y=bino3(n)

T=1;
mu=1.5;
sigma=0.6;
Dt=T/n;



t = Dt*[0:n];

xi = 2*binornd(1,0.5,1,n)-1;

Y  = Dt*mu+xi*sqrt(Dt);
Y2 = cumsum(Y);

plot(t,[0 Y2],'.-r')
