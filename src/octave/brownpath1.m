function [t Y2]=brownpath1(n);

n=n;
T=1;
tao=T/n;


mu=0;
sigma=1;

x=randn(1,n);

t=tao*ones(1,n+1);
Y=mu*t+sigma*sqrt(tao)*[0 x];
Y2=cumsum(Y);
t1=cumsum(t);
plot(t1,Y2,'LineWidth',2)