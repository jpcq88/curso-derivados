SO = 50;
K = 50;
r = 0.1;
sigma = 0.4;
T = 5/12;
N=50 ;
BlsC = blsprice(SO,K,r ,T, sigma) ;
LatticeC = zeros(1,N);
for i=(1:N)
    LatticeC(i) = LatticeEurCall(SO, K , r , T, sigma, i) ;
end
plot(1:N, ones(1,N)*BlsC);
hold on;
plot(1:N, LatticeC);
LatticeC(i) = LatticeEurCall(SO, K , r , T, sigma, i) ;