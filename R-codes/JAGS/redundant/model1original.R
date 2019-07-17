#Null model
#mu does not change

model1.txt =  "model{

# Likelihood

for(j in 1:Nleaf){
mu[j]        <- rho[j]
for(i in 1:Nday){
Y[i,j]      ~ dpois(mu[j])
}
}

}"