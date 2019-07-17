# Null model

model1.txt =  "model{

# Likelihood

for(j in 1:Nleaf){

mu[j] <- rho[,j]
Y[,j] ~ dpois(mu[j])

}

}"

## 1 = toital
### 2:3 = level 1
#### 4:7 = level2