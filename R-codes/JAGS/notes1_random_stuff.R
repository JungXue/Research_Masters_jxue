

n.cat1 = length(levels(raw.df$cat1))
n.leaf = length(levels(raw.df$leaf))
N = nrow(daily.df)

# expected rate of leafs as matrix form
rhomatrix = data.frame(rho.df[,(3+n.cat1+1):length(rho.df)])
head(rhomatrix)

# counts of leafs as matrix form
dailymatrix = data.frame(daily.df[,(3+n.cat1+1):length(rho.df)])
head(dailymatrix)

#####################################################
# expected rate as columnar form
rhocol <- gather(rho.df,key = 'group',value = 'Rho',3:length(rho.df))
head(rhocol)

# expected rate of leafs as columnar form
rholeafcol <- gather(rhomatrix,key = 'Leaf',value = 'Rho',1:length(rhomatrix))
head(rholeafcol)

# Count as columnar form
countcol  <- gather(daily.df,key = 'group',value = 'Count',3:length(daily.df))
head(countcol )

# Count leafs as columnar form
countleafcol <- gather(dailymatrix,key = 'group',value = 'Count',1:length(dailymatrix))
head(countleafcol)
