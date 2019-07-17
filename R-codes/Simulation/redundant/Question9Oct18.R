rand.day.time  <- function(N, st="2006/01/01 00:00:01", et="2018/12/31 23:59:59") {
  st <- as.POSIXct(strptime(st, format="%Y/%m/%d %H:%M:%S",tz="Pacific/Auckland"))
  et <- as.POSIXct(strptime(et, format="%Y/%m/%d %H:%M:%S",tz="Pacific/Auckland"))
  dt <- as.numeric(difftime(et,st,unit="sec"))
  ev <- sort(runif(N, 0, dt))
  rt <- st + ev
}

sim.number = 10000
time.start = "2006/01/01 00:00:01"
time.end = "2018/12/31 23:59:59"

cat2.val = qpcR:::cbind.na(             
  c(200,200,100),                       
  c(200,200,  0),
  c(100,  0,  0))

cat2.val
str(cat2.val)

index = 1:sim.number 
EventID = sprintf("%08d", sample(1:paste(rep(9,8), collapse=""), 
                                 sim.number,replace=F))

### select random times
rand.day.time  <- function(N, st=time.start, et = time.end) {
  st <- as.POSIXct(strptime(st, format="%Y/%m/%d %H:%M:%S",tz="Pacific/Auckland"))
  et <- as.POSIXct(strptime(et, format="%Y/%m/%d %H:%M:%S",tz="Pacific/Auckland"))
  dt <- as.numeric(difftime(et,st,unit="sec"))
  ev <- sort(runif(N, 0, dt)) ##########3<----------delete sort??? or add seasonality???
  rt <- st + ev
}
time = rand.day.time(sim.number, time.start, time.end)
time.char = as.character(time)
time.day = as.character(as.Date(time.char,units = "days"))

#----------------------------------------------------------#
### simulate level 1 and 2 brunch using terminal root matrix

cat1.val = colSums(cat2.val,na.rm=T)
cat1.n = length(cat1.val)

cat2.tot = sum(colSums(!is.na(cat2.val)))
cat2.n = nrow(cat2.val)

cat1.let = c(LETTERS[1:cat1.n])
cat2.let = c(LETTERS[1:cat2.n])

sim.cat1 = rep(cat1.let,cat1.val)
tabulate(as.factor(sim.cat1))
cat1 = sample(sim.cat1 , sim.number, replace = TRUE)
tabulate(as.factor(cat1))

cat2 = rep(NA,sim.number)
Category = cbind(cat1,cat2)
head(Category)

for(i in 1:cat2.n){
  Category[which(Category[,1] == LETTERS[i]),2] = sample(rep(cat2.let,cat2.val[,i]), 
                                                         sum(Category[,1] == LETTERS[i]), replace = TRUE)
}

Category[1:20,]
table(Category[,1], Category[,2])

###assign name and value to dummy variable, name same as value

cat1.name = cat1.let
cat1.name
cat2.name = levels(interaction(cat1.let,cat2.let,sep = "", lex.order = TRUE))
cat2.name 

substr(cat2.name,1,1)
substr(cat2.name,2,2)
names = c(cat1.name,cat2.name)
Category = as.data.frame(Category)

for(i in 1:(cat1.n+cat1.n*cat2.n)){
  Category[,i+2] = rep(names[i],sim.number)
}

colnames(Category) = c("cat1","cat2",cat1.name,cat2.name)
head(Category)

### use ifelse to assign dummies variables

for(i in 1:cat1.n){
  Category[,i+2] = ifelse(Category$cat1 == Category[,i+2], 1, 0)
}

for(i in 1:(cat1.n*cat2.n)){
  Category[,i+2+cat1.n] = ifelse(Category$cat1 == substr(Category[,i+2+cat1.n],1,1) &
                                   Category$cat2 == substr(Category[,i+2+cat1.n],2,2), 1, 0)
}
head(Category)

### delete columns if column sum is 0

dummies = Category[-(1:2)]
cat2.val
colSums(dummies)
emptyroot = which(colSums(dummies) == 0)
emptyroot2 = which(colSums(dummies) == 2)
emptyroot
emptyroot2
test1 = Category[-(emptyroot+2)]
test2 = Category[-(emptyroot2+2)]
head(test1)
head(test2)

#I am trying to delete a variable if colsum is 0
# however 
# I am getting error when none of the category is empty, ie not getting any colsum = 0.
# unable to use ifelse
# how can i fix it so that I can still get a good dtaframe when emptyroot is empty? 
# If the code works head(test2) should be the same as head(Category)