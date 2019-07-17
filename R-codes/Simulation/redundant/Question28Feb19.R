### Question 1

cat2.val1 = cbind(            
  c(200,200,100),                 
  c(200,200,  0),
  c(100,  0,  0))

cat2.val2 = cbind(            
  c(200,200,100),                 
  c(200,200, 50),
  c(100, 20,500))

test1 = data.frame(matrix(cat2.val1,nrow=1,ncol=9))
test1 
test2 = data.frame(matrix(cat2.val2,nrow=1,ncol=9))
test2

emptyleaf1 = which(test1 == 0)
emptyleaf1
emptyleaf2 = which(test2 == 2) 
emptyleaf2



if (any(test1==0)) {
  test1b = test1[-emptyleaf1]
} else {
  test1b = test1
}
test1b

if (any(test2==0)) {
  test2b = test2[-emptyleaf2]
} else {
  test2b = test2
}
test2b
