# Sankey diagram



sankeyx <- function(data = read.csv("daily3.csv"),
                    color = "lightblue",
                    date = c("2008-01-12")
                    ){

### sort data

  sortcountdata <- function(data = data){
    
    catleafcount = table(nchar(names(data[4:length(data)])))
    ncat = catleafcount[1]
    nleaf = catleafcount[2]
    cat1loc =(3+1):(3+ncat)
    leafloc = (3+1+ncat):(length(data))
    
    sortcat1 = sort(names(data[cat1loc]))
    sortleaf = sort(names(data[leafloc]))
    
    data2 = cbind(data[,1:3],data[,sortcat1],data[,sortleaf])
    return(data2)
  }

daily.df = read.csv("daily.csv")

### Layout of plot
layout(rbind(c(0, 3, 3, 0), 
             c(0, 1, 2, 0), 
             c(0, 0, 0, 0)), 
             height = c(lcm(2), 1, lcm(2)), 
             width = c(lcm(2), 1, lcm(2), lcm(1))) 
layout.show(3) 

### Define locations of the data
ncatleaf = table(nchar(names(daily.df[4:length(daily.df)])))
ncat = ncatleaf[1]
nleaf = ncatleaf[2]

### Defining locations of the plot
  plotboarder = list("x"=c(0,120),"y"=c(0,100))
  flowspace = c(40,70,100)
  loc.x = c(0,20,40,60,80,100)
  loc.y = list(total = c((plotboarder[["y"]][2]-flowspace[1])/2, 
                          plotboarder[["y"]][2]-(plotboarder[["y"]][2]-flowspace[1])/2),
               cat1  = c((plotboarder[["y"]][2]-flowspace[2])/2, 
                          plotboarder[["y"]][2]-(plotboarder[["y"]][2]-flowspace[2])/2),
               leaf  = c((plotboarder[["y"]][2]-flowspace[3])/2, 
                          plotboarder[["y"]][2]-(plotboarder[["y"]][2]-flowspace[3])/2))

### Select the total of the day and find scale to use to convert counts

  totaldate.row = which(daily.df$day==date)
  totaldate.col = which(names(daily.df)=="total")
      datetotal = daily.df[totaldate.row ,totaldate.col]
     countscale = flowspace[1]/datetotal 
  
### Count and name of values
  cat1 = as.numeric(as.vector(daily.df[totaldate.row,4:(4+ncat-1)]))
  leaf = as.numeric(as.vector(daily.df[totaldate.row,(4+ncat):ncol(daily.df)]))  
  cat1.name = names(daily.df)[4:(4+ncat-1)]
  leaf.name = names(daily.df)[(4+ncat):(4+ncat+nleaf-1)]
  
### find y values  
  cumsumcat1 = cumsum(cat1)
  cumsumleaf = cumsum(leaf)
  
  cat1space = rep((flowspace[2] - flowspace[1] )/(length(cat1)-1),length(cat1)-1)
  leafspace = rep((flowspace[3] - flowspace[1])/(length(leaf)-1),length(leaf)-1)
  
  cat1yval = rbind(c((plotboarder[["y"]][2] - flowspace[2])/2, cat1space), cat1*countscale)
  leafyval = rbind(c((plotboarder[["y"]][2] - flowspace[3])/2, leafspace), leaf*countscale)
  
  cat1y = plotboarder[["y"]][2] - cumsum(cat1yval)
  leafy = plotboarder[["y"]][2] - cumsum(leafyval)

  leafmatrix = matrix(leafy, ncol = 2, byrow = T)
  cat1matrix = matrix(cat1y, ncol = 2, byrow = T)
  colnames(leafmatrix) = c("top","bottom")
  colnames(cat1matrix) = c("top","bottom")
  
### Draw rectangles
  plot(plotboarder$x, plotboarder$y, type = "n", xlab = "", ylab = "", axes=F)   # add axes=F later
  
  totalxl = loc.x[1]
  totalxr = loc.x[2]
  cat1xl  = loc.x[3]
  cat1xr  = loc.x[4]
  leafxl  = loc.x[5]
  leafxr  = loc.x[6]
  
  totalyt = - (plotboarder[["y"]][2]-flowspace[1])/2 + plotboarder[["y"]][2] 
  totalyb =   (plotboarder[["y"]][2]-flowspace[1])/2
  cat1yt  = cat1matrix[,1]
  cat1yb  = cat1matrix[,2]
  leafyt  = leafmatrix[,1]
  leafyb  = leafmatrix[,2]
  
### rect(xleft, ybottom, xright, ytop)
  rect(totalxl, totalyb, totalxr, totalyt, col = color, border = NA)
  rect(cat1xl,  cat1yb,  cat1xr,  cat1yt,  col = color, border = NA)
  rect(leafxl,  leafyb,  leafxr,  leafyt,  col = color, border = NA)
  

### Draw polygons
  
  # Find x values
  l = c(loc.x[2],loc.x[4])
  r = c(loc.x[3],loc.x[5])
  polygonx = matrix(NA,nrow=5,ncol=ncat +nleaf)
  for (i in 1:ncat){
    for (j in (ncat+1):(ncat+nleaf)){
      polygonx[,j] = c(l[2],r[2],r[2],l[2],l[2])
    }
    polygonx[,i] = c(l[1],r[1],r[1],l[1],l[1])
  }
  polygonx
  
### Find y values
  
  # right of polygons
  pcat1yl  = cat1y
  pcat1ylt = pcat1yl[seq(1,length(cat1y),2)]
  pcat1ylb = pcat1yl[seq(2,length(cat1y),2)]
  
  pleafyl  = leafy
  pleafylt = pleafyl [seq(1,length(leafy),2)]
  pleafylb = pleafyl [seq(2,length(leafy),2)]
  
####
  
  #left of polygons
  
  ptotalyr  = c(totalyt[1],totalyt[1]-cumsum(cat1*countscale))
  ptotalyrt = ptotalyr[seq(1, ncat,1)]
  ptotalyrb = ptotalyr[seq(2, ncat+1,1)]
  
  ins2 <- function(a,bs,pos){
    as <- split(a,cumsum(seq(a)%in%(pos+1)))
    idx <- order(c(seq_along(as),seq_along(bs)))
    unlist(c(as,bs)[idx])
  }
  
  count.leaf = table(substr(leaf.name , 1,1))
       ncat1 = length(cat1)
  ins.length = rep(cat1space,length(cat1space))
  ins.loc    = cumsum(count.leaf)[-length(cumsum(count.leaf))]
  
  pcat1yr = c(cat1yt[1],cat1yt[1]- cumsum(ins2(leaf*countscale, ins.length,ins.loc)))  
  pcat1yr
  
pp = list()
  for (i in 1:ncat1){
  pp[[i]] = rep(1,count.leaf[i])
  pp[[i]][1] = 2
  pp[[1]][1] = 1
  }
k = cumsum(unlist(pp))

pcat1yrt = pcat1yr[k]
pcat1yrb = pcat1yr[k+1]
 
  ###############
 
polygony = matrix(NA,ncol=ncat +nleaf,nrow=5)

for (i in 1:ncat1){
  for (j in 1:nleaf){
    polygony[,i]   = cbind(ptotalyrt[i], pcat1ylt[i], pcat1ylb[i], ptotalyrb[i], ptotalyrt[i])
    polygony[,j+2] = cbind(pcat1yrt[j],  pleafylt[j], pleafylb[j],  pcat1yrb[j],  pcat1yrt[j])
    colnames(polygony) = c(cat1.name,leaf.name)
    row.names(polygony)= c("ptotalyrt", "pcat1ylt", "pcat1ylb", "ptotalyrb", "ptotalyrt")
  }
}
polygony 


  polygon(x=polygonx[,1:ncat1],y=polygony[,1:ncat1],col=color,border = NA)
  polygon(x=polygonx[,(ncat1+1):(ncat1+nleaf)],y=polygony[,(ncat1+1):(ncat1+nleaf)],col="lightblue",border = NA)
  

  
### insert text
  allnames = paste(names(daily.df)[3:length(daily.df)],"  n = ",daily.df[7,3:length(daily.df)])
  textxpos = c(loc.x[1]+2,rep(loc.x[3],length(cat1)),rep(loc.x[6],length(leaf)))+10
  textypos = c(50,rowSums(cat1matrix)/2,rowSums(leafmatrix)/2)
  text(x = textxpos, y = textypos, labels = allnames,cex=1.5)

  date = c("2006-01-07") #Why adding "%d%b%Y" gives NA???
 Titlename  = paste("Flowchart for ",as.character(date)," Arrivals")
 title(main = Titlename,adj=0)

#
#
#
#
# pdf('eg.pdf', width = 8.3, height = 11.7)  ## Device with dimensions of A4 paper
# assign year $ month, weekly data 
 
}

sankeyx.test<- function(){
  
load("rdata/test/raw1.RData")
write.csv(daily1.df, file = "daily.csv")

sankeyx(read.csv("daily.csv"),color = "lightblue",date = c("2006-01-08"))
}


load()





