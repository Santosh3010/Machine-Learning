####################################
####################################
####  Name:SantoshKumar Yadav   ####
####  Email: Yadav.31@wright.edu####
####  Assignment 01             ####
####  Machine Learning          ####
####################################
####################################



#1a
#Setting up a directory
setwd("C:/Users/Mili Bhakta/Desktop/Sany/RStudio/Assignment1_Yadav")

#package required
library(ggplot2)

#reading the.csv file
data <- read.csv("SleepQuality.csv")

#fetching the 7th and 13th row from the .csv file 7th = swsLengthM and 13th = epochCapacity variables
v1 <- data[,7]
v2 <- data[,13]
#Creating a table(data frame) of swsLengthM and epochCapacity.
df <- data.frame(swsLengthM = v1,
                 epochCapacity = v2
)
#Setting two random points 
p1 = c(1,1)
p2 = c(0.23,1.1)
#creating two empty cluster 
cluster1<- data.frame()
cluster2<- data.frame()

pre1 =c(0,0)
pre2 =c(0,0)
while(pre1!=p1){
  pre1=p1;
  pre2=p2;
  cluster1<- data.frame()
  cluster2<- data.frame()
  sumx1<- 0
  sumy1<- 0
  sumx2<- 0
  sumy2<- 0
  c1<- 0
  c2<- 0
  
  for(i in 1:nrow(df )){
    
    dist1=   sqrt( (  (df[i,1]-p1[1])^2  )  + (  ( df[i,2]-p1[2] )^2  )  )
    dist2=   sqrt( (  (df[i,1]-p2[1])^2  )  + (  ( df[i,2]-p2[2] )^2  )  )
    #choosing the closest cluster for datapoint
    if(dist1<=dist2){
      de1 <- data.frame(df[i,])
      cluster1<- rbind(cluster1,de1)
      c1=c1+1
      sumx1=sumx1+df[i,1]
      sumy1=sumy1+df[i,2]
      
    }
    else
    {
      de2 <- data.frame(df[i,])
      cluster2<- rbind(cluster2,de2)
      c2=c2+1
      
      sumx2=sumx2+df[i,1]
      sumy2=sumy2+df[i,2]
    }
  }
  #calculating the new mean 
  if (c1==0){
    p1=c(0,0)
  
  }
  else{
    p1=c(sumx1/c1,sumy1/c1)
  }
  if (c2==0){
    p2=c(0,0)
    
  }
  else{
    p2=c(sumx2/c2,sumy2/c2)
  }
   
  
  
}
centroid1<- p1
centroid2<- p2

print(paste("Centroild1 "))
print(centroid1)
print(paste("Centroild2 "))
print(centroid2)


#ploting the graph of swsLengthM and epochCapacity
ggplot(cluster1, aes(x = swsLengthM, y = epochCapacity) ) +
  geom_point(colour="RED") + geom_point(data=cluster2)


######################################################################################################################################
######################################################################################################################################

#1b and 1c
#Testing the different number of clusters
#K=2 to K=10
for(k in 2:10){
  
  #Selecting 10 random numbers for variables swsLengthM and epochCapacity
  xp<-c(0.5,1.25,1.306,1.1,-0.5,-0.23,-0.11,0.9,2.45,3.142)
  yp<-c(-0.5,-0.23,-0.1,0.9,1.1,1.25,1.306,1.878,2.1,0.5)
  
  
  p<-data.frame(swsLengthM=xp,epochCapacity=yp)
  #creating a k empty clusters
  cluster <- list()
  
  for(i in 1:k){
    cluster[[i]]=data.frame()
  }
  #running a loop for max 10 times
  counter <- 10
  while(counter!=0){
    counter=counter-1;
    sumx<- c()
    sumy<- c()
    cnt<- c()
    for(i in 1:k){
      cluster[[i]]=data.frame()
      sumx[i]=0
      sumy[i]=0
      cnt[i]=0
    }
    
    for(i in 1:nrow(df )){
      minIndx<- 0
      value<- 0
      #calculating the closest cluster for every datapoint
      for(j in 1:k){
        dist=sqrt((df[i,1]-p[j,1])^2 + (df[i,2]-p[j,2])^2)
        if(j==1)
        {
          minIndx=j
          value=dist
        }
        else if(value  >  dist)
        {
          minIndx=j
          value=dist 
        }
      }
      de1 <- data.frame(df[i,])
      cluster[[minIndx]] <- rbind(cluster[[minIndx]],de1)
      cnt[minIndx]=cnt[minIndx]+1
      sumx[minIndx]=sumx[minIndx]+df[i,1]
      sumy[minIndx]=sumy[minIndx]+df[i,2]
    }
    
    vx<-c()
    vy<-c()
    #calculating the mean value 
    for(j in 1:k){
      if(cnt[j] == 0){
        vx[j] = 0
        vy[j] = 0
      }
      else{
          vx[j]=sumx[j]/cnt[j]
          vy[j]=sumy[j]/cnt[j]
      }
    }
    p<-data.frame(swsLengthM=vx,epochCapacity=vy)
    
  }
  centroid<-p
  View(centroid)
  
  colourList <- c('blue','red','yellow','green','brown',
                  'darkgray','lightcoral','cyan','magenta')
  
  
  
  
  
  for(i in 1:k){
    cluster[[i]]$group <- i
  }
  #minimum inter cluster value
  minInterCluster<- 1000.0
  for(i in 1:k){
    for(j in 1:k){
      if(j>i)
      {
        distance=sqrt((p[i,1]-p[j,1])^2 + (p[i,2]-p[j,2])^2)
        if(minInterCluster>distance){
          minInterCluster=distance
        }
      }
    }
  }
  
  #maximum intra cluster value
  maxIntraCluster=-1
  for(itr in 1:k){
    clst=cluster[[itr]]
    for(i in 1:nrow(cluster[[itr]])){
      for(j in 1:nrow(cluster[[itr]])){
        if(j>i)
        {
          intraDistance=sqrt((clst[i,1]-clst[j,1])^2 + (clst[i,2]-clst[j,2])^2)
          if(intraDistance>maxIntraCluster){
            maxIntraCluster=intraDistance
          }
        }
      }
    }
  }
  
  visual12 <- rbind(cluster[[1]], cluster[[2]])
  for(i in 3:k){
    if(k>=i){
      visual12<- rbind(visual12,cluster[[i]])
    }
    
  }
  print(paste("K-> ",k))
  dunnIndex<-(minInterCluster/maxIntraCluster)
  print(paste("Dunn Index: ",dunnIndex))
}
ggplot(visual12, aes(x=swsLengthM, y=epochCapacity, group=group, col=group, fill=group)) +
  geom_point()

######################################################################################################################################
######################################################################################################################################


#2a
#fetching the 7th, 13th and 20th row from the .csv file 7th = swsLengthM,
#13th = epochCapacity variables and 20th = lengthEdaStorm. 
v1 <- data[,7]
v2 <- data[,13]
v3 <- data[,20]
#Creating a table(data frame) 
df <- data.frame(swsLengthM = v1,
                 epochCapacity = v2,
                 lengthEdaStorm= v3
)

k<-9
xp<-c(0.5,1.25,1.306,1.1,-0.5,-0.23,-0.11,0.9,2.45)
yp<-c(-0.5,-0.23,-0.1,0.9,1.1,1.25,1.306,1.878,2.1)
zp<-c(-0.12,0.32,-0.5,1.9,0.1,0.95,1.236,0.18,1.9)


p<-data.frame(swsLengthM=xp,epochCapacity=yp,lengthEdaStorm=zp)
cluster <- list()
for(i in 1:k){
  cluster[[i]]=data.frame()
}
counter <- 10
while(counter!=0){
  counter=counter-1;
  sumx<- c()
  sumy<- c()
  sumz<- c()
  cnt<- c()
  for(i in 1:k){
    cluster[[i]]=data.frame()
    sumx[i]=0
    sumy[i]=0
    sumz[i]=0
    cnt[i]=0
  }
  
  for(i in 1:nrow(df )){
    minIndx<- 0
    value<- 0
    for(j in 1:k){
      dist=sqrt( (df[i,1]-p[j,1])^2 + (df[i,2]-p[j,2])^2 +
                   (df[i,3]-p[j,3])^2  )
      if(j==1)
      {
        minIndx=j
        value=dist
      }
      else if(value  >  dist)
      {
        minIndx=j
        value=dist 
      }
    }
    de1 <- data.frame(df[i,])
    cluster[[minIndx]] <- rbind(cluster[[minIndx]],de1)
    cnt[minIndx]=cnt[minIndx]+1
    sumx[minIndx]=sumx[minIndx]+df[i,1]
    sumy[minIndx]=sumy[minIndx]+df[i,2]
    sumz[minIndx]=sumz[minIndx]+df[i,3]
  }
  
  vx<-c()
  vy<-c()
  vz<-c()
  for(j in 1:k){
    if(cnt[j] == 0){
      vx[j] = 0
      vy[j] = 0
      vz[j] = 0
    }
    else {
      vx[j]=sumx[j]/cnt[j]
      vy[j]=sumy[j]/cnt[j]
      vz[j]=sumz[j]/cnt[j]
    }
   
  }
  p<-data.frame(swsLengthM=vx,epochCapacity=vy,lengthEdaStorm=vz)
  
}
centroid<- p
View(centroid)


for(i in 1:k){
  cluster[[i]]$group <- i
}
print(k)
#minimum inter cluster value
minInterCluster<- 1000.0
for(i in 1:k){
  for(j in 1:k){
    if(j>i)
    {
      distance=sqrt((p[i,1]-p[j,1])^2 + (p[i,2]-p[j,2])^2
                    +  (p[i,3]-p[j,3])^2 )
      if(minInterCluster>distance){
        minInterCluster=distance
      }
    }
  }
}

#maximum intra cluster value
maxIntraCluster=-1
for(itr in 1:k){
  clst=cluster[[itr]]
  for(i in 1:nrow(cluster[[itr]])){
    for(j in 1:nrow(cluster[[itr]])){
      if(j>i)
      {
        intraDistance=sqrt((clst[i,1]-clst[j,1])^2 + 
                             (clst[i,2]-clst[j,2])^2 +
                             (clst[i,3]-clst[j,3])^2 )
        if(intraDistance>maxIntraCluster){
          maxIntraCluster=intraDistance
        }
      }
    }
  }
}

visual12 <- rbind(cluster[[1]], cluster[[2]])
for(i in 3:k){
  if(k>=i){
    visual12<- rbind(visual12,cluster[[i]])
  }
  
}

dunnIndex<-(minInterCluster/maxIntraCluster)
print(paste("Dunn Index: ",dunnIndex))

######################################################################################################################################
######################################################################################################################################


#2b
#fetching the 7th, 13th ,15th,16th and 20th row from the .csv file 7th = swsLengthM,
#13th = epochCapacity variables, 15th = epochPeakCounter, 
#16th = stormPeak and 20th = lengthEdaStorm. 
v1 <- data[,7]
v2 <- data[,13]
v3 <- data[,15]
v4 <- data[,16]
v5 <- data[,20]
#Creating a table(data frame) 
df <- data.frame(swsLengthM = v1,
                 epochCapacity = v2,
                 epochPeakCounter = v3,
                 stormPeak = v4,
                 lengthEdaStorm= v5
)
k<-9
xp<-c(0.5,1.25,1.306,1.1,-0.5,-0.23,-0.11,0.9,2.45)
yp<-c(-0.5,-0.23,-0.1,0.9,1.1,1.25,1.306,1.878,2.1)
zp<-c(-0.12,0.32,-0.5,1.9,0.1,0.95,1.236,0.18,1.9)
kp<-c(-0.11,1.32,-1.5,0.9,1.1,1.95,1.06,1.08,0.92)
lp<-c(1.12,1.04,-0.8,1.1,-0.1,0.55,0.236,-0.18,0.9)


p<-data.frame(swsLengthM=xp,epochCapacity=yp,epochPeakCounter=zp,stormPeak=kp,lengthEdaStorm=lp)
cluster <- list()
for(i in 1:k){
  cluster[[i]]=data.frame()
}
counter <- 10
while(counter!=0){
  counter=counter-1;
  
  sumx<- c()
  sumy<- c()
  sumz<- c()
  sumk<- c()
  suml<- c()
  cnt<- c()
  for(i in 1:k){
    cluster[[i]]=data.frame()
    sumx[i]=0
    sumy[i]=0
    sumz[i]=0
    sumk[i]=0
    suml[i]=0
    cnt[i]=0
  }
  
  for(i in 1:nrow(df )){
    minIndx<- 0
    value<- 0
    for(j in 1:k){
      dist=sqrt( (df[i,1]-p[j,1])^2 + (df[i,2]-p[j,2])^2 +
                   (df[i,3]-p[j,3])^2 +(df[i,4]-p[j,4])^2 +
                   (df[i,5]-p[j,5])^2)
      if(j==1)
      {
        minIndx=j
        value=dist
      }
      else if(value  >  dist)
      {
        minIndx=j
        value=dist 
      }
    }
    de1 <- data.frame(df[i,])
    cluster[[minIndx]] <- rbind(cluster[[minIndx]],de1)
    cnt[minIndx]=cnt[minIndx]+1
    sumx[minIndx]=sumx[minIndx]+df[i,1]
    sumy[minIndx]=sumy[minIndx]+df[i,2]
    sumz[minIndx]=sumz[minIndx]+df[i,3]
    sumk[minIndx]=sumk[minIndx]+df[i,4]
    suml[minIndx]=suml[minIndx]+df[i,5]
  }
  vx<-c()
  vy<-c()
  vz<-c()
  vk<-c()
  vl<-c()
  for(j in 1:k){
    if(cnt[j]!=0){
      vx[j]=sumx[j]/cnt[j]
      vy[j]=sumy[j]/cnt[j]
      vz[j]=sumz[j]/cnt[j]
      vk[j]=sumk[j]/cnt[j]
      vl[j]=suml[j]/cnt[j]
    }
    else{
      vx[j]=0
      vy[j]=0
      vz[j]=0
      vk[j]=0
      vl[j]=0
    }
    
  }
  p<-data.frame(swsLengthM=vx,epochCapacity=vy,epochPeakCounter=vz,stormPeak=vk,lengthEdaStorm=vl)
}



for(i in 1:k){
  cluster[[i]]$group <- i
}

#minimum inter cluster value
minInterCluster<- 1000.0
for(i in 1:k){
  for(j in 1:k){
    if(j>i)
    {
      distance=sqrt((p[i,1]-p[j,1])^2 + (p[i,2]-p[j,2])^2
                    +  (p[i,3]-p[j,3])^2 + (p[i,4]-p[j,4])^2 +
                      (p[i,5]-p[j,5])^2 )
      if(minInterCluster>distance){
        minInterCluster=distance
      }
    }
  }
}

#maximum intra cluster value
maxIntraCluster=-1
for(itr in 1:k){
  clst=cluster[[itr]]
  for(i in 1:nrow(cluster[[itr]])){
    for(j in 1:nrow(cluster[[itr]])){
      if(j>i)
      {
        intraDistance=sqrt((clst[i,1]-clst[j,1])^2 + 
                             (clst[i,2]-clst[j,2])^2 +
                             (clst[i,3]-clst[j,3])^2 +
                             (clst[i,4]-clst[j,4])^2 +
                             (clst[i,5]-clst[j,5])^2)
        if(intraDistance>maxIntraCluster){
          maxIntraCluster=intraDistance
        }
      }
    }
  }
}

visual12 <- rbind(cluster[[1]], cluster[[2]])
for(i in 3:k){
  if(k>=i){
    visual12<- rbind(visual12,cluster[[i]])
  }
  
}
centroid<- p
View(centroid)
dunnIndex<-(minInterCluster/maxIntraCluster)
print(paste("Dunn Index: ",dunnIndex))


ggplot(visual12, aes(x=swsLengthM, y=epochCapacity, group=group, col=group, fill=group)) +
  geom_point()

######################################################################################################################################
######################################################################################################################################


#2c for 2a
#2c
#2a for 100 observation 

idx <- sample.int(218,100)
v1 <- c()
v2 <- c()
v3 <- c()
for(i in 1:100)
{
  v1[i] = data[idx[i],7]
  v2[i] = data[idx[i],13]
  v3[i] = data[idx[i],20]
  
}
print(idx)
#Creating a table(data frame) 
df <- data.frame(swsLengthM = v1,
                 epochCapacity = v2,
                 lengthEdaStorm= v3
)
View(df)
k<-9
xp<-c(0.5,1.25,1.306,1.1,-0.5,-0.23,-0.11,0.9,2.45)
yp<-c(-0.5,-0.23,-0.1,0.9,1.1,1.25,1.306,1.878,2.1)
zp<-c(-0.12,0.32,-0.5,1.9,0.1,0.95,1.236,0.18,1.9)


p<-data.frame(swsLengthM=xp,epochCapacity=yp,lengthEdaStorm=zp)
cluster <- list()
for(i in 1:k){
  cluster[[i]]=data.frame()
}
counter <- 10
while(counter!=0){
  counter=counter-1;
  sumx<- c()
  sumy<- c()
  sumz<- c()
  cnt<- c()
  for(i in 1:k){
    cluster[[i]]=data.frame()
    sumx[i]=0
    sumy[i]=0
    sumz[i]=0
    cnt[i]=0
  }
  
  for(i in 1:nrow(df )){
    minIndx<- 0
    value<- 0
    for(j in 1:k){
      dist=sqrt( (df[i,1]-p[j,1])^2 + (df[i,2]-p[j,2])^2 +
                   (df[i,3]-p[j,3])^2  )
      if(j==1)
      {
        minIndx=j
        value=dist
      }
      else if(value  >  dist)
      {
        minIndx=j
        value=dist 
      }
    }
    de1 <- data.frame(df[i,])
    cluster[[minIndx]] <- rbind(cluster[[minIndx]],de1)
    cnt[minIndx]=cnt[minIndx]+1
    sumx[minIndx]=sumx[minIndx]+df[i,1]
    sumy[minIndx]=sumy[minIndx]+df[i,2]
    sumz[minIndx]=sumz[minIndx]+df[i,3]
  }
  
  vx<-c()
  vy<-c()
  vz<-c()
  for(j in 1:k){
    if(cnt[j] == 0){
      vx[j] = 0
      vy[j] = 0
      vz[j] = 0
    }
    else {
      vx[j]=sumx[j]/cnt[j]
      vy[j]=sumy[j]/cnt[j]
      vz[j]=sumz[j]/cnt[j]
    }
  }
  p<-data.frame(swsLengthM=vx,epochCapacity=vy,lengthEdaStorm=vz)
  
}
centroid<- p
View(centroid)


for(i in 1:k){
  cluster[[i]]$group <- i
}
print(k)
#minimum inter cluster value
minInterCluster<- 1000.0
for(i in 1:k){
  for(j in 1:k){
    if(j>i)
    {
      distance=sqrt((p[i,1]-p[j,1])^2 + (p[i,2]-p[j,2])^2
                    +  (p[i,3]-p[j,3])^2 )
      if(minInterCluster>distance){
        minInterCluster=distance
      }
    }
  }
}

#maximum intra cluster value
maxIntraCluster=-1
for(itr in 1:k){
  clst=cluster[[itr]]
  for(i in 1:nrow(cluster[[itr]])){
    for(j in 1:nrow(cluster[[itr]])){
      if(j>i)
      {
        intraDistance=sqrt((clst[i,1]-clst[j,1])^2 + 
                             (clst[i,2]-clst[j,2])^2 +
                             (clst[i,3]-clst[j,3])^2 )
        if(intraDistance>maxIntraCluster){
          maxIntraCluster=intraDistance
        }
      }
    }
  }
}

visual12 <- rbind(cluster[[1]], cluster[[2]])
for(i in 3:k){
  if(k>=i){
    visual12<- rbind(visual12,cluster[[i]])
  }
  
}

dunnIndex<-(minInterCluster/maxIntraCluster)
print(paste("Dunn Index: ",dunnIndex))

ggplot(visual12, aes(x=swsLengthM, y=epochCapacity, group=group, col=group, fill=group)) +
  geom_point()

######################################################################################################################################
######################################################################################################################################


#2c
#2a for 50 observation 

idx <- sample.int(218,50)
v1 <- c()
v2 <- c()
v3 <- c()
for(i in 1:50)
{
  v1[i] = data[idx[i],7]
  v2[i] = data[idx[i],13]
  v3[i] = data[idx[i],20]
  
}
print(idx) #printing the index number

#Creating a table(data frame) 
df <- data.frame(swsLengthM = v1,
                 epochCapacity = v2,
                 lengthEdaStorm= v3
)
View(df)
k<-9
xp<-c(0.5,1.25,1.306,1.1,-0.5,-0.23,-0.11,0.9,2.45)
yp<-c(-0.5,-0.23,-0.1,0.9,1.1,1.25,1.306,1.878,2.1)
zp<-c(-0.12,0.32,-0.5,1.9,0.1,0.95,1.236,0.18,1.9)


p<-data.frame(swsLengthM=xp,epochCapacity=yp,lengthEdaStorm=zp)
cluster <- list()
for(i in 1:k){
  cluster[[i]]=data.frame()
}
counter <- 10
while(counter!=0){
  counter=counter-1;
  sumx<- c()
  sumy<- c()
  sumz<- c()
  cnt<- c()
  for(i in 1:k){
    cluster[[i]]=data.frame()
    sumx[i]=0
    sumy[i]=0
    sumz[i]=0
    cnt[i]=0
  }
  
  for(i in 1:nrow(df )){
    minIndx<- 0
    value<- 0
    for(j in 1:k){
      dist=sqrt( (df[i,1]-p[j,1])^2 + (df[i,2]-p[j,2])^2 +
                   (df[i,3]-p[j,3])^2  )
      if(j==1)
      {
        minIndx=j
        value=dist
      }
      else if(value  >  dist)
      {
        minIndx=j
        value=dist 
      }
    }
    de1 <- data.frame(df[i,])
    cluster[[minIndx]] <- rbind(cluster[[minIndx]],de1)
    cnt[minIndx]=cnt[minIndx]+1
    sumx[minIndx]=sumx[minIndx]+df[i,1]
    sumy[minIndx]=sumy[minIndx]+df[i,2]
    sumz[minIndx]=sumz[minIndx]+df[i,3]
  }
  
  vx<-c()
  vy<-c()
  vz<-c()
  for(j in 1:k){
    if(cnt[j] == 0){
      vx[j] = 0
      vy[j] = 0
      vz[j] = 0
    }
    else {
      vx[j]=sumx[j]/cnt[j]
      vy[j]=sumy[j]/cnt[j]
      vz[j]=sumz[j]/cnt[j]
    }
  }
  p<-data.frame(swsLengthM=vx,epochCapacity=vy,lengthEdaStorm=vz)
  
}
centroid<- p
View(centroid)


for(i in 1:k){
  cluster[[i]]$group <- i
}
print(k)
#minimum inter cluster value
minInterCluster<- 1000.0
for(i in 1:k){
  for(j in 1:k){
    if(j>i)
    {
      distance=sqrt((p[i,1]-p[j,1])^2 + (p[i,2]-p[j,2])^2
                    +  (p[i,3]-p[j,3])^2 )
      if(minInterCluster>distance){
        minInterCluster=distance
      }
    }
  }
}

#maximum intra cluster value
maxIntraCluster=-1
for(itr in 1:k){
  clst=cluster[[itr]]
  for(i in 1:nrow(cluster[[itr]])){
    for(j in 1:nrow(cluster[[itr]])){
      if(j>i)
      {
        intraDistance=sqrt((clst[i,1]-clst[j,1])^2 + 
                             (clst[i,2]-clst[j,2])^2 +
                             (clst[i,3]-clst[j,3])^2 )
        if(intraDistance>maxIntraCluster){
          maxIntraCluster=intraDistance
        }
      }
    }
  }
}

visual12 <- rbind(cluster[[1]], cluster[[2]])
for(i in 3:k){
  if(k>=i){
    visual12<- rbind(visual12,cluster[[i]])
  }
  
}

dunnIndex<-(minInterCluster/maxIntraCluster)
print(paste("Dunn Index: ",dunnIndex))

ggplot(visual12, aes(x=swsLengthM, y=epochCapacity, group=group, col=group, fill=group)) +
  geom_point()

######################################################################################################################################
######################################################################################################################################


#2c for 2b 
#100 observation 

idx <- sample.int(218,100)
v1 <- c()
v2 <- c()
v3 <- c()
v4 <- c()
v5 <- c()
for(i in 1:100)
{
  v1[i] = data[idx[i],7]
  v2[i] = data[idx[i],13]
  v3[i] = data[idx[i],15]
  v4[i] = data[idx[i],16]
  v5[i] = data[idx[i],20]
  
}
print(idx)
#Creating a table(data frame) 
df <- data.frame(swsLengthM = v1,
                 epochCapacity = v2,
                 epochPeakCounter = v3,
                 stormPeak = v4,
                 lengthEdaStorm= v5
)
View(df)
k<-9
xp<-c(0.5,1.25,1.306,1.1,-0.5,-0.23,-0.11,0.9,2.45)
yp<-c(-0.5,-0.23,-0.1,0.9,1.1,1.25,1.306,1.878,2.1)
zp<-c(-0.12,0.32,-0.5,1.9,0.1,0.95,1.236,0.18,1.9)
kp<-c(-0.11,1.32,-1.5,0.9,1.1,1.95,1.06,1.08,0.92)
lp<-c(1.12,1.04,-0.8,1.1,-0.1,0.55,0.236,-0.18,0.9)


p<-data.frame(swsLengthM=xp,epochCapacity=yp,epochPeakCounter=zp,stormPeak=kp,lengthEdaStorm=lp)
cluster <- list()
for(i in 1:k){
  cluster[[i]]=data.frame()
}
counter <- 10
while(counter!=0){
  counter=counter-1;
  
  sumx<- c()
  sumy<- c()
  sumz<- c()
  sumk<- c()
  suml<- c()
  cnt<- c()
  for(i in 1:k){
    cluster[[i]]=data.frame()
    sumx[i]=0
    sumy[i]=0
    sumz[i]=0
    sumk[i]=0
    suml[i]=0
    cnt[i]=0
  }
  
  for(i in 1:nrow(df )){
    minIndx<- 0
    value<- 0
    for(j in 1:k){
      dist=sqrt( (df[i,1]-p[j,1])^2 + (df[i,2]-p[j,2])^2 +
                   (df[i,3]-p[j,3])^2 +(df[i,4]-p[j,4])^2 +
                   (df[i,5]-p[j,5])^2)
      if(j==1)
      {
        minIndx=j
        value=dist
      }
      else if(value  >  dist)
      {
        minIndx=j
        value=dist 
      }
    }
    de1 <- data.frame(df[i,])
    cluster[[minIndx]] <- rbind(cluster[[minIndx]],de1)
    cnt[minIndx]=cnt[minIndx]+1
    sumx[minIndx]=sumx[minIndx]+df[i,1]
    sumy[minIndx]=sumy[minIndx]+df[i,2]
    sumz[minIndx]=sumz[minIndx]+df[i,3]
    sumk[minIndx]=sumk[minIndx]+df[i,4]
    suml[minIndx]=suml[minIndx]+df[i,5]
  }
  vx<-c()
  vy<-c()
  vz<-c()
  vk<-c()
  vl<-c()
  for(j in 1:k){
    if(cnt[j]!=0){
      vx[j]=sumx[j]/cnt[j]
      vy[j]=sumy[j]/cnt[j]
      vz[j]=sumz[j]/cnt[j]
      vk[j]=sumk[j]/cnt[j]
      vl[j]=suml[j]/cnt[j]
    }
    else{
      vx[j]=0
      vy[j]=0
      vz[j]=0
      vk[j]=0
      vl[j]=0
    }
    
  }
  p<-data.frame(swsLengthM=vx,epochCapacity=vy,epochPeakCounter=vz,stormPeak=vk,lengthEdaStorm=vl)
}



for(i in 1:k){
  cluster[[i]]$group <- i
}

#minimum inter cluster value
minInterCluster<- 1000.0
for(i in 1:k){
  for(j in 1:k){
    if(j>i)
    {
      distance=sqrt((p[i,1]-p[j,1])^2 + (p[i,2]-p[j,2])^2
                    +  (p[i,3]-p[j,3])^2 + (p[i,4]-p[j,4])^2 +
                      (p[i,5]-p[j,5])^2 )
      if(minInterCluster>distance){
        minInterCluster=distance
      }
    }
  }
}

#maximum intra cluster value
maxIntraCluster=-1
for(itr in 1:k){
  clst=cluster[[itr]]
  for(i in 1:nrow(cluster[[itr]])){
    for(j in 1:nrow(cluster[[itr]])){
      if(j>i)
      {
        intraDistance=sqrt((clst[i,1]-clst[j,1])^2 + 
                             (clst[i,2]-clst[j,2])^2 +
                             (clst[i,3]-clst[j,3])^2 +
                             (clst[i,4]-clst[j,4])^2 +
                             (clst[i,5]-clst[j,5])^2)
        if(intraDistance>maxIntraCluster){
          maxIntraCluster=intraDistance
        }
      }
    }
  }
}

visual12 <- rbind(cluster[[1]], cluster[[2]])
for(i in 3:k){
  if(k>=i){
    visual12<- rbind(visual12,cluster[[i]])
  }
  
}
centroid<- p
View(centroid)
dunnIndex<-(minInterCluster/maxIntraCluster)
print(paste("Dunn Index: ",dunnIndex))


ggplot(visual12, aes(x=swsLengthM, y=epochCapacity, group=group, col=group, fill=group)) +
  geom_point()

######################################################################################################################################
######################################################################################################################################


#2c for sb
#50 observation
idx <- sample.int(218,50)
v1 <- c()
v2 <- c()
v3 <- c()
v4 <- c()
v5 <- c()
for(i in 1:50)
{
  v1[i] = data[idx[i],7]
  v2[i] = data[idx[i],13]
  v3[i] = data[idx[i],15]
  v4[i] = data[idx[i],16]
  v5[i] = data[idx[i],20]
  
}
print(idx)
#Creating a table(data frame) 
df <- data.frame(swsLengthM = v1,
                 epochCapacity = v2,
                 epochPeakCounter = v3,
                 stormPeak = v4,
                 lengthEdaStorm= v5
)
View(df)
k<-9
xp<-c(0.5,1.25,1.306,1.1,-0.5,-0.23,-0.11,0.9,2.45)
yp<-c(-0.5,-0.23,-0.1,0.9,1.1,1.25,1.306,1.878,2.1)
zp<-c(-0.12,0.32,-0.5,1.9,0.1,0.95,1.236,0.18,1.9)
kp<-c(-0.11,1.32,-1.5,0.9,1.1,1.95,1.06,1.08,0.92)
lp<-c(1.12,1.04,-0.8,1.1,-0.1,0.55,0.236,-0.18,0.9)


p<-data.frame(swsLengthM=xp,epochCapacity=yp,epochPeakCounter=zp,stormPeak=kp,lengthEdaStorm=lp)
cluster <- list()
for(i in 1:k){
  cluster[[i]]=data.frame()
}
counter <- 10
while(counter!=0){
  counter=counter-1;
  
  sumx<- c()
  sumy<- c()
  sumz<- c()
  sumk<- c()
  suml<- c()
  cnt<- c()
  for(i in 1:k){
    cluster[[i]]=data.frame()
    sumx[i]=0
    sumy[i]=0
    sumz[i]=0
    sumk[i]=0
    suml[i]=0
    cnt[i]=0
  }
  
  for(i in 1:nrow(df )){
    minIndx<- 0
    value<- 0
    for(j in 1:k){
      dist=sqrt( (df[i,1]-p[j,1])^2 + (df[i,2]-p[j,2])^2 +
                   (df[i,3]-p[j,3])^2 +(df[i,4]-p[j,4])^2 +
                   (df[i,5]-p[j,5])^2)
      if(j==1)
      {
        minIndx=j
        value=dist
      }
      else if(value  >  dist)
      {
        minIndx=j
        value=dist 
      }
    }
    de1 <- data.frame(df[i,])
    cluster[[minIndx]] <- rbind(cluster[[minIndx]],de1)
    cnt[minIndx]=cnt[minIndx]+1
    sumx[minIndx]=sumx[minIndx]+df[i,1]
    sumy[minIndx]=sumy[minIndx]+df[i,2]
    sumz[minIndx]=sumz[minIndx]+df[i,3]
    sumk[minIndx]=sumk[minIndx]+df[i,4]
    suml[minIndx]=suml[minIndx]+df[i,5]
  }
  vx<-c()
  vy<-c()
  vz<-c()
  vk<-c()
  vl<-c()
  for(j in 1:k){
    if(cnt[j]!=0){
      vx[j]=sumx[j]/cnt[j]
      vy[j]=sumy[j]/cnt[j]
      vz[j]=sumz[j]/cnt[j]
      vk[j]=sumk[j]/cnt[j]
      vl[j]=suml[j]/cnt[j]
    }
    else{
      vx[j]=0
      vy[j]=0
      vz[j]=0
      vk[j]=0
      vl[j]=0
    }
    
  }
  p<-data.frame(swsLengthM=vx,epochCapacity=vy,epochPeakCounter=vz,stormPeak=vk,lengthEdaStorm=vl)
}



for(i in 1:k){
  cluster[[i]]$group <- i
}

#minimum inter cluster value
minInterCluster<- 1000.0
for(i in 1:k){
  for(j in 1:k){
    if(j>i)
    {
      distance=sqrt((p[i,1]-p[j,1])^2 + (p[i,2]-p[j,2])^2
                    +  (p[i,3]-p[j,3])^2 + (p[i,4]-p[j,4])^2 +
                      (p[i,5]-p[j,5])^2 )
      if(minInterCluster>distance){
        minInterCluster=distance
      }
    }
  }
}

#maximum intra cluster value
maxIntraCluster=-1
for(itr in 1:k){
  clst=cluster[[itr]]
  for(i in 1:nrow(cluster[[itr]])){
    for(j in 1:nrow(cluster[[itr]])){
      if(j>i)
      {
        intraDistance=sqrt((clst[i,1]-clst[j,1])^2 + 
                             (clst[i,2]-clst[j,2])^2 +
                             (clst[i,3]-clst[j,3])^2 +
                             (clst[i,4]-clst[j,4])^2 +
                             (clst[i,5]-clst[j,5])^2)
        if(intraDistance>maxIntraCluster){
          maxIntraCluster=intraDistance
        }
      }
    }
  }
}

visual12 <- rbind(cluster[[1]], cluster[[2]])
for(i in 3:k){
  if(k>=i){
    visual12<- rbind(visual12,cluster[[i]])
  }
  
}
centroid<- p
View(centroid)
dunnIndex<-(minInterCluster/maxIntraCluster)
print(paste("Dunn Index: ",dunnIndex))


ggplot(visual12, aes(x=swsLengthM, y=epochCapacity, group=group, col=group, fill=group)) +
  geom_point()

######################################################################################################################################
######################################################################################################################################



#3a
#
v1 <- data[,7]
v2 <- data[,13]
#Creating a table(data frame) 
df <- data.frame(swsLengthM = v1,
                 epochCapacity = v2
)
#9 clusters 
c<-9
#criteria for convergence 
El<- 0.01
#initialize fuzzy partion/membership matrix  
u<- list()
for(i in 1:c)
{
  rw<- c()
  for(j in 1:nrow(df )){
    
    if(i==j)
    {
      rw[j]=1
    }
    else 
    {
      if(i==1 & j>c){
        rw[j]=1
      }
      else{
        rw[j]=0
      }
      
    }
  }
  u[[i]]=rw
}
#running for maximum 10 times 
count<- 10
while(count!=0){
  count=count-1;
  #calculation of initial cluster center 
  v<- list()

  for(i in 1:c)
  {
    dum<- c()
    for(j in 1:ncol(df ))
    {
      val<- u[[i]]
      len = length(val)
      value1<- 0
      value2<- 0
      for(k in 1:len)
      {
        value1=value1+(val[k]*val[k]*df[k,j])
        value2=value2+(val[k]*val[k])
      }
      value1=value1/value2
      dum[j]=value1
    }
    v[[i]]=dum
  }
  #calculate the distance of each data from each cluster center
  d<- list()
  for(i in 1:c)
  {
    vrt<-c()
    for(j in 1:nrow(df )){
      point1<- v[[i]]
      dist= sqrt((point1[1]-df[j,1])^2 + (point1[2]-df[j,2])^2)
      vrt[j]=dist
    }
    d[[i]]=vrt
  }
  #update membership matrix
  du<-list()
  for(i in 1:c)
  {
    ran2<-d[[i]]
    rwu<- c()
    for(j in 1:nrow(df )){
      mVal <-0
      flag<- 0
      for(k in 1:c){
        ran1<-d[[k]]
        if(ran2[j]==0){
          mVal=1
        }
        else{
          mVal=mVal+ ((ran2[j]/ran1[j])^2)
        }
        
      }
      
      mVal=1/mVal
      
      rwu[j]=mVal
    }
    du[[i]]=rwu
  }
  
  #check for convergence
  
  Emax <- -1
  
  for(i in 1:c)
  {
    pu<- u[[i]]
    pdu<- du[[i]]
    for(j in 1:nrow(df )){
      
      if(abs(pu[j]-pdu[j])> Emax){
        Emax=abs(pu[j]-pdu[j])
      }
    }
  }
  u=du
  if(Emax<=El){
    break
  }
}

cluster <- list()
for(i in 1:c){
  cluster[[i]]=data.frame()
}
#making the c clusters from the membership matrix 
for(i in 1:nrow(df ))
{
  maxIndx<- 0
  value <- -1
  for(j in 1:c){
    chk<- u[[j]]
    if(value<chk[i]){
      value=chk[i]
      maxIndx=j
    }
  }
  de1 <- data.frame(df[i,])
  cluster[[maxIndx]] <- rbind(cluster[[maxIndx]],de1)
}
#grouping the clusters
for(i in 1:c){
  cluster[[i]]$group <- i
}

visual12 <- rbind(cluster[[1]], cluster[[2]])
for(i in 3:c){
  if(c>=i){
    visual12<- rbind(visual12,cluster[[i]])
  }
}

p <- v
centroid<- p
print(centroid)
#minimum inter cluster value
minInterCluster<- 1000.0
for(i in 1:c){
  for(j in 1:c){
    if(j>i)
    {
      vec1<- p[[i]]
      vec2<- p[[j]]
      distance=sqrt((vec1[1]-vec2[1])^2 + (vec1[2]-vec2[2])^2)
      if(minInterCluster>distance){
        minInterCluster=distance
      }
    }
  }
}

#maximum intra cluster value
maxIntraCluster=-1
for(itr in 1:c){
  clst=cluster[[itr]]
  for(i in 1:nrow(cluster[[itr]])){
    for(j in 1:nrow(cluster[[itr]])){
      if(j>i)
      {
        intraDistance=sqrt((clst[i,1]-clst[j,1])^2 + (clst[i,2]-clst[j,2])^2)
        if(intraDistance>maxIntraCluster){
          maxIntraCluster=intraDistance
        }
      }
    }
  }
}

dunnIndex<-(minInterCluster/maxIntraCluster)
print(paste("Dunn Index: ",dunnIndex))

#ploting the graph between swsLengthM and epochCapacity
ggplot(visual12, aes(x=swsLengthM, y=epochCapacity, group=group, col=group, fill=group)) +
  geom_point()

##############################################################################################################################
##############################################################################################################################

#3c
v1 <- data[,20]
v2 <- data[,7]
v3 <- data[,13]

#Creating a table(data frame) 
df <- data.frame(lengthEdaStorm   = v1,
                 swsLengthM = v2,
                 epochCapacity =v3
                 
)
c<-9
El<- 0.01
u<- list()
for(i in 1:c)
{
  rw<- c()
  for(j in 1:nrow(df )){
    
    if(i==j)
    {
      rw[j]=1
    }
    else 
    {
      if(i==1 & j>c){
        rw[j]=1
      }
      else{
        rw[j]=0
      }
      
    }
  }
  u[[i]]=rw
}
count<- 10
while(count!=0){
  count=count-1;
  v<- list()
  for(i in 1:c)
  {
    dum<- c()
    for(j in 1:ncol(df ))
    {
      val<- u[[i]]
      len = length(val)
      value1<- 0
      value2<- 0
      for(k in 1:len)
      {
        value1=value1+(val[k]*val[k]*df[k,j])
        value2=value2+(val[k]*val[k])
      }
      value1=value1/value2
      dum[j]=value1
    }
    v[[i]]=dum
  }
  d<- list()
  for(i in 1:c)
  {
    vrt<-c()
    for(j in 1:nrow(df)){
      point1<- v[[i]]
      dist= sqrt((point1[1]-df[j,1])^2 + (point1[2]-df[j,2])^2 + (point1[3]-df[j,3])^2 )
      vrt[j]=dist
    }
    d[[i]]=vrt
  }
  
  du<-list()
  for(i in 1:c)
  {
    ran2<-d[[i]]
    rwu<- c()
    for(j in 1:nrow(df )){
      mVal <-0
      flag<- 0
      for(k in 1:c){
        ran1<-d[[k]]
        if(ran2[j]==0){
          mVal=1
        }
        else{
          mVal=mVal+ ((ran2[j]/ran1[j])^2)
        }
        
      }
      
      mVal=1/mVal
      
      rwu[j]=mVal
    }
    du[[i]]=rwu
  }
  
  
  
  Emax <- -1
  
  for(i in 1:c)
  {
    pu<- u[[i]]
    pdu<- du[[i]]
    for(j in 1:nrow(df )){
      
      if(abs(pu[j]-pdu[j])> Emax){
        Emax=abs(pu[j]-pdu[j])
      }
    }
  }
  u=du
  if(Emax<=El){
    break
  }
}

cluster <- list()
for(i in 1:c){
  cluster[[i]]=data.frame()
}

for(i in 1:nrow(df ))
{
  maxIndx<- 0
  value <- -1
  for(j in 1:c){
    chk<- u[[j]]
    if(value<chk[i]){
      value=chk[i]
      maxIndx=j
    }
  }
  de1 <- data.frame(df[i,])
  cluster[[maxIndx]] <- rbind(cluster[[maxIndx]],de1)
}

for(i in 1:c){
  cluster[[i]]$group <- i
}

visual12 <- rbind(cluster[[1]], cluster[[2]])
for(i in 3:c){
  if(c>=i){
    visual12<- rbind(visual12,cluster[[i]])
  }
}

p <- v
centroid<- p
print(centroid)
#minimum inter cluster value
minInterCluster<- 1000.0
for(i in 1:c){
  for(j in 1:c){
    if(j>i)
    {
      vec1<- p[[i]]
      vec2<- p[[j]]
      distance=sqrt((vec1[1]-vec2[1])^2 + (vec1[2]-vec2[2])^2)
      if(minInterCluster>distance){
        minInterCluster=distance
      }
    }
  }
}

#maximum intra cluster value
maxIntraCluster=-1
for(itr in 1:c){
  clst=cluster[[itr]]
  for(i in 1:nrow(cluster[[itr]])){
    for(j in 1:nrow(cluster[[itr]])){
      if(j>i)
      {
        intraDistance=sqrt((clst[i,1]-clst[j,1])^2 + (clst[i,2]-clst[j,2])^2)
        if(intraDistance>maxIntraCluster){
          maxIntraCluster=intraDistance
        }
      }
    }
  }
}

dunnIndex<-(minInterCluster/maxIntraCluster)
print(paste("Dunn Index: ",dunnIndex))

ggplot(visual12, aes(x=swsLengthM, y=epochCapacity, group=group, col=group, fill=group)) +
  geom_point()

#########################################################################
#########################################################################
#Bonus 
sampleData <- c(100,50,25)
DunnIndex <- c()
SampleSize <- c()
for(sample in 1:3){
  
  idx <- sample.int(218,sampleData[sample])
  v1 <- c()
  v2 <- c()
  for(i in 1:sampleData[sample])
  {
    v1[i] = data[idx[i],7]
    v2[i] = data[idx[i],13]
  }
  
  #print(idx) #printing the index number
  
  #Creating a table(data frame) 
  df <- data.frame(swsLengthM = v1,
                   epochCapacity =v2
  )
  k<-9
  #View(df) 
  
  #Selecting 9 random numbers for variables swsLengthM and epochCapacity
  xp<-c(0.5,1.25,1.306,1.1,-0.5,-0.23,-0.11,0.9,2.45,3.142)
  yp<-c(-0.5,-0.23,-0.1,0.9,1.1,1.25,1.306,1.878,2.1,0.5)
  
  
  p<-data.frame(swsLengthM=xp,epochCapacity=yp)
  #creating a k empty clusters
  cluster <- list()
  
  for(i in 1:k){
    cluster[[i]]=data.frame()
  }
  #running a loop for max 10 times
  counter <- 10
  while(counter!=0){
    counter=counter-1;
    sumx<- c()
    sumy<- c()
    cnt<- c()
    for(i in 1:k){
      cluster[[i]]=data.frame()
      sumx[i]=0
      sumy[i]=0
      cnt[i]=0
    }
    
    for(i in 1:nrow(df )){
      minIndx<- 0
      value<- 0
      #calculating the closest cluster for every datapoint
      for(j in 1:k){
        dist=sqrt((df[i,1]-p[j,1])^2 + (df[i,2]-p[j,2])^2)
        if(j==1)
        {
          minIndx=j
          value=dist
        }
        else if(value  >  dist)
        {
          minIndx=j
          value=dist 
        }
      }
      de1 <- data.frame(df[i,])
      cluster[[minIndx]] <- rbind(cluster[[minIndx]],de1)
      cnt[minIndx]=cnt[minIndx]+1
      sumx[minIndx]=sumx[minIndx]+df[i,1]
      sumy[minIndx]=sumy[minIndx]+df[i,2]
    }
    
    vx<-c()
    vy<-c()
    #calculating the mean value 
    for(j in 1:k){
      if(cnt[j] == 0){
        vx[j] = 0
        vy[j] = 0
      }
      else{
        vx[j]=sumx[j]/cnt[j]
        vy[j]=sumy[j]/cnt[j]
      }
    }
    p<-data.frame(swsLengthM=vx,epochCapacity=vy)
    
  }
  centroid<-p
  View(centroid)
  
  colourList <- c('blue','red','yellow','green','brown',
                  'darkgray','lightcoral','cyan','magenta')
  
  
  
  
  
  
  #minimum inter cluster value
  minInterCluster<- 1000.0
  for(i in 1:k){
    for(j in 1:k){
      if(j>i)
      {
        distance=sqrt((p[i,1]-p[j,1])^2 + (p[i,2]-p[j,2])^2)
        if(minInterCluster>distance){
          minInterCluster=distance
        }
      }
    }
  }
  #maximum intra cluster value
  maxIntraCluster=-1
  for(itr in 1:k){
    clst=cluster[[itr]]
    if (length(clst) != 0) {
      
      for(i in 1:nrow(cluster[[itr]])){
        for(j in 1:nrow(cluster[[itr]])){
          if(j>i)
          {
            intraDistance=sqrt((clst[i,1]-clst[j,1])^2 + (clst[i,2]-clst[j,2])^2)
            if(intraDistance>maxIntraCluster){
              maxIntraCluster=intraDistance
            }
          }
        }
      }
    }
  }
  for(i in 1:k){
    if (length(cluster[[i]]) != 0) {
      cluster[[i]]$group <- i
    }
    
  }
  visual12 <- rbind(cluster[[1]], cluster[[2]])
  for(i in 3:k){
    if(k>=i){
      visual12<- rbind(visual12,cluster[[i]])
    }
    
  }
  print(paste("K-> ",k))
  dunnIndex<-(minInterCluster/maxIntraCluster)
  print(paste("Dunn Index: ",dunnIndex))
  SampleSize[sample]=sampleData[sample]
  DunnIndex[sample]=dunnIndex
}
visual12 <- data.frame(SampleSize = SampleSize,
                       DunnIndex =DunnIndex )
ggplot(visual12, aes(x=SampleSize, y=DunnIndex)) +
  geom_point()


