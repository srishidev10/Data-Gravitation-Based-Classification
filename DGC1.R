#W4 = c(30.5, 4.25 ,101.25,99 ,0 ,36.25 ,7 ,95.25, 43.25,0 ,83.25, 10.25,0)
crossValidate <- function(x0,Tb,W4,l2) {
    #print(W4)
    for(k in 1:l2){
    R = matrix(data = 0,1,3)
    for(fea in 1:13) {
      R[1] = R[1] + W4[fea]*((x0[1,fea] - Tb[k,fea])**2)
      R[2] = R[2] + W4[fea]*((x0[2,fea] - Tb[k,fea])**2)
      R[3] = R[3] + W4[fea]*((x0[3,fea] - Tb[k,fea])**2)
    }
    R = sqrt(R)
    #print(R)
    Tb[k,15] = which.min(R) 
  }
  #print(Tb)
  total = 0
  for(v1 in (1:l2)) {
    if(Tb[v1,14] == Tb[v1,15]) {
      total = total + 1
    }
  }
  #print(total)
  f1 = total/l2
  #print(Tb)
  return(f1)
}
centroid <- function(Ta,l1,l2) {
  x01 = matrix(data = 0,3,14)
  c1 = 0 #for class 1
  c2 = 0 #for class 2
  c3 = 0 #for class 3 
  for( i in seq(1,l1)){
    if(Ta[i,14] == 1 ){
      x01[1,1:13] = x01[1,1:13] + Ta[i,1:13]
      c1 = c1 + 1
    }
    if(Ta[i,14] == 2 ){
      x01[2,1:13] = x01[2,1:13] + Ta[i,1:13]
      c2 = c2 + 1
    }
    if(Ta[i,14] == 3) {
      x01[3,1:13] = x01[3,1:13] + Ta[i,1:13]
      c3 = c3 + 1
    }
      
  }
  #print(x01)
  #print(colSums(x01))
  #print(colSums(Ta))
  x01[1,14] = 1
  x01[2,14] = 2
  x01[3,14] = 3
  x01[1,1:13] = x01[1,1:13] / c1
  x01[2,1:13] = x01[2,1:13] / c2
  x01[3,1:13] = x01[3,1:13] / c3
  #print(x01)
  return(x01)
  #ceiling((c1+c2+c3)/5)
  #x1 = matrix(data = 0,l1,col+1)
  x1 = matrix(data = 0,26,col+1)
  #x1 = Ta
  
  print(c1)
  print(c2)
  print(c3)
  d=1
  for(i in seq(1,(c1),6)) {
    for(j in 1:col-1){
      for(k in 0:5) {
        if((i+k) <= (c1)) {
          x1[d,j] = x1[d,j] + Ta[(i+k),j]
          x1[d,14] = Ta[(i+k),14]
        }
      }
    }
    x1[d,1:13] = x1[d,1:13] / 6
    x1[d,15] = 6
    d=d+1
    #print(d)
  }
  #print(d)
  for(i in seq(c1+1,(c1+c2),9)) {
    for(j in 1:col-1){
      for(k in 0:8) {
        if((i+k) <= (c1+c2)) {
          x1[d,j] = x1[d,j] + Ta[(i+k),j]
          x1[d,14] = Ta[(i+k),14]
        }
      }
    }
    x1[d,1:13] = x1[d,1:13] / 9
    x1[d,15] = 9
    d=d+1
    
  }
  #print(d)
  for(i in seq(c1+c2+1,(c1+c2+c3),11)) {
    for(j in 1:col-1){
      for(k in 0:10) {
        if((i+k) <= (c1+c2+c3)) {
          x1[d,j] = x1[d,j] + Ta[(i+k),j]
          x1[d,14] = Ta[(i+k),14]
        }
      }
    }
    x1[d,1:13] = x1[d,1:13] / 11
    x1[d,15] = 11
    d=d+1
    #print(d)
  }
  #print(x1[140,])
  i=1
  visited = matrix(data = 0,1,26)
  x0t = matrix(data = 0,3,14)
  while(TRUE) {
    if(all((x1[i,1:13] - x01[x1[i,14],1:13]) < epsilon[x1[i,14]]) && visited[i] == 0) {
      x01[x1[i,14],1:13] = (x01[x1[i,14],1:13] + x1[i,1:13])/2
      #x01[x1[i,14],14] = x01[x1[i,14],14] + 1
      visited[i] = 1
    }
    i=i+1
    if(i > 26){
      i=1
      if((all(visited) == 1) || x0t == x01)
        break

    }
    x0t = x01
  }
  print(x01)
  return(x01)
}
setwd('G:/Programs/Sem 5/Package/AI package')
dgc <- read.csv("wine.csv")
row = nrow(dgc)
col = ncol(dgc)
p = table(dgc$class)
#dgc = dgc[order(dgc$class),]
#print(dgc)
data1 = c()
for(i in 1:col) {
  data1 = c(data1,c(dgc[,i]))
}

data = matrix(data = data1,row,col)
#print(data)
Ta = matrix(data = 0,161,col+1)
Tb = matrix(data = 0,17,col+1)
# 
# 
j=1
T1 = matrix(data = 0,18,col+1)
T2 = matrix(data = 0,18,col+1)
T3 = matrix(data = 0,18,col+1)
T4 = matrix(data = 0,18,col+1)
T5 = matrix(data = 0,18,col+1)
T6 = matrix(data = 0,18,col+1)
T7 = matrix(data = 0,18,col+1)
T8 = matrix(data = 0,18,col+1)
T9 = matrix(data = 0,17,col+1)
T10 = matrix(data = 0,17,col+1)
# 
T1[1:6,1:14] = data[1:6,]
T1[7:13,1:14] = data[60:66,]
T1[14:18,1:14] = data[131:135,]

T2[1:6,1:14] = data[7:12,]
T2[7:13,1:14] = data[67:73,]
T2[14:18,1:14] = data[136:140,]

T3[1:6,1:14] = data[13:18,]
T3[7:13,1:14] = data[74:80,]
T3[14:18,1:14] = data[141:145,]

T4[1:6,1:14] = data[19:24,]
T4[7:13,1:14] = data[81:87,]
T4[14:18,1:14] = data[146:150,]

T5[1:6,1:14] = data[25:30,]
T5[7:13,1:14] = data[88:94,]
T5[14:18,1:14] = data[151:155,]

T6[1:6,1:14] = data[31:36,]
T6[7:13,1:14] = data[95:101,]
T6[14:18,1:14] = data[156:160,]

T7[1:6,1:14] = data[37:42,]
T7[7:13,1:14] = data[102:108,]
T7[14:18,1:14] = data[161:165,]

T8[1:6,1:14] = data[43:48,]
T8[7:13,1:14] = data[109:115,]
T8[14:18,1:14] = data[166:170,]

T9[1:6,1:14] = data[49:54,]
T9[7:13,1:14] = data[116:122,]
T9[14:17,1:14] = data[171:174,]

T10[1:5,1:14] = data[55:59,]
T10[6:13,1:14] = data[123:130,]
T10[14:17,1:14] = data[175:178,]

# 
Ta[1:18,] = T1
Ta[19:36,] = T2
Ta[37:54,] = T3
Ta[55:72,] = T4
Ta[73:90,] = T5
Ta[91:108,] = T6
Ta[109:126,] = T7
Ta[127:144,] = T8
Ta[145:161,] = T9

Tb[1:17,] = T10
# 
# 
epsilon = matrix(data = 0,1,3)
epsilon[1] = 0.5
epsilon[2] = 0.5
epsilon[3] = 0.5
x0 = matrix(data = 0,3,col)

# 
# # #seprating the l=75 training data to l' = 15
l1 = 161
l2=17
x0 = centroid(Ta,l1,l2)
print(x0)
# 
W = matrix(data = 0,1,13)
W1 = matrix(data = 0,1,13)
f = 1.00
i = 1
f0 = 1.00
while( i < 15 && f <= f0){
  x = runif(1,min = 0, max = 13)
  #x = x * 10
  x = ceiling(x)
  #x = (x %% 10) %% 5
  eps = 0.25
  #x = i %% 4
  #x =x+1
  W1 = matrix(data = 0,1,13)
  #print(W)
  W1 = W
  W1[x] = W[x] + eps
  f1 = crossValidate(x0,Tb,W1,l2)
  i=i+1
  if(f1 <= f) {
    f=f1
    W=W1
  }
}
print(f1*100)
#W4 = c(30.5, 4.25 ,101.25,99 ,0 ,36.25 ,7 ,95.25, 43.25,0 ,83.25, 10.25,0)
#W4 = c(0.25,94,79.5,0,0,0,0,56 ,7.75,0,4.5,0  ,0.25)
#W4 = c(0,0.25,0,0,0,0,0, 0.25,0 ,0 , 0.25 ,1,0)
#W4 = c(0,0.25,0,0,0,0,0,0.25,0,0,0.25,1,0)
W1 = c(0.25,0,0,0,0,0,0,0.5,0,0,0.25,0.5,0)
f1 = 0
##10 fold
f10 = crossValidate(x0,Tb,W1,l2)
f1 = f1 + f10
#print(f10)

## T9 as test data
Ta = matrix(data = 0,161,col+1)
Tb = matrix(data = 0,17,col+1)
Ta[1:18,] = T1
Ta[19:36,] = T2
Ta[37:54,] = T3
Ta[55:72,] = T4
Ta[73:90,] = T5
Ta[91:108,] = T6
Ta[109:126,] = T7
Ta[127:144,] = T8
Ta[145:161,] = T10

Tb[1:17,] = T9

l1 = 161
l2 = 17
x0 = centroid(Ta,l1,l2)
f10 = crossValidate(x0,Tb,W1,l2)
f1= f1 + f10 

## T8 as test data
Ta = matrix(data = 0,160,col+1)
Tb = matrix(data = 0,18,col+1)
Ta[1:18,] = T1
Ta[19:36,] = T2
Ta[37:54,] = T3
Ta[55:72,] = T4
Ta[73:90,] = T5
Ta[91:108,] = T6
Ta[109:126,] = T7
Ta[127:143,] = T9
Ta[144:160,] = T10

Tb[1:18,] = T8

l1 = 160
l2 = 18
x0 = centroid(Ta,l1,l2)
f10 = crossValidate(x0,Tb,W1,l2)
f1= f1 + f10 

## T7 as test data
Ta = matrix(data = 0,160,col+1)
Tb = matrix(data = 0,18,col+1)
Ta[1:18,] = T1
Ta[19:36,] = T2
Ta[37:54,] = T3
Ta[55:72,] = T4
Ta[73:90,] = T5
Ta[91:108,] = T6
Ta[109:126,] = T8
Ta[127:143,] = T9
Ta[144:160,] = T10

Tb[1:18,] = T7

l1 = 160
l2 = 18
x0 = centroid(Ta,l1,l2)
f10 = crossValidate(x0,Tb,W1,l2)
f1= f1 + f10 

## T6 as test data
Ta = matrix(data = 0,160,col+1)
Tb = matrix(data = 0,18,col+1)
Ta[1:18,] = T1
Ta[19:36,] = T2
Ta[37:54,] = T3
Ta[55:72,] = T4
Ta[73:90,] = T5
Ta[91:108,] = T8
Ta[109:126,] = T7
Ta[127:143,] = T9
Ta[144:160,] = T10

Tb[1:18,] = T6

l1 = 160
l2 = 18
x0 = centroid(Ta,l1,l2)

f10 = crossValidate(x0,Tb,W1,l2)
f1= f1 + f10 

## T5 as test data
Ta = matrix(data = 0,160,col+1)
Tb = matrix(data = 0,18,col+1)
Ta[1:18,] = T1
Ta[19:36,] = T2
Ta[37:54,] = T3
Ta[55:72,] = T4
Ta[73:90,] = T8
Ta[91:108,] = T6
Ta[109:126,] = T7
Ta[127:143,] = T9
Ta[144:160,] = T10

Tb[1:18,] = T5

l1 = 160
l2 = 18
x0 = centroid(Ta,l1,l2)

f10 = crossValidate(x0,Tb,W1,l2)
f1= f1 + f10 

## T4 as test data
Ta = matrix(data = 0,160,col+1)
Tb = matrix(data = 0,18,col+1)
Ta[1:18,] = T1
Ta[19:36,] = T2
Ta[37:54,] = T3
Ta[55:72,] = T8
Ta[73:90,] = T5
Ta[91:108,] = T6
Ta[109:126,] = T7
Ta[127:143,] = T9
Ta[144:160,] = T10

Tb[1:18,] = T4

l1 = 160
l2 = 18
x0 = centroid(Ta,l1,l2)
#print(x0)
f10 = crossValidate(x0,Tb,W1,l2)
f1= f1 + f10 

## T3 as test data
Ta = matrix(data = 0,160,col+1)
Tb = matrix(data = 0,18,col+1)
Ta[1:18,] = T1
Ta[19:36,] = T2
Ta[37:54,] = T8
Ta[55:72,] = T4
Ta[73:90,] = T5
Ta[91:108,] = T6
Ta[109:126,] = T7
Ta[127:143,] = T9
Ta[144:160,] = T10

Tb[1:18,] = T3

l1 = 160
l2 = 18
x0 = centroid(Ta,l1,l2)
f10 = crossValidate(x0,Tb,W1,l2)
f1= f1 + f10 

## T2 as test data
Ta = matrix(data = 0,160,col+1)
Tb = matrix(data = 0,18,col+1)
Ta[1:18,] = T1
Ta[19:36,] = T8
Ta[37:54,] = T3
Ta[55:72,] = T4
Ta[73:90,] = T5
Ta[91:108,] = T6
Ta[109:126,] = T7
Ta[127:143,] = T9
Ta[144:160,] = T10

Tb[1:18,] = T2

l1 = 160
l2 = 18
x0 = centroid(Ta,l1,l2)
f10 = crossValidate(x0,Tb,W1,l2)
f1= f1 + f10 

## T1 as test data
Ta = matrix(data = 0,160,col+1)
Tb = matrix(data = 0,18,col+1)
Ta[1:18,] = T8
Ta[19:36,] = T2
Ta[37:54,] = T3
Ta[55:72,] = T4
Ta[73:90,] = T5
Ta[91:108,] = T6
Ta[109:126,] = T7
Ta[127:143,] = T9
Ta[144:160,] = T10

Tb[1:18,] = T1

l1 = 160
l2 = 18
x0 = centroid(Ta,l1,l2)
#print(x0)
f10 = crossValidate(x0,Tb,W1,l2)
f1= f1 + f10
f1= f1 /10
#print(x0)
cat("accuracy is", f1*100)