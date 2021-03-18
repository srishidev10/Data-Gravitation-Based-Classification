#W2 = c(0.15,0.3,0.45,.3 ) #94
#W2 = c(0.15,0.3,0.3,0.45) #95.33
W2 = c(0,0.15,0.45,0.3) #96.667
crossValidate <- function(x0,Tb,W1){
  print(W1)
  for (k in 1:15){
    R = matrix(data = 0,1,3)
    for (fea in 1:4){
      R[1] = R[1] + W1[fea]*((x0[1,fea] - Tb[k,fea])**2)
      R[2] = R[2] + W1[fea]*((x0[2,fea] - Tb[k,fea])**2)
      R[3] = R[3] + W1[fea]*((x0[3,fea] - Tb[k,fea])**2)
    }
    R = sqrt(R)
    Tb[k,6] = which.min(R)
    
  }
  #print(Tb)
  total = 0
  for(v1 in (1:15)) {
    if(Tb[v1,5] == Tb[v1,6]){
      total = total + 1
    }
  }
  f1 = total / 15
  return (f1)
}

centroid <- function(Ta){
  x01 = matrix(data = 0,3,5)
  for(i in seq(1,135)){
    if(Ta[i,5]==1){
      x01[1,]=x01[1,]+Ta[i,1:5]
    }
    if(Ta[i,5]==2){
      x01[2,]=x01[2,]+Ta[i,1:5]
    }
    if(Ta[i,5]==3){
      x01[3,]=x01[3,]+Ta[i,1:5]
    }
  }
  for(j in 1:3){
    x01[j,] = x01[j,]/45
    x01[j,5] = 0
  }
    
  i = 1
  x1 = matrix(data = 0,27,col+1) # to store x,m,y
  d=1
  for(i in seq(1,135,5)){
    for(j in 1:col-1){
      for(k in 0:4){
        if((i+k) <=  135){
          x1[d,j]=x1[d,j]+Ta[(i+k),j]
          x1[d,6] = Ta[(i+k),6]
        }
      }
    }
    x1[d,]=x1[d,]/5
    x1[d,5]=5
    d=d+1
  }
  print(d)
  #print(x1)
  i=1
  visited = matrix(data = 0,1,27)
  x0t = matrix(data = 0,3,5)
  while(TRUE){
  #print(x1[i,1:4] - x0[x1[i,6],1:4])
  if(all((x1[i,1:4] - x01[x1[i,6],1:4]) < epsilon[x1[i,6]]) && visited[i] == 0 ){
    x01[x1[i,6],1:4]=(x01[x1[i,6],1:4]+x1[i,1:4])/2
    x01[x1[i,6],5] = x01[x1[i,6],5] +  1
    visited[i] = 1
    #print(i)
  }
  i=i+1
  if(i > 27){
    i=1
    
    if((all(visited) == 1) || x0t == x01)
      break
    
  }
  x0t = x01
  }
  return (x01)  
}


setwd('G:/Programs/Sem 5/Package/AI package')
dgc <- read.csv("Iris data.csv")
row = nrow(dgc)
col = ncol(dgc)

#print(dgc)
data1 = c()
for(i in 1:col) {
  data1 = c(data1,c(dgc[,i]))
}

data = matrix(data = data1,row,col)
Ta = matrix(data = 0,135,col+1)
Tb = matrix(data = 0,15,col+1)
p = table(dgc $ b)
j=1

Ta[1:45,1:5] = data[1:45,]
Ta[46:90,1:5] = data[51:95,]
Ta[91:135,1:5] = data[101:145,]

Tb[1:5,1:5] = data[46:50,]
Tb[6:10,1:5] = data[96:100,]
Tb[11:15,1:5] = data[146:150,]

epsilon = matrix(data = 0,1,3)
epsilon[1] = 0.4
epsilon[2] = 0.3
epsilon[3] = 0.3
x0 = matrix(data = 0,3,5)

#seprating the l=75 training data to l' = 15
x0 = centroid(Ta)
#print(x0)
s0=colSums(x0)
#s1=colSums(x1)

W = matrix(data = 0,1,4)
W1 = matrix(data = 0,1,4)
f = 1.00
i = 1
f0 = 1.00
print('h1')
while(i < 10 && f <= f0){
  x = runif(1,min = 1, max = 10)
  x = x * 10
  x = ceiling(x)
  x = (x %% 10) %% 5
  eps = 0.15
  #x = i %% 4
  #x =x+1 
  W1 = matrix(data = 0,1,4)
  #print(W)
  W1 = W
  W1[x] = W[x] + eps
  #print(W1)
  f1 = crossValidate(x0,Tb,W1)
  i = i + 1
  #print(f1)
  if(f1 <= f){
    f = f1
    W = W1
  }
  # else{
  #   W1 = W
  # }
}
cat("accuracy is",f1)

## for doing 10 fold validation
T1 = matrix(data = 0,15,col+1)
T2 = matrix(data = 0,15,col+1)
T3 = matrix(data = 0,15,col+1)
T4 = matrix(data = 0,15,col+1)
T5 = matrix(data = 0,15,col+1)
T6 = matrix(data = 0,15,col+1)
T7 = matrix(data = 0,15,col+1)
T8 = matrix(data = 0,15,col+1)
T9 = matrix(data = 0,15,col+1)
T10 = matrix(data = 0,15,col+1)

T1[1:5,1:5] = data[1:5,]
T1[6:10,1:5] = data[51:55,]
T1[11:15,1:5] = data[101:105,]

T2[1:5,1:5] = data[6:10,]
T2[6:10,1:5] = data[56:60,]
T2[11:15,1:5] = data[106:110,]

T3[1:5,1:5] = data[11:15,]
T3[6:10,1:5] = data[61:65,]
T3[11:15,1:5] = data[111:115,]

T4[1:5,1:5] = data[16:20,]
T4[6:10,1:5] = data[66:70,]
T4[11:15,1:5] = data[116:120,]

T5[1:5,1:5] = data[21:25,]
T5[6:10,1:5] = data[71:75,]
T5[11:15,1:5] = data[121:125,]

T6[1:5,1:5] = data[26:30,]
T6[6:10,1:5] = data[76:80,]
T6[11:15,1:5] = data[126:130,]

T7[1:5,1:5] = data[31:35,]
T7[6:10,1:5] = data[81:85,]
T7[11:15,1:5] = data[131:135,]

T8[1:5,1:5] = data[36:40,]
T8[6:10,1:5] = data[86:90,]
T8[11:15,1:5] = data[136:140,]

T9[1:5,1:5] = data[41:45,]
T9[6:10,1:5] = data[91:95,]
T9[11:15,1:5] = data[141:145,]

T10[1:5,1:5] = data[46:50,]
T10[6:10,1:5] = data[96:100,]
T10[11:15,1:5] = data[146:150,]

f1 = 0
#T10 as testdata
Ta = matrix(data = 0,135,col+1)
Tb = matrix(data = 0,15,col+1)
x01 = matrix(data = 0,3,5)
Tb = T10
Ta[1:15,] = T1
Ta[16:30,] = T2
Ta[31:45,] = T3
Ta[46:60,] = T4
Ta[61:75,] = T5
Ta[76:90,] = T6
Ta[91:105,] = T7
Ta[106:120,] = T8
Ta[121:135,] = T9
x01 = centroid(Ta)
f10 = crossValidate(x01,Tb,W2)

f1 = f1 + f10


#T9 as testdata
Ta = matrix(data = 0,135,col+1)
Tb = matrix(data = 0,15,col+1)
x01 = matrix(data = 0,3,5)
Tb = T9
Ta[1:15,] = T1
Ta[16:30,] = T2
Ta[31:45,] = T3
Ta[46:60,] = T4
Ta[61:75,] = T5
Ta[76:90,] = T6
Ta[91:105,] = T7
Ta[106:120,] = T8
Ta[121:135,] = T10
x01 = centroid(Ta)
f10 = crossValidate(x01,Tb,W2)

f1 = f1 + f10
#f1 = f1 / 2
#cat("\naccuracy is",f10)

#T8 as testdata
Ta = matrix(data = 0,135,col+1)
Tb = matrix(data = 0,15,col+1)
x01 = matrix(data = 0,3,5)
Tb = T8
Ta[1:15,] = T1
Ta[16:30,] = T2
Ta[31:45,] = T3
Ta[46:60,] = T4
Ta[61:75,] = T5
Ta[76:90,] = T6
Ta[91:105,] = T7
Ta[106:120,] = T9
Ta[121:135,] = T10
x01 = centroid(Ta)
f10 = crossValidate(x01,Tb,W2)


f1 = f1 + f10
#f1 = f1 / 2
#cat("\naccuracy is",f10)

#T7 as testdat???

Ta = matrix(data = 0,135,col+1)
Tb = matrix(data = 0,15,col+1)
x01 = matrix(data = 0,3,5)
Tb = T7
Ta[1:15,] = T1
Ta[16:30,] = T2
Ta[31:45,] = T3
Ta[46:60,] = T4
Ta[61:75,] = T5
Ta[76:90,] = T6
Ta[91:105,] = T9
Ta[106:120,] = T8
Ta[121:135,] = T10
x01 = centroid(Ta)
f10 = crossValidate(x01,Tb,W2)
#cat("\naccuracy is",f10)
f1 = f1 + f10
#f1 = f1 / 2

#T6 as testdata
Ta = matrix(data = 0,135,col+1)
Tb = matrix(data = 0,15,col+1)
x01 = matrix(data = 0,3,5)
Tb = T6
Ta[1:15,] = T1
Ta[16:30,] = T2
Ta[31:45,] = T3
Ta[46:60,] = T4
Ta[61:75,] = T5
Ta[76:90,] = T9
Ta[91:105,] = T7
Ta[106:120,] = T8
Ta[121:135,] = T10
x01 = centroid(Ta)
f10 = crossValidate(x01,Tb,W2)
#cat("\naccuracy is",f10)
f1 = f1 + f10
#f1 = f1 / 2

#T5 as testdata
Ta = matrix(data = 0,135,col+1)
Tb = matrix(data = 0,15,col+1)
x01 = matrix(data = 0,3,5)
Tb = T5
Ta[1:15,] = T1
Ta[16:30,] = T2
Ta[31:45,] = T3
Ta[46:60,] = T4
Ta[61:75,] = T9
Ta[76:90,] = T6
Ta[91:105,] = T7
Ta[106:120,] = T8
Ta[121:135,] = T10
x01 = centroid(Ta)
f10 = crossValidate(x01,Tb,W2)
#cat("\naccuracy is",f10)
f1 = f1 + f10
#f1 = f1 / 2

#T4 as testdata
Ta = matrix(data = 0,135,col+1)
Tb = matrix(data = 0,15,col+1)
x01 = matrix(data = 0,3,5)
Tb = T4
Ta[1:15,] = T1
Ta[16:30,] = T2
Ta[31:45,] = T3
Ta[46:60,] = T9
Ta[61:75,] = T5
Ta[76:90,] = T6
Ta[91:105,] = T7
Ta[106:120,] = T8
Ta[121:135,] = T10
x01 = centroid(Ta)

f10 = crossValidate(x01,Tb,W2)
#cat("\naccuracy is",f10)
f1 = f1 + f10
#f1 = f1 / 2

#T3 as testdata
Ta = matrix(data = 0,135,col+1)
Tb = matrix(data = 0,15,col+1)
x01 = matrix(data = 0,3,5)
Tb = T3
Ta[1:15,] = T1
Ta[16:30,] = T2
Ta[31:45,] = T9
Ta[46:60,] = T4
Ta[61:75,] = T5
Ta[76:90,] = T6
Ta[91:105,] = T7
Ta[106:120,] = T8
Ta[121:135,] = T10
x01 = centroid(Ta)

f10 = crossValidate(x01,Tb,W2)
#cat("\naccuracy is",f10)
f1 = f1 + f10
#f1 = f1 / 2

#T2 as testdata
Ta = matrix(data = 0,135,col+1)
Tb = matrix(data = 0,15,col+1)
x01 = matrix(data = 0,3,5)
Tb = T2
Ta[1:15,] = T1
Ta[16:30,] = T9
Ta[31:45,] = T3
Ta[46:60,] = T4
Ta[61:75,] = T5
Ta[76:90,] = T6
Ta[91:105,] = T7
Ta[106:120,] = T8
Ta[121:135,] = T10
x01 = centroid(Ta)

f10 = crossValidate(x01,Tb,W2)
#cat("\naccuracy is",f10)
f1 = f1 + f10
#f1 = f1 / 2

#T1 as testdata
Ta = matrix(data = 0,135,col+1)
Tb = matrix(data = 0,15,col+1)
x01 = matrix(data = 0,3,5)
Tb = T1
Ta[1:15,] = T9
Ta[16:30,] = T2
Ta[31:45,] = T3
Ta[46:60,] = T4
Ta[61:75,] = T5
Ta[76:90,] = T6
Ta[91:105,] = T7
Ta[106:120,] = T8
Ta[121:135,] = T10
x01 = centroid(Ta)
f10 = crossValidate(x01,Tb,W2)
#cat("\naccuracy is",f10)
f1 = f1 + f10
f1 = f1 / 10

f1=f1*100
cat('\naccuracy is ',f1)