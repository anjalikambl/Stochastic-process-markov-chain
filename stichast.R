s=c(0,1)
p0=c(0,1)
p=matrix(c(0.3,0.6,0.7,0.4),nrow=2)
rowSums(p)
x={}
x=sample(s,1,prob=p0)
x
for(i in 1:15){
  x[i+1]=sample(s,1,prob=p[x[i]+1,])
}
x

#P(X10=0|X0=1)
matpow=function(a,n){
  a1=a
  for(i in 2:n){
    a1=a1%*%a
  }
  return(a1)
}
matpow(p,10)


s=c(0,1)
p0=c(0,1)
p=matrix(c(0.3,0.6,0.7,0.4),nrow=2)
rowSums(p)
y={};N=10000;t=0

for(j in 1:N){
x={}
x=sample(s,1,prob=p0)

for(i in 1:10){
  x[i+1]=sample(s,1,prob=p[x[i]+1,])
}
y[j]=x[11]
}
mean(y==0)
print(y)

#P(X10=0)

s=c(0,1)
p0=c(0,1)
p=matrix(c(0.3,0.6,0.7,0.4),nrow=2)
rowSums(p)
y={}
N=10000;t=0
for(j in 1:N){
x={}
x=sample(s,1,prob=p0)
for(i in 1:20){
  x[i+1]=sample(s,1,prob=p[x[i]+1,])
}
 if(x[11]==0)t=t+1
}

t/N

#E(X10)
s=c(0,1)
p0=c(0,1)
p=matrix(c(0.3,0.6,0.7,0.4),nrow=2)
rowSums(p)
y={};N=10000
for(j in 1:N){
x={}
x=sample(s,1,prob=p0)

for(i in 1:10){
  x[i+1]=sample(s,1,prob=p[x[i]+1,])
}
y[j]=x[11]
}
mean(y)
##################################
r=2
k=10
p=0.45
q=1-p
s1=c(-1,1)
ps1=c(q,p)
x={}
x=r
sk=c(k-1,k)
psk=c(0.6,0.4)
s0=c(0,1)
ps0=c(0.5,0.5)
for(i in 1:30){
  if(x[i]==0){
    x[i+1]=x[i]+sample(s0,1,prob=ps0)
  }else if(x[i]==k){
    x[i+1]=x[i]+sample(sk,1,prob=psk)
  }else{x[i+1]=x[i]+sample(s1,1,prob=ps1)}
  
}
x

##
r=2
k=10
p=0.45
q=1-p
s1=c(-1,1)
ps1=c(q,p)
t=0
sk=c(k-1,k)
psk=c(0.6,0.4)
s0=c(0,1)
ps0=c(0.5,0.5)
for(j in 1:1000){
  x={}
  x=r
for(i in 1:10){
  if(x[i]==0){
    x[i+1]=x[i]+sample(s0,1,prob=ps0)
  }else if(x[i]==k){
    x[i+1]=x[i]+sample(sk,1,prob=psk)
  }else{x[i+1]=x[i]+sample(s1,1,prob=ps1)}
  
}
  if(x[11]==0 && x[5]==5)t=t+1
}
t/N

############
matpow=function(a,n){
  a1=a
  for(i in 2:n){
    a1=a1%*%a
  }
  return(a1)
}
matpow(p,10)



###############
m=7
lm=1
x={}
x=0#initial
y={}
for(i in 1:20){
  y[i]=rpois(1,lm)
  if(x[i]==0){x[i+1]=max(0,min(x[i]+y[i],m))}
  else{x[i+1]=max(0,min(x[i]-1+y[i],m))}
}
data.frame(x,c(y,2))

m=7
lm=1
x={}
x=0#initial
y={};N=10000;t1=0
for(j in 1:N){
for(i in 1:30){
  y[i]=rpois(1,lm)
  if(x[i]==0){x[i+1]=max(0,min(x[i]+y[i],m))}
  else{x[i+1]=max(0,min(x[i]-1+y[i],m))}
}
  if(x[26]==5&&x[6]==1)t1=t1+1
  }

t1/N
