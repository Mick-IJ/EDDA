#exercise 1
nuseq=seq(175,185,by=0.25)


n=m=30; mu=180; sd=5
A = numeric(length(nuseq))
for (i in 1:length(A)){
  D = numeric(1000)
  nu = nuseq[i]
  for (j in 1:1000){
    x = rnorm(n, mu, sd); y = rnorm(m, nu, sd)
    D[j] = t.test(x, y, var.equal=TRUE)[[3]]
  }
  A[i] = mean(D < 0.05)
}

plot(nuseq,A,main="Exercise 1",xlab="nu",ylab="Power",type='l',lwd=2,col="blue")


n=m=100; mu=180; sd=5
B = numeric(length(nuseq))
for (i in 1:length(B)){
  D = numeric(1000)
  nu = nuseq[i]
  for (j in 1:1000){
    x = rnorm(n, mu, sd); y = rnorm(m, nu, sd)
    D[j] = t.test(x, y, var.equal=TRUE)[[3]]
  }
  B[i] = mean(D < 0.05)
}

lines(nuseq,B,type='l',lwd=2,col="red")


n=m=30; mu=180; sd=15
C = numeric(length(nuseq))
for (i in 1:length(C)){
  D = numeric(1000)
  nu = nuseq[i]
  for (j in 1:1000){
    x = rnorm(n, mu, sd); y = rnorm(m, nu, sd)
    D[j] = t.test(x, y, var.equal=TRUE)[[3]]
  }
  C[i] = mean(D < 0.05)
}

lines(nuseq,C,type='l',lwd=2,col="green")

legend(178.7, 0.9, c('a) n = 30, sd = 5', 'b) n = 100, sd=5', 'c) n = 30, sd = 15'), 
       lwd=c(5,3), col=c("blue", "red", "green"))


# exercise 3
telephone = read.table('telephone.txt', header=T)
med = median(telephone$Bills)

lamdaseq = seq(0.01, 0.1, by = 0.002)
A = numeric(length(lamdaseq))

for (i in 1:length(lamdaseq)){
  B = numeric(1000)
  lambda = lamdaseq[i]
  for (j in 1:length(B)){
    sample = rexp(1000, lambda)
    B[j] = median(sample)
  }

  A[i] = (length(B[B<med])/length(B))
}

plot(lamdaseq[1:length(lamdaseq)-1], diff(A), type='l')
