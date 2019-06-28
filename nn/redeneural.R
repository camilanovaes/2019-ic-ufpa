############### XOR
library("neuralnet")
P = data.frame(matrix(c(-0.8,0.7,-0.7,0.65,-0.9,0.75,0.9,-0.8,0.87,-0.7,0.7,-0.9,-0.8,-0.8,
             -0.7,-0.9,-0.7,-0.87,0.8,0.7,0.7,0.9,0.85,0.76),
           nrow = 2, ncol = 12))
P = t(P)
colnames(P) = c("x", "y")
tg = c(0,0,0,0,0,0,1,1,1,1,1,1)
train = as.data.frame(cbind(P,tg))

plot(train$x, train$y, col=train$tg+2, pch=train$tg+2)


nn = neuralnet(tg ~ x+y , data=train, hidden=5, 
               threshold = 0.001, )

compute(nn, t(c(-0.5, 0.5)))

plot(nn)
m = data.frame(x=0, y = 0); cont = 0;
for (i in seq(-1,1, by=0.05)) {
  for (j in seq(-1,1, by=0.05)) {
    cont = cont+1;
    m[cont,1:2] = c(i,j)
  }
}
res = compute(nn, m)
cor = res$net.result > 0.5
cor = as.numeric(cor)+2
plot(m, col = cor, pch = 20)


######################## seno
library("neuralnet")
x = seq(0,10, by=0.4)
y = sin(x)
plot(x,y,pch=3, col="blue", ylim = c(-1,1))

net = neuralnet(y~x, data=x, hidden=5)
plot(net)
net$result.matrix[1:3,]

x2 = seq(0,10, by=0.01)
y2 = compute(net, x2)
par(new=T)
plot(x2,y2$net.result, col="red", type = "l", ylim = c(-1,1))
ysin = sin(x2)
par(new=T)

plot(x2,ysin, col="blue", type = "l", ylim = c(-1,1))


################ anemia

library("neuralnet")


train = read.table("anemia_train.txt", header = T, sep = ";", stringsAsFactors = F)
teste = read.table("anemia_teste.txt", header = T, sep = ";", stringsAsFactors = F)

n = names(train)
colnames(train)[401] = "resp"
f = as.formula(paste("resp ~" , paste(n[!n %in% "resp"], collapse = " + ")))

net = neuralnet(f, data = train, hidden = 30, linear.output = F)
net$result.matrix[1:3,]

r = compute(net, teste[,1:400])
a = as.data.frame(r$net.result)
a$teste = teste$V401
a$erro = abs(a$V1-a$teste)
sum(a$erro > 0.4)

plot(a$teste, col="green", pch=20, ylim = c(0,1))
par(new=T)
plot(r$net.result, col="red",  ylim = c(0,1))

################## Amerindio
library("neuralnet")

train = read.table("amerindio_train.txt", header = T, sep = ";", stringsAsFactors = F)
teste = read.table("amerindio_teste.txt", header = T, sep = ";", stringsAsFactors = F)

nomes = names(train)

f = as.formula(paste("N1+N2+N3+N4 ~" , paste(nomes[!nomes %in% c("N1", "N2", "N3", "N4")], collapse = " + ")))

net = neuralnet(f, data = train, hidden = 10, linear.output = F)
net$result.matrix[1:3,]

r = compute(net, teste[,1:202])
a = data.frame(rede = max.col(r$net.result))
a$teste = max.col(teste[,203:206])
a$erro = abs(a$rede-a$teste)
sum(a$erro != 0)

plot(a$teste, col="green", pch=20, ylim = c(0.5,4.5))
par(new=T)
plot(a$rede, col="red",  ylim = c(0.5,4.5))
