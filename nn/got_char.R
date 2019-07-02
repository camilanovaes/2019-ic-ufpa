library(neuralnet)

dataset = read.csv('got_dataset.csv')
dataset.train = dataset[1:80,]
dataset.test = dataset [81:nrow(dataset),]

quest.qty = 8
ans.qty = 4

quest.formula = paste(colnames(dataset[1:quest.qty]),collapse = " + ")
class.formula = paste(colnames(dataset[(quest.qty+1):length(dataset)]),collapse = " + ")
formula = as.formula(paste(class.formula," ~ ",quest.formula))

nn.train = neuralnet(formula, data = dataset.train,
                     hidden = c(30,30,30), threshold = 0.0001,
                     linear.output = F, stepmax = 3e+5)
nn.test = compute(nn.train,dataset.test[1:quest.qty])
write.csv(nn.test$net.result,"raw-nn-result.csv",quote = F)

nn.result = data.frame(target = max.col(dataset.test[(quest.qty+1):length(dataset)]),
                       output = max.col(nn.test$net.result))
nn.result$error = abs(nn.result$output - nn.result$target)


print(nn.train$result.matrix[1:3,])
write.csv(t(nn.train$result.matrix[1:3,]),"nn-stats.csv",quote = F,row.names = F)
print(paste(sum(nn.result$error == 0)," acertos de ",nrow(nn.result)),quote = F)
print(nn.result)
write.csv(nn.result,"nn-result.csv",quote = F,row.names = F)

hit.rate = nrow(nn.result[nn.result$error == 0,])/nrow(nn.result)*100

plot(nn.result$target, col="green", pch=20,xaxt = "none", yaxt="none", cex = 3,
     main = "Qual personagem de GOT é você",
     xlab = "Testes Realizadoss",ylab = "Personagens")
points(nn.result$output, col="red",cex=2)
axis(1,at=seq(1,nrow(dataset.test)))
axis(2,at=seq(1,4),labels = c("Ned Stark","Robb Stark", "Joffrey Baratheon", "Jon Snow"))
