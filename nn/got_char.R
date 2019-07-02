library(neuralnet)

dataset = read.csv('got_dataset.csv')
dataset.train = dataset[1:70,]
dataset.test = dataset [71:nrow(dataset),]

quest.qty = 8
ans.qty = 4

quest.formula = paste(colnames(dataset[1:quest.qty]),collapse = " + ")
class.formula = paste(colnames(dataset[(quest.qty+1):length(dataset)]),collapse = " + ")
formula = as.formula(paste(class.formula," ~ ",quest.formula))

nn.train = neuralnet(formula, data = dataset.train, hidden = c(20,20), threshold = 0.0001, linear.output = F, stepmax = 3e+5)
nn.test = compute(nn.train,dataset.test[1:quest.qty])

nn.result = data.frame(target = max.col(dataset.test[(quest.qty+1):length(dataset)]),
                       output = max.col(nn.test$net.result))
nn.result$error = abs(nn.result$output - nn.result$target)

plot(nn.result$target, col="green", pch=20,xaxt = "none", yaxt="none",
     main = "Qual personagem de GOT é você?",
     xlab = "Entradas",ylab = "Personagens")
points(nn.result$output, col="red")
axis(1,at=seq(1,nrow(dataset.test)))
axis(2,at=seq(1,4),labels = c("Ned Stark","Robb Stark", "Joffrey Baratheon", "Jon Snow"))

print(nn.train$result.matrix[1:3,])
print(paste(sum(nn.result$error == 0)," acertos de ",nrow(nn.result)),quote = F)
print(nn.result)

