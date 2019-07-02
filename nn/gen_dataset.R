sort.test = dataset.test
sort.test$class = max.col(dataset.test[9:12])
test.final =sort.test[order(sort.test$class),-c(13)]
rownames(test.final) = seq(1,nrow(test.final))
write.csv(test.final,"test_dataset.csv",quote = F)