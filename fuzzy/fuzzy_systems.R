library(sets)

sets_options("universe", seq(0, 35, 0.5))
Temperatura  = fuzzy_variable(Frio       = fuzzy_normal(-5, 5),
                              Medio      = fuzzy_normal(15, 5),
                              Quente     = fuzzy_normal(35, 5))

sets_options("universe", seq(0, 100, 0.5))
Umid_Rel     = fuzzy_variable(Baixo      = fuzzy_cone(12.5, 12.5),
                              Medio      = fuzzy_cone(32.5, 12.5),
                              Alto       = fuzzy_cone(52.2, 12.5))

Umid_Solo    = fuzzy_variable(Seco       = fuzzy_trapezoid(corners = c(-10, 0, 10, 20)),
                              Medio      = fuzzy_trapezoid(corners = c(15, 20, 30, 35)),
                              Molhado    = fuzzy_trapezoid(corners = c(30, 40, 60, 70)))
sets_options("universe", seq(0, 32, 0.5))
Irr_Duration = fuzzy_variable(Zero       = fuzzy_cone(0,8),
                              MuitoBaixo = fuzzy_cone(8,8),
                              Baixo      = fuzzy_cone (16,8),
                              Longo      = fuzzy_cone (24,8),
                              MuitoLongo = fuzzy_cone(32,8))

variables = set(Temperatura, Umid_Rel, Umid_Solo, Irr_Duration)

rules = set(
  fuzzy_rule(Umid_Solo %is% Molhado && Umid_Rel %is% Alto, Irr_Duration %is% Zero),
  fuzzy_rule(Umid_Solo %is% Molhado && Umid_Rel %is% Medio && Temperatura %is% Frio, Irr_Duration %is% Zero),
  fuzzy_rule(Umid_Solo %is% Molhado && Umid_Rel %is% Medio && Temperatura %is% Medio, Irr_Duration %is% MuitoBaixo),
  fuzzy_rule(Umid_Solo %is% Molhado && (Umid_Rel %is% Baixo || Temperatura %is% Quente) , Irr_Duration %is% Baixo),
  
  fuzzy_rule(Umid_Solo %is% Medio && Umid_Rel %is% Baixo, Irr_Duration %is% Longo),
  fuzzy_rule(Umid_Solo %is% Medio && Umid_Rel %is% Medio, Irr_Duration %is% Baixo),
  fuzzy_rule(Umid_Solo %is% Medio && Umid_Rel %is% Alto, Irr_Duration %is% Baixo),
  fuzzy_rule(Umid_Solo %is% Medio && Temperatura %is% Quente && Umid_Rel %is% Baixo, Irr_Duration %is% MuitoLongo),
  fuzzy_rule(Umid_Solo %is% Medio && Temperatura %is% Quente && Umid_Rel %is% Medio, Irr_Duration %is% Longo),

  fuzzy_rule(Umid_Solo %is% Seco && (Temperatura %is% Quente || Umid_Rel %is% Baixo), Irr_Duration %is% MuitoLongo),
  fuzzy_rule(Umid_Solo %is% Seco && Umid_Rel %is% Medio, Irr_Duration %is% Longo),
  fuzzy_rule(Umid_Solo %is% Seco && Temperatura %is% Frio && Umid_Rel %is% Alto, Irr_Duration %is% Longo),
  fuzzy_rule(Umid_Solo %is% Seco && Temperatura %is% Medio && Umid_Rel %is% Alto, Irr_Duration %is% MuitoLongo)
  )

modelo <- fuzzy_system(variables, rules)
print(modelo)
plot(modelo)
lwd = 2
cex.lab = 1.3
ylab = "Grau de Pertinência"

plot(Umid_Solo,col=c('green','blue','red'), xlab= "Umidade do Solo (%)", ylab=ylab, xlim=c(0,70), lwd = lwd, cex.lab = cex.lab)
plot(Temperatura,col=c('green','blue','red'), xlab="Temperatura (ºC)", ylab=ylab, xlim=c(0,35), lwd = lwd, cex.lab = cex.lab)
plot(Umid_Rel,col=c('green','blue','red'), xlab="Umidade Relativa (%)", ylab=ylab, xlim=c(0,70), lwd = lwd, cex.lab = cex.lab)
plot(Irr_Duration,col=c('green','blue','red','yellow','purple') ,xlab="Tempo em minutos (min)", ylab=ylab, xlim=c(0,32), lwd = lwd, cex.lab = cex.lab)

sets_options("universe", seq(0, 32, 0.5))
fi <- fuzzy_inference(modelo, list(Temperatura = 0, Umid_Rel = 15, Umid_Solo = 20))
plot(fi)

a = fi[1][1]
plot(Irr_Duration)
par(new=T)
plot(fi, col = 2)
gset_defuzzify(fi, "centroid")
abline(v=gset_defuzzify(fi, "centroid"), col="blue")

#Geração de Inferência com o a redução dos valores de temperatura e aumento dos valores de umidade
par(mfrow=c(4,3))
result = list(); cont = 0
for (i in seq(5, 50, by=5)){
  lwd = 2
  cex.lab = 1.3
  temp=50
  umd_sl= 70
  umd_rel= 65
  cont = cont+1
  result[[cont]] <- fuzzy_inference(modelo, list(Temperatura = temp-1*i, Umid_Solo = 1.4*i, Umid_Rel = 1.3*i))
  plot(Irr_Duration,col=c('green','blue','red','yellow','purple') ,
      xlab="Tempo em minutos (min)", ylab="Grau de Pertinência", xlim=c(0,32)
      , lwd = lwd, cex.lab = cex.lab)
  par(new=T)
  lines(result[[cont]],col = "brown", lwd = lwd+1, type = "h")
  par(new=T)
  gset_defuzzify(result[[cont]], "centroid")
  abline(v=gset_defuzzify(result[[cont]], "centroid"), col="black", lwd=lwd)
  print(paste(i,temp-1*i, 1.4*i,1.3*i, gset_defuzzify(result[[cont]], "centroid"), sep = " - "))
}
#plot(Irr_Duration,col=c('green','blue','red','yellow','purple'),
#     xlab="Tempo em minutos (min)", ylab="Grau de Pertinência", 
#     xlim=c(0,32), lwd = lwd, cex.lab = cex.lab); grid();
#     abline(v=gset_defuzzify(result[[cont]]),col = "brown", lwd = lwd+1);
#     plot(result[[cont]],col = "brown", lwd = lwd+1); lines(result[[cont]],col = "brown", 
#                                                            lwd = lwd+1, type = "h")


#Geração de Inferência com o a redução dos valores de umidade e aumento dos valores de temperatura
par(mfrow=c(4,3))
result = list(); cont = 0
for (i in seq(5, 50, by=5)){
  lwd = 2
  cex.lab = 1.3
  temp=50
  umd_sl= 70
  umd_rel= 65
  cont = cont+1
  result[[cont]] <- fuzzy_inference(modelo, list(Temperatura = 1*i, Umid_Solo = umd_sl - 1.4*i, Umid_Rel = umd_rel - 1.3*i))
  plot(Irr_Duration,col=c('green','blue','red','yellow','purple') ,
       xlab="Tempo em minutos (min)", ylab="Grau de Pertinência", xlim=c(0,32)
       , lwd = lwd, cex.lab = cex.lab)
  par(new=T)
  lines(result[[cont]],col = "brown", lwd = lwd+1, type = "h")
  par(new=T)
  gset_defuzzify(result[[cont]], "centroid")
  abline(v=gset_defuzzify(result[[cont]], "centroid"), col="black", lwd=lwd)
  print(paste(i,1*i, umd_sl - 1.4*i, umd_rel - 1.3*i, gset_defuzzify(result[[cont]], "centroid"), sep = " - "))
}

#Plot para as inferências que serão utilizadas no trabalho escrito
par (mfrow = c(1,2))
result = list();cont = 0
for (i in seq(1, 2, by=1)){
lwd = 2
cex.lab = 1.3
temp= sample(40,1,replace=T)
umd_sl= sample(70,1,replace = T)
umd_rel= sample(62,1,replace = T)
cont = cont+1
print(paste(temp, umd_sl, umd_rel))

result[[cont]] <- fuzzy_inference(modelo, list(Temperatura = temp, Umid_Solo = umd_sl, Umid_Rel = umd_rel))

plot(Irr_Duration,col=c('green','blue','red','yellow','purple') ,
     xlab="Tempo em minutos (min)", ylab="Grau de Pertinência", xlim=c(0,32)
     , lwd = lwd, cex.lab = cex.lab)
par(new=T)
lines(result[[cont]],col = "brown", lwd = lwd+1, type = "h")
par(new=T)
gset_defuzzify(result[[cont]], "centroid")
abline(v=gset_defuzzify(result[[cont]], "centroid"), col="black",lwd = lwd)
print(paste(temp, umd_sl, umd_rel, gset_defuzzify(result[[cont]], "centroid"), sep = " - "))
}

