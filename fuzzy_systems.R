library(sets)
sets_options("universe", seq(1, 100, 0.5))

Temperatura  = fuzzy_variable(Frio       = fuzzy_normal(0, 10),
                              Medio      = fuzzy_normal(30, 10),
                              Quente     = fuzzy_normal(60, 10))

Umid_Rel     = fuzzy_variable(Baixo      = fuzzy_cone(12.5, 12.5),
                              Medio      = fuzzy_cone(32.5, 12.5),
                              Alto       = fuzzy_cone(52.2, 12.5))

Umid_Solo    = fuzzy_variable(Seco       = fuzzy_trapezoid(corners = c(-10, 0, 10, 20)),
                              Medio      = fuzzy_trapezoid(corners = c(15, 20, 30, 35)),
                              Molhado    = fuzzy_trapezoid(corners = c(30, 40, 60, 70)))

Irr_Duration = fuzzy_variable(Zero       = fuzzy_cone(10,10),
                              MuitoBaixo = fuzzy_cone(30,15),
                              Baixo      = fuzzy_cone (55,15),
                              Longo      = fuzzy_cone (75,10),
                              MuitoLongo = fuzzy_cone(90,10))

variables = set(Temperatura, Umid_Rel, Umid_Solo, Irr_Duration)

rules = set(
  fuzzy_rule(Umid_Solo %is% Molhado && Umid_Rel %is% Alto, Irr_Duration %is% Zero),
  fuzzy_rule(Umid_Solo %is% Molhado && Umid_Rel %is% Medio && Temperatura %is% Frio, Irr_Duration %is% Zero),
  fuzzy_rule(Umid_Solo %is% Molhado && Umid_Rel %is% Medio && Temperatura %is% Medio, Irr_Duration %is% MuitoBaixo),
  fuzzy_rule(Umid_Solo %is% Molhado && Umid_Rel %is% Baixo || Temperatura %is% Quente , Irr_Duration %is% Baixo),
  
  fuzzy_rule(Umid_Solo %is% Medio && Umid_Rel %is% Baixo, Irr_Duration %is% Longo),
  fuzzy_rule(Umid_Solo %is% Baixo && Umid_Rel %is% Baixo, Irr_Duration %is% Baixo),
  fuzzy_rule(Umid_Solo %is% Medio && Umid_Rel %is% Alto, Irr_Duration %is% Baixo),
  fuzzy_rule(Umid_Solo %is% Medio && Temperatura %is% Quente && Umid_Rel %is% Baixo, Irr_Duration %is% MuitoLongo),
  fuzzy_rule(Umid_Solo %is% Medio && Temperatura %is% Quente && Umid_Rel %is% Medium, Irr_Duration %is% Longo),

  fuzzy_rule(Umid_Solo %is% Seco && Temperatura %is% Quente || Umid_Rel %is% Baixo, Irr_Duration %is% MuitoLongo),
  fuzzy_rule(Umid_Solo %is% Seco && Umid_Rel %is% Medio, Irr_Duration %is% Longo),
  fuzzy_rule(Umid_Solo %is% Seco && Temperatura %is% Frio && Umid_Rel %is% Alto, Irr_Duration %is% Longo),
  fuzzy_rule(Umid_Solo %is% Seco && Temperatura %is% Medio && Umid_Rel %is% Alto, Irr_Duration %is% MuitoLongo)
  )

modelo <- fuzzy_system(variables, rules)
print(modelo)
plot(modelo) 

plot(Umid_Solo)
plot(Temperatura)
plot(Umid_Rel)
plot(Irr_Duration)

fi <- fuzzy_inference(modelo, list(Temperatura = 30, Umid_Rel = 20, Umid_Solo = 20))
plot(fi)

a = fi[1][1]
plot(Irr_Duration)
par(new=T)
plot(fi, col = 2)
gset_defuzzify(fi, "centroid")
abline(v=gset_defuzzify(fi, "centroid"), col="blue")


#par(mfrow=c(3,3))
#result = list(); cont = 0
#for (i in seq(5, 50, by=5)){
#  t=45
#  us= 100
#  l= 11000
#  cont = cont+1
#result[[cont]] <- fuzzy_inference(modelo, list(Temperatura = 50-1*i, Umid_Solo = us-2*i, Luminosidade = l - 230*i))
#  plot(result[[cont]])
#  print(paste(v-1*i, us - 2*i, l - 230*i, gset_defuzzify(result[[cont]], "centroid"), sep = " - "))
#}

                        
