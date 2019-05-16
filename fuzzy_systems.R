library(sets)
sets_options("universe", seq(0, 100, 0.5))

Temperatura  = fuzzy_variable(Frio       = fuzzy_normal(-5, 5),
                              Medio      = fuzzy_normal(15, 5),
                              Quente     = fuzzy_normal(35, 5))

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

plot(Umid_Solo)
plot(Temperatura)
plot(Umid_Rel)
plot(Irr_Duration)

fi <- fuzzy_inference(modelo, list(Temperatura = 5, Umid_Rel = 7, Umid_Solo = 6.5))
plot(fi)

a = fi[1][1]
plot(Irr_Duration)
par(new=T)
plot(fi, col = 2)
gset_defuzzify(fi, "centroid")
abline(v=gset_defuzzify(fi, "centroid"), col="blue")


par(mfrow=c(4,3))
result = list(); cont = 0
for (i in seq(5, 50, by=5)){
  temp=50
  umd_sl= 70
  umd_rel= 65
  cont = cont+1
result[[cont]] <- fuzzy_inference(modelo, list(Temperatura = temp-1*i, Umid_Solo = umd_sl-1.4*i, Umid_Rel = umd_rel - 1.3*i))
  plot(result[[cont]])
  print(paste(i,temp-1*i, umd_sl-1.4*i, umd_rel - 1.3*i, gset_defuzzify(result[[cont]], "centroid"), sep = " - "))
}

                        
