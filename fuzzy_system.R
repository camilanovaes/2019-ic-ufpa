library(sets)
sets_options("universe", seq(1, 200, 0.5))

velocidade = fuzzy_variable(baixa = fuzzy_trapezoid(corners = c(-10, 0, 20, 60)),
                             media = fuzzy_trapezoid(corners = c(40, 60, 80, 100)),
                             alta = fuzzy_trapezoid(corners = c(70, 130, 200, 240)))

distancia = fuzzy_variable(perto = fuzzy_normal(mean = 0, sd = 40),
                           media = fuzzy_normal(100, 40),
                           longe = fuzzy_normal(200, 40))

pressao = fuzzy_variable(leve = fuzzy_cone(center = 0, radius = 50),
                         media = fuzzy_cone(50, 50),
                         forte = fuzzy_cone(100, 50))


variables = set(velocidade, distancia, pressao)

rules = set(
  fuzzy_rule(velocidade %is% alta && distancia %is% perto, pressao %is% forte),
  fuzzy_rule(velocidade %is% alta && distancia %is% media, pressao %is% forte),
  fuzzy_rule(velocidade %is% alta && distancia %is% longe, pressao %is% media),
  fuzzy_rule(velocidade %is% media && distancia %is% perto, pressao %is% forte),
  fuzzy_rule(velocidade %is% media && distancia %is% media, pressao %is% media),
  fuzzy_rule(velocidade %is% media && distancia %is% longe, pressao %is% leve),
  fuzzy_rule(velocidade %is% baixa && distancia %is% perto, pressao %is% media),
  fuzzy_rule(velocidade %is% baixa && distancia %is% media, pressao %is% leve),
  fuzzy_rule(velocidade %is% baixa && distancia %is% longe, pressao %is% leve)
)

modelo <- fuzzy_system(variables, rules)
print(modelo)
plot(modelo) 

plot(velocidade)

velocidade[[1]]


ex.1 <- fuzzy_inference(modelo, list(velocidade = 90, distancia = 60))
plot(ex.1)

a = ex.1[1][1]
plot(pressao)
par(new=T)
plot(ex.1, col = 2)
gset_defuzzify(ex.1, "centroid")
abline(v=gset_defuzzify(ex.1, "centroid"), col="blue")


par(mfrow=c(3,3))
result = list(); cont = 0
for (i in seq(5, 50, by=5)){
  v=150
  d = 150
  cont = cont+1
result[[cont]] <- fuzzy_inference(modelo, list(velocidade = v-1.5*i, distancia = d-2*i))
  plot(result[[cont]])
  print(paste(v-1.5*i, d - 2*i, gset_defuzzify(result[[cont]], "centroid"), sep = " - "))
}

                        