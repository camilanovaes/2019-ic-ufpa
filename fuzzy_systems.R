library(sets)
sets_options("universe", seq(1, 200, 0.5))

Temperatura = fuzzy_variable(Cold = fuzzy_trapezoid(corners = c(-10, 0, 25, 30)),
                             Medium = fuzzy_cone(corners = c(30, 5)),
                             Hot = fuzzy_trapezoid(corners = c(30, 35, 200, 240)))

Luminosidade = fuzzy_variable(Dark = fuzzy_trapezoid(-10, 0, 0.4,0.6),
                           Medium = fuzzy_trapezoid(0.4, 0.6,9000,10000),
                           Light = fuzzy_trapezoid(9000, 10000, 12000,13000))

Umid_Solo = fuzzy_variable(Dry = fuzzy_trapezoid(-10, 0, 17, 34),
                         Medium = fuzzy_cone(34, 17),
                         Wet = fuzzy_trapezoid(34, 51, 80, 100))

Irr_Duration = fuzzy_variable(Zero = fuzzy_cone(0,8),
                              VeryShort = fuzzy_cone(8,8),
                              Short = fuzzy_cone (16,8),
                              Long = fuzzy_cone (24,8),
                              VeryLong = fuzzy_cone(32,8))


variables = set(Temperatura, Luminosidade, Umid_Solo, Irr_Duration)

rules = set(
  fuzzy_rule(Umid_Solo %is% Wet, Irr_Duration %is% Zero),
  fuzzy_rule(Umid_solo %is% Medium && Temperatura %is% Cold, Irr_Duration %is% Short),
  fuzzy_rule(Umid_solo %is% Medium && Temperatura %is% Medium && Luminosidade %is% Light, Irr_Duration %is% VeryShort),
  fuzzy_rule(Umid_solo %is% Medium && Temperatura %is% Medium && Luminosidade %is% Medium, Irr_Duration %is% Short),
  fuzzy_rule(Umid_solo %is% Medium && Temperatura %is% Medium && Luminosidade %is% Dark, Irr_Duration %is% Short),
  fuzzy_rule(Umid_solo %is% Medium && Temperatura %is% Hot && Luminosidade %is% Medium, Irr_Duration %is% VeryShort),
  fuzzy_rule(Umid_solo %is% Medium && Temperatura %is% Hot && Luminosidade %is% Dark, Irr_Duration %is% Long),
  fuzzy_rule(Umid_solo %is% Dry && Temperatura %is% Cold, Irr_Duration %is% VeryLong),
  fuzzy_rule(Umid_solo %is% Dry && Temperatura %is% Medium && Luminosidade %is% Light, Irr_Duration %is% Short),
  fuzzy_rule(Umid_solo %is% Dry && Temperatura %is% Medium && Luminosidade %is% Medium, Irr_Duration %is% Long),
  fuzzy_rule(Umid_solo %is% Dry && Temperatura %is% Medium && Luminosidade %is% Dark, Irr_Duration %is% Long),
  fuzzy_rule(Umid_solo %is% Dry && Temperatura %is% Hot && Luminosidade %is% Medium, Irr_Duration %is% VeryShort),
  fuzzy_rule(Umid_solo %is% Dry && Temperatura %is% Hot && Luminosidade %is% Dark, Irr_Duration %is% VeryLong),
  fuzzy_rule(Temperatura %is% Hot && Luminosidade %is% Light, Irr_Duration %is% Zero)
)

modelo <- fuzzy_system(variables, rules)
print(modelo)
plot(modelo) 

#plot(Umid_Solo)
#plot(Temperatura)
#plot(Luminosidade)
#plot(Irr_Duration)


#ex.1 <- fuzzy_inference(modelo, list(Umid_Solo = 19, Temperatura = 27, Luminosidade = 100))
#plot(ex.1)

#a = ex.1[1][1]
#plot(Irr_Duration)
#par(new=T)
#plot(ex.1, col = 2)
#gset_defuzzify(ex.1, "centroid")
#abline(v=gset_defuzzify(ex.1, "centroid"), col="blue")


par(mfrow=c(3,3))
result = list(); cont = 0
for (i in seq(5, 50, by=5)){
  t=45
  us= 100
  l= 11000
  cont = cont+1
result[[cont]] <- fuzzy_inference(modelo, list(Temperatura = 50-1*i, Umid_Solo = us-2*i, Luminosidade = l - 230*i))
  plot(result[[cont]])
  print(paste(v-1*i, us - 2*i, l - 230*i, gset_defuzzify(result[[cont]], "centroid"), sep = " - "))
}

                        
