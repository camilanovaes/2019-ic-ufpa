library(sets)
sets_options("universe", seq(1, 100, 0.5))

Temperatura  = fuzzy_variable(Cold      = fuzzy_normal(0, 10),
                              Medium    = fuzzy_normal(30, 10),
                              Hot       = fuzzy_normal(60, 10))

Umid_Rel     = fuzzy_variable(Low       = fuzzy_cone(12.5, 12.5),
                              Medium    = fuzzy_cone(32.5, 12.5),
                              High      = fuzzy_cone(52.2, 12.5))

Umid_Solo    = fuzzy_variable(Dry       = fuzzy_trapezoid(corners = c(-10, 0, 10, 20)),
                              Medium    = fuzzy_trapezoid(corners = c(15, 20, 30, 35)),
                              Wet       = fuzzy_trapezoid(corners = c(30, 40, 60, 70)))

Irr_Duration = fuzzy_variable(Zero      = fuzzy_cone(10,10),
                              VeryShort = fuzzy_cone(30,15),
                              Short     = fuzzy_cone (55,15),
                              Long      = fuzzy_cone (75,10),
                              VeryLong  = fuzzy_cone(90,10))

variables = set(Temperatura, Umid_Rel, Umid_Solo, Irr_Duration)

rules = set(
  fuzzy_rule(Umid_Solo %is% Wet && Umid_Rel %is% High, Irr_Duration %is% Zero),
  fuzzy_rule(Umid_Solo %is% Wet && Umid_Rel %is% Medium && Temperatura %is% Cold, Irr_Duration %is% Zero),
  fuzzy_rule(Umid_Solo %is% Wet && Umid_Rel %is% Medium && Temperatura %is% Medium, Irr_Duration %is% VeryShort),
  fuzzy_rule(Umid_Solo %is% Wet && Umid_Rel %is% Low || Temperatura %is% Hot , Irr_Duration %is% Short),
  
  fuzzy_rule(Umid_Solo %is% Medium && Umid_Rel %is% Low, Irr_Duration %is% Long),
  fuzzy_rule(Umid_Solo %is% Medium && Umid_Rel %is% Medium, Irr_Duration %is% Short),
  fuzzy_rule(Umid_Solo %is% Medium && Umid_Rel %is% High, Irr_Duration %is% Short),
  fuzzy_rule(Umid_Solo %is% Medium && Temperatura %is% Hot && Umid_Rel %is% Low, Irr_Duration %is% VeryLong),
  fuzzy_rule(Umid_Solo %is% Medium && Temperatura %is% Hot && Umid_Rel %is% Medium, Irr_Duration %is% Long),

  fuzzy_rule(Umid_Solo %is% Dry && Temperatura %is% Hot || Umid_Rel %is% Low, Irr_Duration %is% VeryLong),
  fuzzy_rule(Umid_Solo %is% Dry && Umid_Rel %is% Medium, Irr_Duration %is% Long),
  fuzzy_rule(Umid_Solo %is% Dry && Temperatura %is% Cold && Umid_Rel %is% High, Irr_Duration %is% Long),
  fuzzy_rule(Umid_Solo %is% Dry && Temperatura %is% Medium && Umid_Rel %is% High, Irr_Duration %is% VeryLog)
  )

modelo <- fuzzy_system(variables, rules)
print(modelo)
plot(modelo) 

plot(Umid_Solo)
plot(Temperatura)
plot(Umid_Rel)
plot(Irr_Duration)


fi <- fuzzy_inference(modelo, list(Umid_Solo = 19, Temperatura = 27, Umid_Rel = 20))
plot(fi)

#a = ex.1[1][1]
#plot(Irr_Duration)
#par(new=T)
#plot(ex.1, col = 2)
#gset_defuzzify(ex.1, "centroid")
#abline(v=gset_defuzzify(ex.1, "centroid"), col="blue")


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

                        
