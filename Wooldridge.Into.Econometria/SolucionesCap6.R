

DatosKielMC <- get(load("C:/Users/admin/Desktop/GitHub/Econometria/Wooldridge.Into.Econometria/Data/R data sets for 5e/kielmc.RData"))


DatosKielMC <- subset(data, data$year == '1981')


Modelo6.1.1 <- lm(log(price)~log(dist), data = DatosKielMC)

summary(Modelo6.1.1)


Modelo6.1.2 <- lm(log(price) ~ log(dist) + log(intst) + log(area)
                   + log(land) + rooms + baths + age, data = DatosKielMC)

summary(Modelo6.1.2)

Modelo6.1.3 <- lm(log(price) ~ log(dist) + log(intst) + I(log(intst)^2) + log(area)
                  + log(land) + rooms + baths + age, data = DatosKielMC)

summary(Modelo6.1.3)

Modelo6.1.4 <- lm(log(price) ~ log(dist) + I(log(dist)^2)  + log(intst) 
                  + I(log(intst)^2) + log(area)+ log(land) + rooms 
                  + baths + age, data = DatosKielMC)

summary(Modelo6.1.4)

## C6.2

DatosWage1 <- get(load("C:/Users/admin/Desktop/GitHub/Econometria/Wooldridge.Into.Econometria/Data/R data sets for 5e/wage1.RData"))

Modelo6.2.1 <- lm(log(wage) ~ educ + exper + I(exper^2), data = DatosWage1)

summary(Modelo6.2.1)

coef(Modelo6.2.1)[3]

Opt2 <- abs(coef(Modelo6.2.1)[3])/abs(2*coef(Modelo6.2.1)[4])

Opt2


## C6.3

Modelo6.3.1 <- lm(log(wage) ~ educ + exper + educ:exper, data = DatosWage1)

summary(Modelo6.3.1)

DatosWage2 <- get(load("C:/Users/admin/Desktop/GitHub/Econometria/Wooldridge.Into.Econometria/Data/R data sets for 5e/wage2.RData"))


Modelo6.3.3 <- lm(log(wage) ~ educ + exper + educ:exper, data = DatosWage2)

summary(Modelo6.3.3)

## C6.4


DatosGPA2 <- get(load("C:/Users/admin/Desktop/GitHub/Econometria/Wooldridge.Into.Econometria/Data/R data sets for 5e/gpa2.RData"))


Modelo6.4.1 <- lm( sat ~ hsize + I(hsize^2), data = DatosGPA2)

summary(Modelo6.4.1)

Opt4 <- abs(coef(Modelo6.4.1)[2])/abs(2*coef(Modelo6.4.1)[3])

Opt4


Modelo6.4.4 <- lm( log(sat) ~ hsize + I(hsize^2), data = DatosGPA2)

summary(Modelo6.4.4)

Opt4.4 <- abs(coef(Modelo6.4.4)[2])/abs(2*coef(Modelo6.4.4)[3])

Opt4.4