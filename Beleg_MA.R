h <- diabetes_012_health_indicators_BRFSS2015$Diabetes_012
#absolute Häufigkeit
table(diabetes_012_health_indicators_BRFSS2015$Diabetes_012)
#relative Häufigkeit
f_x <- prop.table(table(diabetes_012_health_indicators_BRFSS2015$Diabetes_012))
#Häufigkeit visualiseren
colors <- c("azure","lightblue",  "steelblue")
label <- c("gesund", "Prädiabetes", "Diabetes")
pie(f_x, col = colors, labels = label, border = "yellow", font=3 )
#wieviel Frauen und Männder 
sexMW <- table(diabetes_012_health_indicators_BRFSS2015$Sex)
#absolute Häufigkeit Funktion von der Anzahl der Männer&Frauen_Teil 
f_sexMW <- prop.table(sexMW)
#visuliseren der Anzahl der Männer&Frauen_Teil
colorss <- c("pink", "lightblue")
labels <- c("Fraun","Männer")
pie(f_sexMW, labels = labels, col = colorss)
#nach M, W, Diabetes, nicht Diabetes mit abolute Häufigkeit gruppiren
color <- c( "lightblue","pink")
m_w_diebets <- table(diabetes_012_health_indicators_BRFSS2015$Sex, diabetes_012_health_indicators_BRFSS2015$Diabetes_012)
barplot(m_w_diebets, col = color)
#die Summe der erkrankten Frauen$MännerTeile einzel rechnen 
m <- count(filter(diabetes_012_health_indicators_BRFSS2015, diabetes_012_health_indicators_BRFSS2015$Sex == 1 & diabetes_012_health_indicators_BRFSS2015$Diabetes_012==2))$n
w <- count(filter(diabetes_012_health_indicators_BRFSS2015, diabetes_012_health_indicators_BRFSS2015$Sex == 0 & diabetes_012_health_indicators_BRFSS2015$Diabetes_012==2))$n
summeAll_mw <- count(filter(diabetes_012_health_indicators_BRFSS2015, diabetes_012_health_indicators_BRFSS2015$Diabetes_012 == 2))$n
#hypergeometrische Verteilung: es wird 5 Patienten unter denn genau 3 Frauen augewählt
phyper(3 , w , summeAll_mw , 5)
#konfidenzintervall, Mitte für die Übergewächte erkrankte Leute 
diabete <- subset(diabetes_012_health_indicators_BRFSS2015, diabetes_012_health_indicators_BRFSS2015$Diabetes_012>0)
plot(diabetes_012_health_indicators_BRFSS2015$Diabetes_012,diabetes_012_health_indicators_BRFSS2015$BMI, col = "blue", pch = 16, xlab = "BMI", ylab = "Diabetes_012", main = "Zusammenhang mit Übergewicht")
result <- t.test(diabetes_012_health_indicators_BRFSS2015$BMI)$conf.int
abline(v = result[1], col="red")
abline(v = result[2], col="red")
#poisson verteilung 
#wie gross ist die Wahrscheinlichkeit, dass 18er an diabetes erkrankt
dpois(x = 18,lambda = mean(diabete$Age) )
#standardnormalverteilung, 
age <- diabete$Age
m= mean(age)
s= sd(age)
hist(age, freq = F, col = "burlywood")
curve(dnorm(x, mean = m, sd = s), add = TRUE, col="red")
abline(v=m, col="red");
ageZwischen=(pnorm(q = 11, mean = m, sd = s)-pnorm(q = 7, mean = m, sd = s))*100
#Häufigkeit mit Cut um der Alterunterschied von Patienten zu visualiseren 
altergrenze <- c(1,5,10,15)
table(cut(diabete$Age, breaks = altergrenze, include.lowest = TRUE))
prop.table(table(cut(diabete$Age, breaks = altergrenze, include.lowest = TRUE)))
barplot(prop.table(table(cut(diabete$Age, breaks = altergrenze, include.lowest = TRUE))), xlab = "Age", ylab = "%", col = "gold3", border = "pink",  main = "Der Alter von den Diabetiker" )
axis(2, col = "blue")



  


    