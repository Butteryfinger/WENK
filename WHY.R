Var <- read.table("bojhall2-win.dat")

Varmed <- Var
Varmed
e <- 1
while(e <= 54){
  Varmed[e,4] <- sum(Var[e,c(4,5,6)])/3
  print(sum(Var[e,c(4,5,6)])/3)
  print(e)
  e <- e + 1
}
Varmed <- Varmed[,-c(5,6)]

Varmed <- Var
e <- 1
while(e <= 9){
  Varmed[(((e-1)*6)+1),4] <- sum(Var[((e-1)*6+1):(e*6),4])/6
  Varmed[((e-1)*6+1),5] <- sum(Var[((e-1)*6+1):(e*6),5])/6
  Varmed[((e-1)*6+1),6] <- sum(Var[((e-1)*6+1):(e*6),6])/6
  e <- e + 1
}
Varallmed <- Varmed[c(1,7,13,19,25,31,37,43,49),c(4,5,6)]

Varmed <- Host
e <- 1
while(e <= 9){
  Varmed[(((e-1)*6)+1),4] <- sum(Host[((e-1)*6+1):(e*6),4])/6
  Varmed[((e-1)*6+1),5] <- sum(Host[((e-1)*6+1):(e*6),5])/6
  Varmed[((e-1)*6+1),6] <- sum(Host[((e-1)*6+1):(e*6),6])/6
  e <- e + 1
}
Varallmed <- Varmed[c(1,7,13,19,25,31,37,43,49),c(4,5,6)]


library(tidyverse)
library(lme4)
library(knitr)

data <- read.table("bojhall2-win.dat") %>% 
  setNames(c("Tillverkare", "Tillfalle", "Skiva", "Boj1", "Boj2", "Boj3")) 

cleanData <- rbind(data %>% select(Tillverkare, Tillfalle, Skiva, Boj = Boj1), 
                   data %>% select(Tillverkare, Tillfalle, Skiva, Boj = Boj2),
                   data %>% select(Tillverkare, Tillfalle, Skiva, Boj = Boj3)) %>% 
  arrange(Tillverkare, Tillfalle,Skiva) %>% 
  mutate(Tillverkare = factor(Tillverkare), Tillfalle = factor(Tillfalle), Skiva = factor(Skiva)) %>% 
  mutate(BojNummer = rep(1:3, nrow(data))) %>% 
  mutate(BojNummer = factor(BojNummer))

medBojNummer <- lmer(Boj ~ Tillverkare + Tillfalle + Tillverkare*Tillfalle + 
                       (1|Tillverkare:Skiva) + (1|Tillverkare:Skiva:BojNummer) 
                     , data = cleanData, REML=FALSE)

utanBojNummer <- lmer(Boj ~ Tillverkare + Tillfalle + Tillverkare*Tillfalle + 
                        (1|Tillverkare:Skiva)
                      , data = cleanData, REML=FALSE)

medBojFrame <- summary(medBojNummer)$varcor %>% 
  data.frame() %>% 
  select(vcov, sdcor) %>% 
  setNames(c("Variance", "Standard Deviation"))

utanBojFrame <- summary(utanBojNummer)$varcor %>% 
  data.frame() %>% 
  select(vcov, sdcor) %>% 
  setNames(c("Variance", "Standard Deviation"))

row.names(medBojFrame) <- c("Mätordning", "Skiva", "Residual")

row.names(utanBojFrame) <- c("Skiva", "Residual")

kable(medBojFrame, caption = "Med skiva och mätordning som slumpeffekter")

kable(utanBojFrame, caption = "Med skiva som slumpeffekt ")

baseModel <- aov(data = cleanData, Boj ~ Tillverkare*Tillfalle + 
                   Tillverkare*Skiva*Tillfalle)

anova(baseModel)
