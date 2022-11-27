library(tidyverse)
library(knitr)
library(vroom)
library(MuMIn)


ang <- seq(45,168.75,length.out = 12) #elbow angle

normF <- c(0.71, 0.77, 0.83, 0.91, 0.97, 1.00, 0.96, 0.94, 0.89, 0.84, 0.77, 0.74) #you'll get this by reading in data and calculating normalized values

qplot(ang,normF)+geom_point(aes(x=ang[which.max(normF)],y=normF[which.max(normF)]),col="red",size=4)

ang[which.max(normF)] #Find angle for Fmax, theta_max








poly.m2 <- lm(normF~poly(ang,2)) #second order
poly.m3 <- lm(normF~poly(ang,3)) #third order
poly.m4 <- lm(normF~poly(ang,4)) #fourth order

AICc(poly.m2,poly.m3,poly.m4) #the second order model fits best






x.pred <- seq(45,157.5,length.out = 1000) #define 1000 angles from our range

normF.pred <- predict(poly.m4,newdata = data.frame(ang=x.pred)) #predict the force using 1000 angles

qplot(ang,normF)+geom_point(aes(x=x.pred,y=normF.pred),col="red")+geom_point(aes(x=x.pred[which.max(normF.pred)],y=normF.pred[which.max(normF.pred)]),size=5,col="blue")

x.pred[which.max(normF.pred)] #theta_max






normF.fat <- c(0.65, 0.71, 0.81, 0.89,0.92, 0.96, 0.99, 1, 0.96, 0.92, 0.84, 0.77) #fatigue data, you'll get this by reading in data and calculating normalized values

qplot(ang,normF.fat)+geom_point(aes(x=ang[which.max(normF.fat)],y=normF.fat[which.max(normF.fat)]),col="red",size=4)

ang[which.max(normF.fat)] #Find angle for Fmax for fatigue data






poly.m2.fat <- lm(normF.fat~poly(ang,2)) #second order
poly.m3.fat <- lm(normF.fat~poly(ang,3)) #third order
poly.m4.fat <- lm(normF.fat~poly(ang,4)) #fourth order

AICc(poly.m2.fat,poly.m3.fat,poly.m4.fat) #the second order model fits best





normF.pred.fat <- predict(poly.m2.fat,newdata = data.frame(ang=x.pred)) #predict the force using 1000 angles

qplot(ang,normF)+geom_point(aes(x=x.pred,y=normF.pred.fat),col="red")+geom_point(aes(x=x.pred[which.max(normF.pred.fat)],y=normF.pred.fat[which.max(normF.pred.fat)]),size=5,col="blue")







x.pred[which.max(normF.pred.fat)]-x.pred[which.max(normF.pred)] #shift in angle for Fmax, theta_max







set.seed(1234) #to keep random noise the same

sub <- 1:10 #ten subjects
dat.l <- list() #empty list

#loop to run through subjects and simulate data based on noise added to data above
for(i in sub){
  noise <- rnorm(length(ang),mean = 0.02,sd = 0.01)
  noise2 <- rnorm(length(ang),mean = 0.02,sd = 0.01)
  dat.l[[i]] <- tibble(
    rbind(
      data.frame(ang,force=normF+noise,subject=i,exp="control"),
      data.frame(ang,force=normF.fat+noise2,subject=i,exp="fatigue")
    )  
  )%>%
    group_by(subject,exp)%>%
    mutate(force=force/max(force))
  
}

dat <- do.call(rbind,dat.l)

dat

dat%>%
  ggplot(aes(ang,force,col=exp))+geom_point()

AICs <- dat%>%
  group_by(subject,exp)

AICs%>%
  summarize(
    m2=AICc(lm(force~poly(ang,2))), #second order
    m3=AICc(lm(force~poly(ang,3))), #third order
    m4=AICc(lm(force~poly(ang,4))) #fourth order
  ) %>% 
  print()

