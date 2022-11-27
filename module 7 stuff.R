
library(ggplot2)
library(MuMIn)
library(tidyverse)

ang <- seq(45,168.75,length.out = 12) #elbow angle

normF <- c(0.71, 0.77, 0.83, 0.91, 0.97, 1.00, 0.96, 0.94, 0.89, 0.84, 0.77, 0.74) #you'll get this by reading in data and calculating normalized values


qplot(ang,normF)+geom_point(aes(x=ang[which.max(normF)],y=normF[which.max(normF)]),col="red",size=4)

ang[which.max(normF)] #Find angle for Fmax, theta_max. but shouldn't just do 
  # this because the max force could be between angles, so should make a model
  # to try to predict what angle the actual Fmax is at

# next stuff: constructing 3 polynomial models of the 2nd, 3rd, 4th variety 
# and then used AICc to find the lowest AICc score. The 4th order model fits 
# best, so we then produced 1000 discrete values within our range with seq() 
# to pass topredict() to predict force over this range using our best fitting 
# model (producing normF.pred). We then found the max of normF.pred (the Fmax) 
# and plotted that, along with our predictions. Lastly, we found θmax, a value 
# of 104.9.

poly.m2 <- lm(normF~poly(ang,2)) #second order
poly.m3 <- lm(normF~poly(ang,3)) #third order
poly.m4 <- lm(normF~poly(ang,4)) #fourth order

AICc(poly.m2,poly.m3,poly.m4) #the second order model fits best

x.pred <- seq(45,157.5,length.out = 1000) #define 1000 angles from our range

normF.pred <- predict(poly.m4,newdata = data.frame(ang=x.pred)) #predict the force using 1000 angles

qplot(ang,normF)+geom_point(aes(x=x.pred,y=normF.pred),col="red")+geom_point(aes(x=x.pred[which.max(normF.pred)],y=normF.pred[which.max(normF.pred)]),size=5,col="blue")

x.pred[which.max(normF.pred)] #theta_max
  # to predict actual angle of max force

# ----

# compare the two θmax, that is, the shift in angle for Fmax predicted by two 
# polynomial models
normF.fat <- c(0.65, 0.71, 0.81, 0.89,0.92, 0.96, 0.99, 1, 0.96, 0.92, 0.84, 0.77) #fatigue data, you'll get this by reading in data and calculating normalized values

qplot(ang,normF.fat)+geom_point(aes(x=ang[which.max(normF.fat)],y=normF.fat[which.max(normF.fat)]),col="red",size=4)

# ----

ang[which.max(normF.fat)] #Find angle for Fmax for fatigue data

poly.m2.fat <- lm(normF.fat~poly(ang,2)) #second order
poly.m3.fat <- lm(normF.fat~poly(ang,3)) #third order
poly.m4.fat <- lm(normF.fat~poly(ang,4)) #fourth order

AICc(poly.m2.fat,poly.m3.fat,poly.m4.fat) #the second order model fits best

normF.pred.fat <- predict(poly.m2.fat,newdata = data.frame(ang=x.pred)) #predict the force using 1000 angles

qplot(ang,normF)+geom_point(aes(x=x.pred,y=normF.pred.fat),col="red")+geom_point(aes(x=x.pred[which.max(normF.pred.fat)],y=normF.pred.fat[which.max(normF.pred.fat)]),size=5,col="blue")

x.pred[which.max(normF.pred.fat)]-x.pred[which.max(normF.pred)] #shift in angle for Fmax, theta_max


# simulating some control and fatigue data, adding random noise drawn form a 
# normal distribution and plot the data with a color according to experiment 
# to quickly assess the pattern

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

dat <- do.call(rbind,dat.l) #get data into one tibble

dat%>%
  ggplot(aes(ang,force,col=exp))+geom_point()

# ----

# take our data and get AICc scores for our three polynomial models. We’ll 
# group the data by subject and experiment and retrieve AICc from each model 
# (m2, m3, and m4). Then, we’ll pivot the tibble to a longer format so that we
# have a single column for AICc values

AICs <- dat%>%
  group_by(subject,exp)%>%
  summarize(
    m2=AICc(lm(force~poly(ang,2))), #second order
    m3=AICc(lm(force~poly(ang,3))), #third order
    m4=AICc(lm(force~poly(ang,4))) #fourth order
  )%>%
  pivot_longer(m2:m4,names_to="model",values_to="AICc")%>%
  print()

# predicting values for 1000 discrete angles across our range of elbow 
# positions. Notice that we’ve summarized the data by subject, experiment and 
# model to find the angle (x.pred) which matches the max force value

fits <- dat%>%
  group_by(subject,exp)%>%
  summarize(
    m2=predict(lm(force~poly(ang,2)),newdata=data.frame(ang=x.pred)), #second order
    m3=predict(lm(force~poly(ang,3)),newdata=data.frame(ang=x.pred)), #third order
    m4=predict(lm(force~poly(ang,4)),newdata=data.frame(ang=x.pred)) #fourth order
  )%>%
  pivot_longer(m2:m4,names_to="model")%>%
  group_by(subject,exp,model)%>%
  summarize(theta_max=x.pred[which.max(value)])%>%
  print()

#  join these the AICs and fits tables so that we can filter the model 
# predictions by which model fits best. Note filter(best==TRUE) after 
# constructing a best column according to the minimum AICc score using mutate()
best.models <- fits%>%
  left_join(AICs)%>%
  group_by(subject,exp)%>%
  mutate(best=AICc==min(AICc))%>%
  filter(best==TRUE)%>%
  dplyr::select(-best)%>%
  print()

anova(lm(theta_max~exp,best.models))
  # to investigate whether a shift if θmax is different between the control and fatigue experiments

# calculate the mean shift with SEM. To find the shift, we can pivot the 
# best.models tibble to a wider format so we can have θmax for each experiment
# in separate columns and find the difference with mutate(). Then, summarise()
# can be used to find the mean and SEM
best.models%>%
  pivot_wider(id_cols=subject,names_from = exp,values_from=theta_max)%>%
  mutate(shift=fatigue-control)%>%
  ungroup()%>%
  summarise(mean.shift=mean(shift),se.shift=sd(shift)/sqrt(length(shift)))
