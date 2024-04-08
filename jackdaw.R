J <-read.table("Jackdaw+BreedingSum.txt",header = T)
Jx <-read.table("jackdawX.txt",header = T)
J$box <- as.factor(J$box)
View(J)
summary(J)
head(J)

par(mfrow=c(1,1))
par(mfrow=c(1,2))
par(mfrow=c(2,2))

palette<-colors()[c("#D00000","#FFBA08","#3F88C5","#032B43","#136F63")]

line()

#####initial plots//feeling out the data set####

plot(J$mean.chick~J$incubating.p,
     xlim=c(0,100),ylim=c(120,280),
     xlab="Percentage of Time spent Incubating (%)",
     ylab="Average Chick Mass",
     main="Average Chick Mass dependant on Percentage of Time spend Incubating",
     pch=19,bty="l")

 plot(J$mean.chick~J$incubating.p)

#%time spent incubating dependant on %time spent male food calling/female begging
plot(J$incubating.p~J$male.food.call.p)
plot(J$incubating.p~J$female.begging.p)

#time spent together and mean chick mass
CT<-lm(together.p~mean.chick,data=J)
CT<-lm(mean.chick~together.p,data=J)
plot(J$mean.chick~J$together.p,xlim=c(0,25))
abline(CT)

#####Female communication vs Other Behaviours#####

  #all these graphs exclude boxes where females spent none of their time begging, to see this, remove the xlim or set the lower bound to 0

#Percentage of time spent together dependent on percentage of time spent begging by female
plot(J$together.p~J$female.begging.p,
     xlim=c(0.0,1),main="Together %")

#Percentage of time spent incubating dependent on percentage of time spent begging by female
plot(J$incubating.p~J$female.begging.p,
     xlim=c(0.05,1),main="Incubating %")

#Percentage of time spent out (by male) dependent on percentage of time spent begging by female
plot(J$male.out.p~J$female.begging.p,
     xlim=c(0.05,1),main="Male OUT %")

#Percentage of time spent together dependent on percentage of time spent begging by female
plot(J$nest.pick.p~J$female.begging.p,
     xlim=c(0.05,1),main="Nest Pick %")

#compare the significance of these with "paired t test"

#testing normality
hist(J$female.begging.p)
qqnorm(J$female.begging.p)
qqline(J$female.begging.p)

shapiro.test(J$female.begging.p)
  #W = 0.48041, p-value = 1.375e-13

fbeg2<-(J$female.begging.p)^2 #squaring didn't work
hist(fbeg2)
qqnorm(fbeg2)
rm(fbeg2)

logfbeg<-log(J$female.begging.p)
hist(logfbeg) #normal (?)

#J$together.p normality
hist(J$together.p,breaks = 20)

logtog<-log(J$together.p)
hist(logtogetherp) #more normal

#by the looks of it I can just "log" all the J$XXXX.p

##quick paired t test
t.test(logfbeg, logtog, 
       paired = TRUE, alternative = "two.sided")
#returned error

is.na(logfbeg)
is.na(logtog)

#this works but obviously isn't right cause neither are normal
t.test(J$female.begging.p,J$together.p,
       paired = TRUE, alternative = "two.sided")

wilcox.test(J$female.begging.p, J$together.p, paired=TRUE)
#p = 7.766e-12
  #significant relationship
#ok so this means

wilcox.test(J$female.begging.p, J$incubating.p,
            paired=TRUE)
#p = 5.294e-12

wilcox.test(J$female.begging.p, J$male.out.p,
            paired=TRUE)
#p-value = 1.139e-11

wilcox.test(J$female.begging.p, J$nest.pick.p,
            paired=TRUE)
#p-value = 2.588e-07

"important bit here"
#alll 4 of these are significant so
  # % of time spent begging by females has a significant effect on:
    #time spent together
    #time spent out of box by m
    #time spent incubation by f
    #time spent nest picking by f
#do those last 2 make sense?
#can a female jackdaw have an effect on her own behaviour????

#####Female behaviour and Male behaviour#####

#male food call dependent on female begging call
plot(J$male.food.call.p~J$female.begging.p)
  #no correlation by the looks of it

#


#### 0 - General trends in Jackdaw Behaviour ####



plot(J$box[J$together.p>0],J$together.p[J$together.p>0])
barplot(J$together.p[J$together.p>0],J$box[J$together.p>0])

boxplot(J$together.p,J$female.exit.p,J$male.out.p)

#the two most common behaviours for each sex, and time together
boxplot(J$incubating.p,J$male.out.p,J$together.p)

#what females got up to
boxplot(J$incubating.p,J$nest.pick.p,J$female.exit.p,
        names=c("Incubating","Nest Pick","Exit"),
        xlab="Female Behaviour",
        ylab="Percentage of Total Time (%)",
        main="Percentage of Total Time spent on different Behaviours in females",
        ylim=c(0,100),
        col=c(wes_palette("FantasticFox1")),pch=18)

#female begging time maybe not worth representing on a graphic, only between 0% and 1% of time

#summary of behaviours

boxplot(J$incubating.p,J$nest.pick.p,J$female.exit.p,
        J$male.out.p,J$together.p,
        names=c("Incubating","Nest Pick","Female Exit",
                "Male Exit","Together"),
        xlab="Behaviour",ylab="Percentage of Total Time (%)",
        ylim=c(0,100),
        col=c("#D00000","#FFBA08","#3F88C5","#F3DAD8","#136F63"),
        pch=18)
title("a",adj=0)

#highest to lowest
#J$incubating.p,J$male.out.p,J$together.p,J$nest.pick.p,J$female.exit.p

wesanderson::wes_palettes

boxplot(Jx$female.begging.p[Jx$female.begging.p>0],
        Jx$male.food.call.p[Jx$male.food.call.p>0],
        names=c("Female Begging Calls","Male Food Calls"),
        xlab="Behaviour",ylab="Percentage of Total Time (%)",
        col=c("#D00000","#FFBA08"),
        pch=18)
title("b",adj = 0)

J$incubating.p,J$nest.pick.p,J$female.exit.p,
J$male.out.p,J$together.p


round(mean(Jx$food.share.p),3)
round(sd(Jx$food.share.p),3)
levels(Jx$together.p)
range(Jx$food.share.p)
round(0.02144,3)

view(Jx)

summary(Jx)

#### 0.1 - Trends in Jackdaw Reproductive success ####

round(mean(Jx$cl.size,na.rm = T),3)
mean(Jx$n.hatch)
any(is.na(Jx$n.hatch))
average
range(Jx$min.chick,na.rm=T)

#boxplot of cluctch size, number hatched, number fledged
boxplot(J$cl.size,J$n.hatch,J$n.fledge)

#mean chick boxplot
boxplot(J$max.chick, J$mean.chick, J$min.chick,
        ylim=c(100,300),
        names=c("Maximum","Mean","Minimum"),
        xlab="Chick Size",ylab="Grams (g)",
        col=c("#D00000","#FFBA08","#3F88C5"),
        pch=18)
title("a",adj=0)

boxplot(Jx$cl.size,Jx$n.hatch,Jx$n.fledge,
        names=c("Clutch Size","Number Hatched","Number Fledged"),
        xlab="Reproductive Stage",ylab="Number of Offspring",
        col=c("#D00000","#FFBA08","#3F88C5"),
        pch=18)
title("b",adj=0)

line()


#### 1 - Effects of female vocalisations####

#non are normal nor could they be transformed
#wilcox.test used

#%time female begging & %time male out
wilcox.test(J$female.begging.p, J$male.out.p,          paired=TRUE)
#V = 0, p-value = 1.139e-11
  #there is a significant difference between the %time spent begging by F and %time spent out by male

#this is the wrong test
#need a correlation test//LINEAR REGRESSION

m1<-lm(male.out.p~female.begging.p, data=J)
summary(m1)
plot(m1)
#linear regression indicates that %time spent out by male can't be predicted by %time begging by female, but as one time increases so does the other
#(no rounding)
#R^2=0.001216,t61=0.273,P=0.7861,Y=75.605-3.994X

m2<-lm(J$male.out.p[J$female.begging.p>0]
       ~J$female.begging.p[J$female.begging.p>0])
plot(m2)
summary(m2)
#R^2=0.02125,t11=0.489,P=0.6347,Y=71.912+11.518X
#lr indicates %t male out can't be predicted by %t female begging call, but one tends to increase as the other does

#%time male food calls ~ %time fem beg call
mMC.FBC<-lm(J$male.food.call.p[J$female.begging.p>0]
            ~J$female.begging.p[J$female.begging.p>0])
plot(mMC.FBC)
#plots were slightly wacky for this one

summary(mMC.FBC)
#R^2=0.2153,t11=1.737,P=0.110,Y=0.007044+0.124408X
#lr indicates %t male food calls can't be predicted by %t female beg call, but there is a positive correlation between the two


# no. foodshares ~ %t fem beg
mF.B<-
  lm(Jx$food.share.p[Jx$female.begging.p>0]
     ~Jx$female.begging.p[Jx$female.begging.p>0])
plot(mF.B)
#plots fine

summary(mF.B)
#R^2=0.4098,t11=2.764,P=0.01843,Y=1.5563+2.9959X

#(for food share t) R^2=0.4409,t11=2.945,P=0.01332,Y=4.587+7.949X
    #OMG SIGNIFICANCE LETS GOOOOOOOOO
#lreg indicates the number of food shares can be predicted by the % of time spent begging by females, as the % of time increases, so does the number of food shares

#%t incubating ~ %t female begging
  #can you predict how much incubation occurs based on how much begging occurs

mIB<-
  lm(J$incubating.p[J$female.begging.p>0]
     ~J$female.begging.p[J$female.begging.p>0])
plot(mIB)
#actually nice graphs

summary(mIB)
#R^2=8.203e-06,t11=0.009,P=0.993,Y=80.7005+0.1623X
#lr indicates %t spent begging doesn't significantly predict time spent incubating

#time together ~ time beg
mTB<-
  lm(J$together.p[J$female.begging.p>0]
     ~J$female.begging.p[J$female.begging.p>0])
plot(mTB)
summary(mTB)

plot(J$together.p[J$female.begging.p>0]
        ~J$female.begging.p[J$female.begging.p>0])

#R^2=0.3911,t11=-2.658,P=0.0223,Y=16.500-17.628X
#lr indicates %t together can be predicted from %t fem beg, as %t fem beg increases the time spent together decreases
  #suggests more begging get male to go out more

##### 2 - advantages of communication #####

######what predicts mean chick mass####

#mean chick mass ~ together
mCmT<-lm(J$mean.chick~J$together.p)
plot(mCmT) #perfect
summary(mCmT)
#R^2=0.02188,t50=-1.058,P=0.295,Y=211.5733-0.8153X
#chick mass can't be predicted by time spent together, but as time together increases, chick mass slightly decreases (v week cor)

#mean chick mass ~ food shares
mCmF<-lm(Jx$mean.chick[Jx$food.share.p>0]~Jx$food.share.p[Jx$food.share.p>0])
plot(mCmF) #good
summary(mCmF)
#R^2=0.04397,t50=1.516,P=0.1357,Y=192.332+2.619X
#not sig
#chick mass can't be predicted by number of food shares

#age discrep
  #age discrep weird cause can only be true/false
hist(J$mean.chick,breaks=20)
shapiro.test(J$mean.chick)
#normal
length(J$mean.chick[J$age.discrep==TRUE])

t.test(J$mean.chick[J$age.discrep==TRUE],
       J$mean.chick[J$age.discrep==FALSE])
#not sig
#t40.898=0.10666, P=0.9156
#there is no significant difference between mean chick mass between pairs of the same age and pairs of different ages

#new pair
#not actually sure how to get this out of the data, maybe the same as age discrep
t.test(J$mean.chick[J$age.discrep==TRUE],
       J$mean.chick[J$age.discrep==FALSE])

#chick mass ~ fem calls
mCmFc<-lm(J$mean.chick[J$female.begging.p>0]~J$female.begging.p[J$female.begging.p>0])
plot(mCmFc)
summary(mCmFc)
#R^2=0.002937,t8=0.154,P=0.8818,Y=195.034+5.721X
#chick mass not dependant on fem beg calls

mCmMc<-lm(J$mean.chick[J$male.food.call.p>0]~J$male.food.call.p[J$male.food.call.p>0])
plot(mCmMc)
summary(mCmMc)
#R^2=0.01023,t17=0.419,P=0.6804,Y=201.932+61.156X
#does not predict

#chick mass ~ incubating
mCmI<-lm(J$mean.chick[J$incubating.p>0]~J$incubating.p[J$incubating.p>0])
summary(mCmI)
#R^2=3.907e-05,t50=0.044,P=0.965,Y=201.71916+0.02171X
#no correlation between these two, let alone significant

#### total chick ####

#age discrep
t.test(J$total.chick[J$age.discrep==TRUE],
       J$total.chick[J$age.discrep==FALSE])
#not sig
#t47.013=0.34552,P=0.7312

#together
n1<-lm(J$total.chick~J$together.p)
summary(n1)
#R^2=0.0272,t50=1.182,P=0.2426,Y=382.035+4.192X

#foodshare

n2<-lm(Jx$total.chick~Jx$food.share.p)
summary(n2)
#R^2=0.04989,t50=1.620,P=0.1114,Y=359.774+12.867X

n3<-lm(J$total.chick[J$male.food.call.p>0]~J$male.food.call.p[J$male.food.call.p>0])
summary(n3)
#R^2=0.01574,t17=0.521,P=0.6088,Y=418.34+386.44X
plot(J$total.chick[J$male.food.call.p>0]~J$male.food.call.p[J$male.food.call.p>0])

n4<-lm(J$mean.chick[J$female.begging.p>0]~J$female.begging.p[J$female.begging.p>0])
summary(n4)
#R^2=0.002937,t8=0.154,P=0.8818,Y=195.034+5.721X

#### clutch size ####

#clutch size ~ together
b5<-lm(Jx$cl.size~Jx$together.p)
summary(b5)
#R2=0.02618, t61=-1.281, P=0.205, Y=4.61733-0.02384X

#food share
b6<-lm(Jx$cl.size~Jx$food.share.p)
summary(b6)
#R2=0.0008725, t61=-0.231, P=0.8182, Y= 4.434683-0.009111X

b7<-lm(Jx$cl.size~Jx$female.begging.p)
summary(b7)
#R2=0.009482, t61=-0.764, P=0.4477, Y=4.4306-0.4781X

b8<-lm(Jx$cl.size~Jx$male.food.call.p)
summary(b8)
#R2=0.001353, t61=-0.287, P=0.7747, Y=4.3797-0.8011X

t.test(Jx$cl.size[Jx$age.discrep==T],
       Jx$cl.size[Jx$age.discrep==F])
#t49.733=1.854, P=0.06968

#### what predicts number hatched####
#together,foodshare,age discrep,calls

#together
m2<-lm(J$n.hatch[J$together.p>0]~J$together.p[J$together.p>0])
plot(m2)
summary(m2)
#R^2=0.0006011,t59=0.188,P=0.8512,Y=3.47034+0.00575X
#not sig
plot(J$n.hatch[J$together.p>0]~J$together.p[J$together.p>0])

#foodshare
m3<-lm(Jx$n.hatch[Jx$food.share.p>0]~Jx$food.share.p[Jx$food.share.p>0])
summary(m3)
#R^2=0.004005,t59=0.487,P=0.628,Y=3.39344+0.03098X
#not sig

#age discrep
t.test(J$n.hatch[J$age.discrep==TRUE],
       J$n.hatch[J$age.discrep==FALSE])
#t44.156=0.5212, P=0.6048
#not sig

#female begging call
m4<-lm(J$n.hatch[J$female.begging.p>0]~J$female.begging.p[J$female.begging.p>0])
summary(m4)
#R^2=0.01659,t11=-0.431,P=0.6749,Y=3.2043-0.8218X
#not sig

#male food cal
m5<-lm(J$n.hatch[J$male.food.call.p>0]~J$male.food.call.p[J$male.food.call.p>0])
summary(m5)
#R^2=0.008562,t23=-0.446,P=0.66,Y=3.5533--2.8384X
#not sig








####fledged####
#together,food,male c,fem c, age discrep

#together
b1<-lm(J$n.fledge~J$together.p)
plot(b1)
summary(b1)
#R^2=0.07651,t61=2.248,P=0.0282,Y=1.11672+0.05259X
#SIGNIFICANT

#foodshare
b2<-lm(Jx$n.fledge~Jx$food.share.p)
summary(b2)
#R2=0.02884, t61=1.346, P=0.183, Y=1.32229+0.06760X

#(t) R^2=0.04717,t61=1.738,P=0.08729,Y=1.301194+0.016095X
#not significant but close

b3<-lm(J$n.fledge[J$male.food.call.p>0]~J$male.food.call.p[J$male.food.call.p>0])
b3.1<-lm(J$n.fledge~J$male.food.call.p)
summary(b3.1)
#different value w / w/o 0s but neither significant
  #so decide what ya doin here
#(without 0s)
#R^2=0.0001115,t23=0.051,P=0.96,Y=1.5461+0.2568X

b4<-lm(J$n.fledge[J$female.begging.p>0]~J$female.begging.p[J$female.begging.p>0])
summary(b4)
#R^2=0.01014,t11=-0.336,P=0.7434,Y=1.4019-0.5001X
b4x<-lm(J$n.fledge~J$female.begging.p)
summary(b4x)


t.test(J$n.fledge[J$age.discrep==T],
       J$n.fledge[J$age.discrep==F])
#t53.834=0.68983, P=0.4933

round(mean(Jx$intruder.p),3)

##### Communication & Incubation #####

#together
c2<-lm(Jx$incubating.p~Jx$together.p)
summary(c2)
#R2=0.02563, t61=1.267, P=0.2101, Y=79.0966+0.2462X

#food shares
c3<-lm(Jx$incubating.p~Jx$food.share.p)
summary(c3)
#R2=0.003824, t61=0.484, P=0.6302, Y=80.5467+0.1991X

#fem beg call
c4<-lm(Jx$incubating.p~Jx$female.begging.p)
summary(c4)
#R2=0.00076, t61=-215, P=0.8302, Y=81.474-1.413X

#mal food call
c5<-lm(Jx$incubating.p~Jx$male.food.call.p)
summary(c5)
#R2=0.04804, t61=1.755, P=0.08435, Y=80.31+49.84X
  #quite strong even if not sig
      #nearly sig

#age discrep
t.test(Jx$incubating.p[Jx$age.discrep==T],
       Jx$incubating.p[Jx$age.discrep==F])
#t53.478=1.1141, P=0.2702

plot()










#### Further Analysis / Hypothesis ####

# variation in chick mass

hist(Jx$mean.chick,breaks=20)

t.test(Jx$max.chick,Jx$min.chick)
summary
var.test(Jx$mean.chick)
chisq.test(Jx$mean.chick)

# greater incubation coordination

c1<-lm(Jx$incubating.p~Jx$together.p)
summary(c1)

# male invesetment
round(mean(Jx$food.share.p),3)
round(sd(Jx$food.share.p),3)

# heatmap
install.packages("ggplot2")
library("ggplot2")
library(reshape2)
H <-read.table("heatmap.txt",header = T)
H1 <-read.table("heatmap1.txt",header = T)

df <- na.omit(H1)

cor_df <- round(cor(as.matrix(df)), 2)

melted_cormat <- melt(cor_df)

ggplot(data = melted_cormat, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile() +
  geom_text(aes(Var2, Var1, label = value), size = 5) +
  scale_fill_gradient2(low = "#3F88C5", high = "#D00000",
                       limit = c(-1,1), name="Correlation") +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
        panel.background = element_blank())

# Get lower triangle of the correlation matrix
get_lower_tri<-function(cor_df){
  cor_df[upper.tri(cor_df)] <- NA
  return(cor_df)
}
# Get upper triangle of the correlation matrix
get_upper_tri <- function(cor_df){
  cor_df[lower.tri(cor_df)]<- NA
  return(cor_df)
}

upper_tri <- get_upper_tri(cor_df)

melted_cormat <- melt(upper_tri, na.rm = TRUE)

ggplot(data = melted_cormat, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal()+ 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))+
  coord_fixed()

reorder_cor_df <- function(cor_df){
  # Use correlation between variables as distance
  dd <- as.dist((1-cor_df)/2)
  hc <- hclust(dd)
  cor_df <-cor_df[hc$order, hc$order]
}

cor_df <- reorder_cor_df(cor_df)
upper_tri <- get_upper_tri(cor_df)

melted_cormat <- melt(upper_tri, na.rm = TRUE)

ggheatmap <- ggplot(melted_cormat, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal()+ # minimal theme
  theme(axis.text.x = element_text(angle = 70, vjust = 1, 
                                   size = 8, hjust = 1))+
  coord_fixed()
print(ggheatmap)

ggheatmap + 
  geom_text(aes(Var2, Var1, label = value), color = "black", size = 1.8,fontface = "bold") +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    axis.ticks = element_blank(),
    legend.justification = c(1, 0),
    legend.position = c(0.6, 0.7),
    legend.direction = "horizontal")+
  guides(fill = guide_colorbar(barwidth = 7, barheight = 1,
                               title.position = "top", title.hjust = 0.5))

+ theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

