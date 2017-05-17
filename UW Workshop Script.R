vaughdatabig <- read.csv(file = "C:\\Users\\montoya.29\\Documents\\MAC Lab\\Workshops\\UW Mediation in R\\Vaugh2017Study2.csv", header = TRUE)
head(vaughdatabig)
summary(vaughdatabig)

vaughdata <- read.csv(file = "C:\\Users\\montoya.29\\Documents\\MAC Lab\\Workshops\\UW Mediation in R\\Vaugh2017Study2Small.csv", header = TRUE)
head(vaughdata)
foot(vaughdata)

vaughdata[12:17,]

vaughdata[,2:3]

vaughdata$as1

names(vaughdata)

str(vaughdata)

summary(vaughdata)

vaughdata$as1[1:6]
attach(vaughdata)
as1[1:6]
detach()
as1[1:6]

vaughdata <- vaughdata[order(vaughdata$age),]

lownsdata <- subset(vaughdata, nscond == 1)
highnsdata <- subset(vaughdata, nscond == 2)

lownsfemale <- subset(vaughdata, (nscond == 1)&(gender == 2))

#missing values
is.na(vaughdata$howdist)
sum(is.na(vaughdata$howdist))

#Moderation Section: Non-contingent model

vaughnlm2 <- lm(relativepromotion ~ nscond + competence, data = vaughndata)
summary(vaughnlm2)


#Moderation Section: Contingent Model
vaughnlm3 <- lm(relativepromotion ~ nscond*competence, data = vaughndata)
summary(vaughnlm3)

# Probing Interactions in R
comp.mod.coef <- coef(vaughnlm3)
probe.contrast <- c(0, 1, 0, 3.3040)
theta.est.3.3 <- comp.mod.coef%*%probe.contrast
vcov(vaughnlm3)

se.theta.3.3 <- sqrt(probe.contrast%*%vcov(vaughnlm3)%*%probe.contrast)
t.theta.3.3 <- theta.est.3.3/se.theta.3.3
vaughnlm3$df.residual

2*(1 - pt(t.theta.3.3, df = vaughnlm3$df.residual))


###Practice 4 Answers
#1
vaughndata$DichGend <- vaughndata$gender
vaughndata$DichGend[vaughndata$DichGend == 3 | vaughndata$DichGend == 4] <- NA

#2
vaughnlm.gend <- lm(relativepromotion ~ DichGend*nscond, data = vaughndata)
summary(vaughnlm.gend)

#3
#probe effect of condition for men
gend1.coef <- coef(vaughnlm.gend)
gend1.contrast <- c(0, 0, 1, 1)
gend1.theta <- gend1.coef%*%gend1.contrast
vcov(vaughnlm.gend)

gend1.se.theta <- sqrt(gend1.contrast%*%vcov(vaughnlm.gend)%*%gend1.contrast)
gend1.t <- gend1.theta/gend1.se.theta
vaughnlm.gend$df.residual

2*(1 - pt(gend1.t, df = vaughnlm.gend$df.residual))

#probe effect of condition for men
gend1.coef <- coef(vaughnlm.gend)
gend1.contrast <- c(0, 0, 1, 1)
gend1.theta <- gend1.coef%*%gend1.contrast
vcov(vaughnlm.gend)

gend1.se.theta <- sqrt(gend1.contrast%*%vcov(vaughnlm.gend)%*%gend1.contrast)
gend1.t <- gend1.theta/gend1.se.theta
vaughnlm.gend$df.residual

2*(1 - pt(gend1.t, df = vaughnlm.gend$df.residual))


#Testing out linear models and moderation

summary(lm(relativepromotion~nscond, data = vaughndata))
summary(lm(autonomy~nscond, data = vaughndata))
summary(lm(competence~nscond, data = vaughndata))
summary(lm(relatedness~nscond, data = vaughndata))

summary(lm(relativepromotion~nscond+autonomy+competence+relatedness, data = vaughndata))
#use autonomy as mediator
summary(lm(relativepromotion~nscond+autonomy, data = vaughndata))

summary(lm(relativepromotion ~nscond:gender, data = vaughndata))
summary(lm(relativepromotion ~nscond:competence, data = vaughndata))
