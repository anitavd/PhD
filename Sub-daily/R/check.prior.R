library(pscl)

#Range, gamma prior
r11 <- 2
r12 <- 2
r21 <- 1.5
r22 <- 1.5
s11 <- 2
s12 <- 6
s21 <- 2
s22 <- 2

#Test priors
s11 <- 2
s12 <- 6
s21 <- 2
s22 <- 2
r11 <- 40
r12 <- 0.1
r21 <- 22.5
r22 <- 0.1

#Location
pri.shape <- r11
pri.scale <- r12
#Posterior
mean <- 3.975
sd <- (5.297 - mean)/1.96
pos.shape <- (mean/sd)^2
pos.scale <- (sd^2)/mean

x <- seq(0, 10, length=100)
pri <- dgamma(x, shape=pri.shape, scale=pri.scale)

#png("/home/anitavd/PhD/Sub-daily/Figs/prior_range_loc_0.5_4.png")

plot(x, pri, type="n", lty=2, lwd=2, xlab="x", ylab=expression(f(x)), ylim=c(0,0.5), frame.plot=F)

lines(x, dgamma(x,shape=pri.shape, scale=pri.scale), lwd=2, col="red")
lines(x, dgamma(x,shape=pos.shape, scale=pos.scale), lwd=2, col="blue")
#dev.off()

#Scale
pri.shape <- r21
pri.scale <- r22
#Posterior
mean <- 4.147
sd <- (23.09-mean)/1.96
pos.shape <- (mean/sd)^2
pos.scale <- (sd^2)/mean

x <- seq(0, 50, length=100)
pri <- dgamma(x, shape=pri.shape, scale=pri.scale)

#png("/home/anitavd/PhD/Sub-daily/Figs/prior_range_scale_1_1.84.png")

plot(x, pri, type="n", lty=2, lwd=2, xlab="x", ylab=expression(f(x)), ylim=c(0,0.5), frame.plot=F)

lines(x, dgamma(x,shape=pri.shape, scale=pri.scale), lwd=2, col="red")
lines(x, dgamma(x,shape=pos.shape, scale=pos.scale), lwd=2, col="blue")
#dev.off()

#Sill
#Location
pri.shape <- s11
pri.scale <- s12

mean.sill <- 2.4389
sd.sill <- (5.8266 - mean.sill)/1.96
pos.shape <- (mean.sill/(sd.sill^2))+2
pos.scale <- mean.sill*((mean.sill/(sd.sill^2))+1)

x <- seq(1, 10, length=100)
pri <- densigamma(x, pri.shape, pri.scale)

png("/home/anitavd/PhD/Sub-daily/Figs/prior_sill_loc_5_7.png")
plot(x, pri, type="n", lty=2, lwd=2, xlab="x", ylab=expression(f(x)), ylim=c(0,0.5), frame.plot=F)

lines(x, densigamma(x,pri.shape, pri.scale), lwd=2, col="red")
lines(x, densigamma(x,pos.shape, pos.scale), lwd=2, col="blue")
dev.off()


#Scale
pri.shape <- s21
pri.scale <- s22

mean.sill <- 1.1417
sd.sill <- (3.0760 - mean.sill)/1.96
pos.shape <- (mean.sill/(sd.sill^2))+2
pos.scale <- mean.sill*((mean.sill/(sd.sill^2))+1)

x <- seq(1, 10, length=100)
pri <- densigamma(x, pri.shape, pri.scale)

png("/home/anitavd/PhD/Sub-daily/Figs/prior_sill_scale_2_2.png")
plot(x, pri, type="n", lty=2, lwd=2, xlab="x", ylab=expression(f(x)), ylim=c(0,0.5), frame.plot=F)

lines(x, densigamma(x,pri.shape, pri.scale), lwd=2, col="red")
lines(x, densigamma(x,pos.shape, pos.scale), lwd=2, col="blue")
dev.off()

