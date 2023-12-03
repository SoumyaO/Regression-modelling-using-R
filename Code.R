# load dataset
wine <- read.table("wine.txt", header = TRUE)
# regress parker using the other variables
# ==========================================
pp <- lm("parker ~ year + h.rain + s.temp + w.rain + h.temp", data=wine)
summary(pp) # R2 30.11
# remove h.temp and year
pp <- lm("parker ~ h.rain + s.temp + w.rain", data=wine)
summary(pp) # R2 30 -> 35, Error 7.94 > 7.6
# remove h.rain
pp <- lm("parker ~ s.temp + w.rain", data=wine)
summary(pp) # R2 32.2
# Regressing price from other parameters
# ==========================================
# remove na rows from price and parker
wine2 <- wine[!is.na(wine$price),]
wine2 <- wine2[!is.na(wine2$parker),]
# correlation matrix
cor(wine2)
# plotting function to save plots
plotter = function(df, name1, name2){
  # plot model fit
  png(name1)
  par(mfrow=c(2,2))
  plot(df)
  dev.off()
  png(name2)
  par(mfrow=c(2, 3))
  plot(wine2$year, residuals(df))
  plot(wine2$parker, residuals(df))
  plot(wine2$h.rain, residuals(df))
  plot(wine2$s.temp, residuals(df))
  plot(wine2$h.temp, residuals(df))
  plot(wine2$w.rain, residuals(df))
  dev.off()}
# regress price on all predictor variables
wp2 <- lm("price ~ year + h.rain + s.temp + w.rain + h.temp + parker",
          data=wine2)

summary(wp2) # An adjusted R2 of 0.66 was observed with a RSE of 4.928.
plotter(wp2, "wp2_plot.png", "wp2_residuals.png")
wp3 <- lm("log10(price) ~ year + h.rain + s.temp + w.rain + h.temp + parker",
          data=wine2)
summary(wp3) # adj R2 was 0.78 and the RSE was 1.2
plotter(wp3, "wp3_plot.png", "wp3_residuals.png")
# removing w.rain from the model as it is least significant
wp4 <- lm("log10(price) ~ year + h.rain + s.temp + h.temp + parker",
          data=wine2)
summary(wp4)
plotter(wp4, "wp4_plot.png", "wp4_residuals.png")
# removing h.rain from the model as it is least significant
wp5 <- lm("log10(price) ~ year + s.temp + h.temp + parker", data=wine2)
summary(wp5)
plotter(wp5, "wp5_plot.png", "wp5_residuals.png")
# removing h.temp from the model as it is least significant
wp6 <- lm("log10(price) ~ year + s.temp + parker", data=wine2)
summary(wp6)
plotter(wp6, "wp6_plot.png", "wp6_residuals.png")
# removing s.temp from the model as it is least significant
wp7 <- lm("log10(price) ~ year + parker", data=wine2)
summary(wp7) # R2 reduced from wp6. Dont remove s.temp
plotter(wp7, "wp7_plot.png", "wp7_residuals.png")
# we confirm our choice of variables by performing a stepwise AIC analysis
library("MASS")
wp0 <- lm("price ~ 1", data=wine2)
stepAIC(wp0, ~year + h.rain + s.temp + w.rain + h.temp + parker, data=wine2,
        direction="both")
anova(wp6) # ANOVA analysis
library("car")
car::vif(wp6) # VIF 1.290710 1.431326 1.425071
# prediction analysis
test <- data.frame(price=27, year=1978, parker=86, s.temp=15.8)
test <- data.frame(price=17, year=1988, parker=87.6, s.temp=17.1)
test <- data.frame(price=11, year=1984, parker=65, s.temp=16.5)
confidence <- predict(wp6, test, se.fit=T, interval=c("confidence"))
prediction <- predict(wp6, test, se.fit=T, interval=c("prediction"))
