#####Figure 2:
setwd("D:/Conditioned survival analysis")
getwd()
install.packages("survival")
install.packages("readxl")
library(survival)
library(readxl)
#####Read data
lung <- read_excel("D:/Conditioned survival analysis/part_solid_LUAD.xlsx")
attach(lung)
fit1 <- survfit(Surv(RFS_month,RFS_status0)~ 1, data = lung) #RFS_status0 represents the full RFS_status
fit2 <- survfit(Surv(RFS_month,RFS_status1)~ 1, data = lung) #RFS_status1 represents the RFS_status after excluding recurrence or censored events within 1 year
fit3 <- survfit(Surv(RFS_month,RFS_status2)~ 1, data = lung) #RFS_status1 represents the RFS_status after excluding recurrence or censored events within 2 year
fit4 <- survfit(Surv(RFS_month,RFS_status3)~ 1, data = lung) #RFS_status1 represents the RFS_status after excluding recurrence or censored events within 3 year
fit5 <- survfit(Surv(RFS_month,RFS_status4)~ 1, data = lung) #RFS_status1 represents the RFS_status after excluding recurrence or censored events within 4 year
fit6 <- survfit(Surv(RFS_month,RFS_status5)~ 1, data = lung) #RFS_status1 represents the RFS_status after excluding recurrence or censored events within 5 year

#####Conditioned survival curves
par(font.lab = 2)
x_axis_limits <- c(0, 125)
plot(fit1, col = "#2d66a5", conf.int = FALSE, 
     xlab = "Time after diagnosis (years)", ylab = "Recurrence-free survival probability",  xaxt = "n",yaxt = "n", 
     ylim = c(0.8, 1.0), xlim = x_axis_limits, lwd = 1.5)
specific_time1 <- 60  
est_prob1 <- summary(fit1, times = specific_time1)

#Setting the x-axis scale pitch
custom_ticks <- seq(0, max(RFS_month)-24, by = 12)
axis(1, at = c(0, custom_ticks), labels = c(0, custom_ticks / 12))
#Setting the y-axis scale pitch
custom_y_ticks <- c(0.8, 0.85,0.9,0.95,1.0)
axis(2, at = custom_y_ticks, las = 1)

lines(fit2, col = "#F6C957", conf.int = FALSE, lwd = 1.5)
specific_time2 <- 72  
est_prob2 <- summary(fit2, times = specific_time2)
lines(fit3, col = "#92B4C8", conf.int = FALSE, lwd = 1.5)
specific_time3 <- 84  
est_prob3 <- summary(fit3, times = specific_time3)
lines(fit4, col = "#E56F5E", conf.int = FALSE, lwd = 1.5)
specific_time4 <- 96  
est_prob4 <- summary(fit4, times = specific_time4)
lines(fit5, col = "#97cebf", conf.int = FALSE, lwd = 1.5)
specific_time5 <- 108  
est_prob5 <- summary(fit5, times = specific_time5)
lines(fit6, col = "#C59D94", conf.int = FALSE, lwd = 1.5)
specific_time6 <- 120  
est_prob6 <- summary(fit6, times = specific_time6)

#Plotting points at a specific time
point_size <- 0.6
points(specific_time1, est_prob1$surv[1], pch = 19, col = "black", cex = point_size)
segments(specific_time1, -1, specific_time1, est_prob1$surv[1], col = "black", lty = 2)
points(specific_time2, est_prob2$surv[1], pch = 19, col = "black", cex = point_size)
segments(specific_time2, -1, specific_time2, est_prob2$surv[1], col = "black", lty = 2)
points(specific_time3, est_prob3$surv[1], pch = 19, col = "black", cex = point_size)
segments(specific_time3, -1, specific_time3, est_prob3$surv[1], col = "black", lty = 2)
points(specific_time4, est_prob4$surv[1], pch = 19, col = "black", cex = point_size)
segments(specific_time4, -1, specific_time4, est_prob4$surv[1], col = "black", lty = 2)
points(specific_time5, est_prob5$surv[1], pch = 19, col = "black", cex = point_size)
segments(specific_time5, -1, specific_time5, est_prob5$surv[1], col = "black", lty = 2)
points(specific_time6, est_prob6$surv[1], pch = 19, col = "black", cex = point_size)
segments(specific_time6, -1, specific_time6, est_prob6$surv[1], col = "black", lty = 2)
#Add a note to the figure
legend(x = 1.6, y = 0.897, legend = c("Overall", "≥1year", "≥2years","≥3years","≥4years","≥5years"), 
       col = c("#2d66a5", "#F6C957", "#92B4C8","#E56F5E", "#97cebf", "#C59D94"), lwd = 2)



#####Figure 3:
setwd("D:/Conditioned survival analysis")
getwd()
install.packages("survival")
install.packages("readxl")
library(survival)
library(readxl)
#####Read data
lung <- read_excel("D:/Conditioned survival analysis/part_solid_LUAD.xlsx")
attach(lung)
#Mapping of 5-year CRFS estimates when the VPI was absence 
subset_VPI0 <- lung[lung$VPI == 0, ]
fit1 <- survfit(Surv(RFS_month,RFS_status0)~ 1, data = subset_VPI0) 
fit2 <- survfit(Surv(RFS_month,RFS_status1)~ 1, data = subset_VPI0)
fit3 <- survfit(Surv(RFS_month,RFS_status2)~ 1, data = subset_VPI0)
fit4 <- survfit(Surv(RFS_month,RFS_status3)~ 1, data = subset_VPI0)
fit5 <- survfit(Surv(RFS_month,RFS_status4)~ 1, data = subset_VPI0)
fit6 <- survfit(Surv(RFS_month,RFS_status5)~ 1, data = subset_VPI0)
#####5-year CRFS estimated curves
specific_time1 <- 60  
est_prob1 <- summary(fit1, times = specific_time1)
specific_time2 <- 72  
est_prob2 <- summary(fit2, times = specific_time2)
specific_time3 <- 84  
est_prob3 <- summary(fit3, times = specific_time3)
specific_time4 <- 96  
est_prob4 <- summary(fit4, times = specific_time4)
specific_time5 <- 108  
est_prob5 <- summary(fit5, times = specific_time5)
specific_time6 <- 120  
est_prob6 <- summary(fit6, times = specific_time6)
par(font.lab = 2)
plot(x = NULL, y = NULL, type = "n", xlim = c(0, 5), ylim = c(0.8, 1), yaxt = "n", 
     xlab = "Time after surgery (years)", ylab = "5−year estimates", main = "Conditional time to recurrence", las = 1)
#Setting the y-axis scale pitch
custom_y_ticks <- c(0.80, 0.85, 0.90, 0.95, 1.00)  # 选择要显示的刻度位置
axis(2, at = custom_y_ticks, las = 1)
point_size <- 1.3
#Adding Points
specific_time1a <- 0  
est_prob1 <- summary(fit1, times = specific_time1)
points(specific_time1a, est_prob1$surv[1], pch = 15, col = "#2d66a5", cex = point_size)
specific_time2a <- 1  
est_prob2 <- summary(fit2, times = specific_time2)
points(specific_time2a, est_prob2$surv[1], pch = 15, col = "#2d66a5", cex = point_size)
specific_time3a <- 2  
est_prob3 <- summary(fit3, times = specific_time3)
points(specific_time3a, est_prob3$surv[1], pch = 15, col = "#2d66a5", cex = point_size)
specific_time4a <- 3  
est_prob4 <- summary(fit4, times = specific_time4)
points(specific_time4a, est_prob4$surv[1], pch = 15, col = "#2d66a5", cex = point_size)
specific_time5a <- 4  
est_prob5 <- summary(fit5, times = specific_time5)
points(specific_time5a, est_prob5$surv[1], pch = 15, col = "#2d66a5", cex = point_size)
specific_time6a <- 5  
est_prob6 <- summary(fit6, times = specific_time6)
points(specific_time6a, est_prob6$surv[1], pch = 15, col = "#2d66a5", cex = point_size)
#Connect the points with a straight line
lines(c(specific_time1a, specific_time2a, specific_time3a, specific_time4a, specific_time5a, specific_time6a), 
      c(est_prob1$surv[1], est_prob2$surv[1], est_prob3$surv[1], est_prob4$surv[1], est_prob5$surv[1], est_prob6$surv[1]), col = "#2d66a5", lwd = 2)
#Mapping of 5-year CRFS estimates when the VPI was present 
subset_VPI1 <- lung[lung$VPI == 1, ]
fit7 <- survfit(Surv(RFS_month,RFS_status0)~ 1, data = subset_VPI1)
fit8 <- survfit(Surv(RFS_month,RFS_status1)~ 1, data = subset_VPI1)
fit9 <- survfit(Surv(RFS_month,RFS_status2)~ 1, data = subset_VPI1)
fit10 <- survfit(Surv(RFS_month,RFS_status3)~ 1, data = subset_VPI1)
fit11 <- survfit(Surv(RFS_month,RFS_status4)~ 1, data = subset_VPI1)
fit12 <- survfit(Surv(RFS_month,RFS_status5)~ 1, data = subset_VPI1)
#####5-year CRFS estimated curves
specific_time1 <- 60  
est_prob1 <- summary(fit7, times = specific_time1)
specific_time2 <- 72  
est_prob2 <- summary(fit8, times = specific_time2)
specific_time3 <- 84  
est_prob3 <- summary(fit9, times = specific_time3)
specific_time4 <- 96  
est_prob4 <- summary(fit10, times = specific_time4)
specific_time5 <- 108  
est_prob5 <- summary(fit11, times = specific_time5)
specific_time6 <- 120  
est_prob6 <- summary(fit12, times = specific_time6)
#Adding Points
point_size <- 1.3
specific_time1a <- 0  
est_prob1 <- summary(fit7, times = specific_time1)
points(specific_time1a, est_prob1$surv[1], pch = 19, col = "#E56F5E", cex = point_size)
specific_time2a <- 1  
est_prob2 <- summary(fit8, times = specific_time2)
points(specific_time2a, est_prob2$surv[1], pch = 19, col = "#E56F5E", cex = point_size)
specific_time3a <- 2  
est_prob3 <- summary(fit9, times = specific_time3)
points(specific_time3a, est_prob3$surv[1], pch = 19, col = "#E56F5E", cex = point_size)
specific_time4a <- 3  
est_prob4 <- summary(fit10, times = specific_time4)
points(specific_time4a, est_prob4$surv[1], pch = 19, col = "#E56F5E", cex = point_size)
specific_time5a <- 4  
est_prob5 <- summary(fit11, times = specific_time5)
points(specific_time5a, est_prob5$surv[1], pch = 19, col = "#E56F5E", cex = point_size)
specific_time6a <- 5  
est_prob6 <- summary(fit12, times = specific_time6)
points(specific_time6a, est_prob6$surv[1], pch = 19, col = "#E56F5E", cex = point_size)
#Connect the points with a straight line
lines(c(specific_time1a, specific_time2a, specific_time3a, specific_time4a, specific_time5a, specific_time6a), 
      c(est_prob1$surv[1], est_prob2$surv[1], est_prob3$surv[1], est_prob4$surv[1], est_prob5$surv[1], est_prob6$surv[1]), col = "#E56F5E", lwd = 2)
#Add a note to the figure
legend_x <- 2
legend_y <- 0.85
legend(x = legend_x, y = legend_y, legend = c("VPI absent", "VPI present"), col = c("#2d66a5", "#E56F5E"), lwd = 2, 
       cex = 1.2,pch =c(15, 19), xpd = TRUE, xjust = 1, yjust = 1)