library(data.table)
library(dplyr)
library(ggplot2)
library(stringr)
library(DT)
library(tidyr)
library(corrplot)
library(leaflet)
library(lubridate)
library(caret)
library('ggplot2') # visualization
library('ggthemes') # visualization
library('scales') # visualization
library('dplyr') # data manipulation
library('mice') # imputation
library('randomForest') # classification algorithm

# set the working directory
# setwd("C:/Users/502689880/Desktop/FMI_Estimates")
# setwd("C:/Users/502689880/Desktop/FMI_Estimates")
getwd() # where am I?
list.files("dataSets") # files in the dataSets folder

setwd("C:/Users/502689880/Desktop/FMI_Estimates")

requirements <- read.csv("requirements.csv", header = TRUE, 
                         stringsAsFactors = FALSE, na.strings = c(""," ","NA"))
hours <- read.csv("hours.csv", header = TRUE, 
                  stringsAsFactors = FALSE, na.strings = c(""," ","NA"))
hourstotal <- aggregate(num_hours ~ id_project + id_task, hours, sum)
colnames(hourstotal)[3] <- "num_totalHours"

hours <- hours[which(hours$id_baseTask == '0124'),]

hours <- merge(hours, hourstotal, by=c("id_project", "id_task"))
hours <- merge(requirements, hours, by="id_project")

hours <- unique(hours[,1:54])

tasks <- read.csv("tasks2.csv", header = TRUE, stringsAsFactors = FALSE, na.strings = c(""," ","NA","#N/A"))

tasks <- subset(tasks, select = c("id_task", "id_baseTask", 
                                  "id_subtask", "desc_baseTask", "desc_task"))
tasks <- unique(tasks[,1:5])
tasks <- tasks[which(tasks$id_baseTask == '0124'),]

req <- read.csv("req.csv", header = TRUE, 
                stringsAsFactors = FALSE)
frame <- read.csv("frame.csv", header = TRUE, 
                  stringsAsFactors = FALSE)
task <- read.csv("task.csv", header = TRUE, 
                 stringsAsFactors = FALSE)
task <- merge(task, tasks, by="id_task")
task <- task[which(task$id_baseTask == '0124'),]
task <- subset(task, select = c("num", "id_task", "id_baseTask"))
task <- unique(task[,1:3])
spec <- merge(req, frame, by="num")
spec <- unique(spec[,1:5])
spec <- merge(spec, task, by="num")
spec <- unique(spec[,1:7])

# Convert character variables to factors
hours$id_frame <- as.factor(hours$id_frame)
hours$id_req <- as.factor(hours$id_req)
hours$id_task <- as.factor(hours$id_task)
hours$desc_frameFam <- as.factor(hours$desc_frameFam)

# Create new columns for numeric representation
hours["num_idFrame"] <- NA
hours["num_idReq"] <- NA
hours["num_idTask"] <- NA
hours["num_descFrameFam"] <- NA

# Convert factor variables to numeric
hours$num_idFrame <- as.numeric(hours$id_frame)
hours$num_idReq <- as.numeric(hours$id_req)
hours$num_idTask <- as.numeric(hours$id_task)
hours$num_descFrameFam <- as.numeric(hours$desc_frameFam)

summary(hours)

hours <- subset(hours, select = c("id_project", "id_req", "id_frame", 
                                  "id_task", "desc_frameFam", 
                                  "num_idReq", "num_idFrame", "num_idTask", 
                                  "num_descFrameFam", "num_totalHours"))
hours <- unique(hours[,1:10])

library(ggplot2)  #install.packages('ggplot2') may need to be run if you don't have the package
qplot(data = hours, x = num_totalHours) + ylab("Number of Projects")

##Create function to remove outliers
remove_outliers <- function(x, na.rm = TRUE, ...) {
  qnt <- quantile(x, probs = c(.25, .75), na.rm = na.rm, ...)
  H <- 1.5 * IQR(x, na.rm = na.rm)
  y <- x
  y[x < (qnt[1] - H)] <- NA
  y[x > (qnt[2] + H)] <- NA
  y
}

##Remove outliers
hours$num_totalHours <- remove_outliers(hours$num_totalHours)
hours <- na.omit(hours)

# Create Estimate Tables Est=T/R/F.v, Est2=T/R/F, Est3=T/R, Est4=T/F.v, 
# Est5=T/F, Est6=T
hours.sub <- subset(hours, select = c("id_req", "id_frame", 
                                      "id_task", "num_idReq", 
                                      "num_idFrame", "num_idTask"))

est <- merge(spec, hours.sub, by=c("id_req", "id_frame", "id_task"))
est <- unique(est[,1:10])
summary(est)

hours2.sub <- subset(hours, select = c("id_req", "desc_frameFam", 
                                       "id_task", "num_idReq", 
                                       "num_descFrameFam", 
                                       "num_idTask"))
est2 <- merge(spec, hours2.sub, by=c("id_req", "desc_frameFam", 
                                     "id_task"))
est2 <- unique(est2[,1:10])
summary(est2)

hours3.sub <- subset(hours, select = c("id_req", "id_task", 
                                       "num_idReq", "num_idTask"))
est3 <- merge(spec, hours3.sub, by=c("id_req", "id_task"))
est3 <- unique(est3[,1:9])
summary(est3)

hours4.sub <- subset(hours, select = c("id_frame", "id_task", 
                                       "num_idFrame", "num_idTask"))
est4 <- merge(spec, hours4.sub, by=c("id_frame", "id_task"))
est4 <- unique(est4[,1:9])
summary(est4)

hours5.sub <- subset(hours, select = c("id_frame", "desc_frameFam", 
                                       "id_task", "num_idFrame", 
                                       "num_descFrameFam", 
                                       "num_idTask"))
est5 <- merge(spec, hours5.sub, by=c("id_frame", "desc_frameFam", 
                                     "id_task"))
est5 <- unique(est5[,1:10])
summary(est5)


# Fit Models
hours.lm <- lm(num_totalHours ~ num_idFrame + num_idTask + num_idReq, 
               data = hours)
summary(hours.lm)

class(hours.lm)
names(hours.lm)
methods(class = class(hours.lm))[1:9]

confint(hours.lm, level = 0.95)

par(mar = c(4,4,2,2), mfrow = c(1, 2))
plot(hours.lm, which = c(1, 2))

anova(hours.lm)
coef(summary(hours.lm))

hours.loglm <- lm(log(num_totalHours) ~ log(num_idFrame + num_idTask + 
                                              num_idReq), data = hours)
summary(hours.loglm)

class(hours.loglm)
names(hours.loglm)
methods(class = class(hours.loglm))[1:9]

confint(hours.loglm, level = 0.95)

par(mar = c(4,4,2,2), mfrow = c(1, 2))
plot(hours.loglm, which = c(1, 2))

anova(hours.loglm)
coef(summary(hours.loglm))

##
hours2.lm <- lm(num_totalHours ~ num_descFrameFam + num_idTask + num_idReq, 
                data = hours)
summary(hours2.lm)

class(hours2.lm)
names(hours2.lm)
methods(class = class(hours2.lm))[1:9]

confint(hours2.lm, level = 0.95)

par(mar = c(4,4,2,2), mfrow = c(1, 2))
plot(hours2.lm, which = c(1, 2))

anova(hours2.lm)
coef(summary(hours2.lm))

hours2.loglm <- lm(log(num_totalHours) ~ log(num_descFrameFam + num_idTask 
                                             + num_idReq), 
                   data = hours)
summary(hours2.loglm)

class(hours2.loglm)
names(hours2.loglm)
methods(class = class(hours2.loglm))[1:9]

confint(hours2.loglm, level = 0.95)

par(mar = c(4,4,2,2), mfrow = c(1, 2))
plot(hours2.loglm, which = c(1, 2))

anova(hours2.loglm)
coef(summary(hours2.loglm))

##
hours3.lm <- lm(num_totalHours ~ num_idTask + num_idReq, data = hours)
summary(hours3.lm)

class(hours3.lm)
names(hours3.lm)
methods(class = class(hours3.lm))[1:9]

confint(hours3.lm, level = 0.95)

par(mar = c(4,4,2,2), mfrow = c(1, 2))
plot(hours3.lm, which = c(1, 2))

anova(hours3.lm)
coef(summary(hours3.lm))

hours3.loglm <- lm(log(num_totalHours) ~ log(num_idTask + num_idReq), 
                   data = hours)
summary(hours3.loglm)

class(hours3.loglm)
names(hours3.loglm)
methods(class = class(hours3.loglm))[1:9]

confint(hours3.loglm, level = 0.95)

par(mar = c(4,4,2,2), mfrow = c(1, 2))
plot(hours3.loglm, which = c(1, 2))

anova(hours3.loglm)
coef(summary(hours3.loglm))

##
hours4.lm <- lm(num_totalHours ~ num_idTask + num_idFrame, data = 
                  hours)
summary(hours4.lm)

class(hours4.lm)
names(hours4.lm)
methods(class = class(hours4.lm))[1:9]

confint(hours4.lm, level = 0.95)

par(mar = c(4,4,2,2), mfrow = c(1, 2))
plot(hours4.lm, which = c(1, 2))

anova(hours4.lm)
coef(summary(hours4.lm))

hours4.loglm <- lm(log(num_totalHours) ~ log(num_idTask + num_idFrame), 
                   data = hours)
summary(hours4.loglm)

class(hours4.loglm)
names(hours4.loglm)
methods(class = class(hours4.loglm))[1:9]

confint(hours4.loglm, level = 0.95)

par(mar = c(4,4,2,2), mfrow = c(1, 2))
plot(hours4.loglm, which = c(1, 2))

anova(hours4.loglm)
coef(summary(hours4.loglm))

##
hours5.lm <- lm(num_totalHours ~ num_idTask + num_descFrameFam, data = 
                  hours)
summary(hours5.lm)

class(hours5.lm)
names(hours5.lm)
methods(class = class(hours5.lm))[1:9]

confint(hours5.lm, level = 0.95)

par(mar = c(4,4,2,2), mfrow = c(1, 2))
plot(hours5.lm, which = c(1, 2))

anova(hours5.lm)
coef(summary(hours5.lm))

hours5.loglm <- lm(log(num_totalHours) ~ log(num_idTask + num_descFrameFam), 
                   data = hours)
summary(hours5.loglm)

class(hours5.loglm)
names(hours5.loglm)
methods(class = class(hours5.loglm))[1:9]

confint(hours5.loglm, level = 0.95)

par(mar = c(4,4,2,2), mfrow = c(1, 2))
plot(hours5.loglm, which = c(1, 2))

anova(hours5.loglm)
coef(summary(hours5.loglm))

est["num_estimate"] <- NA
est$num_estimate <- predict(hours.lm, newdata = est)
est["num_logEstimate"] <- NA
est$num_logEstimate <- predict(hours.loglm, newdata = est)
error <- merge(est, hours, by=c("id_req", "id_frame", "id_task",
                                "desc_frameFam"))
error <- subset(error, select = c("id_project", "id_req", "id_frame", 
                                  "id_task", "desc_frameFam", 
                                  "num_estimate", 
                                  "num_totalHours"))
error <- unique(error[,1:7])
error["num_logError"] <- NA
error$num_logError <- log(error$num_estimate)-log(error$num_totalHours)
## -- Multivariate lm() "mlm" -----------
utils::example("SSD", echo=FALSE)
error["num_resSD"] <- NA
error$num_resSD <- sigma(hours.lm) # is the same as {but more efficient than}
error["num_oneSigma"] <- NA
error$num_oneSigma <- error$num_estimate+error$num_resSD

est2["num_estimate"] <- NA
est2$num_estimate <- predict(hours2.lm, newdata = est2)
est2["num_logEstimate"] <- NA
est2$num_logEstimate <- predict(hours2.loglm, newdata = est2)
error2 <- merge(est2, hours, by=c("id_req", "id_frame", "id_task",
                                  "desc_frameFam"))
error2 <- subset(error2, select = c("id_project", "id_req", "id_frame", 
                                    "id_task", "desc_frameFam", 
                                    "num_estimate", "num_totalHours"))
error2 <- unique(error2[,1:7])
error2["num_logError"] <- NA
error2$num_logError <- log(error2$num_estimate)-log(error2$num_totalHours)
## -- Multivariate lm() "mlm" -----------
utils::example("SSD", echo=FALSE)
error2["num_resSD"] <- NA
error2$num_resSD <- sigma(hours2.lm) # is the same as {but more efficient than}
error2["num_oneSigma"] <- NA
error2$num_oneSigma <- error2$num_estimate+error2$num_resSD

est3["num_estimate"] <- NA
est3$num_estimate <- predict(hours3.lm, newdata = est3)
est3["num_logEstimate"] <- NA
est3$num_logEstimate <- predict(hours3.loglm, newdata = est3)
error3 <- merge(est3, hours, by=c("id_req", "id_task"))
error3 <- subset(error3, select = c("id_project", "id_req", "id_task", "num_estimate", 
                                    "num_totalHours"))
error3 <- unique(error3[,1:5])
error3["num_logError"] <- NA
error3$num_logError <- log(error3$num_estimate)-log(error3$num_totalHours)
## -- Multivariate lm() "mlm" -----------
utils::example("SSD", echo=FALSE)
error3["num_resSD"] <- NA
error3$num_resSD <- sigma(hours3.lm) # is the same as {but more efficient than}
error3["num_oneSigma"] <- NA
error3$num_oneSigma <- error3$num_estimate+error3$num_resSD

est4["num_estimate"] <- NA
est4$num_estimate <- predict(hours4.lm, newdata = est4)
est4["num_logEstimate"] <- NA
est4$num_logEstimate <- predict(hours4.loglm, newdata = est4)
error4 <- merge(est4, hours, by=c("id_frame", "id_task"))
error4 <- subset(error4, select = c("id_project", "id_frame", "id_task", "num_estimate", 
                                    "num_totalHours"))
error4 <- unique(error4[,1:5])
error4["num_logError"] <- NA
error4$num_logError <- log(error4$num_estimate)-log(error4$num_totalHours)
## -- Multivariate lm() "mlm" -----------
utils::example("SSD", echo=FALSE)
error4["num_resSD"] <- NA
error4$num_resSD <- sigma(hours4.lm) # is the same as {but more efficient than}
error4["num_oneSigma"] <- NA
error4$num_oneSigma <- error4$num_estimate+error4$num_resSD

est5["num_estimate"] <- NA
est5$num_estimate <- predict(hours5.lm, newdata = est5)
est5["num_logEstimate"] <- NA
est5$num_logEstimate <- predict(hours5.loglm, newdata = est5)
error5 <- merge(est5, hours, by=c("id_frame", "id_task",
                                  "desc_frameFam"))
error5 <- subset(error5, select = c("id_project", "id_frame", 
                                    "id_task", "desc_frameFam", 
                                    "num_estimate", "num_totalHours"))
error5 <- unique(error5[,1:6])
error5["num_logError"] <- NA
error5$num_logError <- log(error5$num_estimate)-log(error5$num_totalHours)
## -- Multivariate lm() "mlm" -----------
utils::example("SSD", echo=FALSE)
error5["num_resSD"] <- NA
error5$num_resSD <- sigma(hours5.lm) # is the same as {but more efficient than}
error5["num_oneSigma"] <- NA
error5$num_oneSigma <- error5$num_estimate+error5$num_resSD

write.csv(error, file = "0124_Hours_Estimate.csv")
write.csv(error2, file = "0124_Hours_Estimate2.csv")
write.csv(error3, file = "0124_Hours_Estimate3.csv")
write.csv(error4, file = "0124_Hours_Estimate4.csv")
write.csv(error5, file = "0124_Hours_Estimate5.csv")
########################################################################
library(data.table)
library(dplyr)
library(ggplot2)
library(stringr)
library(DT)
library(tidyr)
library(corrplot)
library(leaflet)
library(lubridate)

setwd("C:/Users/502689880/Desktop/FMI_Estimates")
requirements <- read.csv("requirements.csv", header = TRUE, 
                         stringsAsFactors = FALSE, na.strings = c(""," ","NA"))
hours <- read.csv("hours.csv", header = TRUE, 
                  stringsAsFactors = FALSE, na.strings = c(""," ","NA"))
hourstotal <- aggregate(num_hours ~ id_project + id_task, hours, sum)
colnames(hourstotal)[3] <- "num_totalHours"

hours <- hours[which(hours$id_baseTask == '0124'),]

hours <- merge(hours, hourstotal, by=c("id_project", "id_task"))
hours <- merge(requirements, hours, by="id_project")
hours <- subset(hours, select = c("id_project", "id_req", "id_frame", 
                                  "id_task", "desc_frameFam", 
                                  "num_totalHours"))

hours <- unique(hours[,1:6])

estimate1 <- read.csv("0124_Hours_Estimate.csv", header = TRUE, 
                      stringsAsFactors = FALSE)
estimate2 <- read.csv("0124_Hours_Estimate2.csv", header = TRUE, 
                      stringsAsFactors = FALSE)
estimate3 <- read.csv("0124_Hours_Estimate3.csv", header = TRUE, 
                      stringsAsFactors = FALSE)
estimate4 <- read.csv("0124_Hours_Estimate4.csv", header = TRUE, 
                      stringsAsFactors = FALSE)
estimate5 <- read.csv("0124_Hours_Estimate5.csv", header = TRUE, 
                      stringsAsFactors = FALSE)

#Outcome
estimate1 %>% 
  ggplot(aes(x=num_logError)) + 
  geom_histogram(bins=100, fill="red")+
  theme_bw()+theme(axis.title = element_text(size=16),axis.text = element_text(size=14))+
  ylab("Count")+coord_cartesian(x=c(-5,5))

#Absolute logerror
estimate1 <- estimate1 %>% mutate(abs_logError = abs(num_logError))
estimate1 %>% 
  ggplot(aes(x=abs_logError)) + 
  geom_histogram(bins=100, fill="red")+
  theme_bw()+theme(axis.title = element_text(size=16),axis.text = element_text(size=14))+
  ylab("Count")+coord_cartesian(x=c(0,5))
