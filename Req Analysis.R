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
                         stringsAsFactors = FALSE)
durations <- read.csv("durations.csv", header = TRUE, 
                      stringsAsFactors = FALSE)

durations <- merge(requirements, durations, by="id_project")
durations$date_actStart <- as.Date(durations$date_actStart, "%m/%d/%Y")
durations$date_actFinish <- as.Date(durations$date_actFinish, "%m/%d/%Y")
durations <- unique(durations[,1:35])

req <- read.csv("req.csv", header = TRUE, 
                stringsAsFactors = FALSE)
frame <- read.csv("frame.csv", header = TRUE, 
                  stringsAsFactors = FALSE)
task <- read.csv("task.csv", header = TRUE, 
                 stringsAsFactors = FALSE)
req <- req[which(req$id_baseReq == 'FC1H'),]
spec <- merge(req, frame, by="num")
spec <- unique(spec[,1:5])
spec <- merge(spec, task, by="num")
spec <- unique(spec[,1:6])

# Convert character variables to factors
durations$id_frame <- as.factor(durations$id_frame)
durations$id_baseReq <- as.factor(durations$id_baseReq)
durations$id_task <- as.factor(durations$id_task)
durations$desc_frameFam <- as.factor(durations$desc_frameFam)

# Create new columns for numeric representation
durations["num_idFrame"] <- NA
durations["num_idBaseReq"] <- NA
durations["num_idTask"] <- NA
durations["num_descFrameFam"] <- NA

# Convert factor variables to numeric
durations$num_idFrame <- as.numeric(durations$id_frame)
durations$num_idBaseReq <- as.numeric(durations$id_baseReq)
durations$num_idTask <- as.numeric(durations$id_task)
durations$num_descFrameFam <- as.numeric(durations$desc_frameFam)

summary(durations)

durations <- durations[which(durations$id_baseReq == 'FC1H'),]

ggplot(durations, aes(date_actStart, num_actDur)) + geom_line() +
  scale_x_date() + xlab("") + ylab("FC1H Task Duration")

ggplot(durations, aes(date_actFinish, num_actDur)) + geom_line() +
  scale_x_date() + xlab("") + ylab("FC1H Task Duration")

library(ggplot2)  #install.packages('ggplot2') may need to be run if you don't have the package
qplot(data = durations, x = num_actDur) + ylab("Number of Projects")

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
durations$num_actDur <- remove_outliers(durations$num_actDur)
durations <- na.omit(durations)

# Create Estimate Tables Est=T/R/F.v, Est2=T/R/F, Est3=T/R, Est4=T/F.v, 
# Est5=T/F, Est6=T
durations.sub <- subset(durations, select = c("id_baseReq", "id_frame", 
                                              "id_task", "num_idBaseReq", 
                                              "num_idFrame", "num_idTask"))

est <- merge(spec, durations.sub, by=c("id_baseReq", "id_frame", "id_task"))
est <- unique(est[,1:9])
summary(est)

durations2.sub <- subset(durations, select = c("id_baseReq", "desc_frameFam", 
                                               "id_task", "num_idBaseReq", 
                                               "num_descFrameFam", 
                                               "num_idTask"))
est2 <- merge(spec, durations2.sub, by=c("id_baseReq", "desc_frameFam", 
                                         "id_task"))
est2 <- unique(est2[,1:9])
summary(est2)

durations3.sub <- subset(durations, select = c("id_baseReq", "id_task", 
                                               "num_idBaseReq", "num_idTask"))
est3 <- merge(spec, durations3.sub, by=c("id_baseReq", "id_task"))
est3 <- unique(est3[,1:8])
summary(est3)

durations4.sub <- subset(durations, select = c("id_frame", "id_task", 
                                               "num_idFrame", "num_idTask"))
est4 <- merge(spec, durations4.sub, by=c("id_frame", "id_task"))
est4 <- unique(est4[,1:8])
summary(est4)

durations5.sub <- subset(durations, select = c("id_frame", "desc_frameFam", 
                                               "id_task", "num_idFrame", 
                                               "num_descFrameFam", 
                                               "num_idTask"))
est5 <- merge(spec, durations5.sub, by=c("id_frame", "desc_frameFam", 
                                         "id_task"))
est5 <- unique(est5[,1:9])
summary(est5)

# Fit Models
durations.lm <- lm(num_actDur ~ num_idBaseReq + num_idFrame + num_idTask, 
                   data = durations)
summary(durations.lm)

class(durations.lm)
names(durations.lm)
methods(class = class(durations.lm))[1:9]

confint(durations.lm, level = 0.95)

par(mar = c(4,4,2,2), mfrow = c(1, 2))
plot(durations.lm, which = c(1, 2))

anova(durations.lm)
coef(summary(durations.lm))

##
durations2.lm <- lm(num_actDur ~ num_idBaseReq + num_descFrameFam + num_idTask, 
                    data = durations)
summary(durations2.lm)

class(durations2.lm)
names(durations2.lm)
methods(class = class(durations2.lm))[1:9]

confint(durations2.lm, level = 0.95)

par(mar = c(4,4,2,2), mfrow = c(1, 2))
plot(durations2.lm, which = c(1, 2))

anova(durations2.lm)
coef(summary(durations2.lm))

##
durations3.lm <- lm(num_actDur ~ num_idBaseReq + num_idTask, data = durations)
summary(durations3.lm)

class(durations3.lm)
names(durations3.lm)
methods(class = class(durations3.lm))[1:9]

confint(durations3.lm, level = 0.95)

par(mar = c(4,4,2,2), mfrow = c(1, 2))
plot(durations3.lm, which = c(1, 2))

anova(durations3.lm)
coef(summary(durations3.lm))

##
durations4.lm <- lm(num_actDur ~ num_idFrame + num_idTask, data = 
                      durations)
summary(durations4.lm)

class(durations4.lm)
names(durations4.lm)
methods(class = class(durations4.lm))[1:9]

confint(durations4.lm, level = 0.95)

par(mar = c(4,4,2,2), mfrow = c(1, 2))
plot(durations4.lm, which = c(1, 2))

anova(durations4.lm)
coef(summary(durations4.lm))

##
durations5.lm <- lm(num_actDur ~ num_descFrameFam + num_idTask, data = 
                      durations)
summary(durations5.lm)

class(durations5.lm)
names(durations5.lm)
methods(class = class(durations5.lm))[1:9]

confint(durations5.lm, level = 0.95)

par(mar = c(4,4,2,2), mfrow = c(1, 2))
plot(durations5.lm, which = c(1, 2))

anova(durations5.lm)
coef(summary(durations5.lm))

est["num_estimate"] <- NA
est$num_estimate <- predict(durations.lm, newdata = est)
error <- merge(est, durations, by=c("id_req", "id_baseReq", "id_frame", "id_task",
                                    "desc_frameFam"))
error <- subset(error, select = c("id_project", "id_req", "id_baseReq", 
                                  "id_frame", "id_task", "desc_frameFam",
                                  "num_estimate", "num_estDur", 
                                  "num_actDur", "date_actStart", 
                                  "date_actFinish"))
error <- unique(error[,1:11])
error["num_logError"] <- NA
error$num_logError <- log(error$num_estimate)-log(error$num_actDur)
## -- Multivariate lm() "mlm" -----------
utils::example("SSD", echo=FALSE)
error["num_resSD"] <- NA
error$num_resSD <- sigma(durations.lm) # is the same as {but more efficient than}
error["num_oneSigma"] <- NA
error$num_oneSigma <- error$num_estimate+error$num_resSD



est2["num_estimate"] <- NA
est2$num_estimate <- predict(durations2.lm, newdata = est2)
error2 <- merge(est2, durations, by=c("id_req", "id_baseReq", "id_frame", "id_task",
                                      "desc_frameFam"))
error2 <- subset(error2, select = c("id_project", "id_req", "id_baseReq", "id_frame", 
                                    "id_task", "desc_frameFam", 
                                    "num_estimate", "num_estDur", 
                                    "num_actDur", "date_actStart", "date_actFinish"))
error2 <- unique(error2[,1:11])
error2["num_logError"] <- NA
error2$num_logError <- log(error2$num_estimate)-log(error2$num_actDur)
## -- Multivariate lm() "mlm" -----------
utils::example("SSD", echo=FALSE)
error2["num_resSD"] <- NA
error2$num_resSD <- sigma(durations.lm) # is the same as {but more efficient than}
error2["num_oneSigma"] <- NA
error2$num_oneSigma <- error2$num_estimate+error2$num_resSD

est3["num_estimate"] <- NA
est3$num_estimate <- predict(durations3.lm, newdata = est3)
error3 <- merge(est3, durations, by=c("id_req", "id_baseReq", "id_task"))
error3 <- subset(error3, select = c("id_project", "id_req", "id_baseReq", "id_task", 
                                    "num_estimate", 
                                    "num_estDur", "num_actDur", 
                                    "date_actStart", "date_actFinish"))
error3 <- unique(error3[,1:9])
error3["num_logError"] <- NA
error3$num_logError <- log(error3$num_estimate)-log(error3$num_actDur)
## -- Multivariate lm() "mlm" -----------
utils::example("SSD", echo=FALSE)
error3["num_resSD"] <- NA
error3$num_resSD <- sigma(durations.lm) # is the same as {but more efficient than}
error3["num_oneSigma"] <- NA
error3$num_oneSigma <- error3$num_estimate+error3$num_resSD

est4["num_estimate"] <- NA
est4$num_estimate <- predict(durations4.lm, newdata = est4)
error4 <- merge(est4, durations, by=c("id_frame", "id_task"))
error4 <- subset(error4, select = c("id_project", "id_frame", "id_task", 
                                    "num_estimate", 
                                    "num_estDur", "num_actDur", 
                                    "date_actStart", "date_actFinish"))
error4 <- unique(error4[,1:8])
error4["num_logError"] <- NA
error4$num_logError <- log(error4$num_estimate)-log(error4$num_actDur)
## -- Multivariate lm() "mlm" -----------
utils::example("SSD", echo=FALSE)
error4["num_resSD"] <- NA
error4$num_resSD <- sigma(durations.lm) # is the same as {but more efficient than}
error4["num_oneSigma"] <- NA
error4$num_oneSigma <- error4$num_estimate+error4$num_resSD

est5["num_estimate"] <- NA
est5$num_estimate <- predict(durations5.lm, newdata = est5)
error5 <- merge(est5, durations, by=c("id_frame", "id_task",
                                      "desc_frameFam"))
error5 <- subset(error5, select = c("id_project", "id_frame", "id_task", 
                                    "desc_frameFam", 
                                    "num_estimate", "num_estDur", 
                                    "num_actDur", "date_actStart", 
                                    "date_actFinish"))
error5 <- unique(error5[,1:9])
error5["num_logError"] <- NA
error5$num_logError <- log(error5$num_estimate)-log(error5$num_actDur)
## -- Multivariate lm() "mlm" -----------
utils::example("SSD", echo=FALSE)
error5["num_resSD"] <- NA
error5$num_resSD <- sigma(durations.lm) # is the same as {but more efficient than}
error5["num_oneSigma"] <- NA
error5$num_oneSigma <- error5$num_estimate+error5$num_resSD

write.csv(error, file = "FC1H_Duration_Estimate.csv")
write.csv(error2, file = "FC1H_Duration_Estimate2.csv")
write.csv(error3, file = "FC1H_Duration_Estimate3.csv")
write.csv(error4, file = "FC1H_Duration_Estimate4.csv")
write.csv(error5, file = "FC1H_Duration_Estimate5.csv")

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
                         stringsAsFactors = FALSE)
durations <- read.csv("durations.csv", header = TRUE, 
                      stringsAsFactors = FALSE)
durations <- merge(requirements, durations, by="id_project")
durations$date_actStart <- as.Date(durations$date_actStart, "%m/%d/%Y")
durations$date_actFinish <- as.Date(durations$date_actFinish, "%m/%d/%Y")
durations <- unique(durations[,1:35])

estimate1 <- read.csv("FC1H_Duration_Estimate.csv", header = TRUE, 
                      stringsAsFactors = FALSE)
estimate2 <- read.csv("FC1H_Duration_Estimate2.csv", header = TRUE, 
                      stringsAsFactors = FALSE)
estimate3 <- read.csv("FC1H_Duration_Estimate3.csv", header = TRUE, 
                      stringsAsFactors = FALSE)
estimate4 <- read.csv("FC1H_Duration_Estimate4.csv", header = TRUE, 
                      stringsAsFactors = FALSE)
estimate5 <- read.csv("FC1H_Duration_Estimate5.csv", header = TRUE, 
                      stringsAsFactors = FALSE)

#Distribution of transaction dates
tmp <- estimate1 %>% mutate(year_month = make_date(year=year(date_actStart),month=month(date_actStart)))
tmp %>% 
  group_by(year_month) %>% count() %>% 
  ggplot(aes(x=year_month,y=n)) +
  geom_bar(stat="identity", fill="red")+
  geom_vline(aes(xintercept=as.numeric(as.Date("2016-10-01"))),size=2)

tmp <- estimate1 %>% mutate(year_month = make_date(year=year(date_actFinish),month=month(date_actFinish)))
tmp %>% 
  group_by(year_month) %>% count() %>% 
  ggplot(aes(x=year_month,y=n)) +
  geom_bar(stat="identity", fill="red")+
  geom_vline(aes(xintercept=as.numeric(as.Date("2016-10-01"))),size=2)

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


##########################################################################
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
tasks1 <- read.csv("tasks1.csv", header = TRUE, stringsAsFactors = FALSE, na.strings = c(""," ","NA"))
tasks2 <- read.csv("tasks2mod.csv", header = TRUE, stringsAsFactors = FALSE, na.strings = c(""," ","NA"))
tasks <- merge(tasks1, tasks2, by=c("id_project"), allow.cartesian=TRUE)
tasks <- subset(tasks, select = c("id_project", "id_task", "id_baseTask", 
                                  "id_subtask", "num_countSubtask", 
                                  "num_revision", "num_countRevision", 
                                  "desc_baseTask", "desc_task", 
                                  "desc_hwType", "desc_PPE.PPG"))
tasks <- unique(tasks[,1:11])
requirements <- requirements[which(requirements$id_baseReq == 'FC1H'),]
hours <- read.csv("hours.csv", header = TRUE, 
                  stringsAsFactors = FALSE, na.strings = c(""," ","NA"))
hours <- merge(hours, tasks, by=c("id_project", "id_task"))
hourstotal <- read.csv("hourstotal.csv", header = TRUE, 
                       stringsAsFactors = FALSE, na.strings = c(""," ","NA"))

hours <- merge(hours, hourstotal, by=c("id_project", "id_task"))
hours <- merge(requirements, hours, by="id_project")
hours <- hours[which(hours$id_baseReq == 'FC1H'),]
hours <- unique(hours[,1:60])

req <- read.csv("req.csv", header = TRUE, 
                stringsAsFactors = FALSE)
frame <- read.csv("frame.csv", header = TRUE, 
                  stringsAsFactors = FALSE)
task <- read.csv("task.csv", header = TRUE, 
                 stringsAsFactors = FALSE)
task <- merge(task, tasks2, by="id_task")
req <- req[which(req$id_baseReq == 'FC1H'),]
task <- subset(task, select = c("num", "id_task", "id_baseTask"))
task <- unique(task[,1:3])
spec <- merge(req, frame, by="num")
spec <- unique(spec[,1:5])
spec <- merge(spec, task, by="num")
spec <- unique(spec[,1:7])

# Convert character variables to factors
hours$id_frame <- as.factor(hours$id_frame)
hours$id_baseReq <- as.factor(hours$id_baseReq)
hours$id_task <- as.factor(hours$id_task)
hours$desc_frameFam <- as.factor(hours$desc_frameFam)

# Create new columns for numeric representation
hours["num_idFrame"] <- NA
hours["num_idBaseReq"] <- NA
hours["num_idTask"] <- NA
hours["num_descFrameFam"] <- NA

# Convert factor variables to numeric
hours$num_idFrame <- as.numeric(hours$id_frame)
hours$num_idBaseReq <- as.numeric(hours$id_baseReq)
hours$num_idTask <- as.numeric(hours$id_task)
hours$num_descFrameFam <- as.numeric(hours$desc_frameFam)

summary(hours)

hours <- subset(hours, select = c("id_project", "id_req", "id_baseReq", "id_frame", 
                                  "id_task", "desc_frameFam", 
                                  "num_idBaseReq", "num_idFrame", "num_idTask", 
                                  "num_descFrameFam", "num_totalHours"))
hours <- unique(hours[,1:11])

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
hours.sub <- subset(hours, select = c("id_req", "id_baseReq", "id_frame", 
                                      "id_task", "num_idBaseReq", 
                                      "num_idFrame", "num_idTask"))

est <- merge(spec, hours.sub, by=c("id_req", "id_baseReq", "id_frame", "id_task"))
est <- unique(est[,1:10])
summary(est)

hours2.sub <- subset(hours, select = c("id_req", "id_baseReq", "desc_frameFam", 
                                       "id_task", "num_idBaseReq", 
                                       "num_descFrameFam", 
                                       "num_idTask"))
est2 <- merge(spec, hours2.sub, by=c("id_req", "id_baseReq", "desc_frameFam", 
                                     "id_task"))
est2 <- unique(est2[,1:10])
summary(est2)

hours3.sub <- subset(hours, select = c("id_req", "id_baseReq", "id_task", 
                                       "num_idBaseReq", "num_idTask"))
est3 <- merge(spec, hours3.sub, by=c("id_req", "id_baseReq", "id_task"))
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
hours.lm <- lm(num_totalHours ~ num_idBaseReq + num_idFrame + num_idTask, 
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
                                              num_idBaseReq), data = hours)
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
hours2.lm <- lm(num_totalHours ~ num_descFrameFam + num_idTask + num_idBaseReq, 
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
                                             + num_idBaseReq), 
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
hours3.lm <- lm(num_totalHours ~ num_idTask + num_idBaseReq, data = hours)
summary(hours3.lm)

class(hours3.lm)
names(hours3.lm)
methods(class = class(hours3.lm))[1:9]

confint(hours3.lm, level = 0.95)

par(mar = c(4,4,2,2), mfrow = c(1, 2))
plot(hours3.lm, which = c(1, 2))

anova(hours3.lm)
coef(summary(hours3.lm))

hours3.loglm <- lm(log(num_totalHours) ~ log(num_idTask + num_idBaseReq), 
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
error <- merge(est, hours, by=c("id_req", "id_baseReq", "id_frame", "id_task",
                                "desc_frameFam"))
error <- subset(error, select = c("id_project", "id_req", "id_baseReq", "id_frame", 
                                  "id_task", "desc_frameFam", 
                                  "num_estimate", 
                                  "num_totalHours"))
error <- unique(error[,1:8])
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
error2 <- merge(est2, hours, by=c("id_req", "id_baseReq", "id_frame", "id_task",
                                  "desc_frameFam"))
error2 <- subset(error2, select = c("id_project", "id_req", "id_baseReq", "id_frame", 
                                    "id_task", "desc_frameFam", 
                                    "num_estimate", "num_totalHours"))
error2 <- unique(error2[,1:8])
error2["num_logError"] <- NA
error2$num_logError <- log(error2$num_estimate)-log(error2$num_totalHours)
## -- Multivariate lm() "mlm" -----------
utils::example("SSD", echo=FALSE)
error2["num_resSD"] <- NA
error2$num_resSD <- sigma(hours.lm) # is the same as {but more efficient than}
error2["num_oneSigma"] <- NA
error2$num_oneSigma <- error2$num_estimate+error2$num_resSD

est3["num_estimate"] <- NA
est3$num_estimate <- predict(hours3.lm, newdata = est3)
est3["num_logEstimate"] <- NA
est3$num_logEstimate <- predict(hours3.loglm, newdata = est3)
error3 <- merge(est3, hours, by=c("id_req", "id_baseReq", "id_task"))
error3 <- subset(error3, select = c("id_project", "id_req", "id_baseReq", "id_task", "num_estimate", 
                                    "num_totalHours"))
error3 <- unique(error3[,1:6])
error3["num_logError"] <- NA
error3$num_logError <- log(error3$num_estimate)-log(error3$num_totalHours)
## -- Multivariate lm() "mlm" -----------
utils::example("SSD", echo=FALSE)
error3["num_resSD"] <- NA
error3$num_resSD <- sigma(hours.lm) # is the same as {but more efficient than}
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
error4$num_resSD <- sigma(hours.lm) # is the same as {but more efficient than}
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
error5$num_resSD <- sigma(hours.lm) # is the same as {but more efficient than}
error5["num_oneSigma"] <- NA
error5$num_oneSigma <- error5$num_estimate+error5$num_resSD

write.csv(error, file = "FC1H_Hours_Estimate.csv")
write.csv(error2, file = "FC1H_Hours_Estimate2.csv")
write.csv(error3, file = "FC1H_Hours_Estimate3.csv")
write.csv(error4, file = "FC1H_Hours_Estimate4.csv")
write.csv(error5, file = "FC1H_Hours_Estimate5.csv")
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
requirements <- requirements[which(requirements$id_baseReq == 'FC1H'),]
hourstotal <- read.csv("hourstotal.csv", header = TRUE, 
                       stringsAsFactors = FALSE, na.strings = c(""," ","NA"))

hours <- merge(hours, hourstotal, by=c("id_project", "id_task"))
hours <- merge(requirements, hours, by="id_project")

hours <- unique(hours[,1:51])

estimate1 <- read.csv("FC1H_Hours_Estimate.csv", header = TRUE, 
                      stringsAsFactors = FALSE)
estimate2 <- read.csv("FC1H_Hours_Estimate2.csv", header = TRUE, 
                      stringsAsFactors = FALSE)
estimate3 <- read.csv("FC1H_Hours_Estimate3.csv", header = TRUE, 
                      stringsAsFactors = FALSE)
estimate4 <- read.csv("FC1H_Hours_Estimate4.csv", header = TRUE, 
                      stringsAsFactors = FALSE)
estimate5 <- read.csv("FC1H_Hours_Estimate5.csv", header = TRUE, 
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

##########################################################################

library(caret)

# set the working directory
# setwd("~/Desktop/Rstatistics")
# setwd("C:/Users/dataclass/Desktop/Rstatistics")
getwd() # where am I?
list.files("dataSets") # files in the dataSets folder

setwd("C:/Users/502689880/Desktop/FMI_Estimates")
req <- read.csv("requirements.csv", header = TRUE, 
                stringsAsFactors = FALSE, na.strings = c(""," ","NA"))
lt <- read.csv("ltData.csv", header = TRUE, 
               stringsAsFactors = FALSE, na.strings = c(""," ","NA"))

req <- req[which(req$id_baseReq == 'FC1H'),]

lead <- merge(req, lt, by="id_tparent")
lead <- unique(lead[,1:36])

spec <- read.csv("parts.csv", header = TRUE, 
                 stringsAsFactors = FALSE, na.strings = c(""," ","NA"))

# Convert character variables to factors
lead$id_frame <- as.factor(lead$id_frame)
lead$id_baseReq <- as.factor(lead$id_baseReq)
lead$id_task <- as.factor(lead$id_task)
lead$desc_frameFam <- as.factor(lead$desc_frameFam)
lead$id_part <- as.factor(lead$id_part)

# Create new columns for numeric representation
lead["num_idFrame"] <- NA
lead["num_idBaseReq"] <- NA
lead["num_idTask"] <- NA
lead["num_descFrameFam"] <- NA
lead["num_idPart"] <- NA

# Convert factor variables to numeric
lead$num_idFrame <- as.numeric(lead$id_frame)
lead$num_idBaseReq <- as.numeric(lead$id_baseReq)
lead$num_idTask <- as.numeric(lead$id_task)
lead$num_descFrameFam <- as.numeric(lead$desc_frameFam)
lead$num_idPart <- as.numeric(lead$id_part)

summary(lead)

lead <- subset(lead, select = c("id_project", "id_req", "id_baseReq", "id_frame", "id_task", 
                                "id_part", "desc_frameFam", "num_idBaseReq", 
                                "num_idFrame", "num_idTask", "num_idPart", 
                                "num_descFrameFam", "num_totLead"))

lead <- unique(lead[,1:13])

library(ggplot2)  #install.packages('ggplot2') may need to be run if you don't have the package
qplot(data = lead, x = num_totLead) + ylab("Number of Projects")

# Create Estimate Tables Est=T/R/F.v, Est2=T/R/F, Est3=T/R, Est4=T/F.v, 
# Est5=T/F, Est6=T
lead.sub <- subset(lead, select = c("id_req", "id_baseReq", "id_frame", "id_task", 
                                    "id_part", "num_idBaseReq", "num_idFrame", 
                                    "num_idTask", "num_idPart"))

est <- merge(spec, lead.sub, by=c("id_req", "id_frame", "id_task", "id_part"))
est <- unique(est[,1:14])
summary(est)

# Fit Models
lead.lm <- lm(num_totLead ~ num_idFrame + num_idTask + num_idBaseReq + num_idPart, 
              data = lead)
summary(lead.lm)

class(lead.lm)
names(lead.lm)
methods(class = class(lead.lm))[1:9]

confint(lead.lm, level = 0.95)

par(mar = c(4,4,2,2), mfrow = c(1, 2))
plot(lead.lm, which = c(1, 2))

anova(lead.lm)
coef(summary(lead.lm))

est["num_estimate"] <- NA
est$num_estimate <- predict(lead.lm, newdata = est)
error <- merge(est, lead, by=c("id_req", "id_baseReq", "id_frame", "id_task",
                               "desc_frameFam", "id_part"))
error <- subset(error, select = c("id_project", "id_req", "id_baseReq", "id_frame", 
                                  "id_task", "desc_frameFam", "id_part", 
                                  "num_estimate", 
                                  "num_totLead"))
error <- unique(error[,1:9])
error["num_logError"] <- NA
error$num_logError <- log(error$num_estimate)-log(error$num_totLead)
## -- Multivariate lm() "mlm" -----------
utils::example("SSD", echo=FALSE)
error["num_resSD"] <- NA
error$num_resSD <- sigma(lead.lm) # is the same as {but more efficient than}
error["num_oneSigma"] <- NA
error$num_oneSigma <- error$num_estimate+error$num_resSD

write.csv(error, file = "FC1H_lead_Estimate.csv")
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
lead <- read.csv("ltData.csv", header = TRUE, 
                 stringsAsFactors = FALSE, na.strings = c(""," ","NA"))
requirements <- requirements[which(requirements$id_baseReq == 'FC1H'),]

lead <- merge(requirements, lead, by="id_tparent")
estimate1 <- read.csv("FC1H_lead_Estimate.csv", header = TRUE, 
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

