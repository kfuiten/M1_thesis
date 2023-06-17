library(dplyr)
library(ggplot2)

#Data used: all_data.csv

all_data <- read.csv("all_data.csv")

#Remove outliers and add trial ID 
all_data$trialId  <-  factor( (all_data$block_num+1) * 10 +all_data$trial_num)
unique(all_data$participant)
to_remove <- c(14, 20, 28)
all_data <- all_data[!all_data$participant %in% to_remove, ]
probe_data <- all_data[!all_data$participant %in% to_remove, ]
probe_data <- aggregate(trial_score ~ participant + block_settings + block_num + trial_num + trialId + probe1_answ + probe2_answ + asrs_A_raw_score, data = all_data, FUN = mean)
probe_data <- arrange(probe_data, participant, block_num, trial_num)
probe_data <- probe_data[probe_data$probe1_answ != 0, ]
head(probe_data)

#Performance predicted by ASRS score 
mean_trial_score <- evalq(aggregate(trial_score, list(participant=participant), mean), all_data)
all_data <- merge(all_data, mean_trial_score, by = "participant", all.x = FALSE)
all_data$mean_trial_score <- all_data$x
all_data <- select(all_data, -c("x"))

perf_by_asrs <- all_data %>%
  group_by(participant) %>%
  summarise(
    mean_trial_score,
    asrs_A_raw_score
  )
perf_by_asrs <- unique(perf_by_asrs)
perf_by_asrs <- perf_by_asrs[!perf_by_asrs$participant %in% to_remove, ]
perf_lm <- lm(mean_trial_score ~ asrs_A_raw_score, data=perf_by_asrs)
summary(perf_lm)

scatter <- ggplot(data=perf_by_asrs, aes(x=asrs_A_raw_score, y=mean_trial_score)) + labs(x="ASRS Score", y="Mean Trial Score") + geom_jitter(size=3, aes(color=asrs_A_raw_score))
scatter + scale_color_gradient(low='blue', high="red", name='ASRS Score')

#Spearman's correlation test on trial score and on-task rating
probe_data$probe1_answ <- as.numeric(probe_data$probe1_answ)
cor.test(probe_data$probe1_answ, probe_data$trial_score, method = "spearman", exact=FALSE)

#Trial score predicted by on-task rating
probe_data$probe1_answ <- as.numeric(probe_data$probe1_answ)
probe_data$participant <- as.factor(probe_data$participant)
l <- lmer(trial_score ~ probe1_answ + (1| participant), data=probe_data)
summary(l)

#Scatterplot of trial score predicted by on-task rating 
ggplot(data=probe_data , aes(x=probe1_answ,trial_score)) + geom_jitter()

#Spearman's correlation test on-task rating and ASRS score
ontask <- evalq(aggregate(list(probe1_answ=probe1_answ), list(participant=participant, asrs_A_raw_score=asrs_A_raw_score), mean), probe_data)
ontask <- ontask[order(ontask$participant),]
ontask$probe1_answ <- as.numeric(ontask$probe1_answ)
cor.test(ontask$probe1_answ, ontask$asrs_A_raw_score, method = "spearman", exact=FALSE)

#Average on-task rating predicted by ASRS score 
l <-  lm(probe1_answ ~ asrs_A_raw_score, data=ontask)
summary(l)

#Trial accuracy predicted by duration * on-task rating
probe_data$block_settings <- as.factor(probe_data$block_settings)
l <- lmer(trial_score ~ block_settings * probe1_answ + (1|participant), data=probe_data)
summary(l)

#Trial score predicted by duration * ADHD-level (taken from Sackur's analysis)
data <- all_data
data$lAcc  <-  data$letters_correct
data$eqAcc  <- data$equations_correct
data$dur  <- data$block_settings
data$suj  <- data$participant
data$asrsAR  <- data$asrs_A_raw_score
data$bNum  <-  data$block_num
data$tNum  <- data$trial_num
data$eqRt  <-  data$truth_react_time
data$sAsrs  <- scale(data$asrsAR)
data$trialId  <-  factor( (data$bNum+1) * 10 +data$tNum)
data$adhd  <- factor(data$asrsAR > median(data$asrsAR))
data$sp  <- scale(rep(1:5, nrow(data)/5))
data$logEqRt  <-  scale(log(data$eqRt))
data$focus  <- ifelse(data$probe1_answ==0, NA, data$probe1_answ)
data$sFocus  <- scale(data$focus)
data$suj  <- factor(data$suj)
data$dur  <- factor(data$dur)
e  <- evalq(aggregate(list(tAcc=lAcc), list(suj=suj, dur=dur, asrsAR=asrsAR, bNum=bNum, tNum=tNum, trialId=trialId), mean), data)
e <- e[order(e$suj, e$trialId,e$bNum, e$tNum),]

l <- lmer(tAcc ~ dur * asrsAR + (1|suj),  data=e)
summary(l) #Significant with outlier #14, not significant without outlier

#Analysis of local durations#####################################################
#Calculate quartiles for each participant 
quartile <- all_data %>%
  group_by(participant) %>%
  summarize(
    min = quantile(truth_react_time, probs = 0.0, na.rm = TRUE),
    q1 = quantile(truth_react_time, probs = 0.25, na.rm = TRUE),
    med = quantile(truth_react_time, probs = 0.5, na.rm = TRUE),
    q3 = quantile(truth_react_time, probs = 0.75, na.rm = TRUE), 
    max = quantile(truth_react_time, probs = 1.0, na.rm = TRUE)
  )
head(quartile)

all_data <- merge(all_data, quartile, by = "participant", all.x = TRUE)

#quantile(all_data$truth_react_time, na.rm = TRUE)

all_data <- all_data %>%
  mutate(
    quartile = case_when(
      truth_react_time < q1 ~ 1,
      truth_react_time >= q1 & truth_react_time <= med ~ 2,
      truth_react_time > med & truth_react_time <= q3 ~ 3,
      truth_react_time > q3 ~ 4
    )
  )

summary(all_data$quartile)


#Calculate mean scores by quartile within participants
quartile_means <- all_data %>%
  group_by(participant) %>%
  summarize(
    mean_q1 = mean(trial_score[quartile == 1], na.rm = TRUE),
    mean_q2 = mean(trial_score[quartile == 2], na.rm = TRUE),
    mean_q3 = mean(trial_score[quartile == 3], na.rm = TRUE), 
    mean_q4 = mean(trial_score[quartile == 4], na.rm = TRUE)
  )

head(quartile_means)

all_data <- merge(all_data, quartile_means, by = "participant", all.x = TRUE)

mean(all_data$mean_q1) 
mean(all_data$mean_q2) 
mean(all_data$mean_q3) 
mean(all_data$mean_q4) 

#ADD BOXPLOT 
quart <- evalq(aggregate(letters_correct, list(quartile=quartile, participant=participant), mean), all_data)
boxplot(quart$x ~ quart$quartile, ylab='Letter Score Average Accuracy', xlab="Response Time Quartile")
quart$quartile <- as.factor(quart$quartile) 
ggplot(quart, aes(x=quartile, y=x)) + geom_boxplot() + labs(x="Response Time Quartile", y='Letter Score Average Accuracy') 

#Multiple regression equation examining effect of quartile on performance (binary) at the letter level
class(all_data$quartile)
all_data$quartile <- as.factor(all_data$quartile)
q <- glmer(letters_correct ~ quartile + (1|participant) + (1|participant:trial_num), family ="binomial", data=all_data)
summary(q) #Strong effect of quartile on performance for Q1, Q2 and Q4!

#Analysis of serial position#####################################################
#Add serial position to data frame 
all_data <- all_data %>% 
  group_by(participant, block_num, trial_num) %>%
  mutate(serial_position = row_number())

#Calculate mean score by serial position within participants 
serial_means <- all_data %>%
  group_by(participant) %>%
  summarize(
    mean_pos1 = mean(letters_correct[serial_position == 1], na.rm = TRUE),
    mean_pos2 = mean(letters_correct[serial_position == 2], na.rm = TRUE),
    mean_pos3 = mean(letters_correct[serial_position == 3], na.rm = TRUE),
    mean_pos4 = mean(letters_correct[serial_position == 4], na.rm = TRUE),
    mean_pos5 = mean(letters_correct[serial_position == 5], na.rm = TRUE)
  )
head(serial_means)
all_data <- merge(all_data, serial_means, by = "participant", all.x = FALSE)

#Make box plot
serial_means <- serial_means[,-1]
colnames(serial_means) <- c("1","2","3","4","5")
boxplot(serial_means, xlab = "Serial Position", ylab = "Letter Score Average Accuracy", names = colnames(serial_means))

#Log of equation RT WITHIN participants 
logs <- data %>% 
  group_by(suj) %>% 
  summarise(
    bNum,
    tNum, 
    trialId,
    log(eqRt)
  )
colnames(logs) <- c('suj', 'bNum', 'tNum', 'trialId', 'withinsuj_eqRt')
logs$withinsuj_eqRt <- scale(logs$withinsuj_eqRt)
data <- merge(data, logs, by=c("suj"="suj", "bNum"='bNum', "tNum"="tNum", "trialId"="trialId"), all.y = FALSE, all.x = FALSE)
head(data)

l0 <- glmer(lAcc ~ withinsuj_eqRt * sp  + (1|suj) + (1|suj:trialId), family=binomial, data=data)
summary(l0)

l <- glmer(lAcc ~ withinsuj_eqRt + (1|suj) + (1|suj:trialId), family=binomial, data=data)
summary(l)

#Letter accuracy predicted by response time * serial position 
l0 <- glmer(lAcc ~ logEqRt * sp  + (1|suj) + (1|suj:trialId), family=binomial, data=data)
summary(l0)



