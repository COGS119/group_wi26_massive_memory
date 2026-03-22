library(here)
library(tidyverse)
library(lme4)
library(lmerTest)

processed_data_directory <- here("..","data","processed_data")
file_name <- "massive_memory"

#read experiment data
processed_data <- read_csv(here(processed_data_directory,paste0(file_name,"-processed-data.csv")))

#summarize memory accuracy by participant
subj_accuracy <- processed_data %>%
  group_by(participant_id,trial_phase,test_type) %>%
  summarize(
    mean_acc = mean(correct,na.rm=T),
    mean_rt = mean(rt[correct],na.rm=T)
  )

m <- lmer(mean_acc~test_type+(1|participant_id),data=subj_accuracy)
summary(m)

#overall accuracy
overall_accuracy <- subj_accuracy %>%
  filter(trial_phase=="test") %>%
  group_by(test_type) %>%
  summarize(
    N=n(),
    avg = mean(mean_acc),
    sd = sd(mean_acc),
    sem = sd / sqrt(N)
  )

#plot
ggplot(overall_accuracy,aes(test_type,avg,fill=test_type))+
  geom_bar(stat="identity",width=0.5)+
  geom_jitter(data=subj_accuracy,aes(y=mean_acc),width=0.01,alpha=0.5)+
  geom_errorbar(aes(ymin=avg-sem,ymax=avg+sem),width=0.05)+
  geom_hline(yintercept=0.5, linetype="dashed")+
  theme_bw()+
  theme(legend.position="none")+
  xlab("Test Type")+
  ylab("Proportion Correct")

