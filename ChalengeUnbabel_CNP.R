#####################################################################################
#                              Chalenge Unbabel
#####################################################################################
###1) Import packages and set parameters ####
#Defining Directory
setwd("C:\\Users\\canpinto\\Desktop\\exercise")

library(dplyr)
library(ggplot2)

###2) Import data ####
clients<-read.csv('clients.csv')
editors<-read.csv('editors.csv')
tasks<-read.csv('tasks.csv')
tickets<-read.csv('tickets.csv')

###3) Analyse and transform each dataset ####
###3.1) Clients ####
#Clients dataset includes information of the id, domain and category of 50 clients
#The category variable was eliminated since it is not relevant for the current analysis
#I assumed that the clients can only have a domain, which means that all tickets/tasks 
#of a certain client have the same domain 
clients$id<-as.character(clients$id)
clients$X<-NULL
clients$category<-NULL
clients$domain<-as.character(clients$domain)

y<-barplot<-barplot(table(clients$domain)/nrow(clients), col = 'deepskyblue3',
         width = 0.85,horiz = TRUE,yaxt="n",
         main = NULL, 
         ylab = "Frequency")

x<-0.25
text(x,y,names(table(clients$domain)))

table(clients$domain)/nrow(clients)
#48% of clients have travel as domain, followed by fintech (18%), ecommerce (16%), 
#health care (12%), sports (4%) and gamming (2%)

###3.2) Editors ####
#Editors dataset shows the skill according with the different domains for each of the 
#418 editors working with Unbabel. 
editors$id<-NULL
editors$X<-NULL
colMeans(x=editors)
summary(editors)
boxplot(editors, col = 10:15)
#The editors have on average the lowest skill in the domain travel and there isn't any 
#editor with the maximum skill (5) in this domain
#It is possible to conclude that Unbabel should either hire editors with a higher skill
#in the travel domain or invest in post editors' training in travel domain.

###3.3) Tasks ####
#Tasks dataset contains information regarding the number of words of each task and the 
#id of the ticket they belong. 83 153 tasks are considered. 
tasks$id<-NULL
tasks$X<-NULL
tasks$sequence_number<-NULL
tasks$ticket_id<-as.character(tasks$ticket_id)
hist(tasks$number_words,  breaks=5, col="cadetblue3", main = NULL, xlab = 'Number of Words' )
summary(tasks$number_words)
#Most tasks have about 40 words.

n_tasks_by_ticket<-data.frame(table(tasks$ticket_id))
n_tasks_by_ticket$Var1<-as.character(n_tasks_by_ticket$Var1)
names(n_tasks_by_ticket)<-c('ticket_id','n_tasks')
boxplot(n_tasks_by_ticket$n_tasks)
summary(n_tasks_by_ticket$n_tasks)
#The tickets usually are divided into 10 or less tasks. However, there are 
#some outliers, the maximum is a ticket with 384 tasks.

###3.4) Tickets ####
#Tickets dataset comprises information regarding 7 788 tickets. 
#The information includes the number of words of each ticket and the client id
#associated with the ticket. The information regarding language pair and the tone 
#were eliminated since they are not used in the current analysis
#Due to missing information regarding the language pairs in which an editor is
#proficient, I assume that editors are proficient in all language pairs considered
#in the dataset provided
tickets$X<-NULL
tickets$client_id<-NULL
tickets$client_id<-as.character(tickets$client_id.1)
tickets$client_id.1<-NULL
tickets$tone<-NULL
tickets$ticket_id<-as.character(tickets$id)
tickets$id<-NULL
tickets$language_pair<-NULL

boxplot(tickets$number_words)
summary(tickets$number_words)
#distrirbution of number of words by ticket
#Mean:408

unique(tickets$quality_score)
unique(tickets$price)
#the fields quality and price are not filled

###4) Tranform data and joind dataframes####
###4.1) Join clients, tickets and task dataframe ####
cliets_tickets<-merge(tickets, clients, by.x= 'client_id', by.y= 'id', all.y = TRUE)

cliets_tickets$number_words_by_ticket<-cliets_tickets$number_words
cliets_tickets$number_words<-NULL
tasks$number_words_by_task<-tasks$number_words
tasks$number_words<-NULL
#to avoid confusion due to colname repetition

clients_tickets_tasks<-merge(cliets_tickets,tasks, by = 'ticket_id')

###4.2) Randomly assigned editors to each task ####
set.seed(3)
editors$editor_id<-1:nrow(editors)
clients_tickets_tasks$editor_id<-sample(1:nrow(editors), nrow(clients_tickets_tasks), replace=TRUE)
clients_tickets_tasks_editor<-merge(clients_tickets_tasks,editors, by = 'editor_id', all.x=TRUE)
df_ctte<-clients_tickets_tasks_editor

#Assign the skill to the respective task
df_ctte$skill<-ifelse(df_ctte$domain == 'health_care', df_ctte$health_care,
                      ifelse(df_ctte$domain == 'fintech', df_ctte$fintech,
                             ifelse(df_ctte$domain == 'travel', df_ctte$travel,
                                    ifelse(df_ctte$domain == 'ecommerce', df_ctte$ecommerce,
                                           ifelse(df_ctte$domain == 'gamming', df_ctte$gamming,
                                                  df_ctte$sports)))))

df_ctte$travel<-NULL
df_ctte$health_care<-NULL
df_ctte$fintech<-NULL
df_ctte$ecommerce<-NULL
df_ctte$gamming<-NULL
df_ctte$sports<-NULL
###5) Current situation####
###5.1) Analyse current situation - obtain Q(t) and P(t)####
###5.1.1) Quality####
#Assumption: use the distribution of the skill of the different editors as a proxy for 
#quality probabilities P(A)

quality_prob<-as.data.frame(table(unlist(editors[,-7]))/length(unlist(editors[,-7])))
names(quality_prob)<-c('A','P_A')
quality_prob$A<-as.numeric(as.character(quality_prob$A))

#P(Aet)
#Assumption s=3 for P(Aet)=P(A)/A if Se(dt)<s
#B=1 for P(Aet)=P(A)*BA if Se(dt)>=s
quality_prob$lower_skill<-quality_prob$P_A/quality_prob$A
quality_prob$higher_skill<-quality_prob$P_A*quality_prob$A

#probability matrix P(Aet) normalized
paet<-as.data.frame(rbind(quality_prob$lower_skill/sum(quality_prob$lower_skill),
                   quality_prob$higher_skill/sum(quality_prob$higher_skill)))
names(paet)<-c(1,2,3,4,5)

#divide the subset
df_ctte_lower_skill<-subset(df_ctte,df_ctte$skill<3)
df_ctte_higher_skill<-subset(df_ctte,df_ctte$skill>=3)

#create quality vectors according w/ prob function
all_r_l<-c()
all_r_h<-c()
for (i in 1:5)
{
print(i)
r_l<-rep(i, times = as.integer(paet[1,i]*nrow(df_ctte_lower_skill)))
all_r_l<-c(r_l,all_r_l)
r_h<-rep(i, times = as.integer(paet[2,i]*nrow(df_ctte_higher_skill)))
all_r_h<-c(r_h,all_r_h)
}

#few observations were lost
df_ctte_lower_skill<-df_ctte_lower_skill[1:length(all_r_l),]
df_ctte_lower_skill$quality_score<-sample(all_r_l)

df_ctte_higher_skill<-df_ctte_higher_skill[1:length(all_r_h),]
df_ctte_higher_skill$quality_score<-sample(all_r_h)

#join dataframes w/quality score assigned
df_ctte<-rbind(df_ctte_higher_skill, df_ctte_lower_skill)

###5.1.2) Price####
#Remember assumption:the editors are proficient in all pairs of languages considered
#a=0.1, P(t)=a*Wt*Se(dt)
df_ctte$price<-df_ctte$number_words_by_task*df_ctte$skill*0.01

###5.2) Evaluate current situation####
###5.2.1) Quality####

ggplot(data = df_ctte[,c('domain','quality_score')]) +
  geom_bar(aes(x=as.factor(domain), fill=as.factor(quality_score)), position="fill")
#shows the quality distribution of tasks by domain

ggplot(data = df_ctte[,c('domain','skill')]) +
  geom_bar(aes(x=as.factor(domain), fill=as.factor(skill)), position="fill")
#shows the skill distribution of tasks by domain
#as already acessed travel is the domain where editors have less skill

ggplot(data = df_ctte[,c('skill','quality_score')]) +
  geom_bar(aes(x=as.factor(quality_score), fill=as.factor(skill)), position="fill")
#The quality score is positively related with the skill of the editor
#However a editor with skill 5 can produce a task with quality 1 and the other way around

ggplot(data = df_ctte[,c('skill','quality_score')]) +
  geom_bar(aes(x=as.factor(skill), fill=as.factor(quality_score)), position="fill")
#This graph returns the expected results. Since the probability of achieving a certain quality
#is the same for editors with skill 3,4 and 5, it seems a smart move to focus on the editors with
#skill 3 since are cheaper than 4 and 5 and produce the exact same results

table(df_ctte$quality_score)/nrow(df_ctte)
#15% tasks quality 5, 21% tasks quality 4, 14% tasks quality 3, 17% tasks quality 2, 
#33% tasks quality 1

#GOAL: increase quality!

###5.2.2) Price (costs to Unbabel)####

sum(df_ctte$price)
#Price: 79749
#Despite the fact that price is not the variable that we want to influence
#it is important to keep track of price variations than can arise from the aplication
#of a different method of task distribution

###5.2.3) Editors gap####

editors_freq<-as.data.frame(table(df_ctte$editor_id))
summary(editors_freq$Freq)
boxplot(editors_freq$Freq)

#Besides the quality, there is other problem identified: 
#there are editors that are not getting enough tasks

#GOAL:reduce the gap between editors!

###6) Solution####
###6.1) Propose a solution####
df_sol<-df_ctte
df_sol$skill<-NULL
df_sol$quality_score<-NULL
df_sol$price<-NULL
df_sol$editor_id<-NULL

#Establish a limit to the n_task a editor can get
max_tasks<-as.integer(nrow(df_sol)/nrow(editors)*1.05)

#Create an empty counter
count<-data.frame(1:nrow(editors), 0)
names(count)<-c('editor_id','counter')

#Skill priority
priority<-c(3,4,5,1,2)
#As explained above the prefered skill is 3 since a editor w/ skill gets the
#same results in terms of quality as the editor 4 and 5 and it is cheaper
#Since we are prioratizing quality and not price we set the skill priority as:
#3,4,5,1,2

#Create an empty dataframe
all_task_i<-data.frame()

#Run for each task i
for ( i in 1:nrow(df_sol))
{
  print(i)
  #Select the task i 
  task_i<-df_sol[i,]
  
  #Select all editors skills in the domain of the task i
  editor_list<-editors[,c(task_i$domain,'editor_id')]
  
  #Merge with the counter
  editor_list_1<-merge(editor_list, count, by ='editor_id', all.x = TRUE)
  
  #Eliminates the editors with more than the maximum no of tasks defined
  editor_list_1<-subset(editor_list_1, editor_list_1$counter<max_tasks)
  
  #Select the ones with the prefered skill according with the priority
  for (p in priority)  {
    if (nrow(subset(editor_list_1,editor_list_1[task_i$domain]== p))!=0)  {
      editor_list_pref<-subset(editor_list_1,editor_list_1[task_i$domain]==p)
      break
    }
    
  }

  #Select the editors with less tasks assigned
  min_count<-min(editor_list_pref$counter)
  subset_min_count<-subset(editor_list_pref,editor_list_pref$counter== min_count)
  
  #Select the editors that have the less number of prefered skills in the other domains
  #So if an editor receives 50 points for each domain it has 3
  #40 for each domain it has skill 4
  #30 for each domain it has skill 5
  #20 for each domani if has skill 1
  #10 for each domain it has skill 2
  #The editor with less point is selected
  other_domains<-editors[,!names(editors) %in% task_i$domain]
  other_domains<-as.data.frame(cbind(ifelse(other_domains[,1:5] ==3, 50,
                                            ifelse(other_domains[,1:5] ==4, 40,
                                                   ifelse(other_domains[,1:5] ==5, 30,
                                                          ifelse(other_domains[,1:5] ==1, 20,
                                                                 10)))),other_domains$editor_id))
  other_domains$editor_id<-other_domains$V6
  other_domains$n_3<-other_domains[,1]+other_domains[,2]+other_domains[,3]+other_domains[,4]+other_domains[,5]
  subset_min_count<-merge(subset_min_count,other_domains[,c('editor_id','n_3')], by = 'editor_id', all.x = TRUE)
  subset_min <-subset(subset_min_count,subset_min_count$n_3== min(subset_min_count$n_3))
  
  #Select 1 of the options (first of the list by default)
  row_min<-subset_min[1,]
  task_i$skill<-as.numeric(row_min[task_i$domain])
  task_i$editor_id<-row_min$editor_id
  
  #Update counter
  row_min$sum_count<-1
  row_min$counter<-NULL
  row_min$n_3<-NULL
  row_min[task_i$domain]<-NULL
  count<-merge(count,row_min, by = 'editor_id', all.x = TRUE)
  count$counter<-ifelse(is.na(count$sum_count), count$counter,count$counter +count$sum_count)
  count$sum_count<-NULL
  
  #Join task i to the final dataframe  
  all_task_i<-rbind(task_i,all_task_i)
  
}

###6.2) Evaluate solution####
df_sol<-all_task_i
table(df_sol$skill)/nrow(df_sol)
#56% tasks were solved with skill 3, 22% with skill 4 and 21% with skill 1

###6.2.1) Quality####
#To calculate the quality_score the same process is followd
#as well as the same assumptions
#divide the subset
df_sol_lower_skill<-subset(df_sol,df_sol$skill<3)
df_sol_higher_skill<-subset(df_sol,df_sol$skill>=3)

#create quality vectors according w/ prob function
all_r_l<-c()
all_r_h<-c()
for (i in 1:5)
{
  print(i)
  r_l<-rep(i, times = as.integer(paet[1,i]*nrow(df_sol_lower_skill)))
  all_r_l<-c(r_l,all_r_l)
  r_h<-rep(i, times = as.integer(paet[2,i]*nrow(df_sol_higher_skill)))
  all_r_h<-c(r_h,all_r_h)
}

#few observations were lost
df_sol_lower_skill<-df_sol_lower_skill[1:length(all_r_l),]
df_sol_lower_skill$quality_score<-sample(all_r_l)

df_sol_higher_skill<-df_sol_higher_skill[1:length(all_r_h),]
df_sol_higher_skill$quality_score<-sample(all_r_h)

#join dataframes w/quality score assigned
df_sol<-rbind(df_sol_higher_skill, df_sol_lower_skill)

#quality score distribution
table(df_sol$quality_score)/nrow(df_sol)

###6.2.2) Price####
df_sol$price<-df_sol$number_words_by_task*df_sol$skill*0.01
sum(df_sol$price)

###6.2.3)  Editors gap####
editors_freq_sol<-as.data.frame(table(df_sol$editor_id))
boxplot(editors_freq_sol$Freq)
summary(editors_freq_sol$Freq)

###7) Comparison####
###7.1) Quality####
boxplot(df_ctte$quality_score,df_sol$quality_score , names = c('Current Method','New Method'))
summary(df_sol$quality_score)
summary(df_ctte$quality_score)
###7.2) Price####
sum(df_sol$price)-sum(df_ctte$price)
(sum(df_sol$price)-sum(df_ctte$price))/sum(df_ctte$price)
###7.3) Editors gap####
boxplot( editors_freq$Freq, editors_freq_sol$Freq, names = c('Current Method','New Method'))

