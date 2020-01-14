#####################################################################################
#                              Chalenge Unbabel
#####################################################################################
#There have been some complaints lately from both customers and post editors.

# 1. Clients complain that quality is not stable and that they're having problems
# in trusting the service. This is critical and must be solved.

# 2. A large group of post editors claim that sometimes they're starving! In other
# words, they're not getting any tasks. This might cause them to leave and that
# is something that cannot happen.

###1) Import packages and set parameters ####
#Defining Directory
setwd("C:\\Users\\canpinto\\Desktop\\exercise")

library(dplyr)
library(ggplot2)

###2) Import data ####
#Information:
clients<-read.csv('clients.csv')
#list of Unbabel clients
#domain - all tickets of these clients (and respective tasks) are of a certain domain
#category - indicates the dimension of a company, it is not relevant for the analysis

editors<-read.csv('editors.csv')
#list of the editors working with Unbabel currently
#and the associated skill of the editor in the domains unbabel is working at

tasks<-read.csv('tasks.csv')
#tasks laso be solved in the next month
#number of words of each task
#id of the ticket each task belongs
#sequence it is not relvant for the analysis

tickets<-read.csv('tickets.csv')
#tickets to be solved in the next month
#client_id
#number of words of each ticket
#tone not relevant in the analysis
#language_pair
#quality and price are not filled


###3) Analyse and transform each dataset ####
###3.1) Clients ####
#head(clients)
clients$id<-as.character(clients$id)
clients$X<-NULL
clients$category<-NULL
clients$domain<-as.character(clients$domain)

barplot<-barplot(table(clients$domain)/nrow(clients), col = 'deepskyblue3',
         width = 0.85,
         main = "%Clients by domain", 
         ylab = "Frequency")

table(clients$domain)/nrow(clients)
#travel is the main domain (48% of the clients)

###3.2) Editors ####
#head(editors)
editors$id<-NULL
editors$X<-NULL
colMeans(x=editors)
summary(editors)
boxplot(editors, col = 3:8)
#lowest mean: travel
#48% of the clients have the domain travel.
#However the skill of the editors in the travel domain is the lowest
#It is possible to conclude that Unbabel should hire editors with a higher
#profiency in the travel domain

###3.3) Tasks ####
#head(tasks)
tasks$id<-NULL
tasks$X<-NULL
tasks$sequence_number<-NULL
tasks$ticket_id<-as.character(tasks$ticket_id)
hist(tasks$number_words,  breaks=5, col="cadetblue3")
summary(tasks$number_words)
#The majority of the task have about 40 words

n_tasks_by_ticket<-data.frame(table(tasks$ticket_id))
n_tasks_by_ticket$Var1<-as.character(n_tasks_by_ticket$Var1)
names(n_tasks_by_ticket)<-c('ticket_id','n_tasks')

boxplot(n_tasks_by_ticket$n_tasks, col="deepskyblue3")

summary(n_tasks_by_ticket$n_tasks)
#The majority of tickets divides into less than 10 tasks.
#However there is some outliers, max w/ 384 tasks

###3.4) Tickets ####
#head(tickets)
tickets$X<-NULL
tickets$client_id<-NULL
tickets$client_id<-as.character(tickets$client_id.1)
tickets$client_id.1<-NULL
tickets$tone<-NULL
tickets$ticket_id<-as.character(tickets$id)
tickets$id<-NULL
tickets$language_pair<-NULL
#tickets$language_pair<-as.character(tickets$language_pair)


boxplot(tickets$number_words)
summary(tickets$number_words)
#table(tickets$language_pair)/nrow(tickets)

unique(tickets$quality_score)
unique(tickets$price)
#the fields quality and price are not filled

###4) Tranform data and joind dataframes####
###4.1) Join clients, tickets and task dataframe ####
cliets_tickets<-merge(tickets, clients, by.x= 'client_id', by.y= 'id', all.y = TRUE)
#eliminate the tickets that doest have info about the cleint

cliets_tickets$number_words_by_ticket<-cliets_tickets$number_words
cliets_tickets$number_words<-NULL
tasks$number_words_by_task<-tasks$number_words
tasks$number_words<-NULL
#to avoid confusion due to colname repetition

clients_tickets_tasks<-merge(cliets_tickets,tasks, by = 'ticket_id')

###4.2) Randomly assigned editors to each task ####
editors$editor_id<-1:nrow(editors)
set.seed(3)
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
###5.1) Analyse current situation####
###5.1.1) Quality####
#Assumption: use the distribution of the skill of the different editors as a proxy for 
#quality probabilities A, P(A)
#Assumption: The editors are proficient in all pairs of languages considered
#df_ctte$language_pair<-NULL

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

#create quality vector according w/ prob function
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

ggplot(data = df_ctte[,c('domain','skill')]) +
  geom_bar(aes(x=as.factor(domain), fill=as.factor(skill)), position="fill")


ggplot(data = df_ctte[,c('skill','quality_score')]) +
  geom_bar(aes(x=as.factor(quality_score), fill=as.factor(skill)), position="fill")

ggplot(data = df_ctte[,c('skill','quality_score')]) +
  geom_bar(aes(x=as.factor(skill), fill=as.factor(quality_score)), position="fill")

#Explain preference for skill 3
table(df_ctte$quality_score)/nrow(df_ctte)

#goal: increase quality!

###5.2.2) Price (costs to Unbabel)####

sum(df_ctte$price)
#Price: 79749

###5.2.3) Editors starving####

editors_freq<-as.data.frame(table(df_ctte$editor_id))
summary(editors_freq$Freq)
boxplot(editors_freq$Freq)
#goal:reduce the gap between editors!

###6) Solution####
###6.1) Propose a solution####

df_sol<-df_ctte
df_sol$skill<-NULL
df_sol$quality_score<-NULL
df_sol$price<-NULL
df_sol$editor_id<-NULL

###6.1.1)First Attemp
#Set a counter
count<-data.frame(1:nrow(editors), 0)
names(count)<-c('editor_id','counter')

#Create an empty dataframe
all_task_i<-data.frame()

#Run for each task i
for ( i in 1:nrow(df_sol))
#for ( i in 1:10)
{
  print(i)
  #Select the task i 
  task_i<-df_sol[i,]
  
  #Select all editors skills in the domain of the task i
  editor_list<-editors[,c(task_i$domain,'editor_id')]
  
  #Select the ones with the prefered skill 3
  editor_list_pref<-subset(editor_list,editor_list[task_i$domain]==3)
  
  #Merge with the counter
  editor_list_pref<-merge(editor_list_pref, count, by ='editor_id', all.x = TRUE)
  
  #Select the editors with less tasks assigned
  min_count<-min(editor_list_pref$counter)
  subset_min_count<-subset(editor_list_pref,editor_list_pref$counter== min_count)
  
  #Select the editors that dont have the less number 3 as skill in the other tasks
  other_domains<-editors[,!names(editors) %in% task_i$domain]
  other_domains<-as.data.frame(cbind(ifelse(other_domains[,1:5] ==3, 1,0),other_domains$editor_id))
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

#Evaluate quality, price and gaps
df_sol_1<-all_task_i
#####
a<-as.data.frame(table(df_sol_1$editor_id))
boxplot(a$Freq)
summary(a$Freq)



all_r<-c()
for (i in 1:5)
{
  print(i)
  r<-rep(i, times = as.integer(paet[2,i]*nrow(df_sol_1)))
  all_r<-c(r,all_r)
}

df_sol_1<-df_sol_1[1:length(all_r),]
df_sol_1$quality_score<-sample(all_r)

table(df_sol_1$quality_score)/nrow(df_sol_1)
table(df_ctte$quality_score)/nrow(df_ctte)

###6.1.2)Second Attemp
#Establish a limit to the n_task a editor can get

max_tasks<-as.integer(nrow(df_sol)/nrow(editors)*1.1)

count<-data.frame(1:nrow(editors), 0)
names(count)<-c('editor_id','counter')
#skill priority explained
priority<-c(3,4,5,1,2)
#Create an empty dataframe
all_task_i<-data.frame()

#Run for each task i
for ( i in 1:nrow(df_sol))
 # for ( i in 1:10)
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
  
  #Select the editors that dont have the less number 3 as skill in the other tasks
  other_domains<-editors[,!names(editors) %in% task_i$domain]
  other_domains<-as.data.frame(cbind(ifelse(other_domains[,1:5] ==3, 1,0),other_domains$editor_id))
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

#Evaluate quality, price and gaps
df_sol_2<-all_task_i

a<-as.data.frame(table(df_sol_2$editor_id))
boxplot(a$Freq)
summary(a$Freq)

b<-as.data.frame(table(df_ctte$editor_id))
boxplot(b$Freq)
summary(b$Freq)


table(df_sol_2$skill)




all_r<-c()
for (i in 1:5)
{
  print(i)
  r<-rep(i, times = as.integer(paet[2,i]*nrow(df_sol_2)))
  all_r<-c(r,all_r)
}

df_sol_2<-df_sol_2[1:length(all_r),]
df_sol_2$quality_score<-sample(all_r)

table(df_sol_2$quality_score)/nrow(df_sol_2)
table(df_ctte$quality_score)/nrow(df_ctte)



#Assumption: each editor can have max 200 tasks
###6.2) Evaluate solution####
###6) Conclusion####
#comparation
