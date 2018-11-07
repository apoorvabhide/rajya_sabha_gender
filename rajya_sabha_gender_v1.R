#Gender Representation in the Rajya Sabha
rm(list =ls())

current_members <- read.csv('/home/ab/Downloads/Datasets for analysis/rajyasabha-questions/extra_info/Current_mem_Eng_nov_2017.csv',header = TRUE, stringsAsFactors = FALSE)
former_members <- read.csv('/home/ab/Downloads/Datasets for analysis/rajyasabha-questions/extra_info/former_mem_Eng_Nov_2017.csv',header = TRUE, stringsAsFactors = FALSE)

#How much is the gender representation in the current RS?
members_by_gender <- aggregate(Member.Name ~ Gender, current_members, function(x){length(unique(x))})
library(ggplot2)

#Waffle chart of gender representation
library(waffle)
gender_rs <- c(`Women`=28,`Men`=213)
gender_waffle <- waffle(gender_rs,rows = 12, colors = c('#386890','#808080'),
       title = 'Gender Representation in the Rajya Sabha, 2017', size = 2)
gender_waffle

library(stringr)
former_members_2<- as.data.frame(str_split_fixed(former_members$Term.Details, ",", 2))
former_members_3 <- as.data.frame(str_split_fixed(former_members_2$V2, ",", 2))
former_members$Term.Details <- gsub('^0/04','10/04',former_members$Term.Details)
former_members$Term.Details <- gsub('^0/06','10/06',former_members$Term.Details)

former_members$term1_start <- NA
former_members$term1_end <- NA
former_members$term2_start <- NA
former_members$term2_end <- NA
former_members$term3_start <- NA
former_members$term3_end <- NA
former_members$term4_start <- NA
former_members$term4_end <- NA
former_members$term5_start <- NA
former_members$term5_end <- NA
former_members$term6_start <- NA
former_members$term6_end <- NA

library(stringr)
#former_members$term1_start <- strsplit(former_members$Term.Details,"-",fixed = TRUE)[[1]][1]
former_members$term1_start <- gsub("-.*", "", former_members$Term.Details)
former_members$term1_end <- gsub(",.*","",sapply(str_split(former_members$Term.Details, "-",  n = 2), `[`, 2))
former_members$term2_start <- gsub("-.*", "",sapply(str_split(former_members$Term.Details, ",",  n = 2), `[`, 2))
former_members$term2_end <- gsub(".*- ","",sapply(str_split(former_members$Term.Details, ",",  n = 6), `[`, 2))
former_members$term2_end <- gsub(" ,","",former_members$term2_end)
former_members$term3_start <- gsub("-.*", "",sapply(str_split(former_members$Term.Details, ",",  n = 6), `[`, 3))
former_members$term3_end <- gsub(".*-", "",sapply(str_split(former_members$Term.Details, ",",  n = 6), `[`, 3))
former_members$term4_start <- gsub("-.*", "",sapply(str_split(former_members$Term.Details, ",",  n = 6), `[`, 4))
former_members$term4_end <- gsub(".*-", "",sapply(str_split(former_members$Term.Details, ",",  n = 6), `[`, 4))
former_members$term5_start <- gsub("-.*", "",sapply(str_split(former_members$Term.Details, ",",  n = 6), `[`, 5))
former_members$term5_end <- gsub(".*-", "",sapply(str_split(former_members$Term.Details, ",",  n = 6), `[`, 5))
former_members$term6_start <- gsub("-.*", "",sapply(str_split(former_members$Term.Details, ",",  n = 6), `[`, 6))
former_members$term6_end <- gsub(".*-", "",sapply(str_split(former_members$Term.Details, ",",  n = 6), `[`, 6))

former_members$term1_start <- trimws(former_members$term1_start)                                                  
former_members$term1_end <- trimws(former_members$term1_end)                                                  
former_members$term2_start <- trimws(former_members$term2_start)                                                  
former_members$term2_end <- trimws(former_members$term2_end)
former_members$term3_start <- trimws(former_members$term3_start)                                                  
former_members$term3_end <- trimws(former_members$term3_end)
former_members$term4_start <- trimws(former_members$term4_start)                                                  
former_members$term4_end <- trimws(former_members$term4_end)
former_members$term5_start <- trimws(former_members$term5_start)                                                  
former_members$term5_end <- trimws(former_members$term5_end)
former_members$term6_start <- trimws(former_members$term6_start)                                                  
former_members$term6_end <- trimws(former_members$term6_end)

former_members$term1_start <- str_pad(former_members$term1_start,width=10, side="left", pad="0")                                                  
former_members$term1_end <- str_pad(former_members$term1_end,width=10, side="left", pad="0")                                                  
former_members$term2_start <- str_pad(former_members$term2_start,width=10, side="left", pad="0")                                                  
former_members$term2_end <- str_pad(former_members$term2_end,width=10, side="left", pad="0")                                                  
former_members$term3_start <- str_pad(former_members$term3_start,width=10, side="left", pad="0")                                                  
former_members$term3_end <- str_pad(former_members$term3_end,width=10, side="left", pad="0")                                                  
former_members$term4_start <- str_pad(former_members$term4_start,width=10, side="left", pad="0")                                                  
former_members$term4_end <- str_pad(former_members$term4_end,width=10, side="left", pad="0")                                                  
former_members$term5_start <- str_pad(former_members$term5_start,width=10, side="left", pad="0")                                                  
former_members$term5_end <- str_pad(former_members$term5_end,width=10, side="left", pad="0")                                                  
former_members$term6_start <- str_pad(former_members$term6_start,width=10, side="left", pad="0")                                                  
former_members$term6_end <- str_pad(former_members$term6_end,width=10, side="left", pad="0")                                                  

former_members$term1_start <- gsub('000000000',NA,former_members$term1_start)
former_members$term1_end <- gsub('000000000',NA,former_members$term1_end)
former_members$term2_start <- gsub('000000000',NA,former_members$term2_start)
former_members$term2_end <- gsub('000000000',NA,former_members$term2_end)
former_members$term3_start <- gsub('000000000',NA,former_members$term3_start)
former_members$term3_end <- gsub('000000000',NA,former_members$term3_end)
former_members$term4_start <- gsub('000000000',NA,former_members$term4_start)
former_members$term4_end <- gsub('000000000',NA,former_members$term4_end)
former_members$term5_start <- gsub('000000000',NA,former_members$term5_start)
former_members$term5_end <- gsub('000000000',NA,former_members$term5_end)
former_members$term6_start <- gsub('000000000',NA,former_members$term6_start)
former_members$term6_end <- gsub('000000000',NA,former_members$term6_end)

#Some places have 00/,replace them with 01/
former_members$term1_start <- gsub('00/','01/',former_members$term1_start)
former_members$term1_end <- gsub('00/','01/',former_members$term1_end)
former_members$term2_start <- gsub('00/','01/',former_members$term2_start)
former_members$term2_end <- gsub('00/','01/',former_members$term2_end)
former_members$term3_start <- gsub('00/','01/',former_members$term3_start)
former_members$term3_end <- gsub('00/','01/',former_members$term3_end)
former_members$term4_start <- gsub('00/','01/',former_members$term4_start)
former_members$term4_end <- gsub('00/','01/',former_members$term4_end)
former_members$term5_start <- gsub('00/','01/',former_members$term5_start)
former_members$term5_end <- gsub('00/','01/',former_members$term5_end)
former_members$term6_start <- gsub('00/','01/',former_members$term6_start)
former_members$term6_end <- gsub('00/','01/',former_members$term6_end)

write.csv(former_members,'/home/ab/Downloads/Datasets for analysis/rajyasabha-questions/extra_info/former_members.csv',row.names = FALSE)
#Read them in as dates
former_members$term1_start <- as.Date(former_members$term1_start,'%d/%m/%Y')
former_members$term1_end <- as.Date(former_members$term1_end,'%d/%m/%Y')
former_members$term2_start <- as.Date(former_members$term2_start,'%d/%m/%Y')
former_members$term2_end <- as.Date(former_members$term2_end,'%d/%m/%Y')
former_members$term3_start <- as.Date(former_members$term3_start,'%d/%m/%Y')
former_members$term3_end <- as.Date(former_members$term3_end,'%d/%m/%Y')
former_members$term4_start <- as.Date(former_members$term4_start,'%d/%m/%Y')
former_members$term4_end <- as.Date(former_members$term4_end,'%d/%m/%Y')
former_members$term5_start <- as.Date(former_members$term5_start,'%d/%m/%Y')
former_members$term5_end <- as.Date(former_members$term5_end,'%d/%m/%Y')
former_members$term6_start <- as.Date(former_members$term6_start,'%d/%m/%Y')
former_members$term6_end <- as.Date(former_members$term6_end,'%d/%m/%Y')

gender_ratio_by_year <- data.frame(seq.Date(as.Date("1953-01-01"),as.Date("2018-01-01"),by = "year"))
colnames(gender_ratio_by_year) <- c('Year')
library(lubridate)
is.Date(gender_ratio_by_year$Year)
gender_ratio_by_year$male_reps <- 0
gender_ratio_by_year$female_reps <- 0

#Bring current members to the right format
#current_members$Term.Start.Date <- as_date(current_members$Term.Start.Date, guess_formats(current_members$Term.Start.Date,"dmy"))
current_members$Term.Start.Date <- gsub('/1/','/01/',current_members$Term.Start.Date)
current_members$Term.Start.Date <- gsub('/2/','/02/',current_members$Term.Start.Date)
current_members$Term.Start.Date <- gsub('/3/','/03/',current_members$Term.Start.Date)
current_members$Term.Start.Date <- gsub('/4/','/04/',current_members$Term.Start.Date)
current_members$Term.Start.Date <- gsub('/5/','/05/',current_members$Term.Start.Date)
current_members$Term.Start.Date <- gsub('/6/','/06/',current_members$Term.Start.Date)
current_members$Term.Start.Date <- gsub('/7/','/07/',current_members$Term.Start.Date)
current_members$Term.Start.Date <- gsub('/8/','/08/',current_members$Term.Start.Date)
current_members$Term.Start.Date <- gsub('/9/','/09/',current_members$Term.Start.Date)

current_members$Term.Start.Date <- str_pad(current_members$Term.Start.Date, width=10, side='left',pad='0')

current_members$Term.End.Date <- gsub('/1/','/01/',current_members$Term.End.Date)
current_members$Term.End.Date <- gsub('/2/','/02/',current_members$Term.End.Date)
current_members$Term.End.Date <- gsub('/3/','/03/',current_members$Term.End.Date)
current_members$Term.End.Date <- gsub('/4/','/04/',current_members$Term.End.Date)
current_members$Term.End.Date <- gsub('/5/','/05/',current_members$Term.End.Date)
current_members$Term.End.Date <- gsub('/6/','/06/',current_members$Term.End.Date)
current_members$Term.End.Date <- gsub('/7/','/07/',current_members$Term.End.Date)
current_members$Term.End.Date <- gsub('/8/','/08/',current_members$Term.End.Date)
current_members$Term.End.Date <- gsub('/9/','/09/',current_members$Term.End.Date)

current_members$Term.End.Date <- str_pad(current_members$Term.End.Date, width=10, side='left',pad='0')
current_members$Term.Start.Date <- as.Date(current_members$Term.Start.Date,'%d/%m/%Y')
current_members$Term.End.Date <- as.Date(current_members$Term.End.Date,'%d/%m/%Y')

library(dplyr)
for(i in 1:nrow(gender_ratio_by_year))
{
  for(k in 1:nrow(current_members))
  {
    if(current_members$Gender[k] == 'Male')
    {
      if(between(gender_ratio_by_year$Year[i],current_members$Term.Start.Date[k],current_members$Term.End.Date[k]))
      {
        gender_ratio_by_year$male_reps[i] <- gender_ratio_by_year$male_reps[i] + 1
      }
    }
      
    if(current_members$Gender[k] == 'Female')
    {
      if(between(gender_ratio_by_year$Year[i],current_members$Term.Start.Date[k],current_members$Term.End.Date[k]))
      {
        gender_ratio_by_year$female_reps[i] <- gender_ratio_by_year$female_reps[i] + 1
      }
    }
      
  }
  for(k in 1:nrow(former_members))
  {
    if(former_members$Gender[k] == 'Male')
    {
      if(!is.na(former_members$term1_start[k])&(between(gender_ratio_by_year$Year[i],former_members$term1_start[k],former_members$term1_end[k])))
        {
          gender_ratio_by_year$male_reps[i] <- gender_ratio_by_year$male_reps[i] + 1
        }
      else if(!is.na(former_members$term2_start[k])&(between(gender_ratio_by_year$Year[i],former_members$term2_start[k],former_members$term2_end[k])))
        {
          gender_ratio_by_year$male_reps[i] <- gender_ratio_by_year$male_reps[i] + 1
        }
      else if(!is.na(former_members$term3_start[k])&(between(gender_ratio_by_year$Year[i],former_members$term3_start[k],former_members$term3_end[k])))
        {
          gender_ratio_by_year$male_reps[i] <- gender_ratio_by_year$male_reps[i] + 1
        }
      else if(!is.na(former_members$term4_start[k])&(between(gender_ratio_by_year$Year[i],former_members$term4_start[k],former_members$term4_end[k])))
        {
          gender_ratio_by_year$male_reps[i] <- gender_ratio_by_year$male_reps[i] + 1
        }
      else if(!is.na(former_members$term5_start[k])&(between(gender_ratio_by_year$Year[i],former_members$term5_start[k],former_members$term5_end[k])))
        {
          gender_ratio_by_year$male_reps[i] <- gender_ratio_by_year$male_reps[i] + 1
        }
      else if(!is.na(former_members$term6_start[k])&(between(gender_ratio_by_year$Year[i],former_members$term6_start[k],former_members$term6_end[k])))
        {
          gender_ratio_by_year$male_reps[i] <- gender_ratio_by_year$male_reps[i] + 1
        }
    }
    
    if(former_members$Gender[k] == 'Female')
    {
      if(!is.na(former_members$term1_start[k])&(between(gender_ratio_by_year$Year[i],former_members$term1_start[k],former_members$term1_end[k])))
      {
        gender_ratio_by_year$female_reps[i] <- gender_ratio_by_year$female_reps[i] + 1
      }
      else if(!is.na(former_members$term2_start[k])&(between(gender_ratio_by_year$Year[i],former_members$term2_start[k],former_members$term2_end[k])))
      {
        gender_ratio_by_year$female_reps[i] <- gender_ratio_by_year$female_reps[i] + 1
      }
      else if(!is.na(former_members$term3_start[k])&(between(gender_ratio_by_year$Year[i],former_members$term3_start[k],former_members$term3_end[k])))
      {
        gender_ratio_by_year$female_reps[i] <- gender_ratio_by_year$female_reps[i] + 1
      }
      else if(!is.na(former_members$term4_start[k])&(between(gender_ratio_by_year$Year[i],former_members$term4_start[k],former_members$term4_end[k])))
      {
        gender_ratio_by_year$female_reps[i] <- gender_ratio_by_year$female_reps[i] + 1
      }
      else if(!is.na(former_members$term5_start[k])&(between(gender_ratio_by_year$Year[i],former_members$term5_start[k],former_members$term5_end[k])))
      {
        gender_ratio_by_year$female_reps[i] <- gender_ratio_by_year$female_reps[i] + 1
      }
      else if(!is.na(former_members$term6_start[k])&(between(gender_ratio_by_year$Year[i],former_members$term6_start[k],former_members$term6_end[k])))
      {
        gender_ratio_by_year$female_reps[i] <- gender_ratio_by_year$female_reps[i] + 1
      }
    }
  }
}

gender_ratio_by_year$gender_ratio <- gender_ratio_by_year$female_reps/(gender_ratio_by_year$male_reps+gender_ratio_by_year$female_reps)
#Plot gender ratio over time
#write.csv(gender_ratio_by_year, '/home/ab/Downloads/Datasets for analysis/rajyasabha-questions/extra_info/gender_ratio_by_year.csv',row.names=FALSE)
ysmax <- rep(1, nrow(gender_ratio_by_year))
library(ggplot2)
library(scales)
gender_ratio_plot <- ggplot(data=gender_ratio_by_year, aes(x=Year, y=gender_ratio))+
                    ggtitle('Gender Representation in the Rajya Sabha, 1953-2018')+
                  xlab('Year')+ylab('% of Women Representatives')+ylim(0,1)+xlim('1953-01-01','2018-01-01')+
                  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),plot.background = element_blank())+
                  geom_area(fill = "#386890")+geom_line(color ='white')+
                  geom_ribbon(aes(x=Year, ymin=gender_ratio, ymax=ysmax), data=gender_ratio_by_year, fill="#808080")+
                  geom_hline(yintercept=0.5, color = "white")+
                  scale_x_date(date_breaks = "10 years",date_labels = "%Y")+ scale_y_continuous(labels = percent)+
                  geom_text(x=as.Date('1974-01-01'),y=0.59,label="50-50 Gender Representation", size = 4, color = '#FFFFFF',family = 'URWPalladio',fontface='plain')

gender_ratio_plot 

#Read in attendance data
attendance <- read.csv('/home/ab/Downloads/Datasets for analysis/rajyasabha-questions/attendance_new.csv',header = TRUE, stringsAsFactors = FALSE)
colnames(attendance) <- c('s_no','division_seat_no','name','state','attendance','total_sittings','session')
attendance$attendance <- as.numeric(attendance$attendance)
attendance$att_rate <- attendance$attendance/attendance$total_sittings
attendance_by_member <- aggregate(att_rate ~ tolower(trimws(name)),attendance,mean)
colnames(attendance_by_member) <- c('name','attendance_rate')
attendance_by_member$name <- gsub("\\*","",attendance_by_member$name)
#There are duplicates, remove them by taking avg att on the name
attendance_by_member <- aggregate(attendance_rate ~ name, attendance_by_member,mean)

attendance_by_member$gender <- NA
attendance_by_member$gender[grep('shri ',attendance_by_member$name)] <- 'Male'
attendance_by_member$gender[grep('smt. ',attendance_by_member$name)] <- 'Female'
attendance_by_member$gender[grep('chaudhary ',attendance_by_member$name)] <- 'Male'
attendance_by_member$gender[grep('sardar ',attendance_by_member$name)] <- 'Male'
attendance_by_member$gender[grep('ms. ',attendance_by_member$name)] <- 'Female'
attendance_by_member$gender[grep('smt.',attendance_by_member$name)] <- 'Female'
attendance_by_member$gender[grep('miss ',attendance_by_member$name)] <- 'Female'
attendance_by_member$gender[grep('km. ',attendance_by_member$name)] <- 'Female'
attendance_by_member$gender[grep('kumari ',attendance_by_member$name)] <- 'Female'
attendance_by_member$gender[grep('mahant ',attendance_by_member$name)] <- 'Male'
attendance_by_member$gender[grep('mir ',attendance_by_member$name)] <- 'Male'
attendance_by_member$gender[grep('haji ',attendance_by_member$name)] <- 'Male'
attendance_by_member$gender[grep('ch. ',attendance_by_member$name)] <- 'Male'
attendance_by_member$gender[grep('dr.',attendance_by_member$name)] <- 'Male'
attendance_by_member$gender[grep('prof.',attendance_by_member$name)] <- 'Male'

#Some of the dr.'s were females
attendance_by_member$gender[grep('najma ',attendance_by_member$name)] <- 'Female'
attendance_by_member$gender[grep('prabha ',attendance_by_member$name)] <- 'Female'
attendance_by_member$gender[grep('pushpa ',attendance_by_member$name)] <- 'Female'
attendance_by_member$gender[grep(' fatma',attendance_by_member$name)] <- 'Female'
attendance_by_member$gender[grep('najma ',attendance_by_member$name)] <- 'Female'
attendance_by_member$gender[grep(' seema',attendance_by_member$name)] <- 'Female'
attendance_by_member$gender[grep('najma ',attendance_by_member$name)] <- 'Female'
attendance_by_member$gender[grep('sadho',attendance_by_member$name)] <- 'Female'
attendance_by_member$gender[grep('alka',attendance_by_member$name)] <- 'Female'

attendance_by_gender <- aggregate(attendance_rate ~ gender, attendance_by_member, mean)
attendance_by_gender_med <- aggregate(attendance_rate ~ gender, attendance_by_member, median)

#Now, import the questions
questions <- read.csv('/home/ab/Downloads/Datasets for analysis/rajyasabha-questions/rs_questions.csv',header = TRUE, stringsAsFactors = FALSE)

#Clean all the columns, convert to lower and trim
questions$answer_date <- as.Date(questions$answer_date)
questions$ministry <- tolower(trimws(questions$ministry))
questions$question_type <- tolower(trimws(questions$question_type))
questions$question_by <- tolower(trimws(questions$question_by))
questions$question_title <- tolower(trimws(questions$question_title))
#questions$answer_cleaned <- tolower(trimws(questions$answer))

questions$answer_cleaned <- gsub("[^[:alnum:][:blank:]?&/\\-]", "", questions$answer)
questions$answer_cleaned <- gsub("U00..", "", questions$answer_cleaned)
trim <- function (x) gsub("^\\s+|\\s+$", "", x)
questions$answer_cleaned <- tolower(trim(questions$answer_cleaned))

questions_gender <- merge(questions,attendance_by_member,by.x = 'question_by', by.y = 'name', all.x = TRUE)
questions_members <- aggregate(question ~ question_by + gender, questions_gender, function(x){length(unique(x))})

write.csv(questions_members,'/home/ab/Downloads/Datasets for analysis/rajyasabha-questions/questions_by_member.csv',row.names = FALSE)
questions_members <- read.csv('/home/ab/Downloads/Datasets for analysis/rajyasabha-questions/questions_by_member.csv',header = TRUE, stringsAsFactors = FALSE)
avg_questions_by_gender <- aggregate(question ~ gender, questions_members, median)
questions_by_gender <- aggregate(question ~ gender, questions_members, sum)

questions_members <- merge(questions_members, attendance_by_member, by.x = c('question_by','gender'),by.y=c('name','gender'))
questions_members$gender <- as.factor(questions_members$gender)

library(ggplot2)
plot_members <- ggplot(data = questions_members, mapping = aes(x = attendance_rate, y = question, color = gender)) +
  geom_point()
plot_members

cols <- c("#808080" = "steelgrey", "#386890" = "steelblue")

hist_q_male <- ggplot(data=questions_members[questions_members$gender =='Male',], aes(question)) + geom_density(fill = '#808080',alpha=0.5) +
  ggtitle('Questions asked in Rajya Sabha, 2009-2017') + xlab('Questions asked') + ylab('Number of MPs')
hist_q_male
hist_q_mp <- hist_q_male + geom_density(data=questions_members[questions_members$gender =='Female',], aes(question),fill = '#386890',alpha=0.5) +
  ggtitle('Questions asked in Rajya Sabha, 2009-2017') + xlab('Questions asked') + ylab('Number of MPs')+
  guides(colour=guide_legend(override.aes=list(colour = cols)))
hist_q_mp

#write.csv(questions_members,'/home/ab/Downloads/Datasets for analysis/rajyasabha-questions/extra_info/questions_by_member.csv',row.names = FALSE)

questions_density <- ggplot(data = questions_members, aes(x= question,fill = gender))+
                    geom_density(alpha = 0.7)+scale_fill_manual("Gender", values = c("#386890","#808080"))+
                    theme(plot.background = element_blank())+ scale_y_continuous(labels = scales::percent)+
                    ggtitle('Questions asked in Rajya Sabha, 2009-2017') + xlab('Questions asked') + ylab('Proportion of MPs')
questions_density

#Distribution of ministries in questions asked by men
m_questions_by_ministry <- aggregate(question ~ ministry, questions_gender[questions_gender$gender == 'Male',], function(x){length(unique(x))})
m_questions_by_ministry$total_q <- sum(m_questions_by_ministry$question)
m_questions_by_ministry$qpc <- m_questions_by_ministry$question/m_questions_by_ministry$total_q
barplot(m_questions_by_ministry$qpc)
f_questions_by_ministry <- aggregate(question ~ ministry, questions_gender[questions_gender$gender == 'Female',], function(x){length(unique(x))})
f_questions_by_ministry$total_q <- sum(f_questions_by_ministry$question)
f_questions_by_ministry$qpc <- f_questions_by_ministry$question/f_questions_by_ministry$total_q
barplot(f_questions_by_ministry$qpc)

m_questions_by_ministry$gender <- NULL
f_questions_by_ministry$gender <- NULL
questions_by_ministry <- merge(m_questions_by_ministry, f_questions_by_ministry, by = 'ministry')
colnames(questions_by_ministry) <- c('ministry','m_questions','m_total_questions','m_pc','f_questions','f_total_questions','f_pc')
plot(x=questions_by_ministry$m_pc, y=questions_by_ministry$f_pc)


#Now the hardest part: Topic Modeling
library(tm)
stop_words <- stopwords("SMART")

#Trying according to lda tutorial
doc.list <- strsplit(questions_gender$question_title[questions_gender$gender == 'Female'], "[[:space:]]+")

# compute the table of terms:
term.table <- table(unlist(doc.list))
term.table <- sort(term.table, decreasing = TRUE)

# remove terms that are stop words or occur fewer than 5 times:
del <- names(term.table) %in% stop_words | names(term.table) == "" | names(term.table) == "." | term.table < 5
term.table <- term.table[!del]
vocab <- names(term.table)

# now put the documents into the format required by the lda package:
get.terms <- function(x) {
  index <- match(x, vocab)
  index <- index[!is.na(index)]
  rbind(as.integer(index - 1), as.integer(rep(1, length(index))))
}
documents <- lapply(doc.list, get.terms)

D <- length(documents)  # number of documents (14752)
W <- length(vocab)  # number of terms in the vocab (6339)
doc.length <- sapply(documents, function(x) sum(x[2, ]))  # number of tokens per document [312, 288, 170, 436, 291, ...]
N <- sum(doc.length)  # total number of tokens in the data (264953)
term.frequency <- as.integer(term.table)  # frequencies of terms in the corpus [8939, 5544, 2411, 2410, 2143, ...]


#Now train the model to see topics of the female questioners
K <- 20
G <- 5000
alpha <- 0.02
eta <- 0.02

# Fit the model:
library(lda)
set.seed(357)
t1 <- Sys.time()
fit <- lda.collapsed.gibbs.sampler(documents = documents, K = K, vocab = vocab, 
                                   num.iterations = G, alpha = alpha, 
                                   eta = eta, initial = NULL, burnin = 0,
                                   compute.log.likelihood = TRUE)
t2 <- Sys.time()
t2 - t1  # about 8 minutes on laptop

theta <- t(apply(fit$document_sums + alpha, 2, function(x) x/sum(x)))
phi <- t(apply(t(fit$topics) + eta, 2, function(x) x/sum(x)))

f_topics <- list(phi = phi,
                     theta = theta,
                     doc.length = doc.length,
                     vocab = vocab,
                     term.frequency = term.frequency)

library(LDAvis)

# stop_words <- c(stop_words, "")
# doc.length = colSums( as.matrix(dtm) > 0 )[!empty.docs]
# create the JSON object to feed the visualization:

json <- createJSON(phi = f_topics$phi, 
                   theta = f_topics$theta, 
                   doc.length = f_topics$doc.length, 
                   vocab = f_topics$vocab, 
                   term.frequency = f_topics$term.frequency)
write(exportJson, "test.json")
library(servr)
serVis(json, out.dir = '/home/ab/Downloads/Datasets for analysis/rajyasabha-questions/project_files/vis/vis.html', open.browser = FALSE)

#Repeat the same thing for men
doc.list.m <- strsplit(questions_gender$question_title[questions_gender$gender == 'Male'], "[[:space:]]+")

# compute the table of terms:
term.table.m <- table(unlist(doc.list.m))
term.table.m <- sort(term.table.m, decreasing = TRUE)

# remove terms that are stop words or occur fewer than 5 times:
del.m <- names(term.table.m) %in% stop_words | names(term.table.m) == "" | names(term.table.m) == "." | term.table.m < 5
term.table.m <- term.table.m[!del.m]
vocab.m <- names(term.table.m)

# now put the documents into the format required by the lda package:
get.terms <- function(x) {
  index <- match(x, vocab)
  index <- index[!is.na(index)]
  rbind(as.integer(index - 1), as.integer(rep(1, length(index))))
}
documents.m <- lapply(doc.list.m, get.terms)

D.m <- length(documents.m)  # number of documents.m (14752)
W.m <- length(vocab.m)  # number of terms in the vocab (6339)
doc.length.m <- sapply(documents.m, function(x) sum(x[2, ]))  # number of tokens per document [312, 288, 170, 436, 291, ...]
N.m <- sum(doc.length.m)  # total number of tokens in the data (264953)
term.frequency.m <- as.integer(term.table.m)  # frequencies of terms in the corpus [8939, 5544, 2411, 2410, 2143, ...]

#Now train the model to see topics of the female questioners
K <- 20
G <- 5000
alpha <- 0.02
eta <- 0.02

# Fit the model:
library(lda)
set.seed(357)
t3 <- Sys.time()
fit.m <- lda.collapsed.gibbs.sampler(documents = documents.m, K = K, vocab = vocab.m, 
                                   num.iterations = G, alpha = alpha, 
                                   eta = eta, initial = NULL, burnin = 0,
                                   compute.log.likelihood = TRUE)
t4 <- Sys.time()
t4 - t3  # about 24 minutes on laptop

theta <- t(apply(fit.m$document_sums + alpha, 2, function(x) x/sum(x)))
phi <- t(apply(t(fit.m$topics) + eta, 2, function(x) x/sum(x)))

f_topics.m <- list(phi = phi,
                 theta = theta,
                 doc.length = doc.length.m,
                 vocab = vocab.m,
                 term.frequency = term.frequency.m)

library(LDAvis)

# stop_words <- c(stop_words, "")
# doc.length = colSums( as.matrix(dtm) > 0 )[!empty.docs]
# create the JSON object to feed the visualization:

json.m <- createJSON(phi = f_topics.m$phi, 
                   theta = f_topics.m$theta, 
                   doc.length = f_topics.m$doc.length, 
                   vocab = f_topics.m$vocab, 
                   term.frequency = f_topics.m$term.frequency)
library(servr)
serVis(json.m, out.dir = 'vis', open.browser = TRUE)

write.csv(questions_gender,'/home/ab/Downloads/Datasets for analysis/rajyasabha-questions/extra_info/questions_gender.csv',row.names = FALSE)
