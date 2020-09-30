
###  CoMix data :
# Data from here: https://github.com/jarvisc1/comix_covid-19-first_wave

## Clean_participants
clean_participants <- readRDS("~/Desktop/comix_covid-19-first_wave-master/data/clean_participants.rds")
View(clean_participants)
table(clean_participants$n_contacts)
# I believe these include all types of contact. I'll extract number of 'other'
# contacts from the clean_contact dataset.

### Clean_contacts
clean_contacts <- readRDS("~/Desktop/comix_covid-19-first_wave-master/data/clean_contacts.rds")
View(clean_contacts)

# Restrict to the contacts classified as neither home, nor work, nor school, 
# I ASSSUME those are the 'other' contacts
c_all_other <- clean_contacts %>%
  filter(clean_contacts$cnt_home == "No"   &
           clean_contacts$cnt_work == "No" &
           clean_contacts$cnt_school == "No") %>%
  group_by(part_id) %>%
  count()
nrow(c_all_other) #515 participants had 808 'other' contacts
sum(c_all_other$n) 
table(c_all_other$n) #339*1+2*105+3*47+4*13+5*6+6*2+7+8+9 = 808

# Creating the distribution of gatherings (with the + 1)
# And including a gathering of 1 for everyone that did not report any 'other' gathering
# I THINK that's 1139 people, but I'm not certain yet, there are some confusing things in this data set
df1 <- as.data.frame(table(c_all_other$n))
df3 <- rbind(c(0,1139), df1) 
df3$Var1 = c(1:10)
df3

# Weighting it
df3
df3$wt <- df3$Freq / df3$Var1
df3
df3$prob <- df3$wt / sum(df3$wt)
df3
sum(df3$prob) #1
table(sample(df3$Var1, 1000, replace = T, prob = df3$prob))
#
# I'm thinking we might want to compare this distribution of gatherings 
# to the one we obtain under our most drastic restrictions.
#






### THE CONFUSION :
# This is some exploratory code, don't bother with it.
# I'm trying to figure out how many people had zero 'other' contacts...
# Problem : The number of contacts don't add up :'/

# Group contacts by index (part_id)
c_all <- clean_contacts %>% #All contacts (home, work, school and other)
  group_by(part_id) %>%
  count()
nrow(c_all) #1261 participants had 3849 total contacts of all types 
sum(c_all$n)
table(c_all$n)
table(clean_participants$n_contacts) # checking those are the same, + 95 who had zero contacts
sum(clean_participants$n_contacts)
nrow(clean_participants) #1356 total participants = 1261 who had contacts + 95 who had zero contacts

# Restrict to those that report home/work/school contacts
c_all_but_other <- clean_contacts %>%
  filter(clean_contacts$cnt_home == "Yes"   |
           clean_contacts$cnt_work == "Yes" |
           clean_contacts$cnt_school == "Yes") %>%
  group_by(part_id) %>%
  count()
nrow(c_all_but_other) #1123 participants had 3041 contacts at home-work-school. I ASSUME they did not have any 'other' contacts
sum(c_all_but_other$n) 
table(c_all_but_other$n)

sum(clean_contacts$cnt_home == "Yes") #2273
sum(clean_contacts$cnt_home == "No")  #1576
sum(clean_contacts$cnt_work == "Yes") #786
sum(clean_contacts$cnt_work == "No")  #3063
sum(clean_contacts$cnt_school == "Yes") #17
sum(clean_contacts$cnt_school == "No") # 3832
17 + 3832 #3849
2273 + 1576 #3849
786 + 3063 #3849
sum(clean_contacts$cnt_home == "No"   & # These are the 'other' I ASSUME
      clean_contacts$cnt_work == "No" &
      clean_contacts$cnt_school == "No") #808
sum(clean_contacts$cnt_home == "Yes"   |
      clean_contacts$cnt_work == "Yes" |
      clean_contacts$cnt_school == "Yes") #3041
808 + 3041 #3849
# So 808 'other' contacts over 3849 total number of contacts 
# and 3041 contacts were home or work or school

# So 1261 - 515 = 746 did not have any 'other' contacts 
# 1356 total participants
# 95 participants with no contacts
# 1123 - 49 = 1074 participants with work-home-school contacts 
# 515 participants with 'other' contacts
# So  participants had zero 'other' contacts.
# 95 + 1123 = 1218
# 1356 - 515 = 841

sum(clean_contacts$cnt_home == "Yes") 
sum(clean_contacts$cnt_home == "Yes" & clean_contacts$cnt_school == "Yes") #8
sum(clean_contacts$cnt_home == "Yes" & clean_contacts$cnt_work == "Yes") #21
sum(clean_contacts$cnt_school == "Yes" & clean_contacts$cnt_work == "Yes") #13
sum(clean_contacts$cnt_home == "Yes" & clean_contacts$cnt_school == "Yes" & clean_contacts$cnt_work == "Yes") #7
#8 + 21 + 13 + 7 = 49
sum(clean_contacts$cnt_home == "Yes" | clean_contacts$cnt_school == "Yes" | clean_contacts$cnt_work == "Yes") #3041
sum(clean_contacts$cnt_home == "No" & clean_contacts$cnt_school == "No" &  clean_contacts$cnt_work == "No" ) #808

#
my_clean_contacts <- clean_contacts
my_clean_contacts$count <- 0
my_clean_contacts$count <- ifelse(my_clean_contacts$cnt_home == "Yes", yes = my_clean_contacts$count + 1 , my_clean_contacts$count)
my_clean_contacts$count <- ifelse(my_clean_contacts$cnt_school == "Yes", yes = my_clean_contacts$count + 1, my_clean_contacts$count )
my_clean_contacts$count <- ifelse(my_clean_contacts$cnt_work == "Yes", yes = my_clean_contacts$count + 1, my_clean_contacts$count )
#my_clean_contacts
table(my_clean_contacts$count)

test <- my_clean_contacts %>%
  group_by(part_id) %>%
  count(count)
test
sum(test$n) #3849
sum(test$count) #1160
table(test$count) #515+1121+15+3  = 1654
                  #1121+15+3 = 1139

#

####