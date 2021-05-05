
##########################################
### Distribution of number of contacts ###
##########################################


### Load libraries
library(tidyverse)
library(magrittr)


### 1) BBC Pandemic

#Load data of number of contacts from BBC Pandemic   
BBCpandemic <- read_csv("Desktop/Contact_data/Data/BBC_Pandemic/contact_distributions_o18.csv")
names(BBCpandemic) <- c("BBC_home", "BBC_work", "BBC_other")
BBCpandemic
#head(BBCpandemic)

DFcontacts <- merge(
  merge(BBCpandemic %>%
        count(BBC_home) %>%
        set_colnames(c("n", "BBC_home")),
        BBCpandemic %>%
        count(BBC_work)  %>%
        set_colnames(c("n", "BBC_work")),
      by = "n",
      all = TRUE),
  BBCpandemic %>%
    count(BBC_other) %>%
    set_colnames(c("n", "BBC_other")),
  by = "n",
  all = TRUE)

# Add BBC_total
DFcontacts <- DFcontacts %>%
  rowwise() %>% 
  mutate(BBC_total = sum(BBC_home, BBC_work, BBC_other, na.rm = T))

#
DFcontacts
head(DFcontacts)
#

#

### 2) COMIX study
# From : https://bmcmedicine.biomedcentral.com/articles/10.1186/s12916-020-01597-8#availability-of-data-and-materials
# Github : https://github.com/jarvisc1/comix_covid-19-first_wave

# This dataset doesn't seem to have interesting info for us.
#COMIX_households <- readRDS("~/Desktop/Contact_data/COMIX/clean_households.rds")
#View(COMIX_households)

# This dataset has the varibale n_contacts,  which I assume corresponds to all contacts (?)
# TODO : Check that 
COMIX_participants <- readRDS("Desktop/Contact_data/Data/COMIX/clean_participants.rds")
#View(COMIX_participants)
colnames(COMIX_participants)
#unique(COMIX_participants$country) # Only UK
table(COMIX_participants$n_contacts)

DFcontacts <- merge(DFcontacts,
      COMIX_participants %>%
        count(n_contacts) %>%
        set_colnames(c("n", "COMIX_n_contacts")),
      by = "n",
      all = TRUE)

#
head(DFcontacts)
#


# This dataset has number of contacts per context !
COMIX_contacts <- readRDS("~/Desktop/Contact_data/Data/COMIX/clean_contacts.rds")
COMIX_contacts <- COMIX_contacts[with(COMIX_contacts, order(part_id, cont_id)),]
COMIX_contacts <- COMIX_contacts %>% select(-c("cnt_gender", "cnt_age_est_min", "cnt_age_est_max"))
COMIX_contacts
#View(COMIX_contacts)
head(COMIX_contacts)

# Recode to get number of 'other' contacts
# I assume 808 contacts that are not home, not work and not school are other.
nrow(COMIX_contacts %>% filter(cnt_home == "No",
                               cnt_work == "No",
                               cnt_school == "No"))
COMIX_contacts <- COMIX_contacts %>%
  mutate(cnt_other = case_when((cnt_home == "No" & cnt_work == "No" & cnt_school == "No") ~ "Yes",
                               TRUE ~ "No" ))

# 35 with double or triple coding
# TODO : How to deal with that ?
nrow(COMIX_contacts)
sum(COMIX_contacts$cnt_home == "Yes") + sum(COMIX_contacts$cnt_work == "Yes") + sum(COMIX_contacts$cnt_school == "Yes") + sum(COMIX_contacts$cnt_other == "Yes")
# sum(COMIX_contacts$cnt_home == "Yes" & COMIX_contacts$cnt_work == "Yes")
# sum(COMIX_contacts$cnt_home == "Yes" & COMIX_contacts$cnt_school == "Yes")
# sum(COMIX_contacts$cnt_home == "Yes" & COMIX_contacts$cnt_other == "Yes")
# sum(COMIX_contacts$cnt_work == "Yes" & COMIX_contacts$cnt_school == "Yes")
# sum(COMIX_contacts$cnt_work == "Yes" & COMIX_contacts$cnt_other == "Yes")
# sum(COMIX_contacts$cnt_school == "Yes" & COMIX_contacts$cnt_other == "Yes")
# # 42 with double coding
# sum(COMIX_contacts$cnt_home == "Yes" & COMIX_contacts$cnt_work == "Yes" & COMIX_contacts$cnt_school == "Yes")
# sum(COMIX_contacts$cnt_home == "Yes" & COMIX_contacts$cnt_work == "Yes" & COMIX_contacts$cnt_other == "Yes")
# sum(COMIX_contacts$cnt_home == "Yes" & COMIX_contacts$cnt_school == "Yes" & COMIX_contacts$cnt_other == "Yes")
# sum(COMIX_contacts$cnt_work == "Yes" & COMIX_contacts$cnt_school == "Yes" & COMIX_contacts$cnt_other == "Yes")
# # 7 triple coding
# # 42 - 7 = 35

# Compute number of contacts per participant
COMIX_contacts2 <- COMIX_contacts %>%
  group_by(part_id) %>%
  mutate(COMIX_home   = sum(cnt_home == "Yes"),
         COMIX_work   = sum(cnt_work == "Yes"),
         COMIX_school = sum(cnt_school == "Yes"),
         COMIX_other  = sum(cnt_other == "Yes"),
         COMIX_physical = sum(phys_contact == 1),
         COMIX_tot = COMIX_home + COMIX_work + COMIX_school + COMIX_other) %>% 
  slice(1) %>%
  ungroup() %>%
  select(-c("cont_id", "phys_contact",
            "cnt_home", "cnt_work", "cnt_school", "cnt_other"))
head(COMIX_contacts2)
#

DFcontacts <-merge(merge(merge(merge(merge(merge(
  DFcontacts,
  COMIX_contacts2 %>% count(COMIX_home) %>% set_colnames(c("n", "COMIX_home")),
  all = TRUE),
  COMIX_contacts2 %>% count(COMIX_work) %>% set_colnames(c("n", "COMIX_work")),
  all = TRUE),
  COMIX_contacts2 %>% count(COMIX_school) %>% set_colnames(c("n", "COMIX_school")),
  all = TRUE),
  COMIX_contacts2 %>% count(COMIX_other) %>% set_colnames(c("n", "COMIX_other")),
  all = TRUE),
  COMIX_contacts2 %>% count(COMIX_physical) %>% set_colnames(c("n", "COMIX_physical")),
  all = TRUE),
  COMIX_contacts2 %>% count(COMIX_tot) %>% set_colnames(c("n", "COMIX_tot")),
  all = TRUE)

head(DFcontacts)
#

# COMIX_n_contacts and COMIX_tot are slightly different 
DFcontacts %>%
  select(n, COMIX_n_contacts, COMIX_tot)
# COMIX_n_contacts has people with zero contacts, COMIX_tot doesn't.
# and COMIX_tot probably has the 35 overcounting.
sum(DFcontacts$COMIX_n_contacts, na.rm = T) - sum(DFcontacts$COMIX_tot, na.rm = T)
# diff is 95 = number of indiv with zero contacts.


### 3) Sekara et al.
# PNAS paper : https://www.pnas.org/content/113/36/9977
# Data? https://www.nature.com/articles/s41597-019-0325-x#additional-information
# 
# Figure S9 provides size od gatherings (as well as duration and other stuff)
# And Figure S15 for dyadic gatherings, but it shows number of daily occurences I believe.

# I extracted the coordinates from the points of Figure S9 using the online tool WebPlotDigitizer.
# Note : S9 doesn't give the number of occurences, only the proportion of gatherings of each size.
Sekara_S9 <- read_excel("Desktop/Contact_data/Data/Sekara/Sekara_S9.xlsx", col_names = FALSE)
#View(Sekara_S9)
colnames(Sekara_S9) <- c("n", "Sekara_S9")
head(Sekara_S9)
sum(Sekara_S9$Sekara_S9)

Sekara_S9 <- Sekara_S9 %>%
  mutate(n = round(n),
         Sekara_S9 = round(Sekara_S9, 5))

ggplot(Sekara_S9) +
  geom_point(aes(x = n, y = Sekara_S9)) +
  scale_y_log10() + scale_x_log10() +
  theme_minimal()
#
DFcontacts <- merge(DFcontacts, Sekara_S9, all = TRUE)
#


### Plot : 
# Note here that some datasets have info for 0 number of contacts, some don't.
# And log transformation somewhat obscures that.

# With y-axis number of occurences
# ggplot(DFcontacts) +
#   geom_point(aes(x = n, y = BBC_home, colour = "BBC_home")) +
#   geom_point(aes(x = n, y = BBC_work, colour = "BBC_work")) +
#   geom_point(aes(x = n, y = BBC_other, colour = "BBC_other")) +
#   geom_point(aes(x = n, y = BBC_total, colour = "BBC_total")) +
#   geom_point(aes(x = n, y = COMIX_n_contacts, colour = "COMIX_n_contacts")) +
#   geom_point(aes(x = n, y = COMIX_tot, col = "COMIX_tot")) +
#   geom_point(aes(x = n, y = COMIX_home, col = "COMIX_home")) +
#   geom_point(aes(x = n, y = COMIX_work, col = "COMIX_work")) +
#   geom_point(aes(x = n, y = COMIX_school, col = "COMIX_school")) +
#   geom_point(aes(x = n, y = COMIX_other, col = "COMIX_other")) +
#   geom_point(aes(x = n, y = COMIX_physical, col = "COMIX_physical")) +
#   theme_minimal() +
#   # scale_color_manual(values = c("BBCpandemic" = '#ff00ff', 'COMIX_al' = '#3399ff',)) +
#   scale_y_log10() + scale_x_log10() +
#   labs(colour = "Data source") +
#   ylab("Number of occurences") + xlab("Number of contacts")

# With y-axis proportion
DFcontactsprop <- DFcontacts %>%
  mutate(BBC_homeprop = BBC_home / sum(BBC_home, na.rm = T),
         BBC_workprop = BBC_work / sum(BBC_work, na.rm = T),
         BBC_otherprop = BBC_other / sum(BBC_other, na.rm = T),
         BBC_totalprop = BBC_total / sum(BBC_total, na.rm = T),
         COMIX_n_contactsprop = COMIX_n_contacts / sum(COMIX_n_contacts, na.rm = T),
         COMIX_totprop = COMIX_tot / sum(COMIX_tot, na.rm = T),
         COMIX_homeprop = COMIX_home / sum(COMIX_home, na.rm = T),
         COMIX_workprop = COMIX_work / sum(COMIX_work, na.rm = T),
         COMIX_schoolprop = COMIX_school / sum(COMIX_school, na.rm = T),
         COMIX_otherprop = COMIX_other / sum(COMIX_other, na.rm = T),
         COMIX_physicalprop = COMIX_physical / sum(COMIX_physical, na.rm = T),
         Sekara_S9prop = Sekara_S9) # Sekara data are already proportion.s
head(DFcontactsprop)

#
ggplot(DFcontactsprop) +
  geom_point(aes(x = n, y = BBC_homeprop, colour = "BBC_home")) +
  geom_point(aes(x = n, y = BBC_workprop, colour = "BBC_work")) +
  geom_point(aes(x = n, y = BBC_otherprop, colour = "BBC_other")) +
  geom_point(aes(x = n, y = BBC_totalprop, colour = "BBC_total")) +
  geom_point(aes(x = n, y = COMIX_n_contactsprop, colour = "COMIX_n_contacts")) +
  geom_point(aes(x = n, y = COMIX_totprop, col = "COMIX_tot")) +
  geom_point(aes(x = n, y = COMIX_homeprop, col = "COMIX_home")) +
  geom_point(aes(x = n, y = COMIX_workprop, col = "COMIX_work")) +
  geom_point(aes(x = n, y = COMIX_schoolprop, col = "COMIX_school")) +
  geom_point(aes(x = n, y = COMIX_otherprop, col = "COMIX_other")) +
  geom_point(aes(x = n, y = COMIX_physicalprop, col = "COMIX_physical")) +
  geom_point(aes(x = n, y = Sekara_S9prop, col = "Sekara_S9")) +
  theme_minimal() +
  scale_x_log10() +
  scale_y_log10() +
  labs(colour = "Data source") +
  ylab("Proportion") + xlab("Number of contacts")
#





#
##
####
#######

### 4) RFID sensor studies
# http://www.sociopatterns.org/publications/
# Datasets : http://www.sociopatterns.org/datasets/

# Co-location data for several SocioPatterns data sets
# http://www.sociopatterns.org/datasets/co-location-data-for-several-sociopatterns-data-sets/

# Files:
# – co-presence.tar.gz contains all six co-presence networks. Both contacts and co-presence data are
#formatted as tij, i.e. each line represents a contact occurring at a time t between two nodes i and j.
# – metadata.tar contains the lists of nodes, with the first column being the node identifier and the
#second the group affiliation, when available.

#This contains data from 6 different studies ! 
# :D

# LH10
# Hospital ward
tij_pres_LH10 <- read.table("co-presence/tij_pres_LH10.dat")
metadata_LH10 <- read.table("metadata/metadata_LH10.dat")
head(tij_pres_LH10)
head(metadata_LH10)

nrow(metadata_LH10) # 81 participants
nrow(tij_pres_LH10) # 150126 interactions
tij_pres_LH10[tij_pres_LH10$V2 == 1305,]

# Check number of distinct people contacte
tij_pres_LH10count <- tij_pres_LH10 %>%
  select(-V1) %>%
  distinct(V2, V3)  %>%
  pivot_longer(cols = c(V2, V3)) %>%
  count(value)
hist(tij_pres_LH10count$value)
#

# InVS13
# Workplace, first data collection in 2013
tij_pres_InVS13 <- read.table("co-presence/tij_pres_InVS13.dat")
metadata_InVS13 <- read.table("metadata/metadata_InVS13.dat")
head(tij_pres_InVS13)
head(metadata_InVS13)

nrow(metadata_InVS13) # 100 participants
nrow(tij_pres_InVS13) # 394247 interactions

# Check number of distinct people contacte
tij_pres_InVS13count <- tij_pres_InVS13 %>%
  select(-V1) %>%
  distinct(V2, V3)  %>%
  pivot_longer(cols = c(V2, V3)) %>%
  count(value)
hist(tij_pres_InVS13count$value)

# InVS15
# Workplace, second data collection in 2015
tij_pres_InVS15 <- read.table("co-presence/tij_pres_InVS15.dat")
metadata_InVS15 <- read.table("metadata/metadata_InVS15.dat")
head(tij_pres_InVS15)
head(metadata_InVS15)

nrow(metadata_InVS15) # 232 participants
nrow(tij_pres_InVS15) # 1283194 interactions

# Check number of distinct people contacte
tij_pres_InVS15count <- tij_pres_InVS15 %>%
  select(-V1) %>%
  distinct(V2, V3)  %>%
  pivot_longer(cols = c(V2, V3)) %>%
  count(value)
hist(tij_pres_InVS15count$value)
#

# LyonSchool
# Primary school
tij_pres_LyonSchool <- read.table("co-presence/tij_pres_LyonSchool.dat")
metadata_LyonSchool <- read.table("metadata/metadata_LyonSchool.dat")
head(tij_pres_LyonSchool)
head(metadata_LyonSchool)

nrow(metadata_LyonSchool) # 242 participants
nrow(tij_pres_LyonSchool) # 6594492 interactions

# Check number of distinct people contacte
tij_pres_LyonSchool2 <- tij_pres_LyonSchool %>% select(-V1)
tij_pres_LyonSchool3 <- tij_pres_LyonSchool2 %>%
  distinct(V2, V3)  # Wayyyy faster than filter(!duplicated(i, j))
tij_pres_LyonSchool4 <- tij_pres_LyonSchool3 %>%
  pivot_longer(cols = c(V2, V3)) %>%
  count(value)
hist(tij_pres_LyonSchool4$value)
#

# Thiers13
# High school
tij_pres_Thiers13 <- read.table("co-presence/tij_pres_Thiers13.dat")
metadata_Thiers13 <- read.table("metadata/metadata_Thiers13.dat")
head(tij_pres_Thiers13)
head(metadata_Thiers13)

nrow(metadata_Thiers13) # 332 participants
nrow(tij_pres_Thiers13) # 18613039 interactions

# Check number of distinct people contacte
tij_pres_Thiers132 <- tij_pres_Thiers13 %>% select(-V1)
tij_pres_Thiers133 <- tij_pres_Thiers132 %>%
  distinct(V2, V3)  # Wayyyy faster than filter(!duplicated(i, j))
tij_pres_Thiers134 <- tij_pres_Thiers133 %>%
  pivot_longer(cols = c(V2, V3)) %>%
  count(value)
hist(tij_pres_Thiers134$value)
#

# SFHH
# Scientific conference
# http://www.sociopatterns.org/datasets/sfhh-conference-data-set/
tij_pres_SFHH <- read.table("co-presence/tij_pres_SFHH.dat")
colnames(tij_pres_SFHH) <- c("t", "i", "j")
metadata_SFHH <- read.table("metadata/metadata_SFHH.dat")
head(tij_pres_SFHH)
head(metadata_SFHH)

nrow(metadata_SFHH) # 403 participants
nrow(tij_pres_SFHH) # 1417485 interactions

# Maybe start by looking more into the conference, as is closer to 'other' contacts ? And is a laaarger gathering ?
head(tij_pres_SFHH)
head(metadata_SFHH)
unique(metadata_SFHH$V2) # no categories of participants

unique(tij_pres_SFHH$t)
# Proximity is measured every 20 seconds. But contacts can last longer than that.

# Note : Here we first count the number of contacts each participants made in the duration of the conference.
# Option for later maybe : count the actual gathering sizes. 

# TODO : Compute the number of contacts separately for each day of the conference.
# Here's time distribution on two days : hist(tij_pres_SFHH$t)

## Number of discrete individuals each participant gets in contact with.
# Drop time column
tij_pres_SFHH2 <- tij_pres_SFHH %>% select(-t)
# Filter out unique i,j, combination, aka drop duplicate rows (which come from repeated measurements over time)
tij_pres_SFHH3 <- tij_pres_SFHH2 %>%
  distinct(i, j)  # Wayyyy faster than filter(!duplicated(i, j))
# Count how many times each id appears in either i or j
tij_pres_SFHH4 <- tij_pres_SFHH3 %>%
  pivot_longer(cols = c(i,j)) %>%
  count(value)
#
head(tij_pres_SFHH4)
hist(tij_pres_SFHH4$n)
# Doesn't mean much as there were 403 participants, so everyone walked past everyone.
#



#
# There's another conference they collected data at !!!
# The 25C3, Choas conference Berlin 
# https://journals.plos.org/plosone/article?id=10.1371/journal.pone.0011596
# But need to obtain data separately.



###











