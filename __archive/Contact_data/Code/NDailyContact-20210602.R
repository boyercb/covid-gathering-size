
##########################################
### Distribution of number of contacts ###
##########################################

  ######################
  ### Load libraries ###
  ######################

library(tidyverse)
library(magrittr)
library(readxl)
library(poweRlaw)
library(wesanderson)
library(scales)
library(ggpubr)

  #######################################
  ###         Load input data         ###
  ### and join them all in DFcontacts ###
  #######################################

#######################
### 1) BBC Pandemic ###
#######################

#Load data of number of contacts from BBC Pandemic   
BBCpandemic <- read_csv("Contact_data/Data/BBC_Pandemic/contact_distributions_o18.csv")
names(BBCpandemic) <- c("BBC_home", "BBC_work", "BBC_other")
BBCpandemic
#head(BBCpandemic)

# Generate DFcontacts and add all datasets in this df
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

# Check DFcontacts
DFcontacts
head(DFcontacts)
#

######################
### 2) COMIX study ###
######################

# From : https://bmcmedicine.biomedcentral.com/articles/10.1186/s12916-020-01597-8#availability-of-data-and-materials
# Github : https://github.com/jarvisc1/comix_covid-19-first_wave

# This dataset doesn't seem to have interesting info for us.
#COMIX_households <- readRDS("GitHub/covid-gathering-size/Contact_data/Data/COMIX/clean_households.rds")
#View(COMIX_households)

# This dataset has the varibale n_contacts,  which I assume corresponds to all contacts (?)
# TODO : Not using this further, can go back to it later.
# COMIX_participants <- readRDS("GitHub/covid-gathering-size/Contact_data/Data/COMIX/clean_participants.rds")
# #View(COMIX_participants)
# colnames(COMIX_participants)
# #unique(COMIX_participants$country) # Only UK
# table(COMIX_participants$n_contacts)
# 
# # Add COMIX_n_contacts to DFcontacts
# DFcontacts <- merge(DFcontacts,
#       COMIX_participants %>%
#         count(n_contacts) %>%
#         set_colnames(c("n", "COMIX_n_contacts")),
#       by = "n",
#       all = TRUE)
# 
# # Check DFcontacts
# head(DFcontacts)
# #

# This dataset has number of contacts per setting !
COMIX_contacts <- readRDS("Contact_data/Data/COMIX/clean_contacts.rds")
COMIX_contacts <- COMIX_contacts[with(COMIX_contacts, order(part_id, cont_id)),]
COMIX_contacts <- COMIX_contacts %>% select(-c("cnt_gender", "cnt_age_est_min", "cnt_age_est_max"))
COMIX_contacts
#View(COMIX_contacts)
head(COMIX_contacts)

# Recode to get number of 'other' contacts
# I assume 808 contacts that are not home, not work and not school are 'other'.
nrow(COMIX_contacts %>% filter(cnt_home == "No",
                               cnt_work == "No",
                               cnt_school == "No"))
COMIX_contacts <- COMIX_contacts %>%
  mutate(cnt_other = case_when((cnt_home == "No" & cnt_work == "No" & cnt_school == "No") ~ "Yes",
                               TRUE ~ "No" ),
         cnt_workschool = case_when((cnt_work == "Yes" | cnt_school == "Yes") ~ "Yes",
                                    TRUE ~ "No") )

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
# And joing work and school to match the BBC pandemic data
# TODO : check that indeed in the BBC pandemic data 'work' contains both work and school !
COMIX_contacts2 <- COMIX_contacts %>%
  group_by(part_id) %>%
  mutate(COMIX_home   = sum(cnt_home == "Yes"),
         COMIX_work   = sum(cnt_work == "Yes"),
         COMIX_school = sum(cnt_school == "Yes"),
         COMIX_other  = sum(cnt_other == "Yes"),
         COMIX_workschool   = sum(cnt_workschool == "Yes"),
       # COMIX_workschool2  = sum(cnt_work == "Yes" | cnt_school == "Yes"),
         COMIX_physical = sum(phys_contact == 1),
         COMIX_tot = COMIX_home + COMIX_work + COMIX_school + COMIX_other) %>% 
  slice(1) %>%
  ungroup() %>%
  select(-c("cont_id", "phys_contact",
            "cnt_home", "cnt_work", "cnt_school", "cnt_workschool", "cnt_other"))
head(COMIX_contacts2)

# Add COMIX datato DFcontacts
DFcontacts <-merge(merge(merge(merge(merge(merge(merge(
  DFcontacts,
  COMIX_contacts2 %>% count(COMIX_home) %>% set_colnames(c("n", "COMIX_home")),
  all = TRUE),
  COMIX_contacts2 %>% count(COMIX_work) %>% set_colnames(c("n", "COMIX_work")),
  all = TRUE),
  COMIX_contacts2 %>% count(COMIX_school) %>% set_colnames(c("n", "COMIX_school")),
  all = TRUE),
  COMIX_contacts2 %>% count(COMIX_workschool) %>% set_colnames(c("n", "COMIX_workschool")),
  all = TRUE),
  COMIX_contacts2 %>% count(COMIX_other) %>% set_colnames(c("n", "COMIX_other")),
  all = TRUE),
  COMIX_contacts2 %>% count(COMIX_physical) %>% set_colnames(c("n", "COMIX_physical")),
  all = TRUE),
  COMIX_contacts2 %>% count(COMIX_tot) %>% set_colnames(c("n", "COMIX_tot")),
  all = TRUE)

# Check DFcontacts
head(DFcontacts)
#

# # COMIX_n_contacts and COMIX_tot are slightly different 
# DFcontacts %>%
#   select(n, COMIX_n_contacts, COMIX_tot)
# # COMIX_n_contacts has people with zero contacts, COMIX_tot doesn't.
# # and COMIX_tot probably has the 35 overcounting.
# sum(DFcontacts$COMIX_n_contacts, na.rm = T) - sum(DFcontacts$COMIX_tot, na.rm = T)
# # diff is 95 = number of indiv with zero contacts.


########################
### 3) Sekara et al. ###
########################

# PNAS paper : https://www.pnas.org/content/113/36/9977
# Data? https://www.nature.com/articles/s41597-019-0325-x#additional-information
# Figure S9 provides size of gatherings (as well as duration and other stuff)
# And Figure S15 for dyadic gatherings, but it shows number of daily occurences I believe.

# I extracted the coordinates from the points of Figure S9 using the online tool WebPlotDigitizer.
# Note : S9 doesn't give the number of occurences, only the proportion of gatherings of each size.
Sekara_S9 <- read_excel("Contact_data/Data/Sekara/Sekara_S9.xlsx",
                        sheet = "FigureS9",
                        col_names = FALSE)
#View(Sekara_S9)
colnames(Sekara_S9) <- c("n", "Sekara_S9prop")
head(Sekara_S9)
sum(Sekara_S9$Sekara_S9prop) # Ok very close to 1, the extraction of the values worked pretty well.

# Compute the number of gatherings from the proportion of gatherings
# Number of gatherings (page 13 of Appendix)
# Off campus 13872
# On campus   9195
# Total      23067

Sekara_S9 <- Sekara_S9 %>%
  mutate(n = round(n),
         Sekara_S9prop = round(Sekara_S9prop, 5),
         Sekara_S9 = Sekara_S9prop*23067,
         Sekara_S9 = round(Sekara_S9))

# The Sekara data shows gathering size, not the number of contacts, so need to shift n down by one to represent number of contacts.
# TODO : recheck this later
Sekara_S9 <- Sekara_S9 %>%
  mutate(n = n - 1)

head(Sekara_S9)

# Check plot ressembles the S9
# ggplot(Sekara_S9) +
#   geom_point(aes(x = n, y = Sekara_S9)) +
#   scale_y_log10() + scale_x_log10() +
#   theme_minimal()

# Add to DFcontacts and check DFcontacts
DFcontacts <- merge(DFcontacts, Sekara_S9, all = TRUE)
head(DFcontacts)
#


#####################################################
### Modify DFcontacts and generate DFcontactsprop ###
#####################################################

# Note here that some datasets have info for 0 number of contacts, some don't.
# And log transformation somewhat obscures that.
# BBC and COMIX are number of contacts and Sekara is gathering size. 

# Rename n in n_contacts to be more clear and create n_gatherings
DFcontacts <- DFcontacts %>%
  rename(n_contacts = "n") %>%
  mutate(n_gatherings = n_contacts + 1) %>%
  select(n_contacts, n_gatherings, everything())
head(DFcontacts)

# Generate another DFcontactsprop with proportion of gatherings of each size, instead of count.
DFcontactsprop <- DFcontacts %>%
  mutate(BBC_homeprop = BBC_home / sum(BBC_home, na.rm = T),
         BBC_workprop = BBC_work / sum(BBC_work, na.rm = T),
         BBC_otherprop = BBC_other / sum(BBC_other, na.rm = T),
         BBC_totalprop = BBC_total / sum(BBC_total, na.rm = T),
         #  COMIX_n_contactsprop = COMIX_n_contacts / sum(COMIX_n_contacts, na.rm = T),
         COMIX_homeprop = COMIX_home / sum(COMIX_home, na.rm = T),
         COMIX_workprop = COMIX_work / sum(COMIX_work, na.rm = T),
         COMIX_schoolprop = COMIX_school / sum(COMIX_school, na.rm = T),
         COMIX_workschoolprop = COMIX_workschool / sum(COMIX_workschool, na.rm = T),
         COMIX_otherprop = COMIX_other / sum(COMIX_other, na.rm = T),
         COMIX_physicalprop = COMIX_physical / sum(COMIX_physical, na.rm = T),
         COMIX_totprop = COMIX_tot / sum(COMIX_tot, na.rm = T)) %>%
  select(-Sekara_S9prop, Sekara_S9prop)
head(DFcontactsprop)
#

# Plot them ! 
# # With y-axis number of occurences :
# ggplot(DFcontacts) +
#   geom_point(aes(x = n_gatherings, y = BBC_home, colour = "BBC_home")) +
#   geom_point(aes(x = n_gatherings, y = BBC_work, colour = "BBC_work")) +
#   geom_point(aes(x = n_gatherings, y = BBC_other, colour = "BBC_other")) +
#   geom_point(aes(x = n_gatherings, y = BBC_total, colour = "BBC_total")) +
# #  geom_point(aes(x = n_gatherings, y = COMIX_n_contacts, colour = "COMIX_n_contacts")) +
#   geom_point(aes(x = n_gatherings, y = COMIX_tot, col = "COMIX_tot")) +
#   geom_point(aes(x = n_gatherings, y = COMIX_home, col = "COMIX_home")) +
#   geom_point(aes(x = n_gatherings, y = COMIX_work, col = "COMIX_work")) +
#   geom_point(aes(x = n_gatherings, y = COMIX_school, col = "COMIX_school")) +
#   geom_point(aes(x = n_gatherings, y = COMIX_other, col = "COMIX_other")) +
# #  geom_point(aes(x = n_gatherings, y = COMIX_physical, col = "COMIX_physical")) +
#   theme_minimal() +
#   # scale_color_manual(values = c("BBCpandemic" = '#ff00ff', 'COMIX_al' = '#3399ff',)) +
#   scale_y_log10() + scale_x_log10() +
#   labs(colour = "Data source") +
#   ylab("Number of occurences") + xlab("Size of gatherings")

# With y-axis proportion
ggplot(DFcontactsprop) +
  geom_point(aes(x = n_gatherings, y = BBC_homeprop, colour = "BBC_home")) +
  geom_point(aes(x = n_gatherings, y = BBC_workprop, colour = "BBC_work")) +
  geom_point(aes(x = n_gatherings, y = BBC_otherprop, colour = "BBC_other")) +
  geom_point(aes(x = n_gatherings, y = BBC_totalprop, colour = "BBC_total")) +
  geom_point(aes(x = n_gatherings, y = Sekara_S9prop, col = "Sekara_S9")) +
  theme_minimal() +
  scale_x_log10() +
  scale_y_log10() +
  labs(colour = "Data source") +
  ylab("Proportion") + xlab("Size of gatherings")
#

# Note : in the BBC Pandemic dataset (https://www.thelancet.com/journals/laninf/article/PIIS1473-3099(20)30457-6/fulltext),
# 'work' corresponds to work or school. So in COMIX data, use COMIX_workschool corresponds to BBC_work I THINK.
# TODO : recheck this later.

###############################
### Generate Figure 2 and 3 ###
###############################

# My color palettes
wes_palette(name = "Darjeeling1")
wes_palette(name = "Darjeeling1")[1:5]
my_col5 <- c("#FF0000", "#F2AD00", "#5BBCD6", "#F98400",  "#00A08A")
my_col5
my_col9 <- c("#F2AD00", "#5BBCD6", "#F98400",  "#00A08A", "grey50", "grey55", "grey60", "grey65", "grey70")
my_col9
my_col4 <- c("#F2AD00", "#5BBCD6", "#F98400",  "#00A08A")
my_col4

## Figure 1
plotFig1 <- ggplot(DFcontactsprop) +
  geom_point(aes(x = n_gatherings, y = Sekara_S9prop, colour = "Sekara et al."), shape = 16, size = 1.5) +
  geom_point(aes(x = n_gatherings, y = BBC_homeprop, colour = "BBC home"), shape = 16, size = 1.5) +
  geom_point(aes(x = n_gatherings, y = BBC_workprop, colour = "BBC work/school"), shape = 16, size = 1.5) +
  geom_point(aes(x = n_gatherings, y = BBC_otherprop, colour = "BBC other"), shape = 16, size = 1.5) +
  geom_point(aes(x = n_gatherings, y = BBC_totalprop, colour = "BBC total"), shape = 16, size = 1.5) +
  scale_color_manual(values = my_col5,
                     limits = c("Sekara et al.", "BBC work/school", "BBC total", "BBC other", "BBC home")) +
  theme_pubr(base_size = 11, base_family = "Palatino") +
  theme(legend.position = c(0.85, 0.75))+
  scale_x_log10() +
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  annotation_logticks() +
  labs(colour = "Data source") +
  ylab("Pr(K = k)") + xlab("Size of gatherings (k)")
plotFig1


# Figure 2
plotFig2 <- ggplot(DFcontactsprop) +
  geom_point(aes(x = n_gatherings, y = Sekara_S9prop, colour = "Sekara et al."), alpha = 5/10, shape = 3) +
  geom_point(aes(x = n_gatherings, y = BBC_homeprop, colour = "BBC home"), alpha = 5/10, shape = 3) +
  geom_point(aes(x = n_gatherings, y = BBC_workprop, colour = "BBC work/school"), alpha = 5/10, shape = 3) +
  geom_point(aes(x = n_gatherings, y = BBC_otherprop, colour = "BBC other"), alpha = 5/10, shape = 3) +
  geom_point(aes(x = n_gatherings, y = BBC_totalprop, colour = "BBC total"), alpha = 5/10, shape = 3) +
  geom_point(aes(x = n_gatherings, y = COMIX_homeprop, colour = "COMIX home"), size = 1.8, shape = 16) +
  geom_point(aes(x = n_gatherings, y = COMIX_workschoolprop, colour = "COMIX work/school"), size= 1.8, shape = 16) +
  geom_point(aes(x = n_gatherings, y = COMIX_otherprop, colour = "COMIX other"), size = 1.8, shape = 16) +
  geom_point(aes(x = n_gatherings, y = COMIX_totprop, colour = "COMIX total"), size = 1.8, shape = 16) +
  scale_color_manual(values = my_col9,
                     limits = c("COMIX work/school", "COMIX total", "COMIX other", "COMIX home",
                       "Sekara et al.", "BBC work/school", "BBC total", "BBC other", "BBC home")) +
  theme_pubr(base_size = 11, base_family = "Palatino") +
  theme(legend.position = "right")+
  scale_x_log10() +
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  annotation_logticks() +
  labs(colour = "Data source") +
  ylab("Pr(K = k)") + xlab("Size of gatherings (k)")
plotFig2


plotFig2 <- ggplot(DFcontactsprop %>% 
                     select(-Sekara_S9prop, -COMIX_physicalprop, -COMIX_workprop, -COMIX_schoolprop) %>%
                     pivot_longer(cols = ends_with("prop")) %>%
                     separate(name, c("source", "variable"), sep = "_", remove = FALSE) %>%
                     mutate(
                       variable = case_when(
                         variable == "homeprop" ~ "Home",
                         variable == "otherprop" ~ "Other",
                         variable == "totalprop" | variable == "totprop" ~ "Total",
                         variable == "workprop" | variable == "workschoolprop" ~ "Work/school"
                        ),
                       source = case_when(
                         source == "BBC" ~ "BBC Pandemic",
                         source == "COMIX" & variable == "Home" ~ "COMIX home",
                         source == "COMIX" & variable == "Work/school" ~ "COMIX work/school",
                         source == "COMIX" & variable == "Total" ~ "COMIX total",
                         source == "COMIX" & variable == "Other" ~ "COMIX other",
                       ),
                       source = factor(source, levels = c(
                         "BBC Pandemic",
                         "COMIX work/school",
                         "COMIX total",
                         "COMIX other",
                         "COMIX home"
                       )),
                       variable = factor(variable, levels = c(
                         "Home",
                         "Work/school",
                         "Other",
                         "Total"
                       ))
                     )
                   ) +
  facet_wrap(~variable, scales = "free") +
  scale_color_manual(values = c(alpha("grey75", 0.5), "#F2AD00", "#5BBCD6", "#F98400",  "#00A08A")) +
  theme_pubr(base_size = 10, base_family = "Palatino") +
  theme(legend.position = "none") +
  scale_x_log10() +
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  annotation_logticks() +
  geom_point(aes(x = n_gatherings, y = value, color = source), shape = 16, size = 1.5) +
  labs(colour = "Data source") +
  ylab("Pr(K = k)") + xlab("Size of gatherings (k)")
plotFig2
# Join the two plots
ggarrange(plotFig1, plotFig1cdf, legend = "right") #, legend="bottom")



  #####################################
  ### Fitting powerlaw distribution ###
  ###   Using the poweRlaw package  ###
  #####################################

# Using estimate_xmin
# Fit for 5 distributions : BBC total, home, work/school, other and Sekara.
#
colnames(DFcontactsprop)
head(DFcontactsprop)
#

# BBC_total
DF_BBC_total <- DFcontactsprop %>%
  select(n_gatherings, BBC_total) %>%
  filter(!is.na(BBC_total)) %>%
  as_tibble() %>%
  uncount(BBC_total) %>%
  pull(n_gatherings)
DF_BBC_total

displ_BBC_total <- displ$new(DF_BBC_total)
#displ_BBC_total
est_BBC_total <- estimate_xmin(displ_BBC_total)
est_BBC_total
est_BBC_total$pars
est_BBC_total$xmin

displ_BBC_total$setXmin(est_BBC_total)
plot.displ_BBC_total <- plot(displ_BBC_total, draw = F)
fit.displ_BBC_total <- lines(displ_BBC_total, draw = F)

# BBC_work
DF_BBC_work <- DFcontactsprop %>%
  select(n_gatherings, BBC_work) %>%
  filter(!is.na(BBC_work)) %>%
  as_tibble() %>%
  uncount(BBC_work) %>%
  pull(n_gatherings)
DF_BBC_work

displ_BBC_work <- displ$new(DF_BBC_work)
#displ_BBC_work
est_BBC_work <- estimate_xmin(displ_BBC_work)
#est_BBC_work

displ_BBC_work$setXmin(est_BBC_work)
plot.displ_BBC_work <- plot(displ_BBC_work, draw = F)
fit.displ_BBC_work <- lines(displ_BBC_work, draw = F)

# BBC_other
DF_BBC_other <- DFcontactsprop %>%
  select(n_gatherings, BBC_other) %>%
  filter(!is.na(BBC_other)) %>%
  as_tibble() %>%
  uncount(BBC_other) %>%
  pull(n_gatherings)
DF_BBC_other

displ_BBC_other <- displ$new(DF_BBC_other)
#displ_BBC_other
est_BBC_other <- estimate_xmin(displ_BBC_other)
#est_BBC_other

displ_BBC_other$setXmin(est_BBC_other)
plot.displ_BBC_other <- plot(displ_BBC_other, draw = F)
fit.displ_BBC_other <- lines(displ_BBC_other, draw = F)

# BBC_home
DF_BBC_home <- DFcontactsprop %>%
  select(n_gatherings, BBC_home) %>%
  filter(!is.na(BBC_home)) %>%
  as_tibble() %>%
  uncount(BBC_home) %>%
  pull(n_gatherings)
DF_BBC_home

displ_BBC_home <- displ$new(DF_BBC_home)
#displ_BBC_home
est_BBC_home <- estimate_xmin(displ_BBC_home)
#est_BBC_home

displ_BBC_home$setXmin(est_BBC_home)
plot.displ_BBC_home <- plot(displ_BBC_home, draw = F)
fit.displ_BBC_home <- lines(displ_BBC_home, draw = F)

# Sekara_S9
DF_Sekara_S9 <- DFcontactsprop %>%
  select(n_gatherings, Sekara_S9) %>%
  filter(!is.na(Sekara_S9)) %>%
  as_tibble() %>%
  uncount(Sekara_S9) %>%
  pull(n_gatherings)
DF_Sekara_S9

displ_Sekara_S9 <- displ$new(DF_Sekara_S9)
#displ_Sekara_S9
est_Sekara_S9 <- estimate_xmin(displ_Sekara_S9)
#est_Sekara_S9

displ_Sekara_S9$setXmin(est_Sekara_S9)
plot.displ_Sekara_S9 <- plot(displ_Sekara_S9, draw = F)
fit.displ_Sekara_S9 <- lines(displ_Sekara_S9, draw = F)

d <- dislnorm$new(DF_Sekara_S9)
estimate_xmin(d)
# Generate Figure 3 CDF
# Generate geom text to put alpha and xmin values on Figure 3 
df_geomtext <- data.frame(x = c(0.1, 0.1, 0.1, 0.1, 0.1),
                          y = c(-9, -8, -10, -11, -7),
                          xmin = c(est_BBC_total$xmin,
                                   est_BBC_work$xmin,
                                   est_BBC_other$xmin,
                                   est_BBC_home$xmin,
                                   est_Sekara_S9$xmin),
                          alpha = c(est_BBC_total$pars,
                                    est_BBC_work$pars,
                                    est_BBC_other$pars,
                                    est_BBC_home$pars,
                                    est_Sekara_S9$pars),
                          colour = c("BBC total", "BBC work/school", "BBC other", "BBC home", "Sekara et al."))
#df_geomtext$label <- paste("alpha =", round(df_geomtext$alpha, 2), "and xmin =", round(df_geomtext$xmin, 2))
df_geomtext$label <- paste0("alpha ==", format(round(df_geomtext$alpha, 2), nsmall = 2),
                            "~and~k[min] ==", round(df_geomtext$xmin, 2))

# Plot
plotFig3 <- ggplot() +
  geom_point(data = plot.displ_BBC_total, aes(x = x, y = y, col = "BBC total"), alpha = 5/10, shape = 16) +
  geom_point(data = plot.displ_BBC_work, aes(x = x, y = y, col = "BBC work/school"), alpha = 5/10, shape = 16) +
  geom_point(data = plot.displ_BBC_other, aes(x = x, y = y, col = "BBC other"), alpha = 5/10, shape = 16) +
  geom_point(data = plot.displ_BBC_home, aes(x = x, y = y, col = "BBC home"), alpha = 5/10, shape = 16) +
  geom_point(data = plot.displ_Sekara_S9, aes(x = x, y = y, col = "Sekara et al."), alpha = 5/10, shape = 16) +
  geom_line(data = fit.displ_BBC_total, aes(x = x, y = y, col = "BBC total"), size = 1.03) +
  geom_line(data = fit.displ_BBC_work, aes(x = x, y = y, col = "BBC work/school"), size = 1.03) +
  geom_line(data = fit.displ_BBC_other, aes(x = x, y = y, col = "BBC other"), size = 1.03) +
  geom_line(data = fit.displ_BBC_home, aes(x = x, y = y, col = "BBC home"), size = 1.03) +
  geom_line(data = fit.displ_Sekara_S9, aes(x = x, y = y, col = "Sekara et al."), size = 1.03) +
  geom_text(data = df_geomtext, aes(x = 10^x, y = exp(y), label = label, col = colour), hjust = "left", parse = TRUE, show.legend = FALSE) +
  scale_color_manual(values = my_col5,
                     limits = c("Sekara et al.", "BBC work/school", "BBC total", "BBC other", "BBC home")) +
  theme_pubr(base_size = 11, base_family = "Palatino") +
  theme(legend.position = "right")+
  labs(colour = "Data source") +
  scale_x_continuous(
    labels = trans_format('log10', math_format(10^.x)),
    trans = scales::log10_trans(),
  ) +
  scale_y_continuous(
    labels = trans_format('log10', math_format(10^.x)),
    trans = scales::log10_trans()
  ) +
  annotation_logticks() +
  # scale_y_continuous(
  #   labels = scales::trans_format("exp", scales::math_format(.x))
  # ) +
  ylab("Pr(K > k)") + xlab("Size of gatherings (k)")
plotFig3
 a#

 
# CCDF versions of 1 and 2
 plotFig1cdf <-  ggplot() +
   geom_point(data = plot.displ_BBC_total, aes(x = x, y = y, col = "BBC total"), shape = 16, size = 1.5) +
   geom_point(data = plot.displ_BBC_work, aes(x = x, y = y, col = "BBC work/school"), shape = 16, size = 1.5) +
   geom_point(data = plot.displ_BBC_other, aes(x = x, y = y, col = "BBC other"), shape = 16, size = 1.5) +
   geom_point(data = plot.displ_BBC_home, aes(x = x, y = y, col = "BBC home"), shape = 16, size = 1.5) +
   geom_point(data = plot.displ_Sekara_S9, aes(x = x, y = y, col = "Sekara et al."), shape = 16, size = 1.5) +
   scale_color_manual(values = my_col5,
                      limits = c("Sekara et al.", "BBC work/school", "BBC total", "BBC other", "BBC home")) +
   theme_pubr(base_size = 11, base_family = "Palatino") +
   theme(legend.position = "none")+
   labs(colour = "Data source") +
   scale_x_continuous(
     labels = trans_format('log10', math_format(10^.x)),
     trans = scales::log10_trans(),
   ) +
   scale_y_continuous(
     labels = trans_format('log10', math_format(10^.x)),
     trans = scales::log10_trans()
   ) +
   annotation_logticks() +
   # scale_y_continuous(
   #   labels = scales::trans_format("exp", scales::math_format(.x))
   # ) +
   ylab("Pr(K > k)") + xlab("Size of gatherings (k)")
 plotFig1cdf
# TODO : Future : Get how to plot power law estimation line for PDF.
#

pdf("3_results/bbcsekara.pdf", width = 4.5, height = 3.5)
 plotFig1 %>% print()
dev.off()

pdf("3_results/bbcsekara_cdf.pdf", width = 4.5, height = 3.5)
plotFig1cdf %>% print()
dev.off()

pdf("3_results/comix.pdf", width = 6.5, height = 5)
plotFig2 %>% print()
dev.off()

pdf("3_results/bbcsekara_fit.pdf", width = 6.5, height = 4.5)
plotFig3 %>% print()
dev.off()
##
###
##
#



