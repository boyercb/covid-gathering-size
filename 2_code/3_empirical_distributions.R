source('2_code/0_packages.R')

##########################################
### Distribution of number of contacts ###
##########################################

#######################################
###         Load input data         ###
### and join them all in DFcontacts ###
#######################################

#######################
### 1) BBC Pandemic ###
#######################

#Load data of number of contacts from BBC Pandemic   
BBCpandemic <-
  read_csv(
    file = "1_data/BBC_Pandemic/contact_distributions_o18.csv", 
    show_col_types = FALSE
  )

names(BBCpandemic) <- c("BBC_home", "BBC_work", "BBC_other")
BBCpandemic

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
DFcontacts <- 
  DFcontacts %>%
  rowwise() %>% 
  mutate(BBC_total = sum(BBC_home, BBC_work, BBC_other, na.rm = T)) %>%
  ungroup()

# Check DFcontacts
DFcontacts
#

######################
### 2) COMIX study ###
######################

# From : https://bmcmedicine.biomedcentral.com/articles/10.1186/s12916-020-01597-8#availability-of-data-and-materials
# Github : https://github.com/jarvisc1/comix_covid-19-first_wave

# This dataset has number of contacts per setting 
COMIX_contacts <- read_rds("1_data/COMIX/clean_contacts.rds")
COMIX_contacts <- COMIX_contacts[with(COMIX_contacts, order(part_id, cont_id)),]
COMIX_contacts <- COMIX_contacts %>% select(-c("cnt_gender", "cnt_age_est_min", "cnt_age_est_max"))
COMIX_contacts

# Recode to get number of 'other' contacts
# I assume 808 contacts that are not home, not work and not school are 'other'.
nrow(COMIX_contacts %>% filter(cnt_home == "No",
                               cnt_work == "No",
                               cnt_school == "No"))
COMIX_contacts <- 
  COMIX_contacts %>%
  mutate(
    cnt_other = 
      case_when(
        (cnt_home == "No" & cnt_work == "No" & cnt_school == "No") ~ "Yes",
        TRUE ~ "No" 
      ),
    cnt_workschool = 
      case_when(
        (cnt_work == "Yes" | cnt_school == "Yes") ~ "Yes",
        TRUE ~ "No"
        ) 
    )

# 35 with double or triple coding due to contact being classified in multiple
# categories. We'll ignore this as it seems reasonable that some contacts don't
# fit neatly into categories and it's a small fraction of the total.
sum(COMIX_contacts$cnt_home == "Yes") + sum(COMIX_contacts$cnt_work == "Yes") + sum(COMIX_contacts$cnt_school == "Yes") + sum(COMIX_contacts$cnt_other == "Yes")


# Compute number of contacts per participant
# And joining work and school to match the BBC pandemic data
# TODO : check that indeed in the BBC pandemic data 'work' contains both work and school !
COMIX_contacts2 <- 
  COMIX_contacts %>%
  group_by(part_id) %>%
  mutate(COMIX_home   = sum(cnt_home == "Yes"),
         COMIX_work   = sum(cnt_work == "Yes"),
         COMIX_school = sum(cnt_school == "Yes"),
         COMIX_other  = sum(cnt_other == "Yes"),
         COMIX_workschool = sum(cnt_workschool == "Yes"),
         COMIX_physical = sum(phys_contact == 1),
         COMIX_tot = COMIX_home + COMIX_work + COMIX_school + COMIX_other) %>% 
  slice(1) %>%
  ungroup() %>%
  select(-c("cont_id", "phys_contact",
            "cnt_home", "cnt_work", "cnt_school", "cnt_workschool", "cnt_other"))
head(COMIX_contacts2)

# Add COMIX data to DFcontacts
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

####################################
### 3) Copenhagen Networks Study ###
####################################

# PNAS paper : https://www.pnas.org/content/113/36/9977
# Data? https://www.nature.com/articles/s41597-019-0325-x#additional-information
# Figure S9 provides size of gatherings 
# And Figure S15 for dyadic gatherings, but it shows number of daily occurrences.

# I extracted the coordinates from the points of Figure S9 using the online tool WebPlotDigitizer.
# Note : S9 doesn't give the number of occurrences, only the proportion of gatherings of each size.
Sekara_S9 <- read_excel(path = "1_data/Sekara/Sekara_S9.xlsx",
                        sheet = "FigureS9",
                        col_names = FALSE)

colnames(Sekara_S9) <- c("n", "Sekara_S9prop")
head(Sekara_S9)

# verify that proportions sum to 1 (or close) given that data were extracted 
# from a figure
sum(Sekara_S9$Sekara_S9prop) 

# Compute the number of gatherings from the proportion of gatherings
# Number of gatherings (page 13 of Appendix)
# Off campus 13872
# On campus   9195
# Total      23067

Sekara_S9 <- 
  Sekara_S9 %>%
  mutate(n = round(n),
         Sekara_S9prop = round(Sekara_S9prop, 5),
         Sekara_S9 = Sekara_S9prop * 23067,
         Sekara_S9 = round(Sekara_S9)) %>%
  add_row(
    n = 2, Sekara_S9 = 34996, Sekara_S9prop = NA, .before = 1
  ) %>%
  mutate(Sekara_S9prop = Sekara_S9 / sum(Sekara_S9))


# The Sekara data shows gathering size, not the number of contacts, so need to
# shift n down by one to represent number of contacts. We compute gathering size
# for all datasets later
Sekara_S9 <- Sekara_S9 %>%
  mutate(n = n - 1)

head(Sekara_S9)

# Check plot resembles the S9
ggplot(Sekara_S9) +
  geom_point(aes(x = n, y = Sekara_S9)) +
  scale_y_log10() + scale_x_log10() +
  theme_minimal()

# Add to DFcontacts and check DFcontacts
DFcontacts <- merge(DFcontacts, Sekara_S9, all = TRUE)
head(DFcontacts)


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
  # Here we divide by n_gatherings to get the distribution of gatherings instead of that of number of contacts.
  mutate(
    across(
      c(BBC_home,
        BBC_work,
        BBC_other,
        BBC_total,
        COMIX_home,
        COMIX_work,
        COMIX_school,
        COMIX_workschool,
        COMIX_other,
        COMIX_physical,
        COMIX_tot,
      ), ~ .x / n_gatherings)) %>%
  # Here we get the proportions/distribution of gathering sizes
  mutate(BBC_homeprop = BBC_home / sum(BBC_home, na.rm = T),
         BBC_workprop = BBC_work / sum(BBC_work, na.rm = T),
         BBC_otherprop = BBC_other / sum(BBC_other, na.rm = T),
         BBC_totalprop = BBC_total / sum(BBC_total, na.rm = T),
         COMIX_homeprop = COMIX_home / sum(COMIX_home, na.rm = T),
         COMIX_workprop = COMIX_work / sum(COMIX_work, na.rm = T),
         COMIX_schoolprop = COMIX_school / sum(COMIX_school, na.rm = T),
         COMIX_workschoolprop = COMIX_workschool / sum(COMIX_workschool, na.rm = T),
         COMIX_otherprop = COMIX_other / sum(COMIX_other, na.rm = T),
         COMIX_physicalprop = COMIX_physical / sum(COMIX_physical, na.rm = T),
         COMIX_totprop = COMIX_tot / sum(COMIX_tot, na.rm = T)) %>%
  select(-Sekara_S9prop, Sekara_S9prop)

head(DFcontactsprop)

#####################################
### Fitting powerlaw distribution ###
###   Using the poweRlaw package  ###
#####################################

# Using estimate_xmin
# Fit for 5 distributions : BBC total, home, work/school, other and Sekara.

# BBC_total
DF_BBC_total <- 
  DFcontactsprop %>%
  mutate(BBC_total = round(BBC_total, 0)) %>%
  select(n_gatherings, BBC_total) %>%
  filter(!is.na(BBC_total)) %>%
  as_tibble() %>%
  uncount(BBC_total) %>%
  pull(n_gatherings)
DF_BBC_total

# fit discrete power law
displ_BBC_total <- displ$new(DF_BBC_total)
est_BBC_total <- estimate_xmin(displ_BBC_total)

# show estimates
est_BBC_total$pars
est_BBC_total$xmin

# set xmin and get plot data
displ_BBC_total$setXmin(est_BBC_total)
plot.displ_BBC_total <- plot(displ_BBC_total, draw = F)
fit.displ_BBC_total <- lines(displ_BBC_total, draw = F)

# BBC_work
DF_BBC_work <- 
  DFcontactsprop %>%
  mutate(BBC_work = round(BBC_work, 0)) %>%
  select(n_gatherings, BBC_work) %>%
  filter(!is.na(BBC_work)) %>%
  as_tibble() %>%
  uncount(BBC_work) %>%
  pull(n_gatherings)
DF_BBC_work

# fit discrete power law
displ_BBC_work <- displ$new(DF_BBC_work)
est_BBC_work <- estimate_xmin(displ_BBC_work)

# set xmin and get plot data
displ_BBC_work$setXmin(est_BBC_work)
plot.displ_BBC_work <- plot(displ_BBC_work, draw = F)
fit.displ_BBC_work <- lines(displ_BBC_work, draw = F)

# BBC_other
DF_BBC_other <- 
  DFcontactsprop %>%
  mutate(BBC_other = round(BBC_other, 0)) %>%
  select(n_gatherings, BBC_other) %>%
  filter(!is.na(BBC_other)) %>%
  as_tibble() %>%
  uncount(BBC_other) %>%
  pull(n_gatherings)
DF_BBC_other

# fit discrete power law
displ_BBC_other <- displ$new(DF_BBC_other)
est_BBC_other <- estimate_xmin(displ_BBC_other)

# set xmin and get plot data
displ_BBC_other$setXmin(est_BBC_other)
plot.displ_BBC_other <- plot(displ_BBC_other, draw = F)
fit.displ_BBC_other <- lines(displ_BBC_other, draw = F)

# BBC_home
DF_BBC_home <- 
  DFcontactsprop %>%
  mutate(BBC_home = round(BBC_home, 0)) %>%
  select(n_gatherings, BBC_home) %>%
  filter(!is.na(BBC_home)) %>%
  as_tibble() %>%
  uncount(BBC_home) %>%
  pull(n_gatherings)
DF_BBC_home

# fit discrete power law
displ_BBC_home <- displ$new(DF_BBC_home)
est_BBC_home <- estimate_xmin(displ_BBC_home)

# set xmin and get plot data
displ_BBC_home$setXmin(est_BBC_home)
plot.displ_BBC_home <- plot(displ_BBC_home, draw = F)
fit.displ_BBC_home <- lines(displ_BBC_home, draw = F)

# Sekara_S9
DF_Sekara_S9 <- 
  DFcontactsprop %>%
  select(n_gatherings, Sekara_S9) %>%
  filter(!is.na(Sekara_S9)) %>%
  as_tibble() %>%
  uncount(Sekara_S9) %>%
  pull(n_gatherings)
DF_Sekara_S9

# fit discrete power law
displ_Sekara_S9 <- displ$new(DF_Sekara_S9)
est_Sekara_S9 <- estimate_xmin(displ_Sekara_S9)

# set xmin and get plot data
displ_Sekara_S9$setXmin(est_Sekara_S9)
plot.displ_Sekara_S9 <- plot(displ_Sekara_S9, draw = F)
fit.displ_Sekara_S9 <- lines(displ_Sekara_S9, draw = F)


###################################
### Generate Figure 3 a and 3 b ###
###################################

# My color palettes
my_col5 <- c("#FF0000", "#F2AD00", "#5BBCD6", "#F98400",  "#00A08A")
my_col9 <- c("#F2AD00", "#5BBCD6", "#F98400",  "#00A08A", "grey50", "grey55", "grey60", "grey65", "grey70")


## Figure 3a - PDF
plotFig3a <- ggplot(DFcontactsprop) +
  geom_point(aes(x = n_gatherings, y = Sekara_S9prop, colour = "CNS university students"), shape = 16, size = 1.5) +
  geom_point(aes(x = n_gatherings, y = BBC_homeprop, colour = "BBC home"), shape = 16, size = 1.5) +
  geom_point(aes(x = n_gatherings, y = BBC_workprop, colour = "BBC work/school"), shape = 16, size = 1.5) +
  geom_point(aes(x = n_gatherings, y = BBC_otherprop, colour = "BBC other"), shape = 16, size = 1.5) +
  geom_point(aes(x = n_gatherings, y = BBC_totalprop, colour = "BBC total"), shape = 16, size = 1.5) +
  scale_color_manual(values = my_col5,
                     limits = c("CNS university students", "BBC work/school", "BBC total", "BBC other", "BBC home")) +
  theme_pubr(base_size = 11, base_family = "Palatino") +
  theme(legend.position = c(0.8, 0.8), legend.background = element_blank(), legend.title=element_blank())+
  scale_x_log10() +
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  annotation_logticks() +
  labs(colour = "Data source") +
  ylab("Pr(K = k)") + xlab("Size of gatherings (k)") 
plotFig3a

## Figure 3b - CDF
plotFig3b <-  
  ggplot() +
  geom_point(data = plot.displ_BBC_total, aes(x = x, y = y, col = "BBC total"), shape = 16, size = 1.5) +
  geom_point(data = plot.displ_BBC_work, aes(x = x, y = y, col = "BBC work/school"), shape = 16, size = 1.5) +
  geom_point(data = plot.displ_BBC_other, aes(x = x, y = y, col = "BBC other"), shape = 16, size = 1.5) +
  geom_point(data = plot.displ_BBC_home, aes(x = x, y = y, col = "BBC home"), shape = 16, size = 1.5) +
  geom_point(data = plot.displ_Sekara_S9, aes(x = x, y = y, col = "CNS university students"), shape = 16, size = 1.5) +
  scale_color_manual(values = my_col5,
                     limits = c("CNS university students", "BBC work/school", "BBC total", "BBC other", "BBC home")) +
  theme_pubr(base_size = 11, base_family = "Palatino") +
  theme(legend.position = "none")+
  labs(colour = "Data source") +
  scale_x_log10() +
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  annotation_logticks() +
  ylab("Pr(K > k)") + xlab("Size of gatherings (k)")
plotFig3b

#########################
### Generate Figure 4 ###
#########################

# Generate Figure 4 CDF
# Generate geom text to put alpha and xmin values on Figure 3 
df_geomtext <- data.frame(x = c(0.1, 0.1, 0.1, 0.1, 0.1),
                          y = c(-8.5, -7.5, -9.5, -10.5, -6.5),
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
                          colour = c("BBC total", "BBC work/school", "BBC other", "BBC home", "CNS university students"))
df_geomtext$label <- paste0("alpha ==", format(round(df_geomtext$alpha, 2), nsmall = 2),
                            "~and~k[min] ==", round(df_geomtext$xmin, 2))

# Plot
plotFig4 <- 
  ggplot() +
  geom_point(data = plot.displ_BBC_total, aes(x = x, y = y, col = "BBC total"), alpha = 5/10, shape = 16) +
  geom_point(data = plot.displ_BBC_work, aes(x = x, y = y, col = "BBC work/school"), alpha = 5/10, shape = 16) +
  geom_point(data = plot.displ_BBC_other, aes(x = x, y = y, col = "BBC other"), alpha = 5/10, shape = 16) +
  geom_point(data = plot.displ_BBC_home, aes(x = x, y = y, col = "BBC home"), alpha = 5/10, shape = 16) +
  geom_point(data = plot.displ_Sekara_S9, aes(x = x, y = y, col = "CNS university students"), alpha = 5/10, shape = 16) +
  geom_line(data = fit.displ_BBC_total, aes(x = x, y = y, col = "BBC total"), size = 1.03) +
  geom_line(data = fit.displ_BBC_work, aes(x = x, y = y, col = "BBC work/school"), size = 1.03) +
  geom_line(data = fit.displ_BBC_other, aes(x = x, y = y, col = "BBC other"), size = 1.03) +
  geom_line(data = fit.displ_BBC_home, aes(x = x, y = y, col = "BBC home"), size = 1.03) +
  geom_line(data = fit.displ_Sekara_S9, aes(x = x, y = y, col = "CNS university students"), size = 1.03) +
  geom_text(data = df_geomtext, aes(x = 10^x, y = exp(y), label = label, col = colour), hjust = "left", parse = TRUE, show.legend = FALSE) +
  scale_color_manual(values = my_col5,
                     limits = c("CNS university students", "BBC work/school", "BBC total", "BBC other", "BBC home")) +
  theme_pubr(base_size = 11, base_family = "Palatino") +
  theme(legend.position = c(0.8, 0.8), legend.background = element_blank(), legend.title=element_blank())+
  labs(colour = "Data source") +
  scale_x_log10() +
  scale_y_continuous(
    labels = trans_format('log10', math_format(10^.x)),
    trans = scales::log10_trans()
  ) +
  annotation_logticks() +
  # scale_y_continuous(
  #   labels = scales::trans_format("exp", scales::math_format(.x))
  # ) +
  ylab("Pr(K > k)") + xlab("Size of gatherings (k)")
plotFig4


#########################
### Generate Figure 6 ###
#########################

plotFig6 <- 
  ggplot(
    DFcontactsprop %>%
      select(
        -Sekara_S9prop,
        -COMIX_physicalprop,
        -COMIX_workprop,
        -COMIX_schoolprop
      ) %>%
      pivot_longer(cols = ends_with("prop")) %>%
      separate(
        name,
        c("source", "variable"),
        sep = "_",
        remove = FALSE
      ) %>%
      mutate(
        variable = case_when(
          variable == "homeprop" ~ "Home",
          variable == "otherprop" ~ "Other",
          variable == "totalprop" |
            variable == "totprop" ~ "Total",
          variable == "workprop" |
            variable == "workschoolprop" ~ "Work/school"
        ),
        source = case_when(
          source == "BBC" ~ "BBC Pandemic",
          source == "COMIX" &
            variable == "Home" ~ "COMIX home",
          source == "COMIX" &
            variable == "Work/school" ~ "COMIX work/school",
          source == "COMIX" &
            variable == "Total" ~ "COMIX total",
          source == "COMIX" &
            variable == "Other" ~ "COMIX other",
        ),
        source = factor(
          source,
          levels = c(
            "BBC Pandemic",
            "COMIX work/school",
            "COMIX total",
            "COMIX other",
            "COMIX home"
          )
        ),
        variable = factor(variable, levels = c("Home",
                                               "Work/school",
                                               "Other",
                                               "Total"))
      )
  ) +
  facet_wrap( ~ variable, scales = "free") +
  scale_color_manual(values = c(
    alpha("grey75", 0.5),
    "#F2AD00",
    "#5BBCD6",
    "#F98400",
    "#00A08A"
  )) +
  theme_pubr(base_size = 10, base_family = "Palatino") +
  theme(legend.position = "none") +
  scale_x_log10() +
  scale_y_log10(
    breaks = trans_breaks("log10", function(x)
      10 ^ x),
    labels = trans_format("log10", math_format(10 ^ .x))
  ) +
  annotation_logticks() +
  geom_point(aes(x = n_gatherings, y = value, color = source),
             shape = 16,
             size = 1.5) +
  labs(colour = "Data source") +
  ylab("Pr(K = k)") + xlab("Size of gatherings (k)")
plotFig6








pdf("3_results/fig3a.pdf", width = 5, height = 4)
plotFig3a %>% print()
dev.off()

pdf("3_results/fig3b.pdf", width = 5, height = 4)
plotFig3b %>% print()
dev.off()

pdf("3_results/fig4.pdf", width = 6.5, height = 4.5)
plotFig4 %>% print()
dev.off()

pdf("3_results/fig6.pdf", width = 6.5, height = 5)
plotFig6 %>% print()
dev.off()


########################
### Generate Table 1 ###
########################

calc_moments <- function(pdfs) {
  pdfs %>%
    group_by(name) %>%
    summarise(
      M_1 = sum(n_gatherings * value, na.rm = TRUE),
      M_2 = sum(n_gatherings^2 * value, na.rm = TRUE),
      var = M_2 - M_1^2,
      q90 = first(n_gatherings[cumsum(value[!is.na(value)]) >= 0.90]), 
      q99 = first(n_gatherings[cumsum(value[!is.na(value)]) >= 0.99]), 
      min = min(n_gatherings[!is.na(value)], na.rm = TRUE),
      max = max(n_gatherings[!is.na(value)], na.rm = TRUE),
      .groups = 'drop'
    )
}

empirical_pdfs <- 
  DFcontactsprop %>% 
  select(-COMIX_physicalprop, -COMIX_workprop, -COMIX_schoolprop) %>%
  select(-COMIX_homeprop,
         -COMIX_otherprop,
         -COMIX_totprop, 
         -COMIX_workschoolprop) %>%
  pivot_longer(cols = ends_with("prop")) %>%
  select(n_gatherings, name, value)

calc_moments(empirical_pdfs) %>%
  select(-M_2) %>%
  add_column(
    N = DFcontactsprop %>% summarise(
      BBC_home = sum(BBC_home, na.rm = TRUE),
      BBC_other = sum(BBC_other, na.rm = TRUE),
      BBC_total = sum(BBC_total, na.rm = TRUE),
      BBC_work = sum(BBC_work, na.rm = TRUE),
      Sekara_S9 = sum(Sekara_S9, na.rm = TRUE)
    ) %>% t()  %>% as.vector(),
    .before = 'M_1'
  ) %>%
  knitr::kable(., format = "latex", digits = 1)


#####################################
### Fit alternative distributions ###
###   Using the poweRlaw package  ###
#####################################

# Fit for 5 distributions : BBC total, home, work/school, other and Sekara.

# BBC_total

# fit discrete log normal
dislnorm_BBC_total <- dislnorm$new(DF_BBC_total)
dislnorm_BBC_total$setXmin(4)
est_BBC_total <- estimate_pars(dislnorm_BBC_total)

# set pars and get plot data
dislnorm_BBC_total$setPars(est_BBC_total)
plot.dislnorm_BBC_total <- plot(dislnorm_BBC_total, draw = F)
fit.dislnorm_BBC_total <- lines(dislnorm_BBC_total, draw = F)

# BBC_work

# fit discrete log normal
dislnorm_BBC_work <- dislnorm$new(DF_BBC_work)
dislnorm_BBC_work$setXmin(13)
est_BBC_work <- estimate_pars(dislnorm_BBC_work)

# set pars and get plot data
dislnorm_BBC_work$setPars(est_BBC_work)
plot.dislnorm_BBC_work <- plot(dislnorm_BBC_work, draw = F)
fit.dislnorm_BBC_work <- lines(dislnorm_BBC_work, draw = F)

# BBC_other

# fit discrete log normal
dislnorm_BBC_other <- dislnorm$new(DF_BBC_other)
dislnorm_BBC_other$setXmin(8)
est_BBC_other <- estimate_pars(dislnorm_BBC_other)

# set pars and get plot data
dislnorm_BBC_other$setPars(est_BBC_other)
plot.dislnorm_BBC_other <- plot(dislnorm_BBC_other, draw = F)
fit.dislnorm_BBC_other <- lines(dislnorm_BBC_other, draw = F)

# BBC_home

# fit discrete log normal
dislnorm_BBC_home <- dislnorm$new(DF_BBC_home)
dislnorm_BBC_home$setXmin(4)
est_BBC_home <- estimate_pars(dislnorm_BBC_home)

# set pars and get plot data
dislnorm_BBC_home$setPars(est_BBC_home)
plot.dislnorm_BBC_home <- plot(dislnorm_BBC_home, draw = F)
fit.dislnorm_BBC_home <- lines(dislnorm_BBC_home, draw = F)

# Sekara_S9

# fit discrete log normal
dislnorm_Sekara_S9 <- dislnorm$new(DF_Sekara_S9)
dislnorm_Sekara_S9$setXmin(5)
est_Sekara_S9 <- estimate_pars(dislnorm_Sekara_S9)

# set pars and get plot data
dislnorm_Sekara_S9$setPars(est_Sekara_S9)
plot.dislnorm_Sekara_S9 <- plot(dislnorm_Sekara_S9, draw = F)
fit.dislnorm_Sekara_S9 <- lines(dislnorm_Sekara_S9, draw = F)


# Generate Figure A1 CDF
# Generate geom text to put mu and sd values on Figure 3 
df_geomtext <- data.frame(x = c(0.1, 0.1, 0.1, 0.1, 0.1),
                          y = c(-9, -8, -10, -11, -7),
                          mu = c(
                            est_BBC_total$pars[1],
                            est_BBC_work$pars[1],
                            est_BBC_other$pars[1],
                            est_BBC_home$pars[1],
                            est_Sekara_S9$pars[1]
                          ),
                          sd = c(
                            est_BBC_total$pars[[2]],
                            est_BBC_work$pars[[2]],
                            est_BBC_other$pars[[2]],
                            est_BBC_home$pars[[2]],
                            est_Sekara_S9$pars[[2]]
                          ), 
                          colour = c("BBC total", "BBC work/school", "BBC other", "BBC home", "CNS university students"))
df_geomtext$label <- paste0("mu ==", format(round(df_geomtext$mu, 2), nsmall = 2),
                            "~and~sigma ==", round(df_geomtext$sd, 2))

plotFigA1 <- 
  ggplot() +
  geom_point(data = plot.dislnorm_BBC_total, aes(x = x, y = y, col = "BBC total"), alpha = 5/10, shape = 16) +
  geom_point(data = plot.dislnorm_BBC_work, aes(x = x, y = y, col = "BBC work/school"), alpha = 5/10, shape = 16) +
  geom_point(data = plot.dislnorm_BBC_other, aes(x = x, y = y, col = "BBC other"), alpha = 5/10, shape = 16) +
  geom_point(data = plot.dislnorm_BBC_home, aes(x = x, y = y, col = "BBC home"), alpha = 5/10, shape = 16) +
  geom_point(data = plot.dislnorm_Sekara_S9, aes(x = x, y = y, col = "CNS university students"), alpha = 5/10, shape = 16) +
  geom_line(data = fit.dislnorm_BBC_total, aes(x = x, y = y, col = "BBC total"), size = 1.03) +
  geom_line(data = fit.dislnorm_BBC_work, aes(x = x, y = y, col = "BBC work/school"), size = 1.03) +
  geom_line(data = fit.dislnorm_BBC_other, aes(x = x, y = y, col = "BBC other"), size = 1.03) +
  geom_line(data = fit.dislnorm_BBC_home, aes(x = x, y = y, col = "BBC home"), size = 1.03) +
  geom_line(data = fit.dislnorm_Sekara_S9, aes(x = x, y = y, col = "CNS university students"), size = 1.03) +
  geom_text(data = df_geomtext, aes(x = 10^x, y = exp(y), label = label, col = colour), hjust = "left", parse = TRUE, show.legend = FALSE) +
  scale_color_manual(values = my_col5,
                     limits = c("CNS university students", "BBC work/school", "BBC total", "BBC other", "BBC home")) +
  theme_pubr(base_size = 11, base_family = "Palatino") +
  theme(legend.position = c(0.8, 0.8), legend.background = element_blank(), legend.title=element_blank())+
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
plotFigA1

pdf("3_results/lognormal.pdf", width = 6.5, height = 4.5)
plotFigA1 %>% print()
dev.off()


ln_df <- tibble(
  scenario1 = rlnorm(10000, 0, 0.5),
  scenario2 = rlnorm(10000, 0, 1),
  scenario3 = rlnorm(10000, 0, 2)
)

ln_df <-
  pivot_longer(ln_df, cols = everything()) %>%
  mutate(
    name = factor(name, labels = c(bquote(sigma == 0.5), bquote(sigma == 1) , bquote(sigma == 2)))
  )

pdf("3_results/ln_distribution.pdf", width = 6.5, height = 3)
ggplot(ln_df, aes(x = value)) +
  facet_wrap(~name, scales = "free", nrow = 1, labeller = label_parsed) +
  geom_histogram(aes(y = ..density..),
                 bins = 50,
                 color = "lightblue",
                 fill = "lightblue") +
  coord_cartesian(expand = FALSE, clip = "off") +
  # geom_vline(xintercept = 0.08, linetype = "dashed") +
  theme_classic(base_size = 10, base_family = "Palatino") +
  theme(strip.background = element_blank()) +
  labs(
    x = NULL,
    y = NULL
  )
dev.off()


#####################################
### Fit alternative distributions ###
###   Using the poweRlaw package  ###
#####################################

# Fit for 5 distributions : BBC total, home, work/school, other and Sekara.

# BBC_total

# fit discrete log normal
disexp_BBC_total <- disexp$new(DF_BBC_total)
est_BBC_total <- estimate_pars(disexp_BBC_total)

# set pars and get plot data
disexp_BBC_total$setPars(est_BBC_total)
plot.disexp_BBC_total <- plot(disexp_BBC_total, draw = F)
fit.disexp_BBC_total <- lines(disexp_BBC_total, draw = F)

# BBC_work

# fit discrete log normal
disexp_BBC_work <- disexp$new(DF_BBC_work)
est_BBC_work <- estimate_pars(disexp_BBC_work)

# set pars and get plot data
disexp_BBC_work$setPars(est_BBC_work)
plot.disexp_BBC_work <- plot(disexp_BBC_work, draw = F)
fit.disexp_BBC_work <- lines(disexp_BBC_work, draw = F)

# BBC_other

# fit discrete log normal
disexp_BBC_other <- disexp$new(DF_BBC_other)
est_BBC_other <- estimate_pars(disexp_BBC_other)

# set pars and get plot data
disexp_BBC_other$setPars(est_BBC_other)
plot.disexp_BBC_other <- plot(disexp_BBC_other, draw = F)
fit.disexp_BBC_other <- lines(disexp_BBC_other, draw = F)

# BBC_home

# fit discrete log normal
disexp_BBC_home <- disexp$new(DF_BBC_home)
est_BBC_home <- estimate_pars(disexp_BBC_home)

# set pars and get plot data
disexp_BBC_home$setPars(est_BBC_home)
plot.disexp_BBC_home <- plot(disexp_BBC_home, draw = F)
fit.disexp_BBC_home <- lines(disexp_BBC_home, draw = F)

# Sekara_S9

# fit discrete log normal
disexp_Sekara_S9 <- disexp$new(DF_Sekara_S9)
est_Sekara_S9 <- estimate_pars(disexp_Sekara_S9)

# set pars and get plot data
disexp_Sekara_S9$setPars(est_Sekara_S9)
plot.disexp_Sekara_S9 <- plot(disexp_Sekara_S9, draw = F)
fit.disexp_Sekara_S9 <- lines(disexp_Sekara_S9, draw = F)



# Generate Figure A1 CDF
# Generate geom text to put mu and sd values on Figure 3 
df_geomtext <- data.frame(x = c(0.1, 0.1, 0.1, 0.1, 0.1),
                          y = c(-9, -8, -10, -11, -7),
                          lambda = c(
                            est_BBC_total$pars[1],
                            est_BBC_work$pars[1],
                            est_BBC_other$pars[1],
                            est_BBC_home$pars[1],
                            est_Sekara_S9$pars[1]
                          ),
                          colour = c("BBC total", "BBC work/school", "BBC other", "BBC home", "CNS university students"))
df_geomtext$label <- paste0("lambda ==", format(round(df_geomtext$lambda, 2), nsmall = 2))

plotFigA2 <- 
  ggplot() +
  geom_point(data = plot.disexp_BBC_total, aes(x = x, y = y, col = "BBC total"), alpha = 5/10, shape = 16) +
  geom_point(data = plot.disexp_BBC_work, aes(x = x, y = y, col = "BBC work/school"), alpha = 5/10, shape = 16) +
  geom_point(data = plot.disexp_BBC_other, aes(x = x, y = y, col = "BBC other"), alpha = 5/10, shape = 16) +
  geom_point(data = plot.disexp_BBC_home, aes(x = x, y = y, col = "BBC home"), alpha = 5/10, shape = 16) +
  geom_point(data = plot.disexp_Sekara_S9, aes(x = x, y = y, col = "CNS university students"), alpha = 5/10, shape = 16) +
  geom_line(data = fit.disexp_BBC_total, aes(x = x, y = y, col = "BBC total"), size = 1.03) +
  geom_line(data = fit.disexp_BBC_work, aes(x = x, y = y, col = "BBC work/school"), size = 1.03) +
  geom_line(data = fit.disexp_BBC_other, aes(x = x, y = y, col = "BBC other"), size = 1.03) +
  geom_line(data = fit.disexp_BBC_home, aes(x = x, y = y, col = "BBC home"), size = 1.03) +
  geom_line(data = fit.disexp_Sekara_S9, aes(x = x, y = y, col = "CNS university students"), size = 1.03) +
  geom_text(data = df_geomtext, aes(x = 10^x, y = exp(y), label = label, col = colour), hjust = "left", parse = TRUE, show.legend = FALSE) +
  scale_color_manual(values = my_col5,
                     limits = c("CNS university students", "BBC work/school", "BBC total", "BBC other", "BBC home")) +
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
plotFigA2

library(VGAM)

gofstat(list(
  fitdist(DF_Sekara_S9, "lnorm", discrete = TRUE),
  fitdist(DF_Sekara_S9, "nbinom", discrete = TRUE),
  fitdist(DF_Sekara_S9, "llogis", start = list(location = 1, scale = 500), discrete = TRUE),
  fitdist(DF_Sekara_S9, "gamma", discrete = TRUE)
), fitnames = c("lnorm", "nbinom", "llogis", "gamma"))

gofstat(list(
  fitdist(DF_BBC_total, "lnorm"),
  fitdist(DF_BBC_total, "nbinom"),
  fitdist(DF_BBC_total, "llogis", start = list(location = 1, scale = 500)),
  fitdist(DF_BBC_total, "gamma")
  # fitdist(DF_BBC_total, "pareto", start = list(shape = 1, scale = 500))
), fitnames = c("lnorm", "nbinom", "llogis", "gamma"))


fitdist(DF_Sekara_S9, "pldis", start = list("xmin" = 5, "alpha" = 2))



plot(displ_Sekara_S9)
lines(displ_Sekara_S9)
lines(dislnorm_Sekara_S9, col = "red")
lines(pllogis(1:315, location = 1.6953054, scale = 0.5, lower.tail = F), col = "blue")




# compare distributions ---------------------------------------------------

# discrete power law ------------------------------------------------------

# Fit for 5 distributions : BBC total, home, work/school, other and Sekara.

# BBC_total

# fit discrete discrete power law
displ_BBC_total <- displ$new(DF_BBC_total)
displ_BBC_total$setXmin(5)
est_BBC_total <- estimate_pars(displ_BBC_total)

# set pars and get plot data
displ_BBC_total$setPars(est_BBC_total)
plot.displ_BBC_total <- plot(displ_BBC_total, draw = F)
fit.displ_BBC_total <- lines(displ_BBC_total, draw = F)

# BBC_work

# fit discrete discrete power law
displ_BBC_work <- displ$new(DF_BBC_work)
displ_BBC_work$setXmin(5)
est_BBC_work <- estimate_pars(displ_BBC_work)

# set pars and get plot data
displ_BBC_work$setPars(est_BBC_work)
plot.displ_BBC_work <- plot(displ_BBC_work, draw = F)
fit.displ_BBC_work <- lines(displ_BBC_work, draw = F)

# BBC_other

# fit discrete discrete power law
displ_BBC_other <- displ$new(DF_BBC_other)
displ_BBC_other$setXmin(5)
est_BBC_other <- estimate_pars(displ_BBC_other)

# set pars and get plot data
displ_BBC_other$setPars(est_BBC_other)
plot.displ_BBC_other <- plot(displ_BBC_other, draw = F)
fit.displ_BBC_other <- lines(displ_BBC_other, draw = F)

# BBC_home

# fit discrete discrete power law
displ_BBC_home <- displ$new(DF_BBC_home)
displ_BBC_home$setXmin(5)
est_BBC_home <- estimate_pars(displ_BBC_home)

# set pars and get plot data
displ_BBC_home$setPars(est_BBC_home)
plot.displ_BBC_home <- plot(displ_BBC_home, draw = F)
fit.displ_BBC_home <- lines(displ_BBC_home, draw = F)

# Sekara_S9

# fit discrete log normal
displ_Sekara_S9 <- displ$new(DF_Sekara_S9)
displ_Sekara_S9$setXmin(5)
est_Sekara_S9 <- estimate_pars(displ_Sekara_S9)

# set pars and get plot data
displ_Sekara_S9$setPars(est_Sekara_S9)
plot.displ_Sekara_S9 <- plot(displ_Sekara_S9, draw = F)
fit.displ_Sekara_S9 <- lines(displ_Sekara_S9, draw = F)



# lognormal ---------------------------------------------------------------

# BBC_total

# fit discrete log normal
dislnorm_BBC_total <- dislnorm$new(DF_BBC_total)
dislnorm_BBC_total$setXmin(5)
est_BBC_total <- estimate_pars(dislnorm_BBC_total)

# set pars and get plot data
dislnorm_BBC_total$setPars(est_BBC_total)
plot.dislnorm_BBC_total <- plot(dislnorm_BBC_total, draw = F)
fit.dislnorm_BBC_total <- lines(dislnorm_BBC_total, draw = F)

# BBC_work

# fit discrete log normal
dislnorm_BBC_work <- dislnorm$new(DF_BBC_work)
dislnorm_BBC_work$setXmin(5)
est_BBC_work <- estimate_pars(dislnorm_BBC_work)

# set pars and get plot data
dislnorm_BBC_work$setPars(est_BBC_work)
plot.dislnorm_BBC_work <- plot(dislnorm_BBC_work, draw = F)
fit.dislnorm_BBC_work <- lines(dislnorm_BBC_work, draw = F)

# BBC_other

# fit discrete log normal
dislnorm_BBC_other <- dislnorm$new(DF_BBC_other)
dislnorm_BBC_other$setXmin(5)
est_BBC_other <- estimate_pars(dislnorm_BBC_other)

# set pars and get plot data
dislnorm_BBC_other$setPars(est_BBC_other)
plot.dislnorm_BBC_other <- plot(dislnorm_BBC_other, draw = F)
fit.dislnorm_BBC_other <- lines(dislnorm_BBC_other, draw = F)

# BBC_home

# fit discrete log normal
dislnorm_BBC_home <- dislnorm$new(DF_BBC_home)
dislnorm_BBC_home$setXmin(5)
est_BBC_home <- estimate_pars(dislnorm_BBC_home)

# set pars and get plot data
dislnorm_BBC_home$setPars(est_BBC_home)
plot.dislnorm_BBC_home <- plot(dislnorm_BBC_home, draw = F)
fit.dislnorm_BBC_home <- lines(dislnorm_BBC_home, draw = F)

# Sekara_S9

# fit discrete log normal
dislnorm_Sekara_S9 <- dislnorm$new(DF_Sekara_S9)
dislnorm_Sekara_S9$setXmin(5)
est_Sekara_S9 <- estimate_pars(dislnorm_Sekara_S9)

# set pars and get plot data
dislnorm_Sekara_S9$setPars(est_Sekara_S9)
plot.dislnorm_Sekara_S9 <- plot(dislnorm_Sekara_S9, draw = F)
fit.dislnorm_Sekara_S9 <- lines(dislnorm_Sekara_S9, draw = F)


# conduct test ------------------------------------------------------------


vuong <- list(
  compare_distributions(dislnorm_BBC_home, displ_BBC_home),
  compare_distributions(dislnorm_BBC_work, displ_BBC_work),
  compare_distributions(dislnorm_BBC_other, displ_BBC_other),
  compare_distributions(dislnorm_BBC_total, displ_BBC_total),
  compare_distributions(dislnorm_Sekara_S9, displ_Sekara_S9)
)

lapply(vuong, get, x = "test_statistic") 
lapply(vuong, get, x = "p_two_sided") 

       plot(vuong[[1]])
pdata <- lapply(vuong, function(x) plot(x))

ggplot(bind_rows(pdata, .id = "source"), aes(x = x, y = y)) +
  geom_point()

