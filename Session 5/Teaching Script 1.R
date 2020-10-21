

# LESSON 1 ----------------------------------------------------------------

library(tidyverse)
library(lubridate)

set.seed(24072020)

#VECTORS

x <- 10
x

h <- "Hello_World"
print(h)

class(x)
class(h)

x <- as.integer(x)
h <- as.factor(h)

class(x)
class(h)

#using c

ten <- c(1:10)
ten[3] == 3
ten[3] == 4

mean(ten)
sum(ten)
sd(ten)

#rnorm(number of values, mean, SD)
norm <- rnorm(10000, 10, 1)
class(norm)

students <- c('Callum', "Nik", "Hattie", "Nathan")
low <- tolower(students)
high <- toupper(students)
length(students)
levels_of_student <- as.factor(students)


#tidyverse functions
str_length(students)
ordered_students <- str_sort(students)
ordered_students <- str_sort(students, decreasing = TRUE)

#DATAFRAMES - make and merge

ID <- sample(1:100, 100)
Hb <- rnorm(100, 150, 10)
WCC <- rep(4:13, 10)
Time_in_hosp <- rpois(100, lambda = 4)
Date_of_admission <- Sys.Date() + sort(sample(1:100, 100))
Ward <-
  rep(c("Frensham", "Clandon", "Ewhurst", "Bramshott", "Compton"), 20)

Hb_table <- tibble(ID, Hb)
WCC_table <- tibble(ID, WCC)
TIH_table <- tibble(ID, Time_in_hosp)
DOA_table <- tibble(ID, Date_of_admission)
Ward_table <- tibble(ID, Ward)

test_df <-
  list(Hb_table, WCC_table, TIH_table, DOA_table, Ward_table) %>% reduce(full_join, by = "ID")

same_test_df <-
  tibble(ID, Hb, WCC, Time_in_hosp, Date_of_admission, Ward)

test_df_ordered <- test_df[order(test_df$ID), ]
#trailing comma means to take every column, we could specify certain columns instead

test_df <- test_df %>%
  mutate(Hb_dec = Hb / 10)

anaemic_pts <- test_df %>%
  filter(Hb_dec < 14) %>%
  group_by(Ward) %>%
  nest

# LESSON 2 ----------------------------------------------------------------


test_df %>% arrange(Hb_dec)
anaemic_pts <- unnest(anaemic_pts)
anaemic_pts %>% arrange(desc(Hb_dec), .by_group = TRUE)
anaemic_pts %>% arrange(desc(Hb_dec), .by_group = FALSE)

ward_Hb <- summarise(anaemic_pts, mean_Hb = mean(Hb, na.rm = TRUE))

id_hb <- test_df %>% select(ID, contains("Hb")) %>%
  rename(RSCH_ID = ID)

ggplot(data = Hb_table) +
  geom_point(aes(x = ID, y = Hb)) +
  geom_smooth(aes(x = ID, y = Hb))
new
hospital_occupancy <- read.csv("hospital_in_out.csv")
hospital_occupancy$new_date <-
  as.POSIXct(hospital_occupancy$new_date, tz = "", "%d/%m/%Y %H:%M")
example1 <-
  hospital_occupancy %>% filter(new_date >= as.POSIXct("2020-01-01 00:00:00" , tz = "")) %>%
  select(new_date, COVID, total, PALLIATIVE.MEDICINE, GENERAL.MEDICINE)

example1_long <- gather(
  example1,
  key = Specialty,
  value = Beds,
  COVID:GENERAL.MEDICINE,
  factor_key = FALSE
)

example1_long %>%
  ggplot(aes(x = new_date, y = Beds, colour = Specialty)) +
  geom_line() +
  geom_smooth() +
  facet_wrap( ~ Specialty, nrow = 2)

anaemic_pts %>%
  ggplot(aes(x = Ward)) +
  geom_bar()


# LESSON 3 ----------------------------------------------------------------
library(epitools)

spec_date_covid <-
  hospital_occupancy %>% filter(Date_value == "27/05/2020") %>%
  summarise(mean(COVID))

daily_admissions <- read.csv("daily_admissions.csv")
daily_admissions$Admission.Date <-
  as.Date(daily_admissions$Admission.Date, format = "%d/%m/%Y")
daily_admissions2 <-
  daily_admissions %>% select(Admission.Date,
                              Discharge_Main_Specialty_Desc,
                              ICD10.Short,
                              Gender_Desc,
                              Age.Group) %>%
  filter(Age.Group != "0-4",
         Age.Group != "Oct-14")
daily_admissions_COVID <-
  daily_admissions2 %>% filter(Discharge_Main_Specialty_Desc == "COVID")#

daily_admissions2$Discharge_Main_Specialty_Desc <-
  ifelse(
    daily_admissions2$Discharge_Main_Specialty_Desc != "COVID",
    "NON-COVID",
    daily_admissions2$Discharge_Main_Specialty_Desc
  )

oddsratio(
  daily_admissions2$Gender_Desc,
  daily_admissions2$Discharge_Main_Specialty_Desc,
  rev = "columns"
)
chisq.test(daily_admissions2$Discharge_Main_Specialty_Desc,
           daily_admissions2$Gender_Desc)

# LESSON 4 ----------------------------------------------------------------
library(EpiStats)
library(markdown)
library(knitr)

smok_dta <- read.csv("test_smoking.csv")
smok_pub <-
  CCInter(smok_dta,
          cases = "lung_ca",
          exposure = "chem",
          by = "smok")
smok_pub2 <-
  CCInter(smok_dta,
          cases = "lung_ca",
          exposure = "smok",
          by = "chem")

kable(smok_pub$df1)
kable(smok_pub2$df1)

#cleaning and refactoring data
daily_admissions2$Age.Group <-
  as.factor(daily_admissions2$Age.Group)
daily_admissions2$Age.Group <-
  fct_relevel(daily_admissions2$Age.Group, "100-104", after = Inf)
daily_admissions2$Age.Group <-
  as.numeric(daily_admissions2$Age.Group)

#1 = 10-15
#18 = 100-104

#recoding as computer language

daily_admissions2$Discharge_Main_Specialty_Desc <-
  ifelse(daily_admissions2$Discharge_Main_Specialty_Desc != "COVID",
         0,
         1)

daily_admissions2$Gender_Desc <-
  ifelse(daily_admissions2$Gender_Desc != "MALE",
         0,
         1)

covid_mh <- CCInter(
  daily_admissions2,
  cases = "Discharge_Main_Specialty_Desc",
  exposure = "Gender_Desc",
  by = "Age.Group",
  table = TRUE
)

daily_admissions2$Age.Group <-
  as.factor(daily_admissions2$Age.Group)

covid_logit <-
  glm(
    Discharge_Main_Specialty_Desc ~ Gender_Desc + Age.Group,
    data = daily_admissions2,
    family = "binomial"
  )

summary(covid_logit)
exp(cbind(OR = coef(covid_logit), confint(covid_logit)))

#how can we improve this model?
#think about it and tell me the answer next session

# LESSON 5 ----------------------------------------------------------------

pi <- seq(0,1, by=0.00001)
died <- 4
surv <- 6

f_likelihood <- ((pi)^died)*((1-pi)^surv)

likelihood_model <- tibble(pi, f_likelihood)

max(f_likelihood)
likelihood_model %>% filter(f_likelihood == max(f_likelihood))

ggplot(data = likelihood_model) +
  geom_line(aes(x = pi, y = f_likelihood))

likelihood_model <- likelihood_model %>% mutate(l_r = (f_likelihood/max(f_likelihood)))

ggplot(data = likelihood_model) +
  geom_line(aes(x = pi, y = l_r)) +
  geom_hline(yintercept = 0.1465) +
  geom_vline(xintercept = 0.4)

#now change the total sample size to 100, but keep the ratio of F to S the same

f_likelihood <- ((pi)^20)*((1-pi)^30)

likelihood_model <- tibble(pi, f_likelihood)

max(f_likelihood)
likelihood_model %>% filter(f_likelihood == max(f_likelihood))

ggplot(data = likelihood_model) +
  geom_line(aes(x = pi, y = f_likelihood))

likelihood_model <- likelihood_model %>% mutate(l_r = (f_likelihood/max(f_likelihood))) 
max_lr_pi <- likelihood_model %>% filter(l_r == max(l_r)) %>% summarise(pi) %>% unlist

likelihood_model %>% ggplot(aes(x = pi, y = l_r)) +
  geom_line() +
  geom_hline(yintercept = 0.1465) +
  geom_vline(xintercept = max_lr_pi)

