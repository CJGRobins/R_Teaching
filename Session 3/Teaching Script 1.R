
# LESSON 1 ----------------------------------------------------------------

setwd("~/R Model/R Teaching")
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
Ward <- rep(c("Frensham", "Clandon", "Ewhurst", "Bramshott", "Compton"), 20)

Hb_table <- tibble(ID, Hb)
WCC_table <- tibble(ID, WCC)
TIH_table <- tibble(ID, Time_in_hosp)
DOA_table <- tibble(ID, Date_of_admission)
Ward_table <- tibble(ID, Ward)

test_df <-
  list(Hb_table, WCC_table, TIH_table, DOA_table, Ward_table) %>% reduce(full_join, by = "ID")

same_test_df <- tibble(ID, Hb, WCC, Time_in_hosp, Date_of_admission, Ward)

test_df_ordered <- test_df[order(test_df$ID),]
#trailing comma means to take every column, we could specify certain columns instead

test_df <- test_df %>%
  mutate(Hb_dec = Hb/10) 

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
  geom_point(aes(x=ID, y=Hb)) +
  geom_smooth(aes(x=ID, y=Hb))
new
hospital_occupancy <- read.csv("hospital_in_out.csv")
hospital_occupancy$new_date <- as.POSIXct(hospital_occupancy$new_date, tz = "", "%d/%m/%Y %H:%M")
example1 <- hospital_occupancy %>% filter(new_date >= as.POSIXct("2020-01-01 00:00:00" , tz = "")) %>%
  select(new_date, COVID, total, PALLIATIVE.MEDICINE, GENERAL.MEDICINE)

example1_long <- gather(example1,
                        key = Specialty,
                        value = Beds,
                        COVID:GENERAL.MEDICINE,
                        factor_key = FALSE)

example1_long %>%
  ggplot(aes(x = new_date, y = Beds, colour = Specialty)) +
  geom_line() +
  geom_smooth() +
  facet_wrap(~Specialty, nrow = 2)

anaemic_pts %>%
  ggplot(aes(x = Ward)) +
  geom_bar()


# LESSON 3 ----------------------------------------------------------------
library(epitools)

spec_date_covid <- hospital_occupancy %>% filter(Date_value == "27/05/2020") %>%
  summarise(sum(COVID))

daily_admissions <- read.csv("daily_admissions.csv")
daily_admissions$Admission.Date <- as.Date(daily_admissions$Admission.Date, format = "%d/%m/%Y")
daily_admissions2 <- daily_admissions %>% select(Admission.Date, Discharge_Main_Specialty_Desc, ICD10.Short, Gender_Desc, Age.Group)
daily_admissions_COVID <- daily_admissions2 %>% filter(Discharge_Main_Specialty_Desc == "COVID")#

daily_admissions2$Discharge_Main_Specialty_Desc <-
  ifelse(
    daily_admissions2$Discharge_Main_Specialty_Desc != "COVID",
    "NON-COVID",
    daily_admissions2$Discharge_Main_Specialty_Desc
  )

oddsratio(daily_admissions2$Gender_Desc, daily_admissions2$Discharge_Main_Specialty_Desc, rev = "columns")
chisq.test(daily_admissions2$Discharge_Main_Specialty_Desc, daily_admissions2$Gender_Desc)