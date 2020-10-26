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
hospital_occupancy <- read.csv("Datasets/hospital_in_out.csv")
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

daily_admissions <- read.csv("Datasets/daily_admissions.csv")
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

smok_dta <- read.csv("Datasets/test_smoking.csv")
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

likelihood_model %>% filter(f_likelihood == max(f_likelihood))

ggplot(data = likelihood_model) +
  geom_line(aes(x = pi, y = f_likelihood))

likelihood_model <- likelihood_model %>% mutate(l_r = (f_likelihood/max(f_likelihood)))

max_lr_pi <- likelihood_model %>% filter(l_r == max(l_r)) %>% summarise(pi) %>% unlist

likelihood_model %>% ggplot(aes(x = pi, y = l_r)) +
  geom_line() +
  geom_hline(yintercept = 0.1465) +
  geom_vline(xintercept = max_lr_pi)

max_likelihood <- likelihood_model %>%
  filter(f_likelihood == max(f_likelihood)) %>%
  summarise(f_likelihood) %>%
  rename(Maximum_Likelihood = f_likelihood) %>%
  round(digits = 10) %>%
  unlist

lower_limit <- likelihood_model %>% 
  filter(pi < max_lr_pi) %>%
  filter(abs(l_r - 0.1465) == min(abs(l_r - 0.1465))) %>%
  summarise(pi) %>%
  rename(Lower_Limit = pi) %>%
  unlist

upper_limit <- likelihood_model %>% 
  filter(pi > max_lr_pi) %>%
  filter(abs(l_r - 0.1465) == min(abs(l_r - 0.1465))) %>%
  summarise(pi) %>%
  rename(Upper_Limit = pi) %>%
  unlist 

null_pi <- 0.5

print(c(max_lr_pi, lower_limit, upper_limit, max_likelihood))


# LESSON 6 ----------------------------------------------------------------

likelihood_model <- likelihood_model %>% mutate(log_likelihoods = ((died*log(pi))+(surv*log(1-pi))))
ggplot(data = likelihood_model, aes(x = pi, y = log_likelihoods)) +
  geom_line() +
  xlim(0.05, 0.95) +
  ylim(-20,0) 

#that looks like shit
#let's use the ratio

likelihood_model <- likelihood_model %>% mutate(llr = (log_likelihoods)-max(log_likelihoods))
ggplot(data = likelihood_model, aes(x = pi, y = llr)) +
  geom_line() + 
  xlim(0.05, 0.95) +
  ylim(-6,0) +
  geom_hline(yintercept = log(0.1465))
  
#add in S

P <- died/(died+surv)
S <- sqrt((P*(1-P))/(died+surv)) 
likelihood_model <- likelihood_model %>% mutate(approx_llr = (-0.5*(((died/(died+surv))-pi)/S)^2))

ggplot(data = likelihood_model) +
  geom_line(aes(x = pi, y = llr, colour = "Log Likelihood Ratio")) + 
  xlim(0.05, 0.95) +
  ylim(-6,0) +
  geom_hline(yintercept = log(0.1465)) +
  geom_line(aes(x = pi, y = approx_llr, colour = "Approximate LLR")) +
  labs(colour = "LLR")

logit1 <- glm(lung_ca ~ smok, family = "binomial", data = smok_dta)

summary(logit1)
exp(coef(logit1))
oddsratio(smok_dta$smok, smok_dta$lung_ca)

library(lmtest)

lrtest(logit1) #putting only a single var logistic regression model into the lrtest compares against the null

logit_conf <- glm(lung_ca ~ smok + chem, family = "binomial", data = smok_dta)
summary(logit_conf)
exp(coef(logit_conf))

lrtest(logit1, logit_conf)

#does this match up with the ccinter command from before?

logit_em <- glm(lung_ca ~ smok*chem, family = "binomial", data = smok_dta)
summary(logit_em)
exp(coef(logit_em))

#is there any effect modification? which direction?

lrtest(logit_conf, logit_em)

#is the effect modification any different to the confounding model? 
#Yes, but only by 0.1 LL, which is nothing. Unless you have a good reason don't classify something as an effect modifier!!!

# LESSON 7 ----------------------------------------------------------------
#review the last single adjusted model and asses the SE of the main coefficient and the beta value
summary(logit_conf)

#you'll need to install these three packages, haven imports STATA dta files, sjstats gives simple commands for model evaluation
#foreach is a looping function you can use on anything
library(sjstats)
library(haven)
library(foreach)

#import data
uganda_lrti_first <- read_csv("Datasets/uganda.lrti.first.csv")

#create your first a priori model of association and examine it
apmodel <- glm(lrti ~ malaria + magegp + hhsesgp + sex, family = "binomial", data = uganda_lrti_first)
summary(apmodel)
rmse(apmodel)
exp(coef(apmodel))

q_confound <- c("hookworm", "trichuris", "hivchild", "mparity", "meduc", "fuel", "wall", "elec", "location")

models <- lapply(q_confound, function(x) {
  glm(substitute(lrti ~ sex + magegp + hhsesgp + malaria + i, list(i = as.name(x))), family = "binomial", data = uganda_lrti_first)
})
summaries <- lapply(models, summary)
rootmses <- lapply(models, rmse)
foreach(i = 1:length(models)) %do% {
  print(exp(coef(models[[i]])))
  print(summaries[[i]])
  print(rootmses[[i]])
  subset_lrti <- uganda_lrti_first[complete.cases(uganda_lrti_first[,q_confound[i]]),]
  subset_apmodel <- glm(lrti ~ malaria + magegp + hhsesgp + sex, family = "binomial", data = subset_lrti)
  print(lrtest(subset_apmodel, models[[i]]))
}

# LESSON 8 ----------------------------------------------------------------

#Simulate Vaccine Efficacy

r0 <- seq(1,10, by = 0.001)
fraction_vac <- (1-(1/r0))
vac_eff <- 0.8
adjusted_fracvac <- fraction_vac/vac_eff
vaccine <- tibble(r0, fraction_vac, adjusted_fracvac)
ggplot(vaccine) +
  geom_line(aes(x = r0, y = fraction_vac, colour = "100% efficacy")) +
  geom_line(aes(x = r0, y = adjusted_fracvac, colour = "80% efficacy")) +
  geom_hline(yintercept = 1) +
  labs(colour = "Vaccine Efficacy")
 #the r0 where the adjusted_fracvac = 1 is where the disease can't be controlled by 
 #a vaccine with that efficacy alone

library(EpiModel)
#to simulate a deterministic model in epimodel, use the dcm function
#first of all though set up the parameters and initial conditions

SEIR <- function(t, t0, parms) {
  with(as.list(c(t0, parms)), {
    
    # Population size based on the sizes of each of the resevoirs
    num <- s.num + e.num + i.num + r.num
    
    # Effective contact rate and force of infection (lambda = beta*i) from a rearrangement of Beta * c * D
    ce <- R0 / i.dur
    lambda <- ce * i.num/num
    
    #dX is just the rate of change of X
    dS <- -lambda*s.num #the infected people at the next time step = FOI * number of susceptibles
    dE <- lambda*s.num - (1/e.dur)*e.num #The rate of movement out of pre-infectiousness is 1/duration in days - seems simple right?
    dI <- (1/e.dur)*e.num - (1 - cfr)*(1/i.dur)*i.num - cfr*(1/i.dur)*i.num #cfr here is the death of patients,
    #if patients are dying they shouldn't be able to affect the force of infection
    dR <- (1 - cfr)*(1/i.dur)*i.num
    
    # Compartments and flows are part of the derivative vector
    # Other calculations to be output are outside the vector, but within the containing list
    list(c(dS, dE, dI, dR, 
           se.flow = lambda * s.num,
           ei.flow = (1/e.dur) * e.num,
           ir.flow = (1 - cfr)*(1/i.dur) * i.num,
           d.flow = cfr*(1/i.dur)*i.num),
         num = num,
         i.prev = i.num / num, #prevalence of infectious people
         ei.prev = (e.num + i.num)/num) #prevalence of infectED people
  })
}

#Here you set the parameters of your model, based on the natural history of the disease
#by putting there to be 3 values of r0 the model will run three times based on these parameters
#any number of these can be changed and 
param <- param.dcm(R0 = 3.5, e.dur = 10, i.dur = 14, cfr = c(0.4,0.6,0.8))

#The initial conditions vary based on the population you're in
#This is a simple model so doesn't include young and old, and different beta values for them
#Therefore takes the bold assumption that the population is equally susceptible and mixes homogeneously
init <- init.dcm(s.num = 1e6, e.num = 10, i.num = 0, r.num = 0,
                 se.flow = 0, ei.flow = 0, ir.flow = 0, d.flow = 0)

#create the steering tools of your deterministic compartmental model
control <- control.dcm(nsteps = 3000, dt = 1, new.mod = SEIR)

#run the DCM with the params inits and controls
mod <- dcm(param, init, control)
mod

par(mfrow = c(1, 2))
plot(mod, y = "i.num", main = "Number Infected")
plot(mod, y = "i.prev", main = "Percent Infected", ylim = c(0, 0.5), legend = "full")

#doesn't repeat itself because there's no input of new susceptibles (no new births!)

#genetic analysis

#question always comes down to is a genetic bit (transcriptome, rna, epigenetic, gene etc) 
#more prevalent in our cases than out controls?


#git hub
#git commit
#git push


