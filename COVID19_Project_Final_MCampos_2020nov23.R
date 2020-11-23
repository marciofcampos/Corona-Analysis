library(tidyverse)
library(dslabs)
library(ggplot2)
library(caret)
library(lubridate)
library(e1071)
library(dplyr)
library(corrplot)
library(data.table)
library(readr)
library(matrixStats)
library(xgboost)
library(rpart)

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(dslabs)) install.packages("dslabs", repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(lubridate)) install.packages("lubridate", repos = "http://cran.us.r-project.org")
if(!require(e1071)) install.packages("e1071", repos = "http://cran.us.r-project.org")
if(!require(dplyr)) install.packages("dplyr", repos = "http://cran.us.r-project.org")
if(!require(corrplot)) install.packages("corrplot", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(readr)) install.packages("readr", repos = "http://cran.us.r-project.org")
if(!require(matrixStats)) install.packages("matrixStats", repos = "http://cran.us.r-project.org")
if(!require(xgboost)) install.packages("xgboost", repos = "http://cran.us.r-project.org")
if(!require(rpart)) install.packages("rpart", repos = "http://cran.us.r-project.org")

#Data csv from COVID19 - São Paulo State, Brazil - 26set2020. File number 1.
#"https://opendatasus.saude.gov.br/dataset/casos-nacionais"

#downloading the dataset.
set.seed(1, sample.kind = "Rounding") 
covid_full <- read_csv2("dados-sp-1.csv", locale(encoding = "ISO-8859-1"), col_names = TRUE, col_types = NULL)

#checking the target variable
levels(as.factor(covid_full$evolucaoCaso))
covid_full %>% group_by(evolucaoCaso) %>% summarize(n = n())

#filtering the target variable
covid_full <-covid_full %>% filter(evolucaoCaso == "Cura" | evolucaoCaso == "Óbito") 
mean(covid_full$evolucaoCaso == "Óbito")
#prevalence here is 0,28%

## Due to imbalancing of the outcome "Case evolution", lets downsample the dataset
covid_full <- mutate_if(covid_full, is.character, as.factor) 
covid_file <- downSample(covid_full[,!colnames(covid_full) %in% "evolucaoCaso"] , y = covid_full$evolucaoCaso) 
covid_file <-  covid_file %>% mutate( evolucaoCaso = Class) %>% select(-Class)

#saving the dataset
write_csv2(covid_file, "covid.csv", append = FALSE, col_names = TRUE)
rm(covid_full, covid_file)

#creating the dataset for the report

#downloading from Github
urlds <- "https://raw.githubusercontent.com/marciofcampos/Corona-Analysis/master/covid.csv"

#Create the dataset "covid"
covid <- read_csv2(url(urlds), locale(encoding = "UTF-8"), col_names = TRUE, col_types = NULL )

### DATA TIDYING

#checking the structure of the data set
str(covid)

#Selecting only the required columns
covid <- covid %>% select(dataNotificacao, dataInicioSintomas, sintomas, 
                          condicoes, estadoTeste, dataTeste, tipoTeste, 
                          resultadoTeste, sexo, municipioNotificacao, 
                          idade, evolucaoCaso, classificacaoFinal)  

#Changing the column names to english
colnames(covid) <- c("notdate", "symtomdate", "symtom", "conditions", 
                     "teststatus", "testdate", "testtype","testres", 
                     "gender", "city", "age", "evolution", "finalclass") 

#the structure after selecting the desired columns
head(covid)

#check the target variable - case evolution

levels(as.factor(covid$evolution))

covid$evolution <- str_replace_all(covid$evolution, 
                                   c("Cura" = "survive",
                                   "Óbito" = "death")) %>%
  as.factor()

#proportion of the target variable - numberof deaths

death_bar <- round(mean(covid$evolution == "death"), 4)
death_se_hat <- round((sqrt(death_bar*(1-death_bar)/nrow(covid))), 4) # estimated standard error
death_me <- 2*death_se_hat*100 #margin of error
ci <- c(death_bar + 1.96*death_se_hat, death_bar - 1.96*death_se_hat) # 95% Confidence Interval

#Number of cures and deaths
table(covid$evolution) %>% knitr::kable()

#tyding the timestamp of notification date column, by separating the date and time: 
datetime <- covid$notdate
datetime <- as.data.frame(datetime)%>%separate(datetime, c("date", "time"), sep = "T")
datetime$date <- as_datetime(datetime$date)
datetime <- datetime %>% mutate(notweek = week(date)) #Splitting the week
covid <- covid %>% mutate(notdate = datetime$date, notweek = datetime$notweek) %>%
  filter(!is.na(notdate)) #Replacing the dates in the dataset

#tyding the timestamp of symtom date column, by separating the date and time: 
datetime <- covid$symtomdate 
datetime <- as.data.frame(datetime)%>%separate(datetime, c("date", "time"), sep = "T")
datetime$date <- as_datetime(datetime$date)
covid <- covid %>% mutate(symtomdate = datetime$date) %>%
  filter(!is.na(symtomdate)) #Replacing the dates in the dataset


#tyding the timestamp of test date column, by separating the date and time: 
datetime <- covid$testdate 
datetime <- as.data.frame(datetime)%>%separate(datetime, c("date", "time"), sep = "T")
datetime$date <- as_datetime(datetime$date)
covid <- covid %>% mutate(testdate = datetime$date) %>%
  filter(!is.na(testdate)) #Replacing the dates in the dataset


# Filtering in the column test status only concluded tests.
levels(as.factor(covid$teststatus))
covid <- covid %>% filter(teststatus == "Concluído") %>%
  select(-teststatus)

#Translating the testresults. 
levels(as.factor(covid$testres))
covid$testres <- str_replace_all(covid$testres, 
                                 c("Positivo" = "positive", 
                                   "Negativo" = "negative")) %>% 
  as.factor()

#type of tests
levels(as.factor(covid$testtype))

#Filtering and Translating the testtype. The RT-PCR was not traslated.
covid$testtype <- str_replace_all(covid$testtype, 
        c("TESTE RÁPIDO - ANTÍGENO" = "RT-antigen", 
          "TESTE RÁPIDO - ANTICORPO" = "RT-antibody",
          "Enzimaimunoensaio - ELISA IgM" = "ELISA",
          "Imunoensaio por Eletroquimioluminescência \023 ECLIA" = "ECLIA",
          "Imunoensaio por Eletroquimioluminescência - ECLIA IgG" = "ECLIA",
          "Quimioluminescência - CLIA" = "CLIA")) %>% as.factor()

#number of each test
covid %>% group_by(covid$testtype) %>% 
  summarize(n = n()) %>% 
  knitr::kable() 


#removing the tests with low samples
covid <- covid %>% filter(testtype == "RT-PCR" | testtype == "RT-antibody" | testtype == "RT-antigen")


#Levels of the column gender
levels(as.factor(covid$gender))

#Translating the gender.
covid$gender <- str_replace_all(covid$gender, 
                                c("Masculino" = "male", 
                                  "Feminino" = "female")) %>% 
  as.factor()


#filtering the age to remove entries higher than 90 years old and lower than 0.
covid <- covid %>% filter(age < 91 & age > -1)

#What are the symtoms
levels(as.factor(covid$symtom)) %>% head(20)

#Translate the symtoms
covid$symtom <- str_replace_all(covid$symtom, 
                                c("Tosse" = "cought",
                                  "Febre" = "fever", 
                                  "Dor de Garganta" = "sore_throat",
                                  "Dor de Cabeça" = "headache",
                                  "Dispneia" = "dyspnoea",
                                  "Distúrbios Gustativos" = "taste_disorder",
                                  "Distúrbios Olfativos" = "olfactory_disorder",
                                  "Coriza" = "runny_nose",
                                  "Assintomático" = "asymtomatic",
                                  "Outros" = "others"))

#Splitting the symtoms in separated columns and filtering "NAs" entries
covid <- covid %>% mutate(cought = str_detect(covid$symtom,"cought"),
                      fever = str_detect(covid$symtom,"fever"), 
                      sorethroat = str_detect(covid$symtom,"sore_throat"),
                      headache = str_detect(covid$symtom,"headache"),
                      dyspnoea = str_detect(covid$symtom,"dyspnoea"),
                      tastedisorder = str_detect(covid$symtom,"taste_disorder"),
                      olfactorydisorder = str_detect(covid$symtom,"olfactory_disorder"),
                      runnynose = str_detect(covid$symtom,"runny_nose"),
                      asymtomatic = str_detect(covid$symtom, "asymtomatic"),
                      others = str_detect(covid$symtom,"others")) %>%
  filter(!is.na(symtom))

#creating the column "symtom" with symtoms in the alfabetic order
covid$symtom <- sapply(strsplit(covid$symtom, ","), function(x){
  paste(sort(trimws(x)), collapse = ", ")
})

#Analysing the column "finalclass", final classification.
levels(as.factor(covid$finalclass))

#translating the classification
covid$finalclass <- str_replace_all(covid$finalclass, 
                                c("Confirmação Laboratorial" = "confirmed", 
                                  "Confirmado Clínico-Epidemiológico" = "confirmed",
                                  "Confirmado Clínico-Imagem" = "confirmed",
                                  "Confirmado Laboratorial" = "confirmed",
                                  "Confirmado por Critério Clínico" = "confirmed",
                                  "Descartado" = "discarted",
                                  "Síndrome Gripal Não Especificada" = "discarted")) %>%
  as.factor() 

covid <- covid %>% filter(!is.na(finalclass))

#checking the patient previous disease
levels(as.factor(covid$conditions)) %>% head(10)

#translating the previous conditions
covid <- covid %>% filter(!is.na(conditions))
covid$conditions <- str_replace_all(covid$conditions, 
      c("Diabetes" = "diabetes",
        "Doenças cardíacas crônicas" = "heart_disease", 
        "Doenças renais crônicas em estágio avançado \\(graus 3, 4 ou 5\\)" = 
          "kidney_disease",
        "Doenças respiratórias crônicas descompensadas" = "respiratory_disease",
        "Imunossupressão" = "immunodeficiency",
        "Portador de doenças cromossômicas ou estado de fragilidade imunológica" = 
          "immunodeficiency",
        "Obesidade" = "weight",
        "Gestante" = "pregnant",
        "null" = "no_previous_conditions"))

#creating the column "conditions" with the previous diseases or conditions in the alfabetic order
covid$conditions <- sapply(strsplit(covid$conditions, ","), function(x){
  paste(sort(trimws(x)), collapse = ", ")
})

#organized in alfabetic order
levels(as.factor(covid$conditions)) %>% head(10)


#Change the class of the predictors from character to factor, and from logical and integer to numeric

covid <- mutate_if(covid, is.character, as.factor) 
covid <- mutate_if(covid, is.logical, as.numeric) 
covid <- mutate_if(covid, is.integer, as.numeric)


#Checking again the number of recoveries and deaths
table(covid$evolution) %>% knitr::kable()

#creating the workset
workset <- covid

### DATA ANALYSIS


#analysis by gender
n_female <- workset %>% filter(gender == "female") %>% nrow() #amount of female
n_male <- workset %>% filter(gender == "male") %>% nrow() #amount of male
p_female <- round((n_female / nrow(workset)*100), 0)  #pecent of female
p_male <- round((n_male / nrow(workset)*100), 0)  #percent of male

#creating a data frame with the distribution by gender.
gender_sp<- data.frame( c(n_female, p_female), c(n_male, p_male))
colnames(gender_sp) <- c("Female", "Male")
rownames(gender_sp) <- c("number of cases [un]", "percentage [%]")
gender_sp %>% knitr::kable() 

#checking the evolution of the COVID19 for each gender:
workset %>% group_by(gender) %>%
  summarize(death = round(mean(evolution == "death"), 2), 
            survive = round(mean(evolution =="survive"), 2)) %>%
  knitr::kable()


#analysis by age

#notifications by age
workset %>% 
  group_by(age) %>%
  mutate( n = length(age)) %>%
  ggplot(aes(age, n)) +
  geom_point() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  ggtitle("Notifications by Age") +
  ylab("Number of notifications") +
  xlab("Age")


#evolution by age 
workset %>% group_by(testres) %>%
  ggplot(aes(age, y = ..count.., colour = evolution)) +
  geom_density(alpha = 0.2) +
  ggtitle("Disease Evolution by Age") +
  labs(color="Evolution") +
  ylab("Number of notifications") +
  xlab("Age")

#proportion of deaths between 0 and 50 years old.
prop_young <- round(((workset %>% filter(age > 0 & age < 50 & evolution == "death") %>% nrow())/
                       (workset %>% filter(age > 0 & age < 50) %>% nrow())), 2) 

#proportion of positive cases over 50 years old.
prop_old <- round(((workset %>% filter(age > 49 & evolution == "death") %>% nrow())/
                     (workset %>% filter(age > 49) %>% nrow())), 2) 

#odds ratio
inc <- round(prop_old/prop_young, 1)
#odds ratio of deaths in older people against youngers

#analysing gender and age together:

#distribution of age by gender
workset %>%  ggplot(aes(age, y=..count.. , fill=gender)) +
  geom_density(alpha=0.2) +
  ggtitle("Distribution by Age and Gender") +
  ylab("Number of notifications") +
  xlab("Age") +
  labs(fill="Gender")


#distribution of disease evolution by age and gender

workset %>% ggplot(aes(age, y=..count.. , fill = evolution)) +
  geom_density(alpha=0.2) +
  ggtitle("Distribution of Females") +
  ylab("Number of notifications") +
  xlab("Age") +
  labs(fill="Evolution") +
  facet_grid(gender~.) 

#proportion of deaths in males older than 50: 
d_males <- workset %>% filter(gender == "male" & age > 50) %>% 
  summarize(mean = mean(evolution == "death")) %>% round(2)

#proportion of deaths in females older than 50: 
d_females <- workset %>% filter(gender == "female" & age > 50) %>% 
  summarize(mean = mean(evolution == "death")) %>% round(2)


#analysing the type of test

#how many tests per type
workset %>% ggplot(aes(testtype)) + 
  geom_bar(stat = "count") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  ggtitle("Type of Test") +
  xlab("Test Type")

#disease evolution per test type:
workset %>%  
  group_by(testtype) %>%
  ggplot(aes(testtype, fill = evolution)) +
  geom_bar(stat = "count", show.legend = TRUE) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  ggtitle("Results by Test Type") +
  ylab("Number of notifications") +
  xlab("Test Type") +
  labs(fill="Disease Evolution")

#Table format
workset %>% group_by(testtype) %>%
  summarize(death = sum(evolution == "death"), survive = sum(evolution == "survive")) %>% 
  knitr::kable()

# checking the column test result:

#test result related to disease evolution and test type
workset %>% ggplot(aes(testres, fill =  evolution)) +
  geom_histogram(binwidth = 10, stat = "count") +
  xlab("Test Result") +
  ylab("Number of Notifications") +
  labs(fill="Disease Evolution") +
  ggtitle("Disease Evolution per Test Type") +
  facet_grid(testtype~.)


#Analysing the simtoms

#proportion of appearance for each symtom
workset %>% gather(symtoms, appearance, `cought`:`others`, na.rm = TRUE) %>%
  select(symtoms, appearance, testres, evolution) %>%
  group_by(symtoms) %>%
  summarize(proportion = round((mean(appearance)), 2)) %>%
  arrange(desc(proportion)) %>% 
  knitr::kable()

#number of each symtom combination
workset %>% select(symtom) %>% 
  filter(!is.na(symtom)) %>%
  group_by(symtom) %>% 
  summarize( n = n()) %>% 
  arrange(desc(n)) %>%
  head(20) %>% 
  knitr::kable()

#relation between symtom and evolution of disease
workset %>% gather(symtoms, appearance, `cought`:`others`, na.rm = TRUE) %>%
  select(symtoms, appearance, evolution) %>%
  group_by(symtoms, evolution) %>% 
  summarize(n = sum(appearance == 1)) %>%
  ggplot(aes(evolution, symtoms, color = symtoms, size = n)) +
  geom_point(show.legend = FALSE) +
  ggtitle("Disease evolution by Symtoms") +
  ylab("Symtoms") +
  xlab("Disease Evolution")


#table format
workset %>% gather(symtoms, appearance, `cought`:`others`, na.rm = TRUE) %>%
  select(symtoms, appearance, evolution) %>%
  group_by(symtoms, evolution) %>%
  summarize(n = sum(appearance == 1)) %>%
  knitr::kable()
#cought fever and dyspnoea indicates more deaths than survivals


#top 20 symtom combinations with higest death proportion
workset %>% select(symtom, evolution) %>%
  group_by(symtom) %>%
  summarize(n= n(), deathprop = round(mean(evolution =="death"), 2)) %>%
  arrange(desc(deathprop)) %>% 
  head(20) %>%
  knitr::kable()

#Filtering dyspnoea, cought and fever, compared with gender and age
workset %>% select(evolution, gender, age, dyspnoea, cought, fever) %>%
  filter(dyspnoea == 1 & cought == 1 & fever == 1) %>%
  group_by(evolution, age, gender) %>%
  summarize(n= n()) %>%
  ggplot(aes(evolution, age, color = gender)) +
  geom_boxplot() +
  ggtitle("Effect of dyspnoea, cought and fever") + 
  ylab("Age") +
  xlab("Disease Evolution") + 
  labs(fill = "Gender")

# Analysing the timeline

# Notifications per week
workset %>% group_by(notweek) %>% 
  mutate(n = length(notweek)) %>%
  ggplot(aes(notweek, n)) + 
  geom_point(na.rm = TRUE) +
  geom_smooth(method = "loess", span = 0.3, method.args = list(degree = 1)) +
  ggtitle("Notification per Week") +
  ylab("Number of notifications") +
  xlab("Notification week")

#Distance between symtom and test.
workset %>% mutate(distance = as.numeric(difftime(testdate, symtomdate, unit="days"))) %>%
  group_by(distance) %>%
  filter(distance < 50 & distance > 0) %>%
  ggplot(aes(distance, fill = testtype)) +
  geom_histogram(binwidth = 1) +
  ggtitle("Distance between first symtom and test") +
  ylab("Number of notifications") +
  xlab("Days after first symtom") +
  labs(fill = "Test Type")

#Distance between symtom and test considering the disease evolution
workset %>% mutate(distance = as.numeric(difftime(testdate, symtomdate, unit="days"))) %>%
  group_by(distance) %>%
  filter(distance < 20 & distance > -1) %>%
  ggplot(aes(distance, fill = evolution)) +
  geom_histogram(binwidth = 1) +
  ggtitle("Distance between symtom and result - RT-antibody") +
  ylab("Number of notifications") +
  xlab("Days after first symtom") +
  labs(fill="Disease Evolution") +
  facet_grid(testtype~.)

# variation of the proportion of deaths by distance
workset %>%  mutate(distance = as.numeric(difftime(testdate, symtomdate, unit="days"))) %>%
  filter(distance < 20 & distance > -1) %>%
  group_by(distance, testtype) %>%
  summarize(proportion = round((mean(evolution =="death")), 2)) %>%
  ggplot(aes(distance, proportion, testtype), alpha = 0.2) +
  geom_point() +
  geom_smooth(method = "lm") +
  ggtitle("Proportion of Deaths per Distance - RT-antibody") +
  xlab("Days after first symtom") +
  facet_grid(testtype~.)


#Analysing the city

#distribution of the number of cases and cities
workset %>% group_by(city) %>% 
  summarize(n = length(city)) %>%
  arrange(desc(n)) %>%
  ggplot(aes(n)) +
  geom_histogram(binwidth = 10) +
  ggtitle("Distribution of Cases") +
  ylab("Number of Cities") +
  xlab("Number of Cases") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#distribution of the number of cases and cities, less than 400 cases
workset %>% group_by(city) %>% 
  summarize(n = length(city)) %>%
  filter(n < 400) %>%
  arrange(desc(n)) %>%
  ggplot(aes(n)) +
  geom_histogram(binwidth = 1) +
  ggtitle("Distribution of Cases") +
  ylab("Number of Cities") +
  xlab("Number of Cases") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#20 cities with more cases
workset %>% group_by(city) %>%
  summarize(n = length(city))  %>% 
  arrange(desc(n)) %>% 
  head(20) %>% 
  knitr::kable() 


#20 cities with more deaths
workset %>% select(city, evolution) %>%
  group_by(city) %>%
  summarize(cases = n(),
            deaths = sum(evolution == "death"), 
            deathprop = round(mean(evolution == "death"), 2)) %>%
  arrange(desc(deaths)) %>% 
  head(20) %>% 
  knitr::kable()

#20 cities with more cases and evolution of the disease.
cities <- workset %>% select(city, testres, evolution) %>%
  group_by(city) %>%
  summarize(n = length(city), 
         prop_death = round(mean(evolution =="death"), 2),
         prop_pos = round(mean(testres =="positive"),2)) %>%
  arrange(desc(n)) %>%
  head(20) 
cities %>% knitr::kable()

#correlation between the number of cases and the proportion of positive cases
round(cor(cities$n, cities$prop_pos), 3)

#correlation between the number of cases and the proportion of deaths
round(cor(cities$n, cities$prop_death), 3)

#checking the previous conditions of the patient

#disease evolution related with previous conditions
workset %>% mutate(commorbidities = ifelse(conditions =="no_previous_conditions", "no", "yes")) %>%
  group_by(commorbidities) %>%
  summarize(proportion_of_death = mean(evolution =="death") %>% round(2), 
            proportion_of_survival = mean(evolution =="survive") %>% round(2)) %>%
  knitr::kable()
  
#Appearance of each commorbities combination
workset %>% select(conditions) %>% 
  group_by(conditions) %>% 
  summarize( n = n()) %>% 
  arrange(desc(n)) %>%
  head(10) %>% 
  knitr::kable()

#commorbities combination versus COVID19 evolution
workset %>% select(conditions, evolution) %>% 
  group_by(conditions) %>% 
  summarize(n = n(), 
            prop_death = mean(evolution =="death") %>% round(2), 
            prop_survival = mean(evolution =="survive") %>% round(2)) %>%
  arrange(desc(prop_death)) %>%
  filter(n > 10) %>%
  head(10) %>%
  knitr::kable()


#checking the final classification of each case.
workset %>% select(testres, finalclass) %>%
  mutate(testres = ifelse(testres=="positive", "1_positive", "2_negative")) %>%
  table() %>% 
  knitr::kable()

#table with cases definition
cmtable <- data.frame(c("positive","negative"), c("TP", "FN"), c("FP", "TN"))
colnames(cmtable) <- c("", "confirmed", "discarted")
cmtable %>% knitr::kable()

#calculating the accuracy, sensitivity, specificity and precision for each test
workset %>% select(testres, testtype, finalclass) %>%
  group_by(testtype) %>%
  summarize(accuracy = round(((sum(finalclass =="confirmed" & testres=="positive") + 
          sum(finalclass =="discarted" & testres =="negative"))/length(testtype)), 4),
            sensitivity = round((sum(finalclass =="confirmed" & testres=="positive")/ 
                                 sum(finalclass =="confirmed")), 4),
            specificity = round((sum(finalclass =="discarted" & testres=="negative")/ 
                                 sum(finalclass =="discarted")), 4),
            precision = round((sum(finalclass =="confirmed" & testres=="positive")/ 
                               sum(testres =="positive")), 4)) %>%
  knitr::kable()

#evolution od the cases according to the final classification
workset %>%  
  group_by(finalclass) %>%
  ggplot(aes(finalclass, fill = evolution)) +
  geom_bar(stat = "count", show.legend = TRUE) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  ggtitle("Evolution by Final Classification ") +
  ylab("Number of notifications") +
  xlab("Final Classification") +
  labs(fill="Disease Evolution")


# check the association/correlation between the binary predictors
#select only columns with binary information
chworkset <- workset %>% select(-notdate, -symtomdate, -symtom, -conditions, -testdate, 
                                -testtype, -city, -age, - finalclass, -notweek)

#change factors to numeric
chworkset <- mutate_if(chworkset, is.factor, as.numeric)

#chisq test and phi calculation
chisq <- matrix(1, 13, 5)
name <- colnames(chworkset)
for(i in 1:13){
  tab <- table(chworkset$evolution, as.matrix(chworkset[,i]))
  mat <- matrix(as.numeric(tab), nrow = nrow(tab), ncol = ncol(tab))
  info <- chisq.test(mat, correct = FALSE)
  chisq[i, 1] <- "evolution"
  chisq[i, 2] <- name[i]
  chisq[i, 3] <- info$p.value
  chisq[i, 4] <- sqrt(info$statistic/nrow(chworkset))
  chisq[i, 5] <- cor(chworkset$evolution, chworkset[,i])
}
colnames(chisq) <- c("target", "feature", "p-value", "phi-coef", "pearson")
chisq %>% knitr::kable()


#checking and removing non common features
colMeans(chworkset) %>% knitr::kable()
chworkset <- chworkset %>% select(-headache, -tastedisorder, -olfactorydisorder, 
                                  -runnynose, -asymtomatic)

#correlation for binary predictors approximated by pearson.
T <- cor(chworkset)
corrplot::corrplot(cor(T), method = "circle", order = "hclust", addrect = 2, tl.col = "black", 
                   hclust.method = "average")


#Correlation Matrix for non binary predictors
T <- mutate_if(workset, is.factor, as.numeric)
T <- T %>% select(symtom, conditions, testtype, city, age, notweek, evolution)
corrplot::corrplot(cor(T), method = "circle", order = "hclust", addrect = 2,  tl.col = "black", 
                   hclust.method = "average")


#removing predictors not correlated with the target variable
workset <- workset %>% select(-notdate, -symtomdate, -symtom, -testdate, -city, -notweek,
                              -headache, -tastedisorder, -olfactorydisorder, -runnynose,
                              -asymtomatic, -others, -finalclass)


#TRAIN and TESTSET

#Creating the train and test sets
set.seed(1, sample.kind="Rounding")
test_index <- createDataPartition(y = workset$evolution, times = 1, p = 0.5, list = FALSE)
trainset <- workset[-test_index,]
testset <- workset[test_index,]

#filtering the test set 
testset <- testset %>% 
  semi_join(trainset, by = "conditions") %>%
  semi_join(trainset, by = "testtype") %>%
  semi_join(trainset, by = "testres") %>%
  semi_join(trainset, by = "gender") %>%
  semi_join(trainset, by = "age") %>%
  semi_join(trainset, by = "evolution") %>%
  semi_join(trainset, by = "cought") %>%
  semi_join(trainset, by = "fever") %>%
  semi_join(trainset, by = "sorethroat") %>%
  semi_join(trainset, by = "dyspnoea")

#splitting the predictors and the target variable
testset.y <- testset$evolution
testset.x <- testset %>% select(-evolution)

### MODEL FITTING

# 1 - Guessing the outcome 
set.seed(1, sample.kind = "Rounding")
y_hat_guess <- sample(c("death", "survive"), nrow(testset), replace = TRUE) %>% 
  factor(levels = levels(testset.y))
acc <- confusionMatrix(y_hat_guess, testset.y)$overall["Accuracy"] %>% round(4)
sens <- confusionMatrix(y_hat_guess, testset.y)$byClass["Sensitivity"] %>% round(4)
spec <- confusionMatrix(y_hat_guess, testset.y)$byClass["Specificity"] %>% round(4)
prec <- confusionMatrix(y_hat_guess, testset.y)$byClass["Precision"] %>% round(4)
f1 <- confusionMatrix(y_hat_guess, testset.y)$byClass["F1"] %>% round(4)

#creating an output table
test_results <- tibble(method = "Guessing", Accuracy = acc, Sensitivity = sens, Specificity = spec,
                       Precision = prec, F1 = f1)
test_results %>% knitr::kable()

#2 - logistic regession with more correlated predictors: age and dyspnoea
set.seed(1, sample.kind = "Rounding")
fit_glm <- glm(evolution ~ age + dyspnoea, data = trainset, family = "binomial")
p_hat_glm <- predict(fit_glm, testset.x, type = "response")
y_hat_glm <- ifelse(p_hat_glm < 0.5, "death", "survive") %>% factor(levels = levels(testset.y))
acc <- confusionMatrix(y_hat_glm, testset.y)$overall["Accuracy"] %>% round(4)
sens <- confusionMatrix(y_hat_glm, testset.y)$byClass["Sensitivity"] %>% round(4)
spec <- confusionMatrix(y_hat_glm, testset.y)$byClass["Specificity"] %>% round(4)
prec <- confusionMatrix(y_hat_glm, testset.y)$byClass["Precision"] %>% round(4)
f1 <- confusionMatrix(y_hat_glm, testset.y)$byClass["F1"] %>% round(4)


#updating the output table
test_results <- bind_rows(test_results,
                          data_frame(method="Logistic Regression with Age and Dyspnoea ", 
                                     Accuracy = acc, Sensitivity = sens, Specificity = spec, 
                                     Precision = prec, F1 = f1))
test_results[2,] %>% knitr::kable()


#3 - logistic regession with  all predictors.
set.seed(1, sample.kind = "Rounding")
fit_glm_all <- glm(evolution~., data = trainset, family = "binomial")
p_hat_glm_all <- predict(fit_glm_all, testset.x, type = "response")
y_hat_glm_all <- ifelse(p_hat_glm_all<0.5,"death","survive") %>% factor(levels=levels(testset.y))
acc <- confusionMatrix(y_hat_glm_all, testset.y)$overall["Accuracy"] %>% round(4)
sens <- confusionMatrix(y_hat_glm_all, testset.y)$byClass["Sensitivity"] %>% round(4)
spec <- confusionMatrix(y_hat_glm_all, testset.y)$byClass["Specificity"] %>% round(4)
prec <- confusionMatrix(y_hat_glm_all, testset.y)$byClass["Precision"] %>% round(4)
f1 <- confusionMatrix(y_hat_glm_all, testset.y)$byClass["F1"] %>% round(4)

#updating the output table
test_results <- bind_rows(test_results,
                          data_frame(method="Logistic Regression with All Features", 
                                     Accuracy = acc, Sensitivity = sens, Specificity = spec, 
                                     Precision = prec, F1 = f1))
test_results[3,] %>% knitr::kable()


# 4 - KNN  
set.seed(1, sample.kind = "Rounding")
knn_fit <- train(evolution~., method = "knn" , data = trainset) 
y_hat_knn <- predict(knn_fit, testset.x, type = "raw")
acc <- confusionMatrix(y_hat_knn, testset.y)$overall["Accuracy"] %>% round(4)
sens <- confusionMatrix(y_hat_knn, testset.y)$byClass["Sensitivity"] %>% round(4)
spec <- confusionMatrix(y_hat_knn, testset.y)$byClass["Specificity"] %>% round(4)
prec <- confusionMatrix(y_hat_knn, testset.y)$byClass["Precision"] %>% round(4)
f1 <- confusionMatrix(y_hat_knn, testset.y)$byClass["F1"] %>% round(4)

#updating the output table
test_results <- bind_rows(test_results,
                          data_frame(method="K-Nearest Neighbors", 
                                     Accuracy = acc, Sensitivity = sens, Specificity = spec, 
                                     Precision = prec, F1 = f1))
test_results[4,] %>% knitr::kable()         

#5 - Cross Validation to choose the best k parameter
set.seed(1, sample.kind = "Rounding")
control <- trainControl(method = "cv", number = 10, p = .9)
knn_fit <- train(evolution~., method = "knn", data = trainset, 
                 tuneGrid = data.frame(k = seq(6, 30, 2)),
                 trControl = control)
ggplot(knn_fit, highlight = TRUE) # k = 30 is the best choice
knn_fit$bestTune #30

y_hat_knn_cv<- predict(knn_fit, testset.x, type = "raw")
acc <- confusionMatrix(y_hat_knn_cv, testset.y)$overall["Accuracy"] %>% round(4)
sens <- confusionMatrix(y_hat_knn_cv, testset.y)$byClass["Sensitivity"] %>% round(4)
spec <- confusionMatrix(y_hat_knn_cv, testset.y)$byClass["Specificity"] %>% round(4)
prec <- confusionMatrix(y_hat_knn_cv, testset.y)$byClass["Precision"] %>% round(4)
f1 <- confusionMatrix(y_hat_knn_cv, testset.y)$byClass["F1"] %>% round(4)

#updating the output table
test_results <- bind_rows(test_results,
                          data_frame(method="K-Nearest Neighbors Cross Validation", 
                                     Accuracy = acc, Sensitivity = sens, Specificity = spec, 
                                     Precision = prec, F1 = f1))
test_results[5,] %>% knitr::kable()


#6 - Classification Tree (rpart)
set.seed(1, sample.kind = "Rounding")
fit_tree <- rpart(evolution ~ ., data = trainset, 
                  control = rpart.control(cp = 0.008, minsplit = 28), method ="class")
p_hat_tree <- as.data.frame(predict(fit_tree, testset.x)) 
y_hat_tree <- p_hat_tree %>%mutate(pred = ifelse(death > 0.5, "death", "survive")) %>% 
  select(pred)
y_hat_tree <- as.matrix(y_hat_tree) 
y_hat_tree <- as.factor(y_hat_tree)
plot(fit_tree, margin = 0.1)
text(fit_tree, cex = 0.6)

#calculating the output parameters
acc <- confusionMatrix(y_hat_tree, testset.y)$overall["Accuracy"] %>% round(4)
sens <- confusionMatrix(y_hat_tree, testset.y)$byClass["Sensitivity"] %>% round(4)
spec <- confusionMatrix(y_hat_tree, testset.y)$byClass["Specificity"] %>% round(4)
prec <- confusionMatrix(y_hat_tree, testset.y)$byClass["Precision"] %>% round(4)
f1 <- confusionMatrix(y_hat_tree, testset.y)$byClass["F1"] %>% round(4)

#updating the output table
test_results <- bind_rows(test_results,
                          data_frame(method="Classification Tree - rpart()", 
                                     Accuracy = acc, Sensitivity = sens, Specificity = spec, 
                                     Precision = prec, F1 = f1))
test_results[6,] %>% knitr::kable()

#7 - Classification Tree (Caret)
set.seed(1, sample.kind = "Rounding")
fit_tree_caret <- train(evolution ~ ., data = trainset, method = "rpart", 
                  tuneGrid = data.frame(cp = seq(0, 0.05, 0.001)), 
                  control = rpart.control(minsplit = 20, minbucket = 7))
ggplot(fit_tree_caret)
fit_tree_caret$bestTune 
y_hat_tree_caret <- predict(fit_tree_caret, testset.x)
plot(fit_tree_caret$finalModel, margin = 0.1)
text(fit_tree_caret$finalModel, cex = 0.75)

#calculating the output parameters
acc <- confusionMatrix(y_hat_tree_caret, testset.y)$overall["Accuracy"] %>% round(4)
sens <- confusionMatrix(y_hat_tree_caret, testset.y)$byClass["Sensitivity"] %>% round(4)
spec <- confusionMatrix(y_hat_tree_caret, testset.y)$byClass["Specificity"] %>% round(4)
prec <- confusionMatrix(y_hat_tree_caret, testset.y)$byClass["Precision"] %>% round(4)
f1 <- confusionMatrix(y_hat_tree_caret, testset.y)$byClass["F1"] %>% round(4)

#updating the output table
test_results <- bind_rows(test_results,
                          data_frame(method="Classification Tree - caret", 
                                     Accuracy = acc, Sensitivity = sens, Specificity = spec, 
                                     Precision = prec, F1 = f1))
test_results[7, ] %>% knitr::kable()

#8 - XGBoost algorithm

# splitting the target variable, train and testset
trainset <- mutate_if(trainset, is.factor, as.numeric)
testset <- mutate_if(testset, is.factor, as.numeric)
train.x <- trainset %>% select(-evolution)
train.y <- trainset %>% select(evolution) %>% mutate(evolution = as.numeric(evolution))
test.x <- testset %>% select(-evolution) 
test.y <- testset %>% select(evolution) %>% mutate(evolution = as.numeric(evolution))

#change the labels in binary data
train.y$evolution <- ifelse(train.y$evolution == 1, 0, 1)
test.y$evolution <- ifelse(test.y$evolution == 1, 0, 1)

#fitting the model
set.seed(1, sample.kind = "Rounding")
fit_xgboost <- xgboost(data = as.matrix(sapply(train.x, as.numeric)),
                       label = as.matrix(train.y),
                       max.depth = 4, 
                       nrounds = 7,
                       early_stopping_rounds = 5, 
                       objective = "reg:logistic",
                       eta = 0.4,
                       gamma = 3)
p_hat_xgboost <- predict(fit_xgboost, as.matrix(sapply(test.x, as.numeric)))
y_hat_xgboost <- ifelse(p_hat_xgboost<0.5,"death","survive") %>% factor(levels=levels(testset.y))

#calculating theoutput parameters
acc <- confusionMatrix(y_hat_xgboost, testset.y)$overall["Accuracy"] %>% round(4)
sens <- confusionMatrix(y_hat_xgboost, testset.y)$byClass["Sensitivity"] %>% round(4)
spec <- confusionMatrix(y_hat_xgboost, testset.y)$byClass["Specificity"] %>% round(4)
prec <- confusionMatrix(y_hat_xgboost, testset.y)$byClass["Precision"] %>% round(4)
f1 <- confusionMatrix(y_hat_xgboost, testset.y)$byClass["F1"] %>% round(4)

#updating the output table
test_results <- bind_rows(test_results,
                          data_frame(method="XGBoost", 
                                     Accuracy = acc, Sensitivity = sens, Specificity = spec, 
                                     Precision = prec, F1 = f1))
test_results[8, ] %>% knitr::kable()


## FINAL RESULTS

test_results %>% knitr::kable()

### checking the ROC curve for Logistic Regression and XGBoost.
probs <- seq(0, 1, length.out = 10)

glm_probs <- map_df(probs, function(p){
  y_hat <- ifelse(p_hat_glm_all< p,"death","survive") %>% 
    factor(levels=levels(testset.y))
  list(method = "Logistic Regression",
       cutoff = round(p, 2),
       FPR = 1 - specificity(y_hat, testset.y),
       TPR = sensitivity(y_hat, testset.y))
})

XGboost_probs <- map_df(probs, function(p){
  y_hat <- ifelse(p_hat_xgboost< p ,"death","survive") %>% 
    factor(levels=levels(testset.y))
  list(method = "XGBoost",
       cutoff = round(p, 2),
       FPR = 1 - specificity(y_hat, testset.y),
       TPR = sensitivity(y_hat, testset.y))
})

#plotting the ROC
bind_rows(glm_probs, XGboost_probs) %>%
  ggplot(aes(FPR, TPR, label = cutoff, color = method)) +
  geom_line() +
  geom_point() +
  geom_text(size = 3, nudge_x = 0.05) +
  xlab("1 - Specificity") +
  ylab("Sensitivity") +
  ggtitle("ROC curve")


### Checking the Precision-Recall curve for Logistic regression and XGBosst
glm_probs <- map_df(probs, function(p){
  y_hat <- ifelse(p_hat_glm_all< p,"death","survive") %>% 
    factor(levels=levels(testset.y))
  list(method = "Logistic Regression",
       recall = sensitivity(y_hat, testset.y),
       precision = precision(y_hat, testset.y))
})

XGboost_probs <- map_df(probs, function(p){
  y_hat <- ifelse(p_hat_xgboost< p ,"death","survive") %>% 
    factor(levels=levels(testset.y))
  list(method = "XGBoost",
       recall = sensitivity(y_hat, testset.y),
       precision = precision(y_hat, testset.y))
})

#plotting the curve
bind_rows(glm_probs, XGboost_probs) %>%
  ggplot(aes(recall, precision, color = method)) +
  geom_line() +
  geom_point() +
  ggtitle("Precision-Recall curve")

#----------------------------- end ------------------------------------------
