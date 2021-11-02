list.of.packages <-
  c(
    'readstata13',
    'plyr',
    'dplyr',
    'dummies',
    'desc',
    'reporttools',
    'Rmisc',
    'Hmisc',
    'reshape2',
    'ggplot2',
    'scales',
    'papeR',
    'ggsci',
    'stargazer',
    'ordinal',
    'gee',
    'geepack',
    'lme4',
    'xtable'
  )


new.packages <-
  list.of.packages[!(list.of.packages %in% installed.packages()[, "Package"])]
if (length(new.packages))
  install.packages(new.packages)

lapply(list.of.packages, require, character.only = T)



df1 <-
  read.dta13(
    "~/Projects/PERSEAS/meleti_perseas/Datasets/Stata/Elafonisos2012_2013_2014_2015_2016.dta",
    convert.factors = T,
    convert.underscore = F,
    convert.dates = T,
    generate.factors = T,
    nonint.factors = F,
    fromEncoding = 'Windows-1253'
  )


# df1 <- df1[df1$Year <= 2015, ]
# apply(df1, 2, function(x)length(unique(x)))

#####DATACHECKS
#
# by_gender <- df1 %>% group_by(gender)
#
#   by_gender %>% summarise_at(
#     vars(legumes, oliveoil, poultry, dairy,redmeat, fruit_juices, cereal_milled, fish, cheese, vegetable_salads),
#     funs(
#       median = median
#     ),
#     na.rm = T
#   )

df1 <- df1 %>% plyr::mutate(
  Year = factor(Year),
  weight = "is.na<-" (weight, weight > 500) ,
  height = "is.na<-" (height, height > 250 |
                        height < 20) ,
  waist_circumference = "is.na<-" (waist_circumference, waist_circumference < 20) ,
  neck_circumference = "is.na<-" (
    neck_circumference,
    neck_circumference < 2 |
      neck_circumference > 80
  ) ,
  fat_percent = "is.na<-" (fat_percent, fat_percent > 90) ,
  leanbodymass = "is.na<-" (leanbodymass, leanbodymass < 2 |
                              leanbodymass > weight) ,
  systolic1 = "is.na<-" (systolic1, systolic1 < 10 |
                           systolic1 > 300) ,
  systolic2 = "is.na<-" (systolic2, systolic2 < 10 |
                           systolic2 > 300) ,
  diastolic1 = "is.na<-" (diastolic1, diastolic1 < 10 |
                            diastolic1 > 300) ,
  diastolic2 = "is.na<-" (diastolic2, diastolic2 < 10 |
                            diastolic2 > 300) ,
  standing_systolic = "is.na<-" (
    standing_systolic,
    standing_systolic < 10 |
      standing_systolic > 300
  ) ,
  standing_diastolic = "is.na<-" (
    standing_diastolic,
    standing_diastolic < 10 |
      standing_diastolic > 300
  ) ,
  heartrate1 = "is.na<-" (heartrate1, heartrate1 > 500) ,
  heartrate2 = "is.na<-" (heartrate2, heartrate2 > 500) ,
  heartrate3 = "is.na<-" (heartrate3, heartrate3 > 500) ,
  NONHDL = "is.na<-" (NONHDL, NONHDL > 500) ,
  tCholHDL = "is.na<-" (tCholHDL, tCholHDL > 100) ,
  tchol = "is.na<-" (tchol, tchol > 500 | tchol < 10) ,
  HbA1C = "is.na<-" (HbA1C, HbA1C >= 20) ,
  FEV1 = "is.na<-" (FEV1, FEV1 >= 20) ,
  FVC = "is.na<-" (FVC, FVC >= 20) ,
  FEV1FVC = "is.na<-" (FEV1FVC, FEV1FVC > 100) ,
  menopause = "is.na<-" (menopause, menopause > 1 |
                           gender == 0) ,
  snoring = "is.na<-" (snoring, snoring > 1) ,
  alcohol = ifelse(
    !is.na(alc_glasses_daily) |
      !is.na(alc_glasses_weekly),
    1,
    alcohol
  ),
  education = "is.na<-" (education, education >= 5) ,
  physact_type = "is.na<-" (physact_type, physact_type > 1) ,
  famhist_death = "is.na<-" (famhist_death, famhist_death > 7) ,
  w2hipratio = waist_circumference / hip_circumference,
  
  age_65 =  cut2(df1$age, cuts = c(0, 65, max(df1$age, na.rm = T))),
  bmi = weight / ((height / 100) ^ 2),
  ABI_l = ABI_l_d_systolic / pmax(ABI_l_up_systolic, ABI_r_up_systolic) ,
  ABI_r = ABI_r_d_systolic / pmax(ABI_l_up_systolic, ABI_r_up_systolic)
  
  
  
)





df1 <- df1 %>% plyr::mutate(
  bmi = "is.na<-" (bmi, bmi > 70) ,
  
  cholesterol_observed = case_when(
    age > 18 & tchol < 200 ~ "Normal",
    age > 18 & tchol < 240 ~ "High",
    age > 18 & tchol >= 240 ~ "Very High",
    age <= 18 & tchol < 170 ~ "Normal",
    age <= 18 & tchol < 200 ~ "High",
    age <= 18 & tchol >= 200 ~ "Very High"
    
  ),
  
  cholesterol_observed = case_when(
    age > 18 & tchol < 200 ~ "No",
    age > 18 & tchol < 240 ~ "Yes",
    age > 18 & tchol >= 240 ~ "Yes",
    age <= 18 & tchol < 170 ~ "No",
    age <= 18 & tchol < 200 ~ "Yes",
    age <= 18 & tchol >= 200 ~ "No"
    
  ),
  hypertension_observed = case_when(
    systolic1 >= 140 |
      diastolic1 >= 90 |
      systolic2 >= 140 | diastolic2 >= 90 ~ 'Yes',
    systolic1 < 140 &
      diastolic1 < 90 & systolic2 < 140 & diastolic2 < 90 ~ 'No'
    
  ),
  hypertension_treatment = case_when(
    !is.na(treatment_hypertension) &
      treatment_hypertension != 65 ~ 'Yes'
    
  )
  ,
  atrial_fibrillation_2 = case_when(atrial_fibrillation %in% c(1, 2) ~ 1,
                                    atrial_fibrillation == 0 ~ 0),
  
  gluc_fast = case_when(
    GLUC < 100 & nisteias == 1 ~ 0,
    GLUC >= 100 &
      GLUC < 126  & nisteias == 1 ~ 1,
    GLUC >= 126 &  nisteias == 1 ~ 2
    
    
  )
  ,
  dietscore = case_when(
    gender == 0 ~ ifelse(cereal_milled == 0, 0, 1) + ifelse(fruit_juices <=
                                                              2, 0, 1) + ifelse(vegetable_salads <= 1, 0, 1) + ifelse(legumes <=
                                                                                                                        1, 0, 1) + ifelse(fish <= 3, 0, 1) +
      ifelse(redmeat >= 2, 0, 1)  + ifelse(poultry >= 3, 0, 1) + ifelse(dairy >=
                                                                          1, 0, 1) + ifelse(cheese >= 2, 0, 1) + ifelse(oliveoil <= 4, 0, 1),
    
    gender == 1 ~ ifelse(cereal_milled == 0, 0, 1) + ifelse(fruit_juices <=
                                                              2, 0, 1) + ifelse(vegetable_salads <= 2, 0, 1) + ifelse(legumes <=
                                                                                                                        1, 0, 1) + ifelse(fish <= 2, 0, 1) +
      ifelse(redmeat >= 2, 0, 1) + ifelse(poultry >= 3, 0, 1) + ifelse(dairy >=
                                                                         1, 0, 1) + ifelse(cheese >= 2, 0, 1) + ifelse(oliveoil <= 4, 0, 1)
    
  ),
  
  d_alcohol = case_when (
    gender == 0  &
      
      coalesce(alc_glasses_daily, alc_glasses_weekly / 7) <= 3 ~ 1 ,
    gender == 0  &
      
      coalesce(alc_glasses_daily, alc_glasses_weekly / 7) == 0 |
      coalesce(alc_glasses_daily, alc_glasses_weekly / 7) > 3 ~ 0 ,
    
    
    gender == 1  &
      
      coalesce(alc_glasses_daily, alc_glasses_weekly / 7) <= 2 ~ 1 ,
    gender == 1  &
      
      coalesce(alc_glasses_daily, alc_glasses_weekly / 7) == 0 |
      coalesce(alc_glasses_daily, alc_glasses_weekly / 7) > 2 ~ 0
    
  ),
  smok_health = case_when(smoking == 0 ~ 2 ,
                          smoking == 1 ~ 0,
                          smoking == 2 ~ 1),
  phys_activity_health = case_when(
    phys_activity == 0 ~ 0,
    phys_activity == 1 |
      phys_activity == 2 ~ 1,
    phys_activity == 3 ~ 0
  ),
  age_65 = case_when(age < 65 ~ '[0,65)',
                     age >= 65 ~ "65+")
)

df1$d_alcohol = ifelse(df1$alcohol == 0, 0, df1$d_alcohol)
df1$dietscore <- df1$dietscore + df1$d_alcohol

df1$dietscore_cat <-
  cut(df1$dietscore,
      c(0, 3, 7, 11),
      include.lowest = T,
      labels = c(0, 1, 2))

df1$healthyindex = as.numeric(df1$smok_health) + as.numeric(df1$phys_activity_health) + as.numeric(df1$dietscore_cat)



df1$bmi_cat <-
  factor(cut(
    df1$bmi,
    breaks = c(0, 18.5, 25, 30 , 35, 999),
    labels = c(
      'Undeweight',
      'Normal',
      'Overweight',
      'Obese',
      'Extremely obese'
    )
  ), ordered = T)
df1$bmi_cat_3 <- cut2(df1$bmi, cuts = c(0, 25, 30, 999))


df1 <-
  df1 %>% plyr::mutate(
    gender = factor(
      gender,
      levels = c(0, 1),
      labels = c('Male', 'Female')
    ),
    age_65 = as.factor(age_65),
    smoking = factor(
      smoking,
      levels = c(0, 1, 2),
      labels = c('Never', 'Current', 'Former')
    )
  )

df1$smoking_2 <- df1$smoking
levels(df1$smoking_2) <- c("No", "Yes", "Yes")

df1$cholesterol_observed_2 <-
  as.factor(df1$cholesterol_observed)
levels(df1$cholesterol_observed_2) <- c('Yes', 'No', 'Yes')
df1$cholesterol_observed_2 <-
  relevel(df1$cholesterol_observed_2, ref = 'No')



cont.vars <- c(
  "age",
  "sleeptime24",
  "dietscore",
  "height",
  "weight",
  "bmi",
  "waist_circumference",
  "hip_circumference"      ,
  "neck_circumference",
  "fat_percent",
  "leanbodymass",
  "tchol",
  "LDL",
  "HDL",
  "tgl_nisteias",
  "NONHDL",
  "tCholHDL",
  "GLUC",
  "HbA1C",
  "FEV1",
  "FVC",
  "FEV1FVC",
  "ABI_l",
  "ABI_r"
  # "ABI_l_up_systolic",
  # "ABI_l_up_diastolic",
  # "ABI_l_d_systolic",
  # "ABI_l_d_diastolic",
  # "ABI_r_up_systolic",
  # "ABI_r_up_diastolic",
  # "ABI_r_d_systolic",
  # "ABI_r_d_diastolic"
)


cont.labels <- c(
  "Age (Years)",
  "Sleep per day (hours)",
  "Dietary score",
  "Height (cm)",
  "Weight (kg)",
  "Body-mass index (BMI) (kg/m^2)",
  "Waist circumference (cm)",
  "Hip circumference (cm)",
  "Neck circumference (cm)",
  "Body fat (%)",
  "Lean body-mass (kg)",
  "Total cholesterol (mg/dl)",
  "LDL (mg/dl)",
  "HDL (mg/dl)",
  "TGL (Fasting)",
  "Non-HDL Cholesterol (mg/dl)",
  "Cholesterol Ratio",
  "Glucose (mg/dl)",
  "HbA1c (%)",
  "FEV1 (l)",
  "FVC (l)",
  "FEV1/FVC ratio",
  "ABI (Left)",
  "ABI (Right)"
  # ,
  # "ABI_l_up_systolic",
  # "ABI_l_up_diastolic",
  # "ABI_l_d_systolic",
  # "ABI_l_d_diastolic",
  # "ABI_r_up_systolic",
  # "ABI_r_up_diastolic",
  # "ABI_r_d_systolic",
  # "ABI_r_d_diastolic"
)

labels(df1[, cont.vars]) <- cont.labels


cat.vars <- c(
  "gender",
  "age_65",
  "education",
  "menopause",
  "sleepnoon",
  "snoring",
  "smoking",
  "alcohol",
  "alcoholtype" ,
  "cereal_milled",
  "potato",
  "fruit_juices",
  "vegetable_salads",
  "legumes",
  "fish",
  "redmeat",
  "poultry",
  "dairy",
  "oliveoil",
  "cheese",
  "sweets",
  "phys_activity",
  "physact_type",
  "bmi_cat",
  "healthyindex",
  # "hypertension",
  "hypertension_observed",
  "cholesterol_observed",
  "dislipidemia",
  "gluc_fast",
  "diabetes_t2",
  "coronaryhd",
  "stroke",
  "peripheralvasculardicease",
  "atrial_fibrillation"
)

df1 <- df1 %>% mutate (
  gender = as.factor(gender),
  age_65 = as.factor(age_65),
  education = factor(
    education,
    labels = c(
      'Illiterate-Below primary',
      'Primary',
      'Gymnasium',
      'Lyceum' ,
      'Tertiary'
    ),
    ordered = T
  ),
  menopause = factor(menopause, labels = c('No', 'Yes')),
  sleepnoon = factor(
    sleepnoon,
    levels = c(0, 1),
    labels = c('No', 'Yes')
  ),
  snoring = factor(
    snoring,
    levels = c(0, 1),
    labels = c('No', 'Yes')
  ),
  smoking = as.factor(smoking),
  alcohol = factor(
    alcohol,
    levels = c(0, 1),
    labels = c('No', 'Yes')
  ),
  alcoholtype =  factor(
    alcoholtype,
    levels = c(0, 1, 2, 3),
    labels = c('Beer', 'Wine' , 'Other', 'Beer and wine'),
    ordered = T
  ),
  cereal_milled =  factor(
    cereal_milled,
    levels = c(0, 1, 2, 3, 4, 5),
    labels = c(
      'None or less than once weekly',
      '1-3 times weekly' ,
      '4-6 times weekly',
      'Once daily',
      '2-3 times daily',
      'More than 4 times daily'
    ),
    ordered = T
  ),
  potato =  factor(
    potato,
    levels = c(0, 1, 2, 3, 4, 5),
    labels = c(
      'None or less than once weekly',
      '1-3 times weekly' ,
      '4-6 times weekly',
      'Once daily',
      '2-3 times daily',
      'More than 4 times daily'
    ),
    ordered = T
  ),
  fruit_juices =  factor(
    fruit_juices,
    levels = c(0, 1, 2, 3, 4, 5),
    labels = c(
      'None or less than once weekly',
      '1-3 times weekly' ,
      '4-6 times weekly',
      'Once daily',
      '2-3 times daily',
      'More than 4 times daily'
    ),
    ordered = T
  ),
  vegetable_salads =  factor(
    vegetable_salads,
    levels = c(0, 1, 2, 3, 4, 5),
    labels = c(
      'None or less than once weekly',
      '1-3 times weekly' ,
      '4-6 times weekly',
      'Once daily',
      '2-3 times daily',
      'More than 4 times daily'
    ),
    ordered = T
  ),
  legumes =  factor(
    legumes,
    levels = c(0, 1, 2, 3, 4),
    labels = c(
      'None',
      'Less than once weekly' ,
      '1-2 times weekly',
      '3-4 times weekly',
      '5-6 times weekly'
    ),
    ordered = T
  ),
  fish =  factor(
    fish,
    levels = c(0, 1, 2, 3, 4, 5),
    labels = c(
      'None',
      'Less than once weekly',
      '1-2 times weekly' ,
      '3-4 times weekly',
      '5-6 times weekly',
      'More than 6 times weekly'
    ),
    ordered = T
  ),
  redmeat =  factor(
    redmeat,
    levels = c(0, 1, 2, 3, 4, 5),
    labels = c(
      'None',
      'Less than once weekly',
      '1-2 times weekly' ,
      '3-4 times weekly',
      '5-6 times weekly',
      'More than 6 times weekly'
    ),
    ordered = T
  ),
  poultry =  factor(
    poultry,
    levels = c(0, 1, 2, 3, 4, 5),
    labels = c(
      'None',
      'Less than once weekly',
      '1-2 times weekly' ,
      '3-4 times weekly',
      '5-6 times weekly',
      'More than 6 times weekly'
    ),
    ordered = T
  ),
  dairy =  factor(
    dairy,
    levels = c(0, 1, 2, 3, 4, 5),
    labels = c(
      'None or less than once weekly',
      '1-3 times weekly' ,
      '4-6 times weekly',
      'Once daily',
      '2-3 times daily',
      'More than 4 times daily'
    ),
    ordered = T
  ),
  oliveoil =  factor(
    oliveoil,
    levels = c(0, 1, 2, 3, 4, 5),
    labels = c(
      'None',
      'Rarely',
      'Less than once weekly',
      '1-3 times weekly' ,
      '3-5 times weekly',
      'Daily'
    ),
    ordered = T
  ),
  cheese =  factor(
    cheese,
    levels = c(0, 1, 2, 3, 4, 5),
    labels = c(
      'None or less than once weekly',
      '1-3 times weekly' ,
      '4-6 times weekly',
      'Once daily',
      '2-3 times daily',
      'More than 4 times daily'
    ),
    ordered = T
  ),
  sweets =  factor(
    sweets,
    levels = c(0, 1, 2, 3, 4, 5),
    labels = c(
      'Never',
      'Less than once weekly',
      '1-2 times weekly' ,
      '3-4 times weekly',
      '5-6 times weekly',
      'More than 6 times weekly'
    ),
    ordered = T
  ),
  phys_activity =  factor(
    phys_activity,
    levels = c(0, 1, 2, 3),
    labels = c('None', 'Less than 3 times weekly' , '3 times weekly', 'Daily'),
    ordered = T
  ),
  physact_type =  factor(
    physact_type,
    levels = c(0, 1),
    labels = c('Sports', 'Walking')
  )
  
  ,
  healthyindex = factor(healthyindex, ordered = T),
  dislipidemia = factor(dislipidemia, labels = c('No', 'Yes')),
  gluc_fast = factor(
    gluc_fast,
    labels = c('Normal', 'Pre-diabetic', 'Diabetic'),
    ordered = T
  )
  ,
  peripheralvasculardicease = factor(peripheralvasculardicease, labels =
                                       c('No', 'Yes'))
  ,
  cholesterol_observed = factor(cholesterol_observed),
  
  hypertension_observed = factor(hypertension_observed),
  coronaryhd = factor(coronaryhd, labels = c('No', 'Yes')),
  atrial_fibrillation = factor(
    atrial_fibrillation,
    labels = c('No', 'Paroxysmal', 'Permanent'),
    ordered = T
  ),
  stroke = factor(stroke, labels = c('No', 'Yes')),
  diabetes_t2 = factor(diabetes_t2, labels = c('No', 'Yes'))
  
)





cat.labels <-
  c(
    "Gender",
    "Age group",
    "Education",
    "Menopause",
    "Afternoon nap",
    "Snoring",
    "Smoking",
    "Alcohol",
    "Alcohol type",
    "Whole-grain cereal",
    "Potatoes",
    "Fruits-Juice",
    "Vegetables-Salads ",
    "Legumes",
    "Fish",
    "Red meat",
    "Poultry",
    "Dairy products",
    "Olive oil",
    "Cheese",
    "Sweets",
    "Physical activity",
    "Physical activity type",
    "BMI",
    "Healthy habits index",
    # "Hypertension",
    "Hypertension (observed)",
    "Cholesterol (observed)",
    "History of dyslipidemia",
    "Glucose (Fasting)",
    "Type 2 Diabetes",
    "Coronary heart disease",
    "Stroke",
    "Peripheral vascular disease",
    "Atrial fibrillation"
  )

labels(df1[, cat.vars]) <- cat.labels



df.part <- df1 %>%
  group_by(name) %>%  arrange(Year) %>%  dplyr::mutate(
    count = n(),
    Participation = case_when(row_number() == 1 ~ 'First' ,  row_number() == n() ~ 'Last')
  ) %>% filter(!is.na(Participation) &
                 n() > 1) %>%  select(c("Year", "Participation", cont.vars, cat.vars))

labels(df.part[, c(cont.vars, cat.vars)]) <-
  labels(df1[, c(cont.vars, cat.vars)])

df.part$Participation <- factor(df.part$Participation)
df.part <- as.data.frame(df.part)

## TABLE 1

## Longitudinal contiunuous

for (i in cont.vars) {
  s1 <-
    summarySE(
      data = df1,
      measurevar = i,
      groupvars = "Year" ,
      na.rm = T,
      conf.interval = 0.95
    )
  
  s1$LB <- s1[, i] - s1$se
  s1$UB <- s1[, i] + s1$se
  
  
  pd <-
    position_dodge(0.1) # move them .05 to the left and right
  
  
  g <- ggplot(s1, aes_string(x = "Year", y = i)) +
    geom_errorbar(aes(ymin = LB, ymax = UB), width = .1) +
    geom_line(position = pd, aes(group = 1)) +
    geom_point(position = pd) + ylab(labels(df1[, i])) + theme(legend.position =
                                                                 "bottom")
  
  ggsave(
    plot = g,
    filename = paste0('cont_', i),
    path = '~/Projects/PERSEAS/Graphs/',
    device = 'png',
    scale = 0.6,
    width = 12,
    height = 8
  )
  
}



for (i in cat.vars) {
  d2 <- df1  %>%
    group_by_("Year",  i) %>%
    dplyr::summarise(count = n()) %>%    setNames(c("Year", 'var', 'count')) %>% filter(!is.na(var))    %>%
    mutate(perc = count / sum(count))
  
  
  g <- ggplot(d2, aes(
    x = as.factor(Year),
    y = perc * 100,
    fill = as.factor(var)
  )) +
    geom_bar(stat = "identity")  + xlab("Year") + ylab('Sample (%)') + guides(fill =
                                                                                guide_legend(title = labels(df1[, i]), nrow = 2)) +  theme(legend.position =
                                                                                                                                             "bottom") + scale_fill_lancet()
  
  ggsave(
    plot = g,
    filename = paste0('cat_', i),
    path = '~/Projects/PERSEAS/Graphs/',
    device = 'png',
    scale = 0.6,
    width = 12,
    height = 8
  )
}

## BY GENDER

for (i in cat.vars[-1]) {
  d2 <- df1  %>%
    group_by_("Year", "gender", i) %>%
    dplyr::summarise(count = n()) %>%    setNames(c("Year", 'gender', 'var', 'count')) %>% filter(!is.na(gender) &
                                                                                                    !is.na(var))    %>%
    mutate(perc = count / sum(count))
  
  
  g <- ggplot(d2, aes(
    x = as.factor(Year),
    y = perc * 100,
    fill = as.factor(var)
  )) +
    geom_bar(stat = "identity")  + xlab("Year") + ylab('Sample (%)') + guides(fill =
                                                                                guide_legend(title = labels(df1[, i]), nrow = 2)) + facet_wrap(~ gender) + theme(legend.position =
                                                                                                                                                                   "bottom") + scale_fill_lancet()
  ggsave(
    plot = g,
    filename = paste0('cat_gender_', i),
    path = '~/Projects/PERSEAS/Graphs/',
    device = 'png',
    scale = 0.6,
    width = 12,
    height = 8
  )
}

## BY AGE

for (i in cat.vars[-2]) {
  d2 <- df1  %>%
    group_by_("Year", "age_65", i) %>%
    dplyr::summarise(count = n()) %>%    setNames(c("Year", 'age_65', 'var', 'count')) %>% filter(!is.na(age_65) &
                                                                                                    !is.na(var))    %>%
    mutate(perc = count / sum(count))
  
  
  g <- ggplot(d2, aes(
    x = as.factor(Year),
    y = perc * 100,
    fill = as.factor(var)
  )) +
    geom_bar(stat = "identity")  + xlab("Year") + ylab('Sample (%)') + guides(fill =
                                                                                guide_legend(title = labels(df1[, i]), nrow = 2)) + facet_wrap(~ age_65) + theme(legend.position =
                                                                                                                                                                   "bottom") + scale_fill_lancet()
  ggsave(
    plot = g,
    filename = paste0('cat_age_65_', i),
    path = '~/Projects/PERSEAS/Graphs/',
    device = 'png',
    scale = 0.6,
    width = 12,
    height = 8
  )
}


for (i in cont.vars) {
  s1 <-
    summarySE(
      data = df.part,
      measurevar = i,
      groupvars = "Participation" ,
      na.rm = T,
      conf.interval = 0.95
    )
  
  s1$LB <- s1[, i] - s1$se
  s1$UB <- s1[, i] + s1$se
  
  
  pd <-
    position_dodge(0.1) # move them .05 to the left and right
  
  
  g <- ggplot(s1, aes_string(x = "Participation", y = i)) +
    geom_errorbar(aes(ymin = LB, ymax = UB), width = .1) +
    # geom_line(position = pd, aes(group = 1)) +
    geom_point(position = pd) + ylab(labels(df1[, i])) + theme(legend.position =
                                                                 "bottom")
  
  ggsave(
    plot = g,
    filename = paste0('cont_part_', i),
    path = '~/Projects/PERSEAS/Graphs/',
    device = 'png',
    scale = 0.6,
    width = 12,
    height = 8
  )
  
}



for (i in cat.vars) {
  d2 <- df.part  %>%
    group_by_("Participation",  i) %>%
    dplyr::summarise(count = n()) %>%    setNames(c("Participation", 'var', 'count')) %>% filter(!is.na(var))    %>%
    mutate(perc = count / sum(count))
  
  
  g <- ggplot(d2, aes(
    x = as.factor(Participation),
    y = perc * 100,
    fill = as.factor(var)
  )) +
    geom_bar(stat = "identity")  + xlab("Participation") + ylab('Sample (%)') + guides(fill = guide_legend(title = labels(df1[, i]), nrow = 2)) +  theme(legend.position =
                                                                                                                                                           "bottom") + scale_fill_lancet()
  
  
  ggsave(
    plot = g,
    filename = paste0('cat_part_', i),
    path = '~/Projects/PERSEAS/Graphs/',
    device = 'png',
    scale = 0.6,
    width = 12,
    height = 8
  )
}



library(gee)
library(stargazer)
library(geepack)




df.res <-
  data.frame(
    var = character(),
    coef = numeric(),
    lo = numeric(),
    up = numeric(),
    p = numeric()
  )


for (i in c(5:27, 30, 32, 35:45, 47:49, 51:57)) {
  if (class(df.part[, i])[length(class(df.part[, i]))] == 'numeric') {
    formula1 = paste(colnames(df.part)[i], ' ~Participation + (1 |name)')
    mmm1 <- lmer(formula1,
                 data = df.part)
    
    formula2 = paste(colnames(df.part)[i], ' ~ (1 |name)')
    mmm2 <- lmer(formula2,
                 data = df.part)
    
    
    
    print(colnames(df.part)[i])
    
    df.res <-
      rbind(df.res,
            cbind(
              labels(df.part)[i],
              as.numeric(coef(summary(mmm1))["ParticipationLast" , "Estimate"]),
              as.numeric(confint(mmm1)['ParticipationLast', 1]),
              as.numeric(confint(mmm1)['ParticipationLast', 2]),
              anova(mmm1, mmm2)[, 'Pr(>Chisq)'][2]
            ))
  }
  
  else if (class(df.part[, i])[length(class(df.part[, i]))]  == 'factor')
    
  {
    formula1 = paste(colnames(df.part)[i], ' ~ Participation +  (1 |name)')
    mmm1 <- glmer(
      formula1,
      family = binomial,
      data = df.part,
      glmerControl(optimizer = 'bobyqa')
    )
    
    
    print(colnames(df.part)[i])
    
    df.res <-
      rbind(df.res,
            cbind(
              labels(df.part)[i],
              as.numeric(exp(coef(summary(
                mmm1
              ))["ParticipationLast" , "Estimate"])),
              as.numeric(exp(confint(mmm1)['ParticipationLast', 1])),
              as.numeric(exp(confint(mmm1)['ParticipationLast', 2])),
              as.numeric(coef(summary(mmm1))[, "Pr(>|z|)"]['ParticipationLast'])
            ))
    
    
    
  }
  
  else if (class(df.part[, i])[length(class(df.part[, i]))]  == 'ordered') {
    formula1 = paste(colnames(df.part)[i], ' ~ Participation + (1 |name)')
    
    mmm1 <-
      clmm(formula1 ,
           data = df.part,
           link = "probit")
    
    
    print(colnames(df.part)[i])
    df.res <-
      rbind(df.res,
            cbind(
              labels(df.part)[i],
              as.numeric(exp(coef(summary(
                mmm1
              ))["ParticipationLast" , "Estimate"])),
              as.numeric(exp(confint(mmm1)['ParticipationLast', 1])),
              as.numeric(exp(confint(mmm1)['ParticipationLast', 2])),
              as.numeric(coef(summary(mmm1))[, "Pr(>|z|)"]['ParticipationLast'])
            ))
    
  }
  
  
}


df.res <- as.data.frame(df.res)


df.res[, 2:5] <-
  sapply(sapply(
    sapply(df.res[, 2:5], as.character, simplify = F),
    as.numeric,
    simplify = F
  ), round, 3)




df.res$pf = ifelse(df.res[, 5] < 0.001, '<0.001', round(df.res[, 5], 3))
df.res$ci = paste0("(", df.res[, 3], ", ", df.res[, 4], ")")

names(df.res) <-
  c('Variable', 'Coefficient', '', '', '', 'p-value', '95% CI')
x <-
  xtable(
    df.res[df.res[, 5] < 0.05, c(1, 2, 7, 6)],
    include.rownames = F,
    digits = 3,
    align = c("l", "l", "r", "c", "r"),
    label = 'MixedParticipation',
    caption = "Association of the subjects' last participation in the study with measured characteristics, for participants with two or more visits. (reference category: \"First visit\")"
  )
print(x,
      include.rownames = F,
      booktabs = T,
      file = "MixedParticipation.tex")


df3 <-
  df1[!is.na(df1$age_65), c('Year', 'name', cont.vars, cat.vars)]
labels(df3) <- labels(df1[, c('Year', 'name', cont.vars, cat.vars)])

### BY AGE CAT

df.res.age_65 <-
  data.frame(
    var = character(),
    coef = numeric(),
    lo = numeric(),
    up = numeric(),
    p = numeric()
  )

for (i in c(3:26, 29 , 32, 34:48, 50:52, 56))  {
  if (coalesce(class(df3[, i])[2], class(df3[, i])[1]) == 'numeric') {
    formula1 = paste(colnames(df3)[i], ' ~Year + age_65 + (1 |name)')
    mmm1 <- lmer(formula1,
                 data = df3)
    
    formula2 = paste(colnames(df3)[i], ' ~Year +  (1 |name)')
    mmm2 <- lmer(formula2,
                 data = df3)
    
    anova(mmm1, mmm2)[, 'Pr(>Chisq)']
    print(colnames(df3)[i])
    
    df.res.age_65 <-
      rbind(df.res.age_65,
            cbind(
              labels(df3)[i],
              as.numeric(coef(summary(mmm1))["age_6565+" , "Estimate"]),
              as.numeric(confint(mmm1)['age_6565+', 1]),
              as.numeric(confint(mmm1)['age_6565+', 2]),
              anova(mmm1, mmm2)[, 'Pr(>Chisq)'][2]
            ))
  }
  
  else if (coalesce(class(df3[, i])[2], class(df3[, i])[1]) == 'factor')
    
  {
    formula1 = paste(colnames(df3)[i], ' ~ Year + age_65 +  (1 |name)')
    mmm1 <- glmer(
      formula1,
      family = binomial,
      data = df3,
      glmerControl(optimizer = 'bobyqa')
    )
    
    
    print(colnames(df3)[i])
    
    df.res.age_65 <-
      rbind(
        df.res.age_65,
        cbind(
          labels(df3)[i],
          as.numeric(exp(coef(summary(
            mmm1
          ))["age_6565+" , "Estimate"])),
          as.numeric(exp(confint(mmm1)['age_6565+', 1])),
          as.numeric(exp(confint(mmm1)['age_6565+', 2])),
          as.numeric(coef(summary(mmm1))[, "Pr(>|z|)"]['age_6565+'])
        )
      )
    
    
    
  }
  
  else if (coalesce(class(df3[, i])[2], class(df3[, i])[2]) == 'ordered') {
    formula1 = paste(colnames(df3)[i], ' ~ Year + age_65 +  (1 |name)')
    
    mmm1 <-
      clmm(formula1 ,
           data = df3,
           link = "probit")
    
    
    print(colnames(df3)[i])
    df.res.age_65 <-
      rbind(
        df.res.age_65,
        cbind(
          labels(df3)[i],
          as.numeric(exp(coefficients(summary(
            mmm1
          ))["age_6565+" , "Estimate"])),
          as.numeric(exp(confint(mmm1)['age_6565+', 1])),
          as.numeric(exp(confint(mmm1)['age_6565+', 2])),
          as.numeric(coef(summary(mmm1))[, "Pr(>|z|)"]['age_6565+'])
        )
      )
    
  }
  
  
}

df.res.age_65 <- as.data.frame(df.res.age_65)


df.res.age_65[, 2:5] <-
  sapply(sapply(
    sapply(df.res.age_65[, 2:5], as.character, simplify = F),
    as.numeric,
    simplify = F
  ), round, 2)




df.res.age_65$pf = ifelse(df.res.age_65[, 5] < 0.001, '<0.001', round(df.res.age_65[, 5], 3))
df.res.age_65$ci = paste0("(", df.res.age_65[, 3], ", ", df.res.age_65[, 4], ")")

names(df.res.age_65) <-
  c('Variable', 'Coefficient', '', '', '', 'p-value', '95% CI')
x <-
  xtable(
    df.res.age_65[df.res.age_65[, 5] < 0.05, c(1, 2, 7, 6)],
    include.rownames = F,
    digits = 3,
    align = c("l", "l", "r", "c", "r"),
    label = 'MixedAge65',
    caption = "Association of population's characteristics with age group (reference category: \"Age less than 65 years\")"
  )
print(x,
      include.rownames = F,
      # scalebox = 0.5,
      booktabs = T,
      file = "MixedAge65.tex")


### BY GENDER

df4 <-
  df1[!is.na(df1$gender), c('Year', 'name', cont.vars, cat.vars)]
labels(df4) <- labels(df1[, c('Year', 'name', cont.vars, cat.vars)])


df.res.gender <-
  data.frame(
    var = character(),
    coef = numeric(),
    lo = numeric(),
    up = numeric(),
    p = numeric()
  )

for (i in c(3:26, 28:29, 31:57)) {
  if (coalesce(class(df4[, i])[2], class(df4[, i])[1]) == 'numeric') {
    formula1 = paste(colnames(df4)[i], ' ~Year + gender  + (1 |name)')
    mmm1 <- lmer(formula1,
                 data = df4)
    
    formula2 = paste(colnames(df4)[i], ' ~Year +  (1 |name)')
    mmm2 <- lmer(formula2,
                 data = df4)
    
    anova(mmm1, mmm2)[, 'Pr(>Chisq)']
    print(colnames(df4)[i])
    
    df.res.gender <-
      rbind(df.res.gender,
            cbind(
              labels(df4)[i],
              as.numeric(coef(summary(mmm1))["genderFemale" , "Estimate"]),
              as.numeric(confint(mmm1)['genderFemale', 1]),
              as.numeric(confint(mmm1)['genderFemale', 2]),
              anova(mmm1, mmm2)[, 'Pr(>Chisq)'][2]
            ))
  }
  
  else if (coalesce(class(df4[, i])[2], class(df4[, i])[1]) == 'factor')
    
  {
    formula1 = paste(colnames(df4)[i], ' ~ Year + gender +  (1 |name)')
    mmm1 <- glmer(
      formula1,
      family = binomial,
      data = df4,
      glmerControl(optimizer = 'bobyqa')
    )
    
    
    print(colnames(df4)[i])
    
    df.res.gender <-
      rbind(
        df.res.gender,
        cbind(
          labels(df4)[i],
          as.numeric(exp(coef(summary(
            mmm1
          ))["genderFemale" , "Estimate"])),
          as.numeric(exp(confint(mmm1)['genderFemale', 1])),
          as.numeric(exp(confint(mmm1)['genderFemale', 2])),
          as.numeric(coef(summary(mmm1))[, "Pr(>|z|)"]['genderFemale'])
        )
      )
    
    
    
  }
  
  else if (coalesce(class(df4[, i])[2], class(df4[, i])[2]) == 'ordered') {
    formula1 = paste(colnames(df4)[i], ' ~ Year + gender +  (1 |name)')
    
    mmm1 <-
      clmm(formula1 ,
           data = df4,
           link = "probit")
    
    
    print(colnames(df4)[i])
    df.res.gender <-
      rbind(
        df.res.gender,
        cbind(
          labels(df4)[i],
          as.numeric(exp(coefficients(summary(
            mmm1
          ))["genderFemale" , "Estimate"])),
          as.numeric(exp(confint(mmm1)['genderFemale', 1])),
          as.numeric(exp(confint(mmm1)['genderFemale', 2])),
          as.numeric(coef(summary(mmm1))[, "Pr(>|z|)"]['genderFemale'])
        )
      )
    
  }
  
  
}

df.res.gender <- as.data.frame(df.res.gender)


df.res.gender[, 2:5] <-
  sapply(sapply(
    sapply(df.res.gender[, 2:5], as.character, simplify = F),
    as.numeric,
    simplify = F
  ), round, 2)




df.res.gender$pf = ifelse(df.res.gender[, 5] < 0.001, '<0.001', round(df.res.gender[, 5], 3))
df.res.gender$ci = paste0("(", df.res.gender[, 3], ", ", df.res.gender[, 4], ")")

names(df.res.gender) <-
  c('Variable', 'Coefficient', '', '', '', 'p-value', '95% CI')
x <-
  xtable(
    df.res.gender[df.res.gender[, 5] < 0.05, c(1, 2, 7, 6)],
    include.rownames = F,
    digits = 2,
    align = c("l", "l", "r", "c", "r"),
    label = 'MixedGender',
    caption = 'Association of population\'s characteristics with gender (reference category: "Males")'
  )
print(x,
      include.rownames = F,
      # scalebox = 0.5,
      booktabs = T,
      file = "MixedGender.tex")







df1 <-  df1 %>% group_by(Year) %>% arrange(name) %>% mutate(
  stroke = factor(stroke, labels = c(0, 1)),
  coronaryhd = factor(coronaryhd, labels = c(0, 1)),
  peripheralvasculardicease = factor(peripheralvasculardicease, labels = c(0, 1)),
  atrial_fibrillation_2 = factor(atrial_fibrillation_2, labels = c(0, 1))
)




fma <-
  gee(
    stroke ~ Year + age_65 + gender + hypertension_observed + diabetes_t2 + smoking_2  + cholesterol_observed_2 ,
    id = name,
    data = df1,
    corstr = 'exchangeable',
    family = binomial
  )

fmb <-
  gee(
    coronaryhd ~ Year + age_65 + gender + hypertension_observed + diabetes_t2 + smoking_2  + cholesterol_observed_2,
    id = name,
    data = df1,
    corstr = 'exchangeable',
    family = binomial
  )

fmc <-
  gee(
    peripheralvasculardicease ~ Year + age_65 + gender + hypertension_observed + diabetes_t2 + smoking_2  + cholesterol_observed_2,
    id = name,
    data = df1,
    corstr = 'exchangeable',
    family = binomial
  )


fmd <-
  gee(
    atrial_fibrillation_2 ~ Year + age_65 + gender + hypertension_observed + diabetes_t2 + smoking_2  + cholesterol_observed_2,
    id = name,
    data = df1,
    corstr = 'exchangeable',
    family = binomial
  )



confint.geeglm <- function(object, parm, level = 0.95, ...) {
  cc <- coef(summary(object))
  mult <- qnorm((1 + level) / 2)
  citab <- with(
    as.data.frame(cc),
    cbind(
      lwr = Estimate - mult * `Robust S.E.`,
      upr = Estimate + mult * `Robust S.E.`
    )
  )
  rownames(citab) <- rownames(cc)
  citab[parm,]
}

cis <- lapply(list(fma, fmb, fmc, fmd), confint.geeglm)

zscores <-
  lapply(list(fma, fmb, fmc, fmd), function(x) {
    summary(x)$coefficients[, 'Robust z']
  })



stargazer(
  fma,
  fmb,
  fmc,
  fmd,
  out = '~/Projects/PERSEAS/GEE.tex',
  type = 'latex',
  t = zscores,
  p.auto = F,
  ci.custom =  cis,
  apply.coef = exp,
  apply.ci = exp,
  star.cutoffs = c(0.05, 0.01, 0.001),
  notes = 'Association of risk factors with disease',
  notes.append = T,
  keep = c('gender', 'age', 'hyper', 'diab', 'smok', 'chol'),
  style = 'aer',
  colnames = F,
  font.size = 'small',
  covariate.labels = c(
    "Gender (Female)",
    "Age ($\\geq65$ years)",
    "Hypertension observed (Yes)",
    "T-2 diabetes (Yes)",
    "Smoking (Yes)",
    "Cholesterol observed (Yes)"
  ),
  dep.var.labels  = c(
    'Stroke',
    'Coronary heart disease',
    'Peripheral vascular disease',
    'Atrial fibrillation'
  ),
  model.numbers = FALSE,
  label = 'GEE'
)


## TABLES
t.Participation <-
  dummy.data.frame(df1[, c("name", "Year")], names = 'Year')
by_id <- t.Participation %>% group_by(name)
t.Participation <-
  by_id %>% summarise_at(vars(Year2012, Year2013, Year2014, Year2015, Year2016),
                         funs(max = max)) %>% count(Year2012_max,
                                                    Year2013_max,
                                                    Year2014_max,
                                                    Year2015_max,
                                                    Year2016_max) %>%
  mutate(n = n, 'f(%)' = round(100 * n / sum(n), 2))

colnames(t.Participation) <-
  gsub(colnames(t.Participation),
       pattern = 'Year|_max',
       replacement = '')


print(
  xtable(
    t.Participation,
    auto = T,
    caption = 'Participation pattern',
    label = 'TableParticipation'
  ),
  file = "~/Projects/PERSEAS/TableParticipation.tex",
  include.rownames = F
)

t.Participation$years <- rowSums(t.Participation[, 1:5])
t.Participation %>% group_by(years) %>%   summarise_all(sum)




TN <-
  tableNominal(
    as.data.frame(df1[, 1]),
    nams = c("Year", "gender"),
    file = "~/Projects/PERSEAS/t1_1.tex",
    longtable = T,
    lab = 't1_1'
  )






df2 <- as.data.frame(df1[, c("Year", cont.vars, cat.vars)])
colnames(df2) <- c("Year", cont.labels, cat.labels)

## TABLE 2


tableContinuous(
  vars = df2[, cont.labels],
  stats = c(
    "n",
    "mean",
    "sd" = function(x) {
      sd(x, na.rm = T)
    },
    "median",
    "iqr"
  ),
  group = df2[, 1],
  print.pval = "anova",
  pval.bound = 0.001,
  longtable = T,
  font.size = 'tiny',
  prec = 2,
  file = "~/Projects/PERSEAS/TableCont.tex",
  booktabs = T,
  cap = 'Evolution of continuous characteristics over time',
  lab = 'TableCont',
  nams = latexTranslate(cont.labels)
)




#CONTINUOUS PER GENDER#

tableContinuous(
  vars = df2[df2$Gender == 'Male', cont.labels],
  stats = c(
    "n",
    "mean",
    "sd" = function(x) {
      sd(x, na.rm = T)
    },
    "median",
    "iqr"
  ),
  group = df2[, 1],
  print.pval = "anova",
  pval.bound = 0.001,
  longtable = T,
  font.size = 'tiny',
  prec = 2,
  file = "~/Projects/PERSEAS/TableContMale.tex",
  booktabs = T,
  cap = 'Evolution of continuous characteristics over time (Males)',
  lab = 'TableContMale',
  col.tit = c(
    'Variable',
    'Levels',
    '$\\mathbf{n}$',
    '$\\mathbf{\\overline{x}}$',
    'SD',
    '$\\mathbf{\\widetilde{x}}$',
    '\\textbf{IQR}'
  )
)



tableContinuous(
  vars = df2[df2$Gender == 'Female', cont.labels],
  stats = c(
    "n",
    "mean",
    "sd" = function(x) {
      sd(x, na.rm = T)
    },
    "median",
    "iqr"
  ),
  group = df2[, 1],
  print.pval = "anova",
  pval.bound = 0.001,
  longtable = T,
  font.size = 'tiny',
  prec = 2,
  file = "~/Projects/PERSEAS/TableContFemale.tex",
  booktabs = T,
  cap = 'Evolution of continuous characteristics over time (Females)',
  lab = 'TableContFemale'
)



# TABLE 3


TN <- tableNominal(
  vars = df2[, cat.labels] ,
  group = df2[, "Year"],
  font.size = "tiny",
  cumsum = F,
  print.pval = 'chi2',
  pval.bound = 0.001,
  longtable = T,
  vertical = F,
  file = "~/Projects/PERSEAS/TableCatNom.tex",
  booktabs = T,
  cap = "Evolution of categorical and nominal characteristics  over time",
  lab = 'TableCatNom'
  
)

## TABLE 4

## by age_65


TN <- tableNominal(
  vars = df2[df2$`Age group` == '[0,65)', cat.labels[-c(1, 2)]],
  group = df2[df2$`Age group` == '[0,65)', "Year"],
  font.size = "tiny",
  cumsum = F,
  print.pval = 'chi2',
  pval.bound = 0.001,
  longtable = T,
  vertical = F,
  file = "~/Projects/PERSEAS/TableCatNom65less.tex",
  booktabs = T,
  cap = 'Evolution of categorical and nominal characteristics  over time for persons younger than 65 years',
  lab = 'TableCatNom65less'
)



TN <- tableNominal(
  vars = df2[df2$`Age group` == '65+', cat.labels[-c(1:2)]],
  group = df2[df2$`Age group` == '65+', "Year"],
  font.size = "tiny",
  cumsum = F,
  print.pval = 'chi2',
  pval.bound = 0.001,
  longtable = T,
  vertical = F,
  file = "~/Projects/PERSEAS/TableCatNom65more.tex",
  booktabs = T,
  cap = 'Evolution of categorical and nominal characteristics  over time for persons of age 65 years and more',
  lab = 'TableCatNom65more'
)




## TABLE 4

## by gender
TN <- tableNominal(
  vars = df2[df2$Gender == 'Male', cat.labels[-c(1:2, 4)]],
  group = df2[df2$Gender == 'Male', "Year"],
  font.size = "tiny",
  cumsum = F,
  print.pval = 'chi2',
  pval.bound = 0.001,
  longtable = T,
  vertical = F,
  file = "~/Projects/PERSEAS/TableCatNomMale.tex",
  booktabs = T,
  cap = 'Evolution of categorical and nominal characteristics over time for males',
  lab = 'TableCatNomMale'
)





TN <- tableNominal(
  vars = df2[df2$Gender == 'Female', cat.labels[-c(1:2)]],
  group = df2[df2$Gender == 'Female', "Year"],
  font.size = "tiny",
  cumsum = F,
  print.pval = 'chi2',
  pval.bound = 0.001,
  longtable = T,
  vertical = F,
  file = "~/Projects/PERSEAS/TableCatNomFemale.tex",
  booktabs = T,
  cap = 'Evolution of categorical and nominal characteristics over time for females',
  lab = 'TableCatNomFemale'
)
