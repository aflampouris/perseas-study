## ALL BY YEAR

df.res.year <-
  data.frame(var = character(),
             p = numeric())

df.year <- as.data.frame(df1[, c("Year", "name", cont.vars, cat.vars)])

for (i in c(4:27, 29, 31:ncol(df.year))) {
  if (coalesce(class(df.year[, i])[2], class(df.year[, i])[1]) == 'numeric')
    
  {
    formula1 = paste(colnames(df.year)[i], ' ~Year + age+ gender + (1 |name)')
    mmm1 <- lmer(formula1,
                 data = df.year)
    
    formula2 = paste(colnames(df.year)[i], ' ~ age+ gender +(1 |name)')
    mmm2 <- lmer(formula2,
                 data = df.year)
    
    
    
    print(colnames(df.year)[i])
    
    df.res.year <-
      rbind(df.res.year,
            cbind(labels(df.year)[i],
                  anova(mmm1, mmm2)[, 'Pr(>Chisq)'][2]))
  }
  
  else if (coalesce(class(df.year[, i])[2], class(df.year[, i])[1]) == 'factor')
    
  {
    formula1 = paste(colnames(df.year)[i], ' ~Year + age+ gender + (1 |name)')
    mmm1 <- glmer(
      formula1,
      family = binomial,
      data = df.year,
      glmerControl(optimizer = 'bobyqa')
    )
    
    formula2 = paste(colnames(df.year)[i], ' ~ age+ gender + (1 |name)')
    mmm2 <- glmer(
      formula2,
      family = binomial,
      data = df.year,
      glmerControl(optimizer = 'bobyqa')
    )
    
    
    print(colnames(df.year)[i])
    
    df.res.year <-
      rbind(df.res.year,
            cbind(labels(df.year)[i],
                  anova(mmm1, mmm2)[, 'Pr(>Chisq)'][2]))
    
    
  }
  
  else if (coalesce(class(df.year[, i])[2], class(df.year[, i])[2]) == 'ordered') {
    formula1 = paste(colnames(df.year)[i], ' ~ Year + age+ gender + (1 |name)')
    
    mmm1 <-
      clmm(formula1 ,
           data = df.year,
           link = "probit")
    
    formula2 = paste(colnames(df.year)[i], ' ~  age+ gender + (1 |name)')
    
    mmm2 <-
      clmm(formula2 ,
           data = df.year,
           link = "probit")
    
    
    print(colnames(df.year)[i])
    
    df.res.year <-
      rbind(df.res.year,
            cbind(labels(df.year)[i],
                  anova(mmm1, mmm2)[, 'Pr(>Chisq)'][2]))
  }
}



df.res.year[, 2] <- as.numeric(as.character(df.res.year[, 2]))

df.res.year$pf = ifelse(df.res.year[, 2] < 0.001, '<0.001', round(df.res.year[, 2], 3))
View(df.res.year[df.res.year[, 2] <= 0.05, 1:3])


## BY GENDER

# MALES

df.res.year.males <-
  data.frame(var = character(),
             p = numeric())

df.year.males <-
  as.data.frame(df1[df1$gender == 'Male' , c("Year", "name", cont.vars, cat.vars)])

for (i in c(4:26, 29, 31:ncol(df.year.males))) {
  if (coalesce(class(df.year.males[, i])[2], class(df.year.males[, i])[1]) == 'numeric')
    
  {
    formula1 = paste(colnames(df.year.males)[i], ' ~Year + age + (1 |name)')
    mmm1 <- lmer(formula1,
                 data = df.year.males)
    
    formula2 = paste(colnames(df.year.males)[i], ' ~ age + (1 |name)')
    mmm2 <- lmer(formula2,
                 data = df.year.males)
    
    
    
    print(colnames(df.year.males)[i])
    
    df.res.year.males <-
      rbind(df.res.year.males,
            cbind(labels(df.year.males)[i],
                  anova(mmm1, mmm2)[, 'Pr(>Chisq)'][2]))
  }
  
  else if (coalesce(class(df.year.males[, i])[2], class(df.year.males[, i])[1]) == 'factor')
    
  {
    formula1 = paste(colnames(df.year.males)[i], ' ~Year + age + (1 |name)')
    mmm1 <- glmer(
      formula1,
      family = binomial,
      data = df.year.males,
      glmerControl(optimizer = 'bobyqa')
    )
    
    formula2 = paste(colnames(df.year.males)[i], ' ~ age + (1 |name)')
    mmm2 <- glmer(
      formula2,
      family = binomial,
      data = df.year.males,
      glmerControl(optimizer = 'bobyqa')
    )
    
    
    print(colnames(df.year.males)[i])
    
    df.res.year.males <-
      rbind(df.res.year.males,
            cbind(labels(df.year.males)[i],
                  anova(mmm1, mmm2)[, 'Pr(>Chisq)'][2]))
    
    
  }
  
  else if (coalesce(class(df.year.males[, i])[2], class(df.year.males[, i])[2]) == 'ordered') {
    formula1 = paste(colnames(df.year.males)[i], ' ~ Year + age + (1 |name)')
    
    mmm1 <-
      clmm(formula1 ,
           data = df.year.males,
           link = "probit")
    
    formula2 = paste(colnames(df.year.males)[i], ' ~  age + (1 |name)')
    
    mmm2 <-
      clmm(formula2 ,
           data = df.year.males,
           link = "probit")
    
    
    print(colnames(df.year.males)[i])
    
    df.res.year.males <-
      rbind(df.res.year.males,
            cbind(labels(df.year.males)[i],
                  anova(mmm1, mmm2)[, 'Pr(>Chisq)'][2]))
  }
}



df.res.year.males[, 2] <-
  as.numeric(as.character(df.res.year.males[, 2]))

df.res.year.males$pf = ifelse(df.res.year.males[, 2] < 0.001,
                              '<0.001',
                              round(df.res.year.males[, 2], 3))
View(df.res.year.males[df.res.year.males[, 2] <= 0.05, 1:3])



# FEMALES

df.res.year.females <-
  data.frame(var = character(),
             p = numeric())

df.year.females <-
  as.data.frame(df1[df1$gender == 'Female' , c("Year", "name", cont.vars, cat.vars)])

for (i in c(4:26, 29, 31:ncol(df.year.females))) {
  if (coalesce(class(df.year.females[, i])[2], class(df.year.females[, i])[1]) == 'numeric')
    
  {
    formula1 = paste(colnames(df.year.females)[i], ' ~Year + age + (1 |name)')
    mmm1 <- lmer(formula1,
                 data = df.year.females)
    
    formula2 = paste(colnames(df.year.females)[i], ' ~ age + (1 |name)')
    mmm2 <- lmer(formula2,
                 data = df.year.females)
    
    
    
    print(colnames(df.year.females)[i])
    
    df.res.year.females <-
      rbind(df.res.year.females,
            cbind(labels(df.year.females)[i],
                  anova(mmm1, mmm2)[, 'Pr(>Chisq)'][2]))
  }
  
  else if (coalesce(class(df.year.females[, i])[2], class(df.year.females[, i])[1]) == 'factor')
    
  {
    formula1 = paste(colnames(df.year.females)[i], ' ~Year + age + (1 |name)')
    mmm1 <- glmer(
      formula1,
      family = binomial,
      data = df.year.females,
      glmerControl(optimizer = 'bobyqa')
    )
    
    formula2 = paste(colnames(df.year.females)[i], ' ~ age + (1 |name)')
    mmm2 <- glmer(
      formula2,
      family = binomial,
      data = df.year.females,
      glmerControl(optimizer = 'bobyqa')
    )
    
    
    print(colnames(df.year.females)[i])
    
    df.res.year.females <-
      rbind(df.res.year.females,
            cbind(labels(df.year.females)[i],
                  anova(mmm1, mmm2)[, 'Pr(>Chisq)'][2]))
    
    
  }
  
  else if (coalesce(class(df.year.females[, i])[2], class(df.year.females[, i])[2]) == 'ordered') {
    formula1 = paste(colnames(df.year.females)[i], ' ~ Year + age + (1 |name)')
    
    mmm1 <-
      clmm(formula1 ,
           data = df.year.females,
           link = "probit")
    
    formula2 = paste(colnames(df.year.females)[i], ' ~  age + (1 |name)')
    
    mmm2 <-
      clmm(formula2 ,
           data = df.year.females,
           link = "probit")
    
    
    print(colnames(df.year.females)[i])
    
    df.res.year.females <-
      rbind(df.res.year.females,
            cbind(labels(df.year.females)[i],
                  anova(mmm1, mmm2)[, 'Pr(>Chisq)'][2]))
  }
}



df.res.year.females[, 2] <-
  as.numeric(as.character(df.res.year.females[, 2]))

df.res.year.females$pf = ifelse(df.res.year.females[, 2] < 0.001,
                                '<0.001',
                                round(df.res.year.females[, 2], 3))
View(df.res.year.females[df.res.year.females[, 2] <= 0.05, 1:3])
