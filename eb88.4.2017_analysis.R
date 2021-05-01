# Libraries ==========================================================
library(dplyr)      # data manipulation
library(foreign)    # reading/writing data stored by other stat software
library(ordinal)    # ordinal regressions with mixed effects
library(rcompanion) # calculate pseudo R^2 for ordinal models

# Data ===============================================================
# data source: https://doi.org/10.4232/1.13288
eb88.4.2017 <- read.spss("eb88_4_2017/ZA6939_v2-0-0.sav", to.data.frame = TRUE)
# Variables ==========================================================
## recode and assign sensible names to variables
# Regularity of exercising sports and pa -----------------------------
sports <- eb88.4.2017$qb1 # participation in sports
sports.eb88.4 <- data.frame(sports = sports) # start new ds to simplify analysis
sports.eb88.4$sports <- factor(sports.eb88.4$sports, 
                               levels = rev(levels(sports.eb88.4$sport)))
sports.eb88.4$sports <- factor(sports.eb88.4$sport,
                               levels = levels(sports.eb88.4$sport)[2:7])
sports.eb88.4$sports2 <- recode_factor(sports.eb88.4$sports,
                                       "Never" = "Never",
                                       "Less often" = "Seldom",
                                       "1 to 3 times a month" = "Seldom",
                                       "1 to 2 times a week" = "Regularly",
                                       "3 to 4 times a week" = "Regularly",
                                       "5 times a week or more " = "Regularly")

sports.eb88.4$pa <- eb88.4.2017$qb2 # pa
sports.eb88.4$pa <- factor(sports.eb88.4$pa, 
                           levels = rev(levels(sports.eb88.4$pa)))
sports.eb88.4$pa <- factor(sports.eb88.4$pa,
                           levels = levels(sports.eb88.4$pa)[2:7])
sports.eb88.4$pa2 <- recode_factor(sports.eb88.4$pa,
                                   "Never" = "Never",
                                   "Less often" = "Seldom",
                                   "1 to 3 times a month" = "Seldom",
                                   "1 to 2 times a week" = "Regularly",
                                   "3 to 4 times a week" = "Regularly",
                                   "5 times a week or more" = "Regularly")
# Intensivity of PA
vigorous.hours <- recode_factor(eb88.4.2017$qb3b,
                                "30 minutes or less"                       = ".25",
                                "31 to 60 minutes"                         = ".5",
                                "61 to 90 minutes"                         = "1",
                                "91 to 120 minutes"                        = "1.5",
                                "More than 120 minutes"                    = "2",
                                "Never do any vigorous physical activity " = "0",
                                "DK"                                       = "NA")
vigorous.hours <- as.numeric(as.character(vigorous.hours))
vigorous.hours[is.na(vigorous.hours)] <- median(vigorous.hours, na.rm = TRUE)
sports.eb88.4$vigorous_hours <- vigorous.hours

moderate.hours <- recode_factor(eb88.4.2017$qb4b,
                                "30 minutes or less"                      = ".25",
                                "31 to 60 minutes"                        = ".5",
                                "61 to 90 minutes"                        = "1",
                                "91 to 120 minutes"                       = "1.5",
                                "More than 120 minutes"                   = "2",
                                "Never do any moderate physical activity" = "0",
                                "DK"                                      = "NA")
moderate.hours <- as.numeric(as.character(moderate.hours))
moderate.hours[is.na(moderate.hours)] <- median(moderate.hours, na.rm = TRUE)
sports.eb88.4$moderate_hours <- moderate.hours

walk.hours <- recode_factor(eb88.4.2017$qb5b,
                            "30 minutes or less"                  = ".25",
                            "31 to 60 minutes"                    = ".5",
                            "61 to 90 minutes"                    = "1",
                            "91 to 120 minutes"                   = "1.5",
                            "More than 120 minutes"               = "2",
                            "Never walk for 10 minutes at a time" = "0",
                            "DK"                                  = "NA")
walk.hours <- as.numeric(as.character(walk.hours))
walk.hours[is.na(walk.hours)] <- median(walk.hours, na.rm = TRUE)
sports.eb88.4$walk_hours <- walk.hours

sitting.hours <- recode_factor(eb88.4.2017$qb6,
                               "1 hour or less"                           = ".5",
                               "1 hour to 1 hour and 30 minutes"          = "1",
                               "1 hour 31 minutes to 2 hours 30 minutes"  = "1.5",
                               "2 hours 31 minutes to 3 hours 30 minutes" = "2.5",
                               "3 hours 31 minutes to 4 hours 30 minutes" = "3.5",
                               "4 hours 31 minutes to 5 hours 30 minutes" = "4.5",
                               "5 hours 31 minutes to 6 hours 30 minutes" = "5.5",
                               "6 hours 31 minutes to 7 hours 30 minutes" = "6.5",
                               "7 hours 31 minutes to 8 hours 30 minutes" = "7.5",
                               "More than 8 hours and 30 minutes"         = "8.5",
                               "DK"                                       = "NA")
sitting.hours <- as.numeric(as.character(sitting.hours))
sitting.hours[is.na(sitting.hours)] <- median(sitting.hours, na.rm = TRUE)
sports.eb88.4$sitting_hours <- sitting.hours
# Motives ------------------------------------------------------------
# subset motives for doing sports/pa
motives.eb88.4 <- eb88.4.2017 %>% 
  select(qb8.1:qb8.15) #%>% 
# mutate_if(is.factor,as.numeric)
motives.eb88.4[is.na(motives.eb88.4)] <- "Not mentioned" # NA to "Not mentioned"

# assign sensible names to columns
colnames(motives.eb88.4) <- c("health", "phys_appearance",
                              "counter_aging", "fun", "relax", "friends",
                              "acquaintances", "other_cultures",
                              "phys_performance", "fitness",
                              "weight_control", "self_esteem",
                              "new_skills", "competition", "socialisation")
sports.eb88.4 <- cbind(sports.eb88.4, motives.eb88.4)

# Demographic --------------------------------------------------------
sports.eb88.4$sex <- eb88.4.2017$d10 # sex

sports.eb88.4$age <- eb88.4.2017$d11 # age
# convert age to numeric variable
levels(sports.eb88.4$age)[1] <- "15"
levels(sports.eb88.4$age)[83] <- c("98")
levels(sports.eb88.4$age)[84] <- c("99")
sports.eb88.4$age <- as.numeric(as.character(sports.eb88.4$age))

sports.eb88.4$age_cohort <- eb88.4.2017$d11r1 # age cohort

sports.eb88.4$community <- eb88.4.2017$d25 # type of community
sports.eb88.4$community <- recode_factor(sports.eb88.4$community,
                                         "Rural area or village" = "Rural",
                                         "Small/middle town"     = "Urban",
                                         "Large town"            = "Urban")
sports.eb88.4$community <- factor(sports.eb88.4$community,
                                  levels = c("Rural", "Urban")) # DK -> NA

sports.eb88.4$social_class <- eb88.4.2017$d63 # social class
sports.eb88.4$social_class <- factor(sports.eb88.4$social_class,
                                     levels = c("The working class of society",
                                                "The lower middle class of society",
                                                "The middle class of society",
                                                "The upper middle class of society",
                                                "The higher class of society"))  # DK -> NA
levels(sports.eb88.4$social_class) <- c("Working class", "Lower middle class",
                                        "Middle class", "Upper middle class",
                                        "Higher class")

sports.eb88.4$education <- eb88.4.2017$d8r2
sports.eb88.4$education <- factor(sports.eb88.4$education,
                                  levels = c("No full-time education", 
                                             "Up to 15 years",
                                             "16-19", 
                                             "20 years and older", 
                                             "Still Studying"))

# Sport related opportunities ----------------------------------------
sports.eb88.4$area <- eb88.4.2017$qb11_1 # area offers opportunities for doing sports
sports.eb88.4$area <- recode_factor(sports.eb88.4$area,
                                    "Totally agree" = "Agree",
                                    "Tend to agree" = "Agree",
                                    "Tend to disagree" = "Disagree or DK",
                                    "Totally disagree" = "Disagree or DK",
                                    "DK" = "Disagree or DK")
sports.eb88.4$area <- factor(sports.eb88.4$area,
                             levels = c("Disagree or DK", "Agree"))
sports.eb88.4$sport_clubs <- eb88.4.2017$qb11_2# sport clubs offers opportunities
sports.eb88.4$sport_clubs <- recode_factor(sports.eb88.4$sport_clubs,
                                           "Totally agree" = "Agree",
                                           "Tend to agree" = "Agree",
                                           "Tend to disagree" = "Disagree or DK",
                                           "Totally disagree" = "Disagree or DK",
                                           "DK" = "Disagree or DK")
sports.eb88.4$sport_clubs <- factor(sports.eb88.4$sport_clubs,
                                    levels = c("Disagree or DK", "Agree"))

sports.eb88.4$authorities <- eb88.4.2017$qb11_3# authorities offers opportunities
sports.eb88.4$authorities <- recode_factor(sports.eb88.4$authorities,
                                           "Totally agree" = "Agree",
                                           "Tend to agree" = "Agree",
                                           "Tend to disagree" = "Disagree or DK",
                                           "Totally disagree" = "Disagree or DK",
                                           "DK" = "Disagree or DK")
sports.eb88.4$authorities <- factor(sports.eb88.4$authorities,
                                    levels = c("Disagree or DK", "Agree"))

# Where exercise sports or PA
places.eb88.4 <- eb88.4.2017 %>% select(qb7.1:qb7.8)
places.eb88.4[is.na(places.eb88.4)] <- "Not mentioned" # NA to "Not mentioned"
names(places.eb88.4) <- c("fitness_centre", "sport_club", 
                          "sport_centre", "school",
                          "work", "home", "commute",
                          "outdoors")
sports.eb88.4 <- cbind(sports.eb88.4, places.eb88.4)
# Membership in a sport club -----------------------------------------
membership.eb88.4 <- eb88.4.2017 %>% 
  select(qb10.1:qb10.4) %>% 
  mutate_if(is.factor, as.numeric) 
membership.eb88.4[membership.eb88.4 == 1] <- 0
membership.eb88.4[membership.eb88.4 == 2] <- 1
membership.eb88.4$membership <- rowSums(membership.eb88.4)
membership.eb88.4$membership[membership.eb88.4$membership > 0] <- 1
membership.eb88.4$membership <- as.factor(membership.eb88.4$membership)
levels(membership.eb88.4$membership) <- c("Not a member", "Member of a club")
sports.eb88.4$club_membership <- membership.eb88.4$membership

# Antimotivations ----------------------------------------------------
sports.eb88.4$no_time <- eb88.4.2017$qb9.1

# Weights and country ------------------------------------------------
sports.eb88.4$weights <- eb88.4.2017$w1 
sports.eb88.4$country <- eb88.4.2017$country

# Omit NAs -----------------------------------------------------------
sports.eb88.4 <- na.omit(sports.eb88.4)

# ANALYSIS ===========================================================
# null models --------------------------------------------------------
model0 <- clm(sports ~ 1, weights = weights, data = sports.eb88.4) # no random effect
model.null <- clmm(sports ~ 1 + (1|country), weights = weights,
                     control=clmm.control(grtol=1e-6),
                     data = sports.eb88.4, Hess = TRUE) # with random effect
anova(model0, model.null)

# control variables only ---------------------------------------------
model.control <- clmm(sports2 ~ vigorous_hours + moderate_hours + 
                        walk_hours + sitting_hours + (1|country), 
                      weights = weights,
                      control=clmm.control(grtol=1e-6),
                      data = sports.eb88.4, Hess = TRUE) # with random effect
summary(model.control)
anova(model.null, model.control)
nagelkerke(fit = model.control, null = model.null)[2]

# demographic model --------------------------------------------------
model.dem <- update(model.control, .~. + sex + age)
summary(model.dem)
anova(model.null, model.control)
nagelkerke(fit = model.dem, null = model.null)[2]
nagelkerke(fit = model.dem, null = model.control)[2]

# social-economic ----------------------------------------------------
model.se <- update(model.control, .~. + social_class + education + community)
summary(model.se)
anova(model.null, model.se)
nagelkerke(fit = model.se, null = model.null)[2]
nagelkerke(fit = model.se, null = model.control)[2]
nagelkerke(fit = model.se, null = model.dem)[2]

# environmental ------------------------------------------------------
model.env <- update(model.control, .~. + area + sport_clubs + authorities + club_membership)
summary(model.env)
anova(model.null, model.env)
nagelkerke(fit = model.env, null = model.null)[2]
nagelkerke(fit = model.env, null = model.control)[2]
nagelkerke(fit = model.env, null = model.dem)[2]
nagelkerke(fit = model.env, null = model.se)[2]

# integrated ---------------------------------------------------------
model.int <- update(model.dem, .~. + social_class + education + area + sport_clubs + authorities + club_membership)
summary(model.int)
anova(model.null, model.int)
nagelkerke(fit = model.int, null = model.null)[2]
nagelkerke(fit = model.int, null = model.control)[2]
nagelkerke(fit = model.int, null = model.dem)[2]
nagelkerke(fit = model.int, null = model.se)[2]
nagelkerke(fit = model.int, null = model.env)[2]

model.int.i <- update(model.int, .~. + age:social_class) # add interaction term
summary(model.int.i)

# more simple model --------------------------------------------------
model.simp <- clmm(sports2 ~ social_class + education + club_membership + (1|country), 
                   weights = weights,
                   control=clmm.control(grtol=1e-6),
                   data = sports.eb88.4, Hess = TRUE)
summary(model.int)
nagelkerke(fit = model.simp, null = model.int)
anova(model.int, model.simp)

# plogits ------------------------------------------------------------
summary(model.int)
exp.inv.logits <- data.frame(exp(coef(model.int)))
round(exp.inv.logits, 2)
