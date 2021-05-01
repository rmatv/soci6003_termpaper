# for details: https://stats.idre.ucla.edu/r/dae/ordinal-logistic-regression/

library(Hmisc)
library(expss)
levels(sports.eb88.4$education) <- c("No FT educ", "<16 yrs", "16-19",
                                     ">19 yrs", "Still Studying")
levels(sports.eb88.4$social_class) <- c("Working", "Lower Mid", 
                                        "Middle", "Upper Mid", "Higher")
levels(sports.eb88.4$club_membership) <- c("Not a member", "Member")

sports.eb88.4 = apply_labels(sports.eb88.4,
                             sports2 = "Regularity"        ,
                             vigorous_hours = "Vigorous PA",
                             moderate_hours = "Moderate PA",
                             walk_hours = "Walking",
                             sitting_hours = "Sitting",
                             sex = "Sex",
                             age = "Age",
                             social_class = "Soc. class",
                             education = "Education",
                             area = "Area",
                             sport_clubs = "Sport clubs",
                             authorities = "Authorities",
                             club_membership = "Membership")

sf <- function(y) {
  c('Y>=1' = qlogis(mean(y >= 1)),
    'Y>=2' = qlogis(mean(y >= 2)),
    'Y>=3' = qlogis(mean(y >= 3)))
}
s <- with(sports.eb88.4, summary(as.numeric(sports2) ~ vigorous_hours + moderate_hours + walk_hours + sitting_hours + sex + age + social_class + education + area +  
                                   sport_clubs + authorities + club_membership, fun=sf))

