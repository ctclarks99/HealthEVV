# health <- sasxport.get("LLCP2017.XPT")
# health <- health %>% mutate(ID = c(1:450016))
# health <- health %>% filter(x.state == 18)
#
# indiana <- health %>% transmute(ID,Sex = sex,
#                                 # 1 male, 2 female, 9 refused
#                                 Education = educa,
#                                 # 1 no school ever, 2 grade 1-8, 3 grade 9-11, 4 GED, 5 college 1-3 years, 6 college 4+ years, 9 refused
#                                 Income = income2,# 1 less 10000, 2 less 15000, 3 less 20000, 4 less 25000,5 less 35000, 6 less 50000, 7 less 75000, 8 75000+, 77 don't know, 99 refused
#                                 Told_Blood_Pressure_High = bphigh4,
#                                 ## 1 yes, 2 yes during pregnancy, 3 no, 4 borderline, 7 dont know, have you ever been told your blood pressure
#                                 High_Cholesterol = toldhi2,
#                                 ## Have you ever been told your blood cholesterol is high,  1 yes , 2 no##
#                                 Asthma = asthma3,
#                                 ##Have you ever had asthma, 1 yes 2 no##
#                                 Arthritis = havarth3,
#                                 ##Have you ever had some form of arthritis, 1 yes 2 no##
#                                 Depression = addepev2,
#                                 ## Have you ever had a depressive disorder, 1 yes 2 no##
#                                 Diabetes = diabete3,
#                                 ## Have you ever had diabetes, 1 yes, 2 yes during pregnancy, 3 no, 4 borderline##
#                                 overweight = x.rfbmi5,
#                                 #1 - No, 2 - Yes
#                                 smoker = x.smoker3,
#                                 #1 - now smokes every day, 2 - now smokes some days, 3 - former smoker, 4 - never smoked
#                                 binge_drinker = x.rfbing5,
#                                 #1 - No, 2 - Yes
#                                 imp_race = x.imprace
#                                 #1 - white, 2 - black, 3 - asian, 4 - american indian/alaska native, 5 - hispanic
# )
#
# df <- data.frame(ID = indiana$ID,
#                  race = as.factor(indiana$imp_race),
#                  sex = as.factor(if_else(indiana$Sex %in% c(1,2), indiana$Sex, NA_integer_)),
#                  income = as.factor(if_else(indiana$Income %in% c(1:8), indiana$Income, NA_integer_)),
#                  smoker = as.factor(if_else(indiana$smoker %in% c(1:4), indiana$smoker, NA_integer_)),
#                  education = as.factor(if_else(indiana$Education %in% c(1:6), indiana$Education, NA_integer_)),
#                  asthma = as.factor(if_else(indiana$Asthma %in% c(1,2), indiana$Asthma, NA_integer_)),
#                  overweight = as.factor(if_else(indiana$overweight %in% c(1,2), indiana$overweight, NA_integer_)),
#                  binge_drinker = as.factor(if_else(indiana$binge_drinker %in% c(1,2), indiana$binge_drinker, NA_integer_)),
#                  arthritis = as.factor(if_else(indiana$Arthritis %in% c(1,2), indiana$Arthritis, NA_integer_)),
#                  depression = as.factor(if_else(indiana$Depression %in% c(1,2), indiana$Depression, NA_integer_)),
#                  high_cholesterol = as.factor(if_else(indiana$High_Cholesterol %in% c(1,2), indiana$High_Cholesterol, NA_integer_)),
#                  high_blood_pressure = as.factor(if_else(indiana$Told_Blood_Pressure_High %in% c(1:4), indiana$Told_Blood_Pressure_High, NA_integer_))
# )
#
# df_final <- df %>% transmute(smoke = if_else(smoker %in% c(1,2),1,0),
#                              asthma = if_else(asthma == 1,1,0),
#                              overweight = if_else(overweight == 2,1,0),
#                              binge_drinker = if_else(binge_drinker == 2,1,0),
#                              arthritis = if_else(arthritis == 1,1,0),
#                              depression = if_else(depression == 1,1,0),
#                              high_cholesterol = if_else(high_cholesterol == 1,1,0),
#                              high_blood_pressure = if_else(high_blood_pressure == 1,1,0),
#                              male = if_else(sex == 1,1,0),
#                              white = if_else(race == 1,1,0),
#                              black = if_else(race == 2,1,0),
#                              asian = if_else(race == 3,1,0),
#                              alaska_indian = if_else(race == 4,1,0),
#                              hispanic = if_else(race == 5,1,0),
#                              poverty = if_else(income %in% c(1:4),1,0),
#                              upper_class = if_else(income == 8,1,0),
#                              no_GED = if_else(education %in% c(1:3),1,0),
#                              college = if_else(education %in% c(5:6),1,0)
# )
#
# df_final <- na.exclude(df_final)

#' Final Data Frame
df_final <- read.csv(file = "data_final.csv")

#' Basic Model
log_model_per_person_smoke <- glm(smoke ~ male + white + black + asian + alaska_indian + hispanic + poverty + upper_class + no_GED + college,
                                  data = df_final,
                                  family = binomial(link = "logit"))
#' Best Smoking
#' @export
Best_Smoking <- glm(smoke ~ male + white + black + asian + alaska_indian + hispanic +
                      poverty + upper_class + no_GED + college + male:white + male:college +
                      white:poverty + white:upper_class + white:no_GED + white:college +
                      black:poverty + black:college + alaska_indian:upper_class +
                      alaska_indian:no_GED + alaska_indian:college + hispanic:college +
                      upper_class:college,
                    data = df_final,
                    family = binomial(link = "logit"))

#' Best Asthma
#' @export
Best_Asthma <- glm(asthma ~ male + white + black + asian + alaska_indian + hispanic +
                     poverty + upper_class + no_GED + college + male:white + male:black +
                     white:poverty + white:upper_class + white:college + black:poverty +
                     black:upper_class + black:no_GED + alaska_indian:poverty +
                     hispanic:no_GED + hispanic:college,
                   data = df_final,
                   family = binomial(link = "logit"))

#' Best Overweight
#' @export
Best_Overweight <- glm(overweight ~ male + white + asian + alaska_indian + hispanic +
                         poverty + upper_class + no_GED + college + male:white + male:hispanic +
                         male:poverty + male:upper_class + white:poverty + asian:no_GED +
                         alaska_indian:poverty + alaska_indian:upper_class + hispanic:poverty +
                         hispanic:college + poverty:no_GED + poverty:college,
                       data = df_final,
                       family = binomial(link = "logit"))

#' Binge Drinking
#' @export
Best_Binge_Drinker <- glm(binge_drinker ~ male + white + black + asian + alaska_indian +
                            hispanic + poverty + upper_class + no_GED + college + male:alaska_indian +
                            male:college + white:no_GED + black:upper_class + asian:college +
                            hispanic:no_GED + upper_class:no_GED + white:poverty + poverty:no_GED,
                          data = df_final,
                          family = binomial(link = "logit"))

# Best_Arthritis <-
# Best_Depression <-
# Best_High_Cholestorol <-

#' Best High Blood Pressure
#' @export
Best_High_Blood_Pressure <- glm(high_blood_pressure ~ male + white + black + asian + hispanic +
                                  poverty + upper_class + no_GED + college + male:black + male:hispanic +
                                  male:poverty + male:upper_class + male:college + white:poverty +
                                  white:no_GED + asian:poverty + asian:upper_class + asian:college +
                                  hispanic:upper_class + hispanic:no_GED + poverty:no_GED +
                                  upper_class:no_GED,
                                data = df_final,
                                family = binomial(link = "logit"))

#' Evansville Map
evansville <- c(left = -87.70264,
                bottom = 37.83241,
                right = -87.45628,
                top = 38.20472)

#' @importFrom ggmap get_map
evv_map <- get_map(evansville, maptype = "roadmap")

#' Reading
evv <- sf::st_read("Census_Tracts.shp")
#' Transform
evv <- sf::st_transform(evv, crs = "+proj=longlat +datumWGS84 +no_defs +ellps=WGS84")
#' Frame
#' @importFrom dplyr %>% mutate filter select group_by left_join summarise
evv <- data.frame(evv) %>% mutate(GEOID = GEOID10)

#' Census Key
#' @importFrom tidycensus census_api_key get_acs
census_api_key("94bc4d9ee635889131819300fb54c51a0a92b05f", install = FALSE)

#' Education Tract
Education_tract <- data.frame(get_acs(geography = "tract",
                                      geometry = FALSE,
                                      state = "Indiana",
                                      county = "163",
                                      variables = c(education_num_counted = "B06009_001",
                                                    No_GED = "B06009_002",
                                                    High_School_Grad = "B06009_003",
                                                    Some_college_or_associates = "B06009_004",
                                                    Bachelors_degree = "B06009_005",
                                                    Professional_degree = "B06009_006"
                                                    )
                                      )
                              )
#` Sorting Education Data
No_GED <- Education_tract %>%
  filter(variable == "No_GED") %>%
  select(GEOID, no_GED = estimate)

#` High School
High_School <- Education_tract %>%
  filter(variable == "High_School_Grad") %>%
  select(GEOID, high_school_grad = estimate)

#` College
College <- Education_tract %>%
  filter(variable %in% c("Some_college_or_associates", "Bachelors_degree", "Professional_degree")) %>%
  group_by(GEOID) %>%
  summarise(college = sum(estimate))

#` Full Census
Census_data <- No_GED %>%
  left_join(High_School, by = "GEOID") %>%
  left_join(College, by = "GEOID")

#` Sex Data
Sex_tract <- data.frame(get_acs(geography = "tract",
                                geometry = FALSE,
                                state = "Indiana",
                                county = "163",
                                variables = c(Males = "B01001_002")
                                )
                        ) %>% select(GEOID, males = estimate)
#` Joining Sex
Census_data <- Census_data %>% left_join(Sex_tract, by = "GEOID")

#` Income Data
Income_tract <- data.frame(tidycensus::get_acs(geography = "tract",
                                   geometry = FALSE,
                                   state = "Indiana",
                                   county = "163",
                                   variables = c(income_00_10 = "B19001_002",
                                                 income_10_15 = "B19001_003",
                                                 income_15_20 = "B19001_004",
                                                 income_20_25 = "B19001_005",
                                                 income_25_30 = "B19001_006",
                                                 income_30_35 = "B19001_007",
                                                 income_35_40 = "B19001_008",
                                                 income_40_45 = "B19001_009",
                                                 income_45_50 = "B19001_010",
                                                 income_50_60 = "B19001_011",
                                                 income_60_75 = "B19001_012",
                                                 income_75_100 = "B19001_013",
                                                 income_100_125 = "B19001_014",
                                                 income_125_150 = "B19001_015",
                                                 income_150_200 = "B19001_016",
                                                 income_200_up = "B19001_017"
                                                 )
                                   )
                           )
#` Poverty
Poverty <- Income_tract %>%
  filter(variable %in% c("income_00_10", "income_10_15", "income_15_20", "income_20_25")) %>%
  group_by(GEOID) %>%
  summarise(poverty = sum(estimate))

#` Middle Class
Middle_Class <- Income_tract %>% filter(variable %in% c("income_25_30","income_30_35","income_35_40","income_40_45","income_45_50","income_50_55","income_55_60","income_60_65","income_65_70","income_70_75")) %>%
  group_by(GEOID) %>%
  summarise(middle_class = sum(estimate))

#` Upper Class
Upper_Class <- Income_tract %>% filter(variable %in% c("income_75_100","income_100_125","income_125_150","income_150_200","income_200_up")) %>%
  group_by(GEOID) %>%
  summarise(upper_class = sum(estimate))

#` Census Join Income
Census_data <- Census_data %>%
  left_join(Poverty, by = "GEOID") %>%
  left_join(Middle_Class, by = "GEOID") %>%
  left_join(Upper_Class, by = "GEOID")

#` Final Data Frame
eville <- evv %>% left_join(Census_data, by = "GEOID")

#` Convert Census Data
eville <- eville %>% mutate(white = White_NH / Pop_18_and,
                            black = Black_NH / Pop_18_and,
                            asian = Asian_NH / Pop_18_and,
                            alaska_indian = Am_Indian_ / Pop_18_and,
                            hispanic = Hispanic_o / Pop_18_and,
                            other_race = (Asian_NH + Am_Indian_ + Hispanic_o) / Pop_18_and,
                            male = males / Pop_18_and,
                            poverty = poverty / Pop_18_and,
                            upper_class = upper_class / Pop_18_and,
                            no_GED = no_GED / Pop_18_and,
                            high_school = high_school_grad / Pop_18_and,
                            college = college / Pop_18_and)

#` Predictions
eville <- eville %>% mutate(Proportion_Smokers = predict.glm(Best_Smoking, newdata = eville, type = "response"),
                            Predicted_Number_Smokers = predict.glm(Best_Smoking, newdata = eville, type = "response") * Pop_18_and,
                            Difference_From_Average_Smokers = predict.glm(Best_Smoking, newdata = eville, type = "response") - .20139,#NOT ACCURATE
                            Proportion_Overweight = predict.glm(Best_Overweight, newdata = eville, type = "response"),
                            Predicted_Number_Overweight = predict.glm(Best_Overweight, newdata = eville, type = "response") * Pop_18_and,
                            Difference_From_Average_Overweight = predict.glm(Best_Overweight, newdata = eville, type = "response") - .70706)

#` Find correct ggplot to return
#`
#` Returns ggplot
#` @param input1 Selected response from shiny UI
#` @param input2 Selected plot type from shiny UI
#` @return correct ggplot
#` @export
prediction_map <- function(input1,input2) {
  if(input1 == "Smoking" & input2 == "Predicted Proportion") { return(ggmap(evv_map) +
                                                                        geom_sf(aes(fill = Proportion_Smokers),
                                                                                inherit.aes=FALSE,
                                                                                alpha = .75,
                                                                                data = eville) +
                                                                        scale_fill_viridis_c(option = "B") +
                                                                        ggtitle("Predicted Proportion of Smokers") +
                                                                        theme(plot.title = element_text(size = rel(1.3)),
                                                                              axis.text.x = element_blank(),
                                                                              axis.text.y = element_blank(),
                                                                              axis.title.x = element_blank(),
                                                                              axis.title.y = element_blank()
                                                                        )
  )
  }
  if(input1 == "Smoking" & input2 == "Predicted Number of People") { return(ggmap(evv_map) +
                                                                              geom_sf(aes(fill = Predicted_Number_Smokers),
                                                                                      inherit.aes=FALSE,
                                                                                      alpha = .75,
                                                                                      data = eville) +
                                                                              scale_fill_viridis_c(option = "B") +
                                                                              ggtitle("Predicted Number of Smokers") +
                                                                              theme(plot.title = element_text(size = rel(1.3)),
                                                                                    axis.text.x = element_blank(),
                                                                                    axis.text.y = element_blank(),
                                                                                    axis.title.x = element_blank(),
                                                                                    axis.title.y = element_blank()
                                                                              )
  )
  }
  if(input1 == "Smoking" & input2 == "Predicted Difference From Indiana Average") { return(ggmap(evv_map) +
                                                                                             geom_sf(aes(fill = Difference_From_Average_Smokers),
                                                                                                     inherit.aes=FALSE,
                                                                                                     alpha = .75,
                                                                                                     data = eville) +
                                                                                             scale_fill_viridis_c(option = "B") +
                                                                                             ggtitle("diff from average smoking") +
                                                                                             theme(plot.title = element_text(size = rel(1.3)),
                                                                                                   axis.text.x = element_blank(),
                                                                                                   axis.text.y = element_blank(),
                                                                                                   axis.title.x = element_blank(),
                                                                                                   axis.title.y = element_blank()
                                                                                             )
  )
  }
  if(input1 == "Overweight" & input2 == "Predicted Proportion") { return(ggmap(evv_map) +
                                                                           geom_sf(aes(fill = Proportion_Overweight),
                                                                                   inherit.aes=FALSE,
                                                                                   alpha = .75,
                                                                                   data = eville) +
                                                                           scale_fill_viridis_c(option = "B") +
                                                                           ggtitle("Predicted Proportion of Overweight Citizens") +
                                                                           theme(plot.title = element_text(size = rel(1.3)),
                                                                                 axis.text.x = element_blank(),
                                                                                 axis.text.y = element_blank(),
                                                                                 axis.title.x = element_blank(),
                                                                                 axis.title.y = element_blank()
                                                                           )
  )
  }
  if(input1 == "Overweight" & input2 == "Predicted Number of People") { return(ggmap(evv_map) +
                                                                                 geom_sf(aes(fill = Predicted_Number_Overweight),
                                                                                         inherit.aes=FALSE,
                                                                                         alpha = .75,
                                                                                         data = eville) +
                                                                                 scale_fill_viridis_c(option = "B") +
                                                                                 ggtitle("Predicted Number of Overweight Citizens") +
                                                                                 theme(plot.title = element_text(size = rel(1.3)),
                                                                                       axis.text.x = element_blank(),
                                                                                       axis.text.y = element_blank(),
                                                                                       axis.title.x = element_blank(),
                                                                                       axis.title.y = element_blank()
                                                                                 )
  )
  }
  if(input1 == "Overweight" & input2 == "Predicted Difference From Indiana Average") { return(ggmap(evv_map) +
                                                                                                geom_sf(aes(fill = Difference_From_Average_Overweight),
                                                                                                        inherit.aes=FALSE,
                                                                                                        alpha = .75,
                                                                                                        data = eville) +
                                                                                                scale_fill_viridis_c(option = "B") +
                                                                                                ggtitle("diff from average overweight") +
                                                                                                theme(plot.title = element_text(size = rel(1.3)),
                                                                                                      axis.text.x = element_blank(),
                                                                                                      axis.text.y = element_blank(),
                                                                                                      axis.title.x = element_blank(),
                                                                                                      axis.title.y = element_blank()
                                                                                                )
  )
  }
}
