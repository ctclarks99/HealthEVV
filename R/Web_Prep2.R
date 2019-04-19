#' Final Data Frame
#' @export
#' @importFrom dplyr %>% mutate
df_final <- read.csv(file = "data_final.csv") %>% mutate(white_poverty = white*poverty,
                                                         black_older = black*older,
                                                         poverty_older = poverty*older,
                                                         poverty_young = poverty*young,
                                                         hispanic_older = hispanic*older,
                                                         male_upper_class = male*upper_class)

#' Import census data to save time
#' @export
Census_data <- read.csv(file = "Census_Data.csv") %>% mutate(GEOID = as.character(GEOID))

#' Best Smoking
#' @export
Best_Smoking <- glm(smoke ~ male + white + black + asian + alaska_indian + hispanic +
                      poverty + upper_class + no_GED + college + young + older +
                      white_poverty + black_older + hispanic_older,
                    data = df_final,
                    family = binomial(link = "logit"))

#' Best Asthma
#' @export
Best_Asthma <- glm(asthma ~ male + white + black + asian + alaska_indian + hispanic +
                    poverty + upper_class + no_GED + college + young + older +
                    black_older + hispanic_older + poverty_older,
                   data = df_final,
                   family = binomial(link = "logit"))

#' Best Overweight
#' @export
Best_Overweight <- glm(overweight ~ male + white + black + asian + alaska_indian + hispanic +
                        poverty + upper_class + no_GED + college + young + older +
                        male_upper_class + poverty_older,
                       data = df_final,
                       family = binomial(link = "logit"))

#' Binge Drinking
#' @export
Best_Binge_Drinker <- glm(binge_drinker ~ male + white + black + asian + alaska_indian +
                            hispanic + poverty + upper_class + no_GED + college + young +
                            older + black_older + hispanic_older + poverty_young,
                          data = df_final,
                          family = binomial(link = "logit"))

#' Arthritis
#' @export
Best_Arthritis <- glm(arthritis ~ male + white + black + asian + alaska_indian + hispanic +
                        poverty + upper_class + no_GED + college + young + older + male_upper_class +
                        white_poverty + black_older + hispanic_older + poverty_young + poverty_older,
                      data = df_final,
                      family = binomial(link = "logit"))

#' Depression
#' @export
Best_Depression <- glm(depression ~ male + white + black + asian + hispanic + poverty +
                         upper_class + no_GED + college + young + older + white_poverty +
                         black_older + hispanic_older + poverty_young,
                       data = df_final,
                       family = binomial(link = "logit"))

#' High Cholestorol
#' @export
Best_High_Cholesterol <- glm(high_cholesterol ~ male + white + black + asian + alaska_indian +
                               poverty + upper_class + no_GED + college + young + older +
                               male_upper_class + white_poverty + black_older + poverty_older,
                             data = df_final,
                             family = binomial(link = "logit"))

#' Best High Blood Pressure
#' @export
Best_High_Blood_Pressure <- glm(high_blood_pressure ~ male + white + black + asian + alaska_indian +
                                  hispanic + poverty + upper_class + no_GED + college + young +
                                  older + male_upper_class + white_poverty,
                                data = df_final,
                                family = binomial(link = "logit"))

#' Diabetes
#' @export
Best_Diabetes <- glm(diabetic ~ male + white + black + other_race + poverty + upper_class +
                       no_GED + college + young + older + male_upper_class + white_poverty + poverty_young + poverty_older,
                     data = df_final,
                     family = binomial(link = "logit"))

#' Angina or Coronary
#' @export
Best_Angina_Coronary <- glm(angina ~ male + white + black + other_race + poverty +
                              upper_class + no_GED + college + young + older + black_older + poverty_older,
                            data = df_final,
                            family = binomial(link = "logit"))

#' Evansville Map
evansville <- c(left = -87.70264,
                bottom = 37.83241,
                right = -87.45628,
                top = 38.20472)

#' Get Map
#' @importFrom ggmap get_map
evv_map <- get_map(evansville, maptype = "roadmap")

#' Reading
#' @importFrom sf st_read st_transform
evv <- st_read("Census_Tracts.shp")
#' Transform
evv <- st_transform(evv, crs = "+proj=longlat +datumWGS84 +no_defs +ellps=WGS84")
#' Frame
#' @importFrom dplyr filter select group_by left_join summarise transmute
evv <- data.frame(evv) %>% transmute(geometry, GEOID = as.character(GEOID10))

#' #' Census Key
#' #' @importFrom tidycensus census_api_key get_acs
#' census_api_key("94bc4d9ee635889131819300fb54c51a0a92b05f", install = FALSE)
#'
#' #' Education Tract
#' Education_tract <- data.frame(get_acs(geography = "tract",
#'                                       geometry = FALSE,
#'                                       state = 18,
#'                                       county = "163",
#'                                       variables = c(education_num_counted = "B06009_001",
#'                                                     No_GED = "B06009_002",
#'                                                     High_School_Grad = "B06009_003",
#'                                                     Some_college_or_associates = "B06009_004",
#'                                                     Bachelors_degree = "B06009_005",
#'                                                     Professional_degree = "B06009_006"
#'                                                     )
#'                                       )
#'                               )
#' #' Sorting Education Data
#' No_GED <- Education_tract %>%
#'   filter(variable == "No_GED") %>%
#'   select(GEOID, no_GED = estimate)
#'
#' #' High School
#' High_School <- Education_tract %>%
#'   filter(variable == "High_School_Grad") %>%
#'   select(GEOID, high_school_grad = estimate)
#'
#' #' College
#' College <- Education_tract %>%
#'   filter(variable %in% c("Some_college_or_associates", "Bachelors_degree", "Professional_degree")) %>%
#'   group_by(GEOID) %>%
#'   summarise(college = sum(estimate))
#'
#' #' Full Census
#' Census_data <- No_GED %>%
#'   left_join(High_School, by = "GEOID") %>%
#'   left_join(College, by = "GEOID")
#'
#' #' Sex Data
#' Sex_tract <- data.frame(get_acs(geography = "tract",
#'                                 geometry = FALSE,
#'                                 state = 18,
#'                                 county = "163",
#'                                 variables = c(Males = "B01001_002")
#'                                 )
#'                         ) %>% select(GEOID, males = estimate)
#' #' Joining Sex
#' Census_data <- Census_data %>% left_join(Sex_tract, by = "GEOID")
#'
#' #' Income Data
#' Income_tract <- data.frame(get_acs(geography = "tract",
#'                                    geometry = FALSE,
#'                                    state = 18,
#'                                    county = "163",
#'                                    variables = c(income_00_10 = "B19001_002",
#'                                                  income_10_15 = "B19001_003",
#'                                                  income_15_20 = "B19001_004",
#'                                                  income_20_25 = "B19001_005",
#'                                                  income_25_30 = "B19001_006",
#'                                                  income_30_35 = "B19001_007",
#'                                                  income_35_40 = "B19001_008",
#'                                                  income_40_45 = "B19001_009",
#'                                                  income_45_50 = "B19001_010",
#'                                                  income_50_60 = "B19001_011",
#'                                                  income_60_75 = "B19001_012",
#'                                                  income_75_100 = "B19001_013",
#'                                                  income_100_125 = "B19001_014",
#'                                                  income_125_150 = "B19001_015",
#'                                                  income_150_200 = "B19001_016",
#'                                                  income_200_up = "B19001_017"
#'                                                  )
#'                                    )
#'                            )
#' #' Poverty
#' Poverty <- Income_tract %>%
#'   filter(variable %in% c("income_00_10", "income_10_15", "income_15_20", "income_20_25")) %>%
#'   group_by(GEOID) %>%
#'   summarise(poverty = sum(estimate))
#'
#' #' Middle Class
#' Middle_Class <- Income_tract %>% filter(variable %in% c("income_25_30","income_30_35","income_35_40","income_40_45","income_45_50","income_50_55","income_55_60","income_60_65","income_65_70","income_70_75")) %>%
#'   group_by(GEOID) %>%
#'   summarise(middle_class = sum(estimate))
#'
#' #' Upper Class
#' Upper_Class <- Income_tract %>% filter(variable %in% c("income_75_100","income_100_125","income_125_150","income_150_200","income_200_up")) %>%
#'   group_by(GEOID) %>%
#'   summarise(upper_class = sum(estimate))
#'
#' #' Census Join Income
#' Census_data <- Census_data %>%
#'   left_join(Poverty, by = "GEOID") %>%
#'   left_join(Middle_Class, by = "GEOID") %>%
#'   left_join(Upper_Class, by = "GEOID")
#'
#' #' Age Data
#' Age_data <- data.frame(get_acs(geography = "tract",
#'                                geometry = FALSE,
#'                                state = 18,
#'                                county = "163",
#'                                variables = c(Male_Age_18_19 = "B01001_007",
#'                                              Male_Age_20 = "B01001_008",
#'                                              Male_Age_21 = "B01001_009",
#'                                              Male_Age_22_24 = "B01001_010",
#'                                              Male_Age_25_29 = "B01001_011",
#'                                              Male_Age_30_34 = "B01001_012",
#'                                              Male_Age_35_39 = "B01001_013",
#'                                              Male_Age_40_44 = "B01001_014",
#'                                              Male_Age_45_49 = "B01001_015",
#'                                              Male_Age_50_54 = "B01001_016",
#'                                              Male_Age_55_59 = "B01001_017",
#'                                              Male_Age_60_61 = "B01001_018",
#'                                              Male_Age_62_64 = "B01001_019",
#'                                              Male_Age_65_66 = "B01001_020",
#'                                              Male_Age_67_69 = "B01001_021",
#'                                              Male_Age_70_74 = "B01001_022",
#'                                              Male_Age_75_79 = "B01001_023",
#'                                              Male_Age_80_84 = "B01001_024",
#'                                              Male_Age_85_over = "B01001_025",
#'                                              Female_Age_18_19 = "B01001_031",
#'                                              Female_Age_20 = "B01001_032",
#'                                              Female_Age_21 = "B01001_033",
#'                                              Female_Age_22_24 = "B01001_034",
#'                                              Female_Age_25_29 = "B01001_035",
#'                                              Female_Age_30_34 = "B01001_036",
#'                                              Female_Age_35_39 = "B01001_037",
#'                                              Female_Age_40_44 = "B01001_038",
#'                                              Female_Age_45_49 = "B01001_039",
#'                                              Female_Age_50_54 = "B01001_040",
#'                                              Female_Age_55_59 = "B01001_041",
#'                                              Female_Age_60_61 = "B01001_042",
#'                                              Female_Age_62_64 = "B01001_043",
#'                                              Female_Age_65_66 = "B01001_044",
#'                                              Female_Age_67_69 = "B01001_045",
#'                                              Female_Age_70_74 = "B01001_046",
#'                                              Female_Age_75_79 = "B01001_047",
#'                                              Female_Age_80_84 = "B01001_048",
#'                                              Female_Age_85_over = "B01001_049"
#'                                )
#' )
#' )
#'
#' #' Young
#' Young <- Age_data %>%
#'   filter(
#'     variable %in% c(
#'       "Male_Age_18_19",
#'       "Female_Age_18_19",
#'       "Male_Age_20",
#'       "Female_Age_20",
#'       "Male_Age_21",
#'       "Female_Age_21",
#'       "Male_Age_22_24",
#'       "Female_Age_22_24",
#'       "Male_Age_25_29",
#'       "Female_Age_25_29",
#'       "Male_Age_30_34",
#'       "Female_Age_30_34")
#'   ) %>%
#'   group_by(GEOID) %>%
#'   summarise(young = sum(estimate))
#'
#' #' Older
#' Older <- Age_data %>%
#'   filter(
#'     variable %in% c(
#'       "Male_Age_55_59",
#'       "Female_Age_55_59",
#'       "Male_Age_60_61",
#'       "Female_Age_60_61",
#'       "Male_Age_62_64",
#'       "Female_Age_62_64",
#'       "Male_Age_65_66",
#'       "Female_Age_65_66",
#'       "Male_Age_67_69",
#'       "Female_Age_67_69",
#'       "Male_Age_70_74",
#'       "Female_Age_70_74",
#'       "Male_Age_75_79",
#'       "Female_Age_75_79",
#'       "Male_Age_80_84",
#'       "Female_Age_80_84",
#'       "Male_Age_85_over",
#'       "Female_Age_85_over")
#'   ) %>%
#'   group_by(GEOID) %>%
#'   summarise(older = sum(estimate))
#'
#' #' Join Census and Age Data
#' Census_data <- Census_data %>% left_join(Young, by = "GEOID") %>% left_join(Older, by = "GEOID")
#'
#' #' Interactions
#' Interactions <- data.frame(get_acs(geography = "tract",
#'                                    geometry = FALSE,
#'                                    state = 18,
#'                                    county = "163",
#'                                    variables = c(White_Below_Poverty = "B17020A_002",
#'                                                  Black_Male_55_64 = "B01001B_013",
#'                                                  Black_Male_65_74 = "B01001B_014",
#'                                                  Black_Male_75_84 = "B01001B_015",
#'                                                  Black_Male_85_over = "B01001B_016",
#'                                                  Black_Female_55_64 = "B01001B_028",
#'                                                  Black_Female_65_74 = "B01001B_029",
#'                                                  Black_Female_75_84 = "B01001B_030",
#'                                                  Black_Female_85_over = "B01001B_031",
#'                                                  Male_Poverty_18_24 = "B17001_010",
#'                                                  Male_Poverty_25_34 = "B17001_011",
#'                                                  Female_Poverty_18_24 = "B17001_024",
#'                                                  Female_Poverty_25_34 = "B17001_025",
#'                                                  Male_Poverty_55_64 = "B17001_014",
#'                                                  Male_Poverty_65_74 = "B17001_015",
#'                                                  Male_Poverty_75_over = "B17001_016",
#'                                                  Female_Poverty_55_64 = "B17001_028",
#'                                                  Female_Poverty_65_74 = "B17001_029",
#'                                                  Female_Poverty_75_over = "B17001_030",
#'                                                  Hispanic_Male_55_64 = "B01001I_013",
#'                                                  Hispanic_Male_65_74 = "B01001I_014",
#'                                                  Hispanic_Male_75_84 = "B01001I_015",
#'                                                  Hispanic_Male_85_over = "B01001I_016",
#'                                                  Hispanic_Female_55_64 = "B01001I_028",
#'                                                  Hispanic_Female_65_74 = "B01001I_029",
#'                                                  Hispanic_Female_75_84 = "B01001I_030",
#'                                                  Hispanic_Female_85_over = "B01001I_031",
#'                                                  Male_Income_75_100 = "B20001_021",
#'                                                  Male_Income_100_over = "B20001_022")
#' )
#' )
#'
#' #' White Poverty Interaction
#' White_Poverty <- Interactions %>%
#'   filter(variable == "White_Below_Poverty") %>%
#'   select(GEOID, white_poverty = estimate)
#'
#' #' Black Older Interaction
#' Black_Older <- Interactions %>%
#'   filter(variable %in% c("Black_Male_55_64",
#'                          "Black_Female_55_64",
#'                          "Black_Male_65_74",
#'                          "Black_Female_65_74",
#'                          "Black_Male_75_84",
#'                          "Black_Female_75_84",
#'                          "Black_Male_85_over",
#'                          "Black_Female_85_over")) %>%
#'   group_by(GEOID) %>%
#'   summarise(black_older = sum(estimate))
#'
#' #' Povery Older Interaction
#' Poverty_Older <- Interactions %>%
#'   filter(variable %in% c("Male_Poverty_55_64",
#'                          "Female_Poverty_55_64",
#'                          "Male_Poverty_65_74",
#'                          "Female_Poverty_65_74",
#'                          "Male_Poverty_75_over",
#'                          "Female_Poverty_75_over")) %>%
#'   group_by(GEOID) %>%
#'   summarise(poverty_older = sum(estimate))
#'
#' #' Hispanic Older Interaction
#' Hispanic_Older <- Interactions %>%
#'   filter(variable %in% c("Hispanic_Male_55_64",
#'                          "Hispanic_Female_55_64",
#'                          "Hispanic_Male_65_74",
#'                          "Hispanic_Female_65_74",
#'                          "Hispanic_Male_75_84",
#'                          "Hispanic_Female_75_84",
#'                          "Hispanic_Male_85_over",
#'                          "Hispanic_Female_85_over")) %>%
#'   group_by(GEOID) %>%
#'   summarise(hispanic_older = sum(estimate))
#'
#' #' Poverty Young Interaction
#' Poverty_Young <- Interactions %>%
#'   filter(variable %in% c("Male_Poverty_18_24",
#'                          "Female_Poverty_18_24",
#'                          "Male_Poverty_25_34",
#'                          "Female_Poverty_25_34")) %>%
#'   group_by(GEOID) %>%
#'   summarise(poverty_young = sum(estimate))
#'
#' #' Male Upper Class Interaction
#' Male_Upper_Class <- Interactions %>%
#'   filter(variable %in% c("Male_Income_75_100","Male_Income_100_over")) %>%
#'   group_by(GEOID) %>%
#'   summarise(male_upper_class = sum(estimate))
#'
#' #' Census Interaction Join
#' Census_data <- Census_data %>%
#'   left_join(White_Poverty, by = "GEOID") %>%
#'   left_join(Black_Older, by = "GEOID") %>%
#'   left_join(Poverty_Older, by = "GEOID") %>%
#'   left_join(Hispanic_Older, by = "GEOID") %>%
#'   left_join(Poverty_Young, by = "GEOID") %>%
#'   left_join(Male_Upper_Class, by = "GEOID")
#'
#' #' Final Data Frame
#' eville <- evv %>% mutate(GEOID = as.character(GEOID)) %>% left_join(Census_data, by = "GEOID")

#' Complete Data Frame
eville <- evv %>% left_join(Census_data, by = "GEOID")

#' Convert Census Data
eville <- eville %>% mutate(white = white / Pop_18_and,
                            black = black / Pop_18_and,
                            asian = asian / Pop_18_and,
                            alaska_indian = alaska_indian / Pop_18_and,
                            hispanic = hispanic / Pop_18_and,
                            other_race = (asian + alaska_indian + hispanic) / Pop_18_and,
                            male = males / Pop_18_and,
                            poverty = poverty / Pop_18_and,
                            upper_class = upper_class / Pop_18_and,
                            no_GED = no_GED / Pop_18_and,
                            high_school = high_school_grad / Pop_18_and,
                            college = college / Pop_18_and,
                            young = young / Pop_18_and,
                            older = older / Pop_18_and,
                            white_poverty = white_poverty / Pop_18_and,
                            black_older = black_older / Pop_18_and,
                            poverty_older = poverty_older / Pop_18_and,
                            hispanic_older = hispanic_older / Pop_18_and,
                            poverty_young = poverty_young / Pop_18_and,
                            male_upper_class = male_upper_class / Pop_18_and
                            )

#' Prediction Values
#' @export
eville <- eville %>% mutate(Proportion_Smokers = predict.glm(Best_Smoking, newdata = eville, type = "response"),
                            Predicted_Number_Smokers = predict.glm(Best_Smoking, newdata = eville, type = "response") * Pop_18_and,
                            Difference_From_Average_Smokers = (predict.glm(Best_Smoking, newdata = eville, type = "response") - mean(df_final$smoke)),
                            Proportion_Overweight = predict.glm(Best_Overweight, newdata = eville, type = "response"),
                            Predicted_Number_Overweight = predict.glm(Best_Overweight, newdata = eville, type = "response") * Pop_18_and,
                            Difference_From_Average_Overweight = (predict.glm(Best_Overweight, newdata = eville, type = "response") - mean(df_final$overweight)),
                            Proportion_Asthma = predict.glm(Best_Asthma, newdata = eville, type = "response"),
                            Predicted_Number_Asthma = predict.glm(Best_Asthma, newdata = eville, type = "response") * Pop_18_and,
                            Difference_From_Average_Asthma = (predict.glm(Best_Asthma, newdata = eville, type = "response") - mean(df_final$asthma)),
                            Proportion_Binge = predict.glm(Best_Binge_Drinker, newdata = eville, type = "response"),
                            Predicted_Number_Binge = predict.glm(Best_Binge_Drinker, newdata = eville, type = "response") * Pop_18_and,
                            Difference_From_Average_Binge = (predict.glm(Best_Binge_Drinker, newdata = eville, type = "response") - mean(df_final$binge_drinker)),
                            Proportion_Arthritis = predict.glm(Best_Arthritis, newdata = eville, type = "response"),
                            Predicted_Number_Arthritis = predict.glm(Best_Arthritis, newdata = eville, type = "response") * Pop_18_and,
                            Difference_From_Average_Arthritis = (predict.glm(Best_Arthritis, newdata = eville, type = "response") - mean(df_final$arthritis)),
                            Proportion_High_BP = predict.glm(Best_High_Blood_Pressure, newdata = eville, type = "response"),
                            Predicted_Number_High_BP = predict.glm(Best_High_Blood_Pressure, newdata = eville, type = "response") * Pop_18_and,
                            Difference_From_Average_High_BP = (predict.glm(Best_High_Blood_Pressure, newdata = eville, type = "response") - mean(df_final$high_blood_pressure)),
                            Proportion_Angina = predict.glm(Best_Angina_Coronary, newdata = eville, type = "response"),
                            Predicted_Number_Angina = predict.glm(Best_Angina_Coronary, newdata = eville, type = "response") * Pop_18_and,
                            Difference_From_Average_Angina = (predict.glm(Best_Angina_Coronary, newdata = eville, type = "response") - mean(df_final$angina)),
                            Proportion_Depression = predict.glm(Best_Depression, newdata = eville, type = "response"),
                            Predicted_Number_Depression = predict.glm(Best_Depression, newdata = eville, type = "response") * Pop_18_and,
                            Difference_From_Average_Depression = (predict.glm(Best_Depression, newdata = eville, type = "response") - mean(df_final$depression)),
                            Proportion_Diabetes = predict.glm(Best_Diabetes, newdata = eville, type = "response"),
                            Predicted_Number_Diabetes = predict.glm(Best_Diabetes, newdata = eville, type = "response") * Pop_18_and,
                            Difference_From_Average_Diabetes = (predict.glm(Best_Diabetes, newdata = eville, type = "response") - mean(df_final$diabetic)),
                            Proportion_High_Cholesterol = predict.glm(Best_High_Cholesterol, newdata = eville, type = "response"),
                            Predicted_Number_High_Cholesterol = predict.glm(Best_High_Cholesterol, newdata = eville, type = "response") * Pop_18_and,
                            Difference_From_Average_High_Cholesterol = (predict.glm(Best_High_Cholesterol, newdata = eville, type = "response") - mean(df_final$high_cholesterol)))


#' Find correct ggplot to return
#'
#' Returns ggplot
#' @param input1 Selected response from shiny UI
#' @param input2 Selected plot type from shiny UI
#' @return correct ggplot
#' @export
#' @importFrom ggplot2 scale_fill_viridis_c ggtitle theme
prediction_map <- function(input1,input2) {
  if(input1 == "Smoking" & input2 == "Predicted Proportion") { return(ggmap(evv_map) +
                                                                        geom_sf(aes(fill = Proportion_Smokers, geometry = geometry),
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
                                                                              geom_sf(aes(fill = Predicted_Number_Smokers, geometry = geometry),
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
                                                                                             geom_sf(aes(fill = Difference_From_Average_Smokers, geometry = geometry),
                                                                                                     inherit.aes=FALSE,
                                                                                                     alpha = .75,
                                                                                                     data = eville) +
                                                                                             scale_fill_viridis_c(option = "B") +
                                                                                             ggtitle("Difference from Average for Smoking") +
                                                                                             theme(plot.title = element_text(size = rel(1.3)),
                                                                                                   axis.text.x = element_blank(),
                                                                                                   axis.text.y = element_blank(),
                                                                                                   axis.title.x = element_blank(),
                                                                                                   axis.title.y = element_blank()
                                                                                             )
                                                                                           )
  }

  if(input1 == "Overweight" & input2 == "Predicted Proportion") { return(ggmap(evv_map) +
                                                                           geom_sf(aes(fill = Proportion_Overweight, geometry = geometry),
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
                                                                                 geom_sf(aes(fill = Predicted_Number_Overweight, geometry = geometry),
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
                                                                                                geom_sf(aes(fill = Difference_From_Average_Overweight, geometry = geometry),
                                                                                                        inherit.aes=FALSE,
                                                                                                        alpha = .75,
                                                                                                        data = eville) +
                                                                                                scale_fill_viridis_c(option = "B") +
                                                                                                ggtitle("Difference from Average for Overweight") +
                                                                                                theme(plot.title = element_text(size = rel(1.3)),
                                                                                                      axis.text.x = element_blank(),
                                                                                                      axis.text.y = element_blank(),
                                                                                                      axis.title.x = element_blank(),
                                                                                                      axis.title.y = element_blank()
                                                                                                )
                                                                                              )
  }

  if(input1 == "Asthma" & input2 == "Predicted Proportion") { return(ggmap(evv_map) +
                                                                       geom_sf(aes(fill = Proportion_Asthma, geometry = geometry),
                                                                               inherit.aes=FALSE,
                                                                               alpha = .75,
                                                                               data = eville) +
                                                                       scale_fill_viridis_c(option = "B") +
                                                                       ggtitle("Predicted Proportion of Asthmatics") +
                                                                       theme(plot.title = element_text(size = rel(1.3)),
                                                                             axis.text.x = element_blank(),
                                                                             axis.text.y = element_blank(),
                                                                             axis.title.x = element_blank(),
                                                                             axis.title.y = element_blank()
                                                                       )
                                                                     )
  }

  if(input1 == "Asthma" & input2 == "Predicted Number of People") { return(ggmap(evv_map) +
                                                                             geom_sf(aes(fill = Predicted_Number_Asthma, geometry = geometry),
                                                                                     inherit.aes=FALSE,
                                                                                     alpha = .75,
                                                                                     data = eville) +
                                                                             scale_fill_viridis_c(option = "B") +
                                                                             ggtitle("Predicted Number of Asthmatic Citizens") +
                                                                             theme(plot.title = element_text(size = rel(1.3)),
                                                                                   axis.text.x = element_blank(),
                                                                                   axis.text.y = element_blank(),
                                                                                   axis.title.x = element_blank(),
                                                                                   axis.title.y = element_blank()
                                                                             )
                                                                           )
  }

  if(input1 == "Asthma" & input2 == "Predicted Difference From Indiana Average") { return(ggmap(evv_map) +
                                                                                            geom_sf(aes(fill = Difference_From_Average_Asthma,
                                                                                                        geometry = geometry),
                                                                                                        inherit.aes=FALSE,
                                                                                                        alpha = .75,
                                                                                                        data = eville) +
                                                                                            scale_fill_viridis_c(option = "B") +
                                                                                            ggtitle("Difference from Average for Asthmatics") +
                                                                                            theme(plot.title = element_text(size = rel(1.3)),
                                                                                                  axis.text.x = element_blank(),
                                                                                                  axis.text.y = element_blank(),
                                                                                                  axis.title.x = element_blank(),
                                                                                                  axis.title.y = element_blank()
                                                                                            )
                                                                                          )
  }

  if(input1 == "Binge Drinking" & input2 == "Predicted Proportion") { return(ggmap(evv_map) +
                                                                               geom_sf(aes(fill = Proportion_Binge, geometry = geometry),
                                                                                       inherit.aes=FALSE,
                                                                                       alpha = .75,
                                                                                       data = eville) +
                                                                               scale_fill_viridis_c(option = "B") +
                                                                               ggtitle("Predicted Proportion of Binge Drinkers") +
                                                                               theme(plot.title = element_text(size = rel(1.3)),
                                                                                     axis.text.x = element_blank(),
                                                                                     axis.text.y = element_blank(),
                                                                                     axis.title.x = element_blank(),
                                                                                     axis.title.y = element_blank()
                                                                               )
                                                                             )
  }

  if(input1 == "Binge Drinking" & input2 == "Predicted Number of People") { return(ggmap(evv_map) +
                                                                                     geom_sf(aes(fill = Predicted_Number_Binge, geometry = geometry),
                                                                                             inherit.aes=FALSE,
                                                                                             alpha = .75,
                                                                                             data = eville) +
                                                                                     scale_fill_viridis_c(option = "B") +
                                                                                     ggtitle("Predicted Number of Binge Drinkers") +
                                                                                     theme(plot.title = element_text(size = rel(1.3)),
                                                                                           axis.text.x = element_blank(),
                                                                                           axis.text.y = element_blank(),
                                                                                           axis.title.x = element_blank(),
                                                                                           axis.title.y = element_blank()
                                                                                     )
                                                                                   )
  }

  if(input1 == "Binge Drinking" & input2 == "Predicted Difference From Indiana Average") { return(ggmap(evv_map) +
                                                                                                    geom_sf(aes(fill = Difference_From_Average_Binge, geometry = geometry),
                                                                                                            inherit.aes=FALSE,
                                                                                                            alpha = .75,
                                                                                                            data = eville) +
                                                                                                    scale_fill_viridis_c(option = "B") +
                                                                                                    ggtitle("Difference from Average for Binge Drinkers") +
                                                                                                    theme(plot.title = element_text(size = rel(1.3)),
                                                                                                          axis.text.x = element_blank(),
                                                                                                          axis.text.y = element_blank(),
                                                                                                          axis.title.x = element_blank(),
                                                                                                          axis.title.y = element_blank()
                                                                                                    )
                                                                                                  )
  }

  if(input1 == "Arthritis" & input2 == "Predicted Proportion") { return(ggmap(evv_map) +
                                                                         geom_sf(aes(fill = Proportion_Arthritis, geometry = geometry),
                                                                                 inherit.aes=FALSE,
                                                                                 alpha = .75,
                                                                                 data = eville) +
                                                                         scale_fill_viridis_c(option = "B") +
                                                                         ggtitle("Predicted Proportion of Citizens with Arthritis") +
                                                                         theme(plot.title = element_text(size = rel(1.3)),
                                                                               axis.text.x = element_blank(),
                                                                               axis.text.y = element_blank(),
                                                                               axis.title.x = element_blank(),
                                                                               axis.title.y = element_blank()
                                                                         )
                                                                       )
  }

  if(input1 == "Arthritis" & input2 == "Predicted Number of People") { return(ggmap(evv_map) +
                                                                               geom_sf(aes(fill = Predicted_Number_Arthritis, geometry = geometry),
                                                                                       inherit.aes=FALSE,
                                                                                       alpha = .75,
                                                                                       data = eville) +
                                                                               scale_fill_viridis_c(option = "B") +
                                                                               ggtitle("Predicted Number of Citizens with Arthritis") +
                                                                               theme(plot.title = element_text(size = rel(1.3)),
                                                                                     axis.text.x = element_blank(),
                                                                                     axis.text.y = element_blank(),
                                                                                     axis.title.x = element_blank(),
                                                                                     axis.title.y = element_blank()
                                                                               )
                                                                              )
  }

  if(input1 == "Arthritis" & input2 == "Predicted Difference From Indiana Average") { return(ggmap(evv_map) +
                                                                                              geom_sf(aes(fill = Difference_From_Average_Arthritis, geometry = geometry),
                                                                                                      inherit.aes=FALSE,
                                                                                                      alpha = .75,
                                                                                                      data = eville) +
                                                                                              scale_fill_viridis_c(option = "B") +
                                                                                              ggtitle("Difference from Average for Arthritis") +
                                                                                              theme(plot.title = element_text(size = rel(1.3)),
                                                                                                    axis.text.x = element_blank(),
                                                                                                    axis.text.y = element_blank(),
                                                                                                    axis.title.x = element_blank(),
                                                                                                    axis.title.y = element_blank()
                                                                                              )
                                                                                             )
  }

  if(input1 == "High Blood Pressure" & input2 == "Predicted Proportion") { return(ggmap(evv_map) +
                                                                                    geom_sf(aes(fill = Proportion_High_BP, geometry = geometry),
                                                                                            inherit.aes=FALSE,
                                                                                            alpha = .75,
                                                                                            data = eville) +
                                                                                    scale_fill_viridis_c(option = "B") +
                                                                                    ggtitle("Predicted Proportion of Citizens with High Blood Pressure") +
                                                                                    theme(plot.title = element_text(size = rel(1.3)),
                                                                                          axis.text.x = element_blank(),
                                                                                          axis.text.y = element_blank(),
                                                                                          axis.title.x = element_blank(),
                                                                                          axis.title.y = element_blank()
                                                                                    )
                                                                                  )
  }

  if(input1 == "High Blood Pressure" & input2 == "Predicted Number of People") { return(ggmap(evv_map) +
                                                                                          geom_sf(aes(fill = Predicted_Number_High_BP, geometry = geometry),
                                                                                                  inherit.aes=FALSE,
                                                                                                  alpha = .75,
                                                                                                  data = eville) +
                                                                                          scale_fill_viridis_c(option = "B") +
                                                                                          ggtitle("Predicted Number of Citizens with High Blood Pressure") +
                                                                                          theme(plot.title = element_text(size = rel(1.3)),
                                                                                                axis.text.x = element_blank(),
                                                                                                axis.text.y = element_blank(),
                                                                                                axis.title.x = element_blank(),
                                                                                                axis.title.y = element_blank()
                                                                                          )
                                                                                        )
  }

  if(input1 == "High Blood Pressure" & input2 == "Predicted Difference From Indiana Average") { return(ggmap(evv_map) +
                                                                                                         geom_sf(aes(fill = Difference_From_Average_High_BP, geometry = geometry),
                                                                                                                 inherit.aes=FALSE,
                                                                                                                 alpha = .75,
                                                                                                                 data = eville) +
                                                                                                         scale_fill_viridis_c(option = "B") +
                                                                                                         ggtitle("Difference from Average for Citizens with High Blood Pressure") +
                                                                                                         theme(plot.title = element_text(size = rel(1.3)),
                                                                                                               axis.text.x = element_blank(),
                                                                                                               axis.text.y = element_blank(),
                                                                                                               axis.title.x = element_blank(),
                                                                                                               axis.title.y = element_blank()
                                                                                                         )
                                                                                                       )
  }

  if(input1 == "Angina or Coronary Heart Disease" & input2 == "Predicted Proportion") { return(ggmap(evv_map) +
                                                                                                 geom_sf(aes(fill = Proportion_Angina, geometry = geometry),
                                                                                                         inherit.aes=FALSE,
                                                                                                         alpha = .75,
                                                                                                         data = eville) +
                                                                                                 scale_fill_viridis_c(option = "B") +
                                                                                                 ggtitle("Predicted Proportion of Citizens with Angina or Coronary Heart Disease") +
                                                                                                 theme(plot.title = element_text(size = rel(1.3)),
                                                                                                       axis.text.x = element_blank(),
                                                                                                       axis.text.y = element_blank(),
                                                                                                       axis.title.x = element_blank(),
                                                                                                       axis.title.y = element_blank()
                                                                                                 )
                                                                                               )
  }

  if(input1 == "Angina or Coronary Heart Disease" & input2 == "Predicted Number of People") { return(ggmap(evv_map) +
                                                                                                       geom_sf(aes(fill = Predicted_Number_Angina, geometry = geometry),
                                                                                                               inherit.aes=FALSE,
                                                                                                               alpha = .75,
                                                                                                               data = eville) +
                                                                                                       scale_fill_viridis_c(option = "B") +
                                                                                                       ggtitle("Predicted Number of Citizens with Angina or Coronary Heart Disease") +
                                                                                                       theme(plot.title = element_text(size = rel(1.3)),
                                                                                                             axis.text.x = element_blank(),
                                                                                                             axis.text.y = element_blank(),
                                                                                                             axis.title.x = element_blank(),
                                                                                                             axis.title.y = element_blank()
                                                                                                       )
                                                                                                     )
  }

  if(input1 == "Angina or Coronary Heart Disease" & input2 == "Predicted Difference From Indiana Average") { return(ggmap(evv_map) +
                                                                                                                      geom_sf(aes(fill = Difference_From_Average_Angina, geometry = geometry),
                                                                                                                              inherit.aes=FALSE,
                                                                                                                              alpha = .75,
                                                                                                                              data = eville) +
                                                                                                                      scale_fill_viridis_c(option = "B") +
                                                                                                                      ggtitle("Difference from Average for Citizens with Angina or Coronary Heart Disease") +
                                                                                                                      theme(plot.title = element_text(size = rel(1.3)),
                                                                                                                            axis.text.x = element_blank(),
                                                                                                                            axis.text.y = element_blank(),
                                                                                                                            axis.title.x = element_blank(),
                                                                                                                            axis.title.y = element_blank()
                                                                                                                      )
                                                                                                                    )
  }

  if(input1 == "Depression" & input2 == "Predicted Proportion") { return(ggmap(evv_map) +
                                                                           geom_sf(aes(fill = Proportion_Depression, geometry = geometry),
                                                                                   inherit.aes=FALSE,
                                                                                   alpha = .75,
                                                                                   data = eville) +
                                                                           scale_fill_viridis_c(option = "B") +
                                                                           ggtitle("Predicted Proportion of Citizens with Depression") +
                                                                           theme(plot.title = element_text(size = rel(1.3)),
                                                                                 axis.text.x = element_blank(),
                                                                                 axis.text.y = element_blank(),
                                                                                 axis.title.x = element_blank(),
                                                                                 axis.title.y = element_blank()
                                                                           )
                                                                         )
  }

  if(input1 == "Depression" & input2 == "Predicted Number of People") { return(ggmap(evv_map) +
                                                                                 geom_sf(aes(fill = Predicted_Number_Depression, geometry = geometry),
                                                                                         inherit.aes=FALSE,
                                                                                         alpha = .75,
                                                                                         data = eville) +
                                                                                 scale_fill_viridis_c(option = "B") +
                                                                                 ggtitle("Predicted Number of Citizens with Depression") +
                                                                                 theme(plot.title = element_text(size = rel(1.3)),
                                                                                       axis.text.x = element_blank(),
                                                                                       axis.text.y = element_blank(),
                                                                                       axis.title.x = element_blank(),
                                                                                       axis.title.y = element_blank()
                                                                                 )
                                                                               )
  }

  if(input1 == "Depression" & input2 == "Predicted Difference From Indiana Average") { return(ggmap(evv_map) +
                                                                                                geom_sf(aes(fill = Difference_From_Average_Depression, geometry = geometry),
                                                                                                        inherit.aes=FALSE,
                                                                                                        alpha = .75,
                                                                                                        data = eville) +
                                                                                                scale_fill_viridis_c(option = "B") +
                                                                                                ggtitle("Difference from Average for Citizens with Depression") +
                                                                                                theme(plot.title = element_text(size = rel(1.3)),
                                                                                                      axis.text.x = element_blank(),
                                                                                                      axis.text.y = element_blank(),
                                                                                                      axis.title.x = element_blank(),
                                                                                                      axis.title.y = element_blank()
                                                                                                )
                                                                                              )
  }

  if(input1 == "High Cholesterol" & input2 == "Predicted Proportion") { return(ggmap(evv_map) +
                                                                                 geom_sf(aes(fill = Proportion_High_Cholesterol, geometry = geometry),
                                                                                         inherit.aes=FALSE,
                                                                                         alpha = .75,
                                                                                         data = eville) +
                                                                                 scale_fill_viridis_c(option = "B") +
                                                                                 ggtitle("Predicted Proportion of Citizens with High Cholesterol") +
                                                                                 theme(plot.title = element_text(size = rel(1.3)),
                                                                                       axis.text.x = element_blank(),
                                                                                       axis.text.y = element_blank(),
                                                                                       axis.title.x = element_blank(),
                                                                                       axis.title.y = element_blank()
                                                                                 )
                                                                               )
  }

  if(input1 == "High Cholesterol" & input2 == "Predicted Number of People") { return(ggmap(evv_map) +
                                                                                       geom_sf(aes(fill = Predicted_Number_High_Cholesterol, geometry = geometry),
                                                                                               inherit.aes=FALSE,
                                                                                               alpha = .75,
                                                                                               data = eville) +
                                                                                       scale_fill_viridis_c(option = "B") +
                                                                                       ggtitle("Predicted Number of Citizens with High Cholesterol") +
                                                                                       theme(plot.title = element_text(size = rel(1.3)),
                                                                                             axis.text.x = element_blank(),
                                                                                             axis.text.y = element_blank(),
                                                                                             axis.title.x = element_blank(),
                                                                                             axis.title.y = element_blank()
                                                                                       )
                                                                                     )
  }

  if(input1 == "High Cholesterol" & input2 == "Predicted Difference From Indiana Average") { return(ggmap(evv_map) +
                                                                                                      geom_sf(aes(fill = Difference_From_Average_High_Cholesterol, geometry = geometry),
                                                                                                              inherit.aes=FALSE,
                                                                                                              alpha = .75,
                                                                                                              data = eville) +
                                                                                                      scale_fill_viridis_c(option = "B") +
                                                                                                      ggtitle("Difference from Average for Citizens with High Cholesterol") +
                                                                                                      theme(plot.title = element_text(size = rel(1.3)),
                                                                                                            axis.text.x = element_blank(),
                                                                                                            axis.text.y = element_blank(),
                                                                                                            axis.title.x = element_blank(),
                                                                                                            axis.title.y = element_blank()
                                                                                                      )
                                                                                                    )
  }

  if(input1 == "Diabetes" & input2 == "Predicted Proportion") { return(ggmap(evv_map) +
                                                                         geom_sf(aes(fill = Proportion_Diabetes, geometry = geometry),
                                                                                 inherit.aes=FALSE,
                                                                                 alpha = .75,
                                                                                 data = eville) +
                                                                         scale_fill_viridis_c(option = "B") +
                                                                         ggtitle("Predicted Proportion of Citizens with Diabetes") +
                                                                         theme(plot.title = element_text(size = rel(1.3)),
                                                                               axis.text.x = element_blank(),
                                                                               axis.text.y = element_blank(),
                                                                               axis.title.x = element_blank(),
                                                                               axis.title.y = element_blank()
                                                                         )
                                                                       )
  }

  if(input1 == "Diabetes" & input2 == "Predicted Number of People") { return(ggmap(evv_map) +
                                                                               geom_sf(aes(fill = Predicted_Number_Diabetes, geometry = geometry),
                                                                                       inherit.aes=FALSE,
                                                                                       alpha = .75,
                                                                                       data = eville) +
                                                                               scale_fill_viridis_c(option = "B") +
                                                                               ggtitle("Predicted Number of Citizens with Diabetes") +
                                                                               theme(plot.title = element_text(size = rel(1.3)),
                                                                                     axis.text.x = element_blank(),
                                                                                     axis.text.y = element_blank(),
                                                                                     axis.title.x = element_blank(),
                                                                                     axis.title.y = element_blank()
                                                                               )
                                                                             )
  }

  if(input1 == "Diabetes" & input2 == "Predicted Difference From Indiana Average") { return(ggmap(evv_map) +
                                                                                              geom_sf(aes(fill = Difference_From_Average_Diabetes, geometry = geometry),
                                                                                                      inherit.aes=FALSE,
                                                                                                      alpha = .75,
                                                                                                      data = eville) +
                                                                                              scale_fill_viridis_c(option = "B") +
                                                                                              ggtitle("Difference from Average for Citizens with Diabetes") +
                                                                                              theme(plot.title = element_text(size = rel(1.3)),
                                                                                                    axis.text.x = element_blank(),
                                                                                                    axis.text.y = element_blank(),
                                                                                                    axis.title.x = element_blank(),
                                                                                                    axis.title.y = element_blank()
                                                                                              )
                                                                                            )
  }

}
