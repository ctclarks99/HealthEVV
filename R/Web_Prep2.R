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

#' Model_Check
#'
#' @export
check1 <- print("Model Check")

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
evv <- data.frame(evv) %>% transmute(GEOID = as.character(GEOID10), geometry)


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

#' Else Check
#'
#' @export
check2 <- print("Else Check")

#' Find correct ggplot to return
#'
#' Returns ggplot
#' @param input1 Selected response from shiny UI
#' @param input2 Selected plot type from shiny UI
#' @return correct ggplot
#' @export
#' @import ggplot2
#' @import ggmap
#' @import sf
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
