# microAeth AE51 BC artifact Correction
# by Qiang Yang
# last accessed on 2021-04-27
# progress: 


## load packages
library(ggplot2)
library(plotly)
library(scales)
library(lubridate)
library(data.table)
library(gridExtra)
library(cowplot)
library(plyr)
library(dplyr)
library(grid)
library(zoo)
library(openxlsx)
library(beanplot)
library(readr)





# function to convert the AE51 data into 1-minute timebase
function_onemin_bc <- function(df){
  
  df$minute  <- floor_date(df$timestamp, unit = "minute")
  
  df_out <- dplyr::summarise(group_by(df, minute), 
                             Date = df$Date[1],
                             Time = substr(as.character(mean(minute)), 12, 19),
                             Ref = mean(Ref, na.rm=TRUE),
                             Sen = mean(Sen, na.rm=TRUE),
                             ATN = mean(ATN, na.rm=TRUE),
                             Flow = mean(Flow, na.rm = TRUE),
                             PCB.temp = mean(PCB.temp, na.rm = TRUE),
                             Status = max(Status, na.rm=T),
                             Battery = mean(Battery,na.rm = TRUE),
                             BC = mean(BC,na.rm = TRUE),
                             timestamp = mean(minute)#,
                             #subject = df$subject[1],
                             #unitid = df$unitid[1],
                             #session = df$session[1]
  )
  
  df_out$datetime <- df_out$minute
  df_out <- df_out[ , -1]
  df_out$timebase_s <- 60
  return(df_out)
}



### mark all temperature change events in a session

#differentiate_consecutive_events
function_mark_temp_events_detailed <- function(df) {
  
  max_stable_reading <- 60
  
  df$temp_change <- c(NA, diff(x = df$PCB.temp, lag = 1))
  df$temp_event_number <- 0
  
  if (df$temp_change[2] > 0) {
    df$temp_event_number[2] <- 1
  } else if (df$temp_change[2] < 0) {
    df$temp_event_number[2] <- -1
  } else {
    df$temp_event_number[2] <- 0
  }
  
  for (i in 3:nrow(df)) {
    
    if (df$temp_change[i] == 0) { ###### no temperature change ######
      if ((i - max(which(abs(df[1:(i-1), ]$temp_change) > 0)))  < max_stable_reading) { # max minutes of stable temperature before defining a new event is 60
        df$temp_event_number[i] <- df$temp_event_number[i-1]
        } else {
        df$temp_event_number[i] <- 0 # if euqal or exceeding 60 minutes of stable temperature, define the session as 0
        }
    } else if (df$temp_change[i]  > 0) { ###### temperature incease ######
      if ((i - max(which(abs(df[1:(i-1), ]$temp_change) > 0))) >= max_stable_reading) { # last temp change equal or more than 60 minutes ago, define as a new event, # no temp change in the prior minutes calcualted as -inf, so i - max >= 60
        df$temp_event_number[i] <- ifelse(max(df[2:(i-1), ]$temp_change) <= 0, 1, 
                                          df$temp_event_number[max(which(df[1:(i-1), ]$temp_change > 0))] + 1) 
      } else { #last temp change less than 60 minutes ago
        if (df$temp_change[max(which(abs(df[1:(i-1), ]$temp_change) > 0))] < 0) { # last temperaure change was decrease, define as a new event
          df$temp_event_number[i] <- ifelse(max(df[2:(i-1), ]$temp_change) <= 0, 1, 
                                            df$temp_event_number[max(which(df[1:(i-1), ]$temp_change > 0))] + 1)
        } else { ## last temperaure change was increase
            if ((i-6) > 0 & (i+5) < nrow(df) & sum(df$temp_change[(i+1):(i+5)]) > sum(df$temp_change[(i-1):(i-5)]) & sum(df$temp_change[(i+1):(i+5)]) >=2) { # next two temperature insrease faster than previous two, fedine as a new event
              df$temp_event_number[i] <- ifelse(max(df[2:(i-1), ]$temp_change) <= 0, 1, 
                                                df$temp_event_number[max(which(df[1:(i-1), ]$temp_change > 0))] + 1)
            } else {
              df$temp_event_number[i] <- df$temp_event_number[i-1] # otherwise the same increase event from previous
            }
          } 
      }
    } else if (df$temp_change[i]  < 0) { ###### temperature decrease
      if ((i - max(which(abs(df[1:(i-1), ]$temp_change) > 0)))  >= max_stable_reading) { # last temp change more than 60 minutes ago, define as a new event, # no temp change in the prior minutes calcualted as -inf, so i - max >= 60
        df$temp_event_number[i] <- ifelse(min(df[2:(i-1), ]$temp_change) >= 0, -1, 
                                        df$temp_event_number[max(which(df[1:(i-1), ]$temp_change < 0))] - 1)
      } else { #last temp change less than 60 minutes ago
        if (df$temp_change[max(which(abs(df[1:(i-1), ]$temp_change) > 0))] > 0) { #last temp change within 60 minutes and temp increase, define as a new event
          df$temp_event_number[i] <- ifelse(min(df[2:(i-1), ]$temp_change) >= 0, -1, 
                                        df$temp_event_number[max(which(df[1:(i-1), ]$temp_change < 0))] - 1)
        } else {## last temperaure change was decrease
          if ((i-6) > 0 & (i+5) < nrow(df) & sum(df$temp_change[(i+1):(i+5)]) < sum(df$temp_change[(i-1):(i-5)]) & sum(df$temp_change[(i+1):(i+5)]) <= -2) {# next two temperature decrease faster than previous two, fedine as a new event
            df$temp_event_number[i] <- ifelse(min(df[2:(i-1), ]$temp_change) >= 0, -1, 
                                        df$temp_event_number[max(which(df[1:(i-1), ]$temp_change < 0))] - 1)
          } else {
            df$temp_event_number[i] <- df$temp_event_number[i-1] # otherwise the same devrease event from previous
          }
        }
      }
    } else {
      df$temp_event_number[i] <- 0
    }
  } # END of calculating temp_event_number 
  
  df$temp_event_number <- c(df$temp_event_number[2:nrow(df)], NA) # 
  
  # label how much temperature change in each event
  df$event_temp_change <- 0
  
  for (j in 1:nrow(df)) {
    df$event_temp_change[j] <- ifelse(df$temp_event_number[j] > 0, 
                                      max(subset(df, temp_event_number == df$temp_event_number[j])$PCB.temp, na.rm = T) - min(subset(df, temp_event_number == df$temp_event_number[j])$PCB.temp, na.rm = T),
                                      ifelse(df$temp_event_number[j] < 0, 
                                             min(subset(df, temp_event_number == df$temp_event_number[j])$PCB.temp, na.rm = T) - max(subset(df, temp_event_number == df$temp_event_number[j])$PCB.temp, na.rm = T),
                                             0))
  }
  
  df$event_temp_change <- ifelse(abs(df$event_temp_change) <= 1, 0, df$event_temp_change)
  
  return(df)
}



# NOT differentiate_consecutive_events
function_mark_temp_events <- function(df) {
  
  max_stable_reading <- 60
  
  df$temp_change <- c(NA, diff(x = df$PCB.temp, lag = 1))
  
  #for (i in 2:nrow(df)) {
  #  df$temp_change[i] <- ifelse(difftime(df$datetime[i], df$datetime[i-1], units = "mins") > 5, 0, df$temp_change[i])
  #}
  
  df$temp_event_number <- 0
  
  if (df$temp_change[2] > 0) {
    df$temp_event_number[2] <- 1
  } else if (df$temp_change[2] < 0) {
    df$temp_event_number[2] <- -1
  } else {
    df$temp_event_number[2] <- 0
  }
  
  for (i in 3:nrow(df)) {
    if (difftime(df$datetime[i], df$datetime[i-1], units = "mins") <= 5) {
    if (df$temp_change[i] == 0) { ###### no temperature change ######
      if ((i - max(which(abs(df[1:(i-1), ]$temp_change) > 0)))  < max_stable_reading) { # max minutes of stable temperature before defining a new event is 60
        df$temp_event_number[i] <- df$temp_event_number[i-1]
      } else {
        df$temp_event_number[i] <- 0 # if euqal or exceeding 60 minutes of stable temperature, define the session as 0
      }
    } else if (df$temp_change[i]  > 0) { ###### temperature incease ######
      if ((i - max(which(abs(df[1:(i-1), ]$temp_change) > 0))) >= max_stable_reading) { # last temp change equal or more than 60 minutes ago, define as a new event, # no temp change in the prior minutes calcualted as -inf, so i - max >= 60
        df$temp_event_number[i] <- ifelse(max(df[2:(i-1), ]$temp_change) <= 0, 1, 
                                          df$temp_event_number[max(which(df[1:(i-1), ]$temp_change > 0))] + 1) 
      } else { #last temp change less than 60 minutes ago
        if (df$temp_change[max(which(abs(df[1:(i-1), ]$temp_change) > 0))] < 0) { # last temperaure change was decrease, define as a new event
          df$temp_event_number[i] <- ifelse(max(df[2:(i-1), ]$temp_change) <= 0, 1, 
                                            df$temp_event_number[max(which(df[1:(i-1), ]$temp_change > 0))] + 1)
        } else { ## last temperaure change was increase
          df$temp_event_number[i] <- df$temp_event_number[i-1] # otherwise the same increase event from previous
          }
        }
    } else if (df$temp_change[i]  < 0) { ###### temperature decrease
      if ((i - max(which(abs(df[1:(i-1), ]$temp_change) > 0)))  >= max_stable_reading) { # last temp change more than 60 minutes ago, define as a new event, # no temp change in the prior minutes calcualted as -inf, so i - max >= 60
        df$temp_event_number[i] <- ifelse(min(df[2:(i-1), ]$temp_change) >= 0, -1, 
                                          df$temp_event_number[max(which(df[1:(i-1), ]$temp_change < 0))] - 1)
      } else { #last temp change less than 60 minutes ago
        if (df$temp_change[max(which(abs(df[1:(i-1), ]$temp_change) > 0))] > 0) { #last temp change within 60 minutes and temp increase, define as a new event
          df$temp_event_number[i] <- ifelse(min(df[2:(i-1), ]$temp_change) >= 0, -1, 
                                            df$temp_event_number[max(which(df[1:(i-1), ]$temp_change < 0))] - 1)
        } else {## last temperaure change was decrease
          df$temp_event_number[i] <- df$temp_event_number[i-1] # otherwise the same devrease event from previous
        }
      }
    } else {
      df$temp_event_number[i] <- 0
    }
  } else {
    df$temp_event_number[i] <- 0
    }
    } # END of calculating temp_event_number 
  
  df$temp_event_number <- c(df$temp_event_number[2:nrow(df)], NA) # 
  
  # label how much temperature change in each event
  df$event_temp_change <- 0
  
  for (j in 1:nrow(df)) {
    df$event_temp_change[j] <- ifelse(df$temp_event_number[j] > 0, 
                                      max(subset(df, temp_event_number == df$temp_event_number[j])$PCB.temp, na.rm = T) - min(subset(df, temp_event_number == df$temp_event_number[j])$PCB.temp, na.rm = T),
                                      ifelse(df$temp_event_number[j] < 0, 
                                             min(subset(df, temp_event_number == df$temp_event_number[j])$PCB.temp, na.rm = T) - max(subset(df, temp_event_number == df$temp_event_number[j])$PCB.temp, na.rm = T),
                                             0))
    }
  
  df$event_temp_change <- ifelse(abs(df$event_temp_change) <= 1, 0, df$event_temp_change)
  
    return(df)
}



# plot temperature change events
function_plotly_temp_change_event <- function(df) {
  plot_ly(data = df) %>% 
    add_lines(x=~timestamp, y=~PCB.temp, name = "PCB Temperature (C)") %>% 
    add_lines(x=~timestamp, y=~temp_event_number) %>%
    add_lines(x=~timestamp, y=~temp_change*10) %>%
    add_lines(x=~timestamp, y=~BC/1000, name = "BC (ug/m3)") %>%
    add_lines(x=~timestamp, y=~event_temp_change, name = "individual event temperature change (C)")
}




###########################################
### function for BC artifact correction ###
###########################################
function_artificial_BC_correction <- function(df) {
  # get correction model parameters
  gamma_shape_intercept <- subset(abc_model, unitid %in% as.character(df$unitid2[1]))$gamma_shape_intercept[1]
  gamma_shape_mean_speed <- subset(abc_model, unitid %in% as.character(df$unitid2[1]))$gamma_shape_mean_speed[1]
  gamma_rate_intercept <- subset(abc_model, unitid %in% as.character(df$unitid2[1]))$gamma_rate_intercept[1]
  gamma_rate_mean_speed <- subset(abc_model, unitid %in% as.character(df$unitid2[1]))$gamma_rate_mean_speed[1]
  gamma_cf_pcbtemp_change <- subset(abc_model, unitid %in% as.character(df$unitid2[1]))$gamma_cf_pcbtemp_change[1]
  
  df$bc_artifact <- 0 # add bc_artifact variable
  df$bc_corrected <- df$BC/1000
  
  # make dataframes for all temperature increase and decrease events, and non events
  #df_temp_increase_events <- subset(df, event_temp_change > 0)
  #df_temp_decrease_events <- subset(df, event_temp_change < 0)
  df_no_temp_change_events <- subset(df, event_temp_change == 0 | is.na(df$event_temp_change))
  df_temp_change_events <- subset(df, abs(event_temp_change) > 0)
  
  # make list for all temperature change events and calculate the artifacts
  list.temp.change <- list()
  for (i in 1:length(unique(df_temp_change_events$temp_event_number))) {
    list.temp.change[[i]] <- subset(df_temp_change_events, 
                                      temp_event_number == unique(df_temp_change_events$temp_event_number)[i])
    list.temp.change[[i]] <- dplyr::arrange(list.temp.change[[i]], datetime)  # sort by datetime
    list.temp.change[[i]]$min_from_event_start <- (list.temp.change[[i]]$datetime - list.temp.change[[i]]$datetime[1])/60
  }
  
  for (j in 1:length(list.temp.change)) {
    list.temp.change[[j]]$bc_artifact <- dgamma(x = list.temp.change[[j]]$min_from_event_start, 
                                                  shape = gamma_shape_intercept + gamma_shape_mean_speed * 0, 
                                                  rate  = gamma_rate_intercept + gamma_rate_mean_speed * 0) * (gamma_cf_pcbtemp_change * list.temp.change[[j]]$event_temp_change)
    list.temp.change[[j]]$bc_corrected <- list.temp.change[[j]]$BC/1000 + list.temp.change[[j]]$bc_artifact
  }
  
  df_temp_change_events <- do.call(rbind, list.temp.change)
  
  df <- rbind.fill(df_no_temp_change_events, df_temp_change_events)
  df <- dplyr::arrange(df, datetime)  # sort by datetime
  
  # make list for am and pm biking events and calculate the artifacts
  df_nonbiking <- subset(df, biking == 0 | is.na(biking))
  df_biking <- subset(df, abs(biking) > 0)
  
  if (nrow(df_biking) > 0) {
    list.biking <- list()
  for (i in 1:length(unique(df_biking$biking))) {
    list.biking[[i]] <- subset(df_biking, biking == unique(df_biking$biking)[i])
    list.biking[[i]] <- dplyr::arrange(list.biking[[i]], datetime)  # sort by datetime
    list.biking[[i]]$min_from_biking_start <- (list.biking[[i]]$datetime - list.biking[[i]]$datetime[1])/60
    list.biking[[i]]$biking_temp_change <- list.biking[[i]]$PCB.temp[nrow(list.biking[[i]])] - list.biking[[i]]$PCB.temp[1]
  }
  
  for (j in 1:length(list.biking)) {
    list.biking[[j]]$bc_artifact <- dgamma(x = list.biking[[j]]$min_from_biking_start, 
                                                shape = gamma_shape_intercept + gamma_shape_mean_speed * 5, 
                                                rate  = gamma_rate_intercept + gamma_rate_mean_speed * 5) * (gamma_cf_pcbtemp_change * list.biking[[j]]$biking_temp_change)
    list.biking[[j]]$bc_corrected <- list.biking[[j]]$BC/1000 + list.biking[[j]]$bc_artifact
  }
  
  df_biking <- do.call(rbind, list.biking)}
  
  df <- rbind.fill(df_nonbiking, df_biking)
  df <- dplyr::arrange(df, datetime)  # sort by datetime
  
  ## no correction when the microAeth is on charger
  df$charger_on <- 0
  df$charger_on[max(which(df$Battery == min(df$Battery))):nrow(df)] <- 1
  df$bc_corrected[max(which(df$Battery == min(df$Battery))):nrow(df)] <- df$BC[max(which(df$Battery == min(df$Battery))):nrow(df)]/1000
  
  return(df)
}



##### 
function_artificial_BC_correction_no_biking <- function(df) {
  # get correction model parameters
  gamma_shape_intercept <- subset(abc_model, unitid %in% as.character(df$unitid2[1]))$gamma_shape_intercept[1]
  gamma_shape_mean_speed <- subset(abc_model, unitid %in% as.character(df$unitid2[1]))$gamma_shape_mean_speed[1]
  gamma_rate_intercept <- subset(abc_model, unitid %in% as.character(df$unitid2[1]))$gamma_rate_intercept[1]
  gamma_rate_mean_speed <- subset(abc_model, unitid %in% as.character(df$unitid2[1]))$gamma_rate_mean_speed[1]
  gamma_cf_pcbtemp_change <- subset(abc_model, unitid %in% as.character(df$unitid2[1]))$gamma_cf_pcbtemp_change[1]
  
  df$bc_artifact <- 0 # add bc_artifact variable
  df$bc_corrected <- df$BC/1000
  
  # make dataframes for all temperature increase and decrease events, and non events
  #df_temp_increase_events <- subset(df, event_temp_change > 0)
  #df_temp_decrease_events <- subset(df, event_temp_change < 0)
  df_no_temp_change_events <- subset(df, event_temp_change == 0 | is.na(df$event_temp_change))
  df_temp_change_events <- subset(df, abs(event_temp_change) > 0)
  
  # make list for all temperature change events and calculate the artifacts
  list.temp.change <- list()
  for (i in 1:length(unique(df_temp_change_events$temp_event_number))) {
    list.temp.change[[i]] <- subset(df_temp_change_events, 
                                    temp_event_number == unique(df_temp_change_events$temp_event_number)[i])
    list.temp.change[[i]] <- dplyr::arrange(list.temp.change[[i]], datetime)  # sort by datetime
    list.temp.change[[i]]$min_from_event_start <- (list.temp.change[[i]]$datetime - list.temp.change[[i]]$datetime[1])/60
  }
  
  for (j in 1:length(list.temp.change)) {
    list.temp.change[[j]]$bc_artifact <- dgamma(x = list.temp.change[[j]]$min_from_event_start, 
                                                shape = gamma_shape_intercept + gamma_shape_mean_speed * 0, 
                                                rate  = gamma_rate_intercept + gamma_rate_mean_speed * 0) * (gamma_cf_pcbtemp_change * list.temp.change[[j]]$event_temp_change)
    list.temp.change[[j]]$bc_corrected <- list.temp.change[[j]]$BC/1000 + list.temp.change[[j]]$bc_artifact
  }
  
  df_temp_change_events <- do.call(rbind, list.temp.change)
  
  df <- rbind.fill(df_no_temp_change_events, df_temp_change_events)
  df <- dplyr::arrange(df, datetime)  # sort by datetime
  
  ## no correction when the microAeth is on charger
  df$charger_on <- 0
  df$charger_on[max(which(df$Battery == min(df$Battery))):nrow(df)] <- 1
  df$bc_corrected[max(which(df$Battery == min(df$Battery))):nrow(df)] <- df$BC[max(which(df$Battery == min(df$Battery))):nrow(df)]/1000
  
  return(df)
}




function_artificial_BC_correction_no_battery <- function(df) {
  # get correction model parameters
  gamma_shape_intercept <- subset(abc_model, unitid %in% as.character(df$unitid2[1]))$gamma_shape_intercept[1]
  gamma_shape_mean_speed <- subset(abc_model, unitid %in% as.character(df$unitid2[1]))$gamma_shape_mean_speed[1]
  gamma_rate_intercept <- subset(abc_model, unitid %in% as.character(df$unitid2[1]))$gamma_rate_intercept[1]
  gamma_rate_mean_speed <- subset(abc_model, unitid %in% as.character(df$unitid2[1]))$gamma_rate_mean_speed[1]
  gamma_cf_pcbtemp_change <- subset(abc_model, unitid %in% as.character(df$unitid2[1]))$gamma_cf_pcbtemp_change[1]
  
  df$bc_artifact <- 0 # add bc_artifact variable
  df$bc_corrected <- df$BC/1000
  
  # make dataframes for all temperature increase and decrease events, and non events
  #df_temp_increase_events <- subset(df, event_temp_change > 0)
  #df_temp_decrease_events <- subset(df, event_temp_change < 0)
  df_no_temp_change_events <- subset(df, event_temp_change == 0 | is.na(df$event_temp_change))
  df_temp_change_events <- subset(df, abs(event_temp_change) > 0)
  
  # make list for all temperature change events and calculate the artifacts
  list.temp.change <- list()
  for (i in 1:length(unique(df_temp_change_events$temp_event_number))) {
    list.temp.change[[i]] <- subset(df_temp_change_events, 
                                    temp_event_number == unique(df_temp_change_events$temp_event_number)[i])
    list.temp.change[[i]] <- dplyr::arrange(list.temp.change[[i]], datetime)  # sort by datetime
    list.temp.change[[i]]$min_from_event_start <- (list.temp.change[[i]]$datetime - list.temp.change[[i]]$datetime[1])/60
  }
  
  for (j in 1:length(list.temp.change)) {
    list.temp.change[[j]]$bc_artifact <- dgamma(x = list.temp.change[[j]]$min_from_event_start, 
                                                shape = gamma_shape_intercept + gamma_shape_mean_speed * 0, 
                                                rate  = gamma_rate_intercept + gamma_rate_mean_speed * 0) * (gamma_cf_pcbtemp_change * list.temp.change[[j]]$event_temp_change)
    list.temp.change[[j]]$bc_corrected <- list.temp.change[[j]]$BC/1000 + list.temp.change[[j]]$bc_artifact
  }
  
  df_temp_change_events <- do.call(rbind, list.temp.change)
  
  df <- rbind.fill(df_no_temp_change_events, df_temp_change_events)
  df <- dplyr::arrange(df, datetime)  # sort by datetime
  
  # make list for am and pm biking events and calculate the artifacts
  df_nonbiking <- subset(df, biking == 0 | is.na(biking))
  df_biking <- subset(df, abs(biking) > 0)
  
  if (nrow(df_biking) > 0) {
    list.biking <- list()
    for (i in 1:length(unique(df_biking$biking))) {
      list.biking[[i]] <- subset(df_biking, biking == unique(df_biking$biking)[i])
      list.biking[[i]] <- dplyr::arrange(list.biking[[i]], datetime)  # sort by datetime
      list.biking[[i]]$min_from_biking_start <- (list.biking[[i]]$datetime - list.biking[[i]]$datetime[1])/60
      list.biking[[i]]$biking_temp_change <- list.biking[[i]]$PCB.temp[nrow(list.biking[[i]])] - list.biking[[i]]$PCB.temp[1]
    }
    
    for (j in 1:length(list.biking)) {
      list.biking[[j]]$bc_artifact <- dgamma(x = list.biking[[j]]$min_from_biking_start, 
                                             shape = gamma_shape_intercept + gamma_shape_mean_speed * 5, 
                                             rate  = gamma_rate_intercept + gamma_rate_mean_speed * 5) * (gamma_cf_pcbtemp_change * list.biking[[j]]$biking_temp_change)
      list.biking[[j]]$bc_corrected <- list.biking[[j]]$BC/1000 + list.biking[[j]]$bc_artifact
    }
    
    df_biking <- do.call(rbind, list.biking)}
  
  df <- rbind.fill(df_nonbiking, df_biking)
  df <- dplyr::arrange(df, datetime)  # sort by datetime
  
  
  return(df)
}




function_artificial_BC_correction_no_correction <- function(df) {
  df$bc_artifact <- 0 # add bc_artifact variable
  df$bc_corrected <- df$BC/1000
  return(df)
}




# plot BC after correction
function_plotly_correction <- function(df) {
  plot_ly(data = df) %>%
    add_lines(x=~datetime, y=~Battery/2, name = "battery/2 (%)", line = list(color = "lightblue")) %>%
    add_lines(x=~datetime, y=~PCB.temp, name = "PCB.temp (C)", line = list(color = "blue")) %>%
    add_lines(x=~datetime, y=~temp_event_number, name = "event#", line = list(color = "orange")) %>%
    add_lines(x=~datetime, y=~event_temp_change, name = "event temp change (C)", line = list(color = "green")) %>%
    add_lines(x=~datetime, y=~BC/1000, name = "raw BC (ug/m3)", line = list(color = "black")) %>%
    add_lines(x=~datetime, y=~bc_corrected, name = "corrected BC (ug/m3)", line = list(color = "red")) %>%
    add_lines(x=~datetime, y=~abs(biking)*(-5), name = "biking", line = list(color = "purple")) %>%
    layout(title = paste0("list#", df$list_ID[1], "  ", df$unique_session[1], "  ", df$unitid[1]))
}



function_plotly_correction2 <- function(df) {
  plot_ly(data = df) %>%
    add_lines(x=~datetime, y=~PCB.temp, name = "PCB.temp (C)", line = list(color = "blue")) %>%
    add_lines(x=~datetime, y=~BC/1000, name = "raw BC (ug/m3)", line = list(color = "black")) %>%
    add_lines(x=~datetime, y=~bc_corrected, name = "corrected BC (ug/m3)", line = list(color = "red")) %>%
    add_lines(x=~datetime, y=~abs(biking)*(-5), name = "biking", line = list(color = "purple")) %>%
    layout(title = paste0("list#", df$list_ID[1], "  ", df$unique_session[1], "  ", df$unitid[1]))
}



function_plot_correction_pdf <- function(x) {
  
    time.spam <- as.numeric(difftime(max(x$datetime), min(x$datetime), unit="day"))
    major.break <- ifelse(time.spam < 0.5,"1 hour", 
                          ifelse(time.spam < 2, "2 hour", 
                                 ifelse(time.spam < 4, "12 hour", "1 day")))
    y_min <- ifelse(min(x$BC/1000, na.rm = T) < -6, -6, min(x$BC/1000, na.rm = T)) 
    y_max <- max(x$PCB.temp, na.rm = T)
    
    if(sum(x$biking<0)>0) {am1 <- min(subset(x, x$biking == -1)$datetime)} else {am1 <- min(x$datetime)-1}
    if(sum(x$biking<0)>0) {am2 <- max(subset(x, x$biking == -1)$datetime)} else {am2 <- min(x$datetime)-1}
    if(sum(x$biking>0)>0) {pm1 <- min(subset(x, x$biking ==  1)$datetime)} else {pm1 <- max(x$datetime)+1}
    if(sum(x$biking>0)>0) {pm2 <- max(subset(x, x$biking ==  1)$datetime)} else {pm2 <- max(x$datetime)+1}
    
    ggplot(data=x)+
      geom_rect(aes(xmin=am1, xmax=am2, ymin=-Inf, ymax=Inf), fill="green",alpha=0.5)+
      geom_rect(aes(xmin=pm1, xmax=pm2, ymin=-Inf, ymax=Inf), fill="yellow",alpha=0.5)+
      geom_hline(yintercept = 0, color="gray")+
      geom_line(aes(x=datetime, y=Battery/5), color = "light blue")+
      geom_line(aes(x=datetime, y=PCB.temp), color = "blue")+
      geom_line(aes(x=datetime, y=event_temp_change), color = "green")+
      geom_line(aes(x=datetime, y=BC/1000), color="black")+
      geom_line(aes(x=datetime, y=bc_corrected), color="red")+
      xlab("")+
      ylab("PM2.5 (ug/m^3)")+
      #ylim(y_min, y_max)+
      coord_cartesian(ylim = c(y_min, y_max))+
      ggtitle(paste0("list#", x$list_ID[1], "  ", x$unique_session[1], "  ", x$unitid[1]))+
      scale_x_datetime(breaks = date_breaks(major.break),labels = date_format("%m-%d %H:%M",tz="America/New_York"))+
      theme_bw() +
      theme(aspect.ratio=3/6,
            axis.text.y   = element_text(size=18),
            axis.text.x   = element_text(size=15,angle = 60, hjust = 1),
            axis.title.y  = element_text(size=18),
            #axis.title.x  = element_text(size=18),
            panel.background = element_blank(),
            panel.grid.major = element_line(size=1, color="gray97", linetype="solid"),
            panel.grid.minor = element_blank(),
            axis.line = element_line(color = "black"),
            panel.border = element_rect(color = "black", fill=NA, size=1),
            plot.title = element_text(size=18,hjust = 0.5)
            #legend.key = element_blank(),
            #legend.title = element_blank(),
            #legend.text = element_blank()
            )
}



function_plot_correction_pdf2 <- function(x) {
  
  time.spam <- as.numeric(difftime(max(x$datetime), min(x$datetime), unit="day"))
  major.break <- ifelse(time.spam < 0.5,"1 hour", 
                        ifelse(time.spam < 2, "2 hour", 
                               ifelse(time.spam < 4, "12 hour", "1 day")))
  y_min <- ifelse(min(x$BC/1000, na.rm = T) < -6, -6, min(x$BC/1000, na.rm = T)) 
  y_max <- max(x$PCB.temp, na.rm = T)
  
  if(sum(x$biking<0)>0) {am1 <- min(subset(x, x$biking == -1)$datetime)} else {am1 <- min(x$datetime)-1}
  if(sum(x$biking<0)>0) {am2 <- max(subset(x, x$biking == -1)$datetime)} else {am2 <- min(x$datetime)-1}
  if(sum(x$biking>0)>0) {pm1 <- min(subset(x, x$biking ==  1)$datetime)} else {pm1 <- max(x$datetime)+1}
  if(sum(x$biking>0)>0) {pm2 <- max(subset(x, x$biking ==  1)$datetime)} else {pm2 <- max(x$datetime)+1}
  
  ggplot(data=x)+
    geom_rect(aes(xmin=am1, xmax=am2, ymin=-Inf, ymax=Inf), fill="green",alpha=0.5)+
    geom_rect(aes(xmin=pm1, xmax=pm2, ymin=-Inf, ymax=Inf), fill="yellow",alpha=0.5)+
    geom_hline(yintercept = 0, color="gray")+
    #geom_line(aes(x=datetime, y=Battery/5), color = "light blue")+
    geom_line(aes(x=datetime, y=PCB.temp), color = "blue")+
    #geom_line(aes(x=datetime, y=event_temp_change), color = "green")+
    geom_line(aes(x=datetime, y=BC/1000), color="black")+
    geom_line(aes(x=datetime, y=bc_corrected), color="red")+
    xlab("")+
    ylab("BC (ug/m^3)\n or \nTemperature (C)")+
    #ylim(y_min, y_max)+
    coord_cartesian(ylim = c(y_min, y_max))+
    ggtitle(paste0(x$unique_session[1], "  ", x$unitid[1]))+
    scale_x_datetime(breaks = date_breaks(major.break),labels = date_format("%m-%d %H:%M",tz="America/New_York"))+
    theme_bw() +
    theme(aspect.ratio=3/6,
          axis.text.y   = element_text(size=18),
          axis.text.x   = element_text(size=15,angle = 60, hjust = 1),
          axis.title.y  = element_text(size=18),
          #axis.title.x  = element_text(size=18),
          panel.background = element_blank(),
          panel.grid.major = element_line(size=1, color="gray97", linetype="solid"),
          panel.grid.minor = element_blank(),
          axis.line = element_line(color = "black"),
          panel.border = element_rect(color = "black", fill=NA, size=1),
          plot.title = element_text(size=18,hjust = 0.5)
          #legend.key = element_blank(),
          #legend.title = element_blank(),
          #legend.text = element_blank()
    )
}



function_plot_correction_pdf3 <- function(x) {
  
  time.spam <- as.numeric(difftime(max(x$datetime), min(x$datetime), unit="day"))
  major.break <- ifelse(time.spam < 0.5,"1 hour", 
                        ifelse(time.spam < 2, "2 hour", 
                               ifelse(time.spam < 4, "12 hour", "1 day")))
  y_min <- ifelse(min(x$BC/1000, na.rm = T) < -6, -6, min(x$BC/1000, na.rm = T)) 
  y_max <- max(x$PCB.temp, na.rm = T)
  
  if(sum(x$biking<0)>0) {am1 <- min(subset(x, x$biking == -1)$datetime)} else {am1 <- min(x$datetime)-1}
  if(sum(x$biking<0)>0) {am2 <- max(subset(x, x$biking == -1)$datetime)} else {am2 <- min(x$datetime)-1}
  if(sum(x$biking>0)>0) {pm1 <- min(subset(x, x$biking ==  1)$datetime)} else {pm1 <- max(x$datetime)+1}
  if(sum(x$biking>0)>0) {pm2 <- max(subset(x, x$biking ==  1)$datetime)} else {pm2 <- max(x$datetime)+1}
  
  ggplot(data=x)+
    geom_rect(aes(xmin=am1, xmax=am2, ymin=-Inf, ymax=Inf), fill="green",alpha=0.5)+
    geom_rect(aes(xmin=pm1, xmax=pm2, ymin=-Inf, ymax=Inf), fill="yellow",alpha=0.5)+
    geom_hline(yintercept = 0, color="gray")+
    #geom_line(aes(x=datetime, y=Battery/5), color = "light blue")+
    geom_line(aes(x=datetime, y=PCB.temp), color = "blue")+
    #geom_line(aes(x=datetime, y=event_temp_change), color = "green")+
    #geom_line(aes(x=datetime, y=BC/1000), color="black")+
    #geom_line(aes(x=datetime, y=bc_corrected), color="red")+
    xlab("")+
    ylab("Temperature (C)")+
    #ylim(0, 28)+
    coord_cartesian(ylim = c(0, 29))+
    scale_y_continuous(breaks = c(0, 5, 10, 15, 20, 25, 30))+
    ggtitle(paste0(x$unique_session[1], "  ", x$unitid[1]))+
    scale_x_datetime(breaks = date_breaks(major.break),labels = date_format("%m-%d %H:%M",tz="America/New_York"))+
    theme_bw() +
    theme(aspect.ratio=3/6,
          axis.text.y   = element_text(size=18),
          axis.text.x   = element_text(size=15,angle = 60, hjust = 1),
          axis.title.y  = element_text(size=18),
          #axis.title.x  = element_text(size=18),
          panel.background = element_blank(),
          panel.grid.major = element_line(size=1, color="gray97", linetype="solid"),
          panel.grid.minor = element_blank(),
          axis.line = element_line(color = "black"),
          panel.border = element_rect(color = "black", fill=NA, size=1),
          plot.title = element_text(size=18,hjust = 0.5)
          #legend.key = element_blank(),
          #legend.title = element_blank(),
          #legend.text = element_blank()
    )
}



function_plot_correction_pdf4 <- function(x) {
  
  time.spam <- as.numeric(difftime(max(x$datetime), min(x$datetime), unit="day"))
  major.break <- ifelse(time.spam < 0.5,"1 hour", 
                        ifelse(time.spam < 2, "2 hour", 
                               ifelse(time.spam < 4, "12 hour", "1 day")))
  y_min <- ifelse(min(x$BC/1000, na.rm = T) < -6, -6, min(x$BC/1000, na.rm = T)) 
  y_max <- max(x$PCB.temp, na.rm = T)
  
  if(sum(x$biking<0)>0) {am1 <- min(subset(x, x$biking == -1)$datetime)} else {am1 <- min(x$datetime)-1}
  if(sum(x$biking<0)>0) {am2 <- max(subset(x, x$biking == -1)$datetime)} else {am2 <- min(x$datetime)-1}
  if(sum(x$biking>0)>0) {pm1 <- min(subset(x, x$biking ==  1)$datetime)} else {pm1 <- max(x$datetime)+1}
  if(sum(x$biking>0)>0) {pm2 <- max(subset(x, x$biking ==  1)$datetime)} else {pm2 <- max(x$datetime)+1}
  
  ggplot(data=x)+
    geom_rect(aes(xmin=am1, xmax=am2, ymin=-Inf, ymax=Inf), fill="green",alpha=0.5)+
    geom_rect(aes(xmin=pm1, xmax=pm2, ymin=-Inf, ymax=Inf), fill="yellow",alpha=0.5)+
    geom_hline(yintercept = 0, color="gray")+
    #geom_line(aes(x=datetime, y=Battery/5), color = "light blue")+
    #geom_line(aes(x=datetime, y=PCB.temp), color = "blue")+
    #geom_line(aes(x=datetime, y=event_temp_change), color = "green")+
    geom_line(aes(x=datetime, y=BC/1000), color="black")+
    geom_line(aes(x=datetime, y=bc_corrected), color="red")+
    xlab("")+
    ylab("BC (ug/m^3)")+
    ylim(-5, 10)+
    #coord_cartesian(ylim = c(0, 29))+
    #scale_y_continuous(breaks = c(0, 5, 10, 15, 20, 25, 30))+
    ggtitle(paste0(x$unique_session[1], "  ", x$unitid[1]))+
    scale_x_datetime(breaks = date_breaks(major.break),labels = date_format("%m-%d %H:%M",tz="America/New_York"))+
    theme_bw() +
    theme(aspect.ratio=3/6,
          axis.text.y   = element_text(size=18),
          axis.text.x   = element_text(size=15,angle = 60, hjust = 1),
          axis.title.y  = element_text(size=18),
          #axis.title.x  = element_text(size=18),
          panel.background = element_blank(),
          panel.grid.major = element_line(size=1, color="gray97", linetype="solid"),
          panel.grid.minor = element_blank(),
          axis.line = element_line(color = "black"),
          panel.border = element_rect(color = "black", fill=NA, size=1),
          plot.title = element_text(size=18,hjust = 0.5)
          #legend.key = element_blank(),
          #legend.title = element_blank(),
          #legend.text = element_blank()
    )
}
