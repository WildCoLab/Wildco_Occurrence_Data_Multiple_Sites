### WildCo Data Sharing
### Standardize, filter, and export data across projects in Wild3
### April 2025
### Katie Tjaden-McClement

# Load packages
library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)

# What projects are you sharing data from?
# Update this list using the short form names from the database
project_list <- c("MK", "ITCHA", "GE", "GARI", "JOFF", "MG", "BLT", "CATH")

# Put the project folders from the database output in the Data/Input/ folder

# Are you sharing all detections or only certain species? (Change to "No" if not)
filtering_by_species <- "Yes"

# What species are you sharing data for? (update latin names below)
focal_sp <- c("Lynx canadensis", "Lynx rufus", "Gulo gulo", "Canis latrans", "Martes americana",
              "Martes pennanti", "Tamiasciurus hudsonicus", "Odocoileus hemionus",
              "Odocoileus virginianus", "Lepus americanus")

# If filtering by species, do you want to also export all species detections?
# (e.g. to have as an archived, full dataset)
export_all_detections <- "Yes"

# Create output folders
dir.create("Data/Output/tmp_plots")

for(project in project_list){
  dir.create(paste0("Data/Output/", project))
}

# Deployment Sheet #############################################################
# How do these projects record camera activity periods (e.g. snow coverage)?
# The standard way to record this is marking timelapse images as "misfires" when cameras are inactive,
# if projects use another method (e.g. manual recording in a separate datasheet), add them below
cam_activity_method <- tibble(project = project_list,
                              activity_method = if_else(project %in% c("ITCHA"), # add any projects that do camera activity manually
                                                        "manual",
                                                        "timelapse misfires"))

# Make deplployment sheets for projects using timelapse "misfires"
projects_timelapse_activity <- filter(cam_activity_method, 
                                      activity_method == "timelapse misfires") %>% 
  pull(project)

for(project in projects_timelapse_activity){
  ### Activity periods from Timelapse images #####################################
  # load images.csv output file
  all_img <- read.csv(paste0("Data/Input/", project, "/images.csv")) %>% 
    # select only relevant columns
    select(project_id, station_id, orig_file, timestamp_pst,
           image_trigger, misfire, processed, staff) %>% 
    mutate(date_time = as.POSIXct(timestamp_pst)) %>% 
    arrange(station_id, date_time)
  
  ### Fix date/time errors within projects
  # JOFF11 was initially set with the wrong date and fixed later that day, fix:
  if(project == "JOFF"){
    JOFF10_time_diff <- (as.POSIXct("2020-08-03 17:56:00") - # correct time for first image from datasheet
                           all_img$date_time[all_img$station_id == "JOFF10"][1]) # first recorded time
    
    all_img <- all_img %>% 
      filter(!(station_id == "JOFF11" & as.Date(date_time) == "2020-03-04")) %>% 
      mutate(date_time = if_else(station_id == "JOFF10" & 
                                   date_time < ymd_hms("2022-06-30 10:26:38"),
                                 date_time + JOFF10_time_diff,
                                 date_time))
  }
  
  # Fix BLT date/time issues
  if(project == "BLT"){
    BLT29_time_diff <- as.POSIXct("2023-07-12 17:45:00") - # correct time
      as.POSIXct(all_img$date_time[all_img$station_id == "BLT29"][1])
    
    BLT38_time_diff <- as.POSIXct("2023-07-13 11:59:53") - # correct time
      as.POSIXct(all_img$date_time[all_img$station_id == "BLT38"][1])
    
    all_img <- all_img %>% 
      # Remove all BLT43 images with date_time "2024-01-31 07:03:41" - camera malfunction
      filter(!(station_id == "BLT43" & date_time == "2024-01-31 07:03:41")) %>% 
      mutate(date_time = case_when(station_id == "BLT29" ~ date_time + BLT29_time_diff,
                                   station_id == "BLT38" ~ date_time + BLT38_time_diff,
                                   # UPDATE with date_time ranges to correct within once new, fixed data is available
                                   .default = date_time))
  }
  
  all_img <- all_img %>% 
    mutate(date = as.Date(date_time),
           station_id_date = paste(station_id, date, sep = "_"))
  
  tl_img <- all_img %>% 
    # filter to timelapse images
    filter(image_trigger == "Time Lapse") %>% 
    arrange(station_id, date_time)
  
  cam_active <- tl_img %>% 
    filter(misfire == "f",
           processed == "t") %>% 
    # keep only one timelapse per day (in case of mistakes in setting timelapse)
    group_by(station_id, date) %>% 
    slice(1) %>% 
    # check whether there are missing dates in timelapse images
    group_by(station_id) %>% 
    mutate(row_number = row_number(),
           total_rows = n(),
           date_status = case_when(row_number == 1 ~ "activity_start", # first timelapse at station
                                   row_number == total_rows ~ "activity_end", # last timelapse at station
                                   date == lag(date + 1) ~ "consecutive", # continuously active days
                                   date != lag(date + 1) & date != lead(date - 1) ~ "single_active_day",
                                   date != lag(date + 1) ~ "activity_start" # gap in date from last active day
                                   ),
           # make final active day before a new period the "activity_end"
           date_status = if_else(date_status == "consecutive" & 
                                   lead(date_status) %in% c("activity_start", "single_active_day"),
                                 "activity_end", date_status),
           tmp = if_else(date_status == "single_active_day", 2, 1)) %>% 
    # need to duplicate rows for dates when camera activity starts AND ends (only active one day)
    uncount(tmp) %>% 
    # make start and end for single active days
    mutate(date_status = case_when(date_status == "single_active_day" & 
                                     lead(date_status) == "single_active_day" &
                                     date == lead(date) ~ "activity_start",
                                   date_status == "single_active_day" & 
                                     lag(date_status) == "single_active_day" &
                                     date == lag(date) ~ "activity_end",
                                   .default = date_status))
  
  activity_periods <- cam_active %>% 
    select(project_id, station_id, date, date_status, 
           row_number, total_rows) %>% 
    filter(date_status %in% c("activity_start", "activity_end")) %>% 
    # add deployment period number within each station
    group_by(station_id, date_status) %>% 
    mutate(act_period_number = row_number()) 
  
  activity_periods_wide <- activity_periods %>% 
    select(-row_number, -total_rows) %>% 
    pivot_wider(names_from = date_status, values_from = date)
  
  ggplot(activity_periods_wide, aes(y = station_id)) +
    geom_segment(aes(x = activity_start, xend = activity_end))

  # Load motion images to account for gaps in Timelapse
  motion_imgs <- all_img %>% 
    filter(image_trigger != "Time Lapse", # keep motion and "unknown" trigger
           # keep only processed images
           processed == "t") %>% 
    arrange(station_id, date_time)
  
  # Get days when timelapse images marked as misfires (=camera view obstructed/camera non-functional)
  days_obstructed <- tl_img %>% 
    filter(processed == "t",
           misfire == "t") %>%
    select(station_id, date, station_id_date) %>% 
    distinct()
  
  # Get all days when images were taken at a station (minus obstructed days)
  days_images_taken <- motion_imgs %>%
    group_by(station_id, date) %>% 
    slice(1) %>% 
    # remove days when cams were obstructed
    filter(!(station_id_date %in% days_obstructed$station_id_date))
  
  # ggplot(activity_periods_wide, aes(y = station_id)) +
  #   geom_segment(aes(x = activity_start, xend = activity_end)) +
  #   geom_point(data = days_images_taken, aes(x = date, y = station_id),
  #              col = "red", shape = "|", alpha = 0.5)
  
  # check out images that aren't included in activity periods
  image_activity_mismatch <- motion_imgs %>%
    filter(!(station_id_date %in% cam_active$station_id_date))
  
  # Adjust activity starts and activity ends based on motion detection images the day before or after
  activity_periods <- activity_periods %>% 
    mutate(date = case_when(date_status == "activity_start" & 
                              paste(station_id, date - 1, sep = "_") %in% 
                              days_images_taken$station_id_date ~ date - 1, 
                            date_status == "activity_end" &
                              paste(station_id, date + 1, sep = "_") %in% 
                              days_images_taken$station_id_date ~ date + 1,
                            .default = date))
  # QUESTION: Do we always want to include an additional day after activity end if there
  # is a motion detection? Or only for the final deployment end (likely staff)?
  
  activity_periods_wide <- activity_periods %>%
    select(-c(row_number, total_rows)) %>%
    pivot_wider(names_from = date_status, values_from = date)

  # # how does it look?
  # ggplot(activity_periods_wide, aes(y = station_id)) +
  #   geom_segment(aes(x = activity_start, xend = activity_end)) +
  #   geom_point(data = days_images_taken, aes(x = date, y = station_id),
  #              col = "red", shape = "|", alpha = 0.5)
  
  ### Stations without any timelapse photos ######################################
  # use first and last motion images, assume activity between these periods?
  # or incorporate camera check data if available? --> not now.
  if(length(unique(days_images_taken$station_id)) >
     length(unique(activity_periods$station_id))){
    no_timelapse <- days_images_taken %>% 
      filter(!(station_id %in% activity_periods$station_id)) %>% 
      group_by(project_id, station_id) %>% 
      summarise(activity_start = min(date),
                activity_end = max(date))
    
    # merge with activity periods from timelapse
    activity_periods_wide <- bind_rows(activity_periods_wide,
                                       no_timelapse)
  }
  
  ### Stations with periods of missing timelapse #################################
  # check for motion images outside of activity windows
  # start date of deployments should be the most recent check, or just the first image date

  # Check if there are any motion images outside of activity periods
  within_tl_activity_periods <- days_images_taken %>% 
    left_join(activity_periods_wide, by = "station_id",
              relationship = "many-to-many") %>% 
    filter(between(date, activity_start, activity_end))
  
  outside_tl_activity_periods <- days_images_taken %>% 
    filter(!(station_id_date %in% within_tl_activity_periods$station_id_date))
  
  if(nrow(outside_tl_activity_periods) > 0){
    # We need to know whether there was a staff detection at the end of the activity period
    # before the period without timelapse which would confirm that the cam
    # was active at the start of the period. If yes, then make that the start date; 
    # if no, then use the date of the first image
    
    staff_detections <- motion_imgs %>% 
      filter(staff == "t")
    
    # # Visualize staff detections and days with images outside of timelapse activity periods
    # ggplot(activity_periods_wide, aes(y = station_id)) +
    #   geom_segment(aes(x = activity_start, xend = activity_end)) +
    #   geom_point(data = staff_detections, aes(x = date, y = station_id),
    #              col = "blue", alpha = 0.5) +
    #   geom_point(data = outside_tl_activity_periods, aes(x = date, y = station_id),
    #              col = "red", shape = "|", alpha = 0.5)
      
    # Stations with non-timelapse periods
    # need to split up chunks of detections separated by timelapse activity periods
    non_tl_periods <- outside_tl_activity_periods %>% 
      full_join(activity_periods %>% filter(station_id %in% 
                                              outside_tl_activity_periods$station_id),
                by = c("project_id", "station_id", "date")) %>% 
      arrange(station_id, date) %>% 
      group_by(station_id) %>% 
      # Assign activity period id based on presence of activity starts
      mutate(non_tl_act_period_number = cumsum(!is.na(date_status))) %>% 
      filter(is.na(date_status)) %>% 
      group_by(station_id, non_tl_act_period_number) %>% 
      summarise(first = min(date),
                last = max(date)) %>% 
      left_join(activity_periods_wide,
                relationship = "many-to-many") %>% 
      # find tl_activity end date closest to first motion image
      group_by(station_id, non_tl_act_period_number) %>% 
      mutate(days_since_activity_end = first - activity_end,
             no_tl_activity_before = if_else(all(as.numeric(days_since_activity_end) < 0),
                                             T, F)) %>% 
      filter(no_tl_activity_before == T |
               days_since_activity_end > 0) %>% 
      arrange(station_id, non_tl_act_period_number, days_since_activity_end) %>% 
      slice(1) %>% 
      # were staff detected on the final day of the last activity period (or the day before)
      mutate(staff_detected = case_when(no_tl_activity_before == T ~ F,
                                        paste(station_id, activity_end, sep = "_") 
                                        %in% staff_detections$station_id_date |
                                          paste(station_id, activity_end - 1, sep = "_")
                                        %in% staff_detections$station_id_date ~ T, 
                                        .default = F),
             activity_start = if_else(staff_detected == T, activity_end,
                                      first),
             activity_end = last) %>% 
      ungroup() %>% 
      select(-c(first, last, act_period_number, non_tl_act_period_number,
                days_since_activity_end, staff_detected, no_tl_activity_before))
    
    # merge with other activity periods
    activity_periods_wide <- bind_rows(activity_periods_wide,
                                       non_tl_periods)
  }
  
  # # how does everything look?
  # ggplot(activity_periods_wide, aes(y = station_id)) +
  #   geom_segment(aes(x = activity_start, xend = activity_end)) +
  #   geom_point(data = days_images_taken, aes(x = date, y = station_id),
  #              col = "red", shape = "|", alpha = 0.5)
 
  # load other deployment/station data
  stations <- read.csv(paste0("Data/Input/", project, "/stations.csv")) %>% 
    select(station_id, latitude, longitude)
  camera_checks <- read.csv(paste0("Data/Input/", project, "/camera_checks.csv")) %>% 
    select(-c(check_date, stop_date, battery, camera_label, project_id,
              camera_check_tbl_id, camera_status, media_recovered, images_per_trigger)) %>% 
    # Camera heights, bait status, and other features may change between deployments...
    # average camera heights for now and use a single value for each station
    group_by(station_id) %>% 
    mutate(camera_height = mean(camera_height),
           camera_distance = mean(camera_distance)) %>% 
    slice(1) %>% 
    # replace 0's (default) with NAs for camera height
    mutate(camera_height = if_else(camera_height == 0, NA, camera_height))
    
  deployment <- left_join(activity_periods_wide, camera_checks,
                          by = "station_id") %>% 
    left_join(stations)
  
  deployment <- deployment %>%
    transmute(
      project_id,
      station_id,
      latitude,
      longitude,
      start_date = activity_start,
      end_date = activity_end,
      bait_type = bait,
      bait_description = if_else(bait == "None", "", bait),
      feature_type = feature,
      feature_type_methodology = as.character(NA),
      quiet_period,
      sensor_height = camera_height,
      sensor_orientation = camera_angle,
      distance_to_feature = camera_distance,
      plot_treatment = treatment
    )
  
  # assign project-specific name
  assign(paste0(project, "_deployment"), deployment)
  
  # output final plot so that people can take a look for mistakes
  days_images_taken_plotting <- motion_imgs %>%
    group_by(station_id, date) %>% 
    slice(1) %>% 
    mutate(obstructed = if_else((station_id_date %in% days_obstructed$station_id_date),
                                "cam obstructed", "cam active"))
  
  activity_plot <- ggplot(deployment, aes(y = station_id)) +
    geom_segment(aes(x = start_date, xend = end_date)) +
    # plot single day deployments separately
    geom_point(data = deployment %>% filter(start_date == end_date), 
               aes(x = start_date), shape = 15, size = 0.2) +
    geom_point(data = days_images_taken_plotting, aes(x = date, y = station_id,
                                                      colour = obstructed),
               shape = "|", alpha = 0.7, size = 2) +
    labs(x = "", y = "", colour = "Days with \nmotion images") +
    scale_x_date(date_breaks = '1 month',
                 date_labels = '%b %Y') +
    guides(colour = guide_legend(override.aes = list(size = 4))) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  ggsave(activity_plot, filename = paste0("Data/Output/", project, "/",
                                          project, "_activity_plot.png"),
         height = 8, width = 10, units = "in")
  
  print(paste(project, "completed"))
}

projects_manual_activity <- filter(cam_activity_method, 
                                   activity_method == "manual") %>% 
  pull(project)

### For manual projects:
# name datasheet with activity periods "[proj name]_activity.csv" (or change the file path(s) below)
# update column names to "station_id", "activity_start", and "activity_end" to match

for(project in projects_manual_activity){
  activity_periods <- read.csv(paste0("Data/Input/", project, "/", project, "_activity.csv"))
  
  # load other deployment/station data
  stations <- read.csv(paste0("Data/Input/", project, "/stations.csv")) %>% 
    select(station_id, latitude, longitude)
  camera_checks <- read.csv(paste0("Data/Input/", project, "/camera_checks.csv")) %>% 
    select(-c(check_date, stop_date, battery, camera_label, project_id,
              camera_check_tbl_id, camera_status, media_recovered, images_per_trigger)) %>% 
    # Camera heights, bait status, and other features may change between deployments...
    # average camera heights for now and use a single value for each station
    group_by(station_id) %>% 
    mutate(camera_height = mean(camera_height),
           camera_distance = mean(camera_distance)) %>% 
    slice(1) %>% 
    # replace 0's (default) with NAs for camera height
    mutate(camera_height = if_else(camera_height == 0, NA, camera_height))
  
  deployment <- left_join(activity_periods, camera_checks,
                          by = "station_id") %>% 
    left_join(stations)
  
  deployment <- deployment %>%
    transmute(
      project_id = project,
      station_id,
      latitude,
      longitude,
      start_date = as.Date(activity_start),
      end_date = as.Date(activity_end),
      bait_type = bait,
      bait_description = if_else(bait == "None", "", bait),
      feature_type = feature,
      feature_type_methodology = as.character(NA),
      quiet_period,
      sensor_height = camera_height,
      sensor_orientation = camera_angle,
      distance_to_feature = camera_distance,
      plot_treatment = treatment) %>% 
    arrange(station_id, start_date)
  
  # assign project-specific name
  assign(paste0(project, "_deployment"), deployment)
  
  # Plot
  activity_plot <- ggplot(deployment, aes(y = station_id)) +
    geom_segment(aes(x = start_date, xend = end_date)) +
    # plot single day deployments separately
    geom_point(data = deployment %>% filter(start_date == end_date),
               aes(x = start_date), shape = 15, size = 0.2) +
    labs(x = "", y = "") +
    scale_x_date(date_breaks = '1 month',
                 date_labels = '%b %Y') +
    guides(colour = guide_legend(override.aes = list(size = 4))) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  ggsave(activity_plot, filename = paste0("Data/Output/", project, "/",
                                          project, "_activity_plot.png"),
         height = 8, width = 10, units = "in")
  
  print(paste(project, "completed"))
}

# Detections Sheet #############################################################

for(project in project_list){
  idents <- read.csv(paste0("Data/Input/", project, "/images_idents.csv"))
  
  # remove deleted rows, misfire, and blank images
  idents <- idents %>% 
    filter(deleted == "f",
           misfire == "f",
           latin_name != "No animal") %>% 
    # make any blanks for collar type NAs
    mutate(collar_type = if_else(collar_type == "", NA, collar_type))
  
  # Remove duplicate tags (all tagged fields the same except comments)
  idents <- idents %>% 
    distinct(station_id, exif_timestamp, latin_name, sex, age_category, species_count,
             group_count, behaviour, collar, collar_type, collar_tags,
             .keep_all = T) #keep all columns in output
    
  ### Add any project-specific corrections here (e.g. fixing date/time mistakes)
  # JOFF11 was initially set with the wrong date and fixed later that day, 
  # JOFF10 has the wrong time of day, fixed on 2022-06-30:
  if(project == "JOFF"){
    JOFF10_time_diff <- (as.POSIXct("2020-08-03 17:56:00") - # correct time for first image from datasheet
                           as.POSIXct(idents$exif_timestamp[idents$station_id == "JOFF10"][1])) # first recorded time
    
    idents <- idents %>% 
      mutate(date = as.Date(exif_timestamp),
             time = strftime(exif_timestamp, format = "%H:%M:%S"),
             exif_timestamp = ymd_hms(exif_timestamp),
             exif_timestamp = case_when(station_id == "JOFF11" & 
                                          date == "2020-03-04" ~ 
                                          as.POSIXct(paste("2020-08-04", time)),
                                        station_id == "JOFF10" & 
                                          exif_timestamp < ymd_hms("2022-06-30 10:26:38") ~
                                          exif_timestamp + JOFF10_time_diff,
                                        .default = exif_timestamp))
    
  }
  
  # ITCHA time fix for ITCHA3-04, fixed on 2022-08-30
  if(project == "ITCHA"){
    ITCHA3_04_time_diff <- as.POSIXct("2020-09-22 10:24:00") - # correct time
                            as.POSIXct(idents$exif_timestamp[idents$station_id == "ITCHA3-04"][1])
    
    idents <- idents %>% 
      mutate(exif_timestamp = ymd_hms(exif_timestamp, truncated = 3),
             exif_timestamp = if_else(station_id == "ITCHA3-04" & 
                                        exif_timestamp < ymd_hms("2022-08-30 16:00:03"),
                                      exif_timestamp + ITCHA3_04_time_diff,
                                      exif_timestamp))
    }
  
  # Remove detections with frozen time from BLT and fix time errors
  if(project == "BLT"){
    BLT29_time_diff <- as.POSIXct("2023-07-12 17:45:00") - # correct time
      as.POSIXct(idents$exif_timestamp[idents$station_id == "BLT29"][1])
    
    BLT38_time_diff <- as.POSIXct("2023-07-13 11:59:55") - # correct time
      as.POSIXct(idents$exif_timestamp[idents$station_id == "BLT38"][1])
    
    idents <- idents %>% 
      # Remove all BLT43 images with date_time "2024-01-31 07:03:41" - camera malfunction
      filter(!(station_id == "BLT43" & exif_timestamp == "2024-01-31 07:03:41")) %>% 
      mutate(exif_timestamp = ymd_hms(exif_timestamp),
             exif_timestamp = case_when(station_id == "BLT29" ~ exif_timestamp + BLT29_time_diff,
                                        station_id == "BLT38" ~ exif_timestamp + BLT38_time_diff,
                                        # UPDATE with date_time ranges to correct within once new, fixed data is available
                                        .default = exif_timestamp))
  }
  
  # rename to standardized columns
  idents <- idents %>%
    transmute(project_id,
              station_id,
              image_id = orig_file,
              identified_by = staff_name,
              species = latin_name,
              species_common_name = common_names,
              age = age_category,
              sex,
              timestamp = ymd_hms(exif_timestamp, truncated = 3),
              minimum_group_size = group_count,
              number_of_animals = species_count,
              highlighted = favourite,
              behaviour,
              animal_recognizable = if_else(collar == "t" | !is.na(collar_type),
                                            "Yes", "No"),
              individual_id = "None", # or adjust based on your project
              individual_animal_notes = case_when(collar == "t" & !is.na(collar_type) & collar_tags != "" ~
                                                    paste0("collar; ", collar_type, "; ", collar_tags), 
                                                  collar == "t" & !is.na(collar_type) & collar_tags == "" ~
                                                    paste0("collar; ", collar_type),
                                                  collar == "t" & is.na(collar_type) & collar_tags == "" ~
                                                    "collar",
                                                  collar == "f" & !is.na(collar_type) & collar_tags != "" ~
                                                    paste0(collar_type, "; ", collar_tags),
                                                  collar == "f" & !is.na(collar_type) & collar_tags == "" ~
                                                    collar_type,
                                                  .default = "None"),
              comments,
              flagged_for_review = review,
              trigger_mode = image_trigger)
  
  # assign project-specific name
  assign(paste0(project, "_detection_data_all"), idents)
  
  ### Filter to focal species
  if(filtering_by_species == "Yes"){
    focal_idents <- idents %>% 
      filter(species %in% focal_sp)
    
    assign(paste0(project, "_detection_data_focal"), focal_idents)
  }
  
  print(paste(project, "completed"))
}


# Project Sheet ################################################################

for(project in project_list){
  proj <- read.csv(paste0("Data/Input/", project, "/projects.csv"))

  proj <- proj %>% 
    transmute(project_name, # Full name
              project_id = project, # Short form
              project_objectives = objectives,
              project_design = design,
              project_bait_use = "None", # Change in future if needed
              project_sensor_method = "Sensor Detection", # Animals not tagged in Timelapse photos; change in future if needed
              project_sensor_cluster = "No", # Change in future if needed
              country_code)
  
  assign(paste0(project, "_project"), proj)
    
}


# Data Export ##################################################################

for(project in project_list){
  # Deployment
  write.csv(x = get(paste0(project, "_deployment")), 
            file = paste0("Data/Output/", project, "/", project, "_deployment.csv"),
            row.names = F)
  
  # Detections
  if(filtering_by_species == "Yes"){
    write.csv(x = get(paste0(project, "_detection_data_focal")), 
              file = paste0("Data/Output/", project, "/", project, "_detection_data_focal.csv"),
              row.names = F)
    
    if(export_all_detections == "Yes"){
      write.csv(x = get(paste0(project, "_detection_data_all")), 
                file = paste0("Data/Output/", project, "/", project, "_detection_data_all.csv"),
                row.names = F)
      }
  }
    
  if(filtering_by_species == "No"){
    write.csv(x = get(paste0(project, "_detection_data_all")), 
              file = paste0("Data/Output/", project, "/", project, "_detection_data.csv"),
              row.names = F)
  }
  
  # Project
  # write.csv(x = get(paste0(project, "_project")), 
  #           file = paste0("Data/Output/", project, "/", project, "_project.csv"),
  #           row.names = F)
  
}

# ### Re-filtering from already made detection_data_all dataframe 
# for(project in project_list){
#   all_detections <- get(paste0(project, "_detection_data_all"))
#   
#   focal_sp <- c("Canis lupus", "Gulo gulo", "Canis latrans")
#   
#   if(filtering_by_species == "Yes"){
#     focal_idents <- all_detections %>% 
#       filter(species %in% focal_sp)
#     
#     assign(paste0(project, "_detection_data_focal"), focal_idents)
#     
#     write.csv(x = focal_idents, 
#               file = paste0("Data/Output/", project, "/", project, "_detection_data_focal.csv"),
#               row.names = F)
#   }
# }
