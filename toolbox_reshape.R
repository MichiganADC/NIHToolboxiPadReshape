#!/usr/bin/env Rscript 

#####
# Script to reshape NIH Toolbox data (iPads) from semi-wide to wide
# for upload to REDCap
####
 
library(dplyr)
library(readr)
library(lubridate)
library(stringr)
source("~/Desktop/config.R")

ipad1 <- read_csv("ALL_iPad1_AssessmentScores_1-10-19.csv", na = "",
                  col_types = cols(
                    .default = col_double()
                    , PIN = col_character()
                    , DeviceID = col_character()
                    , `Assessment Name` = col_character()
                    , Inst = col_character()
                    # , RawScore = col_integer()
                    , Theta = col_double()
                    , TScore = col_double()
                    , SE = col_double()
                    , ItmCnt = col_integer()
                    , DateFinished = col_datetime(format = "")
                    , Column1 = col_logical()
                    , Column2 = col_logical()
                    , Column3 = col_logical()
                    , Column4 = col_logical()
                    , Column5 = col_logical()
                    , Language = col_character()
                    , `Computed Score` = col_double()
                    # , `Uncorrected Standard Score` = col_integer()
                    # , `Age-Corrected Standard Score` = col_integer()
                    # , `National Percentile (age adjusted)` = col_integer()
                    # , `Fully-Corrected T-score` = col_integer()
                    , InstrumentBreakoff = col_integer()
                    , InstrumentStatus2 = col_integer()
                    , InstrumentRCReason = col_integer()
                    , InstrumentRCReasonOther = col_character()
                    , `App Version` = col_character()
                    , `iPad Version` = col_character()
                    , `Firmware Version` = col_character()
                  ))
ipad2 <- read_csv("ALL_iPad2_AssessmentScores_1-10-19.csv", na = "",
                  col_types = cols(
                    .default = col_double()
                    , PIN = col_character()
                    , DeviceID = col_character()
                    , `Assessment Name` = col_character()
                    , Inst = col_character()
                    # , RawScore = col_integer()
                    , Theta = col_double()
                    , TScore = col_double()
                    , SE = col_double()
                    , ItmCnt = col_integer()
                    , DateFinished = col_datetime(format = "")
                    , Column1 = col_logical()
                    , Column2 = col_logical()
                    , Column3 = col_logical()
                    , Column4 = col_logical()
                    , Column5 = col_logical()
                    , Language = col_character()
                    , `Computed Score` = col_double()
                    # , `Uncorrected Standard Score` = col_integer()
                    # , `Age-Corrected Standard Score` = col_integer()
                    # , `National Percentile (age adjusted)` = col_integer()
                    # , `Fully-Corrected T-score` = col_integer()
                    , InstrumentBreakoff = col_integer()
                    , InstrumentStatus2 = col_integer()
                    , InstrumentRCReason = col_integer()
                    , InstrumentRCReasonOther = col_character()
                    , `App Version` = col_character()
                    , `iPad Version` = col_character()
                    , `Firmware Version` = col_character()
                  ))
ipad3 <- read_csv("ALL_iPad3_AssessmentScores_1-10-19.csv", na = "",
                  col_types = cols(
                    .default = col_double()
                    , PIN = col_character()
                    , DeviceID = col_character()
                    , `Assessment Name` = col_character()
                    , Inst = col_character()
                    # , RawScore = col_integer()
                    , Theta = col_double()
                    , TScore = col_double()
                    , SE = col_double()
                    , ItmCnt = col_integer()
                    , DateFinished = col_datetime(format = "")
                    , Column1 = col_logical()
                    , Column2 = col_logical()
                    , Column3 = col_logical()
                    , Column4 = col_logical()
                    , Column5 = col_logical()
                    , Language = col_character()
                    , `Computed Score` = col_double()
                    # , `Uncorrected Standard Score` = col_integer()
                    # , `Age-Corrected Standard Score` = col_integer()
                    # , `National Percentile (age adjusted)` = col_integer()
                    # , `Fully-Corrected T-score` = col_integer()
                    , InstrumentBreakoff = col_integer()
                    , InstrumentStatus2 = col_integer()
                    , InstrumentRCReason = col_integer()
                    , InstrumentRCReasonOther = col_character()
                    , `App Version` = col_character()
                    , `iPad Version` = col_character()
                    , `Firmware Version` = col_character()
                  ))
ipad4 <- read_csv("ALL_iPad4_AssessmentScores_1-10-19.csv", na = "",
                  col_types = cols(
                    .default = col_double()
                    , PIN = col_character()
                    , DeviceID = col_character()
                    , `Assessment Name` = col_character()
                    , Inst = col_character()
                    # , RawScore = col_integer()
                    , Theta = col_double()
                    , TScore = col_double()
                    , SE = col_double()
                    , ItmCnt = col_integer()
                    , DateFinished = col_datetime(format = "%m/%d/%y %H:%M")
                    , Column1 = col_logical()
                    , Column2 = col_logical()
                    , Column3 = col_logical()
                    , Column4 = col_logical()
                    , Column5 = col_logical()
                    , Language = col_character()
                    , `Computed Score` = col_double()
                    # , `Uncorrected Standard Score` = col_integer()
                    # , `Age-Corrected Standard Score` = col_integer()
                    # , `National Percentile (age adjusted)` = col_integer()
                    # , `Fully-Corrected T-score` = col_integer()
                    , InstrumentBreakoff = col_integer()
                    , InstrumentStatus2 = col_integer()
                    , InstrumentRCReason = col_integer()
                    , InstrumentRCReasonOther = col_character()
                    # , `App Version` = col_character()
                    # , `iPad Version` = col_character()
                    # , `Firmware Version` = col_character()
                  ))


# glimpse(ipad1)
# glimpse(ipad2)
# glimpse(ipad3)
# glimpse(ipad4)

# Rowbind all iPad datasets together
all_ipads <- bind_rows(ipad1, ipad2, ipad3, ipad4)

# Clean up dataset a bit
all_ipads <- all_ipads %>% 
  mutate(DateFinished = as_date(DateFinished)) %>% 
  select(-Column1, -Column2, -Column3, -Column4, -Column5) %>% 
  select(-`App Version`, -`iPad Version`, -`Firmware Version`)

# Loop over dataset adding appropriate dates to NA date cells
for (i in 2:nrow(all_ipads)) {

  prev_pin <- all_ipads[[i-1, "PIN"]]
  curr_pin <- all_ipads[[i, "PIN"]]
  
  prev_date <- all_ipads[[i-1, "DateFinished"]]
  curr_date <- all_ipads[[i, "DateFinished"]]
  
  if (is.na(curr_date) & prev_pin == curr_pin) {
    all_ipads[[i, "DateFinished"]] <- prev_date
  }
}

# glimpse(all_ipads)

# Normalize IDs
all_ipads <- all_ipads %>% 
  mutate(PIN = str_replace(PIN, "\\(.*\\)", "")) %>% 
  mutate(PIN = str_replace(PIN, "v.*$", "")) %>% 
  mutate(PIN = str_trim(PIN))

names(all_ipads)
names(all_ipads) <- 
  gsub(pattern = "\\.+", replacement = "_", x = names(all_ipads))
names(all_ipads) <- 
  gsub(pattern = "_$", replacement = "", x = names(all_ipads))
names(all_ipads) <- 
  gsub(pattern = " ", replacement = "_", x = names(all_ipads))
names(all_ipads) <- 
  gsub(pattern = "-", replacement = "_", x = names(all_ipads))
names(all_ipads) <- 
  gsub(pattern = "\\(|\\)", replacement = "", x = names(all_ipads))
names(all_ipads) <- tolower(names(all_ipads))
names(all_ipads)

all_ipads <- purrr::map_df(all_ipads, ~ as.character(.x))

glimpse(all_ipads)

all_ipads <- all_ipads %>%
  select(pin, datefinished, inst, everything())

all_ipads_long <- 
  tidyr::gather(all_ipads, key, value, rawscore:instrumentrcreasonother)
unique(all_ipads_long$inst)

all_ipads_long <- all_ipads_long %>% 
  dplyr::mutate(inst = dplyr::case_when(
    inst == 
      "NIH Toolbox Picture Vocabulary Test Age 3+ v2.0" ~ 
      "tb_picvocab",              
    inst == 
      "NIH Toolbox Flanker Inhibitory Control and Attention Test Age 12+ v2.1" ~
      "tb_flanker",
    inst == 
      "NIH Toolbox List Sorting Working Memory Test Age 7+ v2.1" ~
      "tb_listsort",
    inst == 
      "NIH Toolbox Dimensional Change Card Sort Test Age 12+ v2.1" ~
      "tb_cardsort",
    inst == 
      "NIH Toolbox Pattern Comparison Processing Speed Test Age 7+ v2.1" ~
      "tb_pattern",
    inst == 
      "NIH Toolbox Picture Sequence Memory Test Age 8+ Form A v2.1" ~
      "tb_picseq",
    inst == 
      "NIH Toolbox Oral Reading Recognition Test Age 3+ v2.0" ~
      "tb_oral",
    inst == 
      "Cognition Fluid Composite v1.1" ~
      "cog_fluid",
    inst == 
      "Cognition Crystallized Composite v1.1" ~
      "cog_crys",
    inst == 
      "Cognition Total Composite Score v1.1" ~
      "cog_total",
    inst == 
      "Cognition Early Childhood Composite v1.1" ~
      "cog_child",
    inst ==
      "NIH Toolbox Picture Vocabulary Test Age 3+ Practice v2.0" ~
      "tb_picvocabprac",
    TRUE ~ NA_character_
  ))
# original to long
# all_ipads_long %>% 
#   tidyr::unite(Inst.key, Inst, key, sep = ".")
all_ipads_long <- all_ipads_long %>% 
  tidyr::unite(inst.key, inst, key, sep = ".")
all_ipads_long <- all_ipads_long %>% 
  filter(!is.na(value))

all_ipads_long[c(16479:16490), ]

# long to wide
# all_ipads_long %>% 
#   tidyr::spread(Inst.key, value)
all_ipads_wide <- all_ipads_long %>% 
  tidyr::spread(inst.key, value)
names(all_ipads_wide)

readr::write_csv(all_ipads_wide, "all_ipads_wide.csv", na = "")

# all_ipads_wide %>% 
#   dplyr::select(PIN, dplyr::ends_with("DateFinished")) %>% 
#   head(.)
# dplyr::select(PIN, endsWith("DateFinished"))
# date <- data.frame(date = as_date(all_ipads_wide$datefinished))
# all_ipads_wide %>% dplyr::select(dplyr::ends_with("DateFinished"))
# all_ipads_wide <- bind_cols(all_ipads_wide, tibble::as_tibble(date))
names(all_ipads_wide)
all_ipads_wide <- all_ipads_wide %>% 
  # dplyr::select(-DeviceID, -Assessment_Name) %>% 
  # dplyr::select(-ends_with("datefinished")) %>% 
  dplyr::select(-starts_with("tb_picvocabprac"))
# all_ipads_wide %>% 
#   dplyr::select(dplyr::ends_with("Theta")) %>% 
#   head(.)

all_ipads_wide <- all_ipads_wide %>% 
  dplyr::mutate(pin = ifelse(nchar(pin) == 3, paste0("UM00000", pin), 
                      ifelse(nchar(pin) == 4, paste0("UM0000", pin), NA)))

# all_ipads_wide <- all_ipads_wide %>% 
#   dplyr::rowwise() %>% 
#   dplyr::mutate(redcap_event_name = NA)

all_ipads_wide <- all_ipads_wide %>%
  rename(ptid = pin,
         date = datefinished) %>%
  mutate(date = as_date(date)) %>% 
  select(ptid, `date`, everything())

# Join data to R/C UDS 3 to get redcap_event_name

fields_u3_raw <- c("ptid", "form_date")
fields_u3 <- fields_u3_raw %>% paste(collapse = ",")

json_u3 <- rc_api_get(token = REDCAP_API_TOKEN_UDS3,
                      fields = fields_u3)
df_u3 <- jsonlite::fromJSON(json_u3)
df_u3 <- df_u3 %>% 
  mutate(form_date = as_date(form_date))

# all_ipads_wide_u3 <- 
#   left_join(x = all_ipads_wide, 
#             y = df_u3, 
#             by = c("ptid" = "ptid", "date" = "form_date"))
all_ipads_wide_u3 <-
  FuzzyDateJoin::outer_left(
    x = all_ipads_wide,
    y = df_u3,
    x_id_col = "ptid",
    y_id_col = "ptid",
    x_date_col = "date",
    y_date_col = "form_date",
    x_intvl_less = 120L,
    x_intvl_more = 120L,
    keep_y_id = TRUE
  ) %>% 
  rename(ptid = ptid_x) %>% 
  select(-ptid_y, -form_date)

all_ipads_wide_u3 <- all_ipads_wide_u3 %>% 
  select(ptid, redcap_event_name, date, everything())

all_ipads_wide_u3 <- all_ipads_wide_u3 %>% 
  filter(!is.na(redcap_event_name)) %>% 
  filter(redcap_event_name != "visit_3_arm_1")

all_ipads_wide_u3 <- all_ipads_wide_u3 %>% 
  select(-deviceid, -assessment_name)

names(all_ipads_wide_u3) <- 
  gsub(pattern = "\\.", replacement = "", x = names(all_ipads_wide_u3))

write.csv(all_ipads_wide_u3, 
          "all_iPads_wide_u3_2019-01-10.csv", 
          na = "", 
          row.names = FALSE)
























