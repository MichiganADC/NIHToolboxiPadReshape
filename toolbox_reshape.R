#!/usr/bin/env Rscript 

#####
# Script to reshape NIH Toolbox data (iPads) from semi-wide to wide
# for upload to REDCap
####

ipad1 <- read.csv("ALL_Ipad1_AssessmentScores_2-20-18.csv", stringsAsFactors = FALSE, na.strings = "")
ipad2 <- read.csv("ALL_Ipad2_AssessmentScores_2-20-18.csv", stringsAsFactors = FALSE, na.strings = "")
ipad3 <- read.csv("ALL_Ipad3_AssessmentScores_2-19-18.csv", stringsAsFactors = FALSE, na.strings = "")
ipad4 <- read.csv("ALL_Ipad4_AssessmentScores_2-20-18.csv", stringsAsFactors = FALSE, na.strings = "")

# class(ipad1$DateFinished); head(ipad1$DateFinished)
# class(ipad2$DateFinished); head(ipad2$DateFinished)
# class(ipad3$DateFinished); head(ipad3$DateFinished)
# class(ipad4$DateFinished); head(ipad4$DateFinished)

# as.POSIXct(ipad1$DateFinished)
ipad1$DateFinished <- as.Date(as.POSIXct(ipad1$DateFinished))
# as.POSIXct(ipad2$DateFinished)
ipad2$DateFinished <- as.Date(as.POSIXct(ipad2$DateFinished))
# as.POSIXct(ipad3$DateFinished, format = "%m/%d/%y %H:%M") # data exported to xlsx, saved as csv
ipad3$DateFinished <- as.Date(as.POSIXct(ipad3$DateFinished, format = "%m/%d/%y %H:%M"))
# as.POSIXct(ipad4$DateFinished)
ipad4$DateFinished <- as.Date(as.POSIXct(ipad4$DateFinished))

allipads <- rbind(ipad1, ipad2)
allipads <- rbind(allipads, ipad3)
allipads <- rbind(allipads, ipad4)

# names(allipads)
names(allipads) <- gsub(pattern = "\\.+", replacement = "_", x = names(allipads))
names(allipads) <- gsub(pattern = "_$", replacement = "", x = names(allipads))
# names(allipads)

library(magrittr) # for pipe operator: %>%

allipads_long <- 
  tidyr::gather(allipads, key, value, RawScore:InstrumentRCReasonOther)
allipads_long <- allipads_long %>% 
  dplyr::mutate(Inst = dplyr::case_when(
    Inst == 
      "NIH Toolbox Picture Vocabulary Test Age 3+ v2.0" ~ 
      "TB_PicVocab",              
    Inst == 
      "NIH Toolbox Flanker Inhibitory Control and Attention Test Age 12+ v2.1" ~
      "TB_Flanker",
    Inst == 
      "NIH Toolbox List Sorting Working Memory Test Age 7+ v2.1" ~
      "TB_ListSort",
    Inst == 
      "NIH Toolbox Dimensional Change Card Sort Test Age 12+ v2.1" ~
      "TB_CardSort",
    Inst == 
      "NIH Toolbox Pattern Comparison Processing Speed Test Age 7+ v2.1" ~
      "TB_Pattern",
    Inst == 
      "NIH Toolbox Picture Sequence Memory Test Age 8+ Form A v2.1" ~
      "TB_PicSeq",
    Inst == 
      "NIH Toolbox Oral Reading Recognition Test Age 3+ v2.0" ~
      "TB_Oral",
    Inst == 
      "Cognition Fluid Composite v1.1" ~
      "Cog_Fluid",
    Inst == 
      "Cognition Crystallized Composite v1.1" ~
      "Cog_Crys",
    Inst == 
      "Cognition Total Composite Score v1.1" ~
      "Cog_Total",
    Inst == 
      "Cognition Early Childhood Composite v1.1" ~
      "Cog_Child"
  ))
# original to long
# allipads_long %>% 
#   tidyr::unite(Inst.key, Inst, key, sep = ".")
allipads_long <- allipads_long %>% 
  tidyr::unite(Inst.key, Inst, key, sep = ".")

# long to wide
# allipads_long %>% 
#   tidyr::spread(Inst.key, value)
allipads_wide <- allipads_long %>% 
  tidyr::spread(Inst.key, value)

# allipads_wide %>% 
#   dplyr::select(PIN, dplyr::ends_with("DateFinished")) %>% 
#   head(.)
# dplyr::select(PIN, endsWith("DateFinished"))
Date <- lubridate::as_date(as.numeric(allipads_wide$TB_PicVocab.DateFinished))
# allipads_wide %>% dplyr::select(dplyr::ends_with("DateFinished"))
allipads_wide <- cbind(allipads_wide, Date)
# allipads_wide %>% 
#   dplyr::select(TB_PicVocab.DateFinished, Date) %>% 
#   dplyr::mutate(new_date = lubridate::as_date(as.numeric(allipads_wide$TB_PicVocab.DateFinished)))
allipads_wide <- allipads_wide %>% 
  dplyr::select(-DeviceID, -Assessment_Name) %>% 
  dplyr::select(-dplyr::ends_with("DateFinished"))
# allipads_wide %>% 
#   dplyr::select(dplyr::ends_with("Theta")) %>% 
#   head(.)

allipads_wide <- allipads_wide %>% 
  dplyr::mutate(PIN = ifelse(nchar(PIN) == 3, paste0("UM00000", PIN), 
                      ifelse(nchar(PIN) == 4, paste0("UM0000", PIN), NA)))

allipads_wide <- allipads_wide %>% 
  dplyr::rowwise() %>% 
  dplyr::mutate(redcap_event_name = NA)

allipads_wide <- allipads_wide %>%
  dplyr::rename(UDS_ID = PIN) %>%
  dplyr::select(UDS_ID, redcap_event_name, Date, dplyr::everything())

write.csv(allipads_wide, "all_iPads_wide.csv", na = "", row.names = FALSE)
























