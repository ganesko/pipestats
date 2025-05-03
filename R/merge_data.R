globalVariables(c("incident_gas_distribution_1970_mid1984",
                  "incident_gas_distribution_mid1984_feb2004",
                  "incident_gas_distribution_mar2004_dec2009",
                  "incident_gas_distribution_jan2010_mar2025"))
#' Clean and Join Gas Pipeline Incident Datasets from 1970 to Present
#' @description
#' This function prepares the variables in each of the datasets from the four time periods to be joined into one full dataset by renaming direct equivalent variables in the first three datasets to their names in the most recent dataset and creating equivalent variables to the most recent dataset's variables from other variables in the first three datasets when they encode equivalent information. This function then joins them and returns the data as a dataframe, which the user can save in their local environment.
#'
#' @return A dataframe of the four datasets joined together.
#'
#' @examples
#' data <- merge_data()
#' print(paste("Since 1970, there have been",as.character(nrow(data)),"gas pipeline incidents."))
#'
#' @import dplyr
#' @import lubridate
#' @import readr
#' @import utils
#' @export

# define function
merge_data <- function() {

  # load data
  df1 <- incident_gas_distribution_1970_mid1984
  df2 <- incident_gas_distribution_mid1984_feb2004
  df3 <- incident_gas_distribution_mar2004_dec2009

  if (exists("incident_gas_distribution_jan2010_present", envir = .GlobalEnv)) {
    df4 <- get("incident_gas_distribution_jan2010_present", envir = .GlobalEnv)
    message("Joining previous data with most recent incident data.")
  } else {
    df4 <- incident_gas_distribution_jan2010_mar2025
    message("Joining previous data with package data up until March 2025.\nThis full dataset is identical to cleaned_full_incident_data_1970_mar2025 included in this package.\nSee documentation for load_recent_data() for information on how to load most recent data.")
  }

  # extract year from df2 date variable
  df2$IDATE <- ymd(df2$IDATE) # parse df2's incident date variable to Date object
  df2$IYEAR <- year(ymd(df2$IDATE)) # extract year from df2's incident date variable (type = double)
  df2$IYEAR <- as.integer(df2$IYEAR) # cast as integer

  # Report ID 1840836 in df1 reported the time the incident took place as 7400.
  # This is not a valid time and failed to parse, so we have replaced the time with 0000 so we will not have to exclude this observation from our datetime analysis.
  df1[14609, 'DETECTION_TIME'] <- 0000

  # parse df1 incident date and time variables as datetime
  df1$DETECTION_TIME <- sprintf("%04d", df1$DETECTION_TIME) # add 0s to front of time so all times are 4 digits
  df1$DETECTION_TIME <- paste(substring(df1$DETECTION_TIME, 1, 2), substring(df1$DETECTION_TIME, 3, 4), sep=':') # add colon to times
  df1$LOCAL_DATETIME <- paste(df1$INCIDENT_DATE, df1$DETECTION_TIME, sep=" ") # create combined datetime variable
  df1$LOCAL_DATETIME <- mdy_hm(df1$LOCAL_DATETIME) # parse as datetime

  # remove df1 old non-parsed date and time variables
  df1$DETECTION_TIME <- NULL
  df1$INCIDENT_DATE <- NULL

  # Report IDs 19920041 and 19920041 in df2 reported the times the incidents took place as NA.
  # This is not a valid time and failed to parse, so we have replaced the time with 0000 so we will not have to exclude this observation from our datetime analysis.
  df2[1372, 'DTHH'] <- 0000
  df2[1978, 'DTHH'] <- 0000

  # parse df2 incident date and time variables as datetime
  df2$DTHH <- sprintf("%04d", df2$DTHH) # add 0s to front of time so all times are 4 digits
  df2$DTHH <- paste(substring(df2$DTHH, 1, 2), substring(df2$DTHH, 3, 4), sep=':') # add colon to times
  df2$LOCAL_DATETIME <- paste(df2$IDATE, df2$DTHH, sep=" ") # create combined datetime variable
  df2$LOCAL_DATETIME <- ymd_hm(df2$LOCAL_DATETIME) # parse as datetime

  # remove df2 old non-parsed date and time variables
  df2$DTHH <- NULL
  df2$IDATE <- NULL

  # parse df3 incident date and time variables as datetime
  df3$IHOUR <- sprintf("%04d", df3$IHOUR) # add 0s to front of time so all times are 4 digits
  df3$IHOUR <- paste(substring(df3$IHOUR, 1, 2), substring(df3$IHOUR, 3, 4), sep=':') # add colon to times
  df3$LOCAL_DATETIME <- paste(df3$IDATE, df3$IHOUR, sep=" ") # create combined datetime variable
  df3$LOCAL_DATETIME <- mdy_hm(df3$LOCAL_DATETIME) # parse as datetime

  # remove df3 old non-parsed date and time variables
  df3$IHOUR <- NULL
  df3$IDATE <- NULL

  # parse df4 incident date and time variable as datetime
  df4$LOCAL_DATETIME <- mdy_hm(df4$LOCAL_DATETIME)

  # parse dates of initial operator report to the National Response Center and rename
  df2$TELRT <- ymd(df2$TELRT)
  df2 <- rename(df2, TELDT = TELRT)
  df3$TELDT <- mdy(df3$TELDT)
  df4$NRC_RPT_DATETIME <- mdy_hm(df4$NRC_RPT_DATETIME)
  df4$TELDT <- date(df4$NRC_RPT_DATETIME) # create TELDT in df4

  # add first two digits of year to df1 year cathodic protection variable, cast as integer, and rename
  # df2 and df3 year cathodic protection variables did not need any changes so they were renamed in the renaming section
  df1$YEAR_CATH_PROT_STARTED <- ifelse(df1$YEAR_CATH_PROT_STARTED != 0, df1$YEAR_CATH_PROT_STARTED <- paste('19', df1$YEAR_CATH_PROT_STARTED, sep=""), 0)
  df1$YEAR_CATH_PROT_STARTED <- as.integer(df1$YEAR_CATH_PROT_STARTED)
  df1 <- rename(df1, CATHODIC_PRO_START_YEAR = YEAR_CATH_PROT_STARTED)

  # convert report recieved date to date type in df2 and df4
  df2$REPORT_RECEIVED_DATE <- ymd(df2$DOR)
  df4$REPORT_RECEIVED_DATE <- mdy(df4$REPORT_RECEIVED_DATE)

  # remove old df2 DOR variable
  df2$DOR <- NULL

  # convert maop established date to date type in df4
  df4$MAOP_ESTABLISHED_DATE <- mdy(df4$MAOP_ESTABLISHED_DATE)

  # convert corrosion hydrotest leak survey date to date type in df4
  df4$COR_HYDROTEST_LEAK_SURVEY_DATE <- mdy(df4$COR_HYDROTEST_LEAK_SURVEY_DATE)

  # convert excavation hydrotest leak survey date to date type in df4
  df4$EX_HYDROTEST_LEAK_SURVEY_DATE <- mdy(df4$EX_HYDROTEST_LEAK_SURVEY_DATE)

  # convert other outside force damage hydrotest leak survey date to date type in df4
  df4$OSF_HYDROTEST_LEAK_SURVEY_DATE <- mdy(df4$OSF_HYDROTEST_LEAK_SURVEY_DATE)

  # convert pressure test year (in df3-4) to datetime
  #combine test day, month and year into datetime
  df3$TEST_MO <- as.numeric(df3$TEST_MO)
  df3$TEST_DAY <- as.numeric(df3$TEST_DAY)
  df3$TEST_YR <- as.numeric(df3$TEST_YR)
  df3$TEST_MO <- sprintf("%02d", df3$TEST_MO)
  df3$TEST_DAY <- sprintf("%02d", df3$TEST_DAY)
  df3$TEST_DATE <- paste(df3$TEST_YR, df3$TEST_MO, df3$TEST_DAY, sep="-") # create combined date variable
  #test date does not exist in df4, will be dropped in merge

  #parse as date variable
  df3$TEST_DATE<- ymd(df3$TEST_DATE, quiet=TRUE)

  # rename pressure test year variable to df4 equivalent
  df3$HYDROTEST_CONDUCTED_YEAR <- as.numeric(df3$TEST_YR)

  # remove old df3 variables
  df3$TEST_MO <- NULL
  df3$TEST_DAY <- NULL
  df3$TEST_YR <- NULL

  # add employee fatalities and non-employee fatalites to get total fatalities for df1
  df1$FATAL <- as.numeric(df1$EMPLOYEE_FATALITIES) + as.numeric(df1$NON_EMPLOYEE_FATALITIES)

  # add employee injuries and non-employee injuries to get total injuries for df1
  df1$INJURE <- as.numeric(df1$EMPLOYEE_INJURIES) + as.numeric(df1$NON_EMPLOYEE_INJURIES)

  #convert to numeric
  df4$EST_COST_UNINTENTIONAL_RELEASE <- as.numeric(df4$EST_COST_UNINTENTIONAL_RELEASE)
  df4$EST_COST_INTENTIONAL_RELEASE <- as.numeric(df4$EST_COST_INTENTIONAL_RELEASE)

  #sum the two variables to get total gas cost
  df4$GASPRP <- df4$EST_COST_UNINTENTIONAL_RELEASE+df4$EST_COST_INTENTIONAL_RELEASE

  # create corrosion location variable for df3
  df3 <- df3 %>%
    mutate(INTERNAL_EXTERNAL = case_when(
      CAUSE_DETAILS_TEXT == "CORROSION, EXTERNAL" ~ "EXTERNAL CORROSION",
      CAUSE_DETAILS_TEXT == "CORROSION, INTERNAL" ~ "INTERNAL CORROSION",
      TRUE ~ "" # all other values for CAUSE_DETAILS_TEXT become empty string in INTERNAL_EXTERNAL
    ))

  # create cause of corrosion indicator variables in df1
  df1$MICROBIOLOGICAL_CORROSION_IND <- ifelse(df1$CAUSE_CORROSION == "BACTERIAL", "YES", "")
  df1$INT_MICROBIOLOGICAL_IND <- ifelse(df1$CAUSE_CORROSION == "BACTERIAL", "YES", "")
  df1$GALVANIC_CORROSION_IND <- ifelse(df1$CAUSE_CORROSION == "GALVANIC", "YES", "")
  df1$STRAY_CURRENT_CORROSION_IND <- ifelse(df1$CAUSE_CORROSION == "STRAY CURRENT", "YES", "")
  df1$ATMOSPHERE_CORROSION_IND <- ifelse(grepl("ATMOSP", df1$CAUSE_CORROSION_OTHER), "YES", "") # grepl() returns TRUE if df1$CAUSE_CORROSION_OTHER contains the substring "ATMOSP"
  df1$OTHER_CORROSION_IND <- ifelse(((df1$CAUSE_CORROSION == "OTHER") & df1$ATMOSPHERE_CORROSION_IND == ""), "YES", "") # set equal to YES only if CAUSE_CORROSION is OTHER and it's not atmospheric
  df1$INT_OTHER_CORROSION_IND <- ifelse(((df1$CAUSE_CORROSION == "OTHER") & df1$ATMOSPHERE_CORROSION_IND == ""), "YES", "") # set equal to YES only if CAUSE_CORROSION is OTHER and it's not atmospheric
  df1$INT_CORROSION_TYPE_DETAILS = df1$CAUSE_CORROSION_OTHER # save other details info
  df1$CAUSE_CORROSION <- NULL # drop old df1 CAUSE_CORROSION variable now that all information has been stored in indicator variables

  # create cause of corrosion indicator variables in df2
  df2$GALVANIC_CORROSION_IND <- ifelse(df2$CAUSR == "GALVANIC", "YES", "")
  df2$ATMOSPHERE_CORROSION_IND <- ifelse(grepl("ATMOSPHERIC", df2$CAUCO), "YES", "") # grepl() returns TRUE if df2$CAUCO contains the substring "ATMOSPHERIC"
  df2$OTHER_CORROSION_IND <- ifelse(((df2$CAUSR == "OTHER") & df2$ATMOSPHERE_CORROSION_IND == ""), "YES", "") # set equal to YES only if CAUSR is OTHER and it's not atmospheric
  df2$INT_OTHER_CORROSION_IND <- ifelse(((df2$CAUSR == "OTHER") & df2$ATMOSPHERE_CORROSION_IND == ""), "YES", "")
  df2$INT_CORROSION_TYPE_DETAILS = df2$CAUCO # save other details info
  df2$CAUSR <- NULL # drop old df2 CAUSR variable now that all information has been stored in indicator variables
  df2$CAULO <- NULL # drop old df2 CAULO variable now that all information has been stored in indicator variables

  # create cause of corrosion indicator variables in df3
  df3$GALVANIC_CORROSION_IND <- ifelse(df3$COR_CAUSE_TEXT == "GALVANIC", "YES", "")
  df3$MICROBIOLOGICAL_CORROSION_IND <- ifelse(df3$COR_CAUSE_TEXT == "MICROBIOLOGICAL", "YES", "")
  df3$INT_MICROBIOLOGICAL_IND <- ifelse(df3$COR_CAUSE_TEXT == "MICROBIOLOGICAL", "YES", "")
  df3$STRAY_CURRENT_CORROSION_IND <- ifelse(df3$COR_CAUSE_TEXT == "STRAY CURRENT", "YES", "")
  df3$ATMOSPHERE_CORROSION_IND <- ifelse(grepl("ATMOSPHERIC", df3$COR_CAUSEO), "YES", "") # grepl() returns TRUE if df3$COR_CAUSEO contains the substring "ATMOSPHERIC"
  df3$OTHER_CORROSION_IND <- ifelse((((df3$COR_CAUSE_TEXT == "OTHER") & (df3$ATMOSPHERE_CORROSION_IND == "")) | df3$COR_CAUSE_TEXT == "IMPROPER CATHODIC PROTECTION"), "YES", "") # set equal to YES only if COR_CAUSE_TEXT is OTHER and it's not atmospheric, or if COR_CAUSE_TEXT is IMPROPER CATHODIC PROTECTION
  df3$INT_OTHER_CORROSION_IND <- ifelse((((df3$COR_CAUSE_TEXT == "OTHER") & (df3$ATMOSPHERE_CORROSION_IND == "")) | df3$COR_CAUSE_TEXT == "IMPROPER CATHODIC PROTECTION"), "YES", "")
  df3$COR_CAUSEO <- ifelse(df3$COR_CAUSE_TEXT == "IMPROPER CATHODIC PROTECTION", "IMPROPER CATHODIC PROTECTION", df3$COR_CAUSEO) # set equal to "IMPROPER CATHODIC PROTECTION" if necessary, so we don't lose information when COR_CAUSE_TEXT is dropped
  df3$INT_CORROSION_TYPE_DETAILS = df3$COR_CAUSEO # save other details info
  df3$COR_CAUSE_TEXT <- NULL # drop COR_CAUSE_TEXT now that all information has been stored in indicator variables

  # create visual description of corrosion variables for df1-3
  # these are lined these up with the appropriate location (internal or external, based on the corrosion location variable)

  # create variables for df1
  df1$VISUAL_EXAM_RESULTS <- ifelse(df1$LOCATION_CORROSION == "EXTERNALLY", df1$DESCRIPTION_CORROSION, NA)
  df1$INT_VISUAL_EXAM_RESULTS <- ifelse(df1$LOCATION_CORROSION == "INTERNALLY", df1$DESCRIPTION_CORROSION, NA)
  df1$VIS_EXAM_TEXT <- df1$DESCRIPTION_CORROSION
  df1$DESCRIPTION_CORROSION <- NULL # remove old df1 variable

  # create variables for df2
  df2$VISUAL_EXAM_RESULTS <- ifelse(df2$LOC == "EXTERNALLY", df2$ADESC, NA)
  df2$INT_VISUAL_EXAM_RESULTS <- ifelse(df2$LOC == "INTERNALLY", df2$ADESC, NA)
  df2$VIS_EXAM_TEXT <- df2$ADESC
  df2$ADESC <- NULL # remove old df2 variable
  df2$VIS_EXAMO <- df2$ADESCO
  df2$ADESCO <- NULL # remove old df2 variable

  # create variables for df3
  df3$VISUAL_EXAM_RESULTS <- ifelse(df3$INTERNAL_EXTERNAL == "EXTERNAL CORROSION", df3$VIS_EXAM_TEXT, NA)
  df3$INT_VISUAL_EXAM_RESULTS <- ifelse(df3$INTERNAL_EXTERNAL == "INTERNAL CORROSION", df3$VIS_EXAM_TEXT, NA)

  # create variables for df4
  df4$VIS_EXAM_TEXT <- coalesce(df4$VISUAL_EXAM_RESULTS, df4$INT_VISUAL_EXAM_RESULTS)
  df4$VIS_EXAMO <- coalesce(df4$VISUAL_EXAM_DETAILS, df4$INT_VISUAL_EXAM_DETAILS) # INT_VISUAL_EXAM_DETAILS is all NA in original df

  # create excavation damage type variable for df3
  df3$EX_PARTY_TYPE <- ifelse(!(is.na(df3$THIRD_PARTY_GRP)), "EXCAVATION DAMAGE BY THIRD PARTY", "NOT SPECIFIED")

  # create notification recieved from indicator variables for df3
  df3$ONE_CALL_SYSTEM_IND <- ifelse(df3$NOTIF_RCVD_TEXT == "ONE-CALL SYSTEM", "YES", "")
  df3$EXCAVATOR_IND <- ifelse(df3$NOTIF_RCVD_TEXT == "EXCAVATOR", "YES", "")
  df3$CONTRACTOR_IND <- ifelse(df3$NOTIF_RCVD_TEXT == "GENERAL CONTRACTOR", "YES", "")
  df3$LANDOWNER_IND <- ifelse(df3$NOTIF_RCVD_TEXT == "LANDOWNER", "YES", "")
  df3$NOTIF_RCVD_TEXT <- NULL

  # create result of construction indicator variable for df1 and df2
  df1$RESULT_CONSTRUCTION_IND <- ifelse(df1$CAUSE_LEAK_CONSTRUCTION == "CONSTRUCTION DEFECT", "YES", "")
  df2$RESULT_CONSTRUCTION_IND <- ifelse(df2$CAULC != "", "YES", "")

  # create material defect indicator variable for df1
  df1$RESULT_MATERIAL_IND <- ifelse(df1$CAUSE_LEAK_CONSTRUCTION == "MATERIAL FAILURE", "YES", "")

  # convert MALFUNC_TEXT into indicator variables for df3
  df3$EQ_FAILURE_TYPE <- case_when(
    df3$MALFUNC_TEXT != "" ~ "MALFUNCTION OF CONTROL/RELIEF EQUIPMENT",
    df3$THREADS_TEXT == "NIPPLES" ~ "THREADED CONNECTION FAILURE"
  )
  df3$PRESSURE_REGULATOR_IND <- ifelse(df3$MALFUNC_TEXT == "PRESSURE REGULATOR", "YES", "")

  # convert IO_TYPE_TEXT into indicator variables for df3
  df3$RELATED_INADEQUATE_PROC_IND <- ifelse(df3$IO_TYPE_TEXT == "INADEQUATE PROCEDURES", "YES", "")
  df3$RELATED_FAILURE_FOLLOW_IND <- ifelse(df3$IO_TYPE_TEXT == "FAILURE TO FOLLOW PROCEDURES", "YES", "")

  # create wall thickness variable in df4
  df4 <- df4 %>%
    mutate(WALLTHK = WT_PLASTIC) %>%
    mutate(WALLTHK = ifelse(is.na(WT_PLASTIC), WT_STEEL, WALLTHK))

  # create puncture diameter variable in df4
  df4$PUNC_DIAM = (df4$PUNCTURE_AXIAL + df4$PUNCTURE_CIRCUM)/2

  # create preparer name and title variable in df4
  df4$PNAME <- case_when(
    (df4$PREPARER_NAME == "") & (df4$PREPARER_TITLE == "") ~ "", # if no preparer info, PNAME is empty string
    (df4$PREPARER_TITLE == "") ~ df4$PREPARER_NAME, # if no title, then PNAME is the name
    (df4$PREPARER_NAME == "") ~ df4$PREPARER_TITLE, # if no name, then PNAME is the title
    TRUE ~ paste(df4$PREPARER_NAME, df4$PREPARER_TITLE, sep=", ") # otherwise, PNAME is Name, Title
  )

  # standardize categories for cause of incident variables in df1-3
  df1 <- df1 %>% mutate(CAUSE_IDENTIFIER = recode_factor(CAUSE_IDENTIFIER, "CORROSION" = "CORROSION FAILURE", "OTHER" = "OTHER INCIDENT CAUSE"))

  df2 <- df2 %>% mutate(CAUSE = recode_factor(CAUSE, "CORROSION" = "CORROSION FAILURE", "OTHER" = "OTHER INCIDENT CAUSE"))

  df3 <- df3 %>% mutate(CAUSE_TEXT = recode_factor(CAUSE_TEXT,
                                                   "CORROSION" = "CORROSION FAILURE",
                                                   "EQUIPMENT" = "EQUIPMENT FAILURE",
                                                   "MATERIAL AND/OR WELD FAILURES" = "PIPE, WELD, OR JOINT FAILURE",
                                                   "NATURAL FORCES" = "NATURAL FORCE DAMAGE",
                                                   "OTHER" = "OTHER INCIDENT CAUSE"
  ))

  # create excavation damage and natural force damage categories in df1 and df2

  df1$CAUSE_IDENTIFIER <- as.character(df1$CAUSE_IDENTIFIER)

  df1$CAUSE_IDENTIFIER <- ifelse(
    ((df1$CAUSE_IDENTIFIER == "DAMAGE BY OUTSIDE FORCES") & (df1$CAUSE_LEAK == "DAMAGE BY EQUIPMENT BY OUTSIDE PARTY" | df1$CAUSE_LEAK == "DAMAGE BY EQUIPMENT OPERATED BY OR FOR OPERATOR")),
    "EXCAVATION DAMAGE",
    ifelse(
      ((df1$CAUSE_IDENTIFIER == "DAMAGE BY OUTSIDE FORCES") & (df1$CAUSE_LEAK == "DAMAGE BY EARTH MOVEMENT")),
      "NATURAL FORCE DAMAGE",
      ifelse(
        (df1$CAUSE_IDENTIFIER == "DAMAGE BY OUTSIDE FORCES"),
        "OTHER OUTSIDE FORCE DAMAGE",
        df1$CAUSE_IDENTIFIER)))
  df1$CAUSE_IDENTIFIER <- ifelse(df1$CAUSE_IDENTIFIER == "CONSTRUCTION DEFECT/MATERIAL FAILURE",
                                 df1$CAUSE_LEAK_CONSTRUCTION, df1$CAUSE_IDENTIFIER)

  df2$CAUSE <- as.character(df2$CAUSE)

  df2$CAUSE <- ifelse(
    ((df2$CAUSE == "DAMAGE BY OUTSIDE FORCES") & (df2$CAULK == "OPERATOR ACTION" | df2$CAULK == "OUTSIDE/THIRD PARTY")),
    "EXCAVATION DAMAGE",
    ifelse(
      ((df2$CAUSE == "DAMAGE BY OUTSIDE FORCES") & (df2$CAULK == "EARTH MOVEMENT: FROST" | df2$CAULK == "EARTH MOVEMENT: LANDSLIDE/WASHOUT" | df2$CAULK == "EARTH MOVEMENT: OTHER" | df2$CAULK == "EARTH MOVEMENT: SUBSIDENCE" | df2$CAULK == "LIGHTNING OR FIRE")),
      "NATURAL FORCE DAMAGE",
      ifelse(
        ((df2$CAUSE == "DAMAGE BY OUTSIDE FORCES") & (df2$CAULK == "NO DATA")),
        "OTHER OUTSIDE FORCE DAMAGE",
        ifelse(
          df2$CAUSE == "",
          "NO DATA",
          df2$CAUSE
        ))))

  # standardize categories for cause of incident details variable in df3
  df3 <- df3 %>% mutate(CAUSE_DETAILS_TEXT = recode_factor(CAUSE_DETAILS_TEXT,
                                                           "CAR, TRUCK OR OTHER VEHICLE NOT RELATED TO EXCAVATION ACTIVITY" = "DAMAGE BY CAR, TRUCK, OR OTHER MOTORIZED VEHICLE/EQUIPMENT NOT ENGAGED IN EXCAVATION",
                                                           "CORROSION, EXTERNAL" = "EXTERNAL CORROSION",
                                                           "CORROSION, INTERNAL" = "INTERNAL CORROSION",
                                                           "EARTH MOVEMENT" = "EARTH MOVEMENT, NOT DUE TO HEAVY RAINS/FLOODS",
                                                           "FIRE/EXPLOSION AS PRIMARY CAUSE" = "NEARBY INDUSTRIAL, MAN-MADE, OR OTHER FIRE/EXPLOSION AS PRIMARY CAUSE OF INCIDENT",
                                                           "OPERATOR EXCAVATION DAMAGE" = "EXCAVATION DAMAGE BY OPERATOR (FIRST PARTY)",
                                                           "THIRD PARTY EXCAVATION DAMAGE" = "EXCAVATION DAMAGE BY THIRD PARTY",
                                                           "THREADS STRIPPED" = "THREADED CONNECTION FAILURE"
  ))

  # standardize categories for pipe coated variable in df1
  df1 <- df1 %>% mutate(COATING = recode_factor(COATING, "BARE" = "NO", "COATED" = "YES", "WRAPPED" = "YES", "NO DATA" = ""))

  # standardize categories for pipe coating material variable for df1
  df1 <- df1 %>% mutate(COATING_MATERIAL = recode_factor(COATING_MATERIAL, "NO DATA" = "UNKNOWN"))

  # standardize categories for pipe marking variable in df2
  df2 <- df2 %>% mutate(MARK = recode_factor(MARK, "No" = "NO", "Yes" = "YES", "Null" = "NULL"))

  # standardize categories for cause of construction defect variables in df2 and df3

  df2$CAULC <- ifelse(df2$CAULC == "OTHER", paste("OTHER:", df2$CAULO, sep=" "), df2$CAULC) # If value is OTHER, put the associated details information (CAULO) into CAULC so it's not lost
  df2 <- df2 %>% mutate(CAULC = recode_factor(CAULC, "OPERATING PROCEDURE INAPPROPRIATE" = "POOR CONSTRUCTION/INSTALLATION PROCEDURES", "ERROR IN OPERATING PROCEDURE APPLICATION" = "PROCEDURE NOT FOLLOWED", "POOR WORKMANSHIP DURING CONSTRUCTION" = "POOR WORKMANSHIP", "PHYSICAL DAMAGE DURING CONSTRUCTION" = "POOR WORKMANSHIP", "NO DATA" = ""))

  df3 <- df3 %>% mutate(CONS_DEF_TEXT = recode_factor(CONS_DEF_TEXT, "POOR CONSTRUCTION PROCEDURES" = "POOR CONSTRUCTION/INSTALLATION PROCEDURES"))

  # standardize categories for was the person involved in the incident qualified variable in df3
  df3 <- df3 %>% mutate(CONS_DEF_TEXT = recode_factor(CONS_DEF_TEXT, "YES" = "YES, THEY WERE QUALIFIED FOR THE TASK(S)", "NO" = "NO, THEY WERE NOT QUALIFIED FOR THE TASK(S) NOR WERE THEY PERFORMING THE TASK(S) UNDER THE DIRECTION AND OBSERVATION OF A QUALIFIED INDIVIDUAL"))

  # standardize categories for incident area type variables in df1-3
  df1 <- df1 %>% mutate(LOCATION_LEAK = recode_factor(LOCATION_LEAK, "BELOW GROUND" = "UNDERGROUND", "ABOVE GROUND" = "ABOVEGROUND", "BELOW WATER" = "UNDER WATER"))
  df2 <- df2 %>% mutate(LOCLK = recode_factor(LOCLK, "UNDER GROUND" = "UNDERGROUND", "ABOVE GROUND" = "ABOVEGROUND", "WITHIN/UNDER BUILDING" = "INSIDE/UNDER BUILDING", "UNDER GROUND OR UNDER WATER" = "UNDERGROUND OR UNDER WATER"))
  df3 <- df3 %>% mutate(LOCLK_TEXT = recode_factor(LOCLK_TEXT, "UNDER GROUND" = "UNDERGROUND", "ABOVE GROUND" = "ABOVEGROUND"))

  # standardize categories for material involved variable in df3
  df3 <- df3 %>% mutate(MLKD_TEXT = recode_factor(MLKD_TEXT, "OTHER MATERIAL" = "OTHER"))

  # standardize categories for rupture orientation variable in df3
  df3 <- df3 %>% mutate(RUPTURE_TEXT = recode_factor(RUPTURE_TEXT, "CIRCUMFERENTIAL-SEPARATION" = "CIRCUMFERENTIAL", "LONGITUDINAL-TEAR/CRACK" = "LONGITUDINAL"))

  # standardize categories for corrosion location variables for df1 and df2
  df1 <- df1 %>% mutate(LOCATION_CORROSION = recode_factor(LOCATION_CORROSION, "EXTERNALLY" = "EXTERNAL CORROSION", "INTERNALLY" = "INTERNAL CORROSION"))
  df2 <- df2 %>% mutate(LOC = recode_factor(LOC, "EXTERNALLY" = "EXTERNAL CORROSION", "INTERNALLY" = "INTERNAL CORROSION"))

  # standardize categories for cathodic protection variable for df2
  df2 <- df2 %>% mutate(PROT = recode_factor(PROT, "No" = "NO", "Yes" = "YES", "Null" = ""))

  # standardize categories for prior notification variables in df2 and df3
  df2 <- df2 %>% mutate(NOTIF = recode_factor(NOTIF, "No" = "NO", "Yes" = "YES", "Null" = ""))
  df3 <- df3 %>% mutate(NOTIF = recode_factor(NOTIF, "NULL" = ""))

  # standardize categories for pressure test variable in df3
  df3 <- df3 %>% mutate(PRS_TEST = recode_factor(PRS_TEST, "NULL" = ""))

  # add subtype when incident cause is unknown for df3
  df3 <- df3 %>% mutate(UNKNOWN_TEXT = recode_factor(UNKNOWN_TEXT, "INVESTIGATION COMPLETE" = "INVESTIGATION COMPLETE, CAUSE OF INCIDENT UNKNOWN", "STILL UNDER INVESTIGATION" = "STILL UNDER INVESTIGATION, CAUSE OF INCIDENT TO BE DETERMINED* (*SUPPLEMENTAL REPORT REQUIRED)"))

  # create damage by earth movement variable in all datasets
  df1 <- df1 %>% mutate(NF_EARTH_MOVEMENT = ifelse(df1$DAMAGE_BY_EARTH_MOVEMENT != "", "YES", ""))
  df2 <- df2 %>% mutate(NF_EARTH_MOVEMENT = ifelse(df2$CAULK == "EARTH MOVEMENT: FROST" | df2$CAULK == "EARTH MOVEMENT: LANDSLIDE/WASHOUT" | df2$CAULK == "EARTH MOVEMENT: OTHER" | df2$CAULK == "EARTH MOVEMENT: SUBSIDENCE", "YES", ""))
  df3 <- df3 %>% mutate(NF_EARTH_MOVEMENT = ifelse(df3$EARTH_MOVE_TEXT != "", "YES", ""))
  df4 <- df4 %>% mutate(NF_EARTH_MOVEMENT = ifelse(df4$NATURAL_FORCE_TYPE == "EARTH MOVEMENT, NOT DUE TO HEAVY RAINS/FLOODS", "YES", ""))

  # create natural force other details variable for df3
  df3 <- df3 %>%
    mutate(NF_OTHER_DETAILS = EARTH_MOVEO) %>%
    mutate(NF_OTHER_DETAILS = ifelse(FLOODSO != "", FLOODSO, NF_OTHER_DETAILS)) %>%
    mutate(NF_OTHER_DETAILS = ifelse(TEMPERO != "", TEMPERO, NF_OTHER_DETAILS))

  # remove old variables from df3 now that all information is stored in other details variable
  df3$EARTH_MOVEO <- NULL
  df3$FLOODSO <- NULL
  df3$TEMPERO <- NULL

  # change encoding of depth of grade variable in df4 to substitute missing symbols with an empty string
  Encoding(df4$DEPTH_OF_GRADE) <- "UTF-8"
  df4$DEPTH_OF_GRADE <- iconv(df4$DEPTH_OF_GRADE, "UTF-8", "UTF-8",sub='')
  df4$DEPTH_OF_GRADE <- ifelse(df4$DEPTH_OF_GRADE == "18  36", "18 - 36", df4$DEPTH_OF_GRADE) # add a dash to the value 18 - 36 inches

  # create variables for data source in all datasets
  df1 <- df1 %>% mutate(SOURCE = "incident_gas_distribution_1970_mid1984")
  df2 <- df2 %>% mutate(SOURCE = "incident_gas_distribution_mid1984_feb2004")
  df3 <- df3 %>% mutate(SOURCE = "incident_gas_distribution_mar2004_dec2009")
  df4 <- df4 %>% mutate(SOURCE = "incident_gas_distribution_jan2010_present")

  # standardize variables present in df1 but not df4 to align with df2 and df3 names and values
  df1 <- df1 %>%
    rename(STHH = STOPPAGE_HOURS,
           STMN = STOPPAGE_MINUTES,
           NOTIF_DATE = NOTIFICATION_DATE,
           STAT = STATUTE_REQUIRE_MARKING) %>%
    mutate(MAOPTST = PRESSURE_LEAK_INITIAL) %>% # make variable MAOPTST to combine with df2 and df3
    mutate(NOTIF_DATE = mdy(NOTIF_DATE)) %>%
    mutate(NOTIF_DATE = as.Date(NOTIF_DATE)) # convert to datetime

  # standardize variables present in df2 but not df4 to align with df3 names and values
  df2 <- df2 %>%
    rename(NOTIF_DATE = NOTDT,
           MAOPTST = TEST) %>%
    mutate(NOTIF_DATE = ymd(NOTIF_DATE)) %>%
    mutate(NOTIF_DATE = as.Date(NOTIF_DATE)) # convert to datetime

  # convert notification date variable to datetime in df3
  df3 <- df3 %>%
    mutate(NOTIF_DATE = mdy(NOTIF_DATE)) %>%
    mutate(NOTIF_DATE = as.Date(NOTIF_DATE))

  # standardize marking type variables in df2 and df3
  # we'll use df1's TYPE_MARKING as the basis since it's the most informative
  df2$TYPE_MARKING <- case_when(
    df2$MRKTP == 1 ~ "PERMANENT MARKERS",
    df2$MRKTP == 2 ~ "STAKES",
    df2$MRKTP == 3 ~ "OTHER",
    TRUE ~ "" # otherwise (i.e. Case 0), TYPE_MARKING is empty string
  )
  df3$TYPE_MARKING <- ifelse(df3$PERM_MARK == "YES", "PERMANENT MARKERS", df3$TEMP_MARK_TEXT)
  df2 <- df2 %>% rename(TYPE_MARKING_OTHER = MRKTO)
  df2$MRKTP <- NULL
  df3$TEMP_MARK_TEXT <- NULL
  df3$PERM_MARK <- NULL

  # remove numeric indicator variables from df3
  df3$LRTYPE <- NULL
  df3$LEAK <- NULL
  df3$RUPTURE <- NULL
  df3$EVAC_REASON <- NULL
  df3$TYSYS <- NULL
  df3$PRTFL <- NULL
  df3$MLKD <- NULL
  df3$LOCLK <- NULL
  df3$CAUSE <- NULL
  df3$PIPE_COAT <- NULL
  df3$VIS_EXAM <- NULL
  df3$COR_CAUSE <- NULL
  df3$EARTH_MOVE <- NULL
  df3$FLOODS <- NULL
  df3$TEMPER <- NULL
  df3$THIRD_PARTY_GRP <- NULL
  df3$THIRD_PARTY_TYPE <- NULL
  df3$NOTIF_RCVD <- NULL
  df3$TEMP_MARK <- NULL
  df3$ACC_MARK <- NULL
  df3$FIRE_EXPLO <- NULL
  df3$PIPE_BODY <- NULL
  df3$COMPONENT <- NULL
  df3$JOINT <- NULL
  df3$BUTT <- NULL
  df3$FILLET <- NULL
  df3$PIPE_SEAM <- NULL
  df3$CONS_DEF <- NULL
  df3$MALFUNC <- NULL
  df3$THREADS <- NULL
  df3$IO_TYPE <- NULL
  df3$UNKNOWN <- NULL

  # rename direct equivalent variables in df1 to the names used in df4
  df1 <- df1 %>%
    rename(REPORT_NUMBER= REPORT_ID,
           OPERATOR_ID = OPERATOR_CODE,
           NAME = OPERATOR_NAME,
           LOCATION_STREET_ADDRESS = INCIDENT_ADDRESS,
           LOCATION_CITY_NAME = INCIDENT_CITY,
           LOCATION_COUNTY_NAME = INCIDENT_COUNTY,
           LOCATION_STATE_ABBREVIATION = INCIDENT_STATE,
           LOCATION_POSTAL_CODE = ZIP_CODE,
           NUM_EMP_FATALITIES = EMPLOYEE_FATALITIES,
           NUM_GP_FATALITIES = NON_EMPLOYEE_FATALITIES,
           NUM_GP_INJURIES = NON_EMPLOYEE_INJURIES,
           NUM_EMP_INJURIES = EMPLOYEE_INJURIES,
           IGNITE_IND = GAS_IGNITED,
           EXPLODE_IND = EXPLOSION_OCCURED,
           INCIDENT_AREA_TYPE = LOCATION_LEAK,
           INCIDENT_AREA_DETAILS = LOCATION_LEAK_OTHER,
           DEPTH_OF_COVER = COVER_DEPTH,
           SYSTEM_PART_INVOLVED = LEAK_PART,
           SYSTEM_PART_DETAILS = LEAK_PART_OTHER,
           INSTALLATION_YEAR = YEAR_PART_INSTALLED,
           PIPE_DIAMETER = NOMINAL_DIAMETER,
           PIPE_SPECIFICATION = SPECIFICATION,
           MATERIAL_INVOLVED = MATERIAL_LEAKED_D,
           MATERIAL_DETAILS = MATERIAL_LEAKED_D_OTHER,
           PLASTIC_TYPE = PLASTIC,
           EST_COST_PROP_DAMAGE = OPERATORS_PROPERTY_DAMAGE,
           ACCIDENT_PSIG = ESTIMATE_INCIDENT_PRESSURE,
           MOP_PSIG = MAX_ALLOWABLE_PRESSURE,
           CAUSE = CAUSE_IDENTIFIER,
           INTERNAL_EXTERNAL = LOCATION_CORROSION,
           CORROSION_TYPE_DETAILS = CAUSE_CORROSION_OTHER,
           UNDER_CATHODIC_PROTECTION_IND = CATHODIC_PROTECTION,
           EXTERNALLY_COATED = COATING,
           COATING_TYPE = COATING_MATERIAL,
           COATING_TYPE_DETAILS = COATING_MATERIAL_OTHER,
           CAULK = CAUSE_LEAK,
           DMGO = DAMAGE_BY_EARTH_MOVEMENT_OTHER,
           PRIOR_NOTIFICATION_IND = PRIOR_NOTIFICATION,
           VISIBLE_MARKS = PIPELINE_MARKED,
           PWJF_FAILURE_TYPE = FAIL_PART,
           PWJF_FAILURE_DETAILS = FAIL_PART_OTHER,
           WALLTHK = THICKNESS)

  # rename direct equivalent variables in df2 to the names used in df4
  df2 <- df2 %>%
    rename(REPORT_NUMBER = RPTID,
           OPERATOR_ID = OPID,
           LOCATION_STREET_ADDRESS = INADR,
           LOCATION_CITY_NAME = ACCTY,
           LOCATION_COUNTY_NAME = ACCNT,
           LOCATION_STATE_ABBREVIATION = ACCST,
           LOCATION_POSTAL_CODE = ACZIP,
           NRC_RPT_NUM = TELRN,
           FATAL = FAT,
           INJURE = INJ,
           FEDERAL = IFED,
           INCIDENT_AREA_TYPE = LOCLK,
           INCIDENT_AREA_DETAILS = LOCLO,
           SYSTEM_PART_INVOLVED = PRTLK,
           SYSTEM_PART_DETAILS = PRTLO,
           INSTALLATION_YEAR = PRTYR,
           MANUFACTURED_YEAR = MANYR,
           PIPE_DIAMETER = NMDIA,
           PIPE_SPECIFICATION = SPEC,
           PIPE_MANUFACTURER = MANU,
           MATERIAL_INVOLVED = MLKD,
           MATERIAL_DETAILS = MLKDO,
           CLASS_LOCATION_TYPE = CLASS,
           ACCIDENT_PSIG = INPRS,
           MOP_PSIG = MXPRS,
           MOP_CFR_SECTION = MPEST,
           INTERNAL_EXTERNAL = LOC,
           CORROSION_TYPE_DETAILS = CAUCO,
           UNDER_CATHODIC_PROTECTION_IND = PROT,
           CATHODIC_PRO_START_YEAR = CPYR,
           EXTERNALLY_COATED = COAT,
           PRIOR_NOTIFICATION_IND = NOTIF,
           VISIBLE_MARKS = MARK,
           PWJF_FAILURE_TYPE = PRTFL,
           PWJF_FAILURE_DETAILS = PRTFO,
           RESULT_CONSTRUCTION_SUBTYPE = CAULC,
           PREPARER_TELEPHONE = PTEL,
           WALLTHK = THK)

  # rename direct equivalent variables in df3 to the names used in df4
  df3 <- df3 %>%
    rename(REPORT_NUMBER= RPTID,
           OPERATOR_STREET_ADDRESS = OPSTREET,
           OPERATOR_CITY_NAME = OPCITY,
           OPERATOR_STATE_ABBREVIATION = OPSTATE,
           OPERATOR_POSTAL_CODE = OPZIP,
           LOCATION_STREET_ADDRESS = ACSTREET,
           LOCATION_CITY_NAME = ACCITY,
           LOCATION_COUNTY_NAME = ACCOUNTY,
           LOCATION_STATE_ABBREVIATION = ACSTATE,
           LOCATION_POSTAL_CODE = ACZIP,
           LOCATION_LATITUDE = LATITUDE,
           LOCATION_LONGITUDE = LONGITUDE,
           NRC_RPT_NUM = TELRN,
           NUM_EMP_FATALITIES = EFAT,
           NUM_CONTR_FATALITIES = NFAT,
           NUM_GP_FATALITIES = GPFAT,
           NUM_EMP_INJURIES = EINJ,
           NUM_CONTR_INJURIES = NINJ,
           NUM_GP_INJURIES = GPINJ,
           IGNITE_IND = IGNITE,
           EXPLODE_IND = EXPLO,
           NUM_PUB_EVACUATED= EVACNO,
           FEDERAL = IFED,
           INCIDENT_AREA_TYPE = LOCLK_TEXT,
           INCIDENT_AREA_DETAILS = LOCLKO,
           DEPTH_OF_COVER = DEPTH_COV,
           SYSTEM_PART_INVOLVED = TYSYS_TEXT,
           SYSTEM_PART_DETAILS = TYSYSO,
           INSTALLATION_YEAR = PRTYR,
           MANUFACTURED_YEAR = MANYR,
           PIPE_DIAMETER = NPS,
           PIPE_SPECIFICATION = SPEC,
           PIPE_MANUFACTURER = MANU,
           MATERIAL_INVOLVED = MLKD_TEXT,
           MATERIAL_DETAILS = MLKDO,
           RELEASE_TYPE = LRTYPE_TEXT,
           LEAK_TYPE = LEAK_TEXT,
           RUPTURE_ORIENT = RUPTURE_TEXT,
           RUPTURE_LENGTH = RUPLN,
           CLASS_LOCATION_TYPE = CLASS,
           EST_COST_OPER_PAID = PPPRP,
           EST_COST_PROP_DAMAGE = OPPRP,
           ACCIDENT_PSIG = INC_PRS,
           MOP_PSIG = MAOP,
           MOP_CFR_SECTION = MAOPEST,
           CAUSE = CAUSE_TEXT,
           CAUSE_DETAILS = CAUSE_DETAILS_TEXT,
           CORROSION_TYPE_DETAILS = COR_CAUSEO,
           UNDER_CATHODIC_PROTECTION_IND = PROT,
           CATHODIC_PRO_START_YEAR = CPYR,
           EXTERNALLY_COATED = PIPE_COAT_TEXT,
           PRIOR_DAMAGE = PREV_DAM,
           EARTH_SUBTYPE = EARTH_MOVE_TEXT,
           HEAVY_RAINS_SUBTYPE = FLOODS_TEXT,
           TEMPERATURE_SUBTYPE = TEMPER_TEXT,
           PRIOR_NOTIFICATION_IND = NOTIF,
           VISIBLE_MARKS = MARKED,
           PWJF_FAILURE_TYPE = PRTFL_TEXT,
           PWJF_FAILURE_DETAILS = PRTFLO,
           PIPE_BODY_SUBTYPE = PIPE_BODY_TEXT,
           PIPE_BODY_DETAILS = PIPE_BODYO,
           BUTT_WELD_SUBTYPE = BUTT_TEXT,
           BUTT_WELD_DETAILS = BUTTO,
           FILLET_WELD_SUBTYPE = FILLET_TEXT,
           FILLET_WELD_DETAILS = FILLETO,
           PIPE_SEAM_SUBTYPE = PIPE_SEAM_TEXT,
           PIPE_SEAM_DETAILS = PIPE_SEAMO,
           RESULT_CONSTRUCTION_IND = FAIL_TYPECONS,
           RESULT_MATERIAL_IND = FAIL_TYPEMAT,
           RESULT_CONSTRUCTION_SUBTYPE = CONS_DEF_TEXT,
           HYDROTEST_CONDUCTED_IND = PRS_TEST,
           HYDROTEST_PRESSURE = TEST_PRS,
           OTHER_CONTROL_RELIEF_DETAILS = MALFUNCO,
           OPERATION_RELATED_DETAILS = IO_TYPEO,
           MISC_DETAILS = MISC,
           UNKNOWN_SUBTYPE = UNKNOWN_TEXT,
           PREPARER_EMAIL = PEMAIL,
           PREPARER_TELEPHONE = PTEL
    )

  # convert variables from across datasets to equivalent types to allow for full join
  df1$LOCATION_POSTAL_CODE <- as.character(df1$LOCATION_POSTAL_CODE)
  df3$LOCATION_POSTAL_CODE <- as.character(df3$LOCATION_POSTAL_CODE)
  df4$LOCATION_LATITUDE <- as.character(df4$LOCATION_LATITUDE)
  df4$LOCATION_LONGITUDE <- as.character(df4$LOCATION_LONGITUDE)
  df3$NRC_RPT_NUM <- as.character(df3$NRC_RPT_NUM)
  df2$NRC_RPT_NUM <- as.character(df2$NRC_RPT_NUM)
  df4$INSTALLATION_YEAR <- ifelse(df4$INSTALLATION_YEAR == "UNKNOWN", NA, df4$INSTALLATION_YEAR)
  df4$INSTALLATION_YEAR <- as.numeric(df4$INSTALLATION_YEAR)
  df4$MANUFACTURED_YEAR <- ifelse(df4$MANUFACTURED_YEAR == "UNKNOWN", NA, df4$MANUFACTURED_YEAR)
  df4$MANUFACTURED_YEAR <- as.numeric(df4$MANUFACTURED_YEAR)
  df3$CLASS_LOCATION_TYPE <- ifelse(!is.na(df3$CLASS_LOCATION_TYPE), paste("CLASS", df3$CLASS_LOCATION_TYPE, "LOCATION"), NA)
  df2$CLASS_LOCATION_TYPE <- ifelse(df2$CLASS_LOCATION_TYPE == 0, "NO DATA", paste("CLASS", df2$CLASS_LOCATION_TYPE, "LOCATION"))
  df3$PREPARER_TELEPHONE <- as.character(df3$PREPARER_TELEPHONE)
  df2$PREPARER_TELEPHONE <- as.character(df2$PREPARER_TELEPHONE)
  df3$WALLTHK <- as.character(df3$WALLTHK)
  df2$WALLTHK <- as.character(df2$WALLTHK)
  df1$WALLTHK <- as.character(df1$WALLTHK)

  # bind rows to preserve all observations and variables
  full_data <- bind_rows(df4, df3, df2, df1) %>% arrange(desc(LOCAL_DATETIME))

  # reorder full dataset so all variables with related information are clustered together
  full_data <- full_data[, c(12,1,2,3,4,5,6,7,8,9,478,10,11,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,469,31,32,517,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,531,58,480,481,482,483,59,60,61,62,63,64,65,66,67,68,69,70,554,555,573,574,575,576,577,578,579,580,581,71,72,73,74,75,76,77,78,79,80,81,82,83,532,533,534,535,536,537,538,539,540,541,542,543,544,545,546,547,548,549,550,551,552,553,562,563,84,85,86,87,88,89,90,91,92,93,94,95,488,489,490,524,525,526,527,528,529,96,97,485,486,487,98,500,501,502,503,559,560,561,590,591,99,100,101,588,102,103,104,105,473,106,107,589,108,109,110,474,111,112,113,114,115,479,116,530,117,522,523,118,119,120,121,122,123,124,125,470,126,127,516,128,129,130,131,132,133,134,135,136,484,137,138,139,140,141,142,143,144,145,146,147,148,149,150,151,556,557,558,152,153,154,155,156,157,158,159,160,161,162,163,164,165,166,167,168,169,170,171,172,564,565,566,173,518,519,584,585,586,582,174,175,176,177,178,179,180,181,182,183,184,185,186,187,188,189,190,191,192,193,491,492,194,195,196,197,198,199,200,201,202,571,572,203,204,205,206,207,567,568,569,570,208,209,471,472,210,211,212,213,214,215,216,217,218,219,220,221,222,223,224,225,226,227,228,229,230,231,476,232,233,234,235,236,237,238,239,240,241,242,243,493,494,495,496,583,244,245,246,247,248,249,250,251,252,253,254,255,256,257,258,259,260,261,262,263,264,265,266,267,268,269,270,271,272,273,274,275,276,277,278,279,280,281,497,498,515,520,521,282,283,284,285,286,287,288,289,290,291,292,293,294,295,296,297,298,299,300,301,499,302,303,304,305,306,307,308,309,310,311,312,313,314,315,316,317,318,319,320,321,322,323,324,325,326,327,328,329,330,331,332,333,334,335,336,337,338,339,340,341,342,343,344,345,346,347,348,349,350,351,352,353,354,355,356,357,358,359,360,361,362,363,364,365,366,367,368,504,369,370,371,505,514,372,373,374,375,376,377,378,379,380,381,382,383,506,384,385,507,508,386,387,388,389,390,391,392,393,394,395,396,397,509,398,399,400,401,402,510,511,512,513,403,404,405,406,407,408,409,410,411,412,413,414,415,416,417,418,419,420,421,422,423,424,425,426,427,428,429,430,431,432,433,434,435,436,437,438,439,440,441,442,443,444,445,446,447,448,449,450,451,452,453,454,455,587,592,593,594,595,596,597,598,599,600,601,602,603,604,605,456,457,475,458,459,460,461,462,463,464,465,466,467,468,477)]

  # return full dataset
  return(full_data)
}
