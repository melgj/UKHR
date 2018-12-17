# Filter AW Races

ukAW <- filter(ukhr_master_BF, RaceType == "AW")

#remove jumping predictor from AW data

ukAW <- select(ukAW, -c(ChaseJumpingAbility, HunterChase, Beginner))

ukAW$Dist_Range <- as.factor(ifelse(ukAW$Furlongs < 8, "Sprint",
                                    ifelse(ukAW$Furlongs < 14,"Middle", "Long")))

# AW data models

#filter data to exclude races before 11th August 14 (New Tapeta surface at Wolverhampton replaced Polytrack)

f1 <- filter(ukAW, Year >= 2015)
f2 <- filter(ukAW, Year == 2014 & Month >= 8)

ukAW2 <- rbind(f1,f2)

unique(ukAW2$Year)

#split into subsets by surface (tapeta, fibresand, polytrack)
tapeta <- c("Wolverhampton","Newcastle")
polytrack <- c("Dundalk","Kempton","Lingfield","Chelmsford City")
fibresand <- c("Southwell")

tapeta <- str_to_upper(tapeta)
polytrack <- str_to_upper(polytrack)
fibresand <- str_to_upper(fibresand)


tap <- filter(ukAW2, Meeting %in% tapeta)
pol <- filter(ukAW, Meeting %in% polytrack)
fib <- filter(ukAW, Meeting %in% fibresand)#

unique(pol$Meeting)

unique(ukAW2$Meeting)
unique(ukAW$Meeting)

todayAW <- filter(today, RaceType == "AW")


################################################

tapSiresT <- read_csv("TapetaSires.csv", col_names = T)

tapSiresT_Quals <- tapSiresT %>%
  left_join(todayAW, by = c("Sire")) %>%
  filter(!is.na(Time24Hour), Meeting %in% tapeta, RaceType == "AW")

tapSiresT_Quals
#
if(nrow(tapSiresT_Quals) > 0) {
  tapSiresT_Quals$System_Name <- "Tapeta_Sires"
}

polSiresT <- read_csv("PolytrackSires.csv", col_names = T)

polSiresT_Quals <- polSiresT %>%
  left_join(todayAW, by = c("Sire")) %>%
  filter(!is.na(Time24Hour), Meeting %in% polytrack, RaceType == "AW")

polSiresT_Quals
#
if(nrow(polSiresT_Quals) > 0) {
  polSiresT_Quals$System_Name <- "Polytrack_Sires"
}


fibSiresT <- read_csv("FibresandSires.csv", col_names = T)

fibSiresT_Quals <- fibSiresT %>%
  left_join(todayAW, by = c("Sire")) %>%
  filter(!is.na(Time24Hour), Meeting %in% fibresand, RaceType == "AW")

fibSiresT_Quals
#
if(nrow(fibSiresT_Quals) > 0) {
  fibSiresT_Quals$System_Name <- "Fibresand_Sires"
}


wolvesSires <- read_csv("wSires.csv", col_names = T)

wolvesSires_Quals <- wolvesSires %>%
  left_join(todayAW, by = c("Sire")) %>%
  filter(!is.na(Time24Hour), Meeting == "WOLVERHAMPTON", RaceType == "AW")

wolvesSires_Quals
#
if(nrow(wolvesSires_Quals) > 0) {
  wolvesSires_Quals$System_Name <- "Wolves_Sires"
}

newcastleSires <- read_csv("nSires.csv", col_names = T)

newcastleSires_Quals <- newcastleSires %>%
  left_join(todayAW, by = c("Sire")) %>%
  filter(!is.na(Time24Hour), Meeting == "NEWCASTLE", RaceType == "AW")

newcastleSires_Quals

if(nrow(newcastleSires_Quals) > 0) {
  newcastleSires_Quals$System_Name <- "Newcastle_Sires"
}


polyMeetingSires <- read_csv("pSires.csv", col_names = T)

polyMeetingSires_Quals <- polyMeetingSires %>%
  left_join(todayAW, by = c("Meeting", "Sire")) %>%
  filter(!is.na(Time24Hour), Meeting %in% polytrack, RaceType == "AW")

polyMeetingSires_Quals

if(nrow(polyMeetingSires_Quals) > 0) {
  polyMeetingSires_Quals$System_Name <- "Poly_Meeting_Sires"
}

newcastleTrainers <- read_csv("nTrainers.csv", col_names = T)

newcastleTrainers_Quals <- newcastleTrainers %>%
  left_join(todayAW, by = c("Trainer")) %>%
  filter(!is.na(Time24Hour), Meeting == "NEWCASTLE", RaceType == "AW")

newcastleTrainers_Quals

if(nrow(newcastleTrainers_Quals) > 0) {
  newcastleTrainers_Quals$System_Name <- "Newcastle_Trainers"
}


wolvesTrainers <- read_csv("wTrainers.csv", col_names = T)

wolvesTrainers_Quals <- wolvesTrainers %>%
  left_join(todayAW, by = c("Trainer")) %>%
  filter(!is.na(Time24Hour), Meeting == "WOLVERHAMPTON", RaceType == "AW")

wolvesTrainers_Quals

if(nrow(wolvesTrainers_Quals) > 0) {
  wolvesTrainers_Quals$System_Name <- "Wolves_Trainers"
}


southwellTrainers <- read_csv("sTrainers.csv", col_names = T)


southwellTrainers_Quals <- southwellTrainers %>%
  left_join(todayAW, by = c("Trainer")) %>%
  filter(!is.na(Time24Hour), Meeting == "SOUTHWELL", RaceType == "AW")

southwellTrainers_Quals

if(nrow(southwellTrainers_Quals) > 0) {
  southwellTrainers_Quals$System_Name <- "Southwell_Trainers"
}


polyMeetingTrainers <- read_csv("pTrainers.csv", col_names = T)


polyMeetingTrainers_Quals <- polyMeetingTrainers %>%
  left_join(todayAW, by = c("Meeting", "Trainer")) %>%
  filter(!is.na(Time24Hour), Meeting %in% polytrack, RaceType == "AW")

polyMeetingTrainers_Quals


if(nrow(polyMeetingTrainers_Quals) > 0) {
  polyMeetingTrainers_Quals$System_Name <- "Poly_Meeting_Trainers"
}



trJkComboAW <- read_csv("TopTrJkCombos_AW.csv", col_names = T)


todayAWTJQ <- trJkComboAW %>%
  left_join(todayAW, by = c("Trainer", "Jockey")) %>%
  filter(!is.na(Time24Hour), RaceType == "AW")

todayAWTJQ

if(nrow(todayAWTJQ) > 0) {
  todayAWTJQ$System_Name <- "AW_TJ_Combo"
}


dRangeSiresTap <- read_csv("TapetaSires_DistanceRange.csv", col_names = T)

dRangeSiresPol <- read_csv("PolySires_DistanceRange.csv", col_names = T)

dRangeSiresFib <- read_csv("FibSires_DistanceRange.csv", col_names = T)


siresDistRange <- rbind(dRangeSiresTap,dRangeSiresPol,dRangeSiresFib)

siresDistRange <- siresDistRange %>%
  arrange(Meeting, Dist_Range, desc(AE_Ratio))

siresDistRange

todayAW$Dist_Range <- as.factor(ifelse(todayAW$Furlongs < 8, "Sprint", "Route"))

levels(todayAW$Dist_Range)

siresDistRangeQuals <- siresDistRange %>%
  left_join(todayAW, by = c("Sire", "Meeting", "Dist_Range")) %>%
  filter(!is.na(Time24Hour))

siresDistRangeQuals

if(nrow(siresDistRangeQuals) > 0) {
  siresDistRangeQuals$System_Name <- "AW_Sires_Distance_Range"
}



todayAllAW_Qualifiers <- tapSiresT_Quals %>%
  full_join(polSiresT_Quals) %>%
  full_join(fibSiresT_Quals) %>%
  full_join(wolvesSires_Quals) %>%
  full_join(newcastleSires_Quals) %>%
  #full_join(southwellSires_Quals) %>%
  full_join(polyMeetingSires_Quals) %>%
  full_join(wolvesTrainers_Quals) %>%
  full_join(newcastleTrainers_Quals) %>%
  full_join(southwellTrainers_Quals) %>%
  full_join(polyMeetingTrainers_Quals) %>%
  full_join(todayAWTJQ) %>%
  full_join(siresDistRangeQuals)

todayAllAW_Qualifiers
