library(dplyr); library(lubridate); library(stringr); library(sf); library(tidyr); library(rgdal)
library(raster); library(rgdal)

# Chick known fate --> most up to date file 11/27/2024
birds = read.csv("E:/ForReport_Nov2024/Data/BobwhiteTelemetryData_11_20_2024/Orton_Bobwhite_Telemetry_Data_Entry_0.csv")

# my working directory 
setwd("E:/NOBO R Projects_Github/NOBO_Chick_KF/NOBO_Chick_KF")

# Preview Data ----
head(birds)
length(unique(birds$Bird.ID)) #1586
unique(birds$Bird.Status)
unique(birds$Location.Type) #includes regular, intensive, nest, brood, and chick 
nrow(birds) # 75053 -- before any alterations
length(unique(birds$Bird.ID)) # n = 1586 before any alterations 



#### Create dataframe for chicks ----
# filter by year, then remove based on the first three digits in the band string

## modifying the dates to something easier to handle
nobodates <- data.frame("Date" = birds$Date)
nobodates$Date <- str_replace(nobodates$Date, " AM", "") #replace AM with nothing
nobodates$Date <- str_replace(nobodates$Date, " PM", "") #replace PM with nothing
nobodates <- separate(nobodates, Date, into = c("date", "time"), sep = " ") # split up day and time
nobodates <- separate(nobodates, date, into = c("month", "day", "year"), sep = "/") # split up month, day, year
nobodates <- separate(nobodates, time, into = c("hour", "min"), sep = ":") # split up hour, minute, and second
birds2 <- cbind(birds, nobodates)

# make numeric
birds2$month <- as.numeric(birds2$month)
birds2$day <- as.numeric(birds2$day)
birds2$year <- as.numeric(birds2$year)
birds2$hour <- as.numeric(birds2$hour)
birds2$min <- as.numeric(birds2$min)
birds2$CombinedDate <- paste0(birds2$day, "_", birds2$month, "_", birds2$year)
birds2$CombinedDate0 <- paste0(birds2$day, "_", birds2$month)

# ORDINAL DATES ---- 
lookuptable1 <- read.csv("./ordinaldate.csv")
merge1 <- merge(x = birds2, by.x = "CombinedDate0", y = lookuptable1, by.y = "day_month", all.x = TRUE)
birds2 = merge1

birds2 = subset(birds2, year == "2022" | year == "2023" | year == "2024") # removes an error made in the data -- some were put back before the project even started 


# break it up by year 
df = birds2[birds2$year == "2022", ]
unique(df$year) # 2022
df2 = birds2[birds2$year == "2023", ]
unique(df2$year) # 2023
df3 = birds2[birds2$year == "2024", ]
unique(df3$year) # 2024

# filtering for chicks in YEAR 2022  by the 3 digit string within the band '_225'
df <- df[grepl("_225", df$Bird.ID), ] # using grepl, filter for chicks in 2022
length(unique(df$Bird.ID)) # 84???? (n = 79 --> this is the correct number ) 

# FIXING ERROR I FOUND: Change all "165.5262_225107" to  "165.262_225107" 
df$Bird.ID[df$Bird.ID == "165.5262_225107"] <-"165.262_225107"
length(unique(df$Bird.ID)) # 85 (it is supposed to be 79)

# filtering for chicks in YEAR 2023  by the 3 digit string within the band '_235'
df2 = df2[grepl("_235", df2$Bird.ID), ] # using grepl, filter for chicks in 2023
length(unique(df2$Bird.ID)) # 89 -- this currently is not updated with the last broods of 2023. (n=81; the actual #) 

# filtering for chicks in YEAR 2024  by the 3 digit string within the band '_245'
df3 = df3[grepl("_245", df3$Bird.ID), ] # using grepl, filter for chicks in 2023
length(unique(df3$Bird.ID)) # 107 -- this currently is not updated with the last broods of 2023. (n=81; the actual #) 


# View(df)
chicks = rbind(df, df2, df3) # pull the 3 years of chicks back together 
length(unique(chicks$Bird.ID)) #276 
unique(chicks$Location.Type) # The correct 2: brood and chick --> anything else will be incorrect 

chicks <- subset(chicks, Location.Type == "Chick" | Location.Type =="Brood") #subset to the location types chick and brood (to elliminate chicks recaught as adults during trapping)
unique(chicks$Location.Type)
length(unique(chicks$Bird.ID)) # n = 265 --> 2022 = 78; 2023 = 80; 2024 = 107


# Now that we have a dataset with just chicks lets start the encounter history file
####################### Create Encounter history ----
unique(chicks$Bird.Status) 

# alive 
alives <- subset(chicks, Bird.Status == "Alive & Active" | Bird.Status == "Alive & Inactive" |
                   Bird.Status == "Brood" | Bird.Status == "Alive - Mort Check Only") # 13201 rows
unique(alives$Bird.Status) #double checking 

# fate (also includes some censors)
fates <- subset(chicks, Bird.Status == "Fate")
fatedead <- subset(fates, Fate != "Censor")
fatecensor <- subset(fates, Fate == "Censor")
nrow(fatedead) + nrow(fatecensor) == nrow(fates)

unique(fates$Bird.Status) #double checking 

# censors
censors <- subset(chicks, Bird.Status == "Suspected Fate - RF" | Bird.Status == "Suspected Fate - DNH" | Bird.Status == "Suspected Fate - RIP")
nrow(censors) # 171??? 
unique(censors$Bird.Status) #double checking 

nrow(alives) + nrow(fatedead) + nrow(fatecensor) + nrow(censors) == nrow(chicks) # they do not equal because  there is a nest entry for whatever reason

# Add Encounter  
alives$encounter = 1
fatedead$encounter = 2
fatecensor$encounter = 0
censors$encounter = 0
chick1 <- rbind(alives, fatedead, fatecensor, censors)
chick1 <- chick1 %>% arrange(ObjectID) # re-order columns
length(unique(chick1$Bird.ID)) # 268
# TAGGED CHICKS YR 1 = 79, YR 2 = 81 


######################## Habitat covariates extraction ---- 

## Extract covariate values at each point
chick1_sp <- SpatialPoints(coords = data.frame("x" = chick1$x, "y" = chick1$y)) # convert DF to Spatial Points
crs(chick1_sp) <- CRS("+init=epsg:4326") # define CRS for the spatial points (EPSG 4326 == lat/lon WGS84 etc)

# read in rasters
list.files("E:/NOBO Project Data/Analyses/Breeding Season/Summer 2023/Adult/rasters")
NDVI <- raster("E:/NOBO Project Data/Analyses/Breeding Season/Summer 2023/Adult/rasters/2019NDVI.tif")
perc_mpine <- raster("E:/NOBO Project Data/Analyses/Breeding Season/Summer 2023/Adult/rasters/PercentMaturePine.tif")
perc_grassy <- raster("E:/NOBO Project Data/Analyses/Breeding Season/Summer 2023/Adult/rasters/PercentGrassy.tif")
perc_decid <- raster("E:/NOBO Project Data/Analyses/Breeding Season/Summer 2023/Adult/rasters/PercentDeciduous.tif")
perc_bf <- raster("E:/NOBO Project Data/Analyses/Breeding Season/Summer 2023/Adult/rasters/PercentBroodField.tif")
perc_water <- raster("E:/NOBO Project Data/Analyses/Breeding Season/Summer 2023/Adult/rasters/PercentWater.tif")
DTN_road <- raster("E:/NOBO Project Data/Analyses/Breeding Season/Summer 2023/Adult/rasters/DTN_road.tif")
DTN_mpine <- raster("E:/NOBO Project Data/Analyses/Breeding Season/Summer 2023/Adult/rasters/DTN_maturepine.tif")
DTN_grassy <- raster("E:/NOBO Project Data/Analyses/Breeding Season/Summer 2023/Adult/rasters/DTN_grassy.tif")
DTN_decid <- raster("E:/NOBO Project Data/Analyses/Breeding Season/Summer 2023/Adult/rasters/DTN_deciduous.tif")
DTN_bf <- raster("E:/NOBO Project Data/Analyses/Breeding Season/Summer 2023/Adult/rasters/DTN_broodfield1.tif")
DTN_water <- raster("E:/NOBO Project Data/Analyses/Breeding Season/Summer 2023/Adult/rasters/DTN_water.tif")
burn2022 <- raster("E:/NOBO Project Data/Analyses/Breeding Season/Summer 2023/Adult/rasters/BurnStat2022.tif")
burn2023 <- raster("E:/NOBO Project Data/Analyses/Breeding Season/Summer 2023/Adult/rasters/BurnStat2023.tif")
#burn2024 <- raster("E:/NOBO Project Data/Analyses/Breeding Season/Summer 2023/Adult/rasters/BurnStat2024.tif")

# reproject nobo and extract
chick1_sp <- spTransform(chick1_sp, crs(NDVI)) # transform to NAD83
plot(NDVI); plot(chick1_sp, add = TRUE)
ndv1_ex <- raster::extract(x = NDVI, y = chick1_sp)
perc_mpine_ex <- raster::extract(x = perc_mpine, y = chick1_sp)
perc_grassy_ex <- raster::extract(x = perc_grassy, y = chick1_sp)
perc_decid_ex <- raster::extract(x = perc_decid, y = chick1_sp)
perc_bf_ex <- raster::extract(x = perc_bf, y = chick1_sp)
perc_water_ex <- raster::extract(x = perc_water, y = chick1_sp)

chick1_sp2 <- spTransform(chick1_sp, crs(DTN_road)) # transform to pseudo-mercator
plot(DTN_road); plot(chick1_sp2, add = TRUE)
DTN_road_ex <- raster::extract(x = DTN_road, y = chick1_sp2)
DTN_mpine_ex <- raster::extract(x = DTN_mpine, y = chick1_sp2)
DTN_grassy_ex <- raster::extract(x = DTN_grassy, y = chick1_sp2)
DTN_decid_ex <- raster::extract(x = DTN_decid, y = chick1_sp2)
DTN_bf_ex <- raster::extract(x = DTN_bf, y = chick1_sp2)
DTN_water_ex <- raster::extract(x = DTN_water, y = chick1_sp2)

# add columns to NOBO and export
chick1$ndvi <- ndv1_ex
chick1$perc_mpine <- perc_mpine_ex
chick1$perc_grassy <- perc_grassy_ex
chick1$perc_decid <- perc_decid_ex
chick1$perc_bf <- perc_bf_ex
chick1$perc_water <- perc_water_ex
chick1$DTN_road <- DTN_road_ex
chick1$DTN_mpine <- DTN_mpine_ex
chick1$DTN_grassy <- DTN_grassy_ex
chick1$DTN_decid <- DTN_decid_ex
chick1$DTN_bf <- DTN_bf_ex
chick1$DTN_water <- DTN_water_ex 

#  replace NAs
chick1$ndvi[is.na(chick1$ndvi)] <- 0
chick1$perc_mpine[is.na(chick1$perc_mpine)] <- 0
chick1$perc_grassy[is.na(chick1$perc_grassy)] <- 0
chick1$perc_decid[is.na(chick1$perc_decid)] <- 0
chick1$perc_bf[is.na(chick1$perc_bf)] <- 0
chick1$perc_water[is.na(chick1$perc_water)] <- 0
#View(nobo1)

length(unique(chick1$Bird.ID)) # 151 


# write.csv(chick1,"E:/NOBO Project Data/Analyses/Breeding Season/Summer 2023/Chick/Allchick_20Nov2023.csv", row.names=FALSE)
# MAX # of days a chick was alive is 140 (164.696_225064)

# need to read in the courses for extraction
courses <- readOGR("E:/NOBO Project Data/Analyses/shapefiles/OrtonCourses.shp")
plot(courses)

# individual morphological  data ---- 
# Assign the points indvidiual chick data collected during trap/broodcaps
trapdata = read.csv("./Trap_data_Corrected_24Nov2024.csv", header = T)
# View(trapdata)
chick_tp = subset(trapdata, Age == "Chick") # subset for chicks only (file contains adult trap data as well)
head(chick_tp)

# To note: if the file that is being used is directly from theron without editing in excel it will produce 2 issues
# 1) the bird frequency will have any "0"s removed if they are at the end. example: 164.000 will be 164.
# 2) there is no column that has birdid (freq_band) 
# To fix 1) To fix in excel add a ' to the beginning of the frequency so it will look like this: '163.450
# To fix 2) concatenate in excel manually [easiest option] or the line of code below 
#           chick_tp$birdid = paste(chick_tp$Frequency_at_Release,chick_tp$Band.ID, sep="_") 
            # need to create a column that has bird.id (freq_band)
####################
# Merge Brood cap data and chick dataframe 
# TO NOTE: before the merge, check the trap csv file. 
#          I manually had to fix any bird that had a frequency ending in '0' as
#          Excel tends to remove the '0', making it so R will not merge properly --> Explanation on lines above
#          
chick1 <- merge(x = chick1, by.x = "Bird.ID", y = chick_tp, by.y = "bird.id", all.x = TRUE) # merge by birdid
length(unique(chick1$Bird.ID)) # still 265

chick2 =chick1[-c(2:4,6:7, 10:12,14:29,32:33, 35:38, 40:41,58:64,66:80)] # remove all unwanted columns 
head(chick2) # Check to make sure there are no birds that have NA in the trap section 

############################################################################
###########################################################################

# data exploration 
# average tracking frequency 
head(chick2) 
library(data.table)
chick_table = data.table(chick2)
counts <- chick_table[, .(rowCount = .N), by = Bird.ID]
counts

mean(counts$rowCount)
hist(counts$rowCount) 

large = subset(chick2, RadioSize == "Large")
small = subset(chick2, RadioSize == "Small")
length(unique(large$Bird.ID))


large_table = data.table(large)
small_table = data.table(small)

large_counts <- large_table[, .(rowCount = .N), by = Bird.ID]
small_counts <- small_table[, .(rowCount = .N), by = Bird.ID]

max(large_counts$rowCount)
min(large_counts$rowCount)

max(small_counts$rowCount) # 
min(small_counts$rowCount) # 

head(chick2)
class(chick2$Date.x)
chick2$Date.x = as.Date(chick2$Date.x, "%m/%d/%Y")
chick2$Capture.Date = as.Date(chick2$Capture.Date, "%m/%d/%Y")

chick2$Dayposthatch = chick2$Date.x - chick2$Capture.Date # DAYS POST CAPTURE DATE BUT I WILL NOT BE CHANGING THIS
max(chick2$Dayposthatch)

chick2$Dayposthatch.num = as.numeric(chick2$Dayposthatch)
head(chick2)



smalls = subset(chick2, RadioSize == "Small")
bigs = subset(chick2, RadioSize == "Large")

length(unique(smalls$Bird.ID)) # 65 
length(unique(bigs$Bird.ID)) # 93 

s_2wks = subset(smalls, Dayposthatch.num > 17) # To view the # of individuals that did not make it to 
b_2wks = subset(bigs, Dayposthatch.num >17)

length(unique(s_2wks$Bird.ID))
length(unique(b_2wks$Bird.ID))


# subset to birds that have lasted 2 weeks post-capture 
# capture is at day 11-13 
# 2 weeks after day 11-13 is 25 - 27 

# write.csv(chick2, "E:/NOBO R Projects_Github/NOBO_Chick_KF/NOBO_Chick_KF/checkdata.csv")

#Add ordinal date of capture into the file 
x = data.frame(unique(chick2$Capture.Date))
x
nrow(x)
lookuptable2 = lookuptable1 # so I dont mess up the actual file 
lookuptable2 = dplyr::select(lookuptable1, "Date" | "Ordinal") # isolate just the date and ordinal date 
lookuptable2$OrdinalCapDay = lookuptable2$Ordinal
lookuptable2 = dplyr::select(lookuptable2, "Date" | "OrdinalCapDay") # isolate just the date and ordinal date 

lookuptable2$Date = lubridate::mdy(lookuptable2$Date) # gotta use lubridate() package

# now to combine the lookuptable2 file (which holds the ordinal dates ) and the chick file  in order to determine the "ordinal capture date"
y <- merge(x = chick2, by.x = "Capture.Date", y = lookuptable2, by.y = "Date", all.x = TRUE) # merge by "Capture.Date" and "Date" 
chick2 = y # change it back to the O.G. DF 


##### FIXED 3/19/2023 
##### ISSUES THAT NEED TO BE ADDRESSED ----

# 1) The trap data file (which includes brood cap data) have a couple incorrect entries 
#   This was determined by looking at the ordinal dates 
#   By subtracting the ordinal brood cap day by the last date for every individual chick it would produce the max # of days a chick was alive 
#   For some, it was as low as -1 meaning that the brood cap day was one off and they were tracked before the actual brood cap day 
#   This can interfere with the encounter history 
#   For some, it showed it went as high as 187 -- the data shows that some have double fates and others dont have fates until december 21st 
#   Both of these need to be fixed manually within the trap data and telemetry data file 
###### FIXED 3/19/2023 

# Making radio weight binary 
# If it is a large radio it gets a 0 (since it is the old way it gets a 0 )
# if it is a small radio it gets a 1 (new way)   
unique(chick2$RadioSize)
c.1 = subset(chick2, RadioSize == "Small") # will be recieving a 1
c.0 = subset(chick2, RadioSize == "Large")

c.1$RadioSize = "1" # small radios 
c.0$RadioSize = "0" # large radios 
chick3 = rbind(c.1, c.0) # put the bitch back toegtehr 
nrow(chick2) #6767 
unique(chick3$RadioSize)
# View(chick3)

# how many died 
unique(chick3$Fate)
confirmeddead = subset(chick3, Fate == "Unknown" | Fate == "Unknown Avian" | Fate == "Snake" | Fate == "Mammal" | Fate == "Large Avian" | Fate == "Fire Ants" | Fate == "Small Avian")
length(confirmeddead$Bird.ID) # 167 have been confirmed dead 


#### For() loop that creates input file
birdlist <- unique(chick3$Bird.ID)

# create an empty dataframe containing the days == the max days a chick was alive is 140.... this is going to suck actual balls  
inp_df <- data.frame("birdID" = NA, 
                     "d1" = NA, "d2" = NA, "d3" = NA, "d4" = NA, "d5" = NA, "d6" = NA,
                     "d7" = NA, "d8" = NA, "d9" = NA, "d10" = NA, "d11" = NA, "d12" = NA,
                     "d13" = NA, "d14" = NA, "d15" = NA, "d16" = NA, "d17" = NA, "d18" = NA,
                     "d19" = NA, "d20" = NA, "d21" = NA, "d22" = NA, "d23" = NA, "d24" = NA,
                     "d25" = NA, "d26" = NA, "d27" = NA, "d28" = NA, "d29" = NA, "d30" = NA,
                     "d31" = NA, "d32" = NA, "d33" = NA, "d34" = NA, "d35" = NA, "d36" = NA,
                     "d37" = NA, "d38" = NA, "d39" = NA, "d40" = NA, "d41" = NA, "d42" = NA,
                     "d43" = NA, "d44" = NA, "d45" = NA, "d46" = NA, "d47" = NA, "d48" = NA,
                     "d49" = NA, "d50" = NA, "d51" = NA, "d52" = NA, "d53" = NA, "d54" = NA,
                     "d55" = NA, "d56" = NA, "d57" = NA, "d58" = NA, "d59" = NA, "d60" = NA,
                     "d61" = NA, "d62" = NA,
                     "ndvi" = NA, "perc_mpine" = NA, "perc_grassy" = NA, "perc_decid" = NA, 
                     "perc_bf" = NA, "perc_water" = NA, "DTN_road" = NA, "DTN_mpine" = NA, 
                     "DTN_grassy" = NA, "DTN_decid" = NA, "DTN_bf" = NA, "DTN_water" = NA,
                     "course" = NA, "radio" = NA, "OrdCapDay" = NA, "TrackedAlive" = NA, "year22" = NA, "year23" = NA, "weight" = NA) # leaving off here  (need to make 2 columns for year)


#### For() loop that creates input file
#nobo_breding_data <- subset(nobo1, breedingseason == 1)

length(birdlist) #265


# reorder by date - 
#       ****  DO NOT DELETE ANYTHING FROM THE NEXT FEW LINES ****
#       date column is giving me a hard time and keeps reverting to character or unknown 
chick3$Date.x = as.character(chick3$Date.x) # gotta use lubridate() package
chick3$Date.x= lubridate::ymd(chick3$Date.x) # gotta use lubridate() package
class(chick3$Date.x) # it says it is a date column 
chick3$Date.x = as.Date(chick3$Date.x) # i understand this is excessive and no i have no idea why it wont work unless i do it in this order exactly like this 
chick3 = chick3[order(as.Date(chick3$Date.x, format="%Y-%m-%d")),]






for(i in 1:length(birdlist)){
  #i = 65
  bird_i_data <- subset(chick3, Bird.ID == birdlist[i])
  vals <- c()
  
  # for() loop extracts max encounter values for each day
  for(j in 1:62){
    #j = 2
    day_j_data <- subset(bird_i_data, Dayposthatch.num == j)
    val_j <- ifelse(nrow(day_j_data) == 0, NA, max(day_j_data$encounter)) # if no data, give NA, if data, take max of encounter
    vals <- c(vals, val_j)
  }
  vals <- c(1, vals)# add "1" for "day 0"
  
  # add course
  meanX <- mean(bird_i_data$x) # average the x coords
  meanY <- mean(bird_i_data$y) # average the y coords
  mean_loc_i <- SpatialPoints(coords = data.frame("x" = meanX, "y" = meanY)) # convert DF to Spatial Points
  crs(mean_loc_i) <- CRS("+init=epsg:4326") # define CRS for the spatial points (EPSG 4326 == lat/lon WGS84 etc)
  mean_loc_i <- spTransform(mean_loc_i, crs(courses))
  # plot(courses); plot(mean_loc_i, add = TRUE)
  course_i <- over(x = mean_loc_i, y = courses)
  course_i <- course_i$course 
  
  # combine all pieces into a new row
  newrow <- data.frame("birdID" = birdlist[i],
                       "d1" = vals[1], "d2" = vals[2], "d3" = vals[3], "d4" = vals[4],
                       "d5" = vals[5], "d6" = vals[6], "d7" = vals[7], "d8" = vals[8],
                       "d9" = vals[9], "d10" = vals[10], "d11" = vals[11], "d12" = vals[12],
                       "d13" = vals[13], "d14" = vals[14], "d15" = vals[15], "d16" = vals[16],
                       "d17" = vals[17], "d18" = vals[18], "d19" = vals[19], "d20" = vals[20],
                       "d21" = vals[21], "d22" = vals[22], "d23" = vals[23], "d24" = vals[24],
                       "d25" = vals[25], "d26" = vals[26], "d27" = vals[27], "d28" = vals[28],
                       "d29" = vals[29], "d30" = vals[30], "d31" = vals[31], "d32" = vals[32],
                       "d33" = vals[33], "d34" = vals[34], "d35" = vals[35], "d36" = vals[36],
                       "d37" = vals[37], "d38" = vals[38], "d39" = vals[39], "d40" = vals[40],
                       "d41" = vals[41], "d42" = vals[42], "d43" = vals[43], "d44" = vals[44],
                       "d45" = vals[45], "d46" = vals[46], "d47" = vals[47], "d48" = vals[48],
                       "d49" = vals[49], "d50" = vals[50], "d51" = vals[51], "d52" = vals[52],
                       "d53" = vals[53], "d54" = vals[54], "d55" = vals[55], "d56" = vals[56],
                       "d57" = vals[57], "d58" = vals[58], "d59" = vals[59], "d60" = vals[60],
                       "d61" = vals[61], "d62" = vals[62],
                       
                       "ndvi" = mean(bird_i_data$ndvi), "perc_mpine" = mean(bird_i_data$perc_mpine), 
                       "perc_grassy" = mean(bird_i_data$perc_grassy), "perc_decid" = mean(bird_i_data$perc_decid), 
                       "perc_bf" = mean(bird_i_data$perc_bf), "perc_water" = mean(bird_i_data$perc_water), 
                       "DTN_road" = mean(bird_i_data$DTN_road), "DTN_mpine" = mean(bird_i_data$DTN_mpine), 
                       "DTN_grassy" = mean(bird_i_data$DTN_grassy), "DTN_decid" = mean(bird_i_data$DTN_decid), 
                       "DTN_bf" = mean(bird_i_data$DTN_bf), "DTN_water" = mean(bird_i_data$DTN_water),
                       "course" = course_i, "radio" = first(bird_i_data$RadioSize), "OrdCapDay" = mean(bird_i_data$OrdinalCapDay), 
                       "TrackedAlive" = sum(lengths(regmatches(vals, gregexpr("1", vals)))), "year22" = first(bird_i_data$year), "year23" = first(bird_i_data$year), "weight" = first(bird_i_data$Weight))
  
  # add new row to "inp_df"
  inp_df <- rbind(inp_df, newrow)
}
# View(inp_df) # seems right; I do not see any glaring patterns or errors 
# IMPORTANT TO NOTE WHEN LOOKING AT THE INP FILE - some birds were put into the system a few days later, however all chicks recieved a 1 for day 0 (saw them alive at time of capture)
#########################

inp_df <- inp_df[2:(nrow(inp_df)), ] # remove the blank row at the top and bottom 9 rows
inp_df <- subset(inp_df, TrackedAlive > 0) # REMOVES ANY CHICKS TRACKED FOR 0 DAYS 
#View(inp_df)

# make year columns binary - year 2022 = 1 0 ; year 2023 = 0 1; year 2024 = 0 0    
inp_df$year22 = ifelse(
  inp_df$year22 == "2022",
  1, 0)
inp_df$year23 = ifelse(
  inp_df$year23 == "2023",
  1, 0)

# turn NA into "unknown"
inp_df$course[is.na(inp_df$course)]  <- "unknown"

# adding quantity - dummy variable: 2 bushel [10], 3 bushel [00], 4 bushel [01]
inp_df$quantity2 <- ifelse(
  inp_df$course == "bigbay" | inp_df$course == "campcrane1" | inp_df$course == "campcrane2" | 
    inp_df$course == "unknown" | inp_df$course == "allenscreek" | inp_df$course == "bluepond" | 
    inp_df$course == "firetower1" | inp_df$course == "unknown6",
  1, 0)
inp_df$quantity4 = ifelse(
  inp_df$course == "bigbay" | inp_df$course == "campcrane1" | inp_df$course == "campcrane2" | 
    inp_df$course == "unknown" | inp_df$course == "allenscreek" | inp_df$course == "bluepond" | 
    inp_df$course == "firetower1" | inp_df$course == "unknown6",
  0, 1)

inp_df$quantity2 <- ifelse(inp_df$year22 == 0 & inp_df$year23 == 0, 0, inp_df$quantity2)
inp_df$quantity4 <- ifelse(inp_df$year22 == 0 & inp_df$year23 == 0, 0, inp_df$quantity4)

# adding method - stays the same 2022 - 2024
inp_df$blower <- ifelse(
  inp_df$course == "bigbay" | inp_df$course == "billjones" | inp_df$course == "bluepond",
  1, 0)
inp_df2 = inp_df
######################### adding individual covariates ----



#write.csv(inp_df2, "C:/Users/User/Desktop/ForAutumnToFix.csv")

######################### creating encounter history

neweo <- inp_df2$birdID# new encounter occasions

for(i in 0:62){ # there are 63 occasions; "day 0" plus 62 monitoring days
  #i = 0
  dat <- as.data.frame(inp_df2[,i+2]) # isolate individual occasion; 0 = column 2
  dat <- ifelse(is.na(dat), "00",                       # if NA, make 00
                ifelse(dat == "0", "00",                # if 0, make 00
                       ifelse(dat == "1", "10", "11"))) # if 1, make 10, otherwise 11
  name_i <- paste0("day", i)
  newcol <- data.frame(name_i = dat)
  names(newcol) <- name_i
  neweo <- cbind(neweo, newcol)
}

# make encounter history
eh <- paste0(neweo[,2],neweo[,3],neweo[,4],neweo[,5],neweo[,6],neweo[,7],neweo[,8],neweo[,9],neweo[,10],
             neweo[,11],neweo[,12],neweo[,13],neweo[,14],neweo[,15],neweo[,16],neweo[,17],neweo[,18],neweo[,19],
             neweo[,20],neweo[,21],neweo[,22],neweo[,23],neweo[,24],neweo[,25],neweo[,26],neweo[,27],neweo[,28],
             neweo[,29],neweo[,30],neweo[,31],neweo[,32],neweo[,33],neweo[,34],neweo[,35],neweo[,36],neweo[,37],
             neweo[,38],neweo[,39],neweo[,40],neweo[,41],neweo[,42],neweo[,43],neweo[,44],neweo[,45],neweo[,46],
             neweo[,47],neweo[,48],neweo[,49],neweo[,50],neweo[,51],neweo[,52],neweo[,53],neweo[,54],neweo[,55],
             neweo[,56],neweo[,57],neweo[,58],neweo[,59],neweo[,60],neweo[,61],neweo[,62],neweo[,63]
)

eh <- data.frame("eh" = eh)
#View(eh)
ncol(neweo)

# add encounter history with covariates
pre_inp <- data.frame("birdid" = inp_df2$birdID, "eh" = eh$eh, inp_df2[,64:85]) #### NEED TO FIX HERE TO THE END!!!!

#fix any NA values 
missing_values <- colSums(is.na(pre_inp))
mean(pre_inp$weight, na.rm = T) # mean value 
pre_inp$weight[is.na(pre_inp$weight)] <- 18.04 # make the missing valuesthe mean of that column


new_inp <- data.frame("forMARK" = paste0("/*", pre_inp$birdid, "*/", # bird ID
                                         pre_inp$eh, " ", # encounter history
                                         1, " ", # group
                                         round(pre_inp[,3],2), " ", # ndvi
                                         round(pre_inp[,4],2), " ", # perc_mpine
                                         round(pre_inp[,5],2), " ", # perc_grassy
                                         round(pre_inp[,6],2), " ", # perc_decid
                                         round(pre_inp[,7],2), " ", # perc_bf
                                         round(pre_inp[,8],2), " ", # perc_water
                                         round(pre_inp[,9],0), " ", # dtn feedline 
                                         round(pre_inp[,10],0), " ", # dtn mpine
                                         round(pre_inp[,11],0), " ", # dtn_grassy 
                                         round(pre_inp[,12],0), " ", # dtn_decid 
                                         round(pre_inp[,13],0), " ", #dtn bf 
                                         round(pre_inp[,14],0), " ", # dtn water 
                                         pre_inp[,16], " ", # radio 
                                         pre_inp[,17], " ", # ordinal capture date
                                         pre_inp[,21], " ", # Weight of chick 
                                         pre_inp[,19], " ", # year 2022 (1 0 )
                                         pre_inp[,20], " ", # year 2023 ( 0 1) [2024 = 0 0]
                                         pre_inp[,22], " ", # Feed Quantity 2 bushels = 1 
                                         pre_inp[,23], " ", # Feed Quantity 4 bushels = 1 [3 bushels is 00] 
                                         pre_inp[,24], ";"  # Distribution Method: Blower = 1 
                                         
))
View(new_inp)

# export
# write.csv(new_inp, "./kf_chicks_ForReport_01Dec2024.inp", row.names = FALSE)







