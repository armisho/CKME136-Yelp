raw_df <- read.csv("/Users/Arahman/Documents/CKME-136/output.csv", header = TRUE)
raw_df$high_rated <- ifelse(raw_df$stars >= 3.5, 1, 0)
raw_df$open_7_days <- ifelse(!is.na(raw_df$Sunday_Open) & !is.na(raw_df$Monday_Open) & !is.na(raw_df$Tuesday_Open) & !is.na(raw_df$Wednesday_Open) & !is.na(raw_df$Thursday_Open) & !is.na(raw_df$Friday_Open) & !is.na(raw_df$Saturday_Open), 1, 0)
refined_df <- raw_df
refined_df$has_somekind_of_parking <- ifelse(refined_df$Parking_Garage == 1 | refined_df$Parking_Lot == 1 | refined_df$Parking_Street == 1 | refined_df$Parking_Valet == 1 | refined_df$Parking_Validated == 1, 1, 0)
refined_df$has_somekind_of_music <- ifelse(refined_df$Music_Background_Music == 1 | refined_df$Music_DJ == 1 | refined_df$Music_Jukebox == 1 | refined_df$Music_Karaoke == 1 | refined_df$Music_Live == 1 | refined_df$Music_Video==1, 1,0)
refined_df$no_of_cuisine <-(refined_df$Sandwiches+refined_df$Fast.Food+refined_df$Nightlife+refined_df$Pizza+refined_df$Bars+refined_df$Mexican+refined_df$Food+refined_df$American..Traditional.+refined_df$Burgers+refined_df$Chinese+refined_df$Italian+refined_df$American..New.+refined_df$Breakfast...Brunch+refined_df$Thai+refined_df$Indian+refined_df$Sushi.Bars+refined_df$Korean+refined_df$Mediterranean+refined_df$Japanese+refined_df$Seafood+refined_df$Middle.Eastern+refined_df$Pakistani+refined_df$Barbeque+refined_df$Vietnamese+refined_df$Asian.Fusion+refined_df$Diners+refined_df$Greek+refined_df$Vegetarian)
refined_df <- refined_df[,-c(1:6,8:24,39:40,42:44,46:86)]
refined_df <- refined_df[,-c(3,8,10,11,12,16,19,20)]
str(refined_df)
sapply(refined_df, function(x) sum(is.na(x)))
refined_df <- refined_df[,-16]
