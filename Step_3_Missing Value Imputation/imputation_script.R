library(rpart)
class_mod <- rpart(Price_Range ~ . - high_rated, data=refined_df[!is.na(refined_df$Price_Range), ], method="class", na.action=na.omit)
price_range_pred <- predict(class_mod, refined_df[is.na(refined_df$Price_Range), ])

class_mod <- rpart(Alcohol ~ . - high_rated, data=refined_df[!is.na(refined_df$Alcohol), ], method="class", na.action=na.omit)
alcohol_pred <- predict(class_mod, refined_df[is.na(refined_df$Alcohol), ])

class_mod <- rpart(Delivery ~ . - high_rated, data=refined_df[!is.na(refined_df$Delivery), ], method="class", na.action=na.omit)
delivery_pred <- predict(class_mod, refined_df[is.na(refined_df$Delivery), ])

class_mod <- rpart(Outdoor_Seating ~ . - high_rated, data=refined_df[!is.na(refined_df$Outdoor_Seating), ], method="class", na.action=na.omit)
outdoor_seating_pred <- predict(class_mod, refined_df[is.na(refined_df$Outdoor_Seating), ])

class_mod <- rpart(Good_for_Groups ~ . - high_rated, data=refined_df[!is.na(refined_df$Good_for_Groups), ], method="class", na.action=na.omit)
good_for_group_pred <- predict(class_mod, refined_df[is.na(refined_df$Good_for_Groups), ])

class_mod <- rpart(Good_for_Kids ~ . - high_rated, data=refined_df[!is.na(refined_df$Good_for_Kids), ], method="class", na.action=na.omit)
good_for_kids_pred <- predict(class_mod, refined_df[is.na(refined_df$Good_for_Kids), ])

class_mod <- rpart(Takes_Reservations ~ . - high_rated, data=refined_df[!is.na(refined_df$Takes_Reservations), ], method="class", na.action=na.omit)
takes_reservations_pred <- predict(class_mod, refined_df[is.na(refined_df$Takes_Reservations), ])

class_mod <- rpart(Take_Out ~ . - high_rated, data=refined_df[!is.na(refined_df$Take_Out), ], method="class", na.action=na.omit)
take_out_pred <- predict(class_mod, refined_df[is.na(refined_df$Take_Out), ])

class_mod <- rpart(has_somekind_of_parking ~ . - high_rated, data=refined_df[!is.na(refined_df$has_somekind_of_parking), ], method="class", na.action=na.omit)
has_somekind_of_parking_pred <- predict(class_mod, refined_df[is.na(refined_df$has_somekind_of_parking), ])

imputeNaFromPrediction <- function(df,col_index, mtx) {
  for(row in (1:nrow(mtx))) {
    rowNum <- rownames(mtx)[row]
    value <- colnames(mtx)[which.max(mtx[row,])]
    df[rowNum,col_index] <- value
  }
  return (df)
}

refined_df <-imputeNaFromPrediction(refined_df,4,price_range_pred)
refined_df <- imputeNaFromPrediction(refined_df,3,alcohol_pred)
refined_df <-imputeNaFromPrediction(refined_df,5,delivery_pred)
refined_df <-imputeNaFromPrediction(refined_df,6,outdoor_seating_pred)
refined_df <-imputeNaFromPrediction(refined_df,7,good_for_group_pred)
refined_df <-imputeNaFromPrediction(refined_df,8,good_for_kids_pred)
refined_df <-imputeNaFromPrediction(refined_df,10,takes_reservations_pred)
refined_df <-imputeNaFromPrediction(refined_df,11,take_out_pred)
refined_df <-imputeNaFromPrediction(refined_df,15,has_somekind_of_parking_pred)