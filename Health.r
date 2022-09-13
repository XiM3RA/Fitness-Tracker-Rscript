#!/usr/bin/env Rscript

Ingredients <- read.csv("DATA/Ingredients.csv")
Log <- read.csv("DATA/Dailylog.csv")
FoodLog <- data.frame(read.csv("DATA/Foodlog.csv", header=T, stringsAsFactor=FALSE))$id


AddFood <- function(food, quantity) {
	if (food %in% Ingredients$Food) {
		entry <- subset(Ingredients, Food==food)[2:5]
		Log[1,1:4] <<- Log[1,1:4] + (entry * quantity)
        if (!food %in% FoodLog) {
            FoodLog <<- append(FoodLog, food)
        }
    }
	else
		print ("Invalid food selection, try again")
}	

AddCals <- function(cals) {
	Log[4] <<- Log[4] + cals
}

ClearLog <- function() {
	Log[1,] <<- c(0,0,0,0)
    FoodLog <- c()
}

SaveLog <- function() {
	write.csv(Log[1,], "DATA/DailyLog.csv", row.names=FALSE)
    a <- data.frame(FoodLog)
    colnames(a) <- c("id")
    write.csv(a, "DATA/Foodlog.csv", row.names=FALSE)
}
