#!/usr/bin/env Rscript

Ingredients <- read.csv("DATA/Ingredients.csv")
Log <- read.csv("DATA/Dailylog.csv")

AddFood <- function(food, quantity) {
	if (food %in% Ingredients$Food) {
		entry <- subset(Ingredients, Food==food)[2:5]
		Log[1,] <<- Log[1,] + (entry * quantity)
	}
	else
		print ("Invalid food selection, try again")
}	

AddCals <- function(cals) {
	Log[4] <<- Log[4] + cals
}

ClearLog <- function() {
	Log[1,] <<- c(0,0,0,0)
}

SaveLog <- function() {
	write.csv(Log[1,], "DATA/testing.csv", row.names=FALSE)
}
