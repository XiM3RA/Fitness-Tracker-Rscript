#!/usr/bin/env Rscript

Ingredients <- read.csv("DATA/Ingredients.csv", stringsAsFactor=FALSE)
Log <- read.csv("DATA/Dailylog.csv")
FoodLog <- data.frame(read.csv("DATA/Foodlog.csv", header=T, stringsAsFactor=FALSE))$id

AddFood <- function(food, quantity) {
# Adds a known ingredient to running log

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
# Simple way to add calories to log

	Log[4] <<- Log[4] + cals
}

AddIngredient <- function(name, serving, protein, carbs, fats, cals) {
# Add an ingredient to the Ingredients list. Will be appended to
# Ingredients.csv if the selection is not already in the list.
# Pass in "name", serving size, protein grams, carb grams, fat grams
# and calories.

    if (name %in% Ingredients$Food) {
        print("Ingredient already exists")
    }
    else {
        temp <- list(name, protein / serving, carbs / serving,
                        fats / serving, cals / serving)
        Ingredients <<- rbind(Ingredients, temp)
        write.csv(Ingredients, "DATA/Ingredients.csv", row.names=FALSE)
    }
}

AddRecipe <- function(Food, foods) {
    # foods needs to be a list: list('food1',100,'food2',200,etc...)

    mass <- 0
    temp <- rep(0,4)
    for (i in seq(1, length(foods), 2)) {
             if (foods[[i]] %in% Ingredients$Food) {
                 mass <- mass + foods[[i+1]]
                 entry <- subset(Ingredients, Food==foods[[i]])[2:5]
                 temp <- temp + entry * foods[[i+1]]
                 }
        }
    temp <- temp / mass
    newEntry <- data.frame(Food, temp)
    Ingredients <<- rbind(Ingredients, newEntry)
    write.csv(Ingredients, "DATA/Ingredients.csv", row.names=FALSE)
}

Archive <- function() {
    foods <- paste(FoodLog,collapse=", ")
    entry <- data.frame(Sys.Date(),Log[1,],foods)
    Arxiv <- read.csv("DATA/Macros.csv", header=TRUE)
    colnames(entry)[1] <- "Date"
    colnames(entry)[6] <- "Foods"
    print(entry)
    Arxiv <- rbind(Arxiv, entry)
    write.csv(Arxiv, "DATA/Macros.csv", row.names=FALSE)
}
# Clears log, does not save log
ClearLog <- function() {
	Log[1,] <<- c(0,0,0,0)
    FoodLog <<- c("")
}

# Saves log for editing at a later time
SaveLog <- function() {
	write.csv(Log[1,], "DATA/DailyLog.csv", row.names=FALSE)
    a <- data.frame(FoodLog)
    colnames(a) <- c("id")
    write.csv(a, "DATA/Foodlog.csv", row.names=FALSE)
}
