rm(list = ls())
pth <- "/Users/stefanogrillini/Documents/R files/Advanced R programming Coursera"
setwd(pth)

library(readr)
library(magrittr)
library(dplyr)
library(tidyr)

df <- read.csv("MIE.csv", sep = ",", header = T)
head(df, n = 10)
summary(df)
str(df)

# LongitudinalData Class and Methods
setClass("LongitudinalData",
         slots = c(id = "numeric", 
                        visit = "numeric",
                        room = "character",
                        value = "numeric",
                        timepoint = "numeric"))

setGeneric("print")
setGeneric("summary")

# Subject class
setClass("subjectClass",
         slots = c(id = "numeric", 
                        visit = "numeric",
                        room = "character",
                        value = "numeric",
                        timepoint = "numeric"))

# Visit class
setClass("visitClass", 
         slots = c(id = "numeric", 
                   visit = "numeric",
                   room = "character",
                   value = "numeric",
                   timepoint = "numeric"))

# Room class
setClass("roomClass", 
         slots = c(id = "numeric", 
                   visit = "numeric",
                   room = "character",
                   value = "numeric",
                   timepoint = "numeric"))

# Function to convert data frame to "lLongitudinalData"
make_LD <- function(x) {
  new("LongitudinalData", id = x$id, visit = x$visit,
      room = x$room, value = x$value, timepoint = x$timepoint)
}


# Methods for subject, visit and room
setGeneric("subject", function(x,...){
  standardGeneric("subject")
})
setGeneric("visit", function(x,...){
  standardGeneric("visit")
})
setGeneric("room", function(x,...){
  standardGeneric("room")
})

# To create the "print" function specific to the the class "LongitudinalData"
setMethod("print", c(x = "LongitudinalData"),
          function(x){
            paste("Longitudinal dataset with", length(unique(x@id)), "subjects")
          })

# Same thing for subject, but with new() for the newly allocated object
setMethod("subject",
          c(x = "LongitudinalData"),
          function(x,n){
            new("subjectClass", id = x@id[x@id == n], visit = x@visit[x@id == n],
                room = x@room[x@id == n], value = x@value[x@id == n],
                timepoint = x@timepoint[x@id == n])
          })

setMethod("print",
          c(x = "subjectClass"),
          function(x){
            if (length(unique(x@id)) > 0) {
              cat(paste("Subject ID:",unique(x@id)))
            } else {
              NULL
            }
          }
          )

setMethod("summary",
          c(object = "subjectClass"),
          function(object){
            new("subjectSummary", id = object@id, visit = object@visit, 
                room = object@room, value = object@value)
          }
          )

setMethod("visit",
          c(x = "subjectClass"),
          function(x,n){
            new("visitClass", id = x@id[x@visit == n], visit = x@visit[x@visit == n],
                room = x@room[x@visit == n], value = x@value[x@visit == n],
                timepoint = x@timepoint[x@visit == n])
          })

# subjectSummary Class and Methods
setClass("subjectSummary",
         representation(id = "numeric", 
                        visit = "numeric",
                        room = "character",
                        value = "numeric"))
setMethod("print",
          c(x = "subjectSummary"),
          function(x){
            cat(paste("ID:", unique(x@id)),"\n")
            as.data.frame(cbind(visit=x@visit,room=x@room,value=x@value),stringsAsFactors = FALSE) %>%
              mutate(value = as.numeric(value)) %>%
              group_by(visit,room) %>%
              summarise(avg = mean(value)) %>%
              spread(room,avg)
          })
# visit Class Methods

setMethod("room",
          c(x = "visitClass"),
          function(x,n){
            new("roomClass", id = x@id[x@room == n], visit = x@visit[x@room == n],
                room = x@room[x@room == n], value = x@value[x@room == n],
                timepoint = x@timepoint[x@room == n])
          })
# room Class Methods

setMethod("print",
          c(x = "roomClass"),
          function(x){
            cat(paste("ID:", unique(x@id)),"\n")
            cat(paste("Visit:",unique(x@visit)),"\n")
            cat(paste("Room:",unique(x@room)))
          })
setMethod("summary",
          c(object = "roomClass"),
          function(object){
            new("roomSummary", id = object@id, value = object@value)
          })

# room_summary Class and Methods
setClass("roomSummary",
         representation(id = "numeric", 
                        value = "numeric"))
setMethod("print",
          c(x = "roomSummary"),
          function(x){
            cat(paste("ID:", unique(x@id)),"\n")
            summary(x@value)
          })




x <- make_LD(df)
print(class(x))
print(x)

## Subject 10 doesn't exist
out <- subject(x, 10)
print(out)

out <- subject(x, 14)
print(out)

out <- subject(x, 54) %>% summary
print(out)

out <- subject(x, 14) %>% summary
print(out)

out <- subject(x, 44) %>% visit(0) %>% room("bedroom")
print(out)

## Show a summary of the pollutant values
out <- subject(x, 44) %>% visit(0) %>% room("bedroom") %>% summary
print(out)

out <- subject(x, 44) %>% visit(1) %>% room("living room") %>% summary
print(out)