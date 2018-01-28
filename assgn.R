library( randomForest )

##########################################################
###   HELPER FUNCTIONS                                 ###
##########################################################
inspect <- function( daDF ) {
    Mmeta <- list()
    for( i in 1 : dim( daDF )[ 2 ] ){
        uvalues <- unique( daDF[ , i ] )
        ucount <- length( uvalues )
        cname <- names( daDF)[ i ]
        cclass <- class( daDF[ , i ] )
        clevcount <- NA
        if( cclass == "factor" ){
            clevcount <-length( levels( daDF[ , i ] ) )
        }
        csumm <- summary( daDF[ , i ] )
        hasBlanks <- TRUE %in% grepl( "^\\s*$", uvalues )
        hasNAs <- NA %in% uvalues
        pctNA <- sum( is.na( daDF[ , i ] ) ) / length( daDF[ , i ] )
        daCol <- list( uValues = uvalues,
                       uCount = ucount,
                       name = cname,
                       class = cclass,
                       levelCount = clevcount,
                       summary = csumm,
                       hasBlanks = hasBlanks,
                       hasNAs = hasNAs,
                       pctNA = pctNA
                      )
        Mmeta[[ i ]] <- daCol
    }
    return( Mmeta )      
}



show_col <- function( n ) {
    message( paste( "NAME:", Mmeta[[ n ]]$name ) )
    message( paste( "CLASS:", Mmeta[[ n ]]$class, " LEV COUNT:", Mmeta[[ n ]]$levelCount ) )
    message( paste( "HAS BLANKS:", Mmeta[[ n ]]$hasBlanks, "  HAS NAS:", Mmeta[[ n ]]$hasNAs, "pctNA:", Mmeta[[ n ]]$pctNA ) )
    print( Mmeta[[ n ]]$summary )
    message( "======================" )   
}



show_Mmeta <- function( n=NULL ) {
    if( !is.null(n) ) {
       show_col( n )
    } else {
        print( "ALL" )
        for( i in 1 : length( Mmeta ) ){ # get an overview of the data
            show_col( i )
        }
    }
}




##########################################################
###   DATA CLEANING & PREPARATION                      ###
##########################################################

### 1. load the data
inTraining <- read.table( './data/pml-training.csv',
                          header = TRUE,
                         sep = ",",
                         row.names = 1
                         )
### 2. look at the data, uncomment to check
### View( inTraining )   
Mmeta <- inspect( inTraining )
### show_Mmeta( 135 )

### 3. fix #DIV/0!, found in numerous columns, for example col 135 min_yaw_forearm
### this happens to take care of the Blanks too
for( i in 1 : length( Mmeta ) ) {
    if( "#DIV/0!" %in% Mmeta[[ i ]]$uValues ) {
        ixs <- which( inTraining[ , i ] == "#DIV/0!" )
        inTraining[ ixs, i ] <- NA
        inTraining[ , i ] <- as.numeric( inTraining[ , i ] )
    }
}

### 4. verify no more columns with #DIV/0!
rm( Mmeta )
Mmeta <- inspect( inTraining )
### show_Mmeta()  # or show_Mmeta( 135 ), uncomment to check

### 5. verify no more columns with Blanks
bl <- c()
for( i in 1:length(Mmeta) ){
   if( Mmeta[[ i ]]$hasBlanks ) {
     bl <- c( bl, Mmeta[[i]]$name )
     print( Mmeta[[i]]$name )
   }
}
bl

### 6. observe the NAs
nas <- c()
for( i in 1 : length( Mmeta ) ){
    nas <- c( nas, Mmeta[[ i ]]$pctNA )
}
pctgNAtraining <- plot( nas,
                        main = "Training Dataset - Pctg NAs Across Variables",
                        xlab = "Variable Index No.",
                        ylab = "Pctg NA"
                       )
pctgNAtraining

### 7. discard columns with > 3% NAs
discard = c()
for( i in 1 : length( nas ) ){
    if( nas[ i ] > 0.03 ){
        discard <- c( discard, i )
    }
}
inTraining <- inTraining[ , -discard ]
dim( inTraining )

### 8. remove incomplete cases
inTraining <- inTraining[ complete.cases( inTraining ), ]
dim( inTraining )

### 9. remove columns username and timestamps
inTraining <- inTraining[ , ( -1 : -4 ) ]
dim( inTraining )




##########################################################
###   CLEAN TEST DATA                                  ###
##########################################################
### we'll repeat steps 1-9 above on the test dataset

### 1. load the data
inTesting <- read.table( './data/pml-testing.csv',
                          header = TRUE,
                         sep = ",",
                         row.names = 1
                         )
### 2. look at the data, uncomment to check
### View( inTesting )   
rm( Mmeta )
Mmeta <- inspect( inTesting )
### show_Mmeta( 135 )

### 3. fix #DIV/0!, found in numerous columns, for example col 135 min_yaw_forearm
### this happens to take care of the Blanks too
for( i in 1 : length( Mmeta ) ) {
    if( "#DIV/0!" %in% Mmeta[[ i ]]$uValues ) {
        ixs <- which( inTesting[ , i ] == "#DIV/0!" )
        inTesting[ ixs, i ] <- NA
        inTesting[ , i ] <- as.numeric( inTesting[ , i ] )
    }
}

### 4. verify no more columns with #DIV/0!
rm( Mmeta )
Mmeta <- inspect( inTesting )
### show_Mmeta()  # or show_Mmeta( 135 ), uncomment to check

### 5. verify no more columns with Blanks
bl <- c()
for( i in 1:length(Mmeta) ){
   if( Mmeta[[ i ]]$hasBlanks ) {
     bl <- c( bl, Mmeta[[i]]$name )
     print( Mmeta[[i]]$name )
   }
}
bl

### 6. observe the NAs
nas <- c()
for( i in 1 : length( Mmeta ) ){
    nas <- c( nas, Mmeta[[ i ]]$pctNA )
}
pctgNAtesting <- plot( nas,
                       main = "Testing Dataset - Pctg NAs Across Variables",
                       xlab = "Variable Index No.",
                       ylab = "Pctg NA"
                     )
pctgNAtesting

### 7. discard columns with > 3% NAs
discard = c()
for( i in 1 : length( nas ) ){
    if( nas[ i ] > 0.03 ){
        discard <- c( discard, i )
    }
}
inTesting <- inTesting[ , -discard ]
dim( inTesting )

### 8. remove incomplete cases
inTesting <- inTesting[ complete.cases( inTesting ), ]
dim( inTesting )

### 9. remove columns username and timestamps / dates
inTesting <- inTesting[ , ( -1 : -4 ) ]
dim( inTesting )




######################################################
###   TRAINING - TESTING DATASET COLUMN MATCHING   ###
######################################################
### upon inspection, the training dataset has many more columns than the test dataset
### now we'll discard columns from the training data set which are NOT contained in the test dataset

not_in_testdata <- c()
for( i in 1 : ( length( names( inTraining ) )-1) ){
    nn <- names( inTraining )[ i ]
    intestinglist <- names( inTesting )
    if( !(nn %in% intestinglist ) ){
        not_in_testdata <- c( not_in_testdata, which( names( inTraining ) == nn ) )
    }
}

### obtain final training dataset
inTraining <- inTraining[ , -not_in_testdata]
dim( inTraining )
dim( inTesting )    



##########################################################
###   MODEL FITTING WITH RANDOM FOREST                 ###
##########################################################

library( randomForest )
library( caret )

set.seed( 56789 )

RFfit <- randomForest( classe ~ .,
                       data = inTraining,
                       importance = TRUE
                     )
RFfit
impPlot <- varImpPlot( RFfit )
impPlot



##########################################################
###   PREDICTION - APPLYING THE MODEL TO TEST DATA     ###
##########################################################

yhat <- predict( RFfit, newdata=inTesting[, -1*(dim(inTesting)[2] ) ] )
yhat



