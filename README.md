# simpleepi


# Install package
devtools::install_github("DHatziioanou/simpleepi")


# Example of packaged functions;

## Deal with CRAN packages
install_load()

## File management of old files into an archive folder
archive()

## Get the latest modified file
getlatestfile()


## Check file sizes and creation/modification dates are identical
simplefilecheck()


## Import data
simpleimport()


## Format data
simpledates()  
simplewords()  
simplenumber()  

## Add isoweeks
simpleIsoweek()  

## make age groups
add_age_groups()    

## add ONS populations if needed
ONS_age_groups()   
merge()


## add NSPL data if needed
index_postcodes()  
match_postcode()

## check against other dataset if needed
fuzzy_match()  

## Assign unique identifiers based on PII and/or ID number
simpleID()

## Assign episodes based on ID and episode duration
episodes()

## Retrieve replicate rows
replicates()

## Remove replicate rows
remove_replicates()

## Aggregate replicates to a single row
simpleaggregate()

## Collect looped itterations of data
simpleappendenv(df)

## Write data
simplewrite(df)

## Copy files to shared location
archive()

## Open File Explorer at path to check files
simpleopenfolder(path)

## Check if objects have duplicated column names
colnamecheck(list(df1,df2))
