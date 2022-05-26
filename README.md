# simpleepi


For information on how to use the below functions type ?function_name

# Install package
devtools::install_github("DHatziioanou/simpleepi")


# Example of packaged functions;

## Deal with CRAN packages
install_load()

## Find the most recently modified file
Automated processing often involves use of routinely updated files from various sources which may or may not be saved with consistent file names. In these instances as long as there is a part of the file names which is consistent this function retrieves the latest file where the consistent string pattern is expected and can return either the file name or the file path for automated import into R.  

For example this will find the most recent test data file from path C:\\temp and its sub-folders and return a data.frame with the file name, full path, and other characteristics such as creation and modification times;  
`testdata_file <- getlatestfile(folder_path = "C:\\temp", file_string = "test_data.csv", return_type = "all", recursive = TRUE)`

If only the path or file name is needed this can be done by setting return_type to path or name respectively  
`testdata_path <- getlatestfile(folder_path = "C:\\temp", file_string = "test_data.csv", return_type = "path", recursive = TRUE)`

A useful feature of this function is the ability to increase the number of times the file search is performed with the `maxTries` argument. This can overcome patchy server connectivity.  


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

## Retrieve or remove replicate rows from dataframe
replicates()
replicates_rmv()

## Remove replicate rows
remove_replicates()

## Create a line list where each row holds data for one unique ID  

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

## Compare update of data across columns
simple_version_control()


## File management of old files into an archive folder
archive()
