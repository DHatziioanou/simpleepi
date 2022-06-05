# simpleepi


For information on how to use the below functions type ?function_name

# Install package
devtools::install_github("DHatziioanou/simpleepi")


# Examples of packaged functions;

## Install and load CRAN packages and their dependencies
install_load()


## Get and save data

### Import data
simpleimport()


### Write data
simplewrite(df)

### Collect looped itterations of data
simpleappendenv(df)


### Execute sql file query by odbc to SQL database  
Run custom sql queries from .sql files with custom connections to SQL server databases. This saves the query data directly to the global environment in an object named using the `name` argument.

    sqlfile_query(server = "ser.path", database ="W001", sqlfile  = "query.sql", name = "df")
    
    

## Manage data

### Find most recently modified file
Automated processing often involves use of routinely updated files from various sources which may or may not be saved with consistent file names. In these instances as long as there is a part of the file names which is consistent this function retrieves the latest file where the consistent string pattern is expected and can return either the file name or the file path for automated import into R.  

For example this will find the most recent test data file from path C:\\temp and its sub-folders and return a data.frame with the file name, full path, and other characteristics such as creation and modification times;  

    testdata_file <- getlatestfile(folder_path = "C:\\temp", file_string = "test_data.csv", return_type = "all", recursive = TRUE)

If only the path or file name is needed this can be done by setting return_type to path or name respectively  

    testdata_path <- getlatestfile(folder_path = "C:\\temp", file_string = "test_data.csv", return_type = "path", recursive = TRUE)

  
A useful feature of this function is the ability to increase the number of times the file search is performed with the `maxTries` argument. This can overcome patchy server connectivity.  


### Check if file size, creation/modification dates identical
Keeping on top of whether you have the latest version of multiple named files at a particular location can be time consuming. If you need to check whether files have been updated before you use them, that they have successfully copied to a location in their entirety or that all work has been backup up `simplefilecheck` can help by checking a file is present in two locations and that it has the same file size, creation and modifications times.  

If the two files have the same size, creation and modification times the function returns TRUE, otherwise it returns FALSE.  

This allows for automated file transfer pipelines to be built and incorporate QC steps into pipelines. Example;

Check latest file is available and retrieve if necessary;   

    check <- simplefilecheck(origin, dest)
    if(!isTRUE(check)){
      file.copy(origin, dest, overwrite = T)
      check <- simplefilecheck(origin, dest)
      if(check) message(paste0("Retrieved ", filename))
    } else { 
    message(paste0("Already have ", filename, " with matching file size, creation and modification dates."))}
    }

Check files and copy any where check returns FALSE;   

    files <- list.files(paths$out_path, full.names = T, recursive = T)
    files_ok <- lapply(files, function(x) simplefilecheck(x = x, file.path(paths$backup, basename(x))))
    if(any(files_ok == FALSE)) { 
        lapply(files[files_ok==FALSE], function(x) file.copy(from = x, to = paths$backup, overwrite = T, recursive = T, copy.date = T, copy.mode = T))
        }

### Create or update a version control column

Two options are available; changes can be captured in a version control column either as a string with comma separated records in the format 'date;record' or as a list of data.tables with columns date, versions and notes where additional notes can be entered for each version of a record.

    vcdb$column_VC <- simple_version_control(vcdb, oldcol = "V1", newcol = "V2", id = "key", olddate = "20220501", newdate = "20220601", type = "flat", out = "vector")

For large databases it may be necessary to split data into chunks for processing. Example for a large data.table `db` with a unique identifier named `key` and values for version control under columns `valueprev` and `value` the example below will process chunks of 10,000,000 rows at a time then combine the chunks into one data.table;  

    # Split into chunks of 10,000,000 rows
    db <- split(db, (as.numeric(rownames(db))-1) %/% 10000000)
    for (s in 1:length(db)){
      db[[s]][, variable_VC := simple_version_control(dt = db[[s]], id = "key", oldcol = "valueprev", newcol = "value", olddate = 20220101, newdate = 20220102, type = "flat", out = "vector", vccol = "variable_VC")] }
    db <- rbindlist(db, use.names = T, fill = T)

Version control update details are captured under column attributes and can be accessed using `attributes(vcdb$column_VC)`.  

Changes which occurred on a particular date can be checked based on the date arguments given to simple_version_control. For example for `vc_date` of `20220601`;  

    vc_date <- "20220601"
    anydifference <- vcdb[grepl(vc_date, column_VC),]
    removed <- vcdb[grepl(paste0(vc_date,";NA"), column_VC, fixed = T),]
    new <- vcdb[grepl(paste0("^",vc_date), column_VC),]

Previous versions of database columns can be make using `simple_vc_snapshot()` using the column and required snapshot date as arguments.  

    date <- "20220529"
    col <- "column_VC"
    df$Historic20220529 <- simple_vc_snapshot(x = df$data_VC, date = "20220529")

### File management of old files into an archive folder
archive()


### Open File Explorer or files from path
Directly open files or folders used in R processes using file or folder path.  

Example; for a list of paths

    paths <- list(in_folder1 = "C:/apath/path1",
                  in_folder2 = "C:/apath/path2",
                  in_file1 = "C:/apath/path3/file1.csv",
                  out_folder ="C:/apath/path6",
                  out_file1 = "C:/apath/path6/out.csv",
                  out_plot1 = "C:/apath/path6/figure1.png")

Open a single path  

    simpleopen(path = paths$out_plot1)
   
Open all files and folders listed in `paths` object

    lapply(paths, function(x) simpleopen(x))




## Format and process data

simpledates()  
simplewords()  
simplenumber()  

### Add isoweeks
simpleIsoweek()  

### Add age groups
Group age data into any custom discrete age groups and mark where age not known using the `unknowns` argument. Can chose to return either a character vector or a factor using the `factor` argument.  

    df$group <- age_groups(df$Age, 
                          groups = c('0-17', "18-69", "60-80+"), 
                          unknowns = c(-1, NA, "Unknown", "not reported"), 
                          factor=TRUE)
    

### add ONS populations if needed
ONS_age_groups()   


### add NSPL data if needed
index_postcodes()  
match_postcode()

### check against other dataset if needed
fuzzy_match()  

### Assign unique identifiers based on PII and/or ID number
simpleID()

### Assign episodes based on ID and episode duration
episodes()

### Retrieve or remove replicate rows from dataframe
replicates()
replicates_rmv()

### Remove replicate rows
remove_replicates()

### Create a line list where each row holds data for one unique ID  

simpleaggregate()



### Check if objects have duplicated column names
colnamecheck(list(df1,df2))


