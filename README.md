## govWebsites

Directory structure:

The pipeline is in the folders 01_* to 07_* and should be executed in that order. Files within these folders also have this structure. Files without a number prefix will be called by other files and do not need to be run explicitly. Files with identical prefixes can be run in any order.

The 01_other* folders will contain plots and figures that aren't produced in any of the other scripts. Ideally, we should move every plot- and table-producing script in here at some point, but currently some of them are contained in scripts that als do something else.

Most of these folders have subfolders called 'out', where the output used later is saved.

The manuscript will continue to sit in the 'paper' folder. This folder is exactly identical and should hopefully be cleaned up too at some point.

The diagnostics folder is for scripts that don't end up in the paper but have some use for us anyway.

The 00_scrapeCovariates folder will contain the scripts necessary for scraping all the data, urls, etc. that are used for the rest. It will take me longer to get all of these in order, for now I am working on getting everything in the 01_* to 07_* folders to work properly.
