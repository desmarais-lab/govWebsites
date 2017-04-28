#verify which .gov websites actually work
#by opening each in a webdriver-controlled browser
#and recording the url it redirects to
verify:
	python2 govWebsitesVerification.py 1 5647

#same as above, but run 3 webdrivers at the same time; faster
#depending on fast internet connection, 6 should be possible too
#THIS MAY NOT WORK FROM THE MAKEFILE :(
verify_parallel:
	python2 govWebsitesVerification.py 1 901 &	python2 govWebsitesVerification.py 901 1801 &	python2 govWebsitesVerification.py 1801 2701
	python2 govWebsitesVerification.py 2701 3601 &	python2 govWebsitesVerification.py 3601 4501 &	python2 govWebsitesVerification.py 4501 5647

#assembles the output from the python scripts into a data frame and makes some corrections by hand
verify_correct:
	R CMD BATCH govWebsitesVerification.R

#merge with census data; important file where a LOT gets done, partially by hand
merge_census:
	R CMD BATCH govWebsitesCensusMerge.R

#uses the matched GSA/Census data to produce states/population breakdown with ggplot
coverage:
	R CMD BATCH govWebsitesCoverage.R

#Use Ruby package (run from within R) to scrape 10 randomly selected
#websites from the Wayback Machine
wayback_downloader:
	R CMD BATCH internetarchive_webarchive.R

#produce tables with filetypes of 10 test cases
check_filetypes:
	R CMD BATCH govWebsitesFiletpyes.R

#get top term frequencies
ttf:
	R CMD BATCH websites/termFrequencies.R

#read in election data from indiana (downloaded from their website)
#then download the gov. websites from the wayback machine
indiana:
	R CMD BATCH govWebsitesIndiana2015.R

#download Indianapolis website, then try out topic models
indianapolis:
	R CMD BATCH govWebsitesIndianapolis.R

#check filetypes of documents of the indianapolis website
indianapolis_filetypes:
	R CMD BATCH govWebsitesIndianapolisFiletypes.R

#scrape snapshot dates from the WaybackMachine 'calendar' and plot them
snapshotsDates:
	R CMD BATCH govWebsitesSnapshotsDates.R

#scrape Louisiana website URLs from Wikipedia
scrapeLouisianaURLs:
	R CMD BATCH scrapeLousianaWebsites.R

#scrape Indiana website URLs from Wikipedia
scrapeIndianaURLs:
	R CMD BATCH scrapeIndianaWebsites.R

#combine the URLs from different sources
combineURLs:
	R CMD BATCH combineURLs.R

#scrape URLs from Google, UNFINISHED
#scrapeLousianaURLsGoogle:
#	findLouisianaWebsites.py
