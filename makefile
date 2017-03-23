#verify which .gov websites actually work
#by opening each in a webdriver-controlled browser
#and recording the url it redirects to
verify:
	python2 govWebsitesVerification_1_900.py
	python2 govWebsitesVerification_901_1800.py
	python2 govWebsitesVerification_1801_2700.py
	python2 govWebsitesVerification_2701_3600.py
	python2 govWebsitesVerification_3601_4500.py
	python2 govWebsitesVerification_4501_5647.py

#same as above, but run 3 webdrivers at the same time; faster
#depending on fast internet connection, 6 should be possible too
verify_parallel:
	python2 govWebsitesVerification_1_900.py&	python2 govWebsitesVerification_901_1800.py&	python2 govWebsitesVerification_1801_2700.py
	python2 govWebsitesVerification_2701_3600.py&	python2 govWebsitesVerification_3601_4500.py&	python2 govWebsitesVerification_4501_5647.py

#assembles the output from the python scripts into a data frame and makes some corrections by hand
verify_correct:
	R CMD BATCH govWebsitesVerification.R

#merge with census data; important file where a LOT gets done, partially by hand
merge_census:
	R CMD BATCH govWebsitesCensusMerge.R

#produce states/population breakdown with ggplot
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

#read in election data from indiana, scrape websites
indiana:
	R CMD BATCH govWebsitesIndiana2015.R

#download Indianapolis website, then try out topic models
indianapolis:
	R CMD BATCH govWebsitesIndianapolis.R
