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

#assembles the output from the python scripts into a data frame
#and merges it with census data on cities
merge_census:
	R CMD BATCH govWebsitesVerification.R
