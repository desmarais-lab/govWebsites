#verify which .gov websites actually work
#by opening each in a webdriver-controlled browser
#and recording the url it redirects to
data/verified_sites.txt: data/current-full.csv
	python2 govWebsitesVerification.py 1 5647

#same as above, but run 3 webdrivers at the same time; faster
#depending on fast internet connection, 6 should be possible too
#THIS MAY NOT WORK FROM THE MAKEFILE :(
#verify_parallel:
#	python2 govWebsitesVerification.py 1 901 &	python2 govWebsitesVerification.py 901 1801 &	python2 govWebsitesVerification.py 1801 2701
#	python2 govWebsitesVerification.py 2701 3601 &	python2 govWebsitesVerification.py 3601 4501 &	python2 govWebsitesVerification.py 4501 5647

#assembles the output from the python scripts into a data frame and makes some corrections by hand
data/govWebsitesVerified.Rdata: data/verified_sites.txt data/current-full.csv
	R CMD BATCH govWebsitesVerification.R

#merge with census data; important file where a LOT gets done, partially by hand
data/govWebsitesVerifiedCensus.Rdata: data/sub-est2015_all.csv data/govWebsitesVerified.Rdata
	R CMD BATCH govWebsitesCensusMerge.R

#uses the matched GSA/Census data to produce states/population breakdown with ggplot
paper/figures/coverage_states.pdf: data/govWebsitesVerifiedCensus.Rdata data/sub-est2015_all.csv
	R CMD BATCH govWebsitesCoverage.R

#Use Ruby package (run from within R) to scrape 10 randomly selected
#websites from the Wayback Machine
#I changed this, this file is now just a function to for source()
#which uses the Ruby waybackmachine downloader in R
#wayback_downloader:
#	R CMD BATCH internetarchive_webarchive.R

#read in election data from indiana (downloaded from their website)
#then download the gov. websites from the wayback machine
#!!!!!!!!!!!!!!!!!
#OLD
#indiana:
#	R CMD BATCH govWebsitesIndiana2015.R

#download Indianapolis website, then try out topic models
### OUTDATED, and not worth the trouble
#indianapolis:
#	R CMD BATCH govWebsitesIndianapolis.R

#check filetypes of documents
#not actually limited to Indy any more
#!!!!!!!!!!!!!!!!!
#OLD
#indianapolis_filetypes:
#	R CMD BATCH govWebsitesIndianapolisFiletypes.R

#scrape snapshot dates from the WaybackMachine 'calendar' and plot them
#given what we know about the wayback machine now, this doesnt actually make sense any more
#snapshotsDates:
#	R CMD BATCH govWebsitesSnapshotsDates.R

#scrape Louisiana website URLs from Wikipedia
data/LouisianaWebsiteURLs.rdata:
	R CMD BATCH scrapeLousianaWebsites.R

#scrape Indiana website URLs from Wikipedia
data/indianaWebsiteURLs.rdata:
	R CMD BATCH scrapeIndianaWebsites.R

#combine the URLs from different sources
data/URLs_IN.rdata: data/louisianaWebsiteURLs.rdata data/indianaWebsiteURLs.rdata data/govWebsitesVerifiedCensus.Rdata data/indianaElections2015.rdata data/LEAP_Louisiana_All_Offices_neumann.xlsx
	R CMD BATCH combineURLs.R

#produce Latex tables with filetypes of websites
#also produce Latex table of number of files and size of sites
#produces latex code, no files
#currently table 1 and 2
#paper/tables/filetypes.tex
filetypes.tex filenumbers.tex: data/URLs_IN.rdata
	R CMD BATCH govWebsitesFiletpyes.R

#scrape URLs from Google, UNFINISHED
#scrapeLousianaURLsGoogle:
#	findLouisianaWebsites.py

#TWO WAYS to parse the websites:
#USE ONLY ONE

#1. Unix Shell

#recursively convert all files from pdf, html, doc, docx
#requires:
#antiword (https://www.archlinux.org/packages/community/x86_64/antiword/)
#docx2txt (https://www.archlinux.org/packages/community/any/docx2txt/)
#pdftotext (contained in many Unix pdf readers such as poppler, or xpdf)
#html2text (https://aur.archlinux.org/packages/html2text-with-utf8)
convertEverything:
	./websites/batchconversion.sh

#2. Kenneth Benoits readtext package for R
#convertEverythingR:
#	R CMD BATCH readtext.R

#only one of the above is necessary

#use Mallet,
#this will also call the hunspell spellchecking
#the hunspell spellchecking takes a long time (i.e. 14 hours on 6 cores)
paper/figures/wtp_current_dem_rep.pdf: rfiles/d.Rdata
	R CMD BATCH malletTraining.R

paper/figures/partisanTopics_all.pdf: rfiles/d.Rdata
	R CMD BATCH malletAnalysisPartisanTopics.R


#compile Latex
# -c option cleans up nonessential results except pdf
manuscript.pdf:
	cd paper; \
	latexmk -quiet -pdf manuscript; \
	latexmk -c
