#Code is a modified version of this post: http://stackoverflow.com/a/39734952

# -*- coding: utf-8 -*-
from selenium import webdriver
from selenium.webdriver.common.keys import Keys
from selenium.common.exceptions import TimeoutException
from selenium.common.exceptions import WebDriverException
import json
import os
import urllib2
import time
import csv

govWebsitesFile = open('data/current-full.csv')
govWebsitesReader = csv.reader(govWebsitesFile)
govWebsitesData = list(govWebsitesReader)

siteslist = []
browser = webdriver.Firefox()
browser.set_page_load_timeout(25)

for i in range(4501,5647): #sum(1 for line in open('data/current-full.csv'))
    website = govWebsitesData[i][0]
    url = "http://www."+str.lower(str(website))
    try:
        browser.get(url)
        #header={'User-Agent':"Mozilla/5.0 (Windows NT 6.1; WOW64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/43.0.2357.134 Safari/537.36"}
        sitelink = browser.current_url
        siteslist.append(sitelink)
    except TimeoutException:
        #try:
        #    time.sleep(5)
        #    browser.get(url)
        #    header={'User-Agent':"Mozilla/5.0 (Windows NT 6.1; WOW64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/43.0.2357.134 Safari/537.36"}
        #    sitelink = browser.current_url
        #    siteslist.append(sitelink)
        #except TimeoutException:
        sitelink = "TimeoutException"
        siteslist.append(sitelink)
    except WebDriverException:
        sitelink = "WebDriverException"
        siteslist.append(sitelink)
    except:
        sitelink = "Error"
        siteslist.append(sitelink)

outfile = open("results/results6.txt", 'w')
for item in siteslist:
    outfile.write("%s\n" % item)
outfile.close()

browser.close()
