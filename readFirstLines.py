import os
import re
import time

PATH = "/home/mneumann/hd2/govWebsites/"
results = [val for sublist in [[os.path.join(i[0], j) for j in i[2]] for i in os.walk(PATH)] for val in sublist]
results = results[8:]

r = re.compile(".*[^\x00-\x7F]")
results2 = [x for x in results if not r.match(x)]

tt = []
firstlines = []
for file in results2:
    start_time = time.time()
    for line in file:
        firstline = open(file, encoding = "ISO-8859-1").readline().rstrip()
        if firstline != "":
            firstlines.append(firstline)
            break
    tt.append(time.time() - start_time)

#write first lines
thefile = open('data/firstlines.txt', 'w')
for item in firstlines:
  thefile.write("%s\n" % item)
thefile.close()


#write file paths
thefile = open('data/filepaths.txt', 'w')
for item in results2:
  thefile.write("%s\n" % item)
thefile.close()
