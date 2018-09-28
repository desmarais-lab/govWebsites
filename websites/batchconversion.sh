# convert doc:
./batchconversion_doc.sh
#convert docx:
./batchconversion_docx.sh
#convert pdf:
./batchconversion_pdf.sh
#convert html:
./htmlrename.sh
./batchconversion_html.sh

#then delete everything in the folders except the text files
find ./websites/ -type f ! -name '*.txt' -delete
