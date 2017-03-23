#make a new folder for just the pdfs
mkdir pdfsfolder
#copy pdfs in subdirectories to said pdf folder
find . -name "*.pdf" -type f -exec cp {} ./pdfsfolder \;
cd pdfsfolder
#assuming that xpdf is installed, convert pdfs to text
for file in *.pdf; do pdftotext "$file" "$file.txt"; done
