mkdir pdfsfolder
find . -name "*.pdf" -type f -exec cp {} ./pdfsfolder \;
cd pdfsfolder
#assuming that xpdf is installed:
for file in *.pdf; do pdftotext "$file" "$file.txt"; done