


xname='FinalArticle' #### EDIT HERE

########
#we can add the code needed later or even ignore this completely and add our plots manually to the assest folder

#Rscript _code_A.R
#Rscript _code_B.R
#Rscript _code_C.R



########### use pdflatex CLI
########### use pdflatex CLI
########### use pdflatex CLI

#rm  "$xname".aux
#rm  "$xname".bbl
#pdflatex -interaction=nonstopmode   "$xname".tex
#bibtex   "$xname"
#pdflatex -interaction=nonstopmode   "$xname".tex
#pdflatex -interaction=nonstopmode   "$xname".tex


####### OR
####### OR
####### OR ##### use latexmk

rm  "$xname".aux
rm  "$xname".bbl

latexmk -pdf "$xname".tex
latexmk -c

###########


