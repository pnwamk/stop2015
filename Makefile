
default: pdf

pdf: dtr-stop2015.scrbl utils.tex
	scribble --latex ++style utils.tex dtr-stop2015.scrbl && pdflatex dtr-stop2015.tex

clean:
    rm *.aux
	rm *.log
	rm *.out
	rm *.cls
