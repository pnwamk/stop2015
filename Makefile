
default: pdf

pdf: dtr-stop2015.scrbl utils.tex
	scribble --latex ++style utils.tex dtr-stop2015.scrbl && \
	pdflatex dtr-stop2015.tex

clean:
	rm -f *.aux
	rm -f *.log
	rm -f *.out
	rm -f *.cls
