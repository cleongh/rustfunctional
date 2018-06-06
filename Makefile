all: slides.html

slides.html: slides.md foto_nata.jpg tweak.css
	pandoc -t revealjs -s -o slides.html slides.md --include-in-header tweak.css -V revealjs-url=reveal.js  -V theme=beige

clean:
	rm -f slides.html

.PHONY: all clean
