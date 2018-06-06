all: slides.html

slides.html: slides.md
	pandoc -t revealjs -s -o slides.html slides.md -V revealjs-url=reveal.js  -V theme=beige

clean:
	rm -f slides.html

.PHONY: all clean
