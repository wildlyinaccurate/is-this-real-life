all:: push

push: build
	cd .build/ && git add . && git commit -m 'Build from source' && git push origin gh-pages

build: clean clone
	elm make --output .build/index.html src/Main.elm

clone:
	git clone --branch gh-pages git@github.com:wildlyinaccurate/is-this-real-life.git .build/

clean:
	rm -rf .build/
