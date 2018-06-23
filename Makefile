all:: push

push: build
	cd .build/ && git add . && git commit -m 'Build from source' && git push origin gh-pages

build: test clean clone elm

elm:
	elm make --warn --output .build/index.html src/Main.elm

format:
	elm format --yes src/

test: format
	elm analyse

clone:
	git clone --branch gh-pages git@github.com:wildlyinaccurate/is-this-real-life.git .build/

clean:
	rm -rf .build/
