.PHONY: all clean

all: js/app.js

clean:
	dune clean && rm -r js/app.js

js/app.js: _build/default/src/app.bc.js node_modules
	npx browserify $< \
	  -r react \
	  -r react-dom \
	  -g [ envify --NODE_ENV production ] \
	  -g uglifyify \
	| npx uglifyjs -c -m -o $@

_build/default/src/app.bc.js: src/app.ml
	dune build --profile=release src/app.bc.js

node_modules: package.json
	npm install && touch $@
