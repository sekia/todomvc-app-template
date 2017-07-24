OCB = ocamlbuild
OCB_FLAGS = -use-ocamlfind -plugin-tag "package(js_of_ocaml.ocamlbuild)" -I src

NPM_BIN = $(shell npm bin)
BROWSERIFY = $(NPM_BIN)/browserify
UGLIFY = $(NPM_BIN)/uglifyjs

.PHONY: all clean

all: js/app.js

clean:
	$(OCB) -clean && rm -r js/app.js node_modules

js/app.js: _build/src/app.js node_modules
	NODE_ENV=production $(BROWSERIFY) $< -d -r react -r react-dom -t envify \
	| $(UGLIFY) -m -c -o $@ --source-map content=inline,url=$@.map

_build/src/app.js: _tags myocamlbuild.ml src/app.ml
	$(OCB) $(OCB_FLAGS) app.js

node_modules: package.json
	npm install && touch $@
