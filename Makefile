all: build

build: install
	yarn next build

format: install
	yarn prettier --write .

install:
	yarn install

lint: install
	yarn eslint ./src

serve: install
	yarn next-remote-watch data/posts/

typecheck: install
	yarn tsc --noEmit --watch
