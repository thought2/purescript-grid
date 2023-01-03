build:
    spago build --purs-args "--stash"

docs-md:
    spago docs --format markdown

format:
    purs-tidy format-in-place 'src/**/*.purs'