build:
    spago build --purs-args "--stash"

docs-md:
    spago docs --format markdown
    rm -rf docs
    mkdir docs
    cp generated-docs/md/Data.Grid.md -t docs

format:
    purs-tidy format-in-place 'src/**/*.purs'