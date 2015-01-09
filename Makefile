todo:
	grep -r TODO src test/client/* README.md
	grep -r undefined src test/client/* README.md

docker-build:
	./scripts/docker-build

upload:
	cabal-s3
	cabl sdist
	cabal upload dist/*.tar.gz

dot:
	find src -name '*.hs' | xargs graphmod -q > doc/modules.dot
	dot -Tsvg doc/modules.dot > doc/modules.svg
	find src -name '*.hs' | xargs graphmod --no-cluster -q > doc/modulesnc.dot
	dot -Tsvg doc/modulesnc.dot > doc/modulesnc.svg
	open doc/modules.svg
	open doc/modulesnc.svg

