todo:
	grep -r TODO src test/client/* README.md
	grep -r undefined src test/client/* README.md

docker-build:
	./scripts/docker-build

dot:
	find src -name '*.hs' | xargs graphmod -q > doc/modules.dot
	dot -Tpng doc/modules.dot > doc/modules.png
	open doc/modules.png
