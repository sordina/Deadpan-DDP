todo:
	grep -r TODO src test/client/* README.md
	grep -r undefined src test/client/* README.md

docker-build:
	docker build .
	docker run
	docker cp /opt/deadpan-ddp/*.zip `pwd`
