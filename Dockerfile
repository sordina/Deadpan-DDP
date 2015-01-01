FROM haskell:7.8
MAINTAINER Lyndon Maydwell <maydwell@gmail.com>
RUN cabal update
ADD . /opt/deadpan-ddp
RUN cd /opt/deadpan-ddp && cabal install -j4

RUN apt-get update
RUN apt-get -y install zip 1> /dev/null 2> /dev/null

ENV PATH /root/.cabal/bin:/bin:/opt/X11/bin:/opt/local/sbin:/sbin:/usr/X11R6/bin:/usr/bin:/usr/local/bin:/usr/local/git/bin:/usr/local/sbin:/usr/pkg/bin:/usr/pkg/sbin:/usr/sbin:~/bin
WORKDIR /opt/deadpan-ddp
RUN ls
RUN ./scripts/cabal-s3
CMD ["deadpan"]
