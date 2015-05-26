FROM haskell:7.8
MAINTAINER roman@bromeco.de

VOLUME ["/workspace"]
RUN mkdir /home/swuser

RUN groupadd -r swuser -g 1000 && \
    useradd -u 1000 -r -g swuser -d /home/swuser -s /sbin/nologin -c "Docker image user" swuser && \
    chown -R swuser:swuser /home/swuser

RUN chown swuser:swuser /workspace

RUN apt-get update && apt-get -fy install\
    vim git make exuberant-ctags

USER swuser
RUN cabal update
RUN cabal install ghc-mod
ENV PATH ~/.cabal/bin:$PATH
CMD bash
