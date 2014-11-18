FROM haskell:7.8

RUN apt-get update && apt-get install -y node golang
RUN cabal update
RUN cabal install mueval
ENV PATH /root/.cabal/bin:$PATH

ADD ./zulip-eval-bot.cabal /opt/bot/zulip-eval-bot.cabal
RUN cd /opt/bot && cabal install --only-dep -j4

ADD ./ /opt/bot
RUN cd /opt/bot && cabal install

WORKDIR /opt/bot
CMD ["cabal run"]
