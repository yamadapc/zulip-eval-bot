FROM haskell:7.8

RUN cabal update
ENV PATH /root/.cabal/bin:$PATH

ADD ./zulip-eval-bot.cabal /opt/bot/zulip-eval-bot.cabal
RUN cd /opt/bot && cabal install --force -j4 process-1.2.0.0 time-1.5
RUN cd /opt/bot && cabal install --force-reinstalls --only-dependencijes -j4

ADD ./ /opt/bot
RUN cd /opt/bot && cabal install

WORKDIR /opt/bot
