FROM erlang:18.3

RUN apt-get update && apt-get install -y \
    curl

RUN mkdir /opt/rebar && \
    curl -L https://github.com/rebar/rebar/archive/2.6.4.tar.gz | \
    tar xzv --strip-components 1 -C /opt/rebar && \
    cd /opt/rebar && \
    ./bootstrap

ENV PATH /opt/rebar:$PATH

WORKDIR /app

COPY rebar.config app.config /app/
COPY src /app/src/
COPY priv /app/priv/
COPY include /app/include/

RUN rebar get-deps
RUN rebar compile

ENV PEWPEW_ENV development

CMD erl -pa ebin -pa deps/*/ebin -config app -s pewpew
