FROM fpco/stack-build:lts

# we should pre-build stack dependencies before coping the rest of the source
# so we will not build dependencies every time the source changes
COPY *.yaml               /src/
COPY subs/curator/*.yaml  /src/subs/curator/
COPY subs/pantry/*.yaml   /src/subs/pantry/
RUN cd ./src                                            && \
    for i in $(stack ls dependencies | sed 's/ /-/g');  \
      do stack build --fast $i;                         \
      done

COPY . /src/

RUN cd ./src                                            && \
    stack build --fast

# we can't use install to install stack so we are installing it manually
RUN cd ./src                                            && \
    cp -a $(dirname $(stack exec which stack))/. $(dirname $(which stack))
