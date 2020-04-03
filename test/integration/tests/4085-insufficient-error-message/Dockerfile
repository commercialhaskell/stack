FROM fpco/stack-build:lts

WORKDIR ./src

# we should pre-build stack dependencies before coping the rest of the source
# so we will not build the dependencies every time the source changes
COPY *.yaml               ./
RUN for i in $(stack ls dependencies | sed 's/ /-/g');  \
      do stack build --fast $i;                         \
      done

COPY . ./
RUN stack build --fast
# we can't use 'stack install' to install stack so we're installing it manually
RUN cp -a $(dirname $(stack exec which stack))/. $(dirname $(which stack))

WORKDIR ..
