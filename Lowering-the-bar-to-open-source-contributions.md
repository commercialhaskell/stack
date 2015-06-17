Stack remembers things for you so that you don't have to. Here's an example.

I had a trivial change that I wanted to try out adding to http-client. Here are the steps I went through:

    git pull
    stack test # yep, project builds and passes test suites
    <<make change>>
    stack test # yep, it still builds and passes test suites
    git add <<my changes>>
    git commit -m <<explain my changes>>
    git push forked-origin
    <<submit PR on github>>

Notice the things I didn't have to remember to do:

    create a sandbox
    install dependencies
    test the other packages in the repo against my changes to http-client

Stack lowers the barrier to open source contributions. If your open source project comes with a stack.yaml, then it is easy for contributors to jump right in and get the real work done, rather than fiddling with package management. Just instruct contributors to download stack and stack test.

Something which is perhaps less obvious at first is how to get ghci to work with your stack-ified project. It's quite simple once you know:

```
$ stack ghci
Configuring GHCi with the following projects: http-client-openssl, http-client-tls, http-client, http-conduit
GHCi, version 7.8.4: ...
Prelude> :load Network.HTTP.Client
Network.HTTP.Client>
```