You can use stack to visualize the dependencies between your projects and optionally also external dependencies.

As an example, let's look at `wreq`:

```
$ stack dot | dot -Tpng -o wreq.png
```
![wreq](https://cloud.githubusercontent.com/assets/591567/8478591/ae10a418-20d2-11e5-8945-55246dcfac62.png)

Okay that is a little boring, let's also look at external dependencies:
```
$ stack dot --external | dot -Tpng -o wreq.png
```
![wreq_ext](https://cloud.githubusercontent.com/assets/591567/8478621/d247247e-20d2-11e5-993d-79096e382abd.png)

Well that is certainly a lot.  Depending on our needs we can either limit the depth:

```
$ stack dot --external --depth 1 | dot -Tpng -o wreq.png
```
![wreq_depth](https://cloud.githubusercontent.com/assets/591567/8478713/6d258a3a-20d3-11e5-920b-30586d3993d6.png)

Or we can prune packages explicitly:

```
$ stack dot --external --prune base,lens,wreq-examples,http-client,aeson,tls,http-client-tls,exceptions | dot -Tpng -o wreq_pruned.png
```
![wreq_pruned](https://cloud.githubusercontent.com/assets/591567/8478768/adbad280-20d3-11e5-9992-914dc24fe569.png)

Keep in mind that you can also save the dot file:
```
$ stack dot --external --depth 1 > wreq.dot
$ dot -Tpng -o wreq.png wreq.dot
```

and pass in options to `dot` or use another graph layout engine like `neato`.
