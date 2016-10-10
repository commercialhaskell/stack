# Dependency visualization

You can use stack to visualize the dependencies between your packages and
optionally also external dependencies.

As an example, let's look at `wreq`:

```
$ stack dot | dot -Tpng -o wreq.png
```
[![wreq](https://cloud.githubusercontent.com/assets/591567/8478591/ae10a418-20d2-11e5-8945-55246dcfac62.png)](https://cloud.githubusercontent.com/assets/591567/8478591/ae10a418-20d2-11e5-8945-55246dcfac62.png)

Okay that is a little boring, let's also look at external dependencies:
```
$ stack dot --external | dot -Tpng -o wreq.png
```
[![wreq_ext](https://cloud.githubusercontent.com/assets/591567/8478621/d247247e-20d2-11e5-993d-79096e382abd.png)](https://cloud.githubusercontent.com/assets/591567/8478621/d247247e-20d2-11e5-993d-79096e382abd.png)

Well that is certainly a lot.  As a start we can exclude `base` and then
depending on our needs we can either limit the depth:

```
$ stack dot --no-include-base --external --depth 1 | dot -Tpng -o wreq.png
```
[![wreq_depth](https://cloud.githubusercontent.com/assets/591567/8484310/45b399a0-20f7-11e5-8068-031c2b352961.png)](https://cloud.githubusercontent.com/assets/591567/8484310/45b399a0-20f7-11e5-8068-031c2b352961.png)

or prune packages explicitly:

```
$ stack dot --external --prune base,lens,wreq-examples,http-client,aeson,tls,http-client-tls,exceptions | dot -Tpng -o wreq_pruned.png
```
[![wreq_pruned](https://cloud.githubusercontent.com/assets/591567/8478768/adbad280-20d3-11e5-9992-914dc24fe569.png)](https://cloud.githubusercontent.com/assets/591567/8478768/adbad280-20d3-11e5-9992-914dc24fe569.png)

Keep in mind that you can also save the dot file:
```
$ stack dot --external --depth 1 > wreq.dot
$ dot -Tpng -o wreq.png wreq.dot
```

and pass in options to `dot` or use another graph layout engine like `twopi`:

```
$ stack dot --external --prune base,lens,wreq-examples,http-client,aeson,tls,http-client-tls,exceptions | twopi -Groot=wreq -Goverlap=false -Tpng -o wreq_pruned.png
```
[![wreq_pruned](https://cloud.githubusercontent.com/assets/591567/8495538/9fae1184-216e-11e5-9931-99e6147f8aed.png)](https://cloud.githubusercontent.com/assets/591567/8495538/9fae1184-216e-11e5-9931-99e6147f8aed.png)

## Specifying local targets and flags

The `dot` and `list-dependencies` commands both also accept the following
options which affect how local packages are considered:

* `TARGET`, same as the targets passed to `build`
* `--test`, specifying that test components should be considered
* `--bench`, specifying that benchmark components should be considered
* `--flag`, specifying flags which may affect cabal file `build-depends`
