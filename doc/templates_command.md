<div class="hidden-warning"><a href="https://docs.haskellstack.org/"><img src="https://cdn.jsdelivr.net/gh/commercialhaskell/stack/doc/img/hidden-warning.svg"></a></div>

# The `stack templates` command

~~~text
stack templates
~~~

`stack templates` provides information about how to find templates available for
`stack new`.

Stack provides multiple templates to start a new project from. You can specify
one of these templates following your project name in the `stack new` command:

~~~text
stack new my-rio-project rio
Downloading template "rio" to create project "my-rio-project" in my-rio-project/ ...
Looking for .cabal or package.yaml files to use to init the project.
Using cabal packages:
- my-rio-project/

Selecting the best among 18 snapshots...

* Matches ...

Selected resolver: ...
Initialising configuration using resolver: ...
Total number of user packages considered: 1
Writing configuration to file: my-rio-project/stack.yaml
All done.
<Stack root>\templates\rio.hsfiles:   10.10 KiB downloaded...
~~~

The default templates repository is
https://github.com/commercialhaskell/stack-templates. You can download templates
from a different GitHub user by prefixing the username and a slash. Command:

~~~text
stack new my-yesod-project yesodweb/simple
~~~

Then template file `simple.hsfiles` would be downloaded from GitHub repository
`yesodweb/stack-templates`.

You can even download templates from a service other that GitHub, such as
[GitLab](https://gitlab.com) or [Bitbucket](https://bitbucket.com). For example,
command:

~~~text
stack new my-project gitlab:user29/foo
~~~

Template file `foo.hsfiles` would be downloaded from GitLab, user account
`user29`, repository `stack-templates`.

If you need more flexibility, you can specify the full URL of the template.
Command:

~~~text
stack new my-project https://my-site.com/content/template9.hsfiles
~~~

(The `.hsfiles` extension is optional; it will be added if it's not specified.)

Alternatively you can use a local template by specifying the path. Command:

~~~text
stack new project <path_to_template>/template.hsfiles
~~~

As a starting point for creating your own templates, you can use the
["simple" template](https://github.com/commercialhaskell/stack-templates/blob/master/simple.hsfiles).
The
[stack-templates repository](https://github.com/commercialhaskell/stack-templates#readme)
provides an introduction into creating templates.
