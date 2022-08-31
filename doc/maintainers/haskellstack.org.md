<div class="hidden-warning"><a href="https://docs.haskellstack.org/"><img src="https://cdn.jsdelivr.net/gh/commercialhaskell/stack/doc/img/hidden-warning.svg"></a></div>

# HaskellStack.org

The domain https://docs.haskellstack.org hosts online documentation for the
Stack project, using [Read the Docs](https://readthedocs.org/) with
[MkDocs](https://www.mkdocs.org/).

## Read the Docs

The Read the Docs project is named
['The Haskell Tool Stack'](https://readthedocs.org/projects/stack/).

The set up on the Read the Docs web site involves two page redirects when there
are HTTP 404 Not Found errors:

    / -> /README/
    /README/ -> /

The 'Home' MkDocs page is `doc/README.md`, which is a symbolic link to
`/README.md`.

The `/ -> /README/` redirect ensures that
https://docs.haskellstack.org/en/stable/ (for example) will, if not found,
redirect to https://docs.haskellstack.org/en/stable/README/.

The `/README/ -> /` redirect ensures that
https://docs.haskellstack.org/en/latest/README/ (for example) will, if not
found, redirect to https://docs.haskellstack.org/en/latest/.

MkDocs rendering of `README.md` differed before and after MkDocs 1.0. Prior to
MkDocs 1.0, `README.md` rendered to `/README/index.html`. From MkDocs 1.0,
`README.md` rendered to `/index.html`. The two redirects above ensure that the
Read the Docs flyout works when moving between different versions of the home
page using the flyout.

Stack moved from MkDocs 0.17.3 to MkDocs 1.3.1 after publishing the
documentation for Stack 2.7.5.

A YAML configuration file, `.readthedocs.yaml` is included in the repository
root directory. See https://docs.readthedocs.io/en/stable/config-file/v2.html.
It specifies a Python requirements file in `doc/requirements.txt`.

## MkDocs

The `doc/requirements.txt` pins the version of MkDocs. As at 31 August 2021 it
is set to:

    mkdocs==1.3.1

A YAML configuration file, `mkdocs.yml` is included in the repository root
directory. See https://www.mkdocs.org/user-guide/configuration/.

`site_dir: _site` specifies the directory where the output HTML and other files
are created. This directory is added to the `.gitignore` file.

MkDocs 1.3.0 replaced the `pages:` key with the `nav:` key.

The 'Home' MkDocs page is `doc/README.md`, which is a symbolic link to
`/README.md`.
