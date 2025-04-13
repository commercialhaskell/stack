<div class="hidden-warning"><a href="https://docs.haskellstack.org/"><img src="https://cdn.jsdelivr.net/gh/commercialhaskell/stack/doc/img/hidden-warning.svg"></a></div>

# HaskellStack.org

The domain https://docs.haskellstack.org hosts online documentation for the
Stack project, using [Read the Docs](https://readthedocs.org/) with
[MkDocs](https://www.mkdocs.org/) and the Material for MkDocs
[theme](https://squidfunk.github.io/mkdocs-material/).

The domain https://get.haskellstack.org provides URLs that redirect to URLs
used to install the Stack executable.

## Read the Docs

The Read the Docs project is named
['The Haskell Tool Stack'](https://readthedocs.org/projects/stack/).

The set up on the Read the Docs web site involves two page redirects when there
are HTTP 404 Not Found errors:

    / -> /README/
    /README/ -> /

The 'Home' MkDocs page is `doc/README.md`.

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

A configuration file, `.readthedocs.yaml` is included in the repository root
directory. See https://docs.readthedocs.io/en/stable/config-file/v2.html. It
specifies a Python requirements file in `doc/requirements.txt`.

## MkDocs

The `doc/requirements.txt` file pins the version of MkDocs. As at
13 April 2025 it is set to:

    mkdocs==1.6.0

A configuration file, `mkdocs.yml` is included in the repository root directory.
See https://www.mkdocs.org/user-guide/configuration/.

`site_dir: _site` specifies the directory where the output HTML and other files
are created. This directory is added to the `.gitignore` file.

## Material for MkDocs

Stack moved from the default `readthedocs` theme to Material for MkDocs after
publishing the documentation for Stack 2.7.5. The new theme has extensive online
documentation and features that the default theme lacked.

The Material for MkDocs theme is loaded in the `doc/requirements.txt` file:

    mkdocs-material

The theme is specified in the `mkdocs.yml` file:

~~~yaml
theme:
  name: material
  palette:
    primary: 'deep purple'
    accent: 'deep purple'
  logo: img/stack-logo-white.svg
  favicon: img/stack-favicon.svg
  features:
  - content.code.annotate
  - content.code.copy
  - content.code.select
  - content.tabs.link
  - navigation.indexes
  - navigation.tabs
  - navigation.top
~~~

Read the Docs requires [JQuery](https://jquery.com/) for its JavaScript code to
inject the flyout menu. Material for MkDocs does not come with JQuery. So, the
following is required in the `mkdocs.yml` file:

~~~yaml
extra_javascript:
- 'https://code.jquery.com/jquery-3.6.1.min.js'
~~~

The Read the Docs flyout is formatted with a `font-size` that is 90% of the
`body` `font-size`. Material for MkDocs has a `body` `font-size` that is
`0.5rem`, which is small. A little additional CSS is added to the `extra.css`
file, to force the final `font-size` to be `0.7rem`. That size is consistent
with that of other elements in the theme.

~~~css
body {
    font-size: 0.777778rem;
}
~~~

Material for MkDocs default suggestions for syntax highlighting in code blocks
are applied. They are specified in the `mkdocs.yml` file as:

~~~yaml
markdown_extensions:
- pymdownx.highlight:
    anchor_linenums: true
- pymdownx.inlinehilite
- pymdownx.snippets
- pymdownx.superfences
~~~

Other extensions to the basic Markdown syntax used include:

* Admonitions

    !!! info

        This is an example of an 'info' admonition.

* Content tabs, which can be nested

    !!! info

        Content tabs are used so that users of different operating systems, or
        different distributions of Linux, can be presented with content specific
        to their needs.

* icons and emojis

    !!! info

        The `octicons-tag-24` icon (:octicons-tag-24:) is used to refer to
        versions of Stack. The `material-cloud-download-outline` icon
        (:material-cloud-download-outline:) is used to signify a download link.
        The `octicons-beaker-24` icon (:octicons-beaker-24:) is used with
        'Experimental' to signify that a feature is experimental.

## Testing online documentation

Online documentation can be tested by establishing a branch on the repository
that is then configured on the Read the Docs web site as 'Active' but
'Hidden' - for example branch `mkdocs-test`. As the branch is 'Hidden' it does
not appear in the Read the Docs flyout or search results.

## get.haskellstack.org redirects

The https://get.haskellstack.org redirects are implemented with
[CloudFlare Pages](https://developers.cloudflare.com/pages/platform/redirects/)
and a `_redirects` file in the root of the
`commercialhaskell/get-haskellstack-org` GitHub
[repository](https://github.com/commercialhaskell/get-haskellstack-org).

Each redirect is defined as a line in the file with format:

~~~text
[source] [destination]
~~~

'Splats' are used in redirects. On matching, a splat (asterisk, `*`) will greedily match all characters and the matched value can be used in the redirect location with `:splat`.

For example, for Stack 2.9.1:

~~~text
/stable/* https://github.com/commercialhaskell/stack/releases/download/v2.9.1/stack-2.9.1-:splat
/upgrade/linux-x86_64-static.tar.gz https://github.com/commercialhaskell/stack/releases/download/v2.9.1/stack-2.9.1-linux-x86_64.tar.gz
/upgrade/* https://github.com/commercialhaskell/stack/releases/download/v2.9.1/stack-2.9.1-:splat
/ https://raw.githubusercontent.com/commercialhaskell/stack/stable/etc/scripts/get-stack.sh
~~~
