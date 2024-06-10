# With the other prerequisites, this file allows users of the 'make' tool to
# automate the use of the 'mkdocs' tool to preview or build Stack's online
# documentation. See CONTRIBUTING.md for more information.

# Preview Stack's online documentation with `make docs-serve`:
.PHONY: docs-serve
docs-serve:
	$(MAKE) -C doc docs-serve

# Build Stack's online documentation with `make _site/index.html`:
_site/index.html: doc/*.md
	$(MAKE) -C doc docs-build
