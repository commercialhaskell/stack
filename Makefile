.PHONY: docs-serve
docs-serve:
	$(MAKE) -C doc docs-serve

_site/index.html: doc/*.md
	$(MAKE) -C doc docs-build