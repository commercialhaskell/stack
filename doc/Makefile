PYTHON_VIRTUALENV:=.python-doc-virtualenv
MK_DOCS_CMD:=$(PYTHON_VIRTUALENV)/bin/mkdocs

../$(PYTHON_VIRTUALENV)/bin/activate:
	python3 -m venv ../$(PYTHON_VIRTUALENV)
	(. ../$(PYTHON_VIRTUALENV)/bin/activate && pip install -r requirements.txt)

docs-serve: ../$(PYTHON_VIRTUALENV)/bin/activate
	cd .. && $(MK_DOCS_CMD) serve

docs-build: ../$(PYTHON_VIRTUALENV)/bin/activate
	cd .. && $(MK_DOCS_CMD) build
