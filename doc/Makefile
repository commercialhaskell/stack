# With the necessary prerequisites, this file allows users of the 'make' tool to
# automate the use of the 'mkdocs' tool to preview or build Stack's online
# documentation. See CONTRIBUTING.md and the file 'Makefile' in Stack's project
# directory for more information.

PYTHON_VIRTUALENV:=.python-doc-virtualenv
MK_DOCS_CMD:=$(PYTHON_VIRTUALENV)/bin/mkdocs

../$(PYTHON_VIRTUALENV)/bin/activate:

# Note, the python3 command is not used with Python on Windows
	python3 -m venv ../$(PYTHON_VIRTUALENV)

# Note, in the MSYS2 environment, the development versions of the libxml2 and
# libxslt packages are also required but not installed by pip
	(. ../$(PYTHON_VIRTUALENV)/bin/activate && pip install -r requirements.txt)

docs-serve: ../$(PYTHON_VIRTUALENV)/bin/activate
	cd .. && $(MK_DOCS_CMD) serve

docs-build: ../$(PYTHON_VIRTUALENV)/bin/activate
	cd .. && $(MK_DOCS_CMD) build
