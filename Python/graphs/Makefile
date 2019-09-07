APP_NAME = ufsc-graphs

SRCDIR = ufsc_graphs
SRC = setup.py LICENSE README.md \
      $(SRCDIR)/__init__.py \
      $(SRCDIR)/graphs.py \
      $(SRCDIR)/search.py \
      $(SRCDIR)/cycle.py \
      $(SRCDIR)/path.py

OBJ = build/ \
      dist/ \
      ufsc_graphs.egg-info/

default:
	make build

build: $(SRC)
	python3 setup.py sdist bdist_wheel

clean:
	- rm -R $(OBJ)
	- rm -R $(SRCDIR)/__pycache__/

local:
	pip install --user .

uninstall:
	pip uninstall $(APP_NAME)

release: $(OBJ)
	python3 -m twine upload --repository-url https://test.pypi.org/legacy/ dist/*

install:
	python3 -m pip install --user --index-url https://test.pypi.org/simple/ --no-deps $(APP_NAME)
