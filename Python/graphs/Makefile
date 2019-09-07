APP_NAME = ufsc-graphs

SRC = ufsc_graphs/graphs.py \
      ufsc_graphs/search.py \
      ufsc_graphs/cycle.py \
      ufsc_graphs/path.py

OBJ = ./build/ \
      ./dist/ \
      ./ufsc_graphs.egg-info/

default:
	make build

build: $(SRC)
	python3 setup.py sdist bdist_wheel

clean:
	- rm -R $(OBJ)
	- rm -R ./ufsc_graphs/__pycache__/

release:
	pip install --user .

rollback:
	pip uninstall $(APP_NAME)
