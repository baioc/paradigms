#Esse makefile só serve para fazer o arquivo a ser enviado pro moodle

.PHONY: all package verify compile submission clean

MVN=mvn
ifeq ($(wildcard .mvn mvnw), .mvn mvnw)
	MVN=./mvnw
endif

all: package

package:
	$(MVN) -DskipTests=true package
verify:
	$(MVN) verify
compile:
	$(MVN) compile

# Prepara .tar.gz pra submissão no moodle
# Note que antes de preparar o tar.gz, é feito um clean
submission: clean
	$(MVN) verify || true
	SUBNAME=$$(basename "$$(pwd)"); \
		cd ..; \
		rm -fr "$$SUBNAME.tar.gz"; \
		tar zcf "$$SUBNAME.tar.gz" "$$SUBNAME"

# Limpa binários
clean:
	rm -fr target
