.PHONY: compile rel cover test typecheck doc ci start stop reset

REBAR=./rebar3
SHORTSHA=`git rev-parse --short HEAD`
PKG_NAME_VER=${SHORTSHA}

OS_NAME=$(shell uname -s)
PROFILE := dev
NETWORK := mainnet

ifeq (${OS_NAME},FreeBSD)
make="gmake"
else
MAKE="make"
endif

compile:
	$(REBAR) compile

shell:
	$(REBAR) shell

clean:
	$(REBAR) clean

cover:
	$(REBAR) cover

test:
	$(REBAR) as test do eunit,ct

ci:
	$(REBAR) do xref, dialyzer

typecheck:
	$(REBAR) dialyzer

doc:
	$(REBAR) edoc

release:
	$(REBAR) as $(PROFILE) do release

.PHONY: docs

start:
	./_build/$(PROFILE)/rel/blockchain_node/bin/blockchain_node start

foreground:
	./_build/$(PROFILE)/rel/blockchain_node/bin/blockchain_node foreground

stop:
	-./_build/$(PROFILE)/rel/blockchain_node/bin/blockchain_node stop

console:
	./_build/$(PROFILE)/rel/blockchain_node/bin/blockchain_node remote_console

docker-build:
	docker build . --build-arg network=$(NETWORK) -t blockchain-node:$(NETWORK)-local

docker-stop:
	docker stop node-${NETWORK}-local

docker-remove: docker-stop
	docker rm node-${NETWORK}-local

docker-run: docker-remove
	mkdir -p "/tmp/$(NETWORK)_node_data"
	docker run -it \
	  --name node-${NETWORK}-local \
	  --init \
	  --publish 4467:4467 \
	  --mount type=bind,source=/tmp/$(NETWORK)_node_data,target=/var/data \
	blockchain-node:$(NETWORK)-local

docs:
	$(MAKE) -C docs
