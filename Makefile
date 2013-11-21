RELEASE := igratch
COOKIE  := node_runner
APPS    := web amqp_client avz cowboy erlydtl gproc kai kvs xmerl lager mimetypes mqs n2o oauth rabbit_common ranch active feed_server
VER     := 1.0.0
VM      := rels/web/files/vm.args
SYS     := rels/web/files/sys.config
ERL_ARGS     := -args_file $(VM) -config $(SYS)
RUN_DIR      ?= ./rels/web/devbox
LOG_DIR      ?= ./rels/web/devbox/logs
N2O          := deps/n2o/priv/static
FILES        := apps/web/priv/static/n2o
BOOTSTRAP    := apps/web/priv/static/bootstrap
FONTAWESOME  := apps/web/priv/static/font-awesome
FD_UI        := apps/web/priv/static/feed_ui
LESSJS       := apps/web/priv/static/less.js
MCE          := apps/web/priv/static/tinymce
N2O_BT       := apps/web/priv/static/n2o_bt


default: compile static-link
static-link:
	rm -rf $(N2O)
	rm -rf $(FILES)
	rm -rf $(BOOTSTRAP)
	rm -rf $(FONTAWESOME)
	rm -rf $(LESSJS)
	rm -rf $(MCE)
	rm -rf $(FD_UI)
	rm -rf $(N2O_BT)
	ln -sf ../../n2o_scripts $(N2O)
	mkdir -p $(shell dirname $(FILES))
	ln -sf ../../../../deps/n2o/priv/static/tinymce $(MCE)
	ln -sf ../../../../deps/feed_server/priv/static $(FD_UI)
	ln -sf ../../../../deps/n2o/priv/static/n2o $(FILES)
	ln -sf ../../../../deps/bootstrap $(BOOTSTRAP)
	ln -sf ../../../../deps/font-awesome $(FONTAWESOME)
	ln -sf ../../../../deps/less.js $(LESSJS)
	ln -sf ../../../../deps/n2o/priv/static/bootstrap $(N2O_BT)


include otp.mk
