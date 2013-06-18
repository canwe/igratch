#!/bin/sh

APP=apps/web/priv/static/igrach
CSS=apps/web/priv/static/css

apps/web/priv/static/less/bin/lessc -x $APP/igrach.less > $CSS/igrach.css

