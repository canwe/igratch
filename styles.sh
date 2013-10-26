#!/bin/sh

APP=apps/web/priv/static/igratch
CSS=apps/web/priv/static/css

/usr/bin/node apps/web/priv/static/less/bin/lessc -x $APP/igratch.less > $CSS/igratch.css
/usr/bin/node apps/web/priv/static/less/bin/lessc -x $APP/reviews.less > $CSS/reviews.css


