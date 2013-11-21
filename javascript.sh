#!/bin/bash

# npm install uglify-js -g

function create_script {
  cd $1
  args=()
  for i in `ls -p -S *.js | egrep -v $3`; do #sort hack to keep order
    echo $i
    args+=($(uglifyjs $i))
  done
  cd -
  echo $2
  echo ${args[@]} > $2
}

create_script apps/web/priv/static/n2o_bt/js apps/web/priv/static/js/bootstrap.min.js "/"
create_script apps/web/priv/static/n2o_bt/js apps/web/priv/static/js/reviews-bs.min.js \
"/|jquery.tinymce.min.js|jquery-scrolltofixed.js|bootstrap-upload.js|bootstrap-carousel.js|bootstrap-scrollspy.js|bootstrap-textboxlist.js|bootstrap-typeahead.js|bootstrap-tooltip.js|bootstrap-slider.js|bootstrap-select.js|bootstrap-popover.js|bootstrap-modal.js|bootstrap-alert.js|bootstrap-affix.js"
create_script apps/web/priv/static/n2o_bt/js apps/web/priv/static/js/index-bs.min.js \
"/|jquery.tinymce.min.js|jquery-scrolltofixed.js|bootstrap-upload.js|bootstrap-scrollspy.js|bootstrap-textboxlist.js|bootstrap-typeahead.js|bootstrap-tooltip.js|bootstrap-slider.js|bootstrap-select.js|bootstrap-popover.js|bootstrap-modal.js|bootstrap-alert.js|bootstrap-affix.js"
create_script apps/web/priv/static/n2o_bt/js apps/web/priv/static/js/login-bs.min.js \
"/|jquery.tinymce.min.js|jquery-scrolltofixed.js|bootstrap-upload.js|bootstrap-carousel.js|bootstrap-scrollspy.js|bootstrap-textboxlist.js|bootstrap-typeahead.js|bootstrap-tooltip.js|bootstrap-slider.js|bootstrap-select.js|bootstrap-popover.js|bootstrap-modal.js|bootstrap-affix.js"
create_script apps/web/priv/static/n2o_bt/js apps/web/priv/static/js/admin-bs.min.js \
"/|jquery-scrolltofixed.js|bootstrap-carousel.js|bootstrap-scrollspy.js|bootstrap-textboxlist.js|bootstrap-typeahead.js|bootstrap-tooltip.js|bootstrap-slider.js|bootstrap-select.js|bootstrap-popover.js|bootstrap-modal.js|bootstrap-affix.js"
create_script apps/web/priv/static/n2o_bt/js apps/web/priv/static/js/cart-bs.min.js \
"/|jquery.tinymce.min.js|jquery-scrolltofixed.js|bootstrap-upload.js|bootstrap-carousel.js|bootstrap-scrollspy.js|bootstrap-textboxlist.js|bootstrap-typeahead.js|bootstrap-tooltip.js|bootstrap-slider.js|bootstrap-select.js|bootstrap-popover.js|bootstrap-modal.js"

create_script apps/web/priv/static/n2o apps/web/priv/static/js/all.min.js "/|all.js|zepto.js"

echo $?
