set -gx WORKSPACE $HOME/Workspace
set -gx PROJECTS $HOME/Projects
set -gx DATA $HOME/Data

set -gx CAFFE_ROOT $PROJECTS/caffe
set -gx TORCH_ROOT $PROJECTS/torch
set -gx DIGITS_ROOT $PROJECTS/digits
set -gx TENSORFLOW_ROOT $PROJECTS/tensorflow
set -gx WARP_ROOT $PROJECTS/warp
set -gx DSSTNE_ROOT $PROJECTS/dsstne

function torch-activate
  set -gx LUA_PATH $HOME.luarocks/share/lua/5.1/?.lua $HOME/.luarocks/share/lua/5.1/?/init.lua $TORCH_ROOT/install/share/lua/5.1/?.lua $TORCH_ROOT/install/share/lua/5.1/?/init.lua ./?.lua $TORCH_ROOT/install/share/luajit-2.1.0-beta1/?.lua /usr/local/share/lua/5.1/?.lua /usr/local/share/lua/5.1/?/init.lua
  set -gx LUA_CPATH $HOME/.luarocks/lib/lua/5.1/?.so $TORCH_ROOT/install/lib/lua/5.1/?.so ./?.so /usr/local/lib/lua/5.1/?.so /usr/local/lib/lua/5.1/loadall.so
  set -gx PATH $TORCH_ROOT//install/bin $PATH
  set -gx LD_LIBRARY_PATH $TORCH_ROOT//install/lib $LD_LIBRARY_PATH
  set -gx DYLD_LIBRARY_PATH $TORCH_ROOT//install/lib $DYLD_LIBRARY_PATH
  set -gx LUA_CPATH $TORCH_ROOT/install/lib/?.so $LUA_CPATH
end 
