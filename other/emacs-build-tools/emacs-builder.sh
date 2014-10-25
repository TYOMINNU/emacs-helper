#!/bin/bash
#
# auto build and install emacs

EMACS_BUILDTOOLS_DIR=$PWD
EMACS_SOURCE_DIR=/home/feng/project/emacs/
EMACS_BUILD_PREFIX=/usr/local/tumashu-emacs/

echo "Stage 1: Installing build depends"
sudo apt-get build-dep emacs

echo "Stage 2: Clean build directory and apply patchs"
cd $EMACS_SOURCE_DIR
sudo chown -R $USER:$USER .
git clean -xfd
git reset --hard

echo "Stage3: Installing emacs ..."
./autogen.sh
./configure --prefix=$EMACS_BUILD_PREFIX
make bootstrap
sudo make install

echo "Stage 4: Install fake emacs-snapshot package ..."
cd $EMACS_BUILDTOOLS_DIR
sudo dpkg -i emacs-snapshot_1.0_all.deb

echo "Done."
echo "Stage 5: Making symlinks ..."
latest=$(cd $EMACS_BUILD_PREFIX/share/emacs && echo 2[34].* | tr ' ' '\n' | sort \
					       | tail -n 1)
sudo ln -sfn $EMACS_BUILD_PREFIX/share/emacs/$latest /usr/share/emacs-snapshot
ln -sf $EMACS_BUILD_PREFIX/bin/emacs /usr/bin/emacs-snapshot
ln -sf $EMACS_BUILD_PREFIX/bin/emacsclient /usr/bin/emacsclient.emacs-snapshot

sudo update-alternatives --install /usr/bin/emacs emacs $EMACS_BUILD_PREFIX/bin/emacs 50
sudo update-alternatives --install /usr/bin/emacsclient emacsclient $EMACS_BUILD_PREFIX/bin/emacsclient 50

sudo mkdir -p /etc/emacs-snapshot/site-start.d

echo "Done."
echo "Stage 6: Installing emacs-snapshot flavor ..."
echo | sudo tee -a /var/lib/emacsen-common/installed-flavors
echo emacs-snapshot | sudo tee /var/lib/emacsen-common/installed-flavors
sudo /usr/lib/emacsen-common/emacs-install emacs-snapshot

echo "Done."