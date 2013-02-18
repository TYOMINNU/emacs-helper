#!/bin/bash

package_name=$1
version=$2

if [ "$package_name" = "" ]; then
    echo "Usage: buile-tar.sh \$PACKAGE_NAME  \$VERSION"
    exit 1
fi

if [ "$version" = "" ]; then
    echo "Usage: build-tar.sh \$PACKAGE_NAME \$VERSION"
    exit 1
fi

dir="${package_name}-${version}"

mkdir $dir

cp ${package_name}.el $dir
cp ${package_name}-pkg.el $dir

tar cf ${package_name}-${version}.tar $dir
