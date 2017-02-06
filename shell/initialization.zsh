#!/bin/zsh

set -u

if [ $# -ne 2 ]; then
    echo "Argument Error:<file_name> <problem_num>">&2
    exit 1
fi

time_data_dir=time_data
file_name=$1:t
problem_num=$2

#問題を解くのにかかった時間を初期化。
#デフォルトでは120秒が制限時間。解いたことのない問題と解いたが時間内に
#答えが分からなかった問題を区別するため、121秒で初期化。
for i in {1..$problem_num}; do
    echo "121"
done > $time_data_dir/$file_name
