# Present Tsumeshogi Effectively
数秒で解けてしまう問題をたくさん解くよりも、解くのに多少時間がかかる問題を解くことのほうが苦手な詰み筋の練習となると考えられます。また、時間がかかる問題に取り組んだほうが、詰将棋等の本を1周するための時間の削減につながります。(80:20の法則)

## 使い方
`shell/initialization.zsh` <生成ファイル名>  <問題数>

新しく時間を記録するためのファイルを生成します。例えば、200問の詰将棋用のファイルを用意したい場合は、`shell/initialization.zsh tsume_book.txt 200`とします。この結果、`time_data/tsume_book.txt`が生成されます。`tsume_book.txt`の部分は、任意の名前で構いません。

`shell/compile.sh`

ソースファイルをコンパイルし、`exe/main`という実行ファイルを生成します。

`./exe/main <ファイル名> <モード>`

`<ファイル名>`の記録データを読み込んでプログラムを実行します。指定した詰将棋の問題のうち、見開き(4ページ)単位で最も時間がかかっている問題を抽出し、問題番号を表示します。エンターキーを押してGoと表示されてから、再びエンターキーを押すまでの時間が計測され、その問題を解いた時間として記録されます。

`<モード>`は`single`か`plural`を指定する。`single`は1問単位、`plural`は見開きページ単位で出題する。