Joos0 compiler
==============

Joos0 コンパイラは、Java(tm)プログラミング言語の小さなサブセットのシンプルなコンパイラです。
Joos0言語では Java 1.3 の小さなサブセットをサポートしています。
より詳しい言語の詳細や概要は [https://services.brics.dk/java/courseadmin/dOvs2012/pages/The+Joos+Languages] を参照してください.

コンパイラはJavaのバイトコードを Jasmin 形式で出力します。
出力された.jファイルを.classファイルに変換するには、Jasmin アセンブラをインストールする必要があります。
Jasminのより詳しい情報は、 http://jasmin.sourceforge.net/ を見てください。

コンパイラをビルドするには、 Menhirパーサジェネレータをインストールする必要があります。http://gallium.inria.fr/~fpottier/menhir/ を参照してください。

このコンパイラ(の前のバージョン)はundergraduate compiler course at Aarhus Universityの一環で開発されました。
このバージョンは およそ4000行のコードを2600行に大幅にリファクタリングしたものです。

Makefile内のテストではJasmin形式のアセンブラにOCaml製のjascアセンブラを使用しています。

ファイル構成
---------

camljoos0には15のソースファイルがあります。
1メインと1つの補助プログラム、3データ定義ファイル、9コンパイラパス(10ファイル)で構成されています。

- 1 main.ml メインルーチン
- 2 error.ml 3 ast.ml 4 types.ml 5 inst.ml エラーと構文木と型、バイトコード
- コンパイラパス
    - 6 parser.ml 7 lexer.ml パーサと字句解析
    - 8 env.ml Astをトラバースして型環境を作成
    - 9 link.ml リンク Astの型から位置情報を取り払う変換をしつつ定義チェック
    - 10 typing.ml 型チェック
    - 11 constfold.ml 定数畳み込み最適化
    - 12 res.ml ローカル変数の番号を付けて、シグニチャをフィールドやメソッド、クラス等に付ける
    - 13 codegen.ml コード生成 ネストした式と文をバイトコードに変換
    - 14 limits.ml スタックの最大値とローカル変数の最大値の計算
    - 15 emit.ml コード出力

構文木データの形式は以下の6種類です。

- 構文木データ
    - 1 ast パース後
    - 2 link types適用
    - 3 typing 型チェック後
    - 4 res リソース適用後
    - 5 codegen コード生成後
    - 6 limits リミット適用後

リファクタリングの詳細
-----------------

- データ定義ファイルの削除
    - 例えば、limitsastはlimitsに含める事でソースファイルを5ファイル削除しました。
- map,foldを使用
    - 主要な操作対象の変数は一番後ろに移動し、map,fold等の関数を使う事で関数の数を減らし、引数を省略しました。
- コメントの削除
    - 型情報のみが書いてあるものは、リファクタリングしていて型が変わってしまったのとツールで出力出来るので削除しました。
- タプルの使用
    - レコードは助長になりがちなのでタプルとパターンマッチを使う事で短くしました。
- 名前を単純化
    - `formal_and_body_decl`のような長い変数名はシンプルに `body` しました。
        通常のアプリケーションでは将来どのようにプログラムを拡張するかの予測は出来ない事が多いため、
        出来るだけ長い名前を最初から使う事で名前のバッティングを防止します。
        しかしコンパイラの作成において拡張は限定的なため短い名前で短く書く事でアルゴリズムに集中しやすくなります。
        短い名前にすれば、関数呼び出しをネストして書く事が可能になり不要な変数を作らずに済む事が増えるため、
        よりプログラムを短く書く事が出来るのです。
        行数が減れば、視界に入る情報量が増えてアルゴリズムの全体像を把握しやすくなるのです。
- モジュール名のエイリアスの削除
    - 名前を単純化したのでTAst等のエイリアスは削除しました。
- 代数データ型の使用
    - コンストラクタ、メイン、メソッド、フィールドを一つのデータ型にまとめました。
        まとめた事で順不同でフィールドを書く事が出来るようになりました。
        複数に別れていた型や関数や処理が１つにまとまりプログラムも短くなります。

-------

This is a simple compiler for a small subset of the Java(tm)
programming language. The supported language, Joos0, is the smallest
of a hierarchy of Java 1.3 subsets. For more language details and an
overview of the hierarchy, see
[https://services.brics.dk/java/courseadmin/dOvs2012/pages/The+Joos+Languages].

The compiler emits Java bytecode in the Jasmin format. You'll need to
install the Jasmin assembler to turn the emitted .j files into .class
files. For more information on Jasmin, see http://jasmin.sourceforge.net/.

To build the compiler you'll also need to install the Menhir parser
generator, see http://gallium.inria.fr/~fpottier/menhir/.

The compiler was developed as part of (a previous version of) the
undergraduate compiler course at Aarhus University.


To build:
---------

    $ make


To run:
-------

Example 1:

    $ ./joos0 tests/IntegerToString.java 
    [...]
    $ jasmin tests/IntegerToString.j
    $ java -classpath . IntegerToString
    Enter a number: 42
    In base 8 that is: 52

(hence the program tests/IntegerToString.java compiles with the Joos0
compiler, the output can be assembled with Jasmin, and the resulting
output run on the JVM)



Example 2:

    $ ./joos0 tests/Cons.java tests/ConsMain.java
    [...]
    $ jasmin tests/Cons.j tests/ConsMain.j
    $ java -classpath . ConsMain
    truefalse

(hence the compiler supports programs that span multiple files)



Example 3:

    $ ./joos0 tests/ClassName2.java 
    [...]
    Error at tests/ClassName2.java: line 3, col 11
        public A() throws Exception { }
    
    Constructor must have the same name as its enclosing class

(hence the program is properly rejected by the compiler with an error message)

TODO:

- [ ] コンストラクタ複数定義時エラーチェック
