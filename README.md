Joos0 compiler
==============

CamlJoos0改

これは、CamlJoosをリファクタリングしてみたものです。

- map,foldを使用
    - 主要な操作対象の変数は一番後ろに移動し、map,fold等の関数を使う事で関数の数を減らしました。
- 不要コメントの削除
    - 型情報のみが書いてあるものは、リファクタリングしていて型が変わってしまったのとツールで吐けば良いので削除しました。
- タプルの使用
    - レコードは助長になりがちなのでタプルとパターンマッチを使う事で短くしました。
- 名前を単純化
    - `formal_and_body_decl`のような長い変数名はシンプルに `body` とする事にしました。
- モジュール名のエイリアスの削除
    - 名前を単純化したのでTAst等のエイリアスは削除しました。
- 
- 代数データ型の使用
    - コンストラクタ、メイン、メソッド、フィールドを一つのデータ型にまとめる
- データ定義ファイルの削除
    - 例えば、limitsastはlimitsに含める事でソースファイルを削減しました。

# ファイル構成

- main.ml メイン
    - error.ml ast.ml types.ml エラーと構文木と型
    - utils.ml ユーティリティ
    - parser.ml lexer.ml パーサと字句解析

    - コンパイラパス
        - env 型環境の作成
        - link リンク Ast.typeexpをTypes.typeexpに変換しつつ定義チェックをする。
        - typing 型チェック
        - constfold 定数畳み込み最適化
        - res リソース
        - codegen コード生成
        - limits リミットの計算
        - emit コード出力

- ツリー
    - 1 ast パース後
    - 2 link types適用
    - 3 typing 型チェック後
    - 4 res リソース適用後
    - 5 codegen コード生成後
    - 6 limits リミット適用後

6種類もあるのでソースが長い

さんざんプログラムを弄っているのだけど各パスについて詳しくは理解してません。


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
