> Translated from [English original on 17.6.2020](https://github.com/SAP/styleguides/tree/012d2e8bdc19de321ed51c1a2310dd07e4f87de3).
> Latest version [in English](CleanABAP.md).

# クリーン ABAP

> [**日本語**](CleanABAP_ja.md)
> &nbsp;·&nbsp;
> [English](CleanABAP.md)
> &nbsp;·&nbsp;
> [中文](CleanABAP_zh.md)
> &nbsp;·&nbsp;
> [Français](CleanABAP_fr.md)
> &nbsp;·&nbsp;
> [Deutsch](CleanABAP_de.md)

このガイドは [ABAP](https://en.wikipedia.org/wiki/ABAP) 向けに [Robert C. Martin の _Clean Code_] を採用したものです。

[チートシート](cheat-sheet/CheatSheet.md) は印刷に最適化されたものです。

[robert c. martin の _clean code_]: https://www.oreilly.com/library/view/clean-code/9780136083238/

## 目次

- [やり方](#やり方)
  - [クリーンコードを始めるには](#クリーンコードを始めるには)
  - [レガシーコードをリファクタするには](#レガシーコードをリファクタするには)
  - [自動的にチェックするには](#自動的にチェックするには)
  - [他のガイドとの関係性](#他のガイドとの関係性)
  - [反対するには](#反対するには)
- [命名](#命名)
  - [意味のある名前を使う](#意味のある名前を使う)
  - [ソリューションドメインと問題ドメインの用語を選ぶ](#ソリューションドメインと問題ドメインの用語を選ぶ)
  - [複数形を使う](#複数形を使う)
  - [発音可能な名前を使う](#発音可能な名前を使う)
  - [略語を避ける](#略語を避ける)
  - [どこでも同じ略語を使う](#どこでも同じ略語を使う)
  - [クラスには名詞を、メソッドには動詞を使う](#クラスには名詞を、メソッドには動詞を使う)
  - ["data", "info", "object" などの意味のない言葉を避ける](#data-info-object-などの意味のない言葉を避ける)
  - [1つの概念には1つの言葉を使う](#1つの概念には1つの言葉を使う)
  - [パターン名はそれを意図する場合にのみ使う](#パターン名はそれを意図する場合にのみ使う)
  - [エンコーディング, 特にハンガリアン記法と接頭辞を避ける](#エンコーディング-特にハンガリアン記法と接頭辞を避ける)
- [言語](#言語)
  - [古いABAPリリースに注意する](#古いABAPリリースに注意する)
  - [パフォーマンスに注意する](#パフォーマンスに注意する)
  - [手続き型プログラミングよりもオブジェクト指向を選ぶ](#手続き型プログラミングよりもオブジェクト指向を選ぶ)
  - [手続き型の言語構造よりも関数型の言語構造を選ぶ](#手続き型の言語構造よりも関数型の言語構造を選ぶ)
  - [廃止された言語要素を避ける](#廃止された言語要素を避ける)
  - [デザインパターンを賢く使う](#デザインパターンを賢く使う)
- [定数](#定数)
  - [マジックナンバーの代わりに定数を使う](#マジックナンバーの代わりに定数を使う)
  - [定数インタフェースよりも列挙クラスを選ぶ](#定数インタフェースよりも列挙クラスを選ぶ)
  - [列挙クラスを使用しない場合は定数をグループ化する](#列挙クラスを使用しない場合は定数をグループ化する)
- [変数](#変数)
  - [事前宣言よりもインライン宣言を選ぶ](#事前宣言よりもインライン宣言を選ぶ)
  - [選択の分岐内でインライン宣言をしない](#選択の分岐内でインライン宣言をしない)
  - [事前宣言を連結させない](#事前宣言を連結させない)
  - [FIELD-SYMBOLよりもREF TOを選択する](#FIELD-SYMBOLよりもREF-TOを選択する)
- [テーブル](#テーブル)
  - [正しいテーブルデータ型を使う](#正しいテーブルデータ型を使う)
  - [DEFAULT KEY を避ける](#DEFAULT-KEY-を避ける)
  - [APPEND TO よりも INSERT INTO TABLE を選ぶ](#APPEND-TO-よりも-INSERT-INTO-TABLE-を選ぶ)
  - [READ TABLE や LOOP AT よりも LINE_EXISTS を選ぶ](#READ-TABLE-や-LOOP-AT-よりも-LINE_EXISTS-を選ぶ)
  - [LOOP AT よりも READ TABLE を選ぶ](#LOOP-AT-よりも-READ-TABLE-を選ぶ)
  - [ネストした IF よりも LOOP AT WHERE を選ぶ](#ネストした-IF-よりも-LOOP-AT-WHERE-を選ぶ)
  - [不要なテーブルの読み取りを避ける](#不要なテーブルの読み取りを避ける)
- [Strings](#strings)
  - [リテラルを定義するには ` を使う](#リテラルを定義するには--を使う)
  - [テキストを組み立てるには | を使う](#テキストを組み立てるには--を使う)
- [ブーリアン](#ブーリアン)
  - [ブーリアン型を賢く使う](#ブーリアン型を賢く使う)
  - [ブーリアン型にはABAP_BOOLを使う](#ブーリアン型にはABAP_BOOLを使う)
  - [比較にはABAP_TRUEとABAP_FALSEを使う](#比較にはABAP_TRUEとABAP_FALSEを使う)
  - [ブーリアン変数をセットするにはXSDBOOLを使う](#ブーリアン変数をセットするにはXSDBOOLを使う)
- [条件](#条件)
  - [条件を肯定にしてみる](#条件を肯定にしてみる)
  - [NOT ISよりもIS NOTを選ぶ](#NOT-ISよりもIS-NOTを選ぶ)
  - [複素条件を分解することを考える](#複素条件を分解することを考える)
  - [複雑な条件を抽出することを考える](#複雑な条件を抽出することを考える)
- [If](#if)
  - [空のIF分岐を作らない](#空のIF分岐を作らない)
  - [複数の択一条件にはELSE IFよりもCASEを選ぶ](#複数の択一条件にはELSE-IFよりもCASEを選ぶ)
  - [ネストの深さを浅くする](#ネストの深さを浅くする)
- [正規表現](#正規表現)
  - [正規表現よりもシンプルなメソッドを選ぶ](#正規表現よりもシンプルなメソッドを選ぶ)
  - [正規表現よりも基本的なチェックを選ぶ](#正規表現よりも基本的なチェックを選ぶ)
  - [複雑な正規表現は組み立てることを考える](#複雑な正規表現は組み立てることを考える)
- [クラス](#クラス)
  - [クラス: オブジェクト指向](#クラス-オブジェクト指向)
    - [静的クラスよりもオブジェクトを選ぶ](#静的クラスよりもオブジェクトを選ぶ)
    - [継承よりもコンポジションを選ぶ](#継承よりもコンポジションを選ぶ)
    - [同じクラスにステートフルとステートレスを混在させない](#同じクラスにステートフルとステートレスを混在させない)
  - [スコープ](#スコープ)
    - [デフォルトではグローバル, 適切な場所でのみローカル](#デフォルトではグローバル-適切な場所でのみローカル)
    - [継承を意図しない場合はFINALにする](#継承を意図しない場合はFINALにする)
    - [メンバーはデフォルトでPRIVATE, 必要な場合にのみPROTECTEDにする](#メンバーはデフォルトでPRIVATE-必要な場合にのみPROTECTEDにする)
    - [getter の代わりにイミュータブルを使用することを考える](#getter-の代わりにイミュータブルを使用することを考える)
    - [READ-ONLY を控えめに使う](#READ-ONLY-を控えめに使う)
  - [コンストラクタ](#コンストラクタ)
    - [CREATE OBJECT よりも NEW を選ぶ](#CREATE-OBJECT-よりも-NEW-を選ぶ)
    - [グローバルクラスが CREATE PRIVATE の場合 CONSTRUCTOR は public のままにする](#グローバルクラスが-CREATE-PRIVATE-の場合-CONSTRUCTOR-は-public-のままにする)
    - [オプションパラメータよりも複数の静的な生成用メソッドを選ぶ](#オプションパラメータよりも複数の静的な生成用メソッドを選ぶ)
    - [複数の生成用メソッドには記述的な名前をつける](#複数の生成用メソッドには記述的な名前をつける)
    - [複数のインスタンスが意味をなさない場合にのみシングルトンにする](#複数のインスタンスが意味をなさない場合にのみシングルトンにする)
- [メソッド](#メソッド)
  - [呼び出し](#呼び出し)
    - [手続き的な呼び出しよりも関数的な呼び出しを選ぶ](#手続き的な呼び出しよりも関数的な呼び出しを選ぶ)
    - [RECEIVING を省略する](#RECEIVING-を省略する)
    - [オプションのキーワード EXPORTING を省略する](#オプションのキーワード-EXPORTING-を省略する)
    - [単一パラメータでの呼び出し時はパラメータ名を省略する](#単一パラメータでの呼び出し時はパラメータ名を省略する)
    - [インスタンスメソッドを呼び出す際の自己参照 me を省略する](#インスタンスメソッドを呼び出す際の自己参照-me-を省略する)
  - [メソッド: オブジェクト指向](#メソッド-オブジェクト指向)
    - [静的メソッドよりもインスタンスメソッドを選ぶ](#静的メソッドよりもインスタンスメソッドを選ぶ)
    - [パブリックインスタンスメソッドはインタフェースの一部でなければならない](#パブリックインスタンスメソッドはインタフェースの一部でなければならない)
  - [パラメータ数](#パラメータ数)
    - [IMPORTINGパラメータは少なく, 3つ以下を目指す](#IMPORTINGパラメータは少なく-3つ以下を目指す)
    - [OPTIONALパラメータを追加するのではなくメソッドを分割する](#OPTIONALパラメータを追加するのではなくメソッドを分割する)
    - [PREFERRED PARAMETER は控えめに使う](#PREFERRED-PARAMETER-は控えめに使う)
    - [RETURN, EXPORT, CHANGE は1つだけのパタメータにする](#RETURN-EXPORT-CHANGE-は1つだけのパタメータにする)
  - [パラメータの型](#パラメータの型)
    - [EXPORTING よりも RETURNING を選ぶ](#EXPORTING-よりも-RETURNING-を選ぶ)
    - [大きなテーブルの RETURNING は通常OK](#大きなテーブルの-RETURNING-は通常OK)
    - [RETURNING, EXPORTING, CHANGING は併用せずにどれかを使う](#RETURNING-EXPORTING-CHANGING-は併用せずにどれかを使う)
    - [CHANGING は適切なところで控えめに使う](#CHANGING-は適切なところで控えめに使う)
    - [ブーリアン型の入力パラメータの代わりにメソッドを分割する](#ブーリアン型の入力パラメータの代わりにメソッドを分割する)
  - [パラメータ名](#パラメータ名)
    - [RETURNING パラメータに RESULT と名付けることを考える](#RETURNING-パラメータに-RESULT-と名付けることを考える)
  - [パラメータの初期化](#パラメータの初期化)
    - [EXPORTING 参照パラメータはクリアするか上書きする](#EXPORTING-参照パラメータはクリアするか上書きする)
      - [入力と出力が同一になる場合に注意する](#入力と出力が同一になる場合に注意する)
    - [VALUE パラメータをクリアしない](#VALUE-パラメータをクリアしない)
  - [メソッドボディ](#メソッドボディ)
    - [1つのことだけをうまくやる](#1つのことだけをうまくやる)
    - [正常系かエラー処理に集中する, 両方ではなく](#正常系かエラー処理に集中する-両方ではなく)
    - [抽象度を1段下げる](#抽象度を1段下げる)
    - [メソッドを小さく保つ](#メソッドを小さく保つ)
  - [制御フロー](#制御フロー)
    - [フェイルファースト](#フェイルファースト)
    - [CHECK vs. RETURN](#check-vs-return)
    - [他の場所での CHECK は避ける](#他の場所での-CHECK-は避ける)
- [エラー処理](#エラー処理)
  - [メッセージ](#メッセージ)
    - [メッセージを見つけやすくする](#メッセージを見つけやすくする)
  - [リターンコード](#リターンコード)
    - [リターンコードよりも例外を選ぶ](#リターンコードよりも例外を選ぶ)
    - [エラーを取り逃がさない](#エラーを取り逃がさない)
  - [例外](#例外)
    - [例外はエラーのために使い, 通常のケースでは使用しない](#例外はエラーのために使い-通常のケースでは使用しない)
    - [クラスベースの例外を使う](#クラスベースの例外を使う)
  - [スロー](#スロー)
    - [独自のスーパークラスを使う](#独自のスーパークラスを使う)
    - [例外は1つの型のみをスローする](#例外は1つの型のみをスローする)
    - [呼び出し元がエラー状況を区別できるようにするためにサブクラスを使う](#呼び出し元がエラー状況を区別できるようにするためにサブクラスを使う)
    - [管理可能な例外のために CX_STATIC_CHECK をスローする](#管理可能な例外のために-CX_STATIC_CHECK-をスローする)
    - [通常は回復不可能な場合に CX_NO_CHECK をスローする](#通常は回復不可能な場合に-CX_NO_CHECK-をスローする)
    - [回避可能な例外には CX_DYNAMIC_CHECK を検討する](#回避可能な例外には-CX_DYNAMIC_CHECK-を検討する)
    - [まったく回復不可能な状況ではダンプする](#まったく回復不可能な状況ではダンプする)
    - [RAISE EXCEPTION TYPE よりも RAISE EXCEPTION NEW を選ぶ](#RAISE-EXCEPTION-TYPE-よりも-RAISE-EXCEPTION-NEW-を選ぶ)
  - [キャッチ](#キャッチ)
    - [外部の例外はコードに侵食しないようラップする](#外部の例外はコードに侵食しないようラップする)
- [コメント](#コメント)
  - [コメントではなくコードで表現する](#コメントではなくコードで表現する)
  - [不適切な命名をコメントで補おうとしない](#不適切な命名をコメントで補おうとしない)
  - [コードをセグメント化するのにコメントではなくメソッドを使用する](#コードをセグメント化するのにコメントではなくメソッドを使用する)
  - [何をではなく, なぜを説明するためにコメントを書く](#何をではなく-なぜを説明するためにコメントを書く)
  - [設計はコードではなく設計ドキュメントに書く](#設計はコードではなく設計ドキュメントに書く)
  - [コメントには * ではなく " を使う](#コメントには--ではなく--を使う)
  - [関連する文の前にコメントを置く](#関連する文の前にコメントを置く)
  - [コメントアウトではなくコードを削除する](#コメントアウトではなくコードを削除する)
  - [FIXME, TODO, XXXを使い, 自分のIDを追加する](#FIXME-TODO-XXXを使い-自分のIDを追加する)
  - [メソッドシグネチャや末尾にコメントを書かない](#メソッドシグネチャや末尾にコメントを書かない)
  - [コメントにメッセージテキストを書かない](#コメントにメッセージテキストを書かない)
  - [ABAP Doc は公開 API のみに書く](#ABAP-Doc-は公開-API-のみに書く)
  - [疑似コメントよりもプラグマを選ぶ](#疑似コメントよりもプラグマを選ぶ)
- [フォーマット](#フォーマット)
  - [一貫性を保つ](#一貫性を保つ)
  - [書くためではなく読むために最適化する](#書くためではなく読むために最適化する)
  - [有効化する前にプリティプリントを使う](#有効化する前にプリティプリントを使う)
  - [プリティプリントのチーム設定を使う](#プリティプリントのチーム設定を使う)
  - [1行につき, 最大1つのステートメント](#1行につき-最大1つのステートメント)
  - [適度な行の長さを守る](#適度な行の長さを守る)
  - [コードを凝縮する](#コードを凝縮する)
  - [区切るための空白行は1行のみとする](#区切るための空白行は1行のみとする)
  - [空白行で区切ることにこだわらない](#空白行で区切ることにこだわらない)
  - [同じオブジェクトへの代入時は位置を揃えるが, 別のオブジェクトの場合はしない](#同じオブジェクトへの代入時は位置を揃えるが-別のオブジェクトの場合はしない)
  - [行末でカッコを閉じる](#行末でカッコを閉じる)
  - [単一パラメータでの呼び出しは1行で書く](#単一パラメータでの呼び出しは1行で書く)
  - [呼び出しの後にパラメータを置く](#呼び出しの後にパラメータを置く)
  - [改行する場合, 呼び出しの下のパラメータをインデントする](#改行する場合-呼び出しの下のパラメータをインデントする)
  - [複数のパラメータを改行する](#複数のパラメータを改行する)
  - [パラメータを整列させる](#パラメータを整列させる)
  - [行が長くなりすぎる場合, 呼び出しを改行する](#行が長くなりすぎる場合-呼び出しを改行する)
  - [インデントしてタブにスナップする](#インデントしてタブにスナップする)
  - [インライン宣言はメソッド呼び出しのようにインデントする](#インライン宣言はメソッド呼び出しのようにインデントする)
  - [型句の位置を揃えない](#型句の位置を揃えない)
- [テスト](#テスト)
  - [原則](#原則)
    - [テスタブルなコードを書く](#テスタブルなコードを書く)
    - [他の人がモックできるようにする](#他の人がモックできるようにする)
    - [可読性のルール](#可読性のルール)
    - [コピーを作成したり, テストレポートを書いたりしない](#コピーを作成したり-テストレポートを書いたりしない)
    - [内部の private ではなく, public をテストする](#内部の-private-ではなく-public-をテストする)
    - [カバレッジにこだわらない](#カバレッジにこだわらない)
  - [テストクラス](#テストクラス)
    - [ローカルテストクラスは目的に応じて命名する](#ローカルテストクラスは目的に応じて命名する)
    - [テストはローカルクラスに置く](#テストはローカルクラスに置く)
    - [ヘルプメソッドはヘルプクラスに置く](#ヘルプメソッドはヘルプクラスに置く)
    - [テストクラスの実行方法](#テストクラスの実行方法)
  - [テスト対象コード](#テスト対象コード)
    - [テスト対象コードに意味のある名前を付けるか, デフォルトを CUT にする](#テスト対象コードに意味のある名前を付けるか-デフォルトを-CUT-にする)
    - [実装ではなくインタフェースに対してテストする](#実装ではなくインタフェースに対してテストする)
    - [テスト対象コードの呼び出しをメソッドに抽出する](#テスト対象コードの呼び出しをメソッドに抽出する)
  - [インジェクション](#インジェクション)
    - [依存関係を逆転させてテストダブルをインジェクトする](#依存関係を逆転させてテストダブルをインジェクトする)
    - [ABAP test double ツールの利用を検討する](#ABAP-test-double-ツールの利用を検討する)
    - [テストツールを利用する](#テストツールを利用する)
    - [test seam は一時的な回避策として使用する](#test-seam-は一時的な回避策として使用する)
    - [LOCAL FRIENDS を使用して依存関係逆転コンストラクタへアクセスする](#LOCAL-FRIENDS-を使用して依存関係逆転コンストラクタへアクセスする)
    - [LOCAL FRIENDS を悪用してテスト対象コードに侵入しない](#LOCAL-FRIENDS-を悪用してテスト対象コードに侵入しない)
    - [テスト可能にするために製品コードを変更しない](#テスト可能にするために製品コードを変更しない)
    - [メソッドをモックするためにサブクラスにしない](#メソッドをモックするためにサブクラスにしない)
    - [不要なものをモックしない](#不要なものをモックしない)
    - [テストフレームワークを作らない](#テストフレームワークを作らない)
  - [テストメソッド](#テストメソッド)
    - [テストメソッド名: 前提条件と期待する結果を反映する](#テストメソッド名-前提条件と期待する結果を反映する)
    - [given-when-then を使用する](#given-when-then-を使用する)
    - [「when」ではちょうど1回呼び出す](#whenではちょうど1回呼び出す)
    - [本当に必要な時以外は TEARDOWN を追加しない](#本当に必要な時以外は-TEARDOWN-を追加しない)
  - [テストデータ](#テストデータ)
    - [意味を見つけやすくする](#意味を見つけやすくする)
    - [違いを見つけやすくする](#違いを見つけやすくする)
    - [定数を使ってテストデータの目的と重要性を説明する](#定数を使ってテストデータの目的と重要性を説明する)
  - [アサーション](#アサーション)
    - [少なく, 焦点を絞ったアサーション](#少なく-焦点を絞ったアサーション)
    - [正しいアサートタイプを使用する](#正しいアサートタイプを使用する)
    - [量ではなく, 内容をアサートする](#量ではなく-内容をアサートする)
    - [内容ではなく, 特性でアサートする](#内容ではなく-特性でアサートする)
    - [期待する例外をチェックするために FAIL を使う](#期待する例外をチェックするために-FAIL-を使う)
    - [予期せぬ例外はキャッチして失敗させるのではなく, 伝播させる](#予期せぬ例外はキャッチして失敗させるのではなく-伝播させる)
    - [コードを短くして, 重複を避けるためにカスタムアサートを書く](#コードを短くして-重複を避けるためにカスタムアサートを書く)

## やり方

> [クリーン ABAP](#クリーン-abap) > [目次](#目次) > [本節](#やり方)

### クリーンコードを始めるには

> [クリーン ABAP](#クリーン-abap) > [目次](#目次) > [やり方](#やり方) > [本節](#クリーンコードを始めるには)

Clean Code を初めて利用する場合は、まず、[Robert C. Martin の _Clean Code_] を読んでください。
[Clean Code Developer initiative](https://clean-code-developer.com/) は一般的にトピックを段階的にスムーズに導入し始めるのに役立つでしょう。

[ブーリアン](#ブーリアン) や [Conditions](#conditions) 、 [Ifs](#ifs) のようにわかりやすく、広く受け入れられるものから始めることをお勧めします。

おそらく [メソッド](#メソッド) 、特に [1つのことだけをうまくやる](#1つのことだけをうまくやる) と [メソッドを小さく保つ](#メソッドを小さく保つ) の節がもっとも有益でしょう。なぜなら、これらはコードの全体的な構造を大幅に改善するからです。

ここに書かれているトピックの中には、経験は豊富だがクリーンコードに慣れていないチームでは難しい議論を引き起こす可能性があるものもあります。これらのトピックは完全に「健全」ですが、最初は慣れるのが難しいかもしれません。

特に [コメント](#コメント)、 [命名](#命名)、 [フォーマット](#フォーマット) は、宗教的な論争に発展する可能性があるので、クリーンコードの効果をすでに経験しているチームにのみ適用してください。

### レガシーコードをリファクタするには

> [クリーン ABAP](#クリーン-abap) > [目次](#目次) > [やり方](#やり方) > [本節](#レガシーコードをリファクタするには)

[ブーリアン](#ブーリアン)、[Conditions](#conditions)、[Ifs](#ifs)、[メソッド](#メソッド) は、コンフリクトなしに新しいコードを適用できるため、変更できない、または変更したくないコードが大量にあるレガシープロジェクトで作業している場合に、最も有益なトピックです。

[命名](#命名) は、古いコードと新しいコードの間に [エンコーディング, 特にハンガリアン記法と接頭辞を避ける](#エンコーディング-特にハンガリアン記法と接頭辞を避ける) のような節を無視した方がよいほどまでの断絶を引き起こす可能性があるため、レガシープロジェクトには非常に厳しいトピックです。

リファクタリングを行う際には、同じ開発オブジェクト内で異なる開発スタイルを混在させないようにしてください。レガシーコードに事前宣言しか含まれておらず、インライン宣言を使用するように完全にリファクタリングすることが不可能な場合、2 つのスタイルを混合するよりもレガシースタイルを続ける方が良いでしょう。スタイルを混合することで混乱を招く同様の状況がいくつかあります。例えば、

- ループ時に `REF TO` と `FIELD-SYMBOL` を混在させる
- `CONSTRUCTOR` 呼び出し時に `NEW` と `CREATE OBJECT` を混在させる
- 1 つのパラメータしか返さないメソッドのシグニチャで `RETURNING` と `EXPORTING` を混在させる

リファクタリングの 4 段階の計画で良好な結果が得られました:

1. チームに参加してもらう。新しいスタイルを伝えて説明し、プロジェクトチームの全員に同意してもらいましょう。一度にすべてのガイドラインにコミットする必要はなく、議論の余地のない小さなサブセットから始めて、そこから進化させてください。

2. ボーイスカウトのルールに従ってください：編集したコードは、常に見つけたときよりも少しきれいな状態にしておきましょう。「キャンプ場の掃除」に何時間もかけて執着するのではなく、数分余分に費やして、時間の経過とともに改善がどのように蓄積されていくかを観察してください。

3. クリーンな島を作る: 時々、小さなオブジェクトやコンポーネントを選んで、あらゆる面でクリーンにします。これらの島は、あなたがやっていることの利点を示し、さらなるリファクタリングのためのしっかりとテストされたホームベースを形成します。

4. それについて話してください。古風な[Fagan コードレビュー](https://en.wikipedia.org/wiki/Fagan_inspection)を設定したり、情報セッションを開催したり、お気に入りのチャットツールでディスカッションボードを形成するかどうかに関わらず、チームが共通の理解を成長させることができるように、あなたの経験や学習について話す必要があります。

### 自動的にチェックするには

> [クリーン ABAP](#クリーン-abap) > [目次](#目次) > [やり方](#やり方) > [本節](#自動的にチェックするには)

ここで説明するアンチパターンを自動的に検出する静的コードチェックの包括的なスイートはありません。

ABAP テストコックピット、コードインスペクタ、拡張チェック、チェックマンは、特定の問題を見つけるのに役立ついくつかのチェックを提供しています。

コードインスペクタチェックのオープンソースのコレクションである [abapOpenChecks](https://github.com/larshp/abapOpenChecks) も記述されているアンチパターンの一部をカバーしています。

[abaplint](https://github.com/abaplint/abaplint) はオープンソースの ABAP パーサの再実装です。SAP システムなしで動作し、abapGit でシリアライズされたコード上で使用されるように設計されています。複数の統合機能（GitHub Actions、Jenkins、テキストエディタなど）を提供し、いくつかのアンチパターンをカバーし、フォーマットやコード規約のチェックにも使用できます。

### 他のガイドとの関係性

> [クリーン ABAP](#クリーン-abap) > [目次](#目次) > [やり方](#やり方) > [本節](#他のガイドとの関係性)

私たちのガイドは、クリーンコードの _精神_ に従っていて、例えば、[管理可能な例外のための CX_STATIC_CHECK のスロー](#管理可能な例外のための CX_STATIC_CHECK のスロー)など、ABAP プログラミング言語にいくつかの調整をしたことを意味します。

いくつかの要素は [ABAP プログラミングガイドライン](https://help.sap.com/doc/abapdocu_751_index_htm/7.51/en-US/index.htm?file=abenabap_pgl.htm) から引用されており、このガイドはガイドラインとほとんど互換性があります。逸脱は示され、常によりクリーンなコードの精神に基づいています。

このガイドでは、[ABAP 開発に関する DSAG の推奨事項](https://www.dsag.de/sites/default/files/dsag_recommendation_abap_development.pdf) も尊重していますが、ほとんどの詳細についてはより正確な情報を提供しています。

発行以来、クリーン ABAP は、S/4HANA に取り組む数百人の開発者を含む多くの SAP 社内開発チームのリファレンスガイドとなっています。

### 反対するには

> [クリーン ABAP](#クリーン-abap) > [目次](#目次) > [やり方](#やり方) > [本節](#反対するには)

このスタイルガイドは、すでに Clean Code を知っている方や、これから Clean Code に取り組む方に向けて、Clean Code を _特に ABAP に_ どのように適用するかに重点を置いて書きました。

そのため、原書や関連資料と同じ長さと深さですべての概念を紹介しているわけではないことに留意ください。それらは、特に私たちがうまく説明していないからといって、ここにあることに反対する場合には、読む価値があります。節の中のリンクを使って、私たちのガイダンスの背景を読んでみてください。

ここで書かれていることについて、議論したり、反対したりすることは自由です。クリーンコードの柱の一つは、 _チームのルール_ です。ただ、捨てる前に公平なチャンスを与えることを忘れないでください。

[CONTRIBUTING.md](../CONTRIBUTING.md) には、このガイドを変更したり、細かい部分で逸脱したりする方法についての提案が書かれています。

## 命名

> [クリーン ABAP](#クリーン-abap) > [目次](#目次) > [本節](#命名)

### 意味のある名前を使う

> [クリーン ABAP](#クリーン-abap) > [目次](#目次) > [命名](#命名) > [本節](#意味のある名前を使う)

物事の内容や意味が伝わる名前を使いましょう。

```ABAP
CONSTANTS max_wait_time_in_seconds TYPE i ...
DATA customizing_entries TYPE STANDARD TABLE ...
METHODS read_user_preferences ...
CLASS /clean/user_preference_reader ...
```

データ型や技術的なエンコーディングに注目してはいけません。これらはコードを理解することにはほとんど貢献しません。

```ABAP
" アンチパターン
CONSTANTS sysubrc_04 TYPE sysubrc ...
DATA iso3166tab TYPE STANDARD TABLE ...
METHODS read_t005 ...
CLASS /dirty/t005_reader ...
```

[不適切な命名をコメントで補おうとしない](#不適切な命名をコメントで補おうとしない)

> 詳細については [Robert C. Martin の _Clean Code_] の _Chapter 2: Meaningful Names: Use Intention-Revealing Names_ を参照してください。

### ソリューションドメインと問題ドメインの用語を選ぶ

> [クリーン ABAP](#クリーン-abap) > [目次](#目次) > [命名](#命名) > [本節](#ソリューションドメインと問題ドメインの用語を選ぶ)

ソリューションドメイン、すなわち「キュー」や「ツリー」などのコンピュータサイエンス用語と、問題ドメイン、すなわち「勘定科目」や「元帳」などのビジネス分野の用語で、良い名前を検索します。

ビジネスライクなレイヤーには、問題のドメインにちなんだ名前をつけるのが最適です。これは特に、API やビジネスオブジェクトなど、ドメイン駆動設計で設計されたコンポーネントに当てはまります。

ファクトリクラスや抽象アルゴリズムなどの主に技術的な機能を提供するレイヤーには、ソリューションのドメインにちなんだ名前をつけると最もよく聞こえます。

いずれにしても、独自の言葉を作ろうとしないでください。開発者、プロダクトオーナー、パートナー、顧客の間で情報を交換できるようにする必要があるので、カスタマイズされた辞書を使わずに、これらすべてに関連していると思われる名前を選びましょう。

> 詳細については [Robert C. Martin の _Clean Code_] の _Chapter 2: Meaningful Names: Use Solution Domain Names_ と _Use Problem Domain Names_ を参照してください。

### 複数形を使う

> [クリーン ABAP](#クリーン-abap) > [目次](#目次) > [命名](#命名) > [本節](#複数形を使う)

SAP では、テーブルの名前を単数形、例えば「国のテーブル」の場合は `country` というように命名するという時代遅れの慣習があります。しかし、SAP 以外の世界では一般的な傾向として、リストには複数形を使用します。そのため、代わりに `countries` を使用することをお勧めします。

> このアドバイスは、主に変数やクラスの属性などを対象としています。開発オブジェクトの場合は、例えば、データベースのテーブル（「透過テーブル」）を単数形で命名するために広く使われている規約など、競合するパターンの方が適切かもしれません。

> 詳細については [Robert C. Martin の _Clean Code_] の _Chapter 2: Meaningful Names: Use Intention-Revealing Names_ を参照してください。

### 発音可能な名前を使う

> [クリーン ABAP](#クリーン-abap) > [目次](#目次) > [命名](#命名) > [本節](#発音可能な名前を使う)

私たちは、対象についてたくさん考えたり、話したりしています。そのため、発音できる名前を使いましょう。例えば、 `dobjt` のような暗号的なものではなく、 `detection_object_types` を使用してください。

> 詳細については [Robert C. Martin の _Clean Code_] の _Chapter 2: Meaningful Names: Use Pronounceable Names_ を参照してください。

### 略語を避ける

> [クリーン ABAP](#クリーン-abap) > [目次](#目次) > [命名](#命名) > [本節](#略語を避ける)

スペースに余裕がある場合は、名前を完全に書き出してください。長さの制限を超える場合のみ省略してください。

どうしても省略しなければならない場合は、 _重要でない_ 単語から始めましょう。

言葉を略すことは一見効率的に見えても、すぐに誤解を招くことになります。例えば、 `cust` の「cust」が「カスタマイズ」を指しているのか、「顧客」を指しているのか、「カスタム」を指しているのかは明確ではありません。この 3 つの用語はいずれも SAP アプリケーションでは一般的なものです。

> 詳細については [Robert C. Martin の _Clean Code_] の _Chapter 2: Meaningful Names: Make Meaningful Distinctions_ を参照してください。

### どこでも同じ略語を使う

> [クリーン ABAP](#クリーン-abap) > [目次](#目次) > [命名](#命名) > [本節](#どこでも同じ略語を使う)

人々は関連するコードを見つけるためにキーワードを検索します。これをサポートするために、同じものを同じ略語で表現します。例えば、「detection object type」を常に「dobjt」と略すようにし、「dot」や「dotype」、「detobjtype」などを混在させないようにしてください。

> 詳細については [Robert C. Martin の _Clean Code_] の _Chapter 2: Meaningful Names: Use Searchable Names_ を参照してください。

### クラスには名詞を、メソッドには動詞を使う

> [クリーン ABAP](#クリーン-abap) > [目次](#目次) > [命名](#命名) > [本節](#クラスには名詞を、メソッドには動詞を使う)

クラス、インタフェース、オブジェクトの名前には名詞や名詞句を使用しましょう：

```ABAP
CLASS /clean/account
CLASS /clean/user_preferences
INTERFACE /clean/customizing_reader
```

メソッドの名前には動詞や動詞句を使用しましょう：

```ABAP
METHODS withdraw
METHODS add_message
METHODS read_entries
```

ブーリアン型メソッドの名前を `is_` や `has_` のような動詞で開始すると、読みやすくなります：

```ABAP
IF is_empty( table ).
```

関数にはメソッドのような名前をつけることをお勧めします：

```ABAP
FUNCTION /clean/read_alerts
```

### "data", "info", "object" などの意味のない言葉を避ける

>[Cle-ABAP-clean-bap) > [目次](#目次) > [命名](#命名) > [本節](#data-info-object-などの意味のない言葉を避ける)

意味のない言葉は省略しましょう

```ABAP
account  " account_data とするのではなく
alert    " alert_object とするのではなく
```

または、より具体的に意味のある言葉に置き換えてください

```ABAP
user_preferences          " user_info とするのではなく
response_time_in_seconds  " response_time_variable とするのではなく
```

> 詳細については [Robert C. Martin の _Clean Code_] の _Chapter 2: Meaningful Names: Make Meaningful Distinctions_ を参照してください。

### 1つの概念には1つの言葉を使う

> [クリーン ABAP](#クリーン-abap) > [目次](#目次) > [命名](#命名) > [本節](#1つの概念には1つの言葉を使う)

```ABAP
METHODS read_this.
METHODS read_that.
METHODS read_those.
```

概念を表す言葉を選び、それに拘ります。他の同義語を混在させないようにしてください。同義語があると、コードを読む人はありもしない意味の違いを見つけようとして時間を浪費してしまいます。

```ABAP
" アンチパターン
METHODS read_this.
METHODS retrieve_that.
METHODS query_those.
```

> 詳細については [Robert C. Martin の _Clean Code_] の _Chapter 2: Meaningful Names: Pick One Word per Concept_ を参照してください。

### パターン名はそれを意図する場合にのみ使う

> [クリーン ABAP](#クリーン-abap) > [目次](#目次) > [命名](#命名) > [本節](#パターン名はそれを意図する場合にのみ使う)

本当にそれを意図しているのでなければ、クラスやインタフェースにソフトウェアデザインパターンの名前を使わないようにしましょう。例えば、本当にファクトリーデザインパターンを実装していない限り、クラス名を `file_factory` とはしないでください。最も一般的なパターンには、
[singleton](https://en.wikipedia.org/wiki/Singleton_pattern)、
[factory](https://en.wikipedia.org/wiki/Factory_method_pattern)、
[facade](https://en.wikipedia.org/wiki/Facade_pattern)、
[composite](https://en.wikipedia.org/wiki/Composite_pattern)、
[decorator](https://en.wikipedia.org/wiki/Decorator_pattern)、
[iterator](https://en.wikipedia.org/wiki/Iterator_pattern)、
[observer](https://en.wikipedia.org/wiki/Observer_pattern)、
[strategy](https://en.wikipedia.org/wiki/Strategy_pattern)
などがあります。

> 詳細については [Robert C. Martin の _Clean Code_] の _Chapter 2: Meaningful Names: Avoid Disinformation_ を参照してください。

### エンコーディング, 特にハンガリアン記法と接頭辞を避ける

> [クリーン ABAP](#クリーン-abap) > [目次](#目次) > [命名](#命名) > [本節](#エンコーディング-特にハンガリアン記法と接頭辞を避ける)

私たちは、 _すべての_ エンコーディング接頭辞を取り除くことをお勧めします。

```ABAP
METHOD add_two_numbers.
  result = a + b.
ENDMETHOD.
```

次のように、不必要に長くするのではなく

```ABAP
METHOD add_two_numbers.
  rv_result = iv_a + iv_b.
ENDMETHOD.
```

> 理由は [Avoid Encodings](sub-sections/AvoidEncodings.md) に詳しく書かれています。

## 言語

> [クリーン ABAP](#クリーン-abap) > [目次](#目次) > [本節](#言語)

### 古いABAPリリースに注意する

> [クリーン ABAP](#クリーン-abap) > [目次](#目次) > [言語](#言語) > [本節](#古いABAPリリースに注意する)

古いABAPリリースでコーディングを行う場合は、このガイドのアドバイスに注意してください。以下の推奨事項の多くは、古いABAPリリースではサポートされていない可能性のある比較的新しい文法や構文を使用しています。サポートしなければならない最古のリリースで適用したいガイドラインを検証してください。ただし、クリーンコード全体を単純に破棄しないでください。ほとんどのルール（名前付けやコメントなど）は _どの_ ABAP バージョンでも動作します。

### パフォーマンスに注意する

> [クリーン ABAP](#クリーン-abap) > [目次](#目次) > [言語](#言語) > [本節](#パフォーマンスに注意する)

高いパフォーマンスが要求されるコンポーネントをコーディングする場合は、このガイドのアドバイスに注意してください。クリーンコードのいくつかの側面は、パフォーマンスを低下させたり（メソッド呼び出しが多くなる）、メモリをより消費したり（オブジェクトが多くなる）する可能性があります。ABAPには、これを強めてしまう可能性のあるいくつかの特殊性があります。例えば、ABAPはメソッドを呼び出す際にデータ型を比較しているため、1つの大きなメソッドを多くのサブメソッドに分割するとコードが遅くなる可能性があります。

しかし、不明確な恐怖心から、時期尚早に最適化しないことを強くお勧めします。大多数のルール(命名、コメントなど)は全く悪影響を与えません。クリーンでオブジェクト指向的な方法で物事を構築するようにしましょう。もし、何かが遅すぎる場合は、パフォーマンスの測定を行います。その時になって初めて、選択したルールを破棄するという事実に基づいた決断をすべきです。

[Martin Fowler の _Refactoring_](https://martinfowler.com/books/refactoring.html) の第2章の一部を抜粋して、さらにいくつかの考えを述べます。

典型的なアプリケーションでは、ランタイムの大部分はコードのごく一部に費やされます。コードのわずか10%がランタイムの90%を占めることもあり、特にABAPではランタイムの大部分がデータベース時間になる可能性が高いです。

したがって、_すべての_ コードを常に超効率的にしようとすることに多大な労力を費やすのは、リソースの最善の使い方ではありません。パフォーマンスを無視しようと言っているわけではなく、開発の初期段階ではクリーンでよく構造化されたコードに注力し、それからプロファイラを使用して最適化すべき重要な領域を特定することをお勧めします。

実際、このようなアプローチは、より的を絞った最適化の努力であるため、パフォーマンスに正味のプラスの効果があり、よく構造化されたコードはパフォーマンスのボトルネックの特定や、リファクタリングやチューニングが容易になるはずです。

### 手続き型プログラミングよりもオブジェクト指向を選ぶ

> [クリーン ABAP](#クリーン-abap) > [目次](#目次) > [言語](#言語) > [本節](#手続き型プログラミングよりもオブジェクト指向を選ぶ)

オブジェクト指向のプログラム(クラスやインタフェース)は、手続き的なコード(関数やプログラム)よりもセグメント化されており、リファクタリングやテストをより容易に行うことができます。手続き的なオブジェクト (RFC のための関数、トランザクションのためのプログラム) を書かなければいけない状況もありますが、これらのオブジェクトは実際の関数を提供するクラスを呼び出すことに限定すべきです。

```ABAP
FUNCTION check_business_partner [...].
  DATA(validator) = NEW /clean/biz_partner_validator( ).
  result = validator->validate( business_partners ).
ENDFUNCTION.
```

> [Function Groups vs. Classes](sub-sections/FunctionGroupsVsClasses.md)
> に違いが詳細に書かれています。

### 手続き型の言語構造よりも関数型の言語構造を選ぶ

> [クリーン ABAP](#クリーン-abap) > [目次](#目次) > [言語](#言語) > [本節](#手続き型の言語構造よりも関数型の言語構造を選ぶ)

これらは通常、短く、モダンなプログラマーにとってはより自然なものになります。

```ABAP
DATA(variable) = 'A'.
" MOVE 'A' TO variable.

DATA(uppercase) = to_upper( lowercase ).
" TRANSLATE lowercase TO UPPER CASE.

index += 1.         " >= NW 7.54
index = index + 1.  " < NW 7.54
" ADD 1 TO index.

DATA(object) = NEW /clean/my_class( ).
" CREATE OBJECT object TYPE /dirty/my_class.

result = VALUE #( FOR row IN input ( row-text ) ).
" LOOP AT input INTO DATA(row).
"  INSERT row-text INTO TABLE result.
" ENDLOOP.

DATA(line) = value_pairs[ name = 'A' ]. " entry must exist
DATA(line) = VALUE #( value_pairs[ name = 'A' ] OPTIONAL ). " entry can be missing
" READ TABLE value_pairs INTO DATA(line) WITH KEY name = 'A'.

DATA(exists) = xsdbool( line_exists( value_pairs[ name = 'A' ] ) ).
IF line_exists( value_pairs[ name = 'A' ] ).
" READ TABLE value_pairs TRANSPORTING NO FIELDS WITH KEY name = 'A'.
" DATA(exists) = xsdbool( sy-subrc = 0 ).
```

以下に示す詳細なルールの多くは、この一般的なアドバイスの具体的な繰り返しに過ぎません。

### 廃止された言語要素を避ける

> [クリーン ABAP](#クリーン-abap) > [目次](#目次) > [言語](#言語) > [本節](#廃止された言語要素を避ける)

ABAPのバージョンをアップグレードする際には、廃止された言語要素を確認し、使用を控えるようにしましょう。

例えば、以下の文中の `@`-エスケープされた「host」変数は、何がプログラム変数で何がデータベースのカラムなのかを明確にしています。

```ABAP
SELECT *
  FROM spfli
  WHERE carrid = @carrid AND
        connid = @connid
  INTO TABLE @itab.
```

[廃止されたエスケープ省略形式](https://help.sap.com/doc/abapdocu_750_index_htm/7.50/en-US/abenopen_sql_hostvar_obsolete.htm) と比べて

```ABAP
SELECT *
  FROM spfli
  WHERE carrid = carrid AND
        connid = connid
  INTO TABLE itab.
```

新しい代替案はコードの可読性を向上させ、最新のプログラミング・パラダイムとの設計の衝突を減らす傾向があり、それらに切り替えることでコードを自動的にクリーンにすることができます。

廃止された要素は処理速度やメモリ消費量の面で最適化の恩恵を受けられなくなる可能性があります。

モダンな言語要素を使用することで、SAPのトレーニングではもはや教えられていないため、時代遅れの構成要素に慣れていないかもしれない若いABAPerを簡単に参加させることができます。

SAP NetWeaver のドキュメントには、廃止された言語要素がリストアップされています。例えば、
[NW 7.50](https://help.sap.com/doc/abapdocu_750_index_htm/7.50/en-US/index.htm?file=abenabap_obsolete.htm)、
[NW 7.51](https://help.sap.com/doc/abapdocu_751_index_htm/7.51/en-US/index.htm?file=abenabap_obsolete.htm)、
[NW 7.52](https://help.sap.com/doc/abapdocu_752_index_htm/7.52/en-US/index.htm?file=abenabap_obsolete.htm)、
[NW 7.53](https://help.sap.com/doc/abapdocu_753_index_htm/7.53/en-US/index.htm?file=abenabap_obsolete.htm)。

### デザインパターンを賢く使う

> [クリーン ABAP](#クリーン-abap) > [目次](#目次) > [言語](#言語) > [本節](#デザインパターンを賢く使う)

デザインパターンは、それが適切であり、目立った効果をもたらす場合に使用します。デザインパターンを使いたいがために、どこにでもデザインパターンを適用してはいけません。

## 定数

> [クリーン ABAP](#クリーン-abap) > [目次](#目次) > [本節](#定数)

### マジックナンバーの代わりに定数を使う

> [クリーン ABAP](#クリーン-abap) > [目次](#目次) > [定数](#定数) > [本節](#マジックナンバーの代わりに定数を使う)

```ABAP
IF abap_type = cl_abap_typedescr=>typekind_date.
```

は以下より明確です

```ABAP
" アンチパターン
IF abap_type = 'D'.
```

> 詳細については [Robert C. Martin の _Clean Code_] の _Chapter 17: Smells and Heuristics: G25:
> Replace Magic Numbers with Named Constants_ を参照してください。

### 定数インタフェースよりも列挙クラスを選ぶ

> [クリーン ABAP](#クリーン-abap) > [目次](#目次) > [定数](#定数) > [本節](#定数インタフェースよりも列挙クラスを選ぶ)

```ABAP
CLASS /clean/message_severity DEFINITION PUBLIC ABSTRACT FINAL.
  PUBLIC SECTION.
    CONSTANTS:
      warning TYPE symsgty VALUE 'W',
      error   TYPE symsgty VALUE 'E'.
ENDCLASS.
```

または

```ABAP
CLASS /clean/message_severity DEFINITION PUBLIC CREATE PRIVATE FINAL.
  PUBLIC SECTION.
    CLASS-DATA:
      warning TYPE REF TO /clean/message_severity READ-ONLY,
      error   TYPE REF TO /clean/message_severity READ-ONLY.
  " ...
ENDCLASS.
```

関係のないものを混ぜたり、定数のコレクションが「実装されている」かもしれないと誤解させたりするのではなく

```ABAP
" アンチパターン
INTERFACE /dirty/common_constants.
  CONSTANTS:
    warning      TYPE symsgty VALUE 'W',
    transitional TYPE i       VALUE 1,
    error        TYPE symsgty VALUE 'E',
    persisted    TYPE i       VALUE 2.
ENDINTERFACE.
```

> [Enumerations](sub-sections/Enumerations.md)
> 一般的な列挙パターンについて説明し、その長所と短所を論じます。
>
> 詳細については [Robert C. Martin の _Clean Code_] の _Chapter 17: Smells and Heuristics: J3: Constants versus Enums_ を参照してください。

### 列挙クラスを使用しない場合は定数をグループ化する

> [クリーン ABAP](#クリーン-abap) > [目次](#目次) > [定数](#定数) > [本節](#列挙クラスを使用しない場合は定数をグループ化する)

インタフェースなどで緩く定数を集める場合は、それらをグループ化します。

```ABAP
CONSTANTS:
  BEGIN OF message_severity,
    warning TYPE symsgty VALUE 'W',
    error   TYPE symsgty VALUE 'E',
  END OF message_severity,
  BEGIN OF message_lifespan,
    transitional TYPE i VALUE 1,
    persisted    TYPE i VALUE 2,
  END OF message_lifespan.
```

次のコードよりも関係がより明確になります

```ABAP
" アンチパターン
CONSTANTS:
  warning      TYPE symsgty VALUE 'W',
  transitional TYPE i       VALUE 1,
  error        TYPE symsgty VALUE 'E',
  persisted    TYPE i       VALUE 2,
```

グループ単位のアクセスも可能です。例えば、入力のバリデーションでは

```ABAP
DO.
  ASSIGN COMPONENT sy-index OF STRUCTURE message_severity TO FIELD-SYMBOL(<constant>).
  IF sy-subrc IS INITIAL.
    IF input = <constant>.
      DATA(is_valid) = abap_true.
      RETURN.
    ENDIF.
  ELSE.
    RETURN.
  ENDIF.
ENDDO.
```

> 詳細については [Robert C. Martin の _Clean Code_] の _Chapter 17: Smells and Heuristics: G27: Structure over Convention_ を参照してください。

## 変数

> [クリーン ABAP](#クリーン-abap) > [目次](#目次) > [本節](#変数)

### 事前宣言よりもインライン宣言を選ぶ

> [クリーン ABAP](#クリーン-abap) > [目次](#目次) > [変数](#変数) > [本節](#事前宣言よりもインライン宣言を選ぶ)

これらのガイドラインに従えば、あなたのメソッドは非常に短くなり (3-5 ステートメント)、最初に出てきた場所で変数をインライン宣言することがより自然に見えるようになります。

```ABAP
METHOD do_something.
  DATA(name) = 'something'.
  DATA(reader) = /clean/reader=>get_instance_for( name ).
  result = reader->read_it( ).
ENDMETHOD.
```

メソッドの最初に `DATA` セクションで変数を宣言するよりも

```ABAP
" アンチパターン
METHOD do_something.
  DATA:
    name   TYPE seoclsname,
    reader TYPE REF TO /dirty/reader.
  name = 'something'.
  reader = /dirty/reader=>get_instance_for( name ).
  result = reader->read_it( ).
ENDMETHOD.
```

> 詳細については [Robert C. Martin の _Clean Code_] の _Chapter 5: Formatting: Vertical Distance: Variable Declarations_ を参照してください。

### 選択の分岐内でインライン宣言をしない

> [クリーン ABAP](#クリーン-abap) > [目次](#目次) > [変数](#変数) > [本節](#選択の分岐内でインライン宣言をしない)

```ABAP
" アンチパターン
IF has_entries = abap_true.
  DATA(value) = 1.
ELSE.
  value = 2.
ENDIF.
```

ABAPはインライン宣言をあたかもメソッドの先頭にあるかのように扱うので、これはうまく動作します。しかし、特にメソッドが長く、すぐに宣言を見つけられない場合は、読み手を非常に混乱させてしまいます。このような場合は、インライン宣言をやめて、宣言を前に置いてください。

```ABAP
DATA value TYPE i.
IF has_entries = abap_true.
  value = 1.
ELSE.
  value = 2.
ENDIF.
```

> 詳細については [Robert C. Martin の _Clean Code_] の _Chapter 5: Formatting: Vertical Distance: Variable Declarations_ を参照してください。

### 事前宣言を連結させない

> [クリーン ABAP](#クリーン-abap) > [目次](#目次) > [変数](#変数) > [本節](#事前宣言を連結させない)

```ABAP
DATA name TYPE seoclsname.
DATA reader TYPE REF TO reader.
```

連結は、定義された変数が論理レベルで関連していることを示唆しています。これを一貫して使用するためには、すべての連結された変数が一緒に属することを確認し、変数を追加するために追加の連結グループを導入しなければなりません。これは可能ですが、通常はその努力に見合うものではありません。

また、連結は、各行が異なるように見え、それらを変更するには、コロン、ドット、カンマをいじらなければならないので、リフォーマットやリファクタリングを不必要に複雑にします。

```ABAP
" アンチパターン
DATA:
  name   TYPE seoclsname,
  reader TYPE REF TO reader.
```

> [型句の位置を揃えない](#型句の位置を揃えない) も参照してください。  
> データ宣言の連結を使用する場合は、一緒に属する変数のグループごとに1つの連結を使用します。

### FIELD-SYMBOLよりもREF TOを選択する

> [クリーン ABAP](#クリーン-abap) > [目次](#目次) > [変数](#変数) > [本節](#FIELD-SYMBOLよりもREF-TOを選択する)

> この節は[議論を引き起こしています](https://github.com/SAP/styleguides/issues/115)。`FIELD-SYMBOL` は、内部テーブルを反復処理する際にかなり高速になるようですが、このような場合に `REF TO` を使用することを推奨するとパフォーマンスが低下する可能性があります。

```ABAP
LOOP AT components REFERENCE INTO DATA(component).
```

の代わりに

```ABAP
" アンチパターン
LOOP AT components ASSIGNING FIELD-SYMBOL(<component>).
```

フィールドシンボルが必要な場合を除いて

```ABAP
ASSIGN generic->* TO FIELD-SYMBOL(<generic>).
ASSIGN COMPONENT name OF STRUCTURE structure TO FIELD-SYMBOL(<component>).
ASSIGN (class_name)=>(static_member) TO FIELD-SYMBOL(<member>).
```

コードレビューは、人々が「ただ〜だから」「いつもそのようにLOOPしているから」「特別な理由がないから」といった恣意的な理由で、この2つから選択する傾向があることを示しています。恣意的な選択は、なぜ一方が他方よりも使用されているのかという無用な質問にコードを読む人の時間を浪費させることになります。したがって、十分な根拠に基づく正確な決定に置き換えるべきです。私たちの推奨は、このような理由に基づいています。

- フィールドシンボルは、構造体のコンポーネントに動的にアクセスするなど、参照ではできないことができます。
  同様に、参照は、動的に型付けされたデータ構造体を構築するなど、フィールドシンボルではできないことを行うことができます。
  まとめると、1つだけに落ち着くことは不可能です。

- オブジェクト指向のABAPでは、すべてのオブジェクトが `REF TO <クラス名>` なので、参照はあらゆる場所に存在し、避けることはできません。
  対照的に、フィールドシンボルが厳密に必要とされるのは、動的な型付けに関係するごく一部の特別なケースだけです。
  そのため、オブジェクト指向のプログラムでは参照がより自然な選択となります。

- フィールドシンボルは参照よりも短いですが、結果として得られるメモリの節約は非常に小さいので、無視しても問題ありません。
  同様に、速度も問題ではありません。結果として、どちらか一方を他方よりも優先するパフォーマンス関連の理由はありません。

> 詳細はこの記事
> [_Accessing Data Objects Dynamically_ in the ABAP Programming Guidelines](https://help.sap.com/doc/abapdocu_751_index_htm/7.51/en-US/index.htm?file=abendyn_access_data_obj_guidl.htm) を参照してください。

## テーブル

> [クリーン ABAP](#クリーン-abap) > [目次](#目次) > [本節](#テーブル)

### 正しいテーブルデータ型を使う

> [クリーン ABAP](#クリーン-abap) > [目次](#目次) > [テーブル](#テーブル) > [本節](#正しいテーブルデータ型を使う)

- 通常、 `HASHED` テーブルは、**1つのステップで入力され**、**変更されず**、**キーによって頻繁に読み取られる**、**大きなテーブル**に使用します。
  ハッシュテーブルには固有のメモリと処理オーバーヘッドがあるため、大量のデータと大量の読み取りアクセスに対してのみ有効です。
  テーブルの内容を変更するたびにハッシュの再計算が必要になるので、頻繁に変更されるテーブルには使用しないでください。

- 通常、`SORTED` テーブルは、**常にソートする必要があり**、**ビット単位で入力されたり**、**変更する必要があり**、**1つ以上の完全キーまたは部分キーで頻繁に読み取られたり**、**特定の順序で処理されたりする**、**大きなテーブル**に使用します。
  コンテンツを追加、変更、削除するには、適切な挿入箇所を見つける必要がありますが、テーブルのインデックスの残りの部分を調整する必要はありません。
  ソートされたテーブルは、多数の読み取りアクセスに対してのみその価値を発揮します。

- `STANDARD` テーブルは、インデックスを作成すると利点よりもオーバーヘッドが大きくなるような**小さなテーブル**や、行の順序をまったく気にしないか、追加された順に正確に処理したい **「配列」** テーブルに使用します。
また、テーブルへの異なるアクセスが必要な場合、例えば、インデックス付きアクセスや `SORT` や `BINARY SEARCH` によるソートされたアクセスがあります。

> これらはあくまでも大まかなガイドラインです。
> 詳細はこの記事 [_Selection of Table Category_ in the ABAP Language Help](https://help.sap.com/doc/abapdocu_751_index_htm/7.51/en-US/abenitab_kind.htm) を参照してください。

### DEFAULT KEY を避ける

> [クリーン ABAP](#クリーン-abap) > [目次](#目次) > [テーブル](#テーブル) > [本節](#DEFAULT-KEY-を避ける)

```ABAP
" アンチパターン
DATA itab TYPE STANDARD TABLE OF row_type WITH DEFAULT KEY.
```

デフォルトキーは、新しい関数文を動作させるためだけに追加されることがよくあります。
実際のところ、キー自体は通常余計なものであり、リソースを無駄に浪費します。
数値データ型を無視するため、不明瞭な間違いにつながることさえあります。
明示的なフィールドリストがない `SORT` や `DELETE ADJACENT` 文は、内部テーブルの主キーに依存します。`DEFAULT KEY` を使用した場合、例えば数値フィールドをキーの構成要素として持つ場合、特に `READ TABLE ... BINARY` などとの組み合わせでは、非常に予期せぬ結果になる可能性があります。

キーコンポーネントを明示的に指定する

```ABAP
DATA itab2 TYPE STANDARD TABLE OF row_type WITH NON-UNIQUE KEY comp1 comp2.
```

または、キーが全く必要ない場合は `EMPTY KEY` を使ってください。

```ABAP
DATA itab1 TYPE STANDARD TABLE OF row_type WITH EMPTY KEY.
```

> [Horst Keller's blog on _Internal Tables with Empty Key_](https://blogs.sap.com/2013/06/27/abap-news-for-release-740-internal-tables-with-empty-key/) によると
>
> **注意:** `EMPTY KEY` を持つ（明示的なソートフィールドを持たない）内部テーブルでの `SORT` は全くソートされませんが、キーが空であるかどうかを静的に判断できる場合には、構文警告が出ます。

### APPEND TO よりも INSERT INTO TABLE を選ぶ

> [クリーン ABAP](#クリーン-abap) > [目次](#目次) > [テーブル](#テーブル) > [本節](#APPEND-TO-よりも-INSERT-INTO-TABLE-を選ぶ)

```ABAP
INSERT VALUE #( ... ) INTO TABLE itab.
```

`INSERT INTO TABLE` はすべてのテーブルとキーの型で動作するため、
パフォーマンス要件が変更されてもテーブルの型とキー定義のリファクタリングが容易になります。

`APPEND TO` は、`STANDARD` テーブルを配列のように使って、追加されたエントリが最終行になることを強調したい場合にのみ使用します。

### READ TABLE や LOOP AT よりも LINE_EXISTS を選ぶ

> [クリーン ABAP](#クリーン-abap) > [目次](#目次) > [テーブル](#テーブル) > [本節](#READ-TABLE-や-LOOP-AT-よりも-LINE_EXISTS-を選ぶ)

```ABAP
IF line_exists( my_table[ key = 'A' ] ).
```

次のコードよりも短く、明確に意図を表現しています。

```ABAP
" アンチパターン
READ TABLE my_table TRANSPORTING NO FIELDS WITH KEY key = 'A'.
IF sy-subrc = 0.
```

ましてや

```ABAP
" アンチパターン
LOOP AT my_table REFERENCE INTO DATA(line) WHERE key = 'A'.
  line_exists = abap_true.
  EXIT.
ENDLOOP.
```

### LOOP AT よりも READ TABLE を選ぶ

> [クリーン ABAP](#クリーン-abap) > [目次](#目次) > [テーブル](#テーブル) > [本節](#LOOP-AT-よりも-READ-TABLE-を選ぶ)

```ABAP
READ TABLE my_table REFERENCE INTO DATA(line) WITH KEY key = 'A'.
```

次のコードよりも短く、明確に意図を表現しています。

```ABAP
" アンチパターン
LOOP AT my_table REFERENCE INTO DATA(line) WHERE key = 'A'.
  EXIT.
ENDLOOP.
```

ましてや

```ABAP
" アンチパターン
LOOP AT my_table REFERENCE INTO DATA(line).
  IF line->key = 'A'.
    EXIT.
  ENDIF.
ENDLOOP.
```

### ネストした IF よりも LOOP AT WHERE を選ぶ

> [クリーン ABAP](#クリーン-abap) > [目次](#目次) > [テーブル](#テーブル) > [本節](#ネストした-IF-よりも-LOOP-AT-WHERE-を選ぶ)

```ABAP
LOOP AT my_table REFERENCE INTO DATA(line) WHERE key = 'A'.
```

次のコードよりも短く、明確に意図を表現しています。

```ABAP
LOOP AT my_table REFERENCE INTO DATA(line).
  IF line->key = 'A'.
    EXIT.
  ENDIF.
ENDLOOP.
```

### 不要なテーブルの読み取りを避ける

> [クリーン ABAP](#クリーン-abap) > [目次](#目次) > [テーブル](#テーブル) > [本節](#不要なテーブルの読み取りを避ける)

行があると _予想される_ 場合は、一度だけ読み取って例外に対応します。

```ABAP
TRY.
    DATA(row) = my_table[ key = input ].
  CATCH cx_sy_itab_line_not_found.
    RAISE EXCEPTION NEW /clean/my_data_not_found( ).
ENDTRY.
```

2度読み取ることで、主制御の流れを散らかして遅くするのではなく

```ABAP
" アンチパターン
IF NOT line_exists( my_table[ key = input ] ).
  RAISE EXCEPTION NEW /clean/my_data_not_found( ).
ENDIF.
DATA(row) = my_table[ key = input ].
```

> パフォーマンスの向上に加えて、これはより一般的な
> [正常系かエラー処理に集中する, 両方ではなく](#正常系かエラー処理に集中する-両方ではなく)
> の具体的なバリエーションです。

## Strings

> [クリーン ABAP](#クリーン-abap) > [目次](#目次) > [本節](#strings)

### リテラルを定義するには ` を使う

> [クリーン ABAP](#クリーン-abap) > [目次](#目次) > [Strings](#strings) > [本節](#リテラルを定義するには--を使う)

```ABAP
CONSTANTS some_constant TYPE string VALUE `ABC`.
DATA(some_string) = `ABC`.  " --> TYPE string
```

`'` の使用は控えましょう。余計な型変換が追加されてしまいますし、`CHAR` なのか `STRING` なのかがわからなくなります。

```ABAP
" アンチパターン
DATA some_string TYPE string.
some_string = 'ABC'.
```

`|` は一般的には問題ありませんが、`CONSTANTS` には使用できず、固定値を指定すると不要なオーバーヘッドがかかります。

```ABAP
" アンチパターン
DATA(some_string) = |ABC|.
```

### テキストを組み立てるには | を使う

> [クリーン ABAP](#クリーン-abap) > [目次](#目次) > [Strings](#strings) > [本節](#テキストを組み立てるには--を使う)

```ABAP
DATA(message) = |Received HTTP code { status_code } with message { text }|.
```

文字列テンプレートは、特にテキストに複数の変数を埋め込む場合に、何がリテラルで何が変数なのかをよりよく強調します。

```ABAP
" アンチパターン
DATA(message) = `Received an unexpected HTTP ` && status_code && ` with message ` && text.
```

## ブーリアン

> [クリーン ABAP](#クリーン-abap) > [目次](#目次) > [本節](#ブーリアン)

### ブーリアン型を賢く使う

> [クリーン ABAP](#クリーン-abap) > [目次](#目次) > [ブーリアン](#ブーリアン) > [本節](#ブーリアン型を賢く使う)

一見、ブーリアン型が自然な選択であるように見える場合はよくあります。

```ABAP
" アンチパターン
is_archived = abap_true.
```

しかし、視点を変えると、列挙がより適切な選択であったことがわかります。

```ABAP
archiving_status = /clean/archivation_status=>archiving_in_process.
```

一般的に、ブーリアン型は、物事の種類を区別するには適しません。
なぜなら、どちらか一方だけではないケースがほぼ必ず出てくるためです。

```ABAP
assert_true( xsdbool( document->is_archived( ) = abap_true AND
                      document->is_partially_archived( ) = abap_true ) ).
```

[ブーリアン型の入力パラメータの代わりにメソッドを分割する](#ブーリアン型の入力パラメータの代わりにメソッドを分割する)
では、さらに、常にブーリアン型のパラメータを疑うべき理由について説明しています。

> 詳細は
> [1](http://www.beyondcode.org/articles/booleanVariables.html)
> を参照してください。

### ブーリアン型にはABAP_BOOLを使う

> [クリーン ABAP](#クリーン-abap) > [目次](#目次) > [ブーリアン](#ブーリアン) > [本節](#ブーリアン型にはABAP_BOOLを使う)

```ABAP
DATA has_entries TYPE abap_bool.
```

汎用型の `char1` を使用しないでください。
技術的には互換性がありますが、ブーリアン変数を扱っているという事実を曖昧にしてしまいます。

また、他のブーリアン型はしばしば奇妙な副作用があるので避けてください。例えば、`boolean` は3番目の値「undefined」をサポートしており、これは微妙なプログラミングエラーを引き起こします。

場合によっては、Dynpro フィールドなどのデータディクショナリエレメントが必要になることがあります。
`abap_bool` はデータディクショナリではなく型プール `abap` で定義されているため、ここでは使用できません。
この場合は、`boole_d` または `xfeld` を使用してください。
カスタム記述が必要な場合は、独自のデータエレメントを作成してください。

> ABAPは、普遍的なブーリアンデータ型を持たない唯一のプログラミング言語かもしれません。
> しかし、これを持つことは欠かせません。
> この推奨はABAPプログラミングガイドラインに基づいています。

### 比較にはABAP_TRUEとABAP_FALSEを使う

> [クリーン ABAP](#クリーン-abap) > [目次](#目次) > [ブーリアン](#ブーリアン) > [本節](#比較にはABAP_TRUEとABAP_FALSEを使う)

```ABAP
has_entries = abap_true.
IF has_entries = abap_false.
```

同等文字の `'X'` や `' '` や `space` は使用しないでください。
これらを使用すると、これがブーリアン式であることがわかりにくくなります。

```ABAP
" アンチパターン
has_entries = 'X'.
IF has_entries = space.
```

`INITIAL` との比較は避けてください - `abap_bool` のデフォルト値が `abap_false` であることを覚えておかなければならなくなります。

```ABAP
" アンチパターン
IF has_entries IS NOT INITIAL.
```

> ABAPは、真と偽の「定数」が組み込まれていない唯一のプログラミング言語かもしれません。
> しかし、それらを持つことは欠かせません。
> この推奨はABAPプログラミングガイドラインに基づいています。

### ブーリアン変数をセットするにはXSDBOOLを使う

> [クリーン ABAP](#クリーン-abap) > [目次](#目次) > [ブーリアン](#ブーリアン) > [本節](#ブーリアン変数をセットするにはXSDBOOLを使う)

```ABAP
DATA(has_entries) = xsdbool( line IS NOT INITIAL ).
```

等価の `IF`-`THEN`-`ELSE` の方が無駄に長い。

```ABAP
" アンチパターン
IF line IS INITIAL.
  has_entries = abap_false.
ELSE.
  has_entries = abap_true.
ENDIF.
```

`xsdbool` は、ブーリアン型 `abap_bool` に最も適した `char1` を直接生成するので、この目的には最適な方法です。
これと同等の関数 `boolc` と `boolx` は異なる型を生成し、余計な暗黙の型変換を行います。

私たちは `xsdbool` という名前が不運で誤解を招くということに同意します。
結局のところ、私たちは「xsd」という接頭辞が示唆する「XML Schema Definition」の部分には全く興味がありません。

`xsdbool` の代替案として、`COND` の三項式が考えられます。
この構文は直感的ですが、`THEN abap_true` セグメントを不必要に繰り返すため少し長くなりますし、暗黙のデフォルト値である `abap_false` の知識を必要とします。
これが二次的な解決策としてのみ提案する理由です。

```ABAP
DATA(has_entries) = COND abap_bool( WHEN line IS NOT INITIAL THEN abap_true ).
```

## 条件

> [クリーン ABAP](#クリーン-abap) > [目次](#目次) > [本節](#条件)

### 条件を肯定にしてみる

> [クリーン ABAP](#クリーン-abap) > [目次](#目次) > [条件](#条件) > [本節](#条件を肯定にしてみる)

```ABAP
IF has_entries = abap_true.
```

比較のために、同じ文を逆にするとどれだけわかりにくくなるかを見てみましょう。

```ABAP
" アンチパターン
IF has_no_entries = abap_false.
```

セクションタイトルの「してみる」は、[空のIF分岐](#空のIF分岐を作らない) のようなものが出てくるところまで無理にやってはいけないという意味です。

```ABAP
" アンチパターン
IF has_entries = abap_true.
ELSE.
  " ELSE ブロックでのみ何かを行い、IF は空のまま
ENDIF.
```

> 詳細は [Robert C. Martin の _Clean Code_] の _Chapter 17: Smells and Heuristics: G29: Avoid Negative Conditionals_ を参照してください。

### NOT ISよりもIS NOTを選ぶ

> [クリーン ABAP](#クリーン-abap) > [目次](#目次) > [条件](#条件) > [本節](#NOT-ISよりもIS-NOTを選ぶ)

```ABAP
IF variable IS NOT INITIAL.
IF variable NP 'TODO*'.
IF variable <> 42.
```

否定は論理的には等価ですが、「頭の中で逆転」が必要になり、理解が難しくなります。

```ABAP
" アンチパターン
IF NOT variable IS INITIAL.
IF NOT variable CP 'TODO*'.
IF NOT variable = 42.
```

> これは、[条件を肯定にしてみる](#条件を肯定にしてみる) のより具体的なバリエーションです。
> ABAPプログラミングガイドラインの
> [Alternative Language Constructs](https://help.sap.com/doc/abapdocu_753_index_htm/7.53/en-US/index.htm?file=abenalternative_langu_guidl.htm)
> の節でも説明されています。

### 複素条件を分解することを考える

> [クリーン ABAP](#クリーン-abap) > [目次](#目次) > [条件](#条件) > [本節](#複素条件を分解することを考える)

条件は、それらを構成する要素に分解すると簡単になります。

```ABAP
DATA(example_provided) = xsdbool( example_a IS NOT INITIAL OR
                                  example_b IS NOT INITIAL ).

DATA(one_example_fits) = xsdbool( applies( example_a ) = abap_true OR
                                  applies( example_b ) = abap_true OR
                                  fits( example_b ) = abap_true ).

IF example_provided = abap_true AND
   one_example_fits = abap_true.
```

すべてを一緒にするのではなく

```ABAP
" アンチパターン
IF ( example_a IS NOT INITIAL OR
     example_b IS NOT INITIAL ) AND
   ( applies( example_a ) = abap_true OR
     applies( example_b ) = abap_true OR
     fits( example_b ) = abap_true ).
```

> ABAP開発ツールのクイックフィックスを使用して、上記のように素早く条件を抽出し、変数を作成します。

### 複雑な条件を抽出することを考える

> [クリーン ABAP](#クリーン-abap) > [目次](#目次) > [条件](#条件) > [本節](#複雑な条件を抽出することを考える)

ほとんどの場合、複雑な条件を独自のメソッドに抽出するのがよいでしょう。

```ABAP
IF is_provided( example ).

METHOD is_provided.
  DATA(is_filled) = xsdbool( example IS NOT INITIAL ).
  DATA(is_working) = xsdbool( applies( example ) = abap_true OR
                              fits( example ) = abap_true ).
  result = xsdbool( is_filled = abap_true AND
                    is_working = abap_true ).
ENDMETHOD.
```

## If

> [クリーン ABAP](#クリーン-abap) > [目次](#目次) > [本節](#if)

### 空のIF分岐を作らない

> [クリーン ABAP](#クリーン-abap) > [目次](#目次) > [If](#if) > [本節](#空のIF分岐を作らない)

```ABAP
IF has_entries = abap_false.
  " do some magic
ENDIF.
```

の方が、次のコードよりも短くてわかりやすいです。

```ABAP
" アンチパターン
IF has_entries = abap_true.
ELSE.
  " do some magic
ENDIF.
```

### 複数の択一条件にはELSE IFよりもCASEを選ぶ

> [クリーン ABAP](#クリーン-abap) > [目次](#目次) > [If](#if) > [本節](#複数の択一条件にはELSE-IFよりもCASEを選ぶ)

```ABAP
CASE type.
  WHEN type-some_type.
    " ...
  WHEN type-some_other_type.
    " ...
  WHEN OTHERS.
    RAISE EXCEPTION NEW /clean/unknown_type_failure( ).
ENDCASE.
```

`CASE` を使用すると、相互に排他的な選択肢の組を簡単に確認できます。
一連の条件を連続して評価するのではなく、別のマイクロプロセッサ命令に変換できるため、一連の `IF` よりも高速に動作する可能性があります。
対象の変数を何度も繰り返さなくても、新しいケースを素早く追加できます。
このステートメントは、`IF`-`ELSEIF` を誤ってネストしたときに発生する可能性のあるいくつかのエラーを防ぐこともできます。

```ABAP
" アンチパターン
IF type = type-some_type.
  " ...
ELSEIF type = type-some_other_type.
  " ...
ELSE.
  RAISE EXCEPTION NEW /dirty/unknown_type_failure( ).
ENDIF.
```

### ネストの深さを浅くする

> [クリーン ABAP](#クリーン-abap) > [目次](#目次) > [If](#if) > [本節](#ネストの深さを浅くする)

```ABAP
" アンチパターン
IF <this>.
  IF <that>.
  ENDIF.
ELSE.
  IF <other>.
  ELSE.
    IF <something>.
    ENDIF.
  ENDIF.
ENDIF.
```

ネストした `IF` はすぐに理解が難しくなり、完全なカバレッジのために指数関数的な数のテストケースが必要になります。

デシジョンツリーは通常、サブメソッドを形成し、ブーリアン型のヘルパー変数を導入することで分離することができます。

他のケースでは、以下のようにIFをマージすることで簡略化することができます。

```ABAP
IF <this> AND <that>.
```

不要なネストをするのではなく

```ABAP
" アンチパターン
IF <this>.
  IF <that>.
```

## 正規表現

> [クリーン ABAP](#クリーン-abap) > [目次](#目次) > [本節](#正規表現)

### 正規表現よりもシンプルなメソッドを選ぶ

> [クリーン ABAP](#クリーン-abap) > [目次](#目次) > [正規表現](#正規表現) > [本節](#正規表現よりもシンプルなメソッドを選ぶ)

```ABAP
IF input IS NOT INITIAL.
" IF matches( val = input  regex = '.+' ).

WHILE contains( val = input  sub = 'abc' ).
" WHILE contains( val = input  regex = 'abc' ).
```

正規表現はすぐに理解するのが難しくなります。
単純な場合は、通常、それらがない方が簡単です。

また、正規表現は通常、式ツリーに解析され、実行可能なマッチャーに実行時にコンパイルする必要があるため、より多くのメモリと処理時間を消費します。
単純な解決策としては、簡単なループと一時的変数を使用します。

### 正規表現よりも基本的なチェックを選ぶ

> [クリーン ABAP](#クリーン-abap) > [目次](#目次) > [正規表現](#正規表現) > [本節](#正規表現よりも基本的なチェックを選ぶ)

```ABAP
CALL FUNCTION 'SEO_CLIF_CHECK_NAME'
  EXPORTING
    cls_name = class_name
  EXCEPTIONS
    ...
```

以下のように、ロジックを再発明するのではなく

```ABAP
" アンチパターン
DATA(is_valid) = matches( val     = class_name
                          pattern = '[A-Z][A-Z0-9_]{0,29}' ).
```

> 正規表現が使用される場合、Don't-Repeat-Yourself (DRY) の原則に目をつぶってしまう傾向があるようです。
> [Robert C. Martin の _Clean Code_] の _Chapter 17: Smells and Heuristics: General: G5: Duplication_ の節を参照してください。

### 複雑な正規表現は組み立てることを考える

> [クリーン ABAP](#クリーン-abap) > [目次](#目次) > [正規表現](#正規表現) > [本節](#複雑な正規表現は組み立てることを考える)

```ABAP
CONSTANTS class_name TYPE string VALUE `CL\_.*`.
CONSTANTS interface_name TYPE string VALUE `IF\_.*`.
DATA(object_name) = |{ class_name }\|{ interface_name }|.
```

複雑な正規表現の中には、より基本的な部分からどのように構築されているかを示すと理解しやすくなるものもあります。

## クラス

> [クリーン ABAP](#クリーン-abap) > [目次](#目次) > [本節](#クラス)

### クラス: オブジェクト指向

> [クリーン ABAP](#クリーン-abap) > [目次](#目次) > [クラス](#クラス) > [本節](#クラス-オブジェクト指向)

#### 静的クラスよりもオブジェクトを選ぶ

> [クリーン ABAP](#クリーン-abap) > [目次](#目次) > [クラス](#クラス) > [クラス: オブジェクト指向](#クラス-オブジェクト指向) > [本節](#静的クラスよりもオブジェクトを選ぶ)

静的クラスでは、そもそもオブジェクト指向によって得られるすべての利点が失われてしまいます。
特に、ユニットテストで本番用の依存関係をテストダブルに置き換えることはほぼ不可能です。

クラスやメソッドを静的にするかどうかを考えると、答えはほとんど常に「いいえ」になります。

このルールの例外として認められているのは、単純なユーティリティクラスです。
これらのメソッドは、特定のABAP型との相互作用を容易にします。
これらは完全にステートレスであるだけでなく、ABAPステートメントや組み込み関数のように見えるほど基本的なものです。
差別化要因は、これらのクラスはそれが使用されるコードに密結合されるので、実際にユニットテストでそれらをモックにしたくはないということです。

```ABAP
CLASS /clean/string_utils DEFINITION [...].
  CLASS-METHODS trim
   IMPORTING
     string        TYPE string
   RETURNING
     VALUE(result) TYPE string.
ENDCLASS.

METHOD retrieve.
  DATA(trimmed_name) = /clean/string_utils=>trim( name ).
  result = read( trimmed_name ).
ENDMETHOD.
```

#### 継承よりもコンポジションを選ぶ

> [クリーン ABAP](#クリーン-abap) > [目次](#目次) > [クラス](#クラス) > [クラス: オブジェクト指向](#クラス-オブジェクト指向) > [本節](#継承よりもコンポジションを選ぶ)

継承を使用してクラスの階層を構築することは避けてください。そうではなく、コンポジションを優先してください。

きれいに継承を設計するのは、[リスコフの置換原則](https://en.wikipedia.org/wiki/Liskov_substitution_principle) のようなルールを尊重する必要があるため、難しいです。
また、階層の背後にある指針を理解し、消化する必要があるため、理解するのも難しいです。
継承は、メソッドがサブクラスでしか利用できなくする傾向があるため、再利用を減らします。
また、メンバーの移動や変更には、階層ツリー全体の変更が必要になる傾向があるため、リファクタリングが複雑になります。

コンポジションとは、それぞれが1つの特定の目的を果たす、小さな独立したオブジェクトを設計することを意味します。
これらのオブジェクトは、単純な委譲やファサードパターンによって、より複雑なオブジェクトに組み替えることができます。
コンポジションはより多くのクラスを生成する可能性がありますが、それ以外のデメリットはありません。

この規則があるからといって、正しい場所で継承を使うことをためらわないでください。
[コンポジットデザインパターン](https://en.wikipedia.org/wiki/Composite_pattern) など、継承が適切な用途もあります。
あなたのケースにおいて、継承が本当にデメリットよりもメリットの方が多いか、批判的に自問してみてください。
確信がもてなければ、一般的にコンポジションの方が無難です。

> [インタフェース vs. 抽象クラス](sub-sections/InterfacesVsAbstractClasses.md)
> では、いくつかの詳細を比較しています。

#### 同じクラスにステートフルとステートレスを混在させない

> [クリーン ABAP](#クリーン-abap) > [目次](#目次) > [クラス](#クラス) > [クラス: オブジェクト指向](#クラス-オブジェクト指向)

ステートレスとステートフルプログラミングパラダイムを同じクラスに混在させてはいけません。

ステートレスプログラミングでは、メソッドは入力を受け取り、_副作用なしに_ 出力を生成するため、メソッドがいつ、どのような順序で呼び出されても同じ結果を生成します。

```ABAP
CLASS /clean/xml_converter DEFINITION PUBLIC FINAL CREATE PUBLIC.
  PUBLIC SECTION.
    METHODS convert
      IMPORTING
        file_content  TYPE xstring
      RETURNING
        VALUE(result) TYPE /clean/some_inbound_message.
ENDCLASS.

CLASS /clean/xml_converter IMPLEMENTATION.
  METHOD convert.
    cl_proxy_xml_transform=>xml_xstring_to_abap(
      EXPORTING
        xml       = file_content
        ext_xml   = abap_true
        svar_name = 'ROOT_NODE'
      IMPORTING
        abap_data = result ).
   ENDMETHOD.
ENDCLASS.
```

ステートフルプログラミングでは、オブジェクトの内部状態をメソッドで操作します。つまり、_副作用がいっぱいある_ ということです。

```ABAP
CLASS /clean/log DEFINITION PUBLIC CREATE PUBLIC.
  PUBLIC SECTION.
    METHODS add_message IMPORTING message TYPE /clean/message.
  PRIVATE SECTION.
    DATA messages TYPE /clean/message_table.
ENDCLASS.

CLASS /clean/log IMPLEMENTATION.
  METHOD add_message.
    INSERT message INTO TABLE messages.
  ENDMETHOD.
ENDCLASS.
```

どちらのパラダイムも問題なく、それぞれの用途があります。
しかし、それらを同じオブジェクト内に _混在させる_ と、コードが理解しにくく、不明瞭な持ち越しエラーや同期性の問題で確実に失敗するようになります。
そのようなことはしないでください。

### スコープ

> [クリーン ABAP](#クリーン-abap) > [目次](#目次) > [クラス](#クラス) > [本節](#スコープ)

#### デフォルトではグローバル, 適切な場所でのみローカル

> [クリーン ABAP](#クリーン-abap) > [目次](#目次) > [クラス](#クラス) > [スコープ](#スコープ) > [本節](#デフォルトではグローバル-適切な場所でのみローカル)

デフォルトではグローバルクラスで動作します。
適切な場所でのみローカルクラスを使用します。

> グローバルクラスは、データディクショナリに表示されるクラスです。
> ローカルクラスは、別の開発オブジェクトのインクルード内に存在し、この別のオブジェクトにのみ表示されます。

ローカルクラスは以下の場合に適しています。

- 例えば、ここでしか必要とされないグローバルクラスのデータのイテレータなど、非常に限定されたプライベートなデータ構造のため

- 例えば、特殊な目的のマルチメソッドソート集計アルゴリズムをクラスの残りのコードから分離するなど、複雑なプライベートアルゴリズムを抽出するため

- 例えば、すべてのデータベースアクセスをユニットテストのテストダブルに置き換えられる個別のローカルクラスに抽出することで、グローバルクラスの特定の側面をモックできるようにするため

ローカルクラスは他の場所では使えないので、再利用の妨げになります。
抽出するのは簡単ですが、通常、人々はそれを見つけることすらできず、望まないコードの重複につながります。
非常に長いローカルクラスのインクルードでのオリエンテーション、ナビゲーション、デバッグは面倒で煩わしいものです。
ABAPはインクルードレベルでロックするので、複数の人がローカルインクルードの異なる部分で同時に作業することができなくなります
（別々のグローバルクラスであれば可能です）。

以下のような場合は、ローカルクラスの使用を再考してください。

- ローカルインクルードが、数十ものクラスと数千行ものコードにわたっている場合
- グローバルクラスを他のクラスを保持する「パッケージ」と考えている場合
- グローバルクラスが空っぽになってしまう場合
- 別々のローカルインクルードで重複したコードが繰り返されている場合
- 開発者たちがお互いにロックアウトし始め、並行して作業できなくなる場合
- チームがお互いのローカルサブツリーを理解できなくなったため、バックログの見積もりが大幅に悪化する場合

#### 継承を意図しない場合はFINALにする

> [クリーン ABAP](#クリーン-abap) > [目次](#目次) > [クラス](#クラス) > [スコープ](#スコープ) > [本節](#継承を意図しない場合はFINALにする)

明示的に継承用に設計されていないクラスは `FINAL` にしましょう。

クラス連携を設計するとき、最初の選択は [継承ではなくコンポジション](#継承よりもコンポジションを選ぶ) であるべきです。
継承を有効にすることは、 `PROTECTED` vs. `PRIVATE` や、 [リスコフの置換原則](https://en.wikipedia.org/wiki/Liskov_substitution_principle) のようなことを考える必要があり、
多くの設計内部をフリーズさせてしまうため、軽々しく行うべきことではありません。
クラス設計でこれらのことを考慮していなかった場合は、
クラスを `FINAL` にすることで誤って継承されることを防ぐべきです。

継承にはもちろん、[コンポジット](https://en.wikipedia.org/wiki/Composite_pattern) デザインパターンなど、いくつかの適切な用途が _あります_。
ビジネスアドインもまた、サブクラスを許可して、元のコードの大部分を再利用できるようにすることで、より便利にすることができます。
ただし、これらのケースはすべて最初から設計によって継承が組み込まれていることに注意してください。

[インタフェースを実装](#パブリックインスタンスメソッドはインタフェースの一部でなければならない) しないクリーンでないクラスは、
ユニットテストでそれらをモックすることを可能にするために、非 `FINAL` のままにしておくべきです。

#### メンバーはデフォルトでPRIVATE, 必要な場合にのみPROTECTEDにする

> [クリーン ABAP](#クリーン-abap) > [目次](#目次) > [クラス](#クラス) > [スコープ](#スコープ) > [本節](#メンバーはデフォルトでPRIVATE-必要な場合にのみPROTECTEDにする)

デフォルトでは属性やメソッド、他のクラス・メンバは `PRIVATE` にします。

それらをオーバーライドするサブクラスを有効にしたい場合にのみ、それらを `PROTECTED` にしてください。

クラスの内部は、知る必要がある場合にのみ他者が利用できるようにする必要があります。
これには外部の呼び出し元だけでなく、サブクラスも含まれます。
情報を過剰に利用可能にすると、予期せぬ再定義によって微妙なエラーが発生したり、外部の人がまだ流動的であるべきメンバーを凍結してしまうためにリファクタリングが妨げられたりする可能性があります。

#### getter の代わりにイミュータブルを使用することを考える

> [クリーン ABAP](#クリーン-abap) > [目次](#目次) > [クラス](#クラス) > [スコープ](#スコープ) > [本節](#getter-の代わりにイミュータブルを使用することを考える)

イミュータブルとは、構築後に決して変更されることのないオブジェクトのことです。
この種のオブジェクトでは、getter メソッドの代わりにパブリックな読み取り専用属性を使用することを検討してください。

```ABAP
CLASS /clean/some_data_container DEFINITION.
  PUBLIC SECTION.
    METHODS constructor
      IMPORTING
        a TYPE i
        b TYPE c
        c TYPE d.
    DATA a TYPE i READ-ONLY.
    DATA b TYPE c READ-ONLY.
    DATA c TYPE d READ-ONLY.
ENDCLASS.
```

以下ではなく

```ABAP
CLASS /dirty/some_data_container DEFINITION.
  PUBLIC SECTION.
    METHODS get_a ...
    METHODS get_b ...
    METHODS get_c ...
  PRIVATE SECTION.
    DATA a TYPE i.
    DATA b TYPE c.
    DATA c TYPE d.
ENDCLASS.
```

> **注意**： 値が変化 **する** オブジェクトには、パブリックな読み取り専用属性を使用しないでください。
> そうしないと、他のコードで値が必要とされているかどうかに関わらず、この属性は常に最新の状態に保ち続ける必要があります。

#### READ-ONLY を控えめに使う

> [クリーン ABAP](#クリーン-abap) > [目次](#目次) > [クラス](#クラス) > [スコープ](#スコープ) > [本節](#READ-ONLY-を控えめに使う)

多くのモダンなプログラミング言語、特にJavaでは、偶発的な副作用を防ぐために、クラス・メンバーを適切な場所で読み取り専用にすることを推奨しています。

ABAPはデータ宣言のための `READ-ONLY` 追加機能を提供 _しています_ が、これは控えめに使用することをお勧めします。

第一に、この追加機能は `PUBLIC SECTION` でしか利用できないため、適用範囲が大幅に狭くなっています。
protected メンバや private メンバにも、メソッド内のローカル変数にも追加することはできません。

第二に、この追加機能は他のプログラミング言語から期待されるものとは微妙に異なります。
READ-ONLY データは、クラス自体やその仲間、サブクラス内のどのメソッドからでも自由に変更することができます。
これは、他の言語で見られる、より一般的な「一度書いたら絶対に変更しない」という挙動と矛盾しています。
この違いは、悪い驚きをもたらすかもしれません。

> 誤解を避けるために： 変数を偶発的な変更から保護することは良い習慣です。
> 適切な文があればABAPにも適用することをお勧めします。

### コンストラクタ

> [クリーン ABAP](#クリーン-abap) > [目次](#目次) > [クラス](#クラス) > [本節](#コンストラクタ)

#### CREATE OBJECT よりも NEW を選ぶ

> [クリーン ABAP](#クリーン-abap) > [目次](#目次) > [クラス](#クラス) > [コンストラクタ](#コンストラクタ) > [本節](#CREATE-OBJECT-よりも-NEW-を選ぶ)

```ABAP
DATA object TYPE REF TO /clean/some_number_range.
object = NEW #( '/CLEAN/CXTGEN' )
...
DATA(object) = NEW /clean/some_number_range( '/CLEAN/CXTGEN' ).
...
DATA(object) = CAST /clean/number_range( NEW /clean/some_number_range( '/CLEAN/CXTGEN' ) ).
```

次のように、不必要に長くするのではなく

```ABAP
" アンチパターン
DATA object TYPE REF TO /dirty/some_number_range.
CREATE OBJECT object
  EXPORTING
    number_range = '/DIRTY/CXTGEN'.
```

もちろん、動的な型が必要な場合は除きます。

```ABAP
CREATE OBJECT number_range TYPE (dynamic_type)
  EXPORTING
    number_range = '/CLEAN/CXTGEN'.
```

#### グローバルクラスが CREATE PRIVATE の場合 CONSTRUCTOR は public のままにする

> [クリーン ABAP](#クリーン-abap) > [目次](#目次) > [クラス](#クラス) > [コンストラクタ](#コンストラクタ) > [本節](#グローバルクラスが-CREATE-PRIVATE-の場合-CONSTRUCTOR-は-public-のままにする)

```ABAP
CLASS /clean/some_api DEFINITION PUBLIC FINAL CREATE PRIVATE.
  PUBLIC SECTION.
    METHODS constructor.
```

これ自体が矛盾していることに同意します。
しかし、[ABAPヘルプの _Instance Constructor_](https://help.sap.com/doc/abapdocu_751_index_htm/7.51/en-US/abeninstance_constructor_guidl.htm) の記事によると、正しいコンパイルと構文チェックを保証するためには、`CONSTRUCTOR` は `PUBLIC SECTION` で指定する必要があります。

これはグローバルクラスにのみ適用されます。
ローカルクラスでは、コンストラクタを本来あるべきようにプライベートにします。

#### オプションパラメータよりも複数の静的な生成用メソッドを選ぶ

> [クリーン ABAP](#クリーン-abap) > [目次](#目次) > [クラス](#クラス) > [コンストラクタ](#コンストラクタ) > [本節](#オプションパラメータよりも複数の静的な生成用メソッドを選ぶ)

```ABAP
CLASS-METHODS describe_by_data IMPORTING data TYPE any [...]
CLASS-METHODS describe_by_name IMPORTING name TYPE any [...]
CLASS-METHODS describe_by_object_ref IMPORTING object_ref TYPE REF TO object [...]
CLASS-METHODS describe_by_data_ref IMPORTING data_ref TYPE REF TO data [...]
```

ABAPは[オーバーロード](https://en.wikipedia.org/wiki/Function_overloading) をサポートしていません。
目的のセマンティクスを達成するために、オプションのパラメータではなく、名前のバリエーションを使用します。

```ABAP
" アンチパターン
METHODS constructor
  IMPORTING
    data       TYPE any OPTIONAL
    name       TYPE any OPTIONAL
    object_ref TYPE REF TO object OPTIONAL
    data_ref   TYPE REF TO data OPTIONAL
  [...]
```

一般的なガイドラインの [_OPTIONALパラメータを追加するのではなくメソッドを分割する_](#OPTIONALパラメータを追加するのではなくメソッドを分割する) でその理由を説明しています。

[ビルダーデザインパターン](https://en.wikipedia.org/wiki/Builder_pattern) を使用して、複雑な構築を複数ステップの構築に解決することを考えてみましょう。

#### 複数の生成用メソッドには記述的な名前をつける

> [クリーン ABAP](#クリーン-abap) > [目次](#目次) > [クラス](#クラス) > [コンストラクタ](#コンストラクタ) > [本節](#複数の生成用メソッドには記述的な名前をつける)

生成用メソッド名は `new_` や `create_`、 `construct_` で始めるのがよいでしょう。
人は直感的にこれらをオブジェクトの構築に結びつけます。
また、`new_from_template`、`create_as_copy`、`create_by_name` のような動詞句を追加するのにも適しています。

```ABAP
CLASS-METHODS new_describe_by_data IMPORTING p_data TYPE any [...]
CLASS-METHODS new_describe_by_name IMPORTING p_name TYPE any [...]
CLASS-METHODS new_describe_by_object_ref IMPORTING p_object_ref TYPE REF TO object [...]
CLASS-METHODS new_describe_by_data_ref IMPORTING p_data_ref TYPE REF TO data [...]
```

以下のような無意味なものではなく

```ABAP
" アンチパターン
CLASS-METHODS create_1 IMPORTING p_data TYPE any [...]
CLASS-METHODS create_2 IMPORTING p_name TYPE any [...]
CLASS-METHODS create_3 IMPORTING p_object_ref TYPE REF TO object [...]
CLASS-METHODS create_4 IMPORTING p_data_ref TYPE REF TO data [...]
```

#### 複数のインスタンスが意味をなさない場合にのみシングルトンにする

> [クリーン ABAP](#クリーン-abap) > [目次](#目次) > [クラス](#クラス) > [コンストラクタ](#コンストラクタ) > [本節](#複数のインスタンスが意味をなさない場合にのみシングルトンにする)

```ABAP
METHOD new.
  IF singleton IS NOT BOUND.
    singleton = NEW /clean/my_class( ).
  ENDIF.
  result = singleton.
ENDMETHOD.
```

シングルトンパターンは、オブジェクト指向設計で何かの2番目のインスタンスが意味をなさない場合にのみ適用してください。
例えば、すべての利用者が同じ状態で同じものを使って、同じデータで作業していることを保証したい場合に使います。

シングルトンパターンを習慣的に使ったり、パフォーマンスの法則があるからといって使用しないでください。
これは最も過剰に使われ、誤って適用されているパターンであり、
予期せぬ相互作用を発生させ、テストを不必要に複雑にします。
単一オブジェクトにする設計上の理由がない場合、
その決定は利用者に委ねてください - 利用者は例えばファクトリなど、コンストラクタ以外の手段で同じことができます。

## メソッド

> [クリーン ABAP](#クリーン-abap) > [目次](#目次) > [本節](#メソッド)

これらのルールは、クラスや汎用モジュールのメソッドに適用されます。

### 呼び出し

> [クリーン ABAP](#クリーン-abap) > [目次](#目次) > [メソッド](#メソッド) > [本節](#呼び出し)

#### 手続き的な呼び出しよりも関数的な呼び出しを選ぶ

> [クリーン ABAP](#クリーン-abap) > [目次](#目次) > [メソッド](#メソッド) > [呼び出し](#呼び出し) > [本節](#手続き的な呼び出しよりも関数的な呼び出しを選ぶ)

```ABAP
modify->update( node           = /clean/my_bo_c=>node-item
                key            = item->key
                data           = item
                changed_fields = changed_fields ).
```

次のように、不必要に長くするのではなく

```ABAP
" アンチパターン
CALL METHOD modify->update
  EXPORTING
    node           = /dirty/my_bo_c=>node-item
    key            = item->key
    data           = item
    changed_fields = changed_fields.
```

動的型付けによって関数呼び出しが禁止されている場合は、手続き型を使用します。

```ABAP
CALL METHOD modify->(method_name)
  EXPORTING
    node           = /clean/my_bo_c=>node-item
    key            = item->key
    data           = item
    changed_fields = changed_fields.
```

以下の詳細なルールの多くは、このアドバイスのより具体的なバリエーションに過ぎません。

#### RECEIVING を省略する

> [クリーン ABAP](#クリーン-abap) > [目次](#目次) > [メソッド](#メソッド) > [呼び出し](#呼び出し) > [本節](#RECEIVING-を省略する)

```ABAP
DATA(sum) = aggregate_values( values ).
```

次のように、不必要に長くするのではなく

```ABAP
" アンチパターン
aggregate_values(
  EXPORTING
    values = values
  RECEIVING
    result = DATA(sum) ).
```

#### オプションのキーワード EXPORTING を省略する

> [クリーン ABAP](#クリーン-abap) > [目次](#目次) > [メソッド](#メソッド) > [呼び出し](#呼び出し) > [本節](#オプションのキーワード-EXPORTING-を省略する)

```ABAP
modify->update( node           = /clean/my_bo_c=>node-item
                key            = item->key
                data           = item
                changed_fields = changed_fields ).
```

次のように、不必要に長くするのではなく

```ABAP
" アンチパターン
modify->update(
  EXPORTING
    node           = /dirty/my_bo_c=>node-item
    key            = item->key
    data           = item
    changed_fields = changed_fields ).
```

#### 単一パラメータでの呼び出し時はパラメータ名を省略する

> [クリーン ABAP](#クリーン-abap) > [目次](#目次) > [メソッド](#メソッド) > [呼び出し](#呼び出し) > [本節](#単一パラメータでの呼び出し時はパラメータ名を省略する)

```ABAP
DATA(unique_list) = remove_duplicates( list ).
```

次のように、不必要に長くするのではなく

```ABAP
" アンチパターン
DATA(unique_list) = remove_duplicates( list = list ).
```

しかし、メソッド名だけではわかりにくく、
パラメータ名を繰り返すことでさらにわかりやすくなる場合もあります。

```ABAP
car->drive( speed = 50 ).
update( asynchronous = abap_true ).
```

#### インスタンスメソッドを呼び出す際の自己参照 me を省略する

> [クリーン ABAP](#クリーン-abap) > [目次](#目次) > [メソッド](#メソッド) > [呼び出し](#呼び出し) > [本節](#インスタンスメソッドを呼び出す際の自己参照-me-を省略する)

自己参照 `me->` はシステムによって暗黙的に設定されているので、インスタンスメソッドを呼び出す際には省略してください。

```ABAP
DATA(sum) = aggregate_values( values ).
```

次のように、不必要に長くするのではなく

```ABAP
" アンチパターン
DATA(sum) = me->aggregate_values( values ).
```

### メソッド: オブジェクト指向

> [クリーン ABAP](#クリーン-abap) > [目次](#目次) > [メソッド](#メソッド) > [本節](#メソッド-オブジェクト指向)

#### 静的メソッドよりもインスタンスメソッドを選ぶ

> [クリーン ABAP](#クリーン-abap) > [目次](#目次) > [メソッド](#メソッド) > [メソッド: オブジェクト指向](#メソッド-オブジェクト指向) > [本節](#静的メソッドよりもインスタンスメソッドを選ぶ)

メソッドはデフォルトではインスタンスメンバでなければなりません。
インスタンスメソッドはクラスの「オブジェクトの性質」をよりよく反映します。
ユニットテストでより簡単にモックすることができます。

```ABAP
METHODS publish.
```

メソッドは、静的な生成メソッドのような例外的な場合にのみ静的にすべきです。

```ABAP
CLASS-METHODS create_instance
  RETURNING
    VALUE(result) TYPE REF TO /clean/blog_post.
```

#### パブリックインスタンスメソッドはインタフェースの一部でなければならない

> [クリーン ABAP](#クリーン-abap) > [目次](#目次) > [メソッド](#メソッド) > [メソッド: オブジェクト指向](#メソッド-オブジェクト指向) > [本節](#パブリックインスタンスメソッドはインタフェースの一部でなければならない)

パブリックインスタンスメソッドは常にインタフェースの一部であるべきです。
これにより、依存関係が切り離され、ユニットテストでのモックが簡単になります。

```ABAP
METHOD /clean/blog_post~publish.
```

クリーンなオブジェクト指向では、列挙クラスのように代替の実装がなく、テストケースでモックされることもないような少数の例外を除き、インタフェースのないメソッドを公開することはあまり意味がありません。

> [インタフェース vs. 抽象クラス](sub-sections/InterfacesVsAbstractClasses.md)
> では、これが継承されたメソッドを上書きするクラスにも適用される理由を説明しています。

### パラメータ数

> [クリーン ABAP](#クリーン-abap) > [目次](#目次) > [メソッド](#メソッド) > [本節](#パラメータ数)

#### IMPORTINGパラメータは少なく, 3つ以下を目指す

> [クリーン ABAP](#クリーン-abap) > [目次](#目次) > [メソッド](#メソッド) > [パラメータ数](#パラメータ数) > [本節](#IMPORTINGパラメータは少なく-3つ以下を目指す)

```ABAP
FUNCTION seo_class_copy
  IMPORTING
    clskey      TYPE seoclskey
    new_clskey  TYPE seoclskey
    config      TYPE class_copy_config
  EXPORTING
    ...
```

の方が、次のコードよりもよほどわかりやすい

```ABAP
" アンチパターン
FUNCTION seo_class_copy
  IMPORTING
    clskey                 TYPE seoclskey
    new_clskey             TYPE seoclskey
    access_permission      TYPE seox_boolean DEFAULT seox_true
    VALUE(save)            TYPE seox_boolean DEFAULT seox_true
    VALUE(suppress_corr)   TYPE seox_boolean DEFAULT seox_false
    VALUE(suppress_dialog) TYPE seox_boolean DEFAULT seox_false
    VALUE(authority_check) TYPE seox_boolean DEFAULT seox_true
    lifecycle_manager      TYPE REF TO if_adt_lifecycle_manager OPTIONAL
    lock_handle            TYPE REF TO if_adt_lock_handle OPTIONAL
    VALUE(suppress_commit) TYPE seox_boolean DEFAULT seox_false
  EXPORTING
    ...
```

入力パラメータが多すぎると、指数関数的な数の組み合わせを処理する必要があるため、メソッドの複雑さが爆発的に増大します。
パラメータ数が多いということは、メソッドが複数のことを行っているかもしれないことを示しています。

構造体やオブジェクトと組み合わせて意味のあるセットにすることで、パラメータの数を減らすことができます。

#### OPTIONALパラメータを追加するのではなくメソッドを分割する

> [クリーン ABAP](#クリーン-abap) > [目次](#目次) > [メソッド](#メソッド) > [パラメータ数](#パラメータ数) > [本節](#OPTIONALパラメータを追加するのではなくメソッドを分割する)

```ABAP
METHODS do_one_thing IMPORTING what_i_need TYPE string.
METHODS do_another_thing IMPORTING something_else TYPE i.
```

ABAPは [オーバーロード](https://en.wikipedia.org/wiki/Function_overloading) をサポートしていないため、目的のセマンティックを達成するために

```ABAP
" アンチパターン
METHODS do_one_or_the_other
  IMPORTING
    what_i_need    TYPE string OPTIONAL
    something_else TYPE i OPTIONAL.
```

のようにすると、オプションのパラメータはメソッドの利用者を混乱させます。

- 本当に必須なのはどれか？
- どの組み合わせが有効か？
- お互いに排他的なのはどれか？

ユースケースのための特定のパラメータを持つ複数のメソッドは、どのパラメータの組み合わせが有効で期待されるかを明確にガイダンスすることで、このような混乱を避けることができます。

#### PREFERRED PARAMETER は控えめに使う

> [クリーン ABAP](#クリーン-abap) > [目次](#目次) > [メソッド](#メソッド) > [パラメータ数](#パラメータ数) > [本節](#PREFERRED-PARAMETER-は控えめに使う)

`PREFERRED PARAMETER` を追加すると、実際にどのパラメータが供給されているのかがわかりにくくなり、コードを理解するのが難しくなります。
パラメータの数、特にオプションのパラメータを最小限にすることで、`PREFERRED PARAMETER` の必要性を自然に減らすことができます。

#### RETURN, EXPORT, CHANGE は1つだけのパタメータにする

> [クリーン ABAP](#クリーン-abap) > [目次](#目次) > [メソッド](#メソッド) > [パラメータ数](#パラメータ数) > [本節](#RETURN-EXPORT-CHANGE-は1つだけのパタメータにする)

よいメソッドは _一つのこと_ を行いますが、それはメソッドが正確に一つのことを返すことにも反映されます。
メソッドの出力パラメータが論理的な実体を形成して _いない_ 場合、
そのメソッドは複数のことを行うので、それを分割する必要があります。

出力が複数のものからなる論理的な実体である場合があります。
これらは構造体やオブジェクトを返すことで簡単に表現できます。

```ABAP
TYPES:
  BEGIN OF check_result,
    result      TYPE result_type,
    failed_keys TYPE /bobf/t_frw_key,
    messages    TYPE /bobf/t_frw_message,
  END OF check_result.

METHODS check_business_partners
  IMPORTING
    business_partners TYPE business_partners
  RETURNING
    VALUE(result)     TYPE check_result.
```

次のようにするのではなく

```ABAP
" アンチパターン
METHODS check_business_partners
  IMPORTING
    business_partners TYPE business_partners
  EXPORTING
    result            TYPE result_type
    failed_keys       TYPE /bobf/t_frw_key
    messages          TYPE /bobf/t_frw_message.
```

特に複数の EXPORTING パラメータと比較して、これにより、関数的な呼び出しスタイルを使用することができ、
`IS SUPPLIED` について考える必要がなくなり、重要な `ERROR_OCCURRED` 情報を取得することをうっかり忘れてしまうことを防ぐことができます。

複数のオプションの出力パラメータの代わりに、意味のある呼び出しパターンに従ってメソッドを分割することを検討してください。

```ABAP
TYPES:
  BEGIN OF check_result,
    result      TYPE result_type,
    failed_keys TYPE /bobf/t_frw_key,
    messages    TYPE /bobf/t_frw_message,
  END OF check_result.

METHODS check
  IMPORTING
    business_partners TYPE business_partners
  RETURNING
    VALUE(result)     TYPE result_type.

METHODS check_and_report
  IMPORTING
    business_partners TYPE business_partners
  RETURNING
    VALUE(result)     TYPE check_result.
```

### パラメータの型

> [クリーン ABAP](#クリーン-abap) > [目次](#目次) > [メソッド](#メソッド) > [本節](#パラメータの型)

#### EXPORTING よりも RETURNING を選ぶ

> [クリーン ABAP](#クリーン-abap) > [目次](#目次) > [メソッド](#メソッド) > [パラメータの型](#パラメータの型) > [本節](#EXPORTING-よりも-RETURNING-を選ぶ)

```ABAP
METHODS square
  IMPORTING
    number        TYPE i
  RETURNING
    VALUE(result) TYPE i.

DATA(result) = square( 42 ).
```

次のように、不必要に長くするのではなく

```ABAP
" アンチパターン
METHODS square
  IMPORTING
    number TYPE i
  EXPORTING
    result TYPE i.

square(
  EXPORTING
    number = 42
  IMPORTING
    result = DATA(result) ).
```

`RETURNING` は呼び出しを短くするだけでなく、
メソッドチェーンを可能にし、[入力と出力が同一の場合のエラー](#入力と出力が同一になる場合に注意する) を防ぐことができます。

#### 大きなテーブルの RETURNING は通常OK

> [クリーン ABAP](#クリーン-abap) > [目次](#目次) > [メソッド](#メソッド) > [パラメータの型](#パラメータの型) > [本節](#大きなテーブルの-RETURNING-は通常OK)

ABAP言語のドキュメントやパフォーマンスガイドには、反対のことが記載されていますが、
VALUEパラメータで大きなテーブルや深くネストしているテーブルを渡すことで、_実際に_ パフォーマンスの問題が発生することはほとんどありません。
そのため、一般的には RETURNING でテーブルを返すことをお勧めします。

```ABAP
METHODS get_large_table
  RETURNING
    VALUE(result) TYPE /clean/some_table_type.

METHOD get_large_table.
  result = me->large_table.
ENDMETHOD.

DATA(my_table) = get_large_table( ).
```

個々のケースで実際の証拠（＝悪いパフォーマンス計測値）がある場合に限り、
より面倒な手続き型スタイルに頼るべきです。

```ABAP
" アンチパターン
METHODS get_large_table
  EXPORTING
    result TYPE /dirty/some_table_type.

METHOD get_large_table.
  result = me->large_table.
ENDMETHOD.

get_large_table( IMPORTING result = DATA(my_table) ).
```

> この節は、ABAPプログラミングガイドラインとコードインスペクタのチェックに矛盾しており、
> どちらもパフォーマンスの低下を避けるために、大きなテーブルは参照によって EXPORT すべきであると提案しています。
> 私たちは一貫してパフォーマンス低下とメモリ不足を再現することができず、
> 一般的に RETURNING のパフォーマンスを向上させているカーネル最適化について知ることになりました。

#### RETURNING, EXPORTING, CHANGING は併用せずにどれかを使う

> [クリーン ABAP](#クリーン-abap) > [目次](#目次) > [メソッド](#メソッド) > [パラメータの型](#パラメータの型) > [本節](#RETURNING-EXPORTING-CHANGING-は併用せずにどれかを使う)

```ABAP
METHODS copy_class
  IMPORTING
    old_name      TYPE seoclsname
    new name      TYPE secolsname
  RETURNING
    VALUE(result) TYPE copy_result
  RAISING
    /clean/class_copy_failure.
```

次のように混ぜ合わせるのではなく

```ABAP
" アンチパターン
METHODS copy_class
  ...
  RETURNING
    VALUE(result)      TYPE vseoclass
  EXPORTING
    error_occurred     TYPE abap_bool
  CHANGING
    correction_request TYPE trkorr
    package            TYPE devclass.
```

異なる種類の出力パラメータを使用するということは、そのメソッドが複数のことを行っていることを示しています。
これは読む人を混乱させ、メソッドの呼び出しを不必要に複雑にします。

出力を生成する際に入力を使用するビルダーは、このルールの例外となる場合があります。

```ABAP
METHODS build_tree
  CHANGING
    tokens        TYPE tokens
  RETURNING
    VALUE(result) TYPE REF TO tree.
```

しかし、それらであっても、入力を客観化することで、より明確にすることができます。

```ABAP
METHODS build_tree
  IMPORTING
    tokens        TYPE REF TO token_stack
  RETURNING
    VALUE(result) TYPE REF TO tree.
```

#### CHANGING は適切なところで控えめに使う

> [クリーン ABAP](#クリーン-abap) > [目次](#目次) > [メソッド](#メソッド) > [パラメータの型](#パラメータの型) > [本節](#CHANGING-は適切なところで控えめに使う)

`CHANGING` は、既に入力されている既存のローカル変数が部分的に更新される場合のためだけに使用するべきです。

```ABAP
METHODS update_references
  IMPORTING
    new_reference TYPE /bobf/conf_key
  CHANGING
    bo_nodes      TYPE root_nodes.

METHOD update_references.
  LOOP AT bo_nodes REFERENCE INTO DATA(bo_node).
    bo_node->reference = new_reference.
  ENDLOOP.
ENDMETHOD.
```

`CHANGING` パラメータを提供するだけのために、呼び出し元に不要なローカル変数を強制的に導入させることはしないでください。
以前に空だった変数を最初に埋めるために `CHANGING` パラメータを使用しないでください。

#### ブーリアン型の入力パラメータの代わりにメソッドを分割する

> [クリーン ABAP](#クリーン-abap) > [目次](#目次) > [メソッド](#メソッド) > [パラメータの型](#パラメータの型) > [本節](#ブーリアン型の入力パラメータの代わりにメソッドを分割する)

ブーリアン型の入力パラメータは、しばしばメソッドが1つのことを行うのではなく _2つ_ のことを行っていることを示します。

```ABAP
" アンチパターン
METHODS update
  IMPORTING
    do_save TYPE abap_bool.
```

また、単一の、つまり名前のないブーリアン型パラメータを持つメソッドの呼び出しは、
パラメータの意味を不明瞭にする傾向があります。

```ABAP
" アンチパターン
update( abap_true ).  " what does 'true' mean? synchronous? simulate? commit?
```

メソッドを分割することで、メソッドのコードを単純化し、
異なる意図をより良く記述することができます。

```ABAP
update_without_saving( ).
update_and_save( ).
```

一般的な認識では、ブーリアン変数のセッターは問題ないとされています。

```ABAP
METHODS set_is_deleted
  IMPORTING
    new_value TYPE abap_bool.
```

> 詳細は
> [1](http://www.beyondcode.org/articles/booleanVariables.html) > [2](https://silkandspinach.net/2004/07/15/avoid-boolean-parameters/) > [3](http://jlebar.com/2011/12/16/Boolean_parameters_to_API_functions_considered_harmful..html)

### パラメータ名

> [クリーン ABAP](#クリーン-abap) > [目次](#目次) > [メソッド](#メソッド) > [本節](#パラメータ名)

#### RETURNING パラメータに RESULT と名付けることを考える

> [クリーン ABAP](#クリーン-abap) > [目次](#目次) > [メソッド](#メソッド) > [パラメータ名](#パラメータ名) > [本節](#RETURNING-パラメータに-RESULT-と名付けることを考える)

よいメソッド名は通常、`RETURNING` パラメータにそれ自体の名前を必要としないほどよいものです。
その名前は、メソッド名をオウム返ししたり、明らかなことを繰り返したりするだけとなります。

メンバー名を繰り返すと、余計な `me->` を追加して解決する必要のある競合が発生することもあります。

```ABAP
" アンチパターン
METHODS get_name
  RETURNING
    VALUE(name) TYPE string.

METHOD get_name.
  name = me->name.
ENDMETHOD.
```

このような場合は、パラメータに単に `RESULT` と名付けます。ハンガリアン記法がお好みの場合は `RV_RESULT` のような名前になります。

例えば、メソッドチェーンのために `me` を返すメソッドや、
何かを生成するが生成された実体を返さずにそのキーだけを返すメソッドなど、
それが何を意味しているのか明らかで _ない_ 場合は、`RETURNING` パラメータに名前を付けます。

### パラメータの初期化

> [クリーン ABAP](#クリーン-abap) > [目次](#目次) > [メソッド](#メソッド) > [本節](#パラメータの初期化)

#### EXPORTING 参照パラメータはクリアするか上書きする

> [クリーン ABAP](#クリーン-abap) > [目次](#目次) > [メソッド](#メソッド) > [パラメータの初期化](#パラメータの初期化) > [本節](#EXPORTING-参照パラメータはクリアするか上書きする)

参照パラメータは、あらかじめ埋められている可能性のある既存のメモリ領域を参照します。
信頼性の高いデータを提供するために、それらをクリアまたは上書きしてください。

```ABAP
METHODS square
  EXPORTING
    result TYPE i.

" clear
METHOD square.
  CLEAR result.
  " ...
ENDMETHOD.

" overwrite
METHOD square.
  result = cl_abap_math=>square( 2 ).
ENDMETHOD.
```

> コードインスペクタと Checkman は、決して書き込まれない `EXPORTING` 変数を指摘します。
> これらの静的チェックを使用して、このような不明瞭なエラー原因を回避してください。

##### 入力と出力が同一になる場合に注意する

> [クリーン ABAP](#クリーン-abap) > [目次](#目次) > [メソッド](#メソッド) > [パラメータの初期化](#パラメータの初期化) > [本節](#入力と出力が同一になる場合に注意する)

一般的に、型やデータの宣言の後に、メソッド内で最初にパラメータをクリアするのはよいアイデアです。
これにより、ステートメントを発見しやすくなり、後のステートメントでまだ格納されている値を誤って使用してしまうことを避けることができます。

しかし、パラメータ構成によっては、入力と出力で同じ変数を使用することがあります。
この場合、早期の `CLEAR` は入力値を使用する前に削除してしまい、間違った結果をもたらします。

```ABAP
" アンチパターン
DATA value TYPE i.

square_dirty(
  EXPORTING
    number = value
  IMPORTING
    result = value ).

METHOD square_dirty.
  CLEAR result.
  result = number * number.
ENDMETHOD.
```

このようなメソッドの戻り値を `EXPORTING` から `RETURNING` に変更することを検討してください。
または、単一の計算結果文で `EXPORTING` パラメータを上書きすることも検討してください。
どちらも適合しない場合にのみ、後で `CLEAR` を使用してください。

#### VALUE パラメータをクリアしない

> [クリーン ABAP](#クリーン-abap) > [目次](#目次) > [メソッド](#メソッド) > [パラメータの初期化](#パラメータの初期化) > [本節](#VALUE-パラメータをクリアしない)

`VALUE` で動作するパラメータは、定義では空の新しい独立したメモリ領域として渡されます。
再度クリアしないようにしてください。

```ABAP
METHODS square
  EXPORTING
    VALUE(result) TYPE i.

METHOD square.
  " no need to CLEAR result
ENDMETHOD.
```

`RETURNING` パラメータは常に `VALUE` パラメータなので、クリアする必要はありません。

```ABAP
METHODS square
  RETURNING
    VALUE(result) TYPE i.

METHOD square.
  " no need to CLEAR result
ENDMETHOD.
```

### メソッドボディ

> [クリーン ABAP](#クリーン-abap) > [目次](#目次) > [メソッド](#メソッド) > [本節](#メソッドボディ)

#### 1つのことだけをうまくやる

> [クリーン ABAP](#クリーン-abap) > [目次](#目次) > [メソッド](#メソッド) > [メソッドボディ](#メソッドボディ) > [本節](#1つのことだけをうまくやる)

メソッドは一つのことを行うべきであり、一つのことだけを行うべきです。
それは可能な限り最善の方法で行うべきです。

以下の場合、メソッドはおそらく1つのことを行います。

- [入力パラメータは少なく](#IMPORTINGパラメータは少なく-3つ以下を目指す)
- [ブーリアン型のパラメータを含まない](#ブーリアン型の入力パラメータの代わりにメソッドを分割する)
- [きっちり一つの出力パラメータ](#RETURN-EXPORT-CHANGE-は1つだけのパタメータにする)
- [小さい](#メソッドを小さく保つ)
- [抽象度を1段下げる](#抽象度を1段下げる)
- [例外は1つの型のみをスローする](#例外は1つの型のみをスローする)
- そのメソッドから意味のある他のメソッドを抽出できない
- そのメソッド内の文を意味のあるまとまりに分類できない

#### 正常系かエラー処理に集中する, 両方ではなく

> [クリーン ABAP](#クリーン-abap) > [目次](#目次) > [メソッド](#メソッド) > [メソッドボディ](#メソッドボディ) > [本節](#正常系かエラー処理に集中する-両方ではなく)

[_1つのことだけをうまくやる_](#1つのことだけをうまくやる) というルールを特化したものとして、
メソッドは目的のハッピーパスを処理するか、そうでない場合にはエラー処理の迂回路を処理すべきですが、その両方ではありません。

```ABAP
" アンチパターン
METHOD append_xs.
  IF input > 0.
    DATA(remainder) = input.
    WHILE remainder > 0.
      result = result && `X`.
      remainder = remainder - 1.
    ENDWHILE.
  ELSEIF input = 0.
    RAISE EXCEPTION /dirty/sorry_cant_do( ).
  ELSE.
    RAISE EXCEPTION cx_sy_illegal_argument( ).
  ENDIF.
ENDMETHOD.
```

これは、以下のように分解することができます。

```ABAP
METHOD append_xs.
  validate( input ).
  DATA(remainder) = input.
  WHILE remainder > 0.
    result = result && `X`.
    remainder = remainder - 1.
  ENDWHILE.
ENDMETHOD.

METHOD validate.
  IF input = 0.
    RAISE EXCEPTION /dirty/sorry_cant_do( ).
  ELSEIF input < 0.
    RAISE EXCEPTION cx_sy_illegal_argument( ).
  ENDIF.
ENDMETHOD.
```

または、検証の部分を強調するために次のように書くこともできます。

```ABAP
METHOD append_xs.
  IF input > 0.
    result = append_xs_without_check( input ).
  ELSEIF input = 0.
    RAISE EXCEPTION /dirty/sorry_cant_do( ).
  ELSE.
    RAISE EXCEPTION cx_sy_illegal_argument( ).
  ENDIF.
ENDMETHOD.

METHOD append_xs_without_check.
  DATA(remainder) = input.
  WHILE remainder > 0.
    result = result && `X`.
    remainder = remainder - 1.
  ENDWHILE.
ENDMETHOD.
```

#### 抽象度を1段下げる

> [クリーン ABAP](#クリーン-abap) > [目次](#目次) > [メソッド](#メソッド) > [メソッドボディ](#メソッドボディ) > [本節](#抽象度を1段下げる)

メソッド内のステートメントは、メソッド自体よりも1つ下の抽象度でなければなりません。
これに対応して、それらはすべて同じ抽象度でなければなりません。

```ABAP
METHOD create_and_publish.
  post = create_post( user_input ).
  post->publish( ).
ENDMETHOD.
```

次のように、低レベル(`trim`、 `to_upper`、 ...)と高レベル(`publish`、 ...)の概念を混ぜ合わせるのではなく

```ABAP
" アンチパターン
METHOD create_and_publish.
  post = NEW blog_post( ).
  DATA(user_name) = trim( to_upper( sy-uname ) ).
  post->set_author( user_name ).
  post->publish( ).
ENDMETHOD.
```

抽象化の正しいレベルを知るための信頼性の高い方法は、
メソッドの作者に、コードを見ずに、そのメソッドが何をするのかを短い言葉で説明してもらうことです。
箇条書きの数字は、メソッドが呼び出すべきサブメソッド、または実行すべきステートメントです。

#### メソッドを小さく保つ

> [クリーン ABAP](#クリーン-abap) > [目次](#目次) > [メソッド](#メソッド) > [メソッドボディ](#メソッドボディ) > [本節](#メソッドを小さく保つ)

メソッドのステートメント数は20未満であるべきで、3～5ステートメント程度が最適です。

```ABAP
METHOD read_and_parse_version_filters.
  DATA(active_model_version) = read_random_version_under( model_guid ).
  DATA(filter_json) = read_model_version_filters( active_model_version-guid ).
  result = parse_model_version_filters( filter_json ).
ENDMETHOD.
```

以下の `DATA` 宣言だけで、周囲のメソッドが一つ以上のことをしていることがわかります。

```ABAP
" アンチパターン
DATA:
  class           TYPE vseoclass,
  attributes      TYPE seoo_attributes_r,
  methods         TYPE seoo_methods_r,
  events          TYPE seoo_events_r,
  types           TYPE seoo_types_r,
  aliases         TYPE seoo_aliases_r,
  implementings   TYPE seor_implementings_r,
  inheritance     TYPE vseoextend,
  friendships     TYPE seof_friendships_r,
  typepusages     TYPE seot_typepusages_r,
  clsdeferrds     TYPE seot_clsdeferrds_r,
  intdeferrds     TYPE seot_intdeferrds_r,
  attribute       TYPE vseoattrib,
  method          TYPE vseomethod,
  event           TYPE vseoevent,
  type            TYPE vseotype,
  alias           TYPE seoaliases,
  implementing    TYPE vseoimplem,
  friendship      TYPE seofriends,
  typepusage      TYPE vseotypep,
  clsdeferrd      TYPE vseocdefer,
  intdeferrd      TYPE vseoidefer,
  new_clskey_save TYPE seoclskey.
```

もちろん、大きなメソッドをさらに減らすのは意味がない場合もあります。
これは、メソッドが [1つのことに集中している](#1つのことだけをうまくやる) 限り、全く問題ありません。

```ABAP
METHOD decide_what_to_do.
  CASE temperature.
    WHEN burning.
      result = air_conditioning.
    WHEN hot.
      result = ice_cream.
    WHEN moderate.
      result = chill.
    WHEN cold.
      result = skiing.
    WHEN freezing.
      result = hot_cocoa.
  ENDCASE.
ENDMETHOD.
```

しかし、冗長なコードをより適切なパターンがないか検証することは、まだ意味があります。

```ABAP
METHOD decide_what_to_do.
  result = VALUE #( spare_time_activities[ temperature = temperature ] OPTIONAL ).
ENDMETHOD.
```

> メソッドを非常に細かくすると、メソッドの呼び出し回数が増えるため、パフォーマンスに悪影響を及ぼす可能性があります。
> [_パフォーマンスに注意する_](#パフォーマンスに注意する) では、Clean Code とパフォーマンスのバランスをとる方法についてのガイダンスを提供しています。

### 制御フロー

> [クリーン ABAP](#クリーン-abap) > [目次](#目次) > [メソッド](#メソッド) > [本節](#制御フロー)

#### フェイルファースト

> [クリーン ABAP](#クリーン-abap) > [目次](#目次) > [メソッド](#メソッド) > [制御フロー](#制御フロー) > [本節](#フェイルファースト)

できるだけ早めに検証して失敗させましょう。

```ABAP
METHOD do_something.
  IF input IS INITIAL.
    RAISE EXCEPTION cx_sy_illegal_argument( ).
  ENDIF.
  DATA(massive_object) = build_expensive_object_from( input ).
  result = massive_object->do_some_fancy_calculation( ).
ENDMETHOD.
```

後になってからの検証では、発見や理解が難しく、その時点までにすでに無駄なリソースを使ってしまっている可能性があります。

```ABAP
" アンチパターン
METHOD do_something.
  DATA(massive_object) = build_expensive_object_from( input ).
  IF massive_object IS NOT BOUND. " happens if input is initial
    RAISE EXCEPTION cx_sy_illegal_argument( ).
  ENDIF.
  result = massive_object->do_some_fancy_calculation( ).
ENDMETHOD.
```

#### CHECK vs. RETURN

> [クリーン ABAP](#クリーン-abap) > [目次](#目次) > [メソッド](#メソッド) > [制御フロー](#制御フロー) > [本節](#check-vs-return)

入力が期待に沿わない場合、`CHECK` や `RETURN` を使ってメソッドを終了させるべきかどうかについては、コンセンサスがありません。

`CHECK` は確かに短い構文を提供していますが

```ABAP
METHOD read_customizing.
  CHECK keys IS NOT INITIAL.
  " do whatever needs doing
ENDMETHOD.
```

ステートメントの名前は、条件が失敗した場合に何が起こるかを明らかにしないので、人々はおそらく長い形式の方が理解しやすいでしょう。

```ABAP
METHOD read_customizing.
  IF keys IS INITIAL.
    RETURN.
  ENDIF.
  " do whatever needs doing
ENDMETHOD.
```

検証を逆にしてシングルリターン制御フローを採用することで、質問を完全に回避することができます。

```ABAP
METHOD read_customizing.
  IF keys IS NOT INITIAL.
    " do whatever needs doing
  ENDIF.
ENDMETHOD.
```

いずれにしても、何も返さないことが本当に適切な動作なのかどうかを考えてみてください。
メソッドは意味のある結果、つまり、値の入った戻り値のパラメータか、例外のどちらかをを提供しなければなりません。
何も返さないというのは、多くの場合 `null` を返すのと似ていますが、これは避けるべきです。

> [ABAP プログラミングガイドラインの _Exiting Procedures_](https://help.sap.com/doc/abapdocu_751_index_htm/7.51/en-US/index.htm?file=abenexit_procedure_guidl.htm) の節では、この場合に `CHECK` を使用することを推奨しています。
> コミュニティでの議論では、この文は非常に不明瞭なので、多くの人がプログラムの動作を理解できないことが示唆されています。

#### 他の場所での CHECK は避ける

> [クリーン ABAP](#クリーン-abap) > [目次](#目次) > [メソッド](#メソッド) > [制御フロー](#制御フロー) > [本節](#他の場所での-CHECK-は避ける)

メソッドの初期化セクション以外で `CHECK` を使用しないでください。
ステートメントの挙動は、その位置によって変化し、不明瞭で予想外の結果を招く可能性があります。

例えば、[`LOOP` 内の `CHECK` は現在のイテレーションを終了して次のイテレーションに進みます](https://help.sap.com/doc/abapdocu_752_index_htm/7.52/en-US/abapcheck_loop.htm)。
メソッドを終了したり、ループを抜けることを誤って期待してしまう人もいるかもしれません。
`CONTINUE` はループの中でしか使えないので、代わりに `CONTINUE` と組み合わせて `IF` 文を使うことをお勧めします。

> [ABAP プログラミングガイドラインの _Exiting Procedures_](https://help.sap.com/doc/abapdocu_751_index_htm/7.51/en-US/index.htm?file=abenexit_procedure_guidl.htm) の節に基づいています。
> これは、[ループ内の `CHECK` のキーワードリファレンス](https://help.sap.com/doc/abapdocu_752_index_htm/7.52/en-US/abapcheck_loop.htm) と矛盾していることに注意してください。

## エラー処理

> [クリーン ABAP](#クリーン-abap) > [目次](#目次) > [本節](#エラー処理)

### メッセージ

> [クリーン ABAP](#クリーン-abap) > [目次](#目次) > [エラー処理](#エラー処理) > [本節](#メッセージ)

#### メッセージを見つけやすくする

> [クリーン ABAP](#クリーン-abap) > [目次](#目次) > [エラー処理](#エラー処理) > [メッセージ](#メッセージ) > [本節](#メッセージを見つけやすくする)

トランザクションSE91からの使用先検索でメッセージを見つけやすくするために、以下のパターンを使用してください。

```ABAP
MESSAGE e001(ad) INTO DATA(message).
```

変数 `message` が不要な場合は、`##NEEDED` というプラグマを追加します。

```ABAP
MESSAGE e001(ad) INTO DATA(message) ##NEEDED.
```

以下のようなコードは避けてください。

```ABAP
" アンチパターン
IF 1 = 2. MESSAGE e001(ad). ENDIF.
```

これは次の理由によりアンチパターンです。

- 到達不可能なコードが含まれています。
- 決して真になりえない等価条件をテストします。

### リターンコード

> [クリーン ABAP](#クリーン-abap) > [目次](#目次) > [エラー処理](#エラー処理) > [本節](#リターンコード)

#### リターンコードよりも例外を選ぶ

> [クリーン ABAP](#クリーン-abap) > [目次](#目次) > [エラー処理](#エラー処理) > [リターンコード](#リターンコード) > [本節](#リターンコードよりも例外を選ぶ)

```ABAP
METHOD try_this_and_that.
  RAISE EXCEPTION NEW cx_failed( ).
ENDMETHOD.
```

次のようにするのではなく

```ABAP
" アンチパターン
METHOD try_this_and_that.
  error_occurred = abap_true.
ENDMETHOD.
```

例外にはリターンコードよりも複数の利点があります。

- 例外はメソッドのシグネチャをクリーンな状態に保ちます。
  `RETURNING` パラメータとしてメソッドの結果を返し、それと並行して例外を投げることができます。
  リターンコードは、エラー処理のための追加のパラメータでシグネチャを汚します。

- 呼び出し側はすぐに反応する必要はありません。
  シンプルに自分のコードの正常系処理を書けばいいのです。
  例外処理の `CATCH` は、メソッドの最後にあってもいいし、完全にメソッドの外側にあってもいいのです。
  
- 例外は、その属性やメソッドを通じてエラーの詳細を提供することができます。
  リターンコードでは、ログも返すなど、独自に別の解決策を考案する必要があります。

- 環境は、呼び出し元に例外を処理するための構文エラーを通知します。
  リターンコードは誰にも気づかれずに誤って無視されてしまうことがあります。

#### エラーを取り逃がさない

> [クリーン ABAP](#クリーン-abap) > [目次](#目次) > [エラー処理](#エラー処理) > [リターンコード](#リターンコード) > [本節](#エラーを取り逃がさない)

自分の管理下ではない関数や古いコードを呼び出すなどしてリターンコードを使用しなければならない場合は、
エラーを取り逃がすことのないようにしてください。

```ABAP
DATA:
  current_date TYPE string,
  response     TYPE bapiret2.

CALL FUNCTION 'BAPI_GET_CURRENT_DATE'
  IMPORTING
    current_date = current_date
  CHANGING
    response     = response.

IF response-type = 'E'.
  RAISE EXCEPTION NEW /clean/some_error( );
ENDIF.
```

### 例外

> [クリーン ABAP](#クリーン-abap) > [目次](#目次) > [エラー処理](#エラー処理) > [本節](#例外)

#### 例外はエラーのために使い, 通常のケースでは使用しない

> [クリーン ABAP](#クリーン-abap) > [目次](#目次) > [エラー処理](#エラー処理) > [例外](#例外) > [本節](#例外はエラーのために使い-通常のケースでは使用しない)

```ABAP
" アンチパターン
METHODS entry_exists_in_db
  IMPORTING
    key TYPE char10
  RAISING
    cx_not_found_exception.
```

通常の、有効なケースであれば、通常の結果パラメータで処理されるべきです。

```ABAP
METHODS entry_exists_in_db
  IMPORTING
    key           TYPE char10
  RETURNING
    VALUE(result) TYPE abap_bool.
```

例外は、予期しない場合やエラーの状況を反映した場合にのみ使用してください。

```ABAP
METHODS assert_user_input_is_valid
  IMPORTING
    user_input TYPE string
  RAISING
    cx_bad_user_input.
```

例外の使い方を誤ると、実際には何も問題がないのに、何か問題が起きたようにコードを読む人に誤解を与えてしまいます。
また、例外は構築する必要があり、多くのコンテキスト情報を収集することが多いため、通常のコードよりもはるかに遅くなります。

#### クラスベースの例外を使う

> [クリーン ABAP](#クリーン-abap) > [目次](#目次) > [エラー処理](#エラー処理) > [例外](#例外) > [本節](#クラスベースの例外を使う)

```ABAP
TRY.
    get_component_types( ).
  CATCH cx_has_deep_components_error.
ENDTRY.
```

時代遅れの非クラスベースの例外はリターンコードと同じ機能を持っているので、もう使うべきではありません。

```ABAP
" アンチパターン
get_component_types(
  EXCEPTIONS
    has_deep_components = 1
    OTHERS              = 2 ).
```

### スロー

> [クリーン ABAP](#クリーン-abap) > [目次](#目次) > [エラー処理](#エラー処理) > [本節](#スロー)

#### 独自のスーパークラスを使う

> [クリーン ABAP](#クリーン-abap) > [目次](#目次) > [エラー処理](#エラー処理) > [スロー](#スロー) > [本節](#独自のスーパークラスを使う)

```ABAP
CLASS cx_fra_static_check DEFINITION ABSTRACT INHERITING FROM cx_static_check.
CLASS cx_fra_no_check DEFINITION ABSTRACT INHERITING FROM cx_no_check.
```
ABAP標準クラスを直接継承するのではなく、
アプリケーションの各例外タイプ用の抽象スーパークラスを作成することを検討してください。
これにより、すべての _アプリケーションの_ 例外を `CATCH` できるようになりますし、
特殊なテキスト処理など、すべての例外に共通の機能を追加することができます。
`ABSTRACT` は、これらの非記述的なエラーを誤って直接使用してしまうことを防ぎます。

#### 例外は1つの型のみをスローする

> [クリーン ABAP](#クリーン-abap) > [目次](#目次) > [エラー処理](#エラー処理) > [スロー](#スロー) > [本節](#例外は1つの型のみをスローする)

```ABAP
METHODS generate
  RAISING
    cx_generation_error.
```

大多数のケースでは、複数の型の例外を投げても何の意味もありません。
呼び出し側は通常、エラーの状況に興味もないし、それを区別することもできません。
そのため、呼び出し元は一般的に、すべての例外を同じように処理することになります。
そして、もしそうであるなら、なぜ最初にそれらを区別するのでしょうか?

```ABAP
" アンチパターン
METHODS generate
  RAISING
    cx_abap_generation
    cx_hdbr_access_error
    cx_model_read_error.
```

異なるエラー状況を認識するためのよりよい解決策は、1 つの例外タイプを使用して、
[呼び出し元がエラー状況を区別できるようにするためにサブクラスを使う](#呼び出し元がエラー状況を区別できるようにするためにサブクラスを使う) で説明されているように、
個々のエラー状況に反応できるようにする（ただし必須ではありません）サブクラスを追加することです。

#### 呼び出し元がエラー状況を区別できるようにするためにサブクラスを使う

> [クリーン ABAP](#クリーン-abap) > [目次](#目次) > [エラー処理](#エラー処理) > [スロー](#スロー) > [本節](#呼び出し元がエラー状況を区別できるようにするためにサブクラスを使う)

```ABAP
CLASS cx_bad_generation_variable DEFINITION INHERITING FROM cx_generation_error.
CLASS cx_bad_code_composer_template DEFINITION INHERITING FROM cx_generation_error.

TRY.
    generator->generate( ).
  CATCH cx_bad_generation_variable.
    log_failure( ).
  CATCH cx_bad_code_composer_template INTO DATA(bad_template_exception).
    show_error_to_user( bad_template_exception ).
  CATCH cx_generation_error INTO DATA(other_exception).
    RAISE EXCEPTION NEW cx_application_error( previous =  other_exception ).
ENDTRY.
```

多くの異なるエラー状況がある場合は、代わりにエラーコードを使用します。

```ABAP
CLASS cx_generation_error DEFINITION ...
  PUBLIC SECTION.
    TYPES error_code_type TYPE i.
    CONSTANTS:
      BEGIN OF error_code_enum,
        bad_generation_variable    TYPE error_code_type VALUE 1,
        bad_code_composer_template TYPE error_code_type VALUE 2,
        ...
      END OF error_code_enum.
    DATA error_code TYPE error_code_type.

TRY.
    generator->generate( ).
  CATCH cx_generation_error INTO DATA(exception).
    CASE exception->error_code.
      WHEN cx_generation_error=>error_code_enum-bad_generation_variable.
      WHEN cx_generation_error=>error_code_enum-bad_code_composer_variable.
      ...
    ENDCASE.
ENDTRY.
```

#### 管理可能な例外のために CX_STATIC_CHECK をスローする

> [クリーン ABAP](#クリーン-abap) > [目次](#目次) > [エラー処理](#エラー処理) > [スロー](#スロー) > [本節](#管理可能な例外のために-CX_STATIC_CHECK-をスローする)

例外が発生することが予想され、キャッチした側が合理的に処理できる場合、
（ユーザ入力の検証に失敗した場合や、フォールバックが存在するリソースが見つからないなどの場合）
`CX_STATIC_CHECK` を継承した検査例外をスローします。

```ABAP
CLASS cx_file_not_found DEFINITION INHERITING FROM cx_static_check.

METHODS read_file
  IMPORTING
    file_name_enterd_by_user TYPE string
  RAISING
    cx_file_not_found.
```

この例外型はメソッドシグネチャで与え _られなければならず_、構文エラーを回避するためには、例外をキャッチするか伝播 _させなければなりません_。
したがって、利用者にとってはわかりやすく、予期せぬ例外に驚くことなく、エラー状況への対応に気を配ることができるようになります。

> これは[ABAPプログラミングガイドライン](https://help.sap.com/doc/abapdocu_751_index_htm/7.51/en-US/abenexception_category_guidl.htm) と同期していますが、
> [Robert C. Martin の _Clean Code_]と矛盾しており、
> そちらでは非検査例外を選ぶことを推奨しています。
> [例外](sub-sections/Exceptions.md) で理由を説明しています。

#### 通常は回復不可能な場合に CX_NO_CHECK をスローする

> [クリーン ABAP](#クリーン-abap) > [目次](#目次) > [エラー処理](#エラー処理) > [スロー](#スロー) > [本節](#通常は回復不可能な場合に-CX_NO_CHECK-をスローする)

例外が非常に深刻で、キャッチした側がそこから回復する可能性が低い場合、
（必須リソースの読み込みに失敗した場合や、要求された依存関係の解決に失敗した場合など）
`CX_NO_CHECK` を使用してください。

```ABAP
CLASS cx_out_of_memory DEFINITION INHERITING FROM cx_no_check.

METHODS create_guid
  RETURNING
    VALUE(result) TYPE /bobf/conf_key.
```

`CX_NO_CHECK` は、メソッドシグネチャで宣言することが _できません_。
そのため、発生すると、利用者には悪い驚きとなります。
回復不可能な状況の場合には、利用者はいずれにせよそれについて何も有用なことができないので、これは問題ありません。

しかし、利用者が実際にこの種のエラーを認識して対応したい場合もある _かもしれません_。
例えば、依存関係マネージャは、要求されたインタフェースの実装を提供できない場合、通常のアプリケーションコードが継続できなくなるため、`CX_NO_CHECK` を投げるかもしれません。
しかし、動作しているかどうかを確認するためだけにあらゆる種類のものをインスタンス化しようとするテストレポートがあるかもしれず、
それはエラーを単にリストの赤い項目として報告するでしょう。
このサービスは、強制的にダンプされるのではなく、例外をキャッチして無視できるようにしなければなりません。

#### 回避可能な例外には CX_DYNAMIC_CHECK を検討する

> [クリーン ABAP](#クリーン-abap) > [目次](#目次) > [エラー処理](#エラー処理) > [スロー](#スロー) > [本節](#回避可能な例外には-CX_DYNAMIC_CHECK-を検討する)

`CX_DYNAMIC_CHECK` の活用事例は稀であり、
一般的には他の例外型を使用することをお勧めします。
しかし、呼び出し元が例外が発生するかどうかを完全に意識的に制御できる場合には、`CX_STATIC_CHECK` の代わりにこの種の例外を検討してもよいかもしれません．

```ABAP
DATA value TYPE decfloat.
value = '7.13'.
cl_abap_math=>get_db_length_decs(
  EXPORTING
    in     = value
  IMPORTING
    length = DATA(length) ).
```

例えば、浮動小数点数の桁数と小数点以下の桁数を教えてくれる
`cl_abap_math` クラスの `get_db_length_decs` メソッドを考えてみましょう。
このメソッドは、入力パラメータが10進浮動小数点数を反映していない場合、
動的例外 `cx_parameter_invalid_type` を発生させます。
通常、このメソッドは完全に静的に型付けされた変数に対して呼び出されるので、
開発者はその例外が発生する可能性があるかどうかを知っています。
この場合、動的例外により、呼び出し元は不要な `CATCH` 句を省略することができます。

#### まったく回復不可能な状況ではダンプする

> [クリーン ABAP](#クリーン-abap) > [目次](#目次) > [エラー処理](#エラー処理) > [スロー](#スロー) > [本節](#まったく回復不可能な状況ではダンプする)

状況が非常に深刻で、エラーを受け取った側がそこから回復する可能性が低いと完全に確信している場合や、
明らかにプログラミングエラーを示している場合は、例外を投げるのではなくダンプしてください。
メモリの取得に失敗した場合や、埋めなければならないテーブルのインデックス読み取りに失敗した場合などです。

```ABAP
RAISE SHORTDUMP TYPE cx_sy_create_object_error.  " >= NW 7.53
MESSAGE x666(general).                           " < NW 7.53
```

これにより、どのような種類の利用者も、その後、何か有用なことをすることができなくなります。
そのためダンプは、確信がある場合にのみ使用してください。

#### RAISE EXCEPTION TYPE よりも RAISE EXCEPTION NEW を選ぶ

> [クリーン ABAP](#クリーン-abap) > [目次](#目次) > [エラー処理](#エラー処理) > [スロー](#スロー) > [本節](#RAISE-EXCEPTION-TYPE-よりも-RAISE-EXCEPTION-NEW-を選ぶ)

注：NW 7.52 以降から利用可能。

```ABAP
RAISE EXCEPTION NEW cx_generation_error( previous = exception ).
```

は、一般的に、不必要に長い次のコードよりも短くなります。

```ABAP
RAISE EXCEPTION TYPE cx_generation_error
  EXPORTING
    previous = exception.
```

しかし、`MESSAGE` を大量に使用するならば、`TYPE` を使用した方がいいかもしれません。

```ABAP
RAISE EXCEPTION TYPE cx_generation_error
  EXPORTING
    previous = exception
  MESSAGE e136(messages).
```

### キャッチ

> [クリーン ABAP](#クリーン-abap) > [目次](#目次) > [エラー処理](#エラー処理) > [本節](#キャッチ)

#### 外部の例外はコードに侵食しないようラップする

> [クリーン ABAP](#クリーン-abap) > [目次](#目次) > [エラー処理](#エラー処理) > [キャッチ](#キャッチ) > [本節](#外部の例外はコードに侵食しないようラップする)

```ABAP
METHODS generate RAISING cx_generation_failure.

METHOD generate.
  TRY.
      generator->generate( ).
    CATCH cx_amdp_generation_failure INTO DATA(exception).
      RAISE EXCEPTION NEW cx_generation_failure( previous = exception ).
  ENDTRY.
ENDMETHOD.
```

[デメテルの法則](https://en.wikipedia.org/wiki/Law_of_Demeter) では、非干渉化することを推奨しています。
他のコンポーネントからの例外を転送することは、この原則に違反します。
これらの例外をキャッチし、独自の例外型でラップすることで、外部のコードから独立したものにしてください。

```ABAP
" アンチパターン
METHODS generate RAISING cx_sy_gateway_failure.

METHOD generate.
  generator->generate( ).
ENDMETHOD.
```

## コメント

> [クリーン ABAP](#クリーン-abap) > [目次](#目次) > [本節](#コメント)

### コメントではなくコードで表現する

> [クリーン ABAP](#クリーン-abap) > [目次](#目次) > [コメント](#コメント) > [本節](#コメントではなくコードで表現する)

```ABAP
METHOD correct_day_to_last_in_month.
  WHILE is_invalid( date ).
    reduce_day_by_one( CHANGING date = date ).
  ENDWHILE.
ENDMETHOD.

METHOD is_invalid.
  DATA zero_if_invalid TYPE i.
  zero_if_invalid = date.
  result = xsdbool( zero_if_invalid = 0 ).
ENDMETHOD.

METHOD reduce_day_by_one.
  date+6(2) = date+6(2) - 1.
ENDMETHOD.
```

次のようにするのではなく

```ABAP
" アンチパターン
" 例えば、非うるう年の2/29を修正するだけでなく、例えば6/31を6/30に修正する
METHOD fix_day_overflow.
  DO 3 TIMES.
    " 31日 - 28日 = 3 なので、最大3回ループすればOK
    lv_dummy = cv_date.
    " ABAPの仕様実装により、date値が存在しない日付の場合 lv_dummy は 0 となる
    IF ( lv_dummy EQ 0 ).
      cv_date+6(2) = cv_date+6(2) - 1. " 与えられた日付から -1 する
    ELSE.
      " 日付は存在するので修正不要
      EXIT.
    ENDIF.
  ENDDO.
ENDMETHOD.
```

クリーンコードは、コードにコメントをつけることを禁止している _わけではありません_。
あなたが _よりよい_ 手段を考え出すことを奨励しており、それがうまくいかない場合にのみコメントに頼ることを奨励しています。

> この例はパフォーマンスの観点から議論を引き起こしており、
> メソッドをこれほど小さく分けるとパフォーマンスが非常に悪化すると主張されています。
> サンプル測定では、リファクタリングされたコードはオリジナルの汚いコードよりも2.13倍遅いことを示しています。
> クリーンなコードは `31-02-2018` の入力を修正するのに9.6マイクロ秒かかりますが、汚いコードは4.5マイクロ秒しかかかりません。
> これは、このメソッドが高性能なアプリケーションで頻繁に実行される場合に問題になるかもしれませんが、
> 通常のユーザー入力の検証では受け入れられるはずです。
> クリーンコードとパフォーマンスの問題に対処するためには、[パフォーマンスに注意する](#パフォーマンスに注意する) の節を参照してください。

### 不適切な命名をコメントで補おうとしない

> [クリーン ABAP](#クリーン-abap) > [目次](#目次) > [コメント](#コメント) > [本節](#不適切な命名をコメントで補おうとしない)

```ABAP
DATA(input_has_entries) = has_entries( input ).
```

名前の本当の意味や、不適切な名前を選んだ理由を説明するのではなく、よりよい名前を使いましょう。

```ABAP
" アンチパターン
" checks whether the table input contains entries
DATA(result) = check_table( input ).
```

### コードをセグメント化するのにコメントではなくメソッドを使用する

> [クリーン ABAP](#クリーン-abap) > [目次](#目次) > [Comments](#comments) > [本節](#コードをセグメント化するのにコメントではなくメソッドを使用する)

```ABAP
DATA(statement) = build_statement( ).
DATA(data) = execute_statement( statement ).
```

これにより、コードの意図、構造、依存関係がより明確になるだけでなく、
セクション間で一時変数が適切にクリアされていない場合のキャリーオーバーエラーを回避することができます。

```ABAP
" アンチパターン
" -----------------
" Build statement
" -----------------
DATA statement TYPE string.
statement = |SELECT * FROM d_document_roots|.

" -----------------
" Execute statement
" -----------------
DATA(result_set) = adbc->execute_sql_query( statement ).
result_set->next_package( IMPORTING data = data ).
```

### 何をではなく, なぜを説明するためにコメントを書く

> [クリーン ABAP](#クリーン-abap) > [目次](#目次) > [コメント](#コメント) > [本節](#何をではなく-なぜを説明するためにコメントを書く)

```ABAP
" すでに1行以上存在することを検証済みなのでエラーにはならない
DATA(first_line) = table[ 1 ].
```

自然言語でコードが何をしているかを書く必要はありません。

```ABAP
" アンチパターン
" キーによりデータベースから alert root を取得する
SELECT * FROM d_alert_root WHERE key = key.
```

### 設計はコードではなく設計ドキュメントに書く

> [クリーン ABAP](#クリーン-abap) > [目次](#目次) > [コメント](#コメント) > [本節](#設計はコードではなく設計ドキュメントに書く)

```ABAP
" アンチパターン
" このクラスには二重の目的があります まず、一つのことをします。次に別のことをします。
" これは、ローカルのヘルパークラスに分散された多くのコードを実行することによって行われます。
" 何が起こっているのかを理解するために、まず宇宙の性質を考えてみましょう。
" 詳しくはあちらこちらを見てください。
```

冗談抜きに、誰も読みません。
あなたのコードを使うために教科書を読まなければならない場合、
コードには他の方法で解決しなければならない深刻な設計上の問題があることを示している可能性があります。
コードによっては、1行のコメントを超えた説明が必要なものも _あります_。
このような場合には、設計ドキュメントをリンクすることを検討してください。

### コメントには * ではなく " を使う

> [クリーン ABAP](#クリーン-abap) > [目次](#目次) > [コメント](#コメント) > [本節](#コメントには--ではなく--を使う)

クォートのついたコメントは、コメントする文と一緒にインデントします。

```ABAP
METHOD do_it.
  IF input IS NOT INITIAL.
    " delegate pattern
    output = calculate_result( input ).
  ENDIF.
ENDMETHOD.
```

アスタリスクのついたコメントは、変な位置にインデントされる傾向があります。

```ABAP
" アンチパターン
METHOD do_it.
  IF input IS NOT INITIAL.
* delegate pattern
    output = calculate_result( input ).
  ENDIF.
ENDMETHOD.
```

### 関連する文の前にコメントを置く

> [クリーン ABAP](#クリーン-abap) > [目次](#目次) > [コメント](#コメント) > [本節](#関連する文の前にコメントを置く)

```ABAP
" delegate pattern
output = calculate_result( input ).
```

これは次のコードよりもより明確です。

```ABAP
" アンチパターン
output = calculate_result( input ).
" delegate pattern
```

また、次のコードよりも侵食が少ないです。

```ABAP
output = calculate_result( input ).  " delegate pattern
```

### コメントアウトではなくコードを削除する

> [クリーン ABAP](#クリーン-abap) > [目次](#目次) > [コメント](#コメント) > [本節](#コメントアウトではなくコードを削除する)

```ABAP
" アンチパターン
* output = calculate_result( input ).
```

このようなものを見つけたら、削除してください。
アプリケーションが動作し、すべてのテストが通っているなら、そのコードは明らかに必要ありません。
削除したコードは、後でバージョン履歴から再現することができます。
コードの一部を永久に保存する必要がある場合は、ファイルや `$TMP` や `HOME` オブジェクトにコピーしてください。

### FIXME, TODO, XXXを使い, 自分のIDを追加する

> [クリーン ABAP](#クリーン-abap) > [目次](#目次) > [コメント](#コメント) > [本節](#FIXME-TODO-XXXを使い-自分のIDを追加する)

```ABAP
METHOD do_something.
  " XXX FH delete this method - it does nothing
ENDMETHOD.
```

- `FIXME` は非常に小さなエラーや、内部的なインシデントに対する多くのエラーの兆しを指します。
- `TODO` は近い（！）将来に何かを完成させたい場所につけます。
- `XXX` は動作するが改善が必要なコードにマークします。

このようなコメントを入力する際には、ニックネームやイニシャル、ユーザーを追加しておくと、共同開発者が連絡を取ったり、
コメントが不明瞭な場合に質問をしたりできるようになります。

### メソッドシグネチャや末尾にコメントを書かない

> [クリーン ABAP](#クリーン-abap) > [目次](#目次) > [コメント](#コメント) > [本節](#メソッドシグネチャや末尾にコメントを書かない)

メソッドシグニチャのコメントは役に立ちません。

```ABAP
" アンチパターン
* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method CALIBRATION_KPIS=>CALCULATE_KPI
* +-------------------------------------------------------------------------------------------------+
* | [--->] STRATEGY_ID                 TYPE        STRATEGY_ID
* | [--->] THRESHOLD                   TYPE        STRATEGY_THRESHOLD
* | [--->] DETECTION_OBJECT_SCORE      TYPE        T_HIT_RESULT
* | [<---] KPI                         TYPE        T_SIMULATED_KPI
* +--------------------------------------------------------------------------------------</SIGNATURE>
```

数十年前、コードを検査するときにメソッドシグニチャを見ることができなかったとき、
あるいは何十ページもあるプリントアウトを使って作業していたとき、これらのコメントは意味のあるものだったかもしれません。
しかし、現代のすべてのABAP IDE (SE24, SE80, ADT) はメソッドシグネチャを簡単に表示するので、
これらのコメントはノイズ以外の何物でもなくなりました。

> SE24/SE80のフォームベースのエディタで、_Signature_ ボタンを押します。
> ABAP Development Tools で、メソッド名を選択して F2 を押すか、
> _ABAP Element Info_ ビューをパースペクティブに追加します。

同様に、末尾コメントも余計なものです。
これらのコメントは、プログラムや関数、そしてその中にあるネストされたIFが何百行もの長いコードであった数十年前には便利だったかもしれません。
しかし、現代のコーディングスタイルでは、`ENDIF` や `ENDMETHOD` がどの開始文に属しているのかを容易に確認できるような短いメソッドを作成しています。

```ABAP
" アンチパターン
METHOD get_kpi_calc.
  IF has_entries = abap_false.
    result = 42.
  ENDIF.  " IF has_entries = abap_false
ENDMETHOD.   " get_kpi_calc
```

### コメントにメッセージテキストを書かない

> [クリーン ABAP](#クリーン-abap) > [目次](#目次) > [コメント](#コメント) > [本節](#コメントにメッセージテキストを書かない)

```ABAP
" アンチパターン
" alert category not filled
MESSAGE e003 INTO dummy.
```

メッセージはコードから独立して変化し、
誰もコメントを調整したことを覚えていないので、
誰も気づくことなく、すぐに時代遅れになり、誤解を招くことさえあります。

最近のIDEでは、メッセージのテキストを簡単に見ることができます。
例えば ABAP Development Tools では、
メッセージIDを選択して Shift+F2 を押します。

より明示的にしたい場合は、
メッセージを独自のメソッドに抽出することを検討してください。

```ABAP
METHOD create_alert_not_found_message.
  MESSAGE e003 INTO dummy.
ENDMETHOD.
```

### ABAP Doc は公開 API のみに書く

> [クリーン ABAP](#クリーン-abap) > [目次](#目次) > [コメント](#コメント) > [本節](#ABAP-Doc-は公開-API-のみに書く)

公開API、つまり他のチームやアプリケーションの開発者向けのAPIを文書化するためにABAP Docを書いてください。
内部用のためにABAP Docを書いてはいけません。

ABAP Docは、すぐに時代遅れになり、誤解を招くようになるという、すべてのコメントと同じ弱点を抱えています。
結果として、意味のあるところでのみ使用すべきであり、すべてのものにABAP Docを書くことを強制するべきではありません。

> 詳細については [Robert C. Martin の _Clean Code_] の _Chapter 4: Good Comments: Javadocs in Public APIs_ と _Chapter 4: Bad Comments:
> Javadocs in Nonpublic Code_ を参照してください。

### 疑似コメントよりもプラグマを選ぶ

> [クリーン ABAP](#クリーン-abap) > [目次](#目次) > [コメント](#コメント) > [本節](#疑似コメントよりもプラグマを選ぶ)

ATCが識別した無関係な警告やエラーを抑制するには、疑似コメントではなくプラグマを使います。
疑似コメントはほとんど廃止されており、プラグマに置き換えられています。

```ABAP
" pattern
MESSAGE e001(ad) INTO DATA(message) ##NEEDED.

" アンチパターン
MESSAGE e001(ad) INTO DATA(message). "#EC NEEDED
```

`ABAP_SLIN_PRAGMAS` プログラムまたは `SLIN_DESC` テーブルを使用して、
廃止された疑似コメントとそれを置き換えたプラグマの間のマッピングを見つけることができます。

## フォーマット

> [クリーン ABAP](#クリーン-abap) > [目次](#目次) > [本節](#フォーマット)

以下の提案は [書くためではなく読むために最適化されています](#書くためではなく読むために最適化する)。
ABAP のプリティプリントはこれらをカバーしていないので、
名前の長さなどが変更されたときに、ステートメントを再フォーマットするために追加の手作業が発生するものもあります。
これを避けたい場合は、[同じオブジェクトへの代入時は位置を揃えるが, 別のオブジェクトの場合はしない](#同じオブジェクトへの代入時は位置を揃えるが-別のオブジェクトの場合はしない)
のようなルールをやめることを検討してください。

### 一貫性を保つ

> [クリーン ABAP](#クリーン-abap) > [目次](#目次) > [本節](#一貫性を保つ)

プロジェクトのすべてのコードを同じようにフォーマットしてください。
チームメンバー全員に同じフォーマットスタイルを使用させます。

外部のコードを編集する場合は、個人的なスタイルにこだわるのではなく、
そのプロジェクトのフォーマットスタイルに従ってください。

時間の経過とともにフォーマットルールを変更する場合は、
[リファクタリングのベストプラクティス](#レガシーコードをリファクタするには)
を使用して、時間の経過とともにコードを更新してください。

### 書くためではなく読むために最適化する

> [クリーン ABAP](#クリーン-abap) > [目次](#目次) > [フォーマット](#フォーマット) > [本節](#書くためではなく読むために最適化する)

開発者はほとんどの時間をコードを _読むこと_ に費やしています。
実際のところ、1日のうちコードを _書いている_ 時間はずっと少ないです。

結果として、コードのフォーマットは書くためではなく、読むためとデバッグのために最適化すべきです。

例えば、以下のようなものが好ましいでしょう。

```ABAP
DATA:
  a TYPE b,
  c TYPE d,
  e TYPE f.
```

次のようなハックよりも。

```ABAP
" アンチパターン
DATA:
  a TYPE b
  ,c TYPE d
  ,e TYPE f.
```

### 有効化する前にプリティプリントを使う

> [クリーン ABAP](#クリーン-abap) > [目次](#目次) > [フォーマット](#フォーマット) > [本節](#有効化する前にプリティプリントを使う)

オブジェクトを有効化する前に、プリティプリント - SE80、SE24、ADT で Shift+F1 - を適用してください。

より大きなフォーマットされていないレガシーコードベースを修正する場合、
変更リストや移送の依存関係が大きくなるのを避けるために、
選択された行のみにプリティプリントを適用することをお勧めします。
別の移送依頼やノートの中で、開発オブジェクト全体をにプリティプリントすることを検討してください。

> 詳細については [Robert C. Martin の _Clean Code_] の _Chapter 5: Formatting: Team Rules_ を参照してください。

### プリティプリントのチーム設定を使う

> [クリーン ABAP](#クリーン-abap) > [目次](#目次) > [フォーマット](#フォーマット) > [本節](#プリティプリントのチーム設定を使う)

常にチームの設定を使用してください。
以下で指定します。  
_メニュー_ > _ユーティリティ_ > _設定 ..._ > _ABAP エディタ_ > _プリティプリント_

チーム内で合意した通りに _開始位置_ と _大文字/小文字の変換_ > _キーワード大文字_ を設定します。

> [Upper vs. Lower Case](sub-sections/UpperVsLowerCase.md) では
> キーワードのタイプケースについて明確なガイダンスを出していない理由を説明しています。
>
> 詳細については [Robert C. Martin の _Clean Code_] の _Chapter 5: Formatting: Team Rules_ を参照してください。

### 1行につき, 最大1つのステートメント

> [クリーン ABAP](#クリーン-abap) > [目次](#目次) > [フォーマット](#フォーマット) > [本節](#1行につき-最大1つのステートメント)

```ABAP
DATA do_this TYPE i.
do_this = input + 3.
```

たとえ何かの拍子に次のコードが読みやすいと誤認してしまったとしても

```ABAP
" アンチパターン
DATA do_this TYPE i. do_this = input + 3.
```

### 適度な行の長さを守る

> [クリーン ABAP](#クリーン-abap) > [目次](#目次) > [フォーマット](#フォーマット) > [本節](#適度な行の長さを守る)

行の長さは最大120文字までとしてください。

人間の目は、行の幅が広すぎない方が快適にテキストを読みます - 
UI デザイナーや目の動きの研究者に聞いてみてください。
また、デバッグや隣り合った2つのソースを比較する際には、コードの幅が狭い方がありがたいと感じるでしょう。

古い端末装置に由来する80文字、あるいは72文字という制限は、少し制限が強すぎます。
よく100文字が推奨され、実行可能な選択肢ですが、ABAPでは言語の一般的な冗長さのためか、120文字の方がもう少し適しているようです。

> 備考：ADT で印刷マージンを 120 文字に設定すると、コードビューに縦線として表示されます。
> _Menu_ > _Window_ > _Preferences_ > _General_ > _Editors_ > _Text Editors_ で設定します。

### コードを凝縮する

> [クリーン ABAP](#クリーン-abap) > [目次](#目次) > [フォーマット](#フォーマット) > [本節](#コードを凝縮する)

```ABAP
DATA(result) = calculate( items ).
```

次のように、不要な空白を追加するのではなく

```ABAP
" アンチパターン
DATA(result)        =      calculate(    items =   items )   .
```

### 区切るための空白行は1行のみとする

> [クリーン ABAP](#クリーン-abap) > [目次](#目次) > [フォーマット](#フォーマット) > [本節](#区切るための空白行は1行のみとする)

```ABAP
DATA(result) = do_something( ).

DATA(else) = calculate_this( result ).
```

1行の空白行により、2つのステートメントが異なることをするということを強調します。複数行の空白行には理由がありません。

```ABAP
" アンチパターン
DATA(result) = do_something( ).



DATA(else) = calculate_this( result ).
```

区切りの空白行を追加したいという衝動は、メソッドが [1つのことだけをして](#1つのことだけをうまくやる) いないことを示唆しているかもしれません。

### 空白行で区切ることにこだわらない

> [クリーン ABAP](#クリーン-abap) > [目次](#目次) > [フォーマット](#フォーマット) > [本節](#空白行で区切ることにこだわらない)

```ABAP
METHOD do_something.
  do_this( ).
  then_that( ).
ENDMETHOD.
```

空白行でコードをバラバラにする悪い習慣には理由がありません。

```ABAP
" アンチパターン
METHOD do_something.

  do_this( ).

  then_that( ).

ENDMETHOD.
```

空白行は、実際には複数行にまたがるステートメントがある場合にのみ意味を持ちます。

```ABAP
METHOD do_something.

  do_this( ).

  then_that(
    EXPORTING
      variable = 'A'
    IMPORTING
      result   = result ).

ENDMETHOD.
```

### 同じオブジェクトへの代入時は位置を揃えるが, 別のオブジェクトの場合はしない

> [クリーン ABAP](#クリーン-abap) > [目次](#目次) > [フォーマット](#フォーマット) > [本節](#同じオブジェクトへの代入時は位置を揃えるが-別のオブジェクトの場合はしない)

これらのものが何らかの形で一緒に属していることを強調するために

```ABAP
structure-type = 'A'.
structure-id   = '4711'.
```

あるいはもっとよいのは

```ABAP
structure = VALUE #( type = 'A'
                     id   = '4711' ).
```

しかし、お互いに関係のないことはそのままにしておいてください。

```ABAP
customizing_reader = fra_cust_obj_model_reader=>s_get_instance( ).
hdb_access = fra_hdbr_access=>s_get_instance( ).
```

> 詳細については [Robert C. Martin の _Clean Code_] の _Chapter 5: Formatting: Horizontal Alignment_ を参照してください。

### 行末でカッコを閉じる

> [クリーン ABAP](#クリーン-abap) > [目次](#目次) > [フォーマット](#フォーマット) > [本節](#行末でカッコを閉じる)

```ABAP
modify->update( node           = if_fra_alert_c=>node-item
                key            = item->key
                data           = item
                changed_fields = changed_fields ).
```

次のように、不必要に長くするのではなく

```ABAP
" アンチパターン
modify->update( node           = if_fra_alert_c=>node-item
                key            = item->key
                data           = item
                changed_fields = changed_fields
).
```

### 単一パラメータでの呼び出しは1行で書く

> [クリーン ABAP](#クリーン-abap) > [目次](#目次) > [フォーマット](#フォーマット) > [本節](#単一パラメータでの呼び出しは1行で書く)

```ABAP
DATA(unique_list) = remove_duplicates( list ).
remove_duplicates( CHANGING list = list ).
```

次のように、不必要に長くするのではなく

```ABAP
" アンチパターン
DATA(unique_list) = remove_duplicates(
                           list ).
DATA(unique_list) = remove_duplicates(
                         CHANGING
                           list = list ).
```

### 呼び出しの後にパラメータを置く

> [クリーン ABAP](#クリーン-abap) > [目次](#目次) > [フォーマット](#フォーマット) > [本節](#呼び出しの後にパラメータを置く)

```ABAP
DATA(sum) = add_two_numbers( value_1 = 5
                             value_2 = 6 ).
```

これにより、行が非常に長くなる場合は、パラメータを次の行に書きます。

```ABAP
DATA(sum) = add_two_numbers(
                value_1 = round_up( input DIV 7 ) * 42 + round_down( 19 * step_size )
                value_2 = VALUE #( ( `Calculation failed with a very weird result` ) ) ).
```

### 改行する場合, 呼び出しの下のパラメータをインデントする

> [クリーン ABAP](#クリーン-abap) > [目次](#目次) > [フォーマット](#フォーマット) > [本節](#改行する場合-呼び出しの下のパラメータをインデントする)

```ABAP
DATA(sum) = add_two_numbers(
                value_1 = 5
                value_2 = 6 ).
```

パラメータを他の位置に合わせると、何に属しているのかを見分けるのが難しくなります。

```ABAP
DATA(sum) = add_two_numbers(
    value_1 = 5
    value_2 = 6 ).
```

しかし、名前の長さの変更でフォーマットが崩れるのを避けたい場合には、このパターンが最適です。

### 複数のパラメータを改行する

> [クリーン ABAP](#クリーン-abap) > [目次](#目次) > [フォーマット](#フォーマット) > [本節](#複数のパラメータを改行する)

```ABAP
DATA(sum) = add_two_numbers( value_1 = 5
                             value_2 = 6 ).
```

はい、これはスペースを浪費します。
しかし、こうしなければ、あるパラメータがどこで終わり、次のパラメータがどこから始まるのかを見分けるのが難しくなります。

```ABAP
" アンチパターン
DATA(sum) = add_two_numbers( value_1 = 5 value_2 = 6 ).
```

### パラメータを整列させる

> [クリーン ABAP](#クリーン-abap) > [目次](#目次) > [フォーマット](#フォーマット) > [本節](#パラメータを整列させる)

```ABAP
modify->update( node           = if_fra_alert_c=>node-item
                key            = item->key
                data           = item
                changed_fields = changed_fields ).
```

余白が不規則になると、パラメータの終了位置と値の開始位置がわかりにくくなります。

```ABAP
" アンチパターン
modify->update( node = if_fra_alert_c=>node-item
                key = item->key
                data = item
                changed_fields = changed_fields ).
```

> 逆に、名前の長さの変更でフォーマットが崩れるのを避けたい場合には、このパターンが最適です。

### 行が長くなりすぎる場合, 呼び出しを改行する

> [クリーン ABAP](#クリーン-abap) > [目次](#目次) > [フォーマット](#フォーマット) > [本節](#行が長くなりすぎる場合-呼び出しを改行する)

```ABAP
DATA(some_super_long_param_name) =
  if_some_annoying_interface~add_two_numbers_in_a_long_name(
      value_1 = 5
      value_2 = 6 ).
```

### インデントしてタブにスナップする

> [クリーン ABAP](#クリーン-abap) > [目次](#目次) > [フォーマット](#フォーマット) > [本節](#インデントしてタブにスナップする)

パラメータのキーワードを2スペースで、パラメータを4スペースでインデントします。

```ABAP
DATA(sum) = add_two_numbers(
              EXPORTING
                value_1 = 5
                value_2 = 6
              CHANGING
                errors  = errors ).
```

キーワードがない場合は、パラメータを4スペースでインデントします。

```ABAP
DATA(sum) = add_two_numbers(
                value_1 = 5
                value_2 = 6 ).
```

インデントするにはTabキーを使用します。これで必要以上にスペースが1つ増えても問題ありません。
(左の `DATA(sum) =` の部分の文字数が奇数の場合に発生します)

### インライン宣言はメソッド呼び出しのようにインデントする

> [クリーン ABAP](#クリーン-abap) > [目次](#目次) > [フォーマット](#フォーマット) > [本節](#インライン宣言はメソッド呼び出しのようにインデントする)

VALUE または NEW を使ったインライン宣言は、メソッド呼び出しのようにインデントします。

```ABAP
DATA(result) = merge_structures( a = VALUE #( field_1 = 'X'
                                              field_2 = 'A' )
                                 b = NEW /clean/structure_type( field_3 = 'C'
                                                                field_4 = 'D' ) ).
```

### 型句の位置を揃えない

> [クリーン ABAP](#クリーン-abap) > [目次](#目次) > [フォーマット](#フォーマット) > [本節](#型句の位置を揃えない)

```ABAP
DATA name TYPE seoclsname.
DATA reader TYPE REF TO /clean/reader.
```

変数とその型は一緒に属しているため、視覚的に近接してグループ化されるべきです。
`TYPE` 句を整列させることは、このことから注意を逸らし、変数が一つの縦のグループを形成し、その型が別のグループを形成することを示唆します。
また、整列によって不要な編集のオーバーヘッドが生じ、最長の変数名の長さが変更されたときにすべてのインデントを調整する必要があります。

```ABAP
" アンチパターン
DATA name   TYPE seoclsname.
DATA reader TYPE REF TO /clean/reader.
```

## テスト

> [クリーン ABAP](#クリーン-abap) > [目次](#目次) > [本節](#テスト)

### 原則

> [クリーン ABAP](#クリーン-abap) > [目次](#目次) > [テスト](#テスト) > [本節](#原則)

#### テスタブルなコードを書く

> [クリーン ABAP](#クリーン-abap) > [目次](#目次) > [テスト](#テスト) > [原則](#原則) > [本節](#テスタブルなコードを書く)

すべてのコードを自動的にテストできるように書いてください。

そのためにコードのリファクタリングが必要な場合は、それを実行してください。
他の機能を追加し始める前に、まずそれを行います。

構造が悪すぎてテストできないレガシーコードに追加する場合は、
少なくとも追加したものをテストできる範囲でリファクタリングしましょう。

#### 他の人がモックできるようにする

> [クリーン ABAP](#クリーン-abap) > [目次](#目次) > [テスト](#テスト) > [原則](#原則) > [本節](#他の人がモックできるようにする)

他の人に利用されるコードを書いている場合、その人のコードのユニットテストが書けるようにしてください。
例えば、すべての外向きの場所にインタフェースを追加したり、
統合テストを容易にする有用なテストダブルを提供したり、
依存関係の逆転を適用して本番の設定をテスト設定で置き換えることができるようにしてください。

#### 可読性のルール

> [クリーン ABAP](#クリーン-abap) > [目次](#目次) > [テスト](#テスト) > [原則](#原則) > [本節](#可読性のルール)

テストコードを製品コードよりもさらに読みやすいものにしましょう。
よいテストがあれば悪い製品コードに取り組むことができますが、テストすらも悪ければ途方に暮れてしまいます。

1年後も理解できるように、テストコードをシンプルで愚鈍なものにしておきましょう。

標準とパターンにこだわることで、同僚がすぐにコードを理解できるようになります。

#### コピーを作成したり, テストレポートを書いたりしない

> [クリーン ABAP](#クリーン-abap) > [目次](#目次) > [テスト](#テスト) > [原則](#原則) > [本節](#コピーを作成したり-テストレポートを書いたりしない)

開発オブジェクトの `$TMP` コピーを作って、それをいじくり回すことで、バックログ項目の作業を開始しないでください。
他の人はこれらのオブジェクトに気づかないので、あなたの作業の状況を知ることができません。
最初に作業コピーを作成することで、おそらく多くの時間を浪費することになるでしょう。
また、後からコピーを削除するのを忘れてしまい、システムや依存関係をスパム化してしまうでしょう。
（信じられませんか？今すぐ開発システムに行って `$TMP` を確認してください。）

また、特定の方法で何かを呼び出すテストレポートを書き始めて、
まだ動作しているか検証することを繰り返してはいけません。
これは、テストレポートを手動で繰り返し、すべてがまだ正常かどうかを目視で検証するという、貧者のテストです。
次のステップとして、コードがまだ大丈夫かどうかを自動アサーションで教えてくれるように、このレポートをユニットテストで自動化してください。
まず、後からユニットテストを書く手間を省くことができます。
次に、手動の繰り返しのための多くの時間を節約でき、さらに、退屈したり疲れたりすることを避けることができます。

#### 内部の private ではなく, public をテストする

> [クリーン ABAP](#クリーン-abap) > [目次](#目次) > [テスト](#テスト) > [原則](#原則) > [本節](#内部の-private-ではなく-public-をテストする)

クラスの public な部分、特に実装しているインタフェースはかなり安定しており、変更される可能性は低いです。
ユニットテストでは、public のみを検証することで堅牢性を高め、
クラスのリファクタリング時に費やす労力を最小限に抑えます。
対照的に、内部の protected と private は、リファクタリングによってとてもすぐに変更される可能性があるため、
テストが不必要に壊れてしまいます。

private メソッドや protected メソッドをテストする緊急の必要性は、数種の設計上の欠陥に対する早期警告サインかもしれません。
自分自身に問いかけてみてください：

- 専用のテストスイートを持った独自のクラスに登場させたいコンセプトを、
  誤って自分のクラスに埋めてしまったことはありませんか？

- ドメインロジックをグルーコードから分離するのを忘れていませんか？
  例えば、action、determination、または validation として BOPF に接続されているクラスや、
  SAP Gateway によって `*_DPC_EXT` データプロバイダとして生成されたクラスに
  ドメインロジックを直接実装するのは、最善の方法ではないかもしれません。

- インタフェースが複雑すぎて、無関係なデータや、簡単にモックできないようなデータを要求していませんか？

#### カバレッジにこだわらない

> [クリーン ABAP](#クリーン-abap) > [目次](#目次) > [テスト](#テスト) > [原則](#原則) > [本節](#カバレッジにこだわらない)

コードカバレッジは、テストし忘れたコードを見つけるのを助けるためにあり、ランダムなKPIを満たすためにあるのではありません。

カバレッジに到達させるだけのために、アサートのないテストやダミーアサートを使ったテストを作ってはいけません。
安全にリファクタリングできないことがわかるように、テストしないままにしておいた方がいいでしょう。
カバレッジが100%未満でも、完璧なテストを行うことができます。
コンストラクタの中にテストダブルを挿入するIFが含まれているなど、100%に達することが現実的でない場合もあります。
よいテストは、異なる分岐や条件に対して、同じ文を複数回カバーする傾向があります。
実際には、想像上の100%以上のカバレッジがあることになります。

### テストクラス

> [クリーン ABAP](#クリーン-abap) > [目次](#目次) > [テスト](#テスト) > [本節](#テストクラス)

#### ローカルテストクラスは目的に応じて命名する

> [クリーン ABAP](#クリーン-abap) > [目次](#目次) > [テスト](#テスト) > [テストクラス](#テストクラス) > [本節](#ローカルテストクラスは目的に応じて命名する)

ローカルテストクラスの名前は、ストーリーの「when」の部分で名付けます。

```ABAP
CLASS ltc_<public method name> DEFINITION FOR TESTING ... ."
```

もしくはストーリーの「given」部分で名付けます。

```ABAP
CLASS ltc_<common setup semantics> DEFINITION FOR TESTING ... .
```

```ABAP
" アンチパターン
CLASS ltc_fra_online_detection_api DEFINITION FOR TESTING ... . " それがテスト対象のクラスであることはわかっています。なぜ繰り返すのでしょうか？
CLASS ltc_test DEFINITION FOR TESTING ....                      " もちろんこれはテストです。他に何がありえるでしょうか？
```

#### テストはローカルクラスに置く

> [クリーン ABAP](#クリーン-abap) > [目次](#目次) > [テスト](#テスト) > [テストクラス](#テストクラス) > [本節](#テストはローカルクラスに置く)

ユニットテストをテスト対象クラスのローカルテストインクルードに配置してください。
これにより、クラスをリファクタリングする際にこれらのテストを見つけられるようになり、
[テストクラスの実行方法](#テストクラスの実行方法) で説明されているように、
1回のキー操作で関連するすべてのテストを実行できるようになります。

コンポーネントテスト、統合テスト、システムテストを、個別のグローバルクラスのローカルテストインクルードに配置してください。
これらのテストは、テスト対象の単一のクラスに直接関連していませんので、みだりに関係するクラスの一つに入れるのではなく、別のクラスに配置すべきです。
このグローバルテストクラスには `FOR TESTING` と `ABSTRACT` というマークをつけて、製品コードで誤って参照されないようにしてください。
テストを他のクラスに入れると、関係するクラスをリファクタリングするときに、テストを見落として実行するのを忘れてしまう危険性があります。

したがって、どのオブジェクトがテストの対象となるかを文書化するために _テスト関係_ を使用することが有益です。
以下の例では、テストクラス `hiring_test` は、
`recruting` クラスや `candidate` クラスの中で `Shift-Crtl-F12` (Windows) または `Cmd-Shift-F12` (macOS) ショートカットを使用して、実行することができます。

```abap
"! @testing recruting
"! @testing candidate
class hiring_test defintion
  for testing risk level dangerous duration medium
  abstract.
  ...
endclass.
```

#### ヘルプメソッドはヘルプクラスに置く

> [クリーン ABAP](#クリーン-abap) > [目次](#目次) > [テスト](#テスト) > [テストクラス](#テストクラス) > [本節](#ヘルプメソッドはヘルプクラスに置く)

複数のテストクラスに利用されるヘルプメソッドはヘルプクラスに配置します。
継承(is-a関係)や委譲(has-a関係)によってヘルプメソッドを利用できるようにします。

```abap
" inheritance example

CLASS lth_unit_tests DEFINITION ABSTRACT.

  PROTECTED SECTION.
    CLASS-METHODS assert_activity_entity
      IMPORTING
        actual_activity_entity TYPE REF TO zcl_activity_entity
        expected_activity_entity TYPE REF TO zcl_activity_entity.
    ...
ENDCLASS.

CLASS lth_unit_tests IMPLEMENTATION.

  METHOD assert_activity_entity.
    ...
  ENDMETHOD.

ENDCLASS.

CLASS ltc_unit_tests DEFINITION INHERITING FROM lth_unit_tests FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.
  ...
ENDCLASS.
```

#### テストクラスの実行方法

> [クリーン ABAP](#クリーン-abap) > [目次](#目次) > [テスト](#テスト) > [テストクラス](#テストクラス) > [本節](#テストクラスの実行方法)

ABAP Development Tools で、クラス内のすべてのテストを実行するには、Ctrl+Shift+F10 を押します。
カバレッジ計測を含めるには、Ctrl+Shift+F11 を押します。
テスト関係として保持されている他のクラスのテストも実行するには、Ctrl+Shift+F12 を押します。

> macOS では、`Ctrl` の代わりに `Cmd` を使用します。

### テスト対象コード

> [クリーン ABAP](#クリーン-abap) > [目次](#目次) > [テスト](#テスト) > [本節](#テスト対象コード)

#### テスト対象コードに意味のある名前を付けるか, デフォルトを CUT にする

> [クリーン ABAP](#クリーン-abap) > [目次](#目次) > [テスト](#テスト) > [テスト対象コード](#テスト対象コード) > [本節](#テスト対象コードに意味のある名前を付けるか-デフォルトを-CUT-にする)

テスト対象のコードを表す変数に意味のある名前をつけてください。

```ABAP
DATA blog_post TYPE REF TO ...
```

価値のない名前空間や接頭辞を使って、単純にクラス名を繰り返してはいけません。

```ABAP
" アンチパターン
DATA clean_fra_blog_post TYPE REF TO ...
```

異なるテストセットアップがある場合、変化するオブジェクトの状態を記述するのに役立ちます。

```ABAP
DATA empty_blog_post TYPE REF TO ...
DATA simple_blog_post TYPE REF TO ...
DATA very_long_blog_post TYPE REF TO ...
```

意味のある名前が見つからない場合は、デフォルトとして `cut` を使います。
「code under test」の略です。

```ABAP
DATA cut TYPE REF TO ...
```

特に汚く紛らわしいテストでは、変数を `cut` と命名することで、
実際にテストされている対象が一時的にわかりやすくなります。
しかし、長期的にはテストを整理するのが現実的な方法です。

#### 実装ではなくインタフェースに対してテストする

> [クリーン ABAP](#クリーン-abap) > [目次](#目次) > [テスト](#テスト) > [テスト対象コード]](#テスト対象コード) > [本節](#実装ではなくインタフェースに対してテストする)

[_内部の private ではなく, public をテストする_](#内部の-private-ではなく-public-をテストする) の実質的な結果として、
_インタフェース_ でテスト対象コードを指定します。

```ABAP
DATA code_under_test TYPE REF TO some_interface.
```

は、次のように _クラス_ で指定するよりもよいです。

```ABAP
" アンチパターン
DATA code_under_test TYPE REF TO some_class.
```

#### テスト対象コードの呼び出しをメソッドに抽出する

> [クリーン ABAP](#クリーン-abap) > [目次](#目次) > [テスト](#テスト) > [テスト対象コード](#テスト対象コード) > [本節](#テスト対象コードの呼び出しをメソッドに抽出する)

テスト対象のメソッドが多くのパラメータやデータの準備を必要とする場合、
そのメソッドの呼び出しを独自のヘルパーメソッドに抽出し、重要でないパラメータをデフォルトにすることができます。

```ABAP
METHODS map_xml_to_itab
  IMPORTING
    xml_string TYPE string
    config     TYPE /clean/xml2itab_config DEFAULT default_config
    format     TYPE /clean/xml2itab_format DEFAULT default_format.

METHOD map_xml_to_itab.
  result = cut->map_xml_to_itab( xml_string = xml_string
                                 config     = config
                                 format     = format ).
ENDMETHOD.

DATA(itab) = map_xml_to_itab( '<xml></xml>' ).
```

元のメソッドを直接呼び出すと、テストが大量の無意味な詳細情報でいっぱいになってしまう可能性があります。

```ABAP
" アンチパターン
DATA(itab) = cut->map_xml_to_itab( xml_string = '<xml></xml>'
                                   config     = VALUE #( 'some meaningless stuff' )
                                   format     = VALUE #( 'more meaningless stuff' ) ).
```

### インジェクション

> [クリーン ABAP](#クリーン-abap) > [目次](#目次) > [テスト](#テスト) > [本節](#インジェクション)

#### 依存関係を逆転させてテストダブルをインジェクトする

> [クリーン ABAP](#クリーン-abap) > [目次](#目次) > [テスト](#テスト) > [インジェクション](#インジェクション) > [本節](#依存関係を逆転させてテストダブルをインジェクトする)

依存関係の逆転とは、すべての依存関係をコンストラクタに引き渡すことを意味します。

```ABAP
METHODS constructor
  IMPORTING
    customizing_reader TYPE REF TO if_fra_cust_obj_model_reader.

METHOD constructor.
  me->customizing_reader = customizing_reader.
ENDMETHOD.
```

セッターインジェクションは使用しないでください。
意図しない方法で製品コードを使用することを可能にしてしまいます。

```ABAP
" アンチパターン
METHODS set_customizing_reader
  IMPORTING
    customizing_reader TYPE REF TO if_fra_cust_obj_model_reader.

METHOD do_something.
  object->set_customizing_reader( a ).
  object->set_customizing_reader( b ). " 誰かがこんなことをすると思いますか？
ENDMETHOD.
```

フレンドインジェクションは使用しないでください。
製品コードの依存関係が置き換えられる前に初期化されてしまい、おそらく予想外の結果になるでしょう。
内部の名前を変更するとすぐに壊れてしまいます。
また、コンストラクタでの初期化を迂回できてしまいます。

```ABAP
" アンチパターン
METHOD setup.
  cut = NEW fra_my_class( ). " <- 最初に製品の customizing_reader を作成します。それで何が壊れるのでしょうか？
  cut->customizing_reader ?= cl_abap_testdouble=>create( 'if_fra_cust_obj_model_reader' ).
ENDMETHOD.

METHOD constructor.
  customizing_reader = fra_cust_obj_model_reader=>s_get_instance( ).
  customizing_reader->fill_buffer( ). " <- テストダブルでは呼び出されないので、これをテストする機会はありません。
ENDMETHOD.
```

#### ABAP test double ツールの利用を検討する

> [クリーン ABAP](#クリーン-abap) > [目次](#目次) > [テスト](#テスト) > [インジェクション](#インジェクション) > [本節](#ABAP-test-double-ツールの利用を検討する)

```ABAP
DATA(customizing_reader) = CAST /clean/customizing_reader( cl_abap_testdouble=>create( '/clean/default_custom_reader' ) ).
cl_abap_testdouble=>configure_call( customizing_reader )->returning( sub_claim_customizing ).
customizing_reader->read( 'SOME_ID' ).
```

は、次のカスタムテストダブルよりも短くてわかりやすいです。

```ABAP
" アンチパターン
CLASS /dirty/default_custom_reader DEFINITION FOR TESTING CREATE PUBLIC.
  PUBLIC SECTION.
    INTERFACES /dirty/customizing_reader.
    DATA customizing TYPE /dirty/customizing_table.
ENDCLASS.

CLASS /dirty/default_custom_reader IMPLEMENTATION.
  METHOD /dirty/customizing_reader~read.
    result = customizing.
  ENDMETHOD.
ENDCLASS.

METHOD test_something.
  DATA(customizing_reader) = NEW /dirty/customizing_reader( ).
  customizing_reader->customizing = sub_claim_customizing.
ENDMETHOD.
```

#### テストツールを利用する

> [クリーン ABAP](#クリーン-abap) > [目次](#目次) > [テスト](#テスト) > [インジェクション](#インジェクション) > [本節](#テストツールを利用する)

一般的に、クリーンなプログラミングスタイルであれば、
標準のABAPユニットテストとテストダブルで多くの作業を行うことができます。
しかし、よりトリッキーなケースにもエレガントな方法で取り組むことができるツールがあります。

- `CL_OSQL_REPLACE` サービスを使用すると、
  システムの他の部分に影響を与えることなく
  複雑な OpenSQL 文を
  テストデータを格納できるテストデータオブジェクトにリダイレクトしてテストできます。

- CDS test framework を使用して CDS ビューをテストします。

#### test seam は一時的な回避策として使用する

> [クリーン ABAP](#クリーン-abap) > [目次](#目次) > [テスト](#テスト) > [インジェクション](#インジェクション) > [本節](#test-seam-は一時的な回避策として使用する)

他のすべての技術でうまくいかない場合、またはレガシーコードの危険な浅瀬にいる場合にのみ
テスト可能なものにするために [test seams](https://help.sap.com/doc/abapdocu_751_index_htm/7.51/en-US/index.htm?file=abaptest-seam.htm) を使用します。

一見快適そうに見えますが、test seam は侵襲性があり、プライベートな依存関係に絡みつきやすいため、
長期的に安定した状態を維持することが難しいのです。

そのため、コードをよりテスト可能な形式にリファクタリングできるようにするための
一時的な回避策としてのみ、test seam を使用することを推奨します。

#### LOCAL FRIENDS を使用して依存関係逆転コンストラクタへアクセスする

> [クリーン ABAP](#クリーン-abap) > [目次](#目次) > [テスト](#テスト) > [インジェクション](#インジェクション) > [本節](#LOCAL-FRIENDS-を使用して依存関係逆転コンストラクタへアクセスする)

```ABAP
CLASS /clean/unit_tests DEFINITION.
  PRIVATE SECTION.
    DATA cut TYPE REF TO /clean/interface_under_test.
    METHODS setup.
ENDCLASS.

CLASS /clean/class_under_test DEFINITION LOCAL FRIENDS unit_tests.

CLASS unit_tests IMPLEMENTATION.
  METHOD setup.
    DATA(mock) = cl_abap_testdouble=>create( '/clean/some_mock' ).
    " /clean/class_under_test is CREATE PRIVATE
     " so this only works because of the LOCAL FRIENDS
    cut = NEW /clean/class_under_test( mock ).
  ENDMETHOD.
ENDCLASS.
```

#### LOCAL FRIENDS を悪用してテスト対象コードに侵入しない

> [クリーン ABAP](#クリーン-abap) > [目次](#目次) > [テスト](#テスト) > [インジェクション](#インジェクション) > [本節](#LOCAL-FRIENDS-を悪用してテスト対象コードに侵入しない)

モックデータを挿入するために private や protected なメンバーにアクセスするユニットテストは脆弱です。
それらは、テスト対象コードの内部構造が変化すると壊れます。

```ABAP
" アンチパターン
CLASS /dirty/class_under_test DEFINITION LOCAL FRIENDS unit_tests.
CLASS unit_tests IMPLEMENTATION.
  METHOD returns_right_result.
    cut->some_private_member = 'AUNIT_DUMMY'.
  ENDMETHOD.
ENDCLASS.
```

#### テスト可能にするために製品コードを変更しない

> [クリーン ABAP](#クリーン-abap) > [目次](#目次) > [テスト](#テスト) > [インジェクション](#インジェクション) > [本節](#テスト可能にするために製品コードを変更しない)

```ABAP
" アンチパターン
IF me->in_test_mode = abap_true.
```

#### メソッドをモックするためにサブクラスにしない

> [クリーン ABAP](#クリーン-abap) > [目次](#目次) > [テスト](#テスト) > [インジェクション](#インジェクション) > [本節](#メソッドをモックするためにサブクラスにしない)

ユニットテストでモックするためにサブクラス化して、メソッドを上書きしたりしないようにしてください。
これは機能しますが、脆弱であり、コードをリファクタリングするとテストが簡単に壊れます。
また、実際の利用者がクラスを継承できるようになるため、
[明示的にそのように設計していない場合、不意をつかれる可能性があります](#継承を意図しない場合はFINALにする)。

```ABAP
" アンチパターン
CLASS unit_tests DEFINITION INHERITING FROM /dirty/real_class FOR TESTING [...].
  PROTECTED SECTION.
    METHODS needs_to_be_mocked REDEFINITION.
```

レガシーコードをテストするには、
[代わりに test seam に頼ってください](#test-seam-は一時的な回避策として使用する)。
これは、同様に脆弱ではありますが、
`FINAL` を削除して継承できるようにしたり、メソッドスコープを `PRIVATE` から `PROTECTED` に変更したりするといった、
少なくともクラスの本番の振る舞いを変更しないため、よりクリーンな方法です。

新しいコードを書くときには、このテスト可能性の問題をクラスを設計するときに直接考慮に入れて、
別のより良い方法を見つけてください。
一般的なベストプラクティスとしては、[他のテストツールに頼る](#テストツールを利用する)ことや、
問題のメソッドを独自のインターフェイスを持つ別のクラスに抽出することなどがあります。

> これは [テスト可能にするために製品コードを変更しない](#テスト可能にするために製品コードを変更しない) のより具体的なバリエーションです。

#### 不要なものをモックしない

> [クリーン ABAP](#クリーン-abap) > [目次](#目次) > [テスト](#テスト) > [インジェクション](#インジェクション) > [本節](#不要なものをモックしない)

```ABAP
cut = NEW /clean/class_under_test( db_reader = db_reader
                                   config    = VALUE #( )
                                   writer    = VALUE #( ) ).
```

テストの前提条件をできるだけ正確に定義してください。
テストが必要としないデータを設定したり、決して呼び出されないオブジェクトをモックしたりしないようにしましょう。
これらのことは、コードを読む人の注意をそらしてしまいます。

```ABAP
" アンチパターン
cut = NEW /dirty/class_under_test( db_reader = db_reader
                                   config    = config
                                   writer    = writer ).
```

また、何かをモックする必要が全くない場合もあります - これは通常、データ構造やデータコンテナの場合です。
例えば、ユニットテストは製品バージョンの `transient_log` を使用しても、それは副作用なくデータを保存するだけなので、問題なく動作するでしょう。

#### テストフレームワークを作らない

> [クリーン ABAP](#クリーン-abap) > [目次](#目次) > [テスト](#テスト) > [インジェクション](#インジェクション) > [本節](#テストフレームワークを作らない)

ユニットテストは、統合テストとは対照的に、必要に応じてすべてのテストデータをオンザフライで定義し、データインデータアウトする必要があります。

```ABAP
cl_abap_testdouble=>configure_call( test_double )->returning( data ).
```

提供するデータを判断するために「_テストケースID_」を区別するフレームワークを作り始めないでください。
結果として生じるコードは非常に長く絡み合ったものになり、これらのテストを長期的に維持することができなくなります。

```ABAP
" アンチパターン

test_double->set_test_case( 1 ).

CASE me->test_case.
  WHEN 1.
  WHEN 2.
ENDCASE.
```

### テストメソッド

> [クリーン ABAP](#クリーン-abap) > [目次](#目次) > [テスト](#テスト) > [本節](#テストメソッド)

#### テストメソッド名: 前提条件と期待する結果を反映する

> [クリーン ABAP](#クリーン-abap) > [目次](#目次) > [テスト](#テスト) > [テストメソッド](#テストメソッド) > [本節](#テストメソッド名-前提条件と期待する結果を反映する)

よい名前は、テストの前提条件と、期待する結果を反映しています。

```ABAP
METHOD reads_existing_entry.
METHOD throws_on_invalid_key.
METHOD detects_invalid_input.
```

悪い名前は、「いつ」（テスト対象の呼び出し方）を反映しているか、無意味な事実を繰り返すか、または隠語的です。

```ABAP
" アンチパターン

" 期待されているのは、成功でしょうか？失敗でしょうか？
METHOD get_conversion_exits.

" テストメソッドですが、「テスト」以外に何をするのでしょうか？
METHOD test_loop.

" まさにパラメータ化テストですが、その目的は何でしょうか？
METHOD parameterized_test.

" 「_wo_w」とは何を意味しているのでしょうか？1年後もそのことを覚えているでしょうか？
METHOD get_attributes_wo_w.
```

ABAP ではメソッド名に30文字しか許されていないので、十分な意味を伝えるには名前が短すぎる場合は、説明的なコメントを追加するのが妥当です。
ABAP Doc やテストメソッドの最初の行がコメントのための適切な選択かもしれません。

名前が長すぎるテストメソッドがたくさんあるということは、
一つのテストクラスをいくつかのテストクラスに分割して、
前提条件の違いをそれぞれのクラス名で表現した方がよいということを示している可能性があります。

#### given-when-then を使用する

> [クリーン ABAP](#クリーン-abap) > [目次](#目次) > [テスト](#テスト) > [テストメソッド](#テストメソッド) > [本節](#given-when-then-を使用する)

テストコードは、given-when-then パラダイムに沿って構成してください。
最初に、前提条件として初期化を行い(「given」)、
2番目に、実際にテスト対象を呼び出し(「when」)、
3番目に、結果を検証します(「then」)。

given または then セクションが長くなり、
もはや3つのセクションを見た目に区別できなくなった場合は、サブメソッドに抽出します。
空白行やコメントで区切るのは、一見良いように見えるかもしれませんが、
実際には見た目の乱雑さを軽減することはできません。
それでも、読み手や初心者のテスト書き手にとってはセクションを区別するのに便利です。

#### 「when」ではちょうど1回呼び出す

> [クリーン ABAP](#クリーン-abap) > [目次](#目次) > [テスト](#テスト) > [テストメソッド](#テストメソッド) > [本節](#whenではちょうど1回呼び出す)

テストメソッドの「when」セクションには、テスト対象クラスへの呼び出しが1回だけ含まれていることを確認してください。

```ABAP
METHOD rejects_invalid_input.
  " when
  DATA(is_valid) = cut->is_valid_input( 'SOME_RANDOM_ENTRY' ).
  " then
  cl_abap_unit_assert=>assert_false( is_valid ).
ENDMETHOD.
```

複数のものを呼び出すということは、メソッドが明確な焦点を持っておらず、あまりに多くのことをテストしていることを示しています。
これにより、テストが失敗したときに原因を見つけるのが難しくなります。
失敗の原因は、1回目、2回目、3回目の呼び出しだったのでしょうか？
また、テスト対象の機能が正確に何なのかがわからず、コードを読む人を混乱させます。

#### 本当に必要な時以外は TEARDOWN を追加しない

> [クリーン ABAP](#クリーン-abap) > [目次](#目次) > [テスト](#テスト) > [テストメソッド](#テストメソッド) > [本節](#本当に必要な時以外は-TEARDOWN-を追加しない)

`teardown` メソッドは通常、統合テストでデータベースエントリやその他の外部リソースをクリアするためにのみ必要とされます。

テストクラスのメンバー、特に `cut` や使用されたテストダブルをリセットすることは余計なことです。
次のテストメソッドが開始される前に、それらは `setup` メソッドによって上書きされます。

### テストデータ

> [クリーン ABAP](#クリーン-abap) > [目次](#目次) > [テスト](#テスト) > [本節](#テストデータ)

#### 意味を見つけやすくする

> [クリーン ABAP](#クリーン-abap) > [目次](#目次) > [テスト](#テスト) > [テストデータ](#テストデータ) > [本節](#意味を見つけやすくする)

ユニットテストでは、どのデータやテストダブルが重要で、
どのデータやテストダブルがコードをクラッシュさせないようにするためだけのためのものなのかを
素早く見分けることができるようにしたいものです。
あからさまに意味のない名前や値を与えることで、これをサポートします。例えば、

```ABAP
DATA(alert_id) = '42'.                             " よく知られた意味のない数字
DATA(detection_object_type) = '?=/"&'.             " 「キーボード事故」
CONSTANTS some_random_number TYPE i VALUE 782346.  " 変数名を明かす
```

実際のオブジェクトや実際のカスタマイズと関係がない場合には、関係があるように見えないようにしてください。

```ABAP
" アンチパターン
DATA(alert_id) = '00000001223678871'.        " このアラートは実際に存在します
DATA(detection_object_type) = 'FRA_SCLAIM'.  " この detection object type も実際に存在します
CONSTANTS memory_limit TYPE i VALUE 4096.    " この数字は慎重に選ばれたように見えます
```

#### 違いを見つけやすくする

> [クリーン ABAP](#クリーン-abap) > [目次](#目次) > [テスト](#テスト) > [テストデータ](#テストデータ) > [本節](#違いを見つけやすくする)

```ABAP
exp_parameter_in = VALUE #( ( parameter_name = '45678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789END1' )
                            ( parameter_name = '45678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789END2' ) ).
```

小さな違いを見極めるために、コードを読む人に意味のない長い文字列を比較させないでください。

#### 定数を使ってテストデータの目的と重要性を説明する

> [クリーン ABAP](#クリーン-abap) > [目次](#目次) > [テスト](#テスト) > [テストデータ](#テストデータ) > [本節](#定数を使ってテストデータの目的と重要性を説明する)

```ABAP
CONSTANTS some_nonsense_key TYPE char8 VALUE 'ABCDEFGH'.

METHOD throws_on_invalid_entry.
  TRY.
      " when
      cut->read_entry( some_nonsense_key ).
      cl_abap_unit_assert=>fail( ).
    CATCH /clean/customizing_reader_error.
      " then
  ENDTRY.
ENDMETHOD.
```

### アサーション

> [クリーン ABAP](#クリーン-abap) > [目次](#目次) > [テスト](#テスト) > [本節](#アサーション)

#### 少なく, 焦点を絞ったアサーション

> [クリーン ABAP](#クリーン-abap) > [目次](#目次) > [テスト](#テスト) > [アサーション](#アサーション) > [本節](#少なく-焦点を絞ったアサーション)

テストメソッドが何についてのものかだけを正確にアサートし、これを少数のアサーションで行います。

```ABAP
METHOD rejects_invalid_input.
  " when
  DATA(is_valid) = cut->is_valid_input( 'SOME_RANDOM_ENTRY' ).
  " then
  cl_abap_unit_assert=>assert_false( is_valid ).
ENDMETHOD.
```

あまりにも多くのことをアサートするのは、そのメソッドが明確な焦点を持っていないことを示しています。
これは、製品コードとテストコードをあまりにも多くの場所で結合させていて、機能を変更すると、
変更された機能には実際には関与していないにもかかわらず、多数のテストを書き換える必要があります。
また、多種多様なアサーションでコードを読む人を混乱させ、その中の1つの重要なアサーションが不明瞭になってしまいます。

```ABAP
" アンチパターン
METHOD rejects_invalid_input.
  " when
  DATA(is_valid) = cut->is_valid_input( 'SOME_RANDOM_ENTRY' ).
  " then
  cl_abap_unit_assert=>assert_false( is_valid ).
  cl_abap_unit_assert=>assert_not_initial( log->get_messages( ) ).
  cl_abap_unit_assert=>assert_equals( act = sy-langu
                                      exp = 'E' ).
ENDMETHOD.
```

#### 正しいアサートタイプを使用する

> [クリーン ABAP](#クリーン-abap) > [目次](#目次) > [テスト](#テスト) > [アサーション](#アサーション) > [本節](#正しいアサートタイプを使用する)

```ABAP
cl_abap_unit_assert=>assert_equals( act = table
                                    exp = test_data ).
```

アサートは、目に見える以上のことをすることが多いです。
例えば `assert_equals` には型のマッチングや、値が異なる場合の的確な説明が含まれています。
間違った、一般的すぎるアサートを使うと、エラーメッセージから何が間違っているのかを知ることができずに、すぐにデバッガに入ってしまうことになります。

```ABAP
" アンチパターン
cl_abap_unit_assert=>assert_true( xsdbool( act = exp ) ).
```

#### 量ではなく, 内容をアサートする

> [クリーン ABAP](#クリーン-abap) > [目次](#目次) > [テスト](#テスト) > [アサーション](#アサーション) > [本節](#量ではなく-内容をアサートする)

```ABAP
assert_contains_exactly( actual   = table
                         expected = VALUE string_table( ( `ABC` ) ( `DEF` ) ( `GHI` ) ) ).
```

期待している実際の内容を表現できるのであれば、マジックナンバー数量のアサーションは書かないようにしましょう。
期待通りの内容であるにもかかわらず、数字が一致しないことがあります。
逆に、全く予想外の内容であるにもかかわらず、数字が一致する場合もあります。

```ABAP
" アンチパターン
assert_equals( act = lines( log_messages )
               exp = 3 ).
```

#### 内容ではなく, 特性でアサートする

> [クリーン ABAP](#クリーン-abap) > [目次](#目次) > [テスト](#テスト) > [アサーション](#アサーション) > [本節](#内容ではなく-特性でアサートする)

実際の内容そのものではなく、結果のメタ品質に興味がある場合は、
適切なアサートでそれを表現してください。

```ABAP
assert_all_lines_shorter_than( actual_lines        = table
                               expected_max_length = 80 ).
```

厳密な内容をアサートすると、実際にテストしたいことが不明瞭になります。
また、脆弱になります。なぜなら、リファクタリングを行うと、厳密すぎるユニットテストをすべて壊してしまうにもかかわらず、
完全に受け入れ可能な差異が発生する可能性があるためです。

```ABAP
" アンチパターン
assert_equals( act = table
               exp = VALUE string_table( ( `ABC` ) ( `DEF` ) ( `GHI` ) ) ).
```

#### 期待する例外をチェックするために FAIL を使う

> [クリーン ABAP](#クリーン-abap) > [目次](#目次) > [テスト](#テスト) > [アサーション](#アサーション) > [本節](#期待する例外をチェックするために-FAIL-を使う)

```ABAP
METHOD throws_on_empty_input.
  TRY.
      " when
      cut->do_something( '' ).
      cl_abap_unit_assert=>fail( ).
    CATCH /clean/some_exception.
      " then
  ENDTRY.
ENDMETHOD.
```

#### 予期せぬ例外はキャッチして失敗させるのではなく, 伝播させる

> [クリーン ABAP](#クリーン-abap) > [目次](#目次) > [テスト](#テスト) > [アサーション](#アサーション) > [本節](#予期せぬ例外はキャッチして失敗させるのではなく-伝播させる)

```ABAP
METHODS reads_entry FOR TESTING RAISING /clean/some_exception.

METHOD reads_entry.
  "when
  DATA(entry) = cut->read_something( ).
  "then
  cl_abap_unit_assert=>assert_not_initial( entry ).
ENDMETHOD.
```

テストコードは正常系に焦点を当てたままなので、次のような場合に比べて読みやすく、理解しやすいです。

```ABAP
" アンチパターン
METHOD reads_entry.
  TRY.
      DATA(entry) = cut->read_something( ).
    CATCH /clean/some_exception INTO DATA(unexpected_exception).
      cl_abap_unit_assert=>fail( unexpected_exception->get_text( ) ).
  ENDTRY.
  cl_abap_unit_assert=>assert_not_initial( entry ).
ENDMETHOD.
```

#### コードを短くして, 重複を避けるためにカスタムアサートを書く

> [クリーン ABAP](#クリーン-abap) > [目次](#目次) > [テスト](#テスト) > [アサーション](#アサーション) > [本節](#コードを短くして-重複を避けるためにカスタムアサートを書く)

```ABAP
METHODS assert_contains
  IMPORTING
    actual_entries TYPE STANDARD TABLE OF entries_tab
    expected_key   TYPE key_structure.

METHOD assert_contains.
  TRY.
      actual_entries[ key = expected_key ].
    CATCH cx_sy_itab_line_not_found.
      cl_abap_unit_assert=>fail( |Couldn't find the key { expected_key }| ).
  ENDTRY.
ENDMETHOD.
```

これを何度も何度もコピーペーストするのではなく。
