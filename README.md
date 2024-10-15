# Scalaによる遺伝的アルゴリズム配列生成プログラム

[Pythonで作成した配列生成プログラム](https://github.com/Harsiharsi/python-ga-layout)をScalaで書き直したもの。


## 変更点

Python版ではアクションを左右の手で独立して扱っていたが、両手で統一した。たとえば`fjdk`という打鍵列は、Python版では左手の`f d`、右手の`j k`というように、別々のアクションにわかれた。本版では`f j d k`と左右のキーが混ざった一つのアクションになる。この打鍵列に左右どちらのキーであっても同指打鍵が加わると、アクションが分断される。たとえば`fjdkr`は`f j d k`、`r`になり、`fjdku`は`f j d k`、`u`になる。


## 各種配列の評価データ

さまざまな配列を本プログラムの評価関数にかけた結果を同梱してある。一つの配列につき評価結果のファイルは3種類ある。  
*<配列名>\_data.txt*はスコアや、出現チャンクの種類とその頻度。  
*<配列名>\_action_data.txt*は出現アクションの種類とその頻度。  
*<配列名>\_ngram_to_actions.txt*は月見草4gramデータを打鍵列に変換したリスト。  
これらは[*data*ディレクトリ](./data)に配置してある。

4gramデータは以下のものを使用。  
[月見草開発に用いた文章サンプル](https://w.atwiki.jp/keylay/pages/16.html)

なお、以上のファイルでなされているような形がこの評価関数の分析を適切にできているかどうかは不明である。

評価した配列は以下の通り。  
- コンポジション#1  
  [基本データ](./data/compositionno1_data.txt)  
  [アクション関係](./data/compositionno1_action_data.txt)  
  [ngram-打鍵列](./data/compositionno1_ngram_to_actions.txt)  
- コンポジション#2  
  [基本データ](./data/compositionno2_data.txt)  
  [アクション関係](./data/compositionno2_action_data.txt)  
  [ngram-打鍵列](./data/compositionno2_ngram_to_actions.txt)  
- 月配列2-263  
  [基本データ](./data/tsuki2-263_data.txt)  
  [アクション関係](./data/tsuki2-263_action_data.txt)  
  [ngram-打鍵列](./data/tsuki2-263_ngram_to_actions.txt)  
- 月配列U9  
  [基本データ](./data/tsukiU9_data.txt)  
  [アクション関係](./data/tsukiU9_action_data.txt)  
  [ngram-打鍵列](./data/tsukiU9_ngram_to_actions.txt)  
- 幸花配列  
  [基本データ](./data/yukika_data.txt)  
  [アクション関係](./data/yukika_action_data.txt)  
  [ngram-打鍵列](./data/yukika_ngram_to_actions.txt)  
- ぶな配列v2.0  
  [基本データ](./data/buna2.0_data.txt)  
  [アクション関係](./data/buna2.0_action_data.txt)  
  [ngram-打鍵列](./data/buna2.0_ngram_to_actions.txt)  
- ミズナラ配列v1.0  
  [基本データ](./data/mizunara1.0_data.txt)  
  [アクション関係](./data/mizunara1.0_action_data.txt)  
  [ngram-打鍵列](./data/mizunara1.0_ngram_to_actions.txt)  
- 月見草配列v2  
  [基本データ](./data/tsukimisou_data.txt)  
  [アクション関係](./data/tsukimisou_action_data.txt)  
  [ngram-打鍵列](./data/tsukimisou_ngram_to_actions.txt)  
- 中指シフト月光2021/06/22  
  [基本データ](./data/gekkou20210622_data.txt)  
  [アクション関係](./data/gekkou20210622_action_data.txt)  
  [ngram-打鍵列](./data/gekkou20210622_ngram_to_actions.txt)  
