# Scalaによる遺伝的アルゴリズム配列生成プログラム

2024年12月、評価関数を大幅に改訂。


## 変更点

打鍵列を三つの性質に抽象化し、それぞれの性質の少数の項目を別々に定義することで、ありうるすべての膨大な打鍵列を直接評価することなく、容易に打鍵列を評価できるようにした。


## 生成された配列 コンポジション\#3

```
みねたゅつ  ・゜にちな
さ Sかあょ  ゃえ゛てら
しりるくい  うんまはす
ともほれそ  っのをおせ
----------------------
・・ぬゆ・  ・゜・・ひ
・ S・ぁよ  やぇ゛こふ
き・けむぃ  ぅろへ。・
、・・ー・  ・め・ぉわ
```

一度に手を打ち下ろしたさい、途切れなくひとまとめにキーを打てるようにすることをコンセプトにしている。

シフト方式は後置シフト。実装はGoogle IMEのローマ字定義を活用することを推奨。


## 各種配列の評価データ

さまざまな配列を本プログラムの評価関数にかけた結果を同梱してある。一つの配列につき評価結果のファイルは3種類ある。  
*<配列名>\_basic_data.txt*はスコアや、出現チャンクの種類とその頻度。  
*<配列名>\_action_data.txt*は出現アクションの種類とその頻度。  
*<配列名>\_ngram_to_actions.txt*は月見草4gramデータを打鍵列に変換したリスト。  

4gramデータは以下のものを使用。  
[月見草開発に用いた文章サンプル](https://w.atwiki.jp/keylay/pages/16.html)

なお、以上のファイルでなされているような形がこの評価関数の分析を適切にできているかどうかは不明である。

評価した配列は以下の通り。  
- コンポジション#1  
  [基本データ](./src/main/resources/layout_analyzed/compositionno1_basic_data.txt)  
  [アクション関係](./src/main/resources/layout_analyzed/compositionno1_action_data.txt)  
  [ngram-打鍵列](./src/main/resources/layout_analyzed/compositionno1_ngram_to_actions.txt)  
- コンポジション#2  
  [基本データ](./src/main/resources/layout_analyzed/compositionno2_basic_data.txt)  
  [アクション関係](./src/main/resources/layout_analyzed/compositionno2_action_data.txt)  
  [ngram-打鍵列](./src/main/resources/layout_analyzed/compositionno2_ngram_to_actions.txt)  
- コンポジション#3  
  [基本データ](./src/main/resources/layout_analyzed/compositionno3_basic_data.txt)  
  [アクション関係](./src/main/resources/layout_analyzed/compositionno3_action_data.txt)  
  [ngram-打鍵列](./src/main/resources/layout_analyzed/compositionno3_ngram_to_actions.txt)  
- コンポジション#3.1  
  [基本データ](./src/main/resources/layout_analyzed/compositionno3.1_basic_data.txt)  
  [アクション関係](./src/main/resources/layout_analyzed/compositionno3.1_action_data.txt)  
  [ngram-打鍵列](./src/main/resources/layout_analyzed/compositionno3.1_ngram_to_actions.txt)  
- 月配列2-263  
  [基本データ](./src/main/resources/layout_analyzed/tsuki2-263_basic_data.txt)  
  [アクション関係](./src/main/resources/layout_analyzed/tsuki2-263_action_data.txt)  
  [ngram-打鍵列](./src/main/resources/layout_analyzed/tsuki2-263_ngram_to_actions.txt)  
- 月配列U9  
  [基本データ](./src/main/resources/layout_analyzed/tsukiU9_basic_data.txt)  
  [アクション関係](./src/main/resources/layout_analyzed/tsukiU9_action_data.txt)  
  [ngram-打鍵列](./src/main/resources/layout_analyzed/tsukiU9_ngram_to_actions.txt)  
- 幸花配列  
  [基本データ](./src/main/resources/layout_analyzed/yukika_basic_data.txt)  
  [アクション関係](./src/main/resources/layout_analyzed/yukika_action_data.txt)  
  [ngram-打鍵列](./src/main/resources/layout_analyzed/yukika_ngram_to_actions.txt)  
- ぶな配列v2.0  
  [基本データ](./src/main/resources/layout_analyzed/buna2.0_basic_data.txt)  
  [アクション関係](./src/main/resources/layout_analyzed/buna2.0_action_data.txt)  
  [ngram-打鍵列](./src/main/resources/layout_analyzed/buna2.0_ngram_to_actions.txt)  
- ミズナラ配列v1.0  
  [基本データ](./src/main/resources/layout_analyzed/mizunara1.0_basic_data.txt)  
  [アクション関係](./src/main/resources/layout_analyzed/mizunara1.0_action_data.txt)  
  [ngram-打鍵列](./src/main/resources/layout_analyzed/mizunara1.0_ngram_to_actions.txt)  
- 月見草配列v2  
  [基本データ](./src/main/resources/layout_analyzed/tsukimisouv2_basic_data.txt)  
  [アクション関係](./src/main/resources/layout_analyzed/tsukimisouv2_action_data.txt)  
  [ngram-打鍵列](./src/main/resources/layout_analyzed/tsukimisouv2_ngram_to_actions.txt)  
- 中指シフト月光2021/06/22  
  [基本データ](./src/main/resources/layout_analyzed/gekkou20210622_basic_data.txt)  
  [アクション関係](./src/main/resources/layout_analyzed/gekkou20210622_action_data.txt)  
  [ngram-打鍵列](./src/main/resources/layout_analyzed/gekkou20210622_ngram_to_actions.txt)  
