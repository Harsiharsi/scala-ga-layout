// import scala.io.{Source => S}
// import scala.util.{Random => R}
// import scala.collection.{mutable => mut}
// import java.io.PrintWriter
// 
// import layout.{LayoutEn => L}
// 
// import layout.TypeAlias._
// import layout.Layout
// import layout.LayoutTools
// import layout.LayoutJa
// import layout.LayoutSimpleEn
// import layout.Evaluator
// 
// 
// object Test {
//   def apply(): Unit = {
//   }
// 
//   type DataTuple = (
//     Double,
//     Int,
//     mut.Map[Chunk, Int],
//     mut.Map[HandAction, Int],
//     mut.Map[NonChunk, Int],
//     mut.ArrayBuffer[(Ngram, List[HandAction], List[NonChunk], Int)]
//   )
//   def evaluateFitness(charToKey: Map[String, String], tools: LayoutTools): DataTuple = {
//     import tools._
// 
//     var score: Double = 0.0
//     var strokes: Int = 0
//     var chunkFreq: mut.Map[Chunk, Int] = mut.Map()
//     var actionFreq: mut.Map[HandAction, Int] = mut.Map()
//     var interactionFreq: mut.Map[NonChunk, Int] = mut.Map()
//     var ngramToAction: mut.ArrayBuffer[(Ngram, List[HandAction], List[NonChunk], Int)] = mut.ArrayBuffer()
//     var ngramCounter = 1
//     ngrams.keys.toList.foreach(ng => {
//       val typingString = ng.map(c => charToKey.get(c.toString).getOrElse("")).mkString
//       val frequency = ngrams(ng)
// 
//       score += evaluator(typingString) * frequency
// 
//       strokes += typingString.length * frequency
// 
//       val chunks = evaluator.typingStringToChunks(typingString)
//       chunks.foreach(ch => chunkFreq.get(ch) match {
//         case Some(freq) => chunkFreq += ch -> (freq + 1)
//         case None => chunkFreq += ch -> frequency
//       })
// 
//       val actions = evaluator.chunksToHandActions(chunks)
//       actions.foreach(a => actionFreq.get(a) match {
//         case Some(freq) => actionFreq += a -> (freq + 1)
//         case None => actionFreq += a -> frequency
//       })
// 
//       val interactions = evaluator.actionsToInteractions(actions)
//       interactions.foreach(ia => interactionFreq.get(ia) match {
//         case Some(freq) => interactionFreq += ia -> (freq + 1)
//         case None => interactionFreq += ia -> frequency
//       })
// 
//       if (ngramCounter <= 5000) ngramToAction :+= (ng, actions, interactions, frequency)
//     })
// 
//     (score, strokes, chunkFreq, actionFreq, interactionFreq, ngramToAction)
//   }
// 
//   def writeFiles(layoutName: String, fileName: String, data: DataTuple): Unit = {
//     val (score, strokes, chunkFreq, actionFreq, interactionFreq, ngramToAction) = data
//     val chunkData = chunkFreq.toList.sortBy{case (x, y) => x.length -> y}.reverse
//     val actionData = actionFreq.toList.sortBy(_._2).reverse
//     val interactionData = interactionFreq.toList.sortBy(_._2).reverse
// 
// 
//     val basicFile = new PrintWriter("./data/" + fileName + "_data.txt", "UTF-8")
//     basicFile.println(layoutName)
//     basicFile.println()
// 
//     basicFile.println("スコア:\t" + score.toString)
//     basicFile.println("打鍵数:\t" + strokes.toString)
//     basicFile.println("総チャンク数:\t" + chunkData.map(_._2).sum)
//     basicFile.println()
// 
//     basicFile.println("チャンクに含まれる打鍵数ごとの頻度")
//     val maxChunkLength = chunkData.map(_._1.length).max
//     (1 to maxChunkLength).reverse.foreach(x => {
//       val newFreq = (for ((chunk, freq) <- chunkData if chunk.length == x) yield freq).sum
// 
//       basicFile.println(s"$x" + s"打鍵チャンク:\t$newFreq")
//     })
//     basicFile.println()
// 
//     val totalInteractionFreq = interactionData.map(_._2).sum
//     basicFile.println("総インターアクション:\t" + totalInteractionFreq)
//     basicFile.println()
// 
//     basicFile.println("チャンク頻度")
//     chunkData.foreach{case (x, y) => basicFile.println(s"$x\t$y")}
//     basicFile.println()
// 
//     basicFile.println("インターアクション頻度")
//     interactionData.foreach{case (x, y) => basicFile.println(s"$x\t$y")}
// 
//     basicFile.close()
// 
// 
//     val actionFile = new PrintWriter("./data/" + fileName + "_action_data.txt", "UTF-8")
//     actionFile.println(layoutName)
//     actionFile.println()
// 
//     actionFile.println("総アクション数:\t" + actionData.map(_._2).sum)
//     actionFile.println()
// 
//     actionFile.println("アクションに含まれるチャンク数ごとの頻度")
//     val maxActionLength = actionData.map(_._1.length).max
//     (1 to maxActionLength).reverse.foreach(x => {
//       val newFreq = (for ((a, freq) <- actionData if a.length == x) yield freq).sum
// 
//       actionFile.println(s"$x" + s"チャンク:\t$newFreq")
//     })
//     actionFile.println()
// 
//     actionFile.println("アクション頻度")
//     actionData.map{case (x, y) => x.mkString(" ") -> y}.foreach{case (x, y) => actionFile.println(s"$x\t$y")}
// 
//     actionFile.close()
// 
// 
//     val ngramToactionFile = new PrintWriter("./data/" + fileName + "_ngram_to_actions.txt", "UTF-8")
//     ngramToactionFile.println(layoutName)
//     ngramToactionFile.println()
//     ngramToactionFile.println("ngram -> アクション列")
//     val ngramToActionData = ngramToAction.toList.sortBy(_._4).reverse.foreach{
//       case (ng, actions, interactions, _) =>
//         val actStrings = actions.map(x => "(" + x.mkString(" ") + ")")
//         val interactStrings = interactions.map(x => " " + x + " ")
//         val s = (actStrings.zip(interactStrings).map{case (x, y) => List(x, y)}.flatten :+ actStrings.last).mkString(" ")
// 
//         ngramToactionFile.println(s"$ng\t$s")
//     }
// 
//     ngramToactionFile.close()
//   }
// 
//   def evaluateTsuki2_263(): Unit = {
//     val charToKey = Map(
//       "あ" -> "kf",
//       "い" -> "i",
//       "う" -> "j",
//       "え" -> "du",
//       "お" -> "dj",
//       "ぁ" -> "kq",
//       "ぃ" -> "ka",
//       "ぅ" -> "kz",
//       "ぇ" -> "dp",
//       "ぉ" -> "d/",
//       "か" -> "s",
//       "き" -> ";",
//       "く" -> "h",
//       "け" -> "x",
//       "こ" -> "w",
//       "が" -> "sl",
//       "ぎ" -> ";l",
//       "ぐ" -> "hl",
//       "げ" -> "xl",
//       "ご" -> "wl",
//       "さ" -> "b",
//       "し" -> "e",
//       "す" -> "z",
//       "せ" -> "kc",
//       "そ" -> "q",
//       "ざ" -> "bl",
//       "じ" -> "el",
//       "ず" -> "zl",
//       "ぜ" -> "kcl",
//       "ぞ" -> "ql",
//       "た" -> "g",
//       "ち" -> "[",
//       "つ" -> "y",
//       "て" -> "r",
//       "と" -> "f",
//       "だ" -> "gl",
//       "ぢ" -> "[l",
//       "づ" -> "yl",
//       "で" -> "rl",
//       "ど" -> "fl",
//       "な" -> "v",
//       "に" -> "c",
//       "ぬ" -> "dy",
//       "ね" -> "d,",
//       "の" -> "o",
//       "は" -> "a",
//       "ひ" -> "kw",
//       "ふ" -> "kr",
//       "へ" -> "kx",
//       "ほ" -> "ke",
//       "ば" -> "al",
//       "び" -> "kwl",
//       "ぶ" -> "krl",
//       "べ" -> "kxl",
//       "ぼ" -> "kel",
//       "ぱ" -> "a/",
//       "ぴ" -> "kw/",
//       "ぷ" -> "kr/",
//       "ぺ" -> "kx/",
//       "ぽ" -> "ke/",
//       "ま" -> "dh",
//       "み" -> "di",
//       "む" -> "dn",
//       "め" -> "kt",
//       "も" -> "dk",
//       "や" -> "do",
//       "ゆ" -> "d;",
//       "よ" -> "kg",
//       "ゃ" -> "kb",
//       "ゅ" -> "kv",
//       "ょ" -> "t",
//       "ら" -> "kd",
//       "り" -> "p",
//       "る" -> "m",
//       "れ" -> "'",
//       "ろ" -> "dm",
//       "わ" -> "dl",
//       "を" -> "ks",
//       "っ" -> "n",
//       "ん" -> "u",
//       "ー" -> "d.",
//       "ヴ" -> "jl",
//       "、" -> ",",
//       "。" -> "."
//     )
// 
//     // 1.8805508314999814E9
//     writeFiles("月配列2-263", "tsuki2-263", evaluateFitness(charToKey, LayoutJa))
//   }
// 
//   def evaluateTsukimisou(): Unit = {
//     val charToKey = Map(
//       "あ" -> "js",
//       "い" -> "k",
//       "う" -> "d",
//       "え" -> "ls",
//       "お" -> "kf",
//       "ぁ" -> "/f",
//       "ぃ" -> "/s",
//       "ぅ" -> ",e",
//       "ぇ" -> "/e",
//       "ぉ" -> ",s",
//       "か" -> "w",
//       "き" -> "u",
//       "く" -> ";",
//       "け" -> "le",
//       "こ" -> "h",
//       "が" -> "r",
//       "ぎ" -> "jv",
//       "ぐ" -> "fs",
//       "げ" -> ";e",
//       "ご" -> ";s",
//       "さ" -> "of",
//       "し" -> "n",
//       "す" -> ".",
//       "せ" -> "oe",
//       "そ" -> "os",
//       "ざ" -> "hf",
//       "じ" -> "ja",
//       "ず" -> ";f",
//       "ぜ" -> "he",
//       "ぞ" -> "hs",
//       "た" -> "o",
//       "ち" -> "[",
//       "つ" -> "p",
//       "て" -> "g",
//       "と" -> "l",
//       "だ" -> "q",
//       "ぢ" -> "jm",
//       "づ" -> "jc",
//       "で" -> "z",
//       "ど" -> "lf",
//       "な" -> "m",
//       "に" -> "a",
//       "ぬ" -> "jx",
//       "ね" -> "me",
//       "の" -> "v",
//       "は" -> "b",
//       "ひ" -> "jk",
//       "ふ" -> "we",
//       "へ" -> "es",
//       "ほ" -> "wf",
//       "ば" -> "df",
//       "び" -> "ji",
//       "ぶ" -> "fe",
//       "べ" -> "ds",
//       "ぼ" -> "sf",
//       "ぱ" -> "ef",
//       "ぴ" -> "j'",
//       "ぷ" -> "se",
//       "ぺ" -> "cs",
//       "ぽ" -> "xf",
//       "ま" -> "c",
//       "み" -> "jd",
//       "む" -> "ie",
//       "め" -> "ke",
//       "も" -> "x",
//       "や" -> "jf",
//       "ゆ" -> "if",
//       "よ" -> "mf",
//       "ゃ" -> "e",
//       "ゅ" -> "f",
//       "ょ" -> "s",
//       "ら" -> "ks",
//       "り" -> "'",
//       "る" -> ",",
//       "れ" -> "/",
//       "ろ" -> "is",
//       "わ" -> "je",
//       "を" -> "t",
//       "っ" -> "j",
//       "ん" -> "i",
//       "ー" -> "ms",
//       "ヴ" -> ",j",
//       "、" -> "ee",
//       "。" -> "ff"
//     )
//     //2.51909318839981E9
// 
//     writeFiles("月見草配列", "tsukimisou", evaluateFitness(charToKey, LayoutJa))
//   }
// 
//   def evaluateYukika(): Unit = {
//     val charToKey = Map(
//       "あ" -> "di",
//       "い" -> "i",
//       "う" -> "m",
//       "え" -> ";",
//       "お" -> "ks",
//       "ぁ" -> "kq",
//       "ぃ" -> "ka",
//       "ぅ" -> "ka",
//       "ぇ" -> "dp",
//       "ぉ" -> "d/",
//       "か" -> "f",
//       "き" -> "v",
//       "く" -> "n",
//       "け" -> "r",
//       "こ" -> "q",
//       "が" -> "fl",
//       "ぎ" -> "vl",
//       "ぐ" -> "nl",
//       "げ" -> "rl",
//       "ご" -> "ql",
//       "さ" -> "b",
//       "し" -> ";",
//       "す" -> "z",
//       "せ" -> "c",
//       "そ" -> "kc",
//       "ざ" -> "bl",
//       "じ" -> ";l",
//       "ず" -> "zl",
//       "ぜ" -> "cl",
//       "ぞ" -> "kcl",
//       "た" -> "a",
//       "ち" -> "dl",
//       "つ" -> "dj",
//       "て" -> "s",
//       "と" -> "e",
//       "だ" -> "al",
//       "ぢ" -> "dll",
//       "づ" -> "djl",
//       "で" -> "sl",
//       "ど" -> "el",
//       "な" -> "w",
//       "に" -> "do",
//       "ぬ" -> "dy",
//       "ね" -> "kw",
//       "の" -> "o",
//       "は" -> "g",
//       "ひ" -> "d,",
//       "ふ" -> "kg",
//       "へ" -> "kb",
//       "ほ" -> "kr",
//       "ば" -> "gl",
//       "び" -> "d,l",
//       "ぶ" -> "kgl",
//       "べ" -> "kbl",
//       "ぼ" -> "krl",
//       "ぱ" -> "g/",
//       "ぴ" -> "d,/",
//       "ぷ" -> "kg/",
//       "ぺ" -> "kb/",
//       "ぽ" -> "kr/",
//       "ま" -> "dk",
//       "み" -> "dn",
//       "む" -> "y",
//       "め" -> "[",
//       "も" -> "f",
//       "や" -> "dh",
//       "ゆ" -> "kt",
//       "よ" -> "kv",
//       "ゃ" -> "ke",
//       "ゅ" -> "kx",
//       "ょ" -> "x",
//       "ら" -> "kd",
//       "り" -> "dm",
//       "る" -> "u",
//       "れ" -> "p",
//       "ろ" -> "t",
//       "わ" -> "du",
//       "を" -> "d.",
//       "っ" -> "h",
//       "ん" -> "j",
//       "ー" -> "'",
//       "ヴ" -> "ml",
//       "、" -> ",",
//       "。" -> "."
//     )
// 
//     writeFiles("幸花配列", "yukika", evaluateFitness(charToKey, LayoutJa))
//   }
// 
//   def evaluateGekkou20210622(): Unit = {
//     val charToKey = Map(
//       "あ" -> "kg",
//       "い" -> "i",
//       "う" -> "j",
//       "え" -> "do",
//       "お" -> "dh",
//       "ぁ" -> "kq",
//       "ぃ" -> "ka",
//       "ぅ" -> "kz",
//       "ぇ" -> "dp",
//       "ぉ" -> "d/",
//       "か" -> "s",
//       "き" -> "x",
//       "く" -> "h",
//       "け" -> "ks",
//       "こ" -> "f",
//       "が" -> "sl",
//       "ぎ" -> "xl",
//       "ぐ" -> "hl",
//       "げ" -> "ksl",
//       "ご" -> "fl",
//       "さ" -> "b",
//       "し" -> "a",
//       "す" -> "z",
//       "せ" -> "ke",
//       "そ" -> "kv",
//       "ざ" -> "bl",
//       "じ" -> "al",
//       "ず" -> "zl",
//       "ぜ" -> "kel",
//       "ぞ" -> "kvl",
//       "た" -> "g",
//       "ち" -> "d;",
//       "つ" -> "y",
//       "て" -> "r",
//       "と" -> "w",
//       "だ" -> "gl",
//       "ぢ" -> "d;l",
//       "づ" -> "yl",
//       "で" -> "rl",
//       "ど" -> "wl",
//       "な" -> "v",
//       "に" -> "c",
//       "ぬ" -> "d.",
//       "ね" -> "kx",
//       "の" -> "o",
//       "は" -> "e",
//       "ひ" -> "d,",
//       "ふ" -> "dj",
//       "へ" -> "kc",
//       "ほ" -> "kr",
//       "ば" -> "el",
//       "び" -> "d,l",
//       "ぶ" -> "djl",
//       "べ" -> "kcl",
//       "ぼ" -> "krl",
//       "ぱ" -> "e;",
//       "ぴ" -> "d,;",
//       "ぷ" -> "dj;",
//       "ぺ" -> "kc;",
//       "ぽ" -> "kr;",
//       "ま" -> "n",
//       "み" -> "du",
//       "む" -> "dy",
//       "め" -> "kt",
//       "も" -> "q",
//       "や" -> "kb",
//       "ゆ" -> "d'",
//       "よ" -> "di",
//       "ゃ" -> "dg",
//       "ゅ" -> "df",
//       "ょ" -> "t",
//       "ら" -> "kf",
//       "り" -> "p",
//       "る" -> "m",
//       "れ" -> "kw",
//       "ろ" -> "dm",
//       "わ" -> "dn",
//       "を" -> "dl",
//       "っ" -> "[",
//       "ん" -> "u",
//       "ー" -> "'",
//       "ヴ" -> "jl",
//       "、" -> ",",
//       "。" -> "."
//     )
//     writeFiles("中指シフト月光 2021/6/22版", "gekkou20210622", evaluateFitness(charToKey, LayoutJa))
//   }
// 
//   def evaluateTsukiU9(): Unit = {
//     val charToKey = Map(
//       "あ" -> "kf",
//       "い" -> "i",
//       "う" -> "j",
//       "え" -> "do",
//       "お" -> "dj",
//       "ぁ" -> "kq",
//       "ぃ" -> "ka",
//       "ぅ" -> "kz",
//       "ぇ" -> "dp",
//       "ぉ" -> "d/",
//       "か" -> "o",
//       "き" -> ";",
//       "く" -> "h",
//       "け" -> "ks",
//       "こ" -> "r",
//       "が" -> "so",
//       "ぎ" -> "s;",
//       "ぐ" -> "sh",
//       "げ" -> "ls",
//       "ご" -> "lr",
//       "さ" -> "b",
//       "し" -> "e",
//       "す" -> "z",
//       "せ" -> "kc",
//       "そ" -> "dl",
//       "ざ" -> "lb",
//       "じ" -> "le",
//       "ず" -> "lz",
//       "ぜ" -> "lc",
//       "ぞ" -> "sl",
//       "た" -> "g",
//       "ち" -> "'",
//       "つ" -> "y",
//       "て" -> "f",
//       "と" -> "w",
//       "だ" -> "lg",
//       "ぢ" -> "s'",
//       "づ" -> "sy",
//       "で" -> "lf",
//       "ど" -> "lw",
//       "な" -> "v",
//       "に" -> "c",
//       "ぬ" -> "dy",
//       "ね" -> "ke",
//       "の" -> ",",
//       "は" -> "a",
//       "ひ" -> "dn",
//       "ふ" -> "d,",
//       "へ" -> "/",
//       "ほ" -> "dk",
//       "ば" -> "la",
//       "び" -> "sn",
//       "ぶ" -> "s,",
//       "べ" -> "s/",
//       "ぼ" -> "sk",
//       "ぱ" -> "lx",
//       "ぴ" -> "su",
//       "ぷ" -> "sm",
//       "ぺ" -> "s.",
//       "ぽ" -> "si",
//       "ま" -> "dh",
//       "み" -> "di",
//       "む" -> "du",
//       "め" -> "kt",
//       "も" -> ".",
//       "や" -> "kx",
//       "ゆ" -> "kb",
//       "よ" -> "kv",
//       "ゃ" -> "kw",
//       "ゅ" -> "q",
//       "ょ" -> "t",
//       "ら" -> "kr",
//       "り" -> "p",
//       "る" -> "m",
//       "れ" -> "kg",
//       "ろ" -> "d;",
//       "わ" -> "dm",
//       "を" -> "x",
//       "っ" -> "n",
//       "ん" -> "u",
//       "ー" -> "[",
//       "ヴ" -> "sj",
//       "、" -> "kd",
//       "。" -> "ld"
//     )
//     writeFiles("月配列U9", "tsukiU9", evaluateFitness(charToKey, LayoutJa))
//   }
// 
//   def evaluateBuna2_0(): Unit = {
//     val charToKey = Map(
//       "あ" -> "kg",
//       "い" -> "l",
//       "う" -> "m",
//       "え" -> "ko",
//       "お" -> "kl",
//       "ぁ" -> "k/",
//       "ぃ" -> "k.",
//       "ぅ" -> "dt",
//       "ぇ" -> "ki",
//       "ぉ" -> "k,",
//       "か" -> "s",
//       "き" -> "p",
//       "く" -> "h",
//       "け" -> "kd",
//       "こ" -> "q",
//       "が" -> "x",
//       "ぎ" -> "d,",
//       "ぐ" -> "d.",
//       "げ" -> "di",
//       "ご" -> "kc",
//       "さ" -> "ds",
//       "し" -> "i",
//       "す" -> "v",
//       "せ" -> "ks",
//       "そ" -> "kr",
//       "ざ" -> "kx",
//       "じ" -> "do",
//       "ず" -> "dn",
//       "ぜ" -> "dg",
//       "ぞ" -> "dx",
//       "た" -> "g",
//       "ち" -> "km",
//       "つ" -> "kj",
//       "て" -> "r",
//       "と" -> ";",
//       "だ" -> "z",
//       "ぢ" -> "de",
//       "づ" -> "dq",
//       "で" -> "a",
//       "ど" -> "kf",
//       "な" -> "f",
//       "に" -> "w",
//       "ぬ" -> "dy",
//       "ね" -> "kv",
//       "の" -> "o",
//       "は" -> "e",
//       "ひ" -> "dh",
//       "ふ" -> "kz",
//       "へ" -> "kt",
//       "ほ" -> "ke",
//       "ば" -> "da",
//       "び" -> "kh",
//       "ぶ" -> "d;",
//       "べ" -> "kq",
//       "ぼ" -> "kw",
//       "ぱ" -> "du",
//       "ぴ" -> "dz",
//       "ぷ" -> "dr",
//       "ぺ" -> "ku",
//       "ぽ" -> "d/",
//       "ま" -> "c",
//       "み" -> "kn",
//       "む" -> "dm",
//       "め" -> "dk",
//       "も" -> "t",
//       "や" -> "kp",
//       "ゆ" -> "dv",
//       "よ" -> "kb",
//       "ゃ" -> "dp",
//       "ゅ" -> "df",
//       "ょ" -> "b",
//       "ら" -> "'",
//       "り" -> "y",
//       "る" -> "n",
//       "れ" -> "[",
//       "ろ" -> "k;",
//       "わ" -> "ka",
//       "を" -> "dj",
//       "っ" -> "u",
//       "ん" -> "j",
//       "ー" -> "dl",
//       "ヴ" -> "db",
//       "、" -> ",",
//       "。" -> "."
//     )
//     writeFiles("ぶな配列2.0", "buna2.0", evaluateFitness(charToKey, LayoutJa))
//   }
// 
//   def evaluateMizunara1_0(): Unit = {
//     val charToKey = Map(
//       "あ" -> "ka",
//       "い" -> ";",
//       "う" -> "m",
//       "え" -> "do",
//       "お" -> "kf",
//       "ぁ" -> "le",
//       "ぃ" -> "lw",
//       "ぅ" -> "s.",
//       "ぇ" -> "lx",
//       "ぉ" -> "dy",
//       "か" -> "a",
//       "き" -> "h",
//       "く" -> "n",
//       "け" -> "ks",
//       "こ" -> "z",
//       "が" -> "x",
//       "ぎ" -> "sh",
//       "ぐ" -> "sn",
//       "げ" -> "lc",
//       "ご" -> "kq",
//       "さ" -> "kg",
//       "し" -> "i",
//       "す" -> "kd",
//       "せ" -> "ls",
//       "そ" -> "lg",
//       "ざ" -> "lb",
//       "じ" -> "dl",
//       "ず" -> "sp",
//       "ぜ" -> "kc",
//       "ぞ" -> "lt",
//       "た" -> "v",
//       "ち" -> "dk",
//       "つ" -> "dj",
//       "て" -> "r",
//       "と" -> "o",
//       "だ" -> "c",
//       "ぢ" -> "s/",
//       "づ" -> "kw",
//       "で" -> "t",
//       "ど" -> "lr",
//       "な" -> "f",
//       "に" -> "w",
//       "ぬ" -> "dn",
//       "ね" -> "kv",
//       "の" -> "p",
//       "は" -> "e",
//       "ひ" -> "dh",
//       "ふ" -> "lv",
//       "へ" -> "kt",
//       "ほ" -> "kr",
//       "ば" -> "so",
//       "び" -> "lz",
//       "ぶ" -> "di",
//       "べ" -> "lq",
//       "ぼ" -> "ke",
//       "ぱ" -> "su",
//       "ぴ" -> "d/",
//       "ぷ" -> "du",
//       "ぺ" -> "d,",
//       "ぽ" -> "si",
//       "ま" -> "d;",
//       "み" -> "dm",
//       "む" -> "sm",
//       "め" -> "sk",
//       "も" -> "q",
//       "や" -> "kz",
//       "ゆ" -> "kx",
//       "よ" -> "kb",
//       "ゃ" -> "dp",
//       "ゅ" -> "d.",
//       "ょ" -> "b",
//       "ら" -> "la",
//       "り" -> "sl",
//       "る" -> "g",
//       "れ" -> "y",
//       "ろ" -> "lf",
//       "わ" -> "ld",
//       "を" -> "s;",
//       "っ" -> "u",
//       "ん" -> "j",
//       "ー" -> "sj",
//       "ヴ" -> "s,",
//       "、" -> ",",
//       "。" -> "."
//     )
//     writeFiles("ミズナラ配列1.0", "mizunara1.0", evaluateFitness(charToKey, LayoutJa))
//   }
// 
//   def evaluateCompositionNo1(): Unit = {
//     val charToKey = Map(
//       "あ" -> ".",
//       "い" -> ";",
//       "う" -> "l",
//       "え" -> "/",
//       "お" -> ",",
//       "ぁ" -> ".e",
//       "ぃ" -> ";e",
//       "ぅ" -> "le",
//       "ぇ" -> "/e",
//       "ぉ" -> ",e",
//       "か" -> "r",
//       "き" -> "5",
//       "く" -> "4",
//       "け" -> "re",
//       "こ" -> "t",
//       "が" -> "rj",
//       "ぎ" -> "5j",
//       "ぐ" -> "4j",
//       "げ" -> "rej",
//       "ご" -> "tj",
//       "さ" -> "b",
//       "し" -> "f",
//       "す" -> "g",
//       "せ" -> "v",
//       "そ" -> "ge",
//       "ざ" -> "bj",
//       "じ" -> "fj",
//       "ず" -> "gj",
//       "ぜ" -> "vj",
//       "ぞ" -> "gej",
//       "た" -> "q",
//       "ち" -> "1",
//       "つ" -> "2",
//       "て" -> "w",
//       "と" -> "s",
//       "だ" -> "qj",
//       "ぢ" -> "1j",
//       "づ" -> "2j",
//       "で" -> "wj",
//       "ど" -> "sj",
//       "な" -> "8",
//       "に" -> "9",
//       "ぬ" -> "9e",
//       "ね" -> "ie",
//       "の" -> "i",
//       "は" -> "a",
//       "ひ" -> "z",
//       "ふ" -> "x",
//       "へ" -> "se",
//       "ほ" -> "ae",
//       "ば" -> "aj",
//       "び" -> "zj",
//       "ぶ" -> "xj",
//       "べ" -> "sej",
//       "ぼ" -> "aej",
//       "ぱ" -> "ajj",
//       "ぴ" -> "zjj",
//       "ぷ" -> "xjj",
//       "ぺ" -> "sejj",
//       "ぽ" -> "aejj",
//       "ま" -> "u",
//       "み" -> "7",
//       "む" -> "7e",
//       "め" -> "ue",
//       "も" -> "m",
//       "や" -> "ye",
//       "ゆ" -> "ne",
//       "よ" -> "he",
//       "ゃ" -> "y",
//       "ゅ" -> "n",
//       "ょ" -> "h",
//       "ら" -> "pe",
//       "り" -> "p",
//       "る" -> "o",
//       "れ" -> "0",
//       "ろ" -> "oe",
//       "わ" -> "ke",
//       "を" -> "k",
//       "っ" -> "3",
//       "ん" -> "d",
//       "ー" -> "c",
//       "ヴ" -> "lj",
//       "、" -> "fe",
//       "。" -> "we"
//     )
//     writeFiles("Composition #1", "compositionno1", evaluateFitness(charToKey, LayoutJa))
//   }
// 
//   def evaluateCompositionNo2(): Unit = {
//     val charToKey = Map(
//       "あ" -> "4",
//       "い" -> "o",
//       "う" -> "j",
//       "え" -> "9",
//       "お" -> "0",
//       "ぁ" -> "4i",
//       "ぃ" -> "oi",
//       "ぅ" -> "ji",
//       "ぇ" -> "9i",
//       "ぉ" -> "0i",
//       "か" -> ";",
//       "き" -> "r",
//       "く" -> "w",
//       "け" -> "k",
//       "こ" -> "g",
//       "が" -> ";l",
//       "ぎ" -> "rl",
//       "ぐ" -> "wl",
//       "げ" -> "kl",
//       "ご" -> "gl",
//       "さ" -> "/",
//       "し" -> "e",
//       "す" -> "3",
//       "せ" -> "mi",
//       "そ" -> "b",
//       "ざ" -> "/l",
//       "じ" -> "el",
//       "ず" -> "3l",
//       "ぜ" -> "mil",
//       "ぞ" -> "bl",
//       "た" -> "n",
//       "ち" -> "x",
//       "つ" -> "5",
//       "て" -> "m",
//       "と" -> "s",
//       "だ" -> "nl",
//       "ぢ" -> "xl",
//       "づ" -> "5l",
//       "で" -> "ml",
//       "ど" -> "sl",
//       "な" -> "u",
//       "に" -> "z",
//       "ぬ" -> "/i",
//       "ね" -> "si",
//       "の" -> "a",
//       "は" -> "d",
//       "ひ" -> "di",
//       "ふ" -> "ni",
//       "へ" -> "ei",
//       "ほ" -> ";i",
//       "ば" -> "dl",
//       "び" -> "dil",
//       "ぶ" -> "nil",
//       "べ" -> "eil",
//       "ぼ" -> ";il",
//       "ぱ" -> "d7",
//       "ぴ" -> "di7",
//       "ぷ" -> "ni7",
//       "ぺ" -> "di7",
//       "ぽ" -> ";i7",
//       "ま" -> "q",
//       "み" -> "fi",
//       "む" -> "ti",
//       "め" -> "wi",
//       "も" -> "c",
//       "や" -> "yi",
//       "ゆ" -> "pi",
//       "よ" -> "hi",
//       "ゃ" -> "y",
//       "ゅ" -> "p",
//       "ょ" -> "h",
//       "ら" -> "1",
//       "り" -> "8",
//       "る" -> "t",
//       "れ" -> ",",
//       "ろ" -> "ui",
//       "わ" -> "ai",
//       "を" -> "2",
//       "っ" -> "v",
//       "ん" -> "f",
//       "ー" -> ".",
//       "ヴ" -> "jl",
//       "、" -> "3i",
//       "。" -> "ri"
//     )
//     //2.7474492673998294E9
//     println(writeFiles("Composition #2", "compositionno2", evaluateFitness(charToKey, LayoutJa)))
//   }

  //def evaluateCompositionNo2(): Unit = {
  //  val chr = Map(
  //    "1" -> "ら", "2" -> "を", "3" -> "す", "4" -> "あ", "5" -> "つ",              "7" -> "゜", "8" -> "り", "9" -> "え", "0" -> "お",
  //    "q" -> "ま", "w" -> "く", "e" -> "し", "r" -> "き", "t" -> "る", "y" -> "ゃ", "u" -> "な", "i" -> " S", "o" -> "い", "p" -> "ゅ",
  //    "a" -> "の", "s" -> "と", "d" -> "は", "f" -> "ん", "g" -> "こ", "h" -> "ょ", "j" -> "う", "k" -> "け", "l" -> "゛", ";" -> "か",
  //    "z" -> "に", "x" -> "ち", "c" -> "も", "v" -> "っ", "b" -> "そ", "n" -> "た", "m" -> "て", "," -> "れ", "." -> "ー", "/" -> "さ",

  //    "!" -> "", "@" -> "", "#" -> "、", "$" -> "", "%" -> "",                      "&" -> "゜", "*" -> "", "(" -> "ぇ", ")" -> "ぉ",
  //    "Q" -> "", "W" -> "め", "E" -> "へ", "R" -> "。", "T" -> "む", "Y" -> "や", "U" -> "ろ", "I" -> " S", "O" -> "ぃ", "P" -> "ゆ",
  //    "A" -> "", "S" -> "ね", "D" -> "ひ", "F" -> "み", "G" -> "", "H" -> "よ", "J" -> "ぅ", "K" -> "", "L" -> "゛", ":" -> "ほ",
  //    "Z" -> "", "X" -> "", "C" -> "", "V" -> "", "B" -> "", "N" -> "ふ", "M" -> "せ", "<" -> "", ">" -> "", "?" -> "ぬ"
  //  )
  //  val l1: Layout = layout.LayoutJa(chr).evaluate
  //  // らをすあつ   *゜りえお
  //  // まくしきる  ゃな Sいゅ
  //  // のとはんこ  ょうけ゛か
  //  // にちもっそ  たてれーさ
  //  // ----------------------
  //  //  * *、 * *   *゜ *ぇぉ
  //  //  *めへ。む  やろ Sぃゆ
  //  //  *ねひみ *  よぅ *゛ほ
  //  //  * * * * *  ふせ * *ぬ
  //  // 1.7769719610999858E9
  //  println(l1)
  //  println()
  //}
// }

// val chr = Map(
//   "1" -> "", "2" -> "", "3" -> "", "4" -> "", "5" -> "",              "7" -> "", "8" -> "", "9" -> "", "0" -> "",
//   "q" -> "", "w" -> "", "e" -> "", "r" -> "", "t" -> "", "y" -> "", "u" -> "", "i" -> "", "o" -> "", "p" -> "",
//   "a" -> "", "s" -> "", "d" -> "", "f" -> "", "g" -> "", "h" -> "", "j" -> "", "k" -> "", "l" -> "", ";" -> "",
//   "z" -> "", "x" -> "", "c" -> "", "v" -> "", "b" -> "", "n" -> "", "m" -> "", "," -> "", "." -> "", "/" -> "",
// 
//   "!" -> "", "@" -> "", "#" -> "", "$" -> "", "%" -> "",                       "&" -> "", "*" -> "", "(" -> "", ")" -> "",
//   "Q" -> "", "W" -> "", "E" -> "", "R" -> "", "T" -> "", "Y" -> "", "U" -> "", "I" -> "", "O" -> "", "P" -> "",
//   "A" -> "", "S" -> "", "D" -> "", "F" -> "", "G" -> "", "H" -> "", "J" -> "", "K" -> "", "L" -> "", ":" -> "",
//   "Z" -> "", "X" -> "", "C" -> "", "V" -> "", "B" -> "", "N" -> "", "M" -> "", "<" -> "", ">" -> "", "?" -> ""
//   )

// val charToKey = Map(
//   "あ" -> "",
//   "い" -> "",
//   "う" -> "",
//   "え" -> "",
//   "お" -> "",
//   "ぁ" -> "",
//   "ぃ" -> "",
//   "ぅ" -> "",
//   "ぇ" -> "",
//   "ぉ" -> "",
//   "か" -> "",
//   "き" -> "",
//   "く" -> "",
//   "け" -> "",
//   "こ" -> "",
//   "が" -> "",
//   "ぎ" -> "",
//   "ぐ" -> "",
//   "げ" -> "",
//   "ご" -> "",
//   "さ" -> "",
//   "し" -> "",
//   "す" -> "",
//   "せ" -> "",
//   "そ" -> "",
//   "ざ" -> "",
//   "じ" -> "",
//   "ず" -> "",
//   "ぜ" -> "",
//   "ぞ" -> "",
//   "た" -> "",
//   "ち" -> "",
//   "つ" -> "",
//   "て" -> "",
//   "と" -> "",
//   "だ" -> "",
//   "ぢ" -> "",
//   "づ" -> "",
//   "で" -> "",
//   "ど" -> "",
//   "な" -> "",
//   "に" -> "",
//   "ぬ" -> "",
//   "ね" -> "",
//   "の" -> "",
//   "は" -> "",
//   "ひ" -> "",
//   "ふ" -> "",
//   "へ" -> "",
//   "ほ" -> "",
//   "ば" -> "",
//   "び" -> "",
//   "ぶ" -> "",
//   "べ" -> "",
//   "ぼ" -> "",
//   "ぱ" -> "",
//   "ぴ" -> "",
//   "ぷ" -> "",
//   "ぺ" -> "",
//   "ぽ" -> "",
//   "ま" -> "",
//   "み" -> "",
//   "む" -> "",
//   "め" -> "",
//   "も" -> "",
//   "や" -> "",
//   "ゆ" -> "",
//   "よ" -> "",
//   "ゃ" -> "",
//   "ゅ" -> "",
//   "ょ" -> "",
//   "ら" -> "",
//   "り" -> "",
//   "る" -> "",
//   "れ" -> "",
//   "ろ" -> "",
//   "わ" -> "",
//   "を" -> "",
//   "っ" -> "",
//   "ん" -> "",
//   "ー" -> "",
//   "ヴ" -> "",
//   "、" -> "",
//   "。" -> ""
// )
