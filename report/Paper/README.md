# リアクティブプログラミングのMVCへの適用に関する考察

## 目次
 1. [背景](#Background)
 2. [Reactive Programming](#RP)
 3. [Functional Reactive Programming](#FRP)
 4. [仕様](#Specification)
 5. [実装](#Implementation)
 6. [評価](#Evaluation)
 7. [まとめ](#Summary)
 8. [参考文献](#Reference)

<a name="Background"></a>
## 1. 背景
 　近年インタラクティブなプログラミングに有効であるパラダイムであるリアクティブプログラミングが注目されている。リアクティブプログラミングとは、イベントベースであり、入力に応じて振る舞い、プログラムをデータフローとして見ることができるようにプログラムを定義するパラダイムである。リアクティブプログラミングは、あくまでも先ほど述べたような特徴をもつプログラムを定義しているだけであり、具体的にどのような手法を用いてリアクティブなプログラムを定義するのかまでは定義されていない。そこで具体的なリアクティブプログラミングの実現手法として、関数型リアクティブプログラミングがある。このパラダイムは、関数型プログラミングのルール（特に合成性）を用いてリアクティブプログラミングを実現する具体的な手法である。関数型リアクティブプログラミングは、1997年のConel Elliot, Paul Hudakによって発表された「Functional Reactive Animation」によって概念が定義され、2007年のConel Elliotによって発表された「Push-Pull Functional Reactive Programming」によって正式に定義された。  
 　このパラダイムの概念を用いると、開発者が状態を管理する必要がなくなるという点があり、イベントベースのプログラムで用いられることのあるObserverパターンよりも優れ、Observerパターンに変わるものであるとブログや記事で紹介されている。  
 　よって、本研究では、関数型リアクティブプログラミングが本当にObserverパターンよりも有用なイベントベースプログラムの実装方法であるのか調査する。ただし、Observerパターンが適用することのできる仕様をもつプログラム全てに対して調査することは不可能であるため、本研究では、MVCにおけるModelとViewのやり取りの実装に、Observerパターンではなく関数型リアクティブプログラミングを用いて、MVCにおいて関数型リアクティブプログラミングが有用であるのか調査する。  
 　調査の方法は、関数型リアクティブプログラミングを実現するためのライブラリを用いて、実際のソフトウェア開発を行う。そのソフトウェア開発において、有用性を感じることができたか、またはできなかったかを具体的な点を交えて記述し、それを評価とする。また、議論では、MVCに限らずObserverパターンが用いられている部分に対して関数型リアクティブプログラミングが有用となり得るのか、本研究のソフトウェア開発を通して得た評価から推測しまとめ、それを記述する。

<a name="RP"></a>
## 2. Reactive Programming（RP）
 　リアクティブプログラミングとは、イベントベースであり、入力に応じた振る舞いをし、データフローとして見ることのできるプログラムを定義するパラダイムである。しかし、具体的にどのようにこれらの特徴を持つプログラムを定義するのかは定義されていない。  
 　リアクティブプログラミングが適用されているプログラムの直感的な動作として身近なものは、スプレッドシートである。

 ![スプレッドシート](../resources/リアクティブプログラミングの例（スプレッドシート）.png "リアクティブプログラミングの例")

 　例えば、Anに商品の定価、B1に消費税率、Cnに商品の税込みの価格を示す値（状態）を持っているとする。この時、CnはAnまたはB1のイベントに依存している。すなわち、AnまたはB1においてイベントが発生した場合、Cnの値はAnまたはB1の入力に応じて状態を自ら更新する。リアクティブプログラミングの適用されているプログラムは、あるイベントに依存している状態は、依存しているイベントが発生した際に、そのイベントに応じて自ら状態を最新の状態に更新する。

<a name="FRP"></a>
## 3. Functional Reactive Programming（FRP）
 　関数型リアクティブプログラミングとは、関数型プログラミングを用いて、リアクティブプログラミングを実現するパラダイムである。関数型プログラミングとは、数学的な意味の関数に基づいてプログラミングを行うパラダイムである。関数型リアクティブプログラミングにおいて、重要な2つのデータ型と10個のプリミティブ操作がある。

#### データ型

##### Event
 　Eventは、一連のイベントを示すデータ型である。Eventが値（オカレンス）を持つのはイベントが発生した時のみである。Reactive Bananaでは次のように考えることができる。

 ``` haskell
 type Event a = [(Time, a)]

 ```

 またEventは、Functorのインスタンスである。

 ``` haskell
 instance Functor Event where
    fmap :: (a -> b) -> Event a -> Event b
    fmap f e = [(time, f a) | (time, a) <- e]
 ```

##### Behavior
 　Behaviorは、時間とともに変化する値を示すデータ型である。Behaviorは、常に値を持つ。Reactive Bananaでは次のように考えることができる。

 ``` Haskell
 type Behavior a = Time -> a
 ```

 またBehaviorは、Applicativeのインスタンスである。

 ``` Haskell
 instance Functor Behavior where
    fmap :: (a -> b) -> Behavior a -> Behavior b
    fmap f b = \time -> f (b time)

 instance Applicative Behavior where
    pure :: a -> Behavior a
    pure x = \time -> x

    (<*>) :: Behavior (a -> b) -> Behavior a -> Behavior b
    fx <*> bx = \time -> fx time $ bx time
 ```


#### プリミティブ操作

 ``` Haskell
 -- EventまたはBehaviorを変換する操作
 fmap :: (Functor f) => (a -> b) -> f a -> f b

 -- Eventの値をBehaviorに保存する操作
 stepper :: a -> Event a -> Behavior a

 -- 定数値を持つBehaviorを出力する操作
 pure :: (Applicative f) => a -> f a

 -- Eventをマージする操作
 union :: [Event (a -> a)] -> Event a

 -- Eventに応じてBehaviorの値を取得する操作
 (<@>) :: Behavior (a -> b) -> Event a -> Event b
 (<@)  :: Behavior b -> Event a -> Behavior b

 -- Eventを随時伝播させる操作
 filterE :: (a -> Bool) -> Event a -> Event a

 -- Behaviorを結合する操作
 (<*>) :: Behavior (a -> b) -> Behavior a -> Behavior b

 -- 決して発火しないEventを出力する操作
 never :: Event a

 -- Behaviorの値を取得する操作
 valueB      :: (MonadMoment m) => Behavior a -> m a
 valueBLater :: (MonadMoment m) => Behavior a -> m a

 -- 新たなデータフローを出力する操作  
 switchE :: (MonadMoment m) => Event (Event a) -> m (Event a)
 switchB :: (MonadMoment m) => Behavior a -> Event (Behavior a) -> m (Behavior a)
 ```

<a name="Specification"></a>
## 4.仕様
 　本研究で開発するソフトウェアの仕様は次のように定義する。

 <details><summary>学期情報の表示する。</summary>学期情報とは、現在行われている履修登録が前期に受ける講義の選択なのか後期に受ける講義の選択であるのか示す情報である。また、履修登録期間も表示する。これらの情報は全てテキストによって表示される。</details>

 <details><summary>学生情報の表示する。</summary><div>

 学生情報とは、次の属性を持つ情報である。
 - 学生証番号
 - 氏名
 - セメスター
 - 学部
 - 学科
 これらは全てテキストで表示される。

 </div></details>

 <details><summary>講義選択を行う選択メニューの設置する。</summary></details>

 <details><summary>現在の総取得単位数の表示する。</summary></details>

 <details><summary>講義選択後の総単位数の表示する。</summary></details>


<a name="Implementation"></a>
## 5.実装

<a name="Evaluation"></a>
## 6.評価

<a name="Summary"></a>
## 7.まとめ

<a name="Reference">
## 8.参考文献
 [1] [id]: url "title"
 [2]
