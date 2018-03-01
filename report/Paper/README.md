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
 　関数型リアクティブプログラミングとは、関数型プログラミングを用いて、リアクティブプログラミングを実現するパラダイムである。関数型プログラミングとは、数学的な意味の関数に基づいてプログラミングを行うパラダイムである。関数型リアクティブプログラミングにおいて、重要な2つのデータ型（`Event`と`Behavior`）と10個のプリミティブ操作がある。

 <br>

#### Event
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

 <br>

#### Behavior
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

 <br>

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

 <br>

#### FRPシステムのI/O
 　FRPシステムのI/Oは、MomentIOモナド内で行う。入力に関連する関数は、主に`fromAddHandler`と`fromChanges`であり、出力に関連する関数は、`reactimate`と`reactimate'`である。それぞれ詳細は後述する。なお、ここでのFRPシステムとは、Reactive Bananaを指しており、後述の説明が他のFRPシステムにおいて当てはまるとは限らない。

 <br>

#### FRPシステムへの入力
 　FRPシステムへの入力は、`Handler a`と`AddHandler a`を利用する。各データ型は、`Control.Event.Handler`において次のように定義されている。各データ型の生成は、`newAddHandler :: IO (AddHandler a, Handler a)`によって行う。

 ```Haskell

 -- FRPシステムの外で利用する。
 type Handler a = a -> IO ()

 -- FRPシステムの中で利用する。
 newtype AddHandler a = Handler a -> IO (IO())

 -- Handler aとAddHandler aの生成
 (addHandler, runHandler) <- newAddHandler

 ```

 `AddHandler a`は、EventまたはBehaviorを出力するために、`fromAddHandler :: AddHandler a -> MomentIO (Event a)`または`fromChanges :: a -> AddHandler a -> MomentIO (Behavior a)` の入力として使用される。`Handler a`は、FRPの外で用いられ、実行されるとFRP内にa型のデータが入力され、EventまたはBehaviorとして出力される。

 ```Haskell
 do
    (addHandler, runHandler) <- newAddHandler

    let networkDescription = do
        -- runHandlerが実行されるとEventが発火する
        e <- fromAddHandler addHandler
                ・
                ・
                ・
    ・
    ・
    ・
    forever $ fmap read getLine >>= runHandler
 ```

 <br>

#### FRPシステムからの出力
 　FRPシステムからの出力は、`reactimate :: Event (IO ()) -> MomentIO ()`または`reactimate' :: Event (Future (IO ())) -> MomentIO ()`で行う。ただし、2つのEventがほぼ同時に発火した場合、各Eventに適用された`reactimate`はインターリーブされる。`reactimate'`は、`Event (Future (IO ()))`を入力とし、`MomentIO ()`のアクションを実行する。`Future a`は、イベント処理が完了したのちに`MomentIO ()`のアクションを実行することを保証する。

 ```Haskell
 do
    (addHandler, runHandler) <- newAddHandler

    let networkDescription = do
        e <- fromAddHandler addHandler
        -- Event IntからEvent (IO ())に変換し、Event (IO ())が持つIO ()を実行する。
        reactimate $ (\x -> putStrLn $ replicate x "*") <$> e
    ・
    ・
    ・
    forever $ fmap read getLine >>= runHandler
 ```

 <br>

#### まとめ
 　標準入力から数字を入力し、数字の分だけ' * 'を出力するプログラムをReactive Bananaを用いて実装した。そのプログラムを下記に示す。

 ```Haskell
 module Main
    ( main
    ) where

 import Control.Monad
 import Reactive.Banana
 import Reactive.Banana.Frameworks

 main :: IO ()
 main = do
    (addHandler, runHandler) <- newAddHandler

    let networkDescription :: MomentIO ()
        networkDescription = do
            e <- fromAddHandler addHandler
            reactimate $ (\x -> putStrLn $ replicate x '*') <$> e

    -- MomentIO ()をEventNetworkにコンパイルする。
    network <- compile networkDescription

    -- EventNetworkを実行する。
    actuate network

    forever $ fmap read getLine >>= runHandler
 ```

 `compile :: MomentIO () -> IO EventNetwork`は、FRPを用いて定義したイベントロジックを`EventNetwork`にコンパイルする。`EventNetwork`は、コンパイルされたイベントロジックを示すデータ型であり、実行中または停止中の２つの状態を取りうる。`EventNetwork`を実行状態にするには、`actuate :: EventNetwork -> IO ()`を用いる。一方、停止状態にするには、`pause :: EventNetwork -> IO ()`を実行する。

 <br>

<a name="Specification"></a>
## 4.仕様
 　本研究で開発するソフトウェアの仕様は次のように定義する。

 <details><summary>学期情報の表示する。</summary><br>
 学期情報とは、現在行われている履修登録が前期に受ける講義の選択なのか後期に受ける講義の選択であるのか示す情報である。また、履修登録期間も表示する。これらの情報は全てテキストによって表示される。
 </details>

 <br>

 <details><summary>学生情報の表示する。</summary><br>
 学生情報とは、次の属性を持つ情報である。
 <ul>
    <li>学生証番号</li>
    <li>氏名</li>
    <li>セメスター</li>
    <li>学部</li>
    <li>学科</li>
 </ul>
 これらは全てテキストで表示される。
 </details>

 <br>

 <details><summary>講義選択を行う選択メニューの設置する。</summary><br>
 講義の情報は次のような属性を持つ情報である。
 <ul>
    <li>ID</li>
    <li>講義名</li>
    <li>分野</li>
    <li>開講期間</li>
    <li>単位</li>
 </ul>
 ただし、IDは講義において一意に決まる数値を示し、分野は共通または専門のどちらかを示す情報であり、開講期間は前期または後期の情報と何曜日の何時限目に行われる講義であるのかを示す情報である。また,月曜日から土曜日の一限目から五限目まで開講している大学を想定している。これらは、選択メニューのコンポーネントによって示される。
 </details>

 <br>

 <details><summary>現在の総取得単位数の表示する。</summary><br>
 現在の総取得単位数は、履修登録をしている学生の現在取得している単位の合計である。この情報は、テキストによって示される。<br>
 </details>

 <br>

 <details><summary>講義選択後の総単位数の表示する。</summary><br>
 講義選択後の総単位数は、現在の総取得単位数と選択した講義の単位全ての合計との和である。この情報はテキストによって示される。
 </details>


<a name="Implementation"></a>
## 5.実装
 次の環境の元で、定義した仕様を満たすソフトウェアを開発する。

### 開発環境

 <dl>
    <dt>プラットフォーム</dt>
    <dd><table>
        <tr>
            <th>マシン</th>
            <th>オペレーティングシステム</th>
            <th>プロセッサ</th>
            <th>メモリ</th>
        </tr>
        <tr>
            <td>Apple Macbook Air(11-inch, Mid2013)</td>
            <td>macOS-ver10.13.3</td>
            <td>1.3GHz-intel-Core-i5</td>
            <td>4GB-1600MHz-DDR3</td>
        </tr>
    </table></dd>

    <dt>DBMS</dt>
    <dd>PostgreSQL(ver10.1)</dd>

    <dt>言語</dt>
    <dd><table>
        <tr>
            <th>言語</th>
            <th>コンパイラ</th>
            <th>ビルドツール</th>
        </tr>
        <tr>
            <td><a href=https://www.haskell.org/>Haskell</a></td>
            <td>GHC(ver8.0.2)</td>
            <td><a href=https://docs.haskellstack.org/en/stable/README/>Haskell Stack(ver1.6.3)</a></td>
        </tr>
    </table></dd>

    <dt>パッケージ</dt>
    <dd>
        <ul>
            <li><a href=https://hackage.haskell.org/package/HDBC>HDBC(ver2.4.0.2)</a></li>
            <li><a href=https://hackage.haskell.org/package/relational-record-0.1.8.0>relational-record(ver0.1.8.0)</a></li>
            <li><a href=https://hackage.haskell.org/package/wx>wx(ver0.92.3.0)</a></li>
            <li><a href=https://hackage.haskell.org/package/reactive-banana>reactive-banana(ver1.1.0.1)</a></li>
            <li><a href=https://hackage.haskell.org/package/reactive-banana-wx>reactive-banana-wx(ver1.1.1.0)</a></li>
        </ul>
    </dd>
    <dd>ただし、これらのパッケージが依存しているパッケージについては記述を省略している。</dd>
 </dl>

#### 開発
 　先ほど定義した仕様を満たすソフトウェアの構造は、MVCアーキテクチャに従う。MVCアーキテクチャを用いるということは、ModelとViewのやり取りが発生する。既存の手法を用いる場合、Observerパターンを用いてModelとViewのやり取りを実装する。しかし、Observerパターンで実装した場合、各GUIコンポーネントが持つ値は開発者が管理し、それらの値を用いて仕様を満たす機能を実装する必要があり、解決すべき問題のみを考えて、機能を実装することはできない。この問題点を解決すべく、本ソフトウェア開発はModelとViewのやり取りをObserverパターンではなく、FRPを用いて実装する。各GUIのコンポーネントは[wxHaskell](https://wiki.haskell.org/WxHaskell)を用いて生成する。GUIの詳細な実装に関しては説明を省略する。<br>
 　選択メニューとボタンは各コンポーネントから発生したイベントに対しての振る舞いを定義する必要がある。イベントロジックは次の図のように定義する。

 ![イベントロジックのデータフロー](../resources/イベントロジックの概念図.png "イベントロジックの概念図")

 この図に用いられている図形ならびに矢印の意味は次のように定義する。

 - 雲型の図形 = GUIコンポーネント
 - 五角形 = FRP内の演算
 - 黒矢印 = Event
 - 白矢印 = Behavior


 　Choicesは30個のChoiceを示し、現在の共通または専門の総取得単位数を表示するコンポーネントをそれぞれCurrentCommonとCurrentSpecilizedとする。また、講義選択後の共通または専門の総単位数を表示するコンポーネントをAfterCommonとAfterSpecilizedとする。実装において、講義の情報を表すデータ型を`Lecture`とする。FRPシステム内において、Choicesのが持つ値を`Event [Maybe Lecture]`、CurrentCommonとCurrentSpecilizedが持つ値を`Behavior String`として表現する。<br>
 　実装したプログラムは、選択後の共通または専門の講義の総単位数をコンポーネントに表示し、登録ボタンが押された際には、データベースへの登録処理をおこなうプログラムである。`let`の内側に記述されているコードは`Event`または`Behavior`がもつ値を変換している。一方、`let`の外側はFRPシステム内における入出力を担う関数により、FRPシステムの外側のやり取りをしている。Choicesが持つ値は、`fromAddHandler :: AddHandler a -> MomentIO (Event a)`によって`Event [Maybe Lecture]`が出力される。一方、CurrentCommonとCurrentSpecilizedが持つ値は、`fromChanges`ではなく`fromPoll :: IO a -> MomentIO (Behavior a)`によって`Behavior String`が出力されている。<br>
 　`fromChanges :: a -> AddHandler a -> MomentIO (Behavior a)`は、次のように実装されている。

 ```Haskell
 fromChanges initialValue addHandler = do
    e <- fromAddHandler addHandler
    b <- stepper initialValue e
    return b
 ```

 このことから`fromChanges`は外界でのイベントをFRP内で時間とともに変化する値として扱うことを示している。CurrentCommonとCurrentSpecilizedにおいてイベントは発生しない。よって、IOアクションの出力の値をもつBehaviorを出力する`fromPoll`を用いている。<br>
 　講義選択のイベントが発生してから講義選択後の各分野の総単位数の表示のプロセスは次のようになる。

 1. Choicesにおいてイベントが発生する。
 2. Choicesが持つ値がFRPシステム内に入力され、その値を持つEventが出力される。
 3. 2で出力されたEventを用いて、選択されている講義の情報のみを値として持つEventを出力する。
 4. 各分野の現在の総取得単位数の値を持つBehaviorに対して、Behaviorが持つ値に`(+) :: (Num a) => a -> a -> a`を適用し、これを値としてもつBehaviorを出力する。
 5. 3で出力されたEventについて、各分野（共通, 専門）ごとに選択している講義の単位の合計を値として持つEventを出力する。
 6. 4と5において出力されたBehaviorと各Eventについて、3で出力したEventに対して、2で出力したBehaviorがもつ値（`a -> a`型の関数）を適用し、その結果を値としてもつEventを出力する。
 7. 6で出力された各Eventについて、このEventが持つ値をAfterCommonまたはAfterSpecilizedの`text`属性に反映させるアクションをもつEventに変換する。
 8. 7で出力された各Eventに対して、`reactimate :: Event (IO ()) -> MomentIO ()`を適用する。


 　プログラムは次のようになる。

 ```Haskell
 networkDescription :: MomentIO ()
 networkDescription = mdo
    -- Notingを含むLectureのリストを値を持つEventをechoiceとする。
    echoice     <- fromAddHandler choiceAddHandler
    -- 現在の共通の総取得単位数を示す文字列の値を持つBehaviorをbcommonとする。
    bcommon     <- fromPoll $ get beforeCommon text
    -- 現在の専門の総取得単位数を示す文字列の値を持つBehaviorをbspecilizedとする。
    bspecilized <- fromPoll $ get beforeSpecilized text

    let -- Lectureのリストを値を持つEventをechoices'とする。
        echoice' = fmap catMaybes echoice
        -- 現在の共通の総取得単位数と選択している共通の講義の単位の合計の和を持つEventをeres1とする。
        eres1    = (+) <$> fmap read bcommon     <@> fmap sumOfCommonLectureCredit echoices'
        -- 現在の専門の総取得単位数と選択している専門の後尾の単位の合計の和を持つEventをeres2とする。
        eres2    = (+) <$> fmap read bspecilized <@> fmap sumOfSpecilizedLectureCredit echoices'
    ・
    ・
    ・
    -- eres1が持つ値を選択後の共通の講義の総単位数を表示するコンポーネントに反映する。
    reactimate $ changeCommonCreditText     <$> eres1
    -- eres2が持つ値を選択後の専門の講義の総単位数を表示するコンポーネントに反映する。
    reactimate $ changeSpecilizedCreditText <$> eres2
 ```


 　登録ボタンのイベントが発生してからデータベースへの登録処理の実行のプロセスは次のようになる。

 1. Buttonにおいてイベントが発生する。
 2. FRPシステムに()が入力され、それを値として持つEventを出力する。
 3. 選択した講義の情報のリストを値としてもつEventに、`stepper :: a -> Event a -> MomentIO (Behavior a)`を適用しBehaviorを出力する。
 4. `const :: a -> b -> a`を持ち上げ、2で出力されたBehaviorに適用した値をもつBehaviorを出力する。
 5. 2と4で出力されたEventとBehaviorに、`apply :: Behavior (a -> b) -> Event a -> Event b`を適用し、選択された講義のリストを値として持つEventを出力する。
 6. `Student -> [Lecture] -> IO ()`型の関数を持ち上げ、5で出力されたEventがもつ値に適用し、その結果の値を持つEventを出力する。(Student = 学生の情報)
 7. 6で出力されたEventに、`reactimate :: Event (IO ()) -> MomentIO ()`を適用する。


 プログラムは次のようになる。

 ```Haskell
 networkDescription :: MomentIO ()
 networkDescription = mdo
    ・
    ・
    ・
    -- 登録ボタンのイベントによって発火するEvent
    ebutton <- fromAddHandler buttonAddHandler
    -- echoice'にstepper []を適用し、Behaviorを出力する。（RecusiveDo言語拡張より、前方参照が可能）
    bchoice <- stepper [] echoice'

    let echoice' = fmap catMaybes echoice
        .
        .
        .
    .
    .
    .
    -- ebuttonが発火した場合のみ、bchoiceが持つ値をデータベースに登録する。
    reactimate $ (\x -> registrationAction student x) <$> (bchoice <@ ebutton)
 ```

<a name="Evaluation"></a>
## 6.評価
 　本研究において、定義した仕様を満たすソフトウェア開発において、ModelとViewのやり取りをObserverパターンではなく、FRPを用いて実装した際の評価できる点として、次のような点をあげることができる。

 - イベントを発生させるコンポーネントがもつ値を開発者が管理する必要がなく、FRPシステムに任せることができる。
 - 解決すべき問題に集中して開発に取り組むことができる。
 - 入力に応じた振る舞いが不可分に行われるため、同時イベントを簡単に実装することができる。
 - 入力と出力を担うインターフェースが定義されているので、イベントロジックと入出力を明確に分離することができる。


 ただし、これらの評価は、本研究で定義した仕様を満たすソフトウェアの開発における評価である。つまり、さらにインタラクティブ性の高い仕様を満たすようなソフトウェア（ゲームなど）の開発においても、これらの評価が当てはまるとは限らない。これらの評価から、MVCにおいてFRPを適用することは有用であると言える。

<a name="Summary"></a>
## 7.まとめ
 　本研究では、関数型リアクティブプログラミングがMVCにおいて適用することでどのような有用性があるのか実際にFRPシステムを用いたソフトウェア開発を通して調査した。その結果、ModelとViewのやり取りの実装でFRPライブラリを用いることで、GUIコンポーネントがもつ値の管理をFRPシステムに任せることができる点などFRPがMVCに適用することにおいて有用性があることがわかった。これらの有用性を示す点から、FRPをMVCに適用することは有用であると言える。

<a name="Reference">
## 8.参考文献
 [1] Conel Elliot, Paul Huduk (1997)『[Functional Reactive Animation](http://conal.net/papers/icfp97/)』<br>
 [2] Conel Elliot (2009) 『[Push-Pull Functional Reactive Programming](http://conal.net/papers/push-pull-frp/)』<br>
 [3] Stephen Blackheath, Anthony Jones (2017)『[関数型リアクティブプログラミング](https://www.google.co.jp/url?sa=t&rct=j&q=&esrc=s&source=web&cd=1&ved=0ahUKEwjDlOPEurzZAhUMiLwKHfw6A0oQFggoMAA&url=https%3A%2F%2Fwww.amazon.co.jp%2F%25E9%2596%25A2%25E6%2595%25B0%25E5%259E%258B%25E3%2583%25AA%25E3%2582%25A2%25E3%2582%25AF%25E3%2583%2586%25E3%2582%25A3%25E3%2583%2596%25E3%2583%2597%25E3%2583%25AD%25E3%2582%25B0%25E3%2583%25A9%25E3%2583%259F%25E3%2583%25B3%25E3%2582%25B0-Programmers-SELECTION-Stephen-Blackheath%2Fdp%2F4798145564&usg=AOvVaw1uniYw4x4MOvWn2CxE1Hnp)』<br>
