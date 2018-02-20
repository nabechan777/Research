#リアクティブプログラミングのMVCへの適用に関する考察


##背景
 　近年インタラクティブなプログラミングに有効であるパラダイムであるリアクティブプログラミングが注目されている。リアクティブプログラミングとは、イベントベースであり、入力に応じて振る舞い、プログラムをデータフローとして見ることができるようにプログラムを定義するパラダイムである。リアクティブプログラミングは、あくまでも先ほど述べたような特徴をもつプログラムを定義しているだけであり、具体的にどのような手法を用いてリアクティブなプログラムを定義するのかまでは定義されていない。そこで具体的なリアクティブプログラミングの実現手法として、関数型リアクティブプログラミングがある。このパラダイムは、関数型プログラミングのルール（特に合成性）を用いてリアクティブプログラミングを実現する具体的な手法である。関数型リアクティブプログラミングは、1997年のConel Elliot, Paul Hudakによって発表された「Functional Reactive Animation」によって概念が定義され、2007年のConel Elliotによって発表された「Push-Pull Functional Reactive Programming」によって正式に定義された。
 　このパラダイムの概念を用いると、開発者が状態を管理する必要がなくなるという点があり、イベントベースのプログラムで用いられることのあるObserverパターンよりも優れ、Observerパターンに変わるものであるとブログや記事で紹介されている。
 　よって、本研究では、関数型リアクティブプログラミングが本当にObserverパターンよりも有用なイベントベースプログラムの実装方法であるのか調査する。ただし、Observerパターンが適用することのできる仕様をもつプログラム全てに対して調査することは不可能であるため、本研究では、MVCにおけるModelとViewのやり取りの実装に、Observerパターンではなく関数型リアクティブプログラミングを用いて、MVCにおいて関数型リアクティブプログラミングが有用であるのか調査する。
 　調査の方法は、関数型リアクティブプログラミングを実現するためのライブラリを用いて、実際のソフトウェア開発を行う。そのソフトウェア開発において、有用性を感じることができたか、またはできなかったかを具体的な点を交えて記述し、それを評価とする。また、議論では、MVCに限らずObserverパターンが用いられている部分に対して関数型リアクティブプログラミングが有用となり得るのか、本研究のソフトウェア開発を通して得た評価から推測しまとめ、それを記述する。

##Reactive Programming（RP）
 　リアクティブプログラミングとは、イベントベースであり、入力に応じた振る舞いをし、データフローとして見ることのできるプログラムを定義するパラダイムである。しかし、具体的にどのようにこれらの特徴を持つプログラムを定義するのかは定義されていない。
 　リアクティブプログラミングが適用されているプログラムの直感的な動作として身近なものは、スプレッドシートである。
 <!-- スプレッドシートの画像を貼る -->
 　例えば、Anに商品の定価、B1に消費税率、Cnに商品の税込みの価格を示す値（状態）を持っているとする。この時、CnはAnまたはB1のイベントに依存している。すなわち、AnまたはB1においてイベントが発生した場合、Cnの値はAnまたはB1の入力に応じて状態を自ら更新する。リアクティブプログラミングの適用されているプログラムは、あるイベントに依存している状態は、依存しているイベントが発生した際に、そのイベントに応じて自ら状態を最新の状態に更新する。

##Functional Reactive Programming（FRP）
 