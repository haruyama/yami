(load "./4.1.ss")

; 4
; しかし徐々に複雑な問題に立ち向うにつれ, Lispや他の固定したプログラム言語は目的に不十分なことが分ってくる. われわれの考えをより効果的に表すため, 絶えず新しい言語に変らなければならない.

;   新しい言語を創設する 超言語的抽象(metalinguistic abstraction) は, 工学的設計のすべての分野ãneocomplete_start_auto_complete)§重要な役割を果す. これはプログラミングにおいて, 新しい言語が形式化出来るだけでなく, 評価器を構築してこれらの言語を実装し得るが故に, 特に計算機のプログラミングで重要である. プログラム言語の 評価器(evaluator)(または解釈系(interpreter))とは, その言(neocomplete_start_auto_complete)語の式に作用させられると, その式を評価するのに必要な行動を実行する手続きのことである.
;   これをプログラミングの最も基本的な考えと見るのは決して誇張ではない:
; プログラム言語での式の意味を決定する評価器は, もう一つのプログラムに過ぎない.


; 4.1
;   超循環評価器は, 本質的に3.2節に述べた評価の環境モデルのSchemeによる形式化である. そのモデルには二つの基本部分があったことを思い出そう:

; 1. (特殊形式以外の合成式である)組合せを評価するには, 部分式を評価し, 演算子の部分式の値を, 被演算子の部分式の値に作用させる.

; 2. 合成手続きを一組の引数に作用させるには, 手続き本体を新しい環境で評価する. この環境を構成するには, 手続きオブジェクトの環境部分を, 手続きの仮パラメタが, 手続きを作用させる引数に束縛されるフレームで拡張する.


; 4.1.1
;  評価プロセスは二つの手続き: evalとapplyの間の相互作用として記述出来る.

(define the-global-environment (setup-environment))
;(eval '(define x 1) the-global-environment)
(eval-definition '(define x 1) the-global-environment)
(list-of-values
  '((begin (set! x (+ x 3)) (newline) (display x))
    (begin (set! x (* x 2)) (newline) (display x)))
  the-global-environment)

; 4.1.2
; 評価器は2.3.2節で論じた記号微分プログラムを思い起させる. どちらのプログラムも記号式を操作する. どちらのプログラムでも, 合成式への操作の結果は, 式の部分部分を再帰的に操作し, 式の型に依存した方法で結果を組み合せて得られる. どちらのプログラムでも式の表現に関する細部の情報から操作の一般的規則を分離するため, データ抽象を使った. 微分のプログラムでは, 同じ微分手続きが, 前置形, 中置形, また別の形であっても, 代数式を扱えるということであった. 評価器の場合は, 評価される言語の構文は, 式を分類し要素を取り出す手続きによってのみ, 決るということである. 

(self-evaluating? 1)
(self-evaluating? "aaa")

(self-evaluating? 'a)
(self-evaluating? '())


(quoted? 'a)
(quoted? ''a)
(text-of-quotation ''a)

(quoted? 1)


(assignment? '(set! x 'hoge))
(assignment? '(define x 'hoge))

(assignment-variable '(set! x 'hoge))
(assignment-value '(set! x 'hoge))


(definition? '(define x 'hoge))
(definition-variable '(define x 'hoge))
(definition-value '(define x 'hoge))
(definition-value '(define (add x y) (+ x y)))

(sequence->exp '() )
(sequence->exp '( (display x) )  )
(sequence->exp '( (display x) (display y) )  )

(cond->if '(cond ((> x 0) x)
                 ((= x 0)  (display 'zero) 0)
                 (else (- x)))
          )

; 4.1.3
; 式の外部構文の定義に加え, 評価器の実装にはプログラム実行の一部として, 手続きや環境の表現や, 真や偽の表現のような, 評価器が内部的に操作するデータ構造を定義しなければならない. 
(true? "a")
(true? 'a)
(true? '())
(true? true)

(true? false)
(true? #f)

(make-frame '() '())
(make-frame '(a) '(1))
(make-frame '(a b) '(1 2))

(make-frame '(a b) '(1 2))
 
(define f1 (make-frame '(a b) '(1 2)))
(add-binding-to-frame! 'c 3 f1)
f1

(define env1 the-empty-environment)
env1
(define env2 (extend-environment '(a b) '(1 2) env1))
env2

(define env3 (extend-environment '(c) '(3) env2))
env3

(first-frame env3)
(enclosing-environment env3)

(lookup-variable-value 'c env3)
(lookup-variable-value 'a env3)
(lookup-variable-value 'd env3)

(set-variable-value! 'c 5 env3)
env3
(set-variable-value! 'a 0 env3)
env3

(define-variable! 'c 10 env3)
env3
(define-variable! 'a 15 env3)
env3

;4.1.4
; 評価プログラムは式を簡約し, ついには基本手続きの作用にまで行く. 従って評価器を走らせるのに必要なものは, 基本手続きの作用をモデル化するために, 基盤になるLispシステムを呼び出す機構を作ることである.


(setup-environment)
(primitive-procedure-objects)

(define the-global-environment (setup-environment))

(driver-loop)

(define (append x y)
  (if (null? x)
      y
      (cons (car x)
            (append (cdr x) y))))

append

(append '(a b c) ' (d e f))

#q


;4.1.5
;  同様に評価器を機械の記述を入力としてとる, 非常に特別な機械として見ることが出来る. この入力が与えられると, 評価器は記述された機械をエミュレートするように自分を構成する. 例えば図4.3に示すように, 評価器にfactorialの定義を食べさせたとすると, 評価器は階乗が計算出来るようになる.

;  この観点から, われわれの評価器は万能機械(universal machine)と見てよい.

;  評価器のもう一つの衝撃的な点は, それがプログラム言語で操作されるデータオブジェクトと, プログラム言語自身の間の橋として働くことである.

;(eval '(* 5 5) the-global-environment)
;(eval (cons '* (list 5 5)) the-global-environment)


;4.1.6
; しかし(1.1.8節で説明し)ブロック構造を実装するのに使う内部定義について, 注意深く考えてみると, 名前毎の環境の拡張は, 局所変数を定義する最良の方法とは思えない.

; 二つのdefineの満足出来る唯一の解釈は, 名前even? とodd?が同時に環境に追加されたと見ることである.

; 実際われわれの解釈系は, 「偶然な」理由により, fの呼出しは正しく評価する. 内部手続きの定義が最初に来るので, それらのすべてが定義されるまでこれらの手続きの呼出しが評価されないからである.

; しかしここで内部で定義した名前が真に同時有効範囲を持つように定義を扱う単純な方法がある.

(define the-global-environment (setup-environment))

(driver-loop)
(define (f x)
  (define (even? n)
    (if (= n 0)
        true
        (odd? (- n 1))))
  (define (odd? n)
    (if (= n 0)
        false
        (even? (- n 1))))
  (even? x))

(f 10)
(f 9)

#q
