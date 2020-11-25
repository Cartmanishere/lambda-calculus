(ns lambda-calculus.core)

;; In the beginning, there was just a function...

;; ──────────────── Lambda Calculus from the Ground Up ────────────────

;; A switch has 2 inputs and a single output. A switch chooses either one of the
;; two inputs.

(defn LEFT
  [a]
  (fn [b]
    a))

(defn RIGHT
  [a]
  (fn [b]
    b))


((LEFT "5v") "gnd")
;; => 5v
((RIGHT "5v") "gnd")
;; => gnd

;; The LEFT function chooses the left value between a and b
;; RIGHT function chooses the right value between a and b
;; LEFT and RIGHT are modeling a behavior using just fucntions.

;; CURRYING: Taking multiple arugment function and turning into single
;; argument function.


;; Can you represent the concept of a TRUTH or FALSE using just functions?

(defn TRUE
  [x]
  (fn [y]
    x))

(defn FALSE
  [x]
  (fn [y]
    y))

;; We can say that if we pass two things, then the one that returns
;; the LEFT value is the TRUE behavior.  FALSE is by definition
;; opposite of TRUE behavior. So anything that returns the RIGHT value
;; is the FALSE behavior.

((TRUE "5v") "gnd")
;; => 5v
((FALSE "5v") "gnd")
;; => gnd




;; NOT is a behavior which turns TRUE to FALSE and FALSE to TRUE.
;; NOT(TRUE) = FALSE
;; NOT(FALSE) = TRUE

(defn NOT
  [x]
  ((x FALSE) TRUE))

;; If x is TRUE, then we pick the left thing. Here, so we make the
;; left thing passed to x as FALSE.  And vice versa, we pass the TRUE
;; as the right thing. So if x is false, then we TRUE in return.

(NOT TRUE)
;; => #function[lambda-calculus.core/FALSE]
(NOT FALSE)
;; => #function[lambda-calculus.core/TRUE]

;; Thus we've modeled the behavior of a NOT using just functions.

;; What about other operators like AND and OR?

(defn AND
  [x]
  (fn [y]
    ((x y) x)))

;; The behavior of AND is that if the 1st (left) value is TRUE, then
;; you go check the 2nd (right) value.  If the 1st value is FALSE then
;; AND is also FALSE.

((AND FALSE) FALSE)
;; => #function[lambda-calculus.core/FALSE]
((AND TRUE) FALSE)
;; => #function[lambda-calculus.core/FALSE]
((AND FALSE) TRUE)
;; => #function[lambda-calculus.core/FALSE]
((AND TRUE) TRUE)
;; => #function[lambda-calculus.core/TRUE]

;; Similarly,

(defn OR
  [x]
  (fn [y]
    ((x x) y)))

((OR FALSE) FALSE)
;; => #function[lambda-calculus.core/FALSE]
((OR TRUE) FALSE)
;; => #function[lambda-calculus.core/TRUE]
((OR FALSE) TRUE)
;; => #function[lambda-calculus.core/TRUE]
((OR TRUE) TRUE)
;; => #function[lambda-calculus.core/TRUE]



;; How can we represent numbers using just functions?
;; What is a number?

;; A number in its most primitive form is a count. A number just
;; counts a certain thing.

(def ONE (fn [f] (fn [x] (f x))))

;; We can define a number as the _number_ of the times a function is
;; called with x.  For TWO, we would do something like --

(def TWO (fn [f] (fn [x] (f (f x)))))

;; Similarly,

(def THREE (fn [f] (fn [x] (f (f (f x))))))
(def FOUR (fn [f] (fn [x] (f (f (f (f x)))))))

;; and so on...

;; The numbers here are representing a behavior. For e.g behavior of
;; three is calling a function 3 times.  A way to demonstrate this
;; behavior from outside of just functions, we can do --
(defn show
  [n]
  ((n inc) 0))

;; NOTE: We are just using `inc` as a way to demonstrate the behavior
;; of ONE. You can pass any other function behavior of ONE is to call
;; is one time.

;; One way to imagine this is like a crank. ONE turns the crank once,
;; TWO twice, THREE thrice and so on..

(show ONE)
;; => 1
(show TWO)
;; => 2
(show THREE)
;; => 3
(show FOUR)
;; => 4

;; How to implement the zero?
;; Behavior of a zero is that a passed function is called zero times.
(def ZERO (fn [f] (fn [x] x)))
(show ZERO)
;; => 0

;; What do you think will happen is we pass a number to another number?
(show (FOUR THREE))
;; => ???

;; Now there is a problem with this method of representing numbers.

;; How do you get more numbers? It is impractical to define the
;; numbers like this or we would have to define every number with that
;; many function calls.

;; What is the most minimal thing that you need to define a mathematical system?
;; PEANO AXIOMS:
;; - 0 is a number.
;; - If n is a number, then the successor of n is a number.
;; - 0 is not the successor of any number.
;; - succ(n1) == succ(n2) iff n1 == n2.
;; - If S is a set that contains 0 and the successor of every number
;;   in S, then S is the set of all numbers.

;; So to define the set of all possible numbers, we need a system that
;; satisfies these axioms. We already have a concept of _number_ in
;; our system, now we need concept of SUCCESSOR

;; What is the behavior of SUCC?
;; SUCC(TWO) => THREE

;; A number is just cranking the shaft that many times. To get the
;; next number, we need to call a function one more time than the
;; number we want to increment.

(def SUCC (fn [n] (fn [f] (fn [x] (f ((n f) x))))))
;; ((n f) x) is our original number. So we call `f` one more time.

(show (SUCC ZERO))
;; => 1
(show (SUCC ONE))
;; => 2
(show (SUCC TWO))
;; => 3


;; Arithmetic:
;; How do we define Addition and Multiplication?

(def ADD (fn [x] (fn [y] ((y SUCC) x))))

;; Behavior of addition is repeatedly doing successor from x -- y times.

(show ((ADD THREE) FOUR))
;; => 7

;; We can also implement multiplication as follows:
(def MUL (fn [x] (fn [y] (fn [f] (y (x f))))))

;; What we are doing is repeating the cranking of `f` x times -- y times.

(show ((MUL THREE) FOUR))
;; => 12


;; Data:
;; Can we store data using just functions?
;; E.g
;; (cons 2 3) --> (2, 3)
;; (car p) --> 2
;; (cdr p) --> 3

;; If you think about the behavior of a pair/tuple, you can write something
;; like this which also emulates that behavior.
(defn cons
  [a b]
  (fn [m]
    (if (= 0 m)
      a
      b)))

(defn car
  [p]
  (p 0))

(defn cdr
  [p]
  (p 1))

(car (cons 2 3))
;; => 2
(cdr (cons 2 3))
;; => 3

;; So technically, we are creating a pair using the `cons` function as
;; defined above because the resultant thing has the same behavior as
;; a tuple when we call `car` or `cdr` functions.

;; So we _can_ write a data structure using just some function definitions.

(def CONS (fn [a] (fn [b] (fn [s] ((s a) b)))))

(((CONS 2) 3) TRUE)
;; => 2
(((CONS 2) 3) FALSE)
;; => 3

;; So we also have the same data structure type behavior using just lambdas.
;; We can also write CAR and CDR functions in this way.

(def CAR (fn [p] (p TRUE)))
(def CDR (fn [p] (p FALSE)))

(CAR ((CONS 2) 3))
;; => 2
(CDR ((CONS 2) 3))
;; => 3

;; Once you have a way to define the behavior of a pair, you can go
;; beyond just a tuple.
(def p ((CONS 2) ((CONS 3) 4)))


;; We saw the SUCC function from going from TWO -> THREE
;; How do we get the previous number? i.e THREE -> TWO

;; One way to think about this is --
;; (0, 0)
;; (1, 0)
;; (2, 1)
;; (3, 2)
;;
;; So we build pairs of a number with keeping track of the previous number.
;; For e.g
(defn t
  [p]
  [(+ 1 (first p))
   (first p)])

((THREE t) [0, 0])
;; => [3 2]

(def T (fn [p] ((CONS (SUCC (CAR p))) (CAR p))))
;; This is the same as what we did in Clojure.

(def _ ((FOUR T) ((CONS ZERO) ZERO)))
;; This will return as the 4th pair.
(show (CAR _))
;; => 4
(show (CDR _))
;; => 3

;; Now we can use the above T function to write a PRED function which
;; gives use the predecessor for any _number_ in our system.
;; THREE -> TWO

(def PRED (fn [n] (CDR ((n T) ((CONS ZERO) ZERO)))))

(show (PRED TWO))
;; => 1
(show (PRED THREE))
;; => 2
(show (PRED (FOUR THREE)))
;; => 80


;; Now this allows us to do substraction as follows --
(def SUB (fn [x] (fn [y] ((y PRED) x))))

;; Apply PRED y times to x. This is analogous to how we implemented
;; ADD function using SUCC.

(show ((SUB FOUR) TWO))
;; => 2


;; Next thing we want to do, try to test if a number is zero. If we
;; are able to add a test for a number, this puts us on a path to
;; implement primitive control flow.

;; The behavior of number ZERO in our system is that it does not call
;; the given function `f` zero times.
;; So we want an implementation such that if `f` gets called, we get FALSE.
;; else we get TRUE.

(def ISZERO (fn [n] ((n (fn [_] FALSE)) TRUE)))

(ISZERO ZERO)
;; => #function[lambda-calculus.core/TRUE]
(ISZERO TWO)
;; => #function[lambda-calculus.core/FALSE]


;; To recap, we have implemented the following things in our system.
;; AND
;; OR
;; NOT
;; SUCC
;; PRED
;; ADD
;; MUL
;; SUB
;; CONS
;; CAR
;; CDR
;; ISZERO
;; This is like a instruction set analogous to an "assembly code".

;; Can we do more interesting things with this instrution set?
;; Can we write a recursive function using these building blocks?

;; This is how you would generally implement the Factorial function in
;; any programming language.

(defn fact
  [n]
  (if (= 0 n)
    1
    (* n (fact (dec n)))))

(fact 10)
;; => 3628800

;; Let's try this using the lambda functions.

(def FACT (fn [n] (((ISZERO n)
                   ONE)
                  ((MUL n) (FACT (PRED n))))))

;; This is the same behavior that we had in the `fact` function.

(FACT THREE)

;; Now if you try running the above function, you will get `java.lang.StackOverflowError`.
;; Why?

;; The reason we are getting the error is because after the ISZERO check, the two branches
;; are arguments to the returned functions.
;; In clojure, the argument would get resolved first before passing into the function.
;; So in our ISZERO check, both the branches are being evaluated which blows up in
;; `java.lang.StackOverflowError`.

(+ (do (println "First arg") 1)
   (do (println "Second arg") 2))

;; If you run the above code, you will see two print statements.
;; That's exactly what's happening in our FACT function.

;; The behavior that we do want is `n` is `ISZERO`, then we shouldn't
;; evaluate the 2nd arg i.e 2nd branch of the function. This is purely
;; a restriction based on how all programming languages work that do
;; not evaluate things lazily.

;; How do we get around this?
;; We can wrap it in a lambda and only evaluate if that particular branch
;; is being considered.
(defn choose
  [pred a b]
  (if pred
    (a)
    (b)))

(choose true
        #(do (println "First arg") 1)
        #(do (println "Second arg") 2))

;; We only see one print statement because only the required arg is resolved.
;; And not at the time of passing into the function.

;; Using this hack, we can write
(def LAZY_TRUE (fn [x] (fn [y] (x))))
(def LAZY_FALSE (fn [x] (fn [y] (y))))
(def LAZY_ISZERO (fn [n] ((n (fn [_] LAZY_FALSE)) LAZY_TRUE)))

(def FACT (fn [n] (((LAZY_ISZERO n)
                   (fn [] ONE))
                  (fn [] ((MUL n) (FACT (PRED n)))))))

(show (FACT THREE))
;; => 6
(show (FACT ((ADD THREE) TWO)))
;; => 120


;; In our implementation of a recursive FACT, we called the same function
;; FACT. But in order for us to do that, we needed a reference to the
;; original FACT function to itself.
;; BUT
;; In lambda calculus, there are no variables. So our implementation
;; violates that rule.
;; 1. There are no variables.
;; 2. No globals.
;; 3. No assignment.
;; 4. Just single argument functions.

;; For e.g
(def fact (fn [n] (if (= 0 n) 1 (* n (fact (- n 1))))))
;;                                   ^^^^
;; How can we write the above function with a self reference to `fact`?

(def fact ((fn [f] (fn [n] (if (= 0 n) 1 (* n (f (- n 1)))))) fact))

;; This is the same as doing --
(def x 2)
(def x ((fn [y] y) 2))
;; This is the same kind of trick we did with `fact`.

;; Buuuuut.. we still have that `fact` reference in our implementation.
;; We haven't gotten rid of it yet.
;; How about this?
(def fact ((fn [f] (fn [n] (if (= 0 n) 1 (* n (f (- n 1))))))
           (fn [f] (fn [n] (if (= 0 n) 1 (* n (f (- n 1))))))))

;; We repeat the whole function as and pass it as itself. Since that's what
;; we're defining as `fact`.

;; If you try to run the above `fact` function, it still doesn't work. Why?
;; Hint: (* n (f (- n 1)))
;;            #^^^^^^^^

;; We passed the whole function wrapped inside a lambda to itself.
;; So whenever it gets called, it expects the first arg to be `f` and
;; second arg to be `n`. That's the reason this doesn't work.

(def fact ((fn [f] (fn [n] (if (= 0 n) 1 (* n ((f f) (- n 1))))))
           (fn [f] (fn [n] (if (= 0 n) 1 (* n ((f f) (- n 1))))))))

(fact 4)
;; => 24

;; That actually works! :mind-blown:

;; So we are able to do a recursive function without actually having
;; a self reference.
(((fn [f] (fn [n] (if (= 0 n) 1 (* n ((f f) (- n 1))))))
  (fn [f] (fn [n] (if (= 0 n) 1 (* n ((f f) (- n 1))))))) 4)
;; => 24

;; Let's do the same thing to our lambda version of FACT function.
(def FACT ((fn [f] (fn [n] (((LAZY_ISZERO n)
                           (fn [] ONE))
                          (fn [] ((MUL n) ((f f) (PRED n)))))))
           (fn [f] (fn [n] (((LAZY_ISZERO n)
                           (fn [] ONE))
                          (fn [] ((MUL n) ((f f) (PRED n)))))))))

(show (FACT FOUR))
;; => 24

;; Now, technically. we have achieved recursion in our system. But we
;; have are having to repeat our entire function for the sake of not
;; having a global reference to it (which is not allowed in lambda
;; calculus anyway).

;; Let's park it at that and talk about something else.

;; What is a fixed point?
;; For any value x, if x = f(x), then x is known as a fixed point of f.
;; For e.g sqrt(1) = 1. So 1 is a fixed point of sqrt function.

;; But what does a fixed point has to do with recursion?

;; We had defined FACT as --
(def fact ((fn [f] (fn [n] (if (= 0 n) 1 (* n (f (- n 1)))))) fact))
;; We can take out and separate the first function, so this becomes --
;; NOTE: This would work with the lambda version as well.
(def R (fn [f] (fn [n] (if (= 0 n) 1 (* n (f (- n 1)))))))
(def nfact (R nfact))
(nfact 24)

;; What's happening here is that we're passing `nfact` to `R` and then
;; getting `nfact` back.
;; So `nfact` must be a fixed point of `R`.

;; But unfortunately, above code does not work.
;; Because we're passing fact = (R fact) when we don't even have
;; a definition for `nfact` yet. So this will reuslt in an error.

;; So we don't know what `nfact` is, we only have a definition of R.
;; But we know that if we can find out `nfact`, then nfact = (R nfact)
;; i.e it would be a fixed point of R.

;;
;; ────────────────────────────── LEAP! ─────────────────────────────
;;
;; Suppose there's a function Y that computes the fixed point of R.
;;
;; Y(R) --> Fixed point of R.
;;
;; If such a function exists, then we can say that --
;; Y(R) = R(Y(R))
;;
;; In clojure form: (Y R) = (R (Y R))
;; We can rewrite this as we've done with `fact` --
;; (Y R) = ((fn [x] (R x)) (Y R)
;;
;; But we can't refer to the Y on the left side,
;; just like we didn't refer `fact`. Instead we repeated
;; the whole function as passed it as an argument.
;;
;; (Y R) = ((fn [x] (R x)) (fn [x] (R x)))
;; We repeated ourself, but for this work to work, we need another change.
;;
;; (Y R) = ((fn [x] (R (x x))) (fn [x] (R (x x))))
;; This is the same when we did `(f f)` in the `fact` function.
;;
;; Let's do one more thing, we can even abstract the R, part of the above
;; equation by wrapping it in another lambda.
;;
;; (Y R) = ((fn [f] ((fn [x] (f (x x)))
;;                  (fn [x] (f (x x)))))
;;          R)
;; Now, we can just drop the R,

(def Y (fn [f] ((fn [x] (f (x x)))
               (fn [x] (f (x x))))))

;; What we have dervied now, the Y is known as the Y Combinator.
;; This is a device to do recursion in functional world without
;; having a self reference (and without having to repeat yourself).

;; To use it, you just define a function R.
(def R (fn [f] (fn [n] (if (= 0 n) 1 (* n (f (- n 1)))))))

(def fact (Y R))

;; So you pass it a function R and it returns you a fixed point of R.
;; And you can call this fixed point to get a recursive calling function.

;; Unfortunately, if you run above code, you will run into our friend `StackOverflowError`.
;; So this works in theory, but not in Clojure :(

;; The reason is the same as when we were implementing `FACT` function.
;; So this is a more of a language restriction than a problem with the core
;; concept.

;; So we can do the same thing we did at the time of `FACT` and wrap
;; the things in a lambda so that it evaluates lazily.

(def Y (fn [f] ((fn [x] (f (fn [z] ((x x) z))))
               (fn [x] (f (fn [z] ((x x) z)))))))

(def fact (Y R))

(fact 4)
;; => 24

;; And that works!! But how?!!!!

;; Just to prove that this allows us to do generic recursion..

(def R1 (fn [f] (fn [n] (if (<= n 2) 1 (+ (f (- n 1)) (f (- n 2)))))))
;; Yes, this is the recursive Fibonaaci series.

(def fib (Y R1))

(fib 10)
;; => 55

;; If you ask me how it works, I am going to say :MAGIC:.
;; Goodbye.
