# Language description


**flower** is a purely functional language with strong static typing.


## Data


**flower** provides following primitive data types


  * `Int`


        0 1 42 0x4f 0b0100 0o77


  * `Float`


        1.0 1.0e-19 0.001 6.022e+23


  * `Bool`


        true : Bool
        false : Bool


  * `Char`


        'a' 'x'


Additionally there are three type constructors given (all specified on every type - e.g. of kind `*`):


  * `Maybe A` which introduces possible lack of result


  * `Stream A` which specifies lazy data stream


  * `Pair A B` - a cartesian product or a 2-tuple


## Built-in functions:


  * `Int`


        sum : Int -> Int -> Int
        sub : Int -> Int -> Int
        mul : Int -> Int -> Int
        quot : Int -> Int -> Maybe Int
        mod : Int -> Int -> Maybe Int
        neg : Int -> Int
        eq : Int -> Int -> Bool
        leq : Int -> Int -> Bool
        float : Int -> Float


  * `Float`


        sumf : Float -> Float -> Float
        subf : Float -> Float -> Float
        mulf : Float -> Float -> Float
        divf : Float -> Float -> Maybe Float
        negf : Float -> Float
        eqf : Float -> Float -> Bool
        leqf : Float -> Float -> Bool
        floor : Float -> Int
        ceil : Float -> Int


  * `Bool`


        and : Bool -> Bool -> Bool
        not : Bool -> Bool


  * `Char`


        upper : Char -> Char
        lower : Char -> Char


Data constructors for composed types:


  * `Maybe A`


        for A : *
        some : A -> Maybe A

        for A : *
        none : Maybe A


  * `Stream A`


        for A : *
        cons : A -> Stream A -> Stream A

        for A : *
        nil : Stream A


  * `Pair A B`


        for A : *, B : *
        pair : A -> B -> Pair A B


And following selectors/modifiers:


  * `Maybe A`


        for A : *, B : *
        maybe : (A -> B) -> B -> Maybe A -> B


  * `Stream A`


        for A : *
        head : Stream A -> Maybe A

        for A : *
        tail : Stream A -> Maybe (Stream A)

        for A : *, B : *
        stream : (A -> Stream A -> B) -> B -> Stream A -> B


  * `Pair A B`


        for A : *, B : *
        fst : Pair A B -> A

        for A : *, B : *
        snd : Pair A B -> B


## Comments


    # End of line comment
    (# Delimited comment #)


# Syntactic sugar


  * Not much of that (at the time being), but the following are equal:


        cons 'A' (cons 'S' (cons 'D' nil))

        "ASD"


# Examples


## Function composition


    for A : *, B : *, C : *
    let compose : (B -> C) -> (A -> B) -> (A -> C) =
      fun g f x ->
        g (f x)
      end


## Fold


    for A : *, B : *, C : *
    let compose : (B -> C) -> (A -> B) -> (A -> C) =
      fun f g a -> f (g a) end


    for E : *, R : *
    let foldLeft : (R -> E -> R) -> R -> Stream E -> R =
      fun f acc ->
        stream
          (compose (foldLeft f) (f acc))
          acc
      end


Using the `loop` keyword (which refers to the inner most lambda):


    for E : *, R : *
    let foldLeft : (R -> E -> R) -> R -> Stream E -> R =
      fun f ->
        fun acc ->
          stream
            (compose loop (f acc))
            acc
        end
      end


## QuickSort


    # Examples of concat, gt or filter implementations
    # can be found in `stdlib.fl`
    let qSort : Stream Int -> Stream Int =
      stream
        fun h t ->
          concat
            (quickSort (filter (leq h) t))
            (cons h (quickSort (filter (gt h) t)))
        end
        nil
