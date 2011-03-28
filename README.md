# Opis języka


**flower** jest językiem czysto funkcyjnym z silnym, jawnym, dynamicznym typowaniem.


## Dane


**flower** udostępnia następujące typy podstawowe:


  * `Int` liczba całkowita, przykłady:


        0 -1 42


  * `Float` liczba zmiennopozycyjna pojedynczej precyzji, przykłady:


        1.0 1e-19 0.001


  * `Bool` wartość logiczna o konstruktorach:


        true : Bool
        false : Bool


Dodatkowo dostępne są trzy konstruktory typów (wszystkie określone na dowolnych
typach rodzaju `*`):
  * `Maybe A` określający możliwość braku wyniku.
  * `Stream A` określający leniwy strumień danych.
  * `Pair A B` będący odpowiednikiem iloczynu kartezjańskiego.


## Funkcje wbudowane


Typy prymitywne definiują następujące funkcje:


  * `Int`


        sum : Int -> Int -> Int
        sub : Int -> Int -> Int
        mul : Int -> Int -> Int
        quot : Int -> Int -> Maybe Int
        mod : Int -> Int -> Maybe Int
        eq : Int -> Int -> Bool
        leq : Int -> Int -> Bool
        float : Int -> Float


  * `Float`


        sumf : Float -> Float -> Float
        subf : Float -> Float -> Float
        mulf : Float -> Float -> Float
        divf : Float -> Float -> Maybe Float
        eqf : Float -> Float -> Bool
        leqf : Float -> Float -> Bool
        floor : Float -> Int
        ceil : Float -> Int


  * `Bool`


        and : Bool -> Bool -> Bool
        not : Bool -> Bool


Wbudowane konstruktory typów posiadają następujące konstruktory wartości:


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


Oraz następujące selektory:


  * `Maybe A`


        for A : *, B : *
        maybe : Maybe A -> (A -> B) -> B -> B


  * `Stream A`


        for A : *
        head : Stream A -> Maybe A

        for A : *
        tail : Stream A -> Maybe (Stream A)


  * `Pair A B`


        for A : *, B : *
        fst : Pair A B -> A
        for A : *, B : *
        snd : Pair A B -> B


## Typowanie


Poza dynamicznym typowaniem podobnym jak w przykładach niżej nie posiada
(na razie) funkcji polimorficznych (to oznacza, że np. jeżeli istnieje wartość
o nazwie "plus" określona zarówno na liczbach całkowitych, np. `Int -> Int -> Int`
oraz na liczbach zmiennopozycyjnych `Float -> Float -> Float` to musi
być określona również na wszystkich innych typach, tj być w rzeczywistości typu
`for A : * A -> A -> A`).


W obecnej wersji nie ma możliwości tworzenia własnych typów danych,
można za to składać typy algebraiczne, np: `Pair (Stream Float) Int`.


## Komentarze


Dostępne są dwa rodzaje komentarzy:


    # Komentarz do końca linii
    (# Komentarz ograniczony #)


# Przykłady


## Składanie funkcji


    for A : *, B : *, C : *
    let compose : (B -> C) -> (A -> B) -> (A -> C) =
      fun g f x ->
        g (f x)
      end


## Fold


    for E : *, R : *
    let foldLeft : (R -> E -> R) -> R -> Stream E -> R =
      fun f acc l ->
        if empty l then
          acc
        else
          loop f (f acc (head l)) (tail l)
        end
      end


Co można zapisać też inaczej:


    for E : *, R : *
    let foldLeft : (R -> E -> R) -> R -> Stream E -> R =
      fun f ->
        fun acc l ->
          if empty l then
            acc
          else
            loop (f acc (head l)) (tail l)
          end
        end
      end


Różnica jest subtelna - w drugim przypadku `loop`owi brakuje funkcji `f`.
Jest tak ponieważ loop odnosi się zawsze do syntaktycznie najbliższego `fun`a
(właściwie będąc po prostu nazwą dla obecnej lambdy).
