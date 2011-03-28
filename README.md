# Opis języka

flower jest językiem czysto funkcyjnym z silnym, jawnym, dynamicznym typowaniem.

## Dane

Posiada wbudowany typ leniwego strumienia danych (odpowiednik list w haskellu).
Z pozostałych prymitywów można wymienić liczby całkowite, zmiennopozycyjne
pojedynczej precyzji oraz wymierne.

## Typowanie

Poza dynamicznym typowaniem podobnym jak w przykładach niżej nie posiada
(na razie) funkcji polimorficznych (to oznacza, że np. jeżeli istnieje wartość
o nazwie "plus" określona zarówno na liczbach całkowitych, np. `Int -> Int -> Int`
oraz na liczbach wymiernych `Frac Int -> Frac Int -> Frac Int` to musi
być określona również na wszystkich innych typach, tj być w rzeczywistości typu
`for A : * A -> A -> A`).

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
    let fold-left : (R -> E -> R) -> R -> Stream E -> R =
      fun f acc l ->
        if empty l then
          acc
        else
          loop f (f acc (head l)) (tail l)
        end
      end

Co można zapisać też inaczej:

    for E : *, R : *
    let fold-left : (R -> E -> R) -> R -> List E -> R =
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
