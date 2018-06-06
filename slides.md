---
title: Programación funcional en Rust con abstracciones sin coste
author: Carlos León
date: 6 de junio de 2018
---

# ¿Qué es la programación funcional?

---

<!-- Elementos que suelen o pueden estar, no es una lista exhaustiva-->

- Funciones de primer orden
- Datos inmutables
- Transparencia referencial
- Expresiones lambda

---

- Clausuras
- Recursión
- Evaluación perezosa
- Sistemas de tipos estrictos

# ¿Por qué no usar programación funcional siempre?


## Porque hay problemas que se modelan bien como iterativos

Aunque los lenguajes funcionales suelen proveer estructuras de computación iterativa

## Porque aprender FP cuesta mucho

O, al menos, hay mucho menos entrenamiento que en lenguajes iterativos

## Porque *perdemos control con las abstracciones*

En casi todos los lenguajes funcionales hay maneras de acceder al bajo nivel

Pero los beneficios del estilo funcional *son menores*

# Diseño de lenguajes modernos

---

![Programación funcional en lenguajes modernos](foto_nata.jpg)

## Haskell

```haskell
main :: IO ()
main = do
  putStrLn "¡Hola, mundo!"
```


## Scala

```scala
object HelloWorld {
  def main(args: Array[String]): Unit = {
    println("¡Hola, mundo!")
  }
}
```

## Erlang

```erlang
-module(hello).
-export([hello_world/0]).

hello_world() -> io:fwrite("hello, world\n").
```

## La abstracción añade coste

Los lenguajes funcionales clásicos no aportan ideas para manejar memoria *a bajo nivel*


# Rust

---

```rust
fn main() {
    println!("¡Hola, mundo!");
}
```

---

Rust en un lenguaje moderno creado por Mozilla (apareció en 2010)

---

Está diseñado para ser *seguro*, *concurrente* y *práctico*



## Variables inmutables

Si $x = 5$, no puede ser que $x = 6$

```rust
fn main() {
    let x = 1;
    x = 2; // no compila...
}
```

---

Puedo mutar variables con `mut`{.rust}

```rust
fn main() {
    let mut x = 1;
    x = 2;
    println!("Puedo mutar x: {}", x);
}
```

## Propietarios

Cada variable *sólo* tiene un propietario, la memoria se liberará con su él

```rust
let s1 = String::from("¡Hola");
let s2 = s1;

println!("{}, mundo!", s1); // no se puede!
```


## *Lifetime*


En C/C++ es posible manipular una variable más allá de su *lifetime*

```cpp
struct Jugador {
   std::string *nombre; // el nombre podría eliminarse antes que el jugador
}
```

---

En Rust nos aseguramos de que `name`{.rust} sobreviva a cualquier `Jugador`{.rust}

```rust
struct Jugador<’a> {
 nombre: &’a str,
}
```


# Rust y estructuras funcionales

---

## Enumerados estructurados

```rust
enum Option<T> { None, Some(T) }
```

```haskell
data Maybe a = Nothing | Just a
```

---

Resolvemos los estructurados con `match`{.rust} (*¡exhaustivo!*)

```rust
let j : Option<u32> = Some(5);
match j {
    Some(x) => println!("Número {}", x),
    None    => println!("Ningún número...")
} // imprime 5
```

```haskell
let j = Just 5
case j of
    Just x  -> putStr "Número " >> print x
    Nothing -> putStrLn "Ningún número"
```

## Tipos de datos recursivos

```rust
enum List<T> { Nil, Cons(T, Box<List<T>>) }
```

```haskell
data List a = Nil | Cons a (List a)
```

## Lambdas

```rust
let y = 5;
let closure = |x| x + y;
println!("Captura: {}", closure(1)); // imprime 6
```  

```haskell
let y = 5
let closure = \x -> x + x
putStr "Captura :" >> print (closure 1)
```


## Traits

```rust
trait Portador {
    fn pesoTotalInventario(&self) -> i32;
}
```

```haskell
class Portador a where
    pesoTotalInventario :: a -> Integer
```

## Iteradores

Con el *trait* `Iterator` conseguimos evaluación perezosa

```rust
v.iter().map(|x| x + 1);
```

```haskell
map (+1) v
```

# Limitaciones *funcionales* de Rust

---

## Falta de pureza

```rust
fn pura(x: u32) {
  println!("ups..."); // el compilador no lo prohíbe
}
```

## Eliminación de llamada recursiva

```rust
fn a() { b() }
fn b() { a() }
fn main() { a() } // la pila se desborda
```

```haskell
a = b
b = a
main = a -- bucle infinito
```

## Evaluación perezosa

Rust no hace evaluación perezosa por defecto

Usa [macros higiénicos](https://doc.rust-lang.org/book/first-edition/macros.html) e [iteradores](https://doc.rust-lang.org/book/second-edition/ch13-02-iterators.html)

Los iteradores generan código **muy eficiente**


# ¿Se puede alcanzar un buen rendimiento con abstracciones en otros lenguajes?

---

## Haskell, Scala...

El recolector de basura es un problema

Usar memoria a bajo nivel *no es seguro*

## ¿Y qué hay de C++?

Con C++ se inventó la idea de *abstracciones sin coste*

---

Pero el compilador no ofrece *seguridad*

```cpp
MiObjeto * o = Factoria.CreaObjeto();
delete o; // no es seguro
```


# Resumen

---

## Rust se acerca mucho a estructuras funcionales

---

## Las estructuras funcionales se resuelven en la compilación


---

## Se puede alcanzar una eficiencia en compilación muy alta


---

## Gracias

Carlos León \
<<cleon@ucm.es>> \
Universidad Complutense de Madrid
