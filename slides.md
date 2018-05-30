---
title: Programación funcional en Rust con abstracciones sin coste
author: Carlos León
date: 6 de junio de 2018
---

# Pero... ¿qué es la programación funcional?

# Si la PF es tan buena, ¿por qué no usarla siempre?

## Porque aprender FP cuesta mucho

## Porque *perdemos control con las abstracciones*

# Diseño de lenguajes modernos

## Haskell (moderno)

## Scala

# Pero seguimos teniendo coste añadido por la abstracción

# Rust

---

Rust en un lenguaje moderno (2010)

---

Está diseñado para ser *seguro*, *concurrente* y *práctico*

## Seguridad en Rust

<!-- TODO (Carlos): pointers--luego habla de option -->

# Rust y estructuras funcionales

<!-- TODO (Carlos): que no se te olvide hablar de option -->

## Estructuración de datos

## Lambdas

## Variables inmutables

Si $x == 5$, no puede ser que $x == 6$

## Enumerados estructurados

## "Pattern matching"

# ¿Por qué no hay prácticamente coste añadido con Rust?

# Limitaciones *funcionales* de Rust

## Eliminación de llamada recursiva

## Evaluación perezosa

# ¿Se puede alcanzar un buen rendimiento con abstracciones en otros lenguajes?

## Haskell

## ¿Y qué hay de C++?

Con C++ se inventó la idea de *abstracciones sin coste*

---

Pero el compilador no me ofrece *seguridad*:

```cpp
MiObjeto * o = Factoria.CreaObjeto();
delete o; // ¿es seguro?
```

# Gracias

Carlos León
<cleon@ucm.es>
