# PuCProject - Top-Level Definition und Unit-Testing
*Von: Louis Marceau Holetzke*

## Unit-Testing

- Auch bekannt als Modultest oder Komponententest
- Werden benutzt um (Teil-)Ergebnisse nachzuweisen
- assert ⇒ feststellen
    - Bsp. assertTrue ⇒ wahr feststellen

### Implementierte Unit Tests

#### Bool
```kotlin
assertTrue(Expr)
assertFalse(Expr)
```

#### Int
```kotlin
assertGreaterThan(Expr, Expr)
assertGreaterEqualThan(Expr, Expr)
assertSmallerThan(Expr, Expr)
assertSmallerEqualThan(Expr, Expr)
```

#### Bool – Int – String – Funktionen
```kotlin
assertEqual(Expr, Expr)
assertNotEqual(Expr, Expr)
assertType(Expr, Type)
assertNotType(Expr, Type)
assertThrows(Expr)
```

## Top-Level Definition
- Auch bekannt unter Globalization
- Wird benötigt um Expressions mehrfach zu benutzen
- def ⇒ Top-Level Definition von Expressions
    - Bsp. def quad = λx: Int => x*x;
    
*Wir benötigen, wenn wir mehrere Expressions evaluieren wollen ein Trennsymbol, bspw. `;` welches in diesem Projekt auch benutzt wird.*

