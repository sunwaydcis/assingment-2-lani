Project: Hotel Booking EDA (Scala)

Overview
--------
This project performs exploratory data analysis (EDA) on a hotel booking dataset. The Scala program loads a CSV file `Hotel_Dataset.csv` and answers three questions:

1. Which country has the highest number of bookings?
2. Which hotel is the most economical (considering booking price, discount and profit margin)?
3. Which hotel is the most profitable considering visitor counts and profit margin?

Changes made
------------
- Introduced a generic `Analyzer` trait and three concrete analyzers to demonstrate polymorphism (inheritance and parametric polymorphism).
- Reworked collection usage to use modern Scala collection APIs: `groupMapReduce`, `groupMap`, `view`, and `mapValues` for efficient aggregation.
- Improved CSV reading to avoid charset errors by reading with `ISO-8859-1` (safe single-byte fallback).

Running
-------
From the project root (PowerShell):

```powershell
sbt compile
sbt run
```

Polymorphism & Collections (Discussion)
--------------------------------------
1) Polymorphism used in this project
- Subtype polymorphism (inheritance): The `Analyzer` trait defines an interface that multiple analyzers (`CountryAnalyzer`, `EconomicalHotelAnalyzer`, `ProfitableHotelAnalyzer`) extend. Code can call `analyze` on any subtype without knowing the concrete implementation.
- Parametric polymorphism (generics): `Analyzer` is generic (`Analyzer[+A]`) so each analyzer can return different result types while still conforming to the same interface. This enables strong typing and code reuse.

Benefits
- Reuse & decoupling: Consumers of `Analyzer` don't need to know concrete implementations and can work with the trait.
- Type safety: Parametric polymorphism preserves type information for analyzer results.
- Expressive collection operations: APIs like `groupMapReduce` make aggregation concise and efficient.

Limitations
- Type erasure & runtime checks: Since different analyzers return different types, working with a heterogeneous `List[Analyzer[?]]` can require runtime pattern matching or upcasting to `Analyzer[Any]`, which erodes static type guarantees.
- Complexity: Advanced collection operations can be concise but sometimes less readable to beginners.
- Encoding issues: CSV files from external sources may have unknown encodings; we used `ISO-8859-1` as a permissive fallback. For production, detect/convert to UTF-8 instead.

Mapping to SLOs
---------------
- SLO1 (Collections & Exceptions): The solution makes heavy use of Scala collection APIs (`groupMapReduce`, `groupMap`, `mapValues`, `view`) and uses `Try` for safe parsing and option handling. This demonstrates using collections and basic exception-safe parsing.
- SLO2 (Advanced OOP concepts): The code demonstrates inheritance (trait + concrete classes), polymorphism (both subtype and parametric/generic polymorphism), and shows how the collection API works with generic types.

If you want:
- I can convert the CSV file to UTF-8 and switch the reader back to strict UTF-8.
- I can add unit tests for each analyzer.
- I can further document the code with inline examples mapping to the grading rubric.
