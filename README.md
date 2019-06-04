A toy implementation of a linked list, regarding its catamorphism (fold) as the primitive data.

The concept is laid out along with the implementation in `CataList.scala`.

There is a ScalaCheck specification in `TestCataList`; running that spec will generate
100 random tests for each specified property.  If any fail, it will attempt to reduce
the failure to a minimal test-case to make identification of the bug simpler. 