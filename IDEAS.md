# Feature ideas

## Arbitrary number bases

- has to be bigger than 1 and smaller than 37
    | prefix | shorthand | postfix |  base   |         result          |
    | :----- | :-------: | :-----: | :-----: | :---------------------: |
    | 02_    |   0b/B    |   b2    | base 2  | two's complement binary |
    | 08_    |   0o/O    |   b8    | base 8  |          octal          |
    | 016_   |   0x/X    |   b16   | base 16 |           hex           |
    | 01_    |           |   b1    |         |          error          |
    | 04_    |           |   b4    |         |         base 4          |
    | 035_   |           |   b35   |         |         base 35         |
    | 037_   |           |   b37   |         |          error          |
