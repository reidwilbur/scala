Write a function which calculates the sum of the minimal path through a triangle. The triangle is represented as a collection of vectors. The path should start at the top of the triangle and move to an adjacent number on the next row until the bottom of the triangle is reached.

assert
f(  [[1]
    [2 4]
   [5 1 4]
  [2 3 4 5]] ) == 7 ; 1+2+1+3

assert
f(    [[3]
      [2 4]
     [1 9 3]
    [9 9 2 4]
   [4 6 6 7 8]
  [5 7 3 5 1 4]] ) == 20 ; 3+4+3+2+7+1

