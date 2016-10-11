module CIS194.NavinK.HW04Tests where

import CIS194.NavinK.HW04
import CIS194.Testing

-- Ex2 Tests

ex2Tests :: [Test]
ex2Tests = [ testF2 "Eq test" (==)
             [ (P [1,2,1], P [1,2,1], True),
               (P [1,2,1], P [0,1,2,1], False),
               (P [1,2,1], P [1,2,1,0,0], True),
               (P [1,2,1,0,0], P [1,2,1,0], True)]]

-- ex3 Tests (show) ------------------------------------------

ex3Tests :: [Test]
ex3Tests = [ testF1 "Show test" show
             [ (P [0], "0"),
               (P [1], "1"),
               (P [1, 2], "2x + 1"),
               (P [0, 2, 1], "x^2 + 2x"),
               (P [0, -1, 2], "2x^2 + -x"),
               (P [-1, 2, 3], "3x^2 + 2x + -1"),
               (P [1, 0, 0, 2], "2x^3 + 1")
             ]
           ]

-- ex4 Tests (plus) ------------------------------------------

ex4Tests :: [Test]
ex4Tests = [ testF2 "Plus test" (+)
             [ (P [5,0,1], P [1,1,2], P [6,1,3] ),
               (P [1, 0, 1],  P [1, 1], P [2, 1, 1]) ]
           ]

ex5Tests :: [Test]
ex5Tests = [ testF2 "Mult test" (*)
             [ (P [1, 1, 1], P [2, 2], P [2, 4, 4, 2] ),
               (P [0], P [2,2], P [0])]]

ex6Tests :: [Test]
ex6Tests = [testF1 "Negate test" negate
            [ (P [1,2,3], P [-1, -2, -3]), (P [0], P[0]) ]
           ]

ex7Tests :: [Test]
ex7Tests = [ testF2 "applyP test" applyP
             [ (P [1, 1], 2, 3),
               (P [1,2,1], 1, 4),
               (P [1,2,1], 2, 9) ]]

ex8Tests :: [Test]
ex8Tests = [ testF1 "deriv" deriv
             [ (P [6, 3, 1], P [3, 2]),
               (P [4], P[0]),
               (P [3, 1], P[1]) ]]

allTests :: [Test]
allTests = concat [ ex2Tests
                  , ex3Tests
                  , ex4Tests
                  , ex5Tests
                  , ex6Tests
                  , ex7Tests
                  , ex8Tests
                  ]
