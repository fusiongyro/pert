module Main where

{-

pert FILENAME.CSV > FILENAME.dot

simulate FILENAME.CSV [ -b 2.0,3.0    -- beta distribution
                      | -n            -- normal distribution
                      | -e            -- "expected value" ]
                      [ -i NUM        -- iterations (default: 1000) ]

What should I keep? One column per node, early finish-late finish, plus last finish overall?
Should I output the critical path?


-- All we need to keep are the last finish values
-- We could also just display percent finished by deadlines (40, 50, etc.)

-}


