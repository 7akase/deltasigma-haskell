import Test.HUnit
import System.IO

import TransferFunction

fact 1 = 1
fact n = n * fact (n-1)

tests = TestList
  [ "fact 1" ~: fact 1 ~?= 1
  , "fact 2" ~: fact 2 ~?= 2
  , "fact 3" ~: fact 3 ~?= 6
  , TestCase $ assertBool "lpf1" (abs lpf1Test < 0.01)
  ]


lpf1Test = (\x -> x - ans) . (!! 1) $ solve (lpf1 fp1) 0.0 x0 t
  where
    tau = 1.0
    fp1 = 1/2/pi/tau
    x0 = [x, y]
    x = 1
    y = 0
    t = tau
    ans = 1 - exp (-t)

main = do
  runTestText (putTextToHandle stderr False) tests
