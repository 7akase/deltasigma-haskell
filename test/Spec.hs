import Test.HUnit
import System.IO

import TransferFunction

fact 1 = 1
fact n = n * fact (n-1)

tests = TestList
  [ "fact 1" ~: fact 1 ~?= 1
  , TestCase $ assertBool "lpf1" (abs lpf1Test < 0.01)
  ]


lpf1Test = (\x -> x - ans) . (!! 1) $ solve (lpf1 fp1 xdot) 0.0 x0 t
  where
    x0 = [x, y]
    (x, y, xdot) = (1, 0, diffStepFunc)

    tau = 1.0
    fp1 = 1/2/pi/tau
    t = tau
    ans = 1 - exp (-t)

main = do
  runTestText (putTextToHandle stderr False) tests
