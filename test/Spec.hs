import Test.HUnit
import System.IO

import TransferFunction

fact 1 = 1
fact n = n * fact (n-1)

tests = TestList
  [ "fact 1" ~: fact 1 ~?= 1
  , TestCase $ assertBool "lpf1" (abs lpf1Test < 0.01)
  , TestCase $ assertBool "lpf2" (abs lpf2Test < 0.01)
  ]


lpf1Test = (\x -> x - ans) . (!! 1) $ solve (lpf1 fp1 xdot) 0.0 x0 t
  where
    (x, y, xdot) = (1, 0, diffStepFunc)
    x0 = [x, y]

    tau = 1.0
    fp1 = 1/2/pi/tau
    t = tau
    ans = 1 - exp (-t)

lpf2Test = (\x -> x - ans) . (!! 1) $ solve (lpf2 (fp1,fp2) xdot) 0.0 x0 t
  where
    (x, xdot, y, sy) = (1, diffStepFunc, 0, 0)
    x0 = [x, y, sy]

    (tau, fp1, fp2) = (1.0, 1/2/pi/tau, fp1) 
    (t, ans) = (tau, 0.2642)

main = do
  runTestText (putTextToHandle stderr False) tests
