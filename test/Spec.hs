import Test.HUnit
import System.IO

import TransferFunction

fact 1 = 1
fact n = n * fact (n-1)

tests = TestList
  [ "fact 1" ~: fact 1 ~?= 1
  , TestCase $ assertBool "lpf1"    (abs lpf1Test    < 0.01)
  , TestCase $ assertBool "lpf2"    (abs lpf2Test    < 0.01)
  , TestCase $ assertBool "hpf1"    (abs hpf1Test    < 0.01)
  , TestCase $ assertBool "laglead" (abs lagleadTest < 0.01)
  ]


lpf1Test = (\x -> x-ans) . (!! 1) $ solve (lpf1 fp1 xdot) 0.0 x0 t
  where
    (x, y, xdot) = (0, 0, diffStepFunc)
    x0 = [x, y]

    tau = 1.0
    fp1 = 1/2/pi/tau
    t = tau
    ans = 1 - exp (-t)

lpf2Test = (\x -> x-ans) . (!! 1) $ solve (lpf2 (fp1,fp2) xdot) 0.0 x0 t
  where
    (x, xdot, y, sy) = (0, diffStepFunc, 0, 0)
    x0 = [x, y, sy]

    (tau, fp1, fp2) = (1.0, 1/2/pi/tau, fp1) 
    (t, ans) = (tau, 0.2642)

hpf1Test = (\x -> x-ans) . (!! 1) $ solve (hpf1 fp1 xdot) 0.0 x0 t
  where
    x0 = [x, y]
    (x, xdot, y) = (0, diffStepFunc, 0)

    (tau, fp1) = (1.0, 1/2/pi/tau) 
    (t, ans) = (tau, 0.3697)

lagleadTest = (\x -> x-ans) . (!! 1) $ solve (laglead (fp,fz) xdot) 0.0 x0 t
  where
    x0 = [x, y]
    (x, xdot, y) = (0, diffStepFunc, 0)

    (tau, fp, fz) = (1.0, 1/2/pi/tau, 1/2/pi/tau*2) 
    (t, ans) = (tau, 0.8151)

main = do
  runTestText (putTextToHandle stderr False) tests
