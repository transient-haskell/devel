module Main where
import Test.Properties
import Control.Exception
import System.IO.Unsafe
stringProperty= [Property "length" (\(x, y)-> length (x++y)== length x + length y)]

main= do
        let s=  "hello "
        let s2= "world"
        print $ s++ s2        --`verify` stringProperty `with`(s,s2)
        print "that's all!"




quickCheckProperty x y=  length (x ++ y)== length x + length y + 1

main2= do
        let s=  "hello "
        let s2= "world"       `check` assert $ quickCheckProperty s s
        print $ s++ s2        --`verify` [Property "stringSumLength" $ uncurry quickCheckProperty] `with`(s,s2)
        print "that's all!"


check v f t  =  unsafePerformIO . handle handle1 . return $ f t v
 where
 handle1 (AssertionFailed e) =   do
           print  . AssertionFailed $  "holaaaaa" ++ "\n"++ show e
           return v
