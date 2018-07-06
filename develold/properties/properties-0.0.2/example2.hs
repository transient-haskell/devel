module Main where
import Test.Properties

stringProperty= [Property "length" (\(x, y)-> length (x++y)== length x + length y)]

main= do
        let s=  "hello "
        let s2= "world"
        print $ s++ s2        `verify` stringProperty `with`(s,s2)
        print "that's all!"




quickCheckProperty x y=  length (x++y)== length x + length y

main2= do
        let s=  "hello "
        let s2= "world"
        print $ s++ s2        `verify` [Property "stringSumLength" $ uncurry quickCheckProperty] `with`(s,s2)
        print "that's all!"
