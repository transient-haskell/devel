
clas InterfaceHandler x where


class Prototype where
     type :: Int
     typeIdentifier : String
     methods ::  [(String,  [IDynamic] -> IDynamic)] -> IO ()
     addMethod::  (String,  [IDynamic] -> IDynamic) -> IO ()

     implements :: MethodName  -> Bool
     implements= False



     imvoke ::

     ( . ) = invoke


instance Prototype String where
    methods ("show", show . fromIDyn, "read",
