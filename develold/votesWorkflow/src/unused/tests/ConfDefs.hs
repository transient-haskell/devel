module ConfDefs where

import Data.Typeable

data Conf= Conf Int Int


instance Show Conf where
  show (Conf a b)= "Conf "++show a++" "++show b


instance Typeable Conf where
  typeOf _= mkTyConApp (mkTyCon "ConfDefs.Conf") []





