data Dict a b 
    = Empty 
    | Entry a b (Dict a b)
    deriving (Show)

instance Functor (Dict a) where
    fmap f Empty = Empty
    fmap f (Entry a b d) = Entry a (f b) (fmap f d)

